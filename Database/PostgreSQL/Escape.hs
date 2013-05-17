{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Database.PostgreSQL.Escape (
    fmtSql, quoteIdent
  , buildSql, buildAction, buildLiteral, buildByteA
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Internal
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Storable (poke, pokeByteOff)
import Foreign.Ptr
import GHC.Prim ((+#), Addr#, and#, geAddr#, geWord#, Int#, int2Word#
                , minusAddr#, ord# , plusAddr#, readWord8OffAddr#
                , State# , uncheckedShiftRL#, word2Int#, writeWord8OffAddr#
                , Word#)
import GHC.Ptr (Ptr(Ptr))
import GHC.Types (Char(C#), Int(I#), IO(IO))
import GHC.Word (Word8(W8#))
import System.IO.Unsafe (unsafeDupablePerformIO)

c2b :: Char -> Word8
c2b (C# i) = W8# (int2Word# (ord# i))

c2b# :: Char -> Word#
c2b# (C# i) = int2Word# (ord# i)

fastFindIndex :: (Word# -> Bool) -> S.ByteString -> Maybe Int
fastFindIndex test bs =
  S.inlinePerformIO $ S.unsafeUseAsCStringLen bs $ \(Ptr bsp0, I# bsl0) -> do
    let bse = bsp0 `plusAddr#` bsl0
        check bsp = IO $ \rw -> case readWord8OffAddr# bsp 0# rw of
          (# rw1, w #) -> (# rw1, test w #)
        go bsp | bsp `geAddr#` bse = return Nothing
               | otherwise = do
                 match <- check bsp
                 if match
                   then return $ Just $ I# (bsp `minusAddr#` bsp0)
                   else go (bsp `plusAddr#` 1#)
    go bsp0

fastBreak :: (Word# -> Bool) -> S.ByteString -> (S.ByteString, S.ByteString)
fastBreak test bs
  | Just n <- fastFindIndex test bs = (S.unsafeTake n bs, S.unsafeDrop n bs)
  | otherwise                       = (bs, S.empty)

quoter :: S.ByteString -> S.ByteString -> (Word# -> Bool)
          -> (Word8 -> Builder) -> S.ByteString -> Builder
quoter start end escPred escFn bs0 =
  mconcat [copyByteString start, escaped bs0, copyByteString end]
  where escaped bs = case fastBreak escPred bs of
          (h, t) | S.null t  -> fromByteString h
                 | otherwise -> fromByteString h <>
                                escFn (S.unsafeHead t) <>
                                escaped (S.unsafeTail t)

quoteIdent :: S.ByteString -> S.ByteString
quoteIdent ident
  | Just _ <- fastFindIndex isQuestionmark ident = uquoteIdent ident
  | otherwise = toByteString $ quoter "\"" "\"" isDQuote
                (const $ copyByteString "\"\"") ident
  where isQuestionmark 63## = True
        isQuestionmark 0##  = error "quoteIdent: illegal NUL character"
        isQuestionmark _    = False
        isDQuote 34## = True
        isDQuote _    = False

uquoteIdent :: S.ByteString -> S.ByteString
uquoteIdent ident = toByteString $ quoter " U&\"" "\"" isSpecial esc ident
  where isSpecial 34## = True   -- '"'
        isSpecial 63## = True   -- '?'
        isSpecial 92## = True   -- '\\'
        isSpecial _    = False
        esc c = copyByteString $ case () of
          _ | c == c2b '"'  -> "\"\""
            | c == c2b '?'  -> "\\003f"
            | c == c2b '\\' -> "\\\\"
            | otherwise     -> error "uquoteIdent"
          

hexNibblesPtr :: Ptr Word8
{-# NOINLINE hexNibblesPtr #-}
hexNibblesPtr = unsafeDupablePerformIO $ do
  ptr <- mallocBytes 16
  sequence_ $ zipWith (\o v -> pokeByteOff ptr o $ c2b v)
    [0..] (['0'..'9'] ++ ['a'..'f'])
  return ptr

-- | Bad things will happen if the argument is greater than 0xff.
uncheckedWriteNibblesOff# :: Addr# -> Int# -> Word# -> State# d -> State# d
{-# INLINE uncheckedWriteNibblesOff# #-}
uncheckedWriteNibblesOff# p o w rw0 =
  case (# word2Int# (w `uncheckedShiftRL#` 4# )
       , word2Int# (w `and#` 0xf## ) #) of { (# h, l #) ->
  case readWord8OffAddr# nibbles h rw0 of { (# rw1, hascii #) ->
  case writeWord8OffAddr# p o hascii rw1 of { rw2 ->
  case readWord8OffAddr# nibbles l rw2 of { (# rw3, lascii #) ->
  writeWord8OffAddr# p (o +# 1#) lascii rw3 }}}}
  where !(Ptr nibbles) = hexNibblesPtr

hexCharEscBuilder :: Word8 -> Builder
{-# INLINE hexCharEscBuilder #-}
hexCharEscBuilder (W8# w) = fromWrite $ exactWrite 4 $ \(Ptr p) -> IO $ \rw0 ->
  (# uncheckedWriteNibblesOff# p 2# w
      (writeWord8OffAddr# p 1# (c2b# 'x')
       (writeWord8OffAddr# p 0# (c2b# '\\') rw0))
  , () #)

buildLiteral :: S.ByteString -> Builder
buildLiteral = quoter " E'" "'" isSpecial esc
  where isSpecial 39## = True   -- '\''
        isSpecial 63## = True   -- '?'
        isSpecial 92## = True   -- '\\'
        isSpecial b    = b `geWord#` 128##
        esc b | b == c2b '\'' = copyByteString "''"
              | b == c2b '\\' = copyByteString "\\\\"
              | otherwise     = hexCharEscBuilder b


copyByteToNibbles :: Addr# -> Addr# -> IO ()
{-# INLINE copyByteToNibbles #-}
copyByteToNibbles src dst = IO $ \rw0 ->
  case readWord8OffAddr# src 0# rw0 of
    (# rw1, w #) -> (# uncheckedWriteNibblesOff# dst 0# w rw1, () #)

buildByteA :: S.ByteString -> Builder
buildByteA bs = mappend (fromByteString "'\\x") $
  fromBuildStepCont $ \cont (BufRange (Ptr bb0) (Ptr be0)) ->
  S.unsafeUseAsCStringLen bs $ \(Ptr inptr0, I# inlen0) -> do
  let ine = plusAddr# inptr0 inlen0
      fill oute inp outp
        | inp `geAddr#` ine = closeQuote (Ptr oute) (Ptr outp)
        | plusAddr# outp 2# `geAddr#` oute = return $
            bufferFull (2 * (I# (ine `minusAddr#` inp)) + 1) (Ptr outp) $
            \(BufRange (Ptr bb) (Ptr be)) -> fill be inp bb
        | otherwise = do copyByteToNibbles inp outp
                         fill oute (inp `plusAddr#` 1#) (outp `plusAddr#` 2#)
      closeQuote oute outp
        | outp >= oute =
          return $ bufferFull 1 outp $ \(BufRange bb be) -> closeQuote be bb
        | otherwise = do poke outp (c2b '\'')
                         cont (BufRange (outp `plusPtr` 1) oute)
  fill be0 inptr0 bb0


buildAction :: Action -> Builder
buildAction (Plain b)        = b
buildAction (Escape bs)      = buildLiteral bs
buildAction (EscapeByteA bs) = buildByteA bs
buildAction (Many bs)        = mconcat $ map buildAction bs


buildSql :: (ToRow p) => S.ByteString -> p -> Builder
buildSql template param =
  intercatlate (split template) (map buildAction $ toRow param)
  where intercatlate (t:ts) (p:ps) = t <> p <> intercatlate ts ps
        intercatlate [t] []        = t
        intercatlate _ _           =
          error $ "buildSql: wrong number of parameters for " ++ show template
        split s = case S.breakByte (c2b '?') s of
          (h,t) | S.null t  -> [fromByteString h]
                | otherwise -> fromByteString h : split (S.unsafeTail t)

fmtSql :: (ToRow p) => S.ByteString -> p -> S.ByteString
fmtSql template param = toByteString $ buildSql template param
