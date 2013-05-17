{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Database.PostgreSQL.Escape (
    fmtSql, quoteIdent, Id(..)
  , buildSql, buildAction, buildLiteral, buildByteA, buildIdent
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8 (fromChar)
import Blaze.ByteString.Builder.Internal
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import Data.Monoid
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Storable (pokeByteOff)
import Foreign.Ptr
import GHC.Prim (Addr#, and#, geAddr#, geWord#, int2Word#
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
{-# INLINE fastFindIndex #-}
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
{-# INLINE fastBreak #-}
fastBreak test bs
  | Just n <- fastFindIndex test bs = (S.unsafeTake n bs, S.unsafeDrop n bs)
  | otherwise                       = (bs, S.empty)

quoter :: S.ByteString -> S.ByteString -> (Word# -> Bool)
          -> (Word8 -> Builder) -> S.ByteString -> Builder
{-# INLINE quoter #-}
quoter start end escPred escFn bs0 =
  mconcat [copyByteString start, escaped bs0, copyByteString end]
  where escaped bs = case fastBreak escPred bs of
          (h, t) | S.null t  -> fromByteString h
                 | otherwise -> fromByteString h <>
                                escFn (S.unsafeHead t) <>
                                escaped (S.unsafeTail t)

-- | Quote an identifier using unicode quoting syntax.  This is
-- necessary for identifiers containing a question mark, as otherwise
-- "PostgreSQL.Simple"'s naive formatting code will attempt to match
-- the question mark to a paremeter.
uBuildIdent :: S.ByteString -> Builder
uBuildIdent ident = quoter " U&\"" "\"" isSpecial esc ident
  where isSpecial 34## = True   -- '"'
        isSpecial 63## = True   -- '?'
        isSpecial 92## = True   -- '\\'
        isSpecial _    = False
        esc c = copyByteString $ case () of
          _ | c == c2b '"'  -> "\"\""
            | c == c2b '?'  -> "\\003f"
            | c == c2b '\\' -> "\\\\"
            | otherwise     -> error "uquoteIdent"

-- | Build a quoted identifier.  Generally you will want to use
-- 'quoteIdent', and for repeated use it will be faster to use
-- @'fromByteString' . 'quoteIdent'@, but this internal function is
-- exposed in case it is useful.
buildIdent :: S.ByteString -> Builder
buildIdent ident
  | Just _ <- fastFindIndex isQuestionmark ident = uBuildIdent ident
  | otherwise = quoter "\"" "\"" isDQuote (const $ copyByteString "\"\"") ident
  where isQuestionmark 63## = True
        isQuestionmark 0##  = error "quoteIdent: illegal NUL character"
        isQuestionmark _    = False
        isDQuote 34## = True
        isDQuote _    = False

-- | Quote an identifier such as a table or column name using
-- double-quote characters.  Note this has nothing to do with quoting
-- /values/, which must be quoted using single quotes.  (Anyway, all
-- values should be quoted by 'query' or 'fmtSql'.)  This function
-- uses a unicode escape sequence to escape \'?\' characters, which
-- would otherwise be expanded by 'query', 'formatQuery', or 'fmtSql'.
--
-- >>> S8.putStrLn $ quoteIdent "hello \"world\"!"
-- "hello ""world""!"
-- >>> S8.putStrLn $ quoteIdent "hello \"world\"?"
--  U&"hello ""world""\003f"
--
-- Note that this quoting function is correct only if
-- @client_encoding@ is @SQL_ASCII@, @client_coding@ is @UTF8@, or the
-- identifier contains no multi-byte characters.  For other coding
-- schemes, this function may erroneously duplicate bytes that look
-- like quote characters but are actually part of a multi-byte
-- character code.  In such cases, maliciously crafted identifiers
-- will, even after quoting, allow injection of arbitrary SQL commands
-- to the server.
--
-- The upshot is that it is unwise to use this function on identifiers
-- provided by untrustworthy sources.  Note this is true anyway,
-- regardless of @client_encoding@ setting, because certain \"system
-- column\" names (e.g., @oid@, @tableoid@, @xmin@, @cmin@, @xmax@,
-- @cmax@, @ctid@) are likely to produce unexpected results even when
-- properly quoted.
--
-- See 'Id' for a convenient way to include quoted identifiers in
-- parameter lists.
quoteIdent :: S.ByteString -> S.ByteString
quoteIdent = toByteString . buildIdent

-- | An identifier is a table or column name.  When rendered into a
-- SQL query by 'fmtSql', it will be double-quoted, rather than
-- single-quoted.  For example:
--
-- >>> fmtSql "select * from ? where name = ?" (Id "MyTable", "A Name")
-- "select * from \"MyTable\" where name =  E'A Name'"
newtype Id = Id S.ByteString deriving Show
instance IsString Id where
  fromString = Id . fromString
instance ToField Id where
  toField (Id name) = Plain $ buildIdent name

hexNibblesPtr :: Ptr Word8
{-# NOINLINE hexNibblesPtr #-}
hexNibblesPtr = unsafeDupablePerformIO $ do
  ptr <- mallocBytes 16
  sequence_ $ zipWith (\o v -> pokeByteOff ptr o $ c2b v)
    [0..] (['0'..'9'] ++ ['a'..'f'])
  return ptr

-- | Bad things will happen if the argument is greater than 0xff.
uncheckedWriteNibbles# :: Addr# -> Word# -> State# d -> State# d
{-# INLINE uncheckedWriteNibbles# #-}
uncheckedWriteNibbles# p w rw0 =
  case (# word2Int# (w `uncheckedShiftRL#` 4# )
       , word2Int# (w `and#` 0xf## ) #) of { (# h, l #) ->
  case readWord8OffAddr# nibbles h rw0 of { (# rw1, hascii #) ->
  case writeWord8OffAddr# p 0# hascii rw1 of { rw2 ->
  case readWord8OffAddr# nibbles l rw2 of { (# rw3, lascii #) ->
  writeWord8OffAddr# p 1# lascii rw3 }}}}
  where !(Ptr nibbles) = hexNibblesPtr

hexCharEscBuilder :: Word8 -> Builder
{-# INLINE hexCharEscBuilder #-}
hexCharEscBuilder (W8# w) = fromWrite $ exactWrite 4 $ \(Ptr p) -> IO $ \rw0 ->
  (# uncheckedWriteNibbles# (p `plusAddr#` 2#) w
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
    (# rw1, w #) -> (# uncheckedWriteNibbles# dst w rw1, () #)

buildByteA :: S.ByteString -> Builder
buildByteA bs = equote $
  fromBuildStepCont $ \cont (BufRange (Ptr bb0) (Ptr be0)) ->
  S.unsafeUseAsCStringLen bs $ \(Ptr inptr0, I# inlen0) -> do
  let ine = plusAddr# inptr0 inlen0
      fill oute inp outp
        | inp `geAddr#` ine = cont (BufRange (Ptr outp) (Ptr oute))
        | plusAddr# outp 2# `geAddr#` oute = return $
            bufferFull (2 * (I# (ine `minusAddr#` inp)) + 1) (Ptr outp) $
            \(BufRange (Ptr bb) (Ptr be)) -> fill be inp bb
        | otherwise = do copyByteToNibbles inp outp
                         fill oute (inp `plusAddr#` 1#) (outp `plusAddr#` 2#)
  fill be0 inptr0 bb0
  where equote b = mconcat [fromByteString " E'\\\\x", b, fromChar '\'']



buildAction :: Action -> Builder
buildAction (Plain b)        = b
buildAction (Escape bs)      = buildLiteral bs
buildAction (EscapeByteA bs) = buildByteA bs
buildAction (Many bs)        = mconcat $ map buildAction bs


-- | A builder version of 'fmtSql', possibly useful if you are about
-- to concatenate various individually formatted query fragments and
-- want to save the work of concatenating each individually.
buildSql :: (ToRow p) => Query -> p -> Builder
buildSql (Query template) param =
  intercatlate (split template) (map buildAction $ toRow param)
  where intercatlate (t:ts) (p:ps) = t <> p <> intercatlate ts ps
        intercatlate [t] []        = t
        intercatlate _ _           =
          error $ "buildSql: wrong number of parameters for " ++ show template
        split s = case S.breakByte (c2b '?') s of
          (h,t) | S.null t  -> [fromByteString h]
                | otherwise -> fromByteString h : split (S.unsafeTail t)

-- | Take a SQL template containing \'?\' characters and a list of
-- paremeters whose length must match the number of \'?\' characters,
-- and format the result as an escaped 'S.ByteString' that can be used
-- as a query.
--
-- Like 'formatQuery', this function is naive about the placement of
-- \'?\' characters and will expand all of them, even ones within
-- quotes.  To avoid this, you must use 'quoteIdent' on identifiers
-- containing question marks.
--
-- Also like 'formatQuery', \'?\' characters touching other \'?\'
-- characters or quoted strings may do the wrong thing, and end up
-- doubling a quote, so avoid substrings such as @\"??\"@ or
-- @\"?'string'\"@, as these could get expanded to, e.g.,
-- @\"\'param''string'\"@, which is a single string containing an
-- apostrophe, when you probably wanted two strings.
fmtSql :: (ToRow p) => Query -> p -> Query
fmtSql template param = Query $ toByteString $ buildSql template param
