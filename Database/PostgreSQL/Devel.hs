
module Database.PostgreSQL.Devel where

import Control.Exception
import Control.Monad
import Data.Functor
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Env
import System.Posix.Files
import System.Process

isNonEmptyDir :: FilePath -> IO Bool
isNonEmptyDir dir =
  catchJust (\e -> if isDoesNotExistError e then Just () else Nothing)
  ((> 2) . length <$> getDirectoryContents dir)
  (const $ return False)


addDirectives :: [(String, String)] -> [String] -> [String]
addDirectives directives [] = map snd directives
addDirectives directives (cl:cls)
  | Just l <- lookup directive directives =
      (if comment then [l, cl] else [l]) ++
      addDirectives (directives \\ [(directive,l)]) cls
  | otherwise = cl : addDirectives directives cls
  where (comment, directive)
          | '#':clr <- cl, [(d,_)] <- lex clr = (True, d)
          | [(d,_)] <- lex cl                 = (False, d)
          | otherwise                         = (False, "")

pgDirectives :: FilePath -> [(String, String)]
pgDirectives dir = [
    ("unix_socket_directory", "unix_socket_directory = '" ++ dir ++ "'")
  , ("logging_collector",  "logging_collector = yes")
  , ("listen_addresses", "listen_addresses = ''")]

initLocalDb :: FilePath -> IO ()
initLocalDb dir = do
  (exit, out, err) <- readProcessWithExitCode "pg_ctl"
                      ["-D", dir, "-o", "--no-locale", "init"] ""
  when (exit /= ExitSuccess) $ fail err
  dir' <- canonicalizePath dir
  let confpath = dir </> "postgresql.conf"
  oldconf <- lines <$> readFile confpath
  let conf = unlines $ addDirectives (pgDirectives dir') oldconf
  length conf `seq` writeFile confpath conf
  return ()


