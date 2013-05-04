
module Database.PostgreSQL.Devel
       (initLocalDB, stopLocalDB, setLocalDB) where

import Control.Exception
import Control.Monad
import Data.Functor
import Data.List
import Database.PostgreSQL.Simple
import System.Directory
import System.Exit
import System.FilePath
-- import System.IO
import System.IO.Error
import System.Posix.Env
-- import System.Posix.Files
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

createLocalDB :: FilePath -> IO ()
createLocalDB dir = do
  (exit, _, err) <- readProcessWithExitCode "pg_ctl"
                      ["-D", dir, "-o", "--no-locale", "init"] ""
  when (exit /= ExitSuccess) $ fail err
  dir' <- canonicalizePath dir
  writeFile (dir </> "README_BEFORE_DELETING") $
    "## IMPORTANT:  Run the following command before deleting this " ++
    "directory ##\n\n" ++
    "pg_ctl -D " ++ showCommandForUser dir' [] ++ " stop -m immediate\n\n"
  let confpath = dir </> "postgresql.conf"
  oldconf <- lines <$> readFile confpath
  let conf = unlines $ addDirectives (pgDirectives dir') oldconf
  length conf `seq` writeFile confpath conf
  return ()

-- | Create a local database cluster entirely self-contained within
-- one directory.
initLocalDB :: FilePath -> IO ConnectInfo
initLocalDB dir0 = do
  exists <- isNonEmptyDir dir0
  unless exists $ createLocalDB dir0
  dir <- canonicalizePath dir0
  (e0, _, _) <- readProcessWithExitCode "pg_ctl" ["-D", dir, "status"] ""
  when (e0 /= ExitSuccess) $ do
    e1 <- rawSystem "pg_ctl" ["-D", dir, "start"]
    when (e1 /= ExitSuccess) $ fail "could not start postgres"
  return defaultConnectInfo { connectHost = dir }

-- | Stop a local database cluster entirely self-contained within one
-- directory.  You must call this before deleting the directory, or
-- else stray postgres processes will linger forever.  If the argument
-- is the empty string, looks for the database directory in the
-- @PGDATA@ environment variable.
stopLocalDB :: FilePath -> IO ()
stopLocalDB dir0 = do
  dir <- if not (null dir0) then return dir0 else do
    mpgd <- getEnv "PGDATA"
    case mpgd of Just pgd -> return pgd
                 _        -> fail "stopLocalDB: must specify database"
  e <- rawSystem "pg_ctl" ["-D", dir, "stop", "-m", "fast"]
  when (e /= ExitSuccess) $ fail "could not stop postgres"

-- | Shell commands you can execute to make @pg_ctl@ and @psql@ work
-- without additional arguments.
setLocalDB :: FilePath -> IO String
setLocalDB dir0 = do
  dir1 <- canonicalizePath dir0
  let dir = showCommandForUser dir1 []
  msh <- getEnv "SHELL"
  return $ case msh of Just sh | isSuffixOf "csh" sh ->
                         "setenv PGDATA " ++ dir ++ "; setenv PGHOST " ++ dir
                       _ -> "export PGDATA=" ++ dir ++ " PGHOST=" ++ dir
