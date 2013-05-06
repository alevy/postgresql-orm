
-- | Functions for initializing self-contained local postgreSQL
-- database clusters (useful in development more than production).
module Database.PostgreSQL.Devel (
    initLocalDB, stopLocalDB, setLocalDB, withTempDB
  ) where

import Control.Exception
import Control.Monad
import Data.Functor
import Data.List
import Database.PostgreSQL.Simple
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Env
import System.Posix.Temp
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
    ("unix_socket_directory", "unix_socket_directory = '" ++ q dir ++ "'")
  , ("logging_collector",  "logging_collector = yes")
  , ("listen_addresses", "listen_addresses = ''")]
  where q ('\'':t) = "''" ++ q t
        q (h:t)    = h : q t
        q []       = ""

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

systemNoStdout :: String -> [String] -> IO ExitCode
systemNoStdout prog args =
  bracket (openFile "/dev/null" ReadWriteMode) hClose $ \devnull -> do
    let cp = (proc prog args) { std_in = UseHandle devnull
                              , std_out = UseHandle devnull }
    (_,_,_,pid) <- createProcess cp
    waitForProcess pid

-- | Initialize a local database cluster entirely self-contained
-- within one directory.  If a database already exists in the
-- directory, then start the postgres server if necessary.  Either way
-- returns a 'ConnectInfo' that will connect to this local database.
--
-- Note that if @initLocalDB@ starts a postgres server, the server
-- process will continue running after the process exits.  This is
-- normally fine.  Since multiple client processes may access the same
-- postgres database, it makes sense for the first client to start the
-- database and no one to stop it.  See 'stopLocalDB' if you wish to
-- stop the server process (which you should always do before deleting
-- a test cluster).  See also 'withTempDB' to create a temporary
-- cluster for a test suite.
initLocalDB :: FilePath -> IO ConnectInfo
initLocalDB dir0 = do
  exists <- isNonEmptyDir dir0
  unless exists $ createLocalDB dir0
  dir <- canonicalizePath dir0
  (e0, _, _) <- readProcessWithExitCode "pg_ctl" ["status", "-D", dir] ""
  when (e0 /= ExitSuccess) $ do
    e1 <- systemNoStdout "pg_ctl" ["start", "-w", "-D", dir]
    when (e1 /= ExitSuccess) $ fail "could not start postgres"
  return defaultConnectInfo { connectHost = dir
                            , connectUser = ""
                            , connectDatabase = "postgres"
                            }

-- | Stop the server for a local database cluster entirely
-- self-contained within one directory.  You must call this before
-- deleting the directory, or else stray postgres processes will
-- linger forever.  If the argument is the empty string, looks for the
-- database directory in the @PGDATA@ environment variable.
stopLocalDB :: FilePath -> IO ()
stopLocalDB dir0 = do
  dir <- if not (null dir0) then return dir0 else do
    mpgd <- getEnv "PGDATA"
    case mpgd of Just pgd -> return pgd
                 _        -> fail "stopLocalDB: must specify database"
  e <- systemNoStdout "pg_ctl" ["stop", "-D", dir, "-m", "fast"]
  when (e /= ExitSuccess) $ fail "could not stop postgres"

-- | Set environment variables to make a local database cluster the
-- default.  Also returns shell commands you can eval or cut-and-paste
-- into your shell to make @pg_ctl@ and @psql@ access a local database
-- cluster.
setLocalDB :: FilePath -> IO String
setLocalDB dir0 = do
  dir1 <- canonicalizePath dir0
  setEnv "PGHOST" dir1 True
  setEnv "PGDATA" dir1 True
  setEnv "PGDATABASE" "postgres" True
  let dir = showCommandForUser dir1 []
  msh <- getEnv "SHELL"
  return $ case msh of Just sh | isSuffixOf "csh" sh ->
                         "setenv PGDATA " ++ dir ++ "; setenv PGHOST " ++ dir
                       _ -> "export PGDATA=" ++ dir ++ " PGHOST=" ++ dir

-- | Run a function with a completely fresh database cluster that gets
-- deleted on return.  Since the entire database is blown away when
-- the function returns, @withTempDB@ is obviously only useful for
-- test suites.
withTempDB :: (ConnectInfo -> IO a) -> IO a
withTempDB f = bracket createdir removeDirectoryRecursive $ \d ->
  (initLocalDB d >>= f) `finally` stopLocalDB d
  where createdir = do
          tmp <- getTemporaryDirectory
          mkdtemp $ tmp </> "db."

