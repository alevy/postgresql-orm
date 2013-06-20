import Database.PostgreSQL.Migrate
import System.Environment
import System.Exit 
import System.IO

main :: IO ()
main = do
  args <- getArgs
  ec <- case args of
    "init":[] -> initializeDb
    "dump":[] -> dumpDb stdout
    "migrate":[] -> runMigrationsForDir defaultMigrationsDir
    "migrate":dir:[] -> runMigrationsForDir dir
    "rollback":[] -> runRollbackForDir defaultMigrationsDir
    "rollback":dir:[] -> runRollbackForDir dir
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " migrate|rollback [DIRECTORY]"
      putStrLn $ "       " ++ progName ++ " init"
      putStrLn $ "       " ++ progName ++ " dump"
      return $ ExitFailure 1
  if ec == ExitSuccess then
    return ()
    else exitWith ec

