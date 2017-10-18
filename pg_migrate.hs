import Database.PostgreSQL.Migrate
import System.Environment
import System.Exit
import System.FilePath
import System.IO

main :: IO ()
main = do
  args <- getArgs
  ec <- case args of
    "init":[] -> initializeDb >> return ExitSuccess
    "dump":[] -> withFile (defaultMigrationsDir </> ".." </> "schema.sql")
                  WriteMode $ dumpDb
    "dump":file:[] -> withFile file WriteMode $ dumpDb
    "migrate":[] -> runMigrationsForDir stdout defaultMigrationsDir
    "migrate":dir:[] -> runMigrationsForDir stdout dir
    "rollback":[] -> runRollbackForDir defaultMigrationsDir
    "rollback":dir:[] -> runRollbackForDir dir
    "new":name:[] -> newMigration name defaultMigrationsDir >>
                      return ExitSuccess
    "new":name:dir:[] -> newMigration name dir >> return ExitSuccess
    "compile":out:[] -> compileMigrationsForDir defaultMigrationsDir out
    "compile":out:dir:[] -> compileMigrationsForDir dir out
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " migrate|rollback [DIRECTORY]"
      putStrLn $ "       " ++ progName ++ " compile [PROGRAM] [DIRECTORY]"
      putStrLn $ "       " ++ progName ++ " init"
      putStrLn $ "       " ++ progName ++ " dump [FILE]"
      putStrLn $ "       " ++ progName ++ " new NAME [DIRECTORY]"
      return $ ExitFailure 1
  if ec == ExitSuccess then
    return ()
    else exitWith ec
