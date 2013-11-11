{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $
  error "Not implemented"

down :: Connection -> IO ()
down = migrate $ do
  error "Not implemented"

main :: IO ()
main = defaultMain up down

