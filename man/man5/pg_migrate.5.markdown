#NAME

PostgreSQL ORM migration

#SYNOPSIS

    201305041232_migration_file.hs COMMAND [--with-db-commit]

#DESCRIPTION

A Migrations is an executable Haskell source file (e.g. a Haskell source file
with a "`main :: IO ()`" function, runnable by `runghc`). It must respond to the
commands "up" and "down" by running a migration and rollback, respectively.
Unless the "--with-db-commit" flag is set, the result of the migration or
rollback should not be committed to the database.

While you can implement a migration however you like, the module
`Database.PostgreSQL.Migrations` in the `postgresql-orm` package defines a
function called `defaultMain` that implements most of the boiler plate
functionality of a migration. You need only implement the actual migration and
rollback code.

#COMMANDS

##up

Upgrade the database by running the migration

##down

Downgrade the database by rolling back the migration

#FLAGS

##--with-db-commit

MUST be included in order to actually commit the migration or rollback to the
database. If this argument is not included, a test run is implied, and the
migration should affect the database, probably by rolling back the transaction
before exiting.

#SEE ALSO

pg\_migrate(1), runghc(1)

