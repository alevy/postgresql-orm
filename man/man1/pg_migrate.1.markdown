#NAME

pg\_migrate - PostgreSQL ORM migrations runner

#SYNOPSIS

    pg_migrate COMMAND [directory]

#DESCRIPTION

pg\_migrate is a utility for managing migrations on a PostgreSQL database. It
allows you to run migrations from a directory, only running those
not yet applied and rollback to previous versions. For example, in development,
use pg\_migrate to iterate on your database schema, and in production, use it
to retain a rollback path when pushing code that requires a new schema.

Migrations are executable Haskell source code files that run a single migration
and corresponding rollback. All migrations for a project should be stored in
the same directory. See pg\_migrate(5) for details on how
migrations are run.

##Migration File Names

Migrations files are underscore delimited and contain the version followed by a descriptive name:

      201304021245_create_users_table.hs

Versions are ordered lexically and can be any string not containing the characters '.' or '\_'. However, a common choice is the GMT time of creation formatted in decreasing order of significance: %Y%m%d%H%M.

##Database format

The only requirement on the database is that it contain a table schema\_migrations with a VARCHAR column version. running "_pg\_migrate init_" will create such a table, by running the following SQL:

      CREATE TABLE schema_migrations (
        version VARCHAR(28)
      );

When a migration is run, it's version is inserted into this table. When it is rolled back, the version is removed. Maintaining all migration versions in the database allows us to easily run only new migrations. 

#COMMANDS

##migrate [directory]

Run all migrations found in _directory_, starting with the oldest
migrations not yet commited to the database. If not specified,
_directory_ is "db/migrations".

##rollback [directory]

Rollback the newest migration in _directory_ committed to the database.
If not specified, _directory_ is "db/migrations".

##init

Prepare the database for using migrations by creating the "schema\_migrations"
table. This command will fail if such a table already exists without destroying
the existing table.

##dump [file]

Dump the current database schema to the _file_, or "db/schema.sql" if not
_file_ is specified. The current implementation simply runs pg\_dump(1):

    pg_dump --schema-only -O -x

#SEE ALSO

pg\_migrate(5), pg\_dump(1)

