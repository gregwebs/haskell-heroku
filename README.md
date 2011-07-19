Helpers for deploying to Heroku

Currently exports two functions.
parseDatabaseUrl return an alist of connection parameters with the following keys: user, password, host, dbname
dbConnParams first reads the DATABASE_URL environment variable and then calls parseDatabaseUrl
