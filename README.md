Helpers for deploying to Heroku

Currently exports one function, dbConnParams, which reads the DATABASE_URL environment variable and return an alist of connection parameters with the following keys: user, password, host, port, dbname
