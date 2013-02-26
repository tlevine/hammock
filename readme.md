Hammock
====

Hammock is a ReST server in Haskell. You specify table schemas and validation rules,
and the resulting endpoints are created.

Data are sent as JSON lists of dicts, at least for now.

Data are stored in the server in an SQLite database.

GET, POST, PUT and PATCH requests are supported for ordinary tables.

    GET   /table -> SELECT * FROM table
    POST  /table -> INSERT INTO table
    PUT   /table -> UPDATE table
    PATCH /table -> UPDATE table

GET requests are supported for SQLite views.
