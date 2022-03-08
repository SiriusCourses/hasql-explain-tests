This library proves a test interface for [hasql](https://hackage.haskell.org/package/hasql) library.
Hasql is very performant Postgres client library for Haskell, that allows to write raw SQL queries.
Ability to write raw queries allows to use the full set of the database engine features and easy
debug and analysis using external tools.

However when writing raw queries it's very easy to make a mistake and this mistake
will not be checked by the compiler. This library tries to solve the problem by introducing
helpers to test queries.

