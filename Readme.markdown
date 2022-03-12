This library proves a test interface for [hasql](https://hackage.haskell.org/package/hasql) library.
Hasql is very performant Postgres client library for Haskell, that allows to write raw SQL queries.
Ability to write raw queries allows to use the full set of the database engine features and easy
debug and analysis using external tools. 

However when writing raw queries it's very easy to make a mistake and this mistake
will not be checked by the compiler. This library tries to solve the problem by introducing
helpers to test queries.

# Features

The library provides some basic tests of the database queries:

   * `explain tests` - tests that checks that we can call explain on the query
        this test will guarantee that the database can parse the query and run
        it in the current schema. This test also automatically checks parameters
        encoding.

# How to use the library.

At this moment there is no automation that would allow to gather all the queries across the
project. So usually the simplest way is to export all the queries from the module:

```haskell
module A 
  ( ...
  , queries
  ) where


queries :: (String, SomeQuery)
queries =
  [ "queryName" =>> queryName 
  , "anotherQuery" =>> anotherQuery
  ]
```

And use that function in the test suite:

```haskell
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import A qualified

main = defaultMain $
  withResource (startupPostgres) (teardownPostgres) $ mkDb ->
    withResource (mkDb >>= allocateConnection) (freeConnection) $ conn ->
      tests conn

tests :: IO HC.Connection -> TestGroup
tests mkConn = testGroup "explain-tests" $
  [ testGroup "A" $ A.queries <&> \(name, SomeQuery q) ->
      testCase name $ mkConn >>= explain q
  ]
```

## Using the library with Nix

There are a plenty of the approaches that can allow to use the library with
nix, but we prefer the following:
  
  1. Add the package to the `haskellPackages` set by any means
  2. Write a wrapper that will add a dependency on the Postgres server of
     the required version:

    ```nix
    { hpkgs   # haskell packages set
    , haskell # haskell lib
    , postgresql_11 # postgres version
    }:
    
    haskell.lib.overrideCabal hpkgs.db-tests (drv: {
      # Add postgres server to the tests dependencies:
      testDepends = (drv.testDepends or []) ++ [postgresql_11];
      # have streaming logs, very useful for CI
      testTarget = "--show-details=streaming";
      doCheck = true;

      # there can be any other modifications:
      #
      # enableSharedExecutables = false;
      # enableSeparateDataOutput = true;
      # enableSeparateDocOutput = true;
      # enableLibraryProfiling = false;
      # isLibrary = true;
      # doHaddock = false;
    })
   ```


# Tricks

1. In order to substitute the parameters we require all the parameters to have
`Arbitrary` instance, however it's not always possible as not all parameters may
have a sane instance. In such cases it worth mapping the query input. It can be
done by applying `lmap` from `profunctors` package:

