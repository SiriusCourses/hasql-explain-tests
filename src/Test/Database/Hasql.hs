-- |
-- An interface for testing hasql queries. As @hasql@ is pretty low level
-- library and does not provide additional checks in compile time we are 
-- intered if the queries are well formed from the database point of view.
-- 
-- This library provides a number of tests that helps to check that the
-- basic properties are held. In order to run the tests the library provides
-- helpers for running the temporary database.
--
module Test.Database.Hasql
  ( -- * Running tests
    -- $running-tests
    startupPostgres
  , teardownPostgres 
  , allocateConnection
  , freeConnection
  , InitException(..)
    -- * Explain tests
    -- $explain-tests
  , explain
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Profunctor
import Data.Typeable
import Database.Postgres.Temp qualified as Temp
import Hasql.Connection qualified       as HC
import Hasql.Decoders qualified         as HD
import Hasql.Session qualified          as HS
import Hasql.Statement qualified        as HST
import Test.QuickCheck
import Test.Hspec

-- $explain-tests
--
-- Explain tests are the tests that are based on the idea to run
-- @explain@ on the query. It means that if we have some SQL query we run
-- @EXPLAIN $SQL@ and provide some variables. Then we check if this query
-- succeeds.
--
-- If this test passes it guarantees:
--
--   1. that the query is well formed and that encoders works.
--
-- However it does not check:
--
--   1. If encoder works
--   2. Complexity of the query
--   3. Locks that the query holds

-- | Runs explain test.
--
-- __Note__ In order to run the query we need to substitute parameters,
-- we chose to pass an arbitrary value (using quickcheck), however some values may
-- miss the arbitrary instance, in such cases one can use 'lmap' to map values from
-- the ones that have this interface. I.e.
--
-- @
-- explain (lmap (\() -> constValue) query)
-- @
-- 
explain
  :: (Arbitrary input)
  => HST.Statement input output -- ^ Original statement
  -> HC.Connection -- ^ Connection to the database
  -> Expectation 
explain t c = do
  let t' = case t of
             HST.Statement sql enc _dec _ -> HST.Statement ("EXPLAIN " <> sql) enc HD.noResult False
  input <- liftIO $ generate $ resize 2 $ arbitrary
  HS.run (HS.statement input t') c `shouldReturn` Right ()

-- $running-tests
--
-- The library if the test-framework agnostic so it provides only the basic
-- commands that can be used in order to run the tests using diferrent frameworks.
--
-- For example using tasty + tasty-hunit one can do:
-- 
-- @
-- import Test.Tasty
-- import Test.Tasty.HUnit
--
-- main = defaultMain $
--   withResource (startupPostgres) (teardownPostgres) $ \mkDb ->
--     withResource (mkDb >>= allocateConnection) (freeConnection) $ \conn ->
--       tests conn
--
-- tests :: IO HC.Connection -> TestGroup
-- tests mkConn = testGroup "explain-tests"
--   [ testCase "select 1" $ mkConn >>= explain select1
--   ]
-- @

-- | Possible exceptions that may happen during the initialization process
data InitException
  = InitException HS.QueryError
    -- ^ Exception during running of the initialization script
  | ConnectException HC.ConnectionError
    -- ^ Can't allocate connection to the local db
  | PostgresStartException Temp.StartError
    -- ^ We have failed to start temporary postgres.
  deriving (Show, Typeable)

instance Exception InitException

-- | Start and initialize temporary database.
--
-- Accepts database initialization script that can contain multiple commands
-- and is run in the separate transaction.
--
-- @throws: In case if the database initialization fails throws 'InitException'.
startupPostgres :: ByteString -> IO Temp.DB
startupPostgres init_script = do
  Temp.start >>= \case
    Left e -> throwIO $ PostgresStartException e
    Right db -> do
      c <- HC.acquire (Temp.toConnectionString db) >>= \case
             Left e -> throwIO $ ConnectException e
             Right c -> pure c
      HS.run (HS.sql init_script ) c >>= \case
        Right {} -> pure db
        Left e -> throwIO $ InitException e

-- | Teardown database and associated resources
teardownPostgres :: Temp.DB -> IO ()
teardownPostgres = Temp.stop

-- | Allocates connection to the temporary database
allocateConnection :: Temp.DB -> IO HC.Connection
allocateConnection db = HC.acquire (Temp.toConnectionString db) >>= \case
  Left e -> throwIO $ ConnectException e
  Right conn -> pure conn

-- | Frees connection to the temporary database
freeConnection :: HC.Connection -> IO ()
freeConnection = HC.release
