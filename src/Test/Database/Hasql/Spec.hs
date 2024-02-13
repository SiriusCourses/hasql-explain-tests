module Test.Database.Hasql.Spec
  ( explainTestsSpec
  , buildExplainTestsSpec
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.IORef
import Data.Foldable
import Data.Pool
import Data.Profunctor
import Data.Typeable
import Database.Postgres.Temp qualified as Temp
import Hasql.Connection qualified       as HC
import Hasql.Decoders qualified         as HD
import Hasql.Session qualified          as HS
import Hasql.Statement qualified        as HST
import System.IO.Unsafe
import Test.Database.Hasql
import Test.QuickCheck
import Test.Hspec


type ModuleName = String
type QueryName  = String

-- | Build spec to run explain tests for queries
explainTestsSpec
  :: Arbitrary input
  => IO () -- ^ Action to run before explain tests.
  -> IO () -- ^ Action to run after  explain tests.
  -> (ActionWith HC.Connection -> IO ()) -- ^ Action to run around explain tests using db connection.
  -> [(ModuleName, [(QueryName, HST.Statement input output)])] -- ^ Queries to test
  -> Spec
explainTestsSpec run_before_all run_after_all run_around queries =
  beforeAll_ run_before_all
    $ afterAll_ run_after_all
    $ around run_around
       $ describe "Database queries"
       $ for_ queries
       $ \(module_name, module_queries) ->
          context module_name
            $ for_ module_queries
            $ \(query_name, query) -> it query_name $ explain query

-- | Build spec to run explain tests for queries.
-- Initialize temprorary database, using it for tests and teardown it after tests
buildExplainTestsSpec
  :: Arbitrary input
  => [(ModuleName, [(QueryName, HST.Statement input output)])] -- ^ Queries to test
  -> (Pool HC.Connection -> IO ()) -- ^ Action to run database migrations
  -> Spec
buildExplainTestsSpec queries run_db_migrations =
  explainTestsSpec
    (startupPostgresInit initializeDb >>= writeIORef db_ref . Just) -- ^ Initialize db before explain tests
    (readIORef db_ref >>= maybe (pure ()) teardownPostgres)         -- ^ Teardown   db after  explain tests
    setupConnection                                                 -- ^ Setup db connection around explain tests
    queries
  where
    initializeDb conn = do
      pool <- newPool $ setNumStripes (Just 1) $ defaultPoolConfig (pure conn) (HC.release) 10 10
      run_db_migrations pool
    setupConnection :: ActionWith HC.Connection -> IO ()
    setupConnection = bracket prepareConnection freeConnection where
      prepareConnection = do
        readIORef db_ref >>= \case
          Nothing -> error "No connection"
          Just db -> allocateConnection db
    db_ref :: IORef (Maybe Temp.DB)
    db_ref = unsafePerformIO $ newIORef Nothing