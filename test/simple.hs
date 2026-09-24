module Main (main) where

import Control.Exception
import Data.IORef
import Data.Int
import qualified Data.Text as Text
import Hasql.Connection as HC
import Hasql.Decoders as HD
import Hasql.Encoders as HE
import qualified Hasql.Session as HS
import Hasql.Statement as HST
import System.Directory
import Test.Database.Hasql
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  withResource (startupPostgres init_script) (teardownPostgres) $ \mkDb ->
    withResource (mkDb >>= allocateConnection) (freeConnection) $ \conn ->
      tests conn
  where
    init_script = "create table a (id int8 not null generated always as identity  primary key)"

tests :: IO HC.Connection -> TestTree
tests mkConn = testGroup "explain-tests"
  [ testCase "select 1" $ mkConn >>= explain select1
  , testCase "select 1 with retry" $ mkConn >>= explainWithRetry select1
  , testCase "tears down postgres when initialization fails" $ do
      dataDirectoryRef <- newIORef Nothing
      result <- try (do
        db <- startupPostgresInit $ \connection -> do
          queryResult <- HS.run (HS.statement () dataDirectory) connection
          case queryResult of
            Left err -> assertFailure $ show err
            Right path -> do
              writeIORef dataDirectoryRef $ Just $ Text.unpack path
              throwIO ExpectedInitializationFailure
        teardownPostgres db)
        :: IO (Either ExpectedInitializationFailure ())
      case result of
        Left ExpectedInitializationFailure -> pure ()
        Right () -> assertFailure "initialization unexpectedly succeeded"
      dataDirectory <- readIORef dataDirectoryRef >>=
        maybe (assertFailure "initialization did not record the data directory") pure
      exists <- doesDirectoryExist dataDirectory
      assertBool "temporary postgres data directory still exists" $ not exists
  ]

data ExpectedInitializationFailure = ExpectedInitializationFailure
  deriving Show

instance Exception ExpectedInitializationFailure

dataDirectory :: HST.Statement () Text.Text
dataDirectory =
  HST.Statement "show data_directory" HE.noParams decoder True
  where
    decoder = HD.singleRow $ HD.column $ HD.nonNullable HD.text

select1 :: HST.Statement () Int64
select1 = HST.Statement sql encoder decoder False where
  sql = "select 1::int8"
  encoder = HE.noParams
  decoder = HD.singleRow $ HD.column $ HD.nonNullable HD.int8
