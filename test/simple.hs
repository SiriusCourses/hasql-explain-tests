module Main (main) where

import Data.Int
import Test.Tasty
import Test.Tasty.HUnit
import Test.Database.Hasql
import Hasql.Connection as HC
import Hasql.Decoders as HD
import Hasql.Encoders as HE
import Hasql.Statement as HST

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
  ]

select1 :: HST.Statement () Int64
select1 = HST.Statement sql encoder decoder False where
  sql = "select 1::int8"
  encoder = HE.noParams
  decoder = HD.singleRow $ HD.column $ HD.nonNullable HD.int8
