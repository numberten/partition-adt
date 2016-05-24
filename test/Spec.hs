import Test.HUnit

import Partition

import qualified Data.Map.Lazy as M

main :: IO ()
main = runTestTT partitionADTTests >> return ()

partitionADTTests :: Test
partitionADTTests = TestList [
    makeTest ([] :: [Maybe (Maybe Bool)])  M.empty

  , makeTest ([Left 10, Right 'a', Right 'b', Left 42, Right 'c'] :: [Either Int Char])  $
      M.fromList [
                   ("Left", [Left 10, Left 42])
                 , ("Right",[Right 'a',Right 'b', Right 'c'])
                 ]
  ]
  where
    makeTest input output =
      TestCase $
        assertEqual ("for (partitionADT " ++ (show input) ++ "),")
                    (partitionADT input)
                    output
