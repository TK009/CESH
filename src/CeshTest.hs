module Main (main) where
import Test.HUnit

import Types

tests, testMaxIndex, testIndex :: Test
testMaxIndex = TestCase $ assertEqual "max tag index" 7 tagIndexMax
testIndex    = TestCase $ assertEqual "max tag index" 2 (tagIndex DynamicBody)

tests = TestList [ TestLabel "maxTagIndex" testMaxIndex
                 , TestLabel "tagIndex" testIndex
                 ]

main :: IO ()
main = do 
    _ <- runTestTT $ tests
    return ()

