module TypecheckerSpec where

import Test.Hspec

import TestUtils

spec :: Spec
spec = generalTest

generalTest :: Spec
generalTest = describe "typechecker" $
  it "checksPrograms" $ do
    putStrLn "XD"
