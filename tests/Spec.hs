module Main where

import Cli (moduleNameParser)
import Common (ModuleName (..))
import Data.Either (isLeft)
import Test.Hspec (context, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $
  describe "Spec" $ do
    context "moduleNameParser" $ do
      it "root level" $ moduleNameParser "T" `shouldBe` Right (ModuleName "T")
      it "second level" $ moduleNameParser "Main.T" `shouldBe` Right (ModuleName "Main.T")
      it "not matches root level" $ do
        let res = moduleNameParser "T2"
        isLeft res `shouldBe` True
      it "not matches second level" $ do
        let res = moduleNameParser "Main2.T"
        isLeft res `shouldBe` True
