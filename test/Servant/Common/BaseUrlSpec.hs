{-# LANGUAGE OverloadedStrings #-}

module Servant.Common.BaseUrlSpec
  ( spec
  ) where

import           Data.Aeson
import           Test.Hspec

import           Servant.Common.BaseUrl

spec :: Spec
spec =
    describe "BaseUrl.fromJSON" $ do
      it "parses valid local dev server url" $
        eitherDecode "\"http://127.0.0.1:8080/api\"" `shouldBe`
          Right (BaseFullUrl Http "127.0.0.1" 8080 "api")
      it "parses https url" $
        eitherDecode "\"https://127.0.0.1:8080/api\"" `shouldBe`
          Right (BaseFullUrl Https "127.0.0.1" 8080 "api")
      it "parses BasePath" $
        eitherDecode "\"/api\"" `shouldBe`
          Right (BasePath "/api")
