{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
  describe "Homepage" $ do
    it "returns a JSON message" $ do
      get HomeR
      statusIs 200
      bodyContains "Welcome to Yesod"
