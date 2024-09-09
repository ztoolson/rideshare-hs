{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
 
module Handler.Api.HelloWorldSpec (spec) where
 
import TestImport
import Data.Time (addUTCTime)
import Handler.Api.Auth (generateJWT)
import qualified Data.Text.Encoding as TE

 
spec :: Spec
spec = withApp $ do
    describe "getHelloWorldR" $ do
        it "responds with 401 when no token is provided" $ do
            get HelloWorldR
            statusIs 401

        it "returns 401 with an invalid token" $ do
            request $ do
                setMethod "GET"
                setUrl HelloWorldR
                addRequestHeader ("Authorization", "Bearer invalidtoken")
            statusIs 401

        it "responds with 200 and 'Hello World' when a valid token is provided" $ do
            -- TODO generate a jwt
            -- use the JWT in the HelloWorldR get request
            -- validate 200 and conains body "Hello World"
                    -- Generate a JWT
            time <- liftIO getCurrentTime
            let userId = 1 :: Int64
                expTime = addUTCTime (24 * 3600) time -- token expires in 24 hours
            jwt <- runHandler $ generateJWT userId expTime

            -- Use the JWT in the HelloWorldR get request
            request $ do
                setMethod "GET"
                setUrl HelloWorldR
                addRequestHeader ("Authorization", "Bearer " <> TE.encodeUtf8 jwt)

            -- Validate 200 and contains body "Hello World"
            statusIs 200
            bodyContains "Hello world!"