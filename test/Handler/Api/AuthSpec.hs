{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Api.AuthSpec (spec) where

import TestImport
import Handler.Api.Auth
import Data.Aeson (object, (.=), encode)
import Database.Persist.Sql (toSqlKey)

spec :: Spec
spec = withApp $ do
    describe "postAuthSignupR" $ do
        it "successfully creates a new user" $ do
            let signupRequest = object
                    [ "email" .= ("test@example.com" :: Text)
                    , "password" .= ("password123" :: Text)
                    ]

            request $ do
                setMethod "POST"
                setUrl AuthSignupR
                setRequestBody $ encode signupRequest
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200
            bodyContains "token"
            bodyContains "userId"

        it "returns an error for duplicate email" $ do
            _ <- runDB $ insert $ User "existing@example.com" "hashedpassword"

            let duplicateRequest = object
                    [ "email" .= ("existing@example.com" :: Text)
                    , "password" .= ("password123" :: Text)
                    ]

            request $ do
                setMethod "POST"
                setUrl AuthSignupR
                setRequestBody $ encode duplicateRequest
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400
            bodyContains "Email already in use"

    describe "postAuthLoginR" $ do
        it "successfully logs in an existing user" $ do
            let email = "login@example.com"
                password = "password123"
            mHashedPass <- liftIO $ hashPassword $ PlainTextPassword password
            case mHashedPass of
                Just (HashedPassword hashedPass) -> do
                    _ <- runDB $ insert $ User email hashedPass
                    let loginRequest = object
                            ["email" .= email
                            , "password" .= password
                            ]
                    request $ do
                        setMethod "POST"
                        setUrl AuthLoginR
                        setRequestBody $ encode loginRequest
                        addRequestHeader ("Content-Type", "application/json")

                    statusIs 200
                    bodyContains "token"
                    bodyContains "userId"

        it "returns an error for invalid credentials" $ do
            let loginRequest = object
                    [ "email" .= ("nonexistent@example.com" :: Text)
                    , "password" .= ("wrongpassword" :: Text)
                    ]

            request $ do
                setMethod "POST"
                setUrl AuthLoginR
                setRequestBody $ encode loginRequest
                addRequestHeader ("Content-Type", "application/json")

            statusIs 401
            bodyContains "Invalid email or password"
