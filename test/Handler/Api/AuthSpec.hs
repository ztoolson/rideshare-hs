{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Api.AuthSpec (spec) where

import Data.Aeson (encode, object, (.=))
import qualified Data.Text as T
import Handler.Api.Auth
import Test.HUnit (assertFailure)
import TestImport

spec :: Spec
spec = withApp $ do
    describe "postAuthSignupR" $ do
        it "successfully creates a new user" $ do
            let signupRequest =
                    object
                        [ "email" .= ("test@example.com" :: Text)
                        , "password" .= ("password123" :: Text)
                        , "firstName" .= ("John" :: Text)
                        , "lastName" .= ("Doe" :: Text)
                        , "type" .= ("Driver" :: Text)
                        , "driversLicenseNumber" .= ("DL123456" :: Text)
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
            currentTime <- liftIO getCurrentTime
            _ <-
                runDB $
                    insert $
                        User
                            { userEmail = "existing@example.com"
                            , userPasswordHash = "hashedpassword"
                            , userFirstName = "John"
                            , userLastName = "Doe"
                            , userType = "Driver"
                            , userCreatedAt = currentTime
                            , userUpdatedAt = currentTime
                            , userTripsCount = Nothing
                            , userDriversLicenseNumber = Nothing
                            }

            let duplicateRequest =
                    object
                        [ "email" .= ("existing@example.com" :: Text)
                        , "password" .= ("password123" :: Text)
                        , "firstName" .= ("John" :: Text)
                        , "lastName" .= ("Doe" :: Text)
                        , "type" .= ("Driver" :: Text)
                        , "driversLicenseNumber" .= ("DL123456" :: Text)
                        ]

            request $ do
                setMethod "POST"
                setUrl AuthSignupR
                setRequestBody $ encode duplicateRequest
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400
            bodyContains "Email already in use"

        it "returns errors for empty email and password" $ do
            let allInvalidRequest =
                    object
                        [ "email" .= ("" :: Text)
                        , "password" .= ("" :: Text)
                        , "firstName" .= ("John" :: Text)
                        , "lastName" .= ("Doe" :: Text)
                        , "type" .= ("Driver" :: Text)
                        , "driversLicenseNumber" .= ("DL123456" :: Text)
                        ]

            request $ do
                setMethod "POST"
                setUrl AuthSignupR
                setRequestBody $ encode allInvalidRequest
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400
            bodyContains "Email is required"
            bodyContains "Password is required"

        it "handles if not all fields are sent" $ do
            let allInvalidRequest =
                    object
                        [ "email" .= ("" :: Text)
                        , "password" .= ("" :: Text)
                        ]

            request $ do
                setMethod "POST"
                setUrl AuthSignupR
                setRequestBody $ encode allInvalidRequest
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400

        it "returns errors for all invalid fields" $ do
            let allInvalidRequest =
                    object
                        [ "email" .= ("invalid-email" :: Text)
                        , "password" .= ("short" :: Text)
                        , "firstName" .= ("" :: Text)
                        , "lastName" .= ("" :: Text)
                        , "type" .= ("" :: Text)
                        , "driversLicenseNumber" .= (T.replicate 101 "a" :: Text) -- 101 characters long
                        ]

            request $ do
                setMethod "POST"
                setUrl AuthSignupR
                setRequestBody $ encode allInvalidRequest
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400
            bodyContains "Invalid email format"
            bodyContains "Password must be at least 8 characters long"
            bodyContains "First name is required"
            bodyContains "Last name is required"
            bodyContains "Type is required"
            bodyContains "Driver's license number must be at most 100 characters"

    describe "postAuthLoginR" $ do
        it "successfully logs in an existing user" $ do
            let email = "login@example.com"
                password = "password123"
            mHashedPass <- liftIO $ hashPassword $ PlainTextPassword password
            case mHashedPass of
                Just (HashedPassword hashedPass) -> do
                    currentTime <- liftIO getCurrentTime
                    _ <-
                        runDB $
                            insert $
                                User
                                    { userEmail = email
                                    , userPasswordHash = hashedPass
                                    , userFirstName = "John"
                                    , userLastName = "Doe"
                                    , userType = "Driver"
                                    , userCreatedAt = currentTime
                                    , userUpdatedAt = currentTime
                                    , userTripsCount = Nothing
                                    , userDriversLicenseNumber = Nothing
                                    }
                    let loginRequest =
                            object
                                [ "email" .= email
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
                Nothing -> do
                    liftIO $ assertFailure "Failed to hash password"

        it "handles if not all fields are sent" $ do
            let allInvalidRequest =
                    object
                        [ "email" .= ("" :: Text)
                        ]

            request $ do
                setMethod "POST"
                setUrl AuthSignupR
                setRequestBody $ encode allInvalidRequest
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400

        it "returns an error for invalid credentials" $ do
            let loginRequest =
                    object
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
