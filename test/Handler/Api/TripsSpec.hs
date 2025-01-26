{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Api.TripsSpec (spec) where

import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (addUTCTime)
import Database.Persist.Sql (fromSqlKey)
import Handler.Api.Auth (generateJWT)
import TestImport hiding (decodeUtf8, toStrict, unpack, (.))

createAuthToken :: Int64 -> Handler Text
createAuthToken userId = do
    time <- liftIO getCurrentTime
    let expTime = addUTCTime (24 * 3600) time -- token expires in 24 hours
    generateJWT userId expTime

spec :: Spec
spec = withApp $ do
    describe "GET /api/trips/my" $ do
        it "returns a list of completed trips for the authenticated user" $ do
            time <- liftIO getCurrentTime

            -- Insert rider and driver directly into database
            riderId <-
                runDB $
                    createUser
                        "rider@example.com"
                        "hashedpass"
                        "TestFirstName"
                        "RiderLastName"
                        "rider"
                        time
                        Nothing

            driverId <-
                runDB $
                    createUser
                        "driver@example.com"
                        "hashedpass"
                        "TestFirstname"
                        "DriverLastName"
                        "driver"
                        time
                        (Just "DL123456")

            -- Insert start and end locations
            startLocId <-
                runDB $
                    createLocation
                        "123 Start St"
                        "UnknownCity"
                        "UnknownState"
                        (Point 0 0)
                        time

            endLocId <-
                runDB $
                    createLocation
                        "456 End Ave"
                        "UnknownCity"
                        "UnknownState"
                        (Point 0 0)
                        time

            -- Create trip request
            tripRequestId <-
                runDB $
                    createTripRequest
                        riderId
                        startLocId
                        endLocId
                        time

            -- Create completed trip
            tripId <-
                runDB $
                    createTrip
                        tripRequestId
                        driverId
                        (Just time)
                        Nothing
                        time

            -- Generate auth token for rider
            token <- runHandler $ createAuthToken (fromSqlKey riderId)

            -- Make request
            request $ do
                setMethod "GET"
                setUrl MyTripsR
                addRequestHeader ("Authorization", "Bearer " <> encodeUtf8 token)

            -- Verify response
            statusIs 200

            let expectedResponse =
                    object
                        [ "trips"
                            .= [ object
                                    [ "driver"
                                        .= object
                                            [ "createdAt" .= time
                                            , "driversLicenseNumber" .= ("DL123456" :: Text)
                                            , "email" .= ("driver@example.com" :: Text)
                                            , "firstName" .= ("TestFirstname" :: Text)
                                            , "id" .= fromSqlKey driverId
                                            , "lastName" .= ("DriverLastName" :: Text)
                                            , "tripsCount" .= Null
                                            , "type" .= ("driver" :: Text)
                                            , "updatedAt" .= time
                                            ]
                                    , "request"
                                        .= object
                                            [ "createdAt" .= time
                                            , "endLocation" .= fromSqlKey endLocId
                                            , "id" .= fromSqlKey tripRequestId
                                            , "rider" .= fromSqlKey riderId
                                            , "startLocation" .= fromSqlKey startLocId
                                            , "updatedAt" .= time
                                            ]
                                    , "rider"
                                        .= object
                                            [ "createdAt" .= time
                                            , "driversLicenseNumber" .= Null
                                            , "email" .= ("rider@example.com" :: Text)
                                            , "firstName" .= ("TestFirstName" :: Text)
                                            , "id" .= fromSqlKey riderId
                                            , "lastName" .= ("RiderLastName" :: Text)
                                            , "tripsCount" .= Null
                                            , "type" .= ("rider" :: Text)
                                            , "updatedAt" .= time
                                            ]
                                    , "trip"
                                        .= object
                                            [ "completedAt" .= time
                                            , "createdAt" .= time
                                            , "driver" .= fromSqlKey driverId
                                            , "id" .= fromSqlKey tripId
                                            , "rating" .= Null
                                            , "tripRequest" .= fromSqlKey tripRequestId
                                            , "updatedAt" .= time
                                            ]
                                    ]
                               ]
                        ]
            bodyEquals $ (unpack . decodeUtf8 . toStrict . encode) expectedResponse

        it "returns an empty list when no completed trips exist for the user" $ do
            time <- liftIO getCurrentTime

            -- Create a rider
            riderId <-
                runDB $
                    createUser
                        "rider@example.com"
                        "hashedpass"
                        "TestFirstName"
                        "RiderLastName"
                        "rider"
                        time
                        Nothing

            -- Generate auth token for rider
            token <- runHandler $ createAuthToken (fromSqlKey riderId)

            -- Make request
            request $ do
                setMethod "GET"
                setUrl MyTripsR
                addRequestHeader ("Authorization", "Bearer " <> encodeUtf8 token)

            -- Verify response
            statusIs 200

            let expectedResponse = object ["trips" .= ([] :: [Value])]
            bodyEquals $ (unpack . decodeUtf8 . toStrict . encode) expectedResponse
