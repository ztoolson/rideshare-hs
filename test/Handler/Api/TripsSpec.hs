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
                    insert $
                        User
                            { userEmail = "rider@example.com"
                            , userPasswordHash = "hashedpass"
                            , userFirstName = "Test"
                            , userLastName = "Rider"
                            , userType = "rider"
                            , userCreatedAt = time
                            , userUpdatedAt = time
                            , userTripsCount = Nothing
                            , userDriversLicenseNumber = Nothing
                            }

            driverId <-
                runDB $
                    insert $
                        User
                            { userEmail = "driver@example.com"
                            , userPasswordHash = "hashedpass"
                            , userFirstName = "Test"
                            , userLastName = "Driver"
                            , userType = "driver"
                            , userCreatedAt = time
                            , userUpdatedAt = time
                            , userTripsCount = Nothing
                            , userDriversLicenseNumber = Just "DL123456"
                            }
            -- Insert start and end locations
            startLocId <-
                runDB $
                    insert $
                        Location
                            { locationAddress = "123 Start St"
                            , locationCity = "Unknown"
                            , locationState = "Unknown"
                            , locationPosition = Point 0 0
                            , locationCreatedAt = time
                            , locationUpdatedAt = time
                            }

            endLocId <-
                runDB $
                    insert $
                        Location
                            { locationAddress = "456 End Ave"
                            , locationCity = "Unknown"
                            , locationState = "Unknown"
                            , locationPosition = Point 0 0
                            , locationCreatedAt = time
                            , locationUpdatedAt = time
                            }

            -- Create trip request
            tripRequestId <-
                runDB $
                    insert $
                        TripRequest
                            { tripRequestRider = riderId
                            , tripRequestStartLocation = startLocId
                            , tripRequestEndLocation = endLocId
                            , tripRequestCreatedAt = time
                            , tripRequestUpdatedAt = time
                            }

            -- Create completed trip
            tripId <-
                runDB $
                    insert $
                        Trip
                            { tripTripRequest = tripRequestId
                            , tripDriver = driverId
                            , tripCompletedAt = Just time
                            , tripRating = Nothing
                            , tripCreatedAt = time
                            , tripUpdatedAt = time
                            }

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
                                            , "firstName" .= ("Test" :: Text)
                                            , "id" .= fromSqlKey driverId
                                            , "lastName" .= ("Driver" :: Text)
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
                                            , "firstName" .= ("Test" :: Text)
                                            , "id" .= fromSqlKey riderId
                                            , "lastName" .= ("Rider" :: Text)
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
                    insert $
                        User
                            { userEmail = "rider@example.com"
                            , userPasswordHash = "hashedpass"
                            , userFirstName = "Test"
                            , userLastName = "Rider"
                            , userType = "rider"
                            , userCreatedAt = time
                            , userUpdatedAt = time
                            , userTripsCount = Nothing
                            , userDriversLicenseNumber = Nothing
                            }

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