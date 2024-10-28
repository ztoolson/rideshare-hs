{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Api.TripRequestsSpec (spec) where

import TestImport hiding (toStrict, decodeUtf8, unpack, (.))
import Test.HUnit (assertFailure)
import Data.Aeson (object, (.=), encode)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Data.Time.Clock (addUTCTime)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack)
import Data.ByteString.Lazy (toStrict)
import Handler.Api.Auth (generateJWT)

createAuthToken :: Int64 -> Handler Text
createAuthToken userId = do
    time <- liftIO getCurrentTime
    let expTime = addUTCTime (24 * 3600) time  -- token expires in 24 hours
    generateJWT userId expTime

spec :: Spec
spec = withApp $ do
    describe "POST /api/trip_requests" $ do
        it "successfully creates a trip request for authenticated rider" $ do
            time <- liftIO getCurrentTime
            
            -- Insert rider and driver directly into database
            riderId <- runDB $ insert $ User
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
                
            driverId <- runDB $ insert $ User
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

            -- Generate auth token for rider
            token <- runHandler $ createAuthToken (fromSqlKey riderId)

            -- Create trip request payload
            let tripRequestPayload = object
                    [ "riderId" .= riderId
                    , "startAddress" .= ("123 Start St" :: Text)
                    , "endAddress" .= ("456 End Ave" :: Text)
                    ]

            -- Perform trip request
            request $ do
                setMethod "POST"
                setUrl CreateTripRequestsR
                setRequestBody $ encode tripRequestPayload
                addRequestHeader ("Content-Type", "application/json")
                addRequestHeader ("Authorization", "Bearer " <> encodeUtf8 token)

            -- Verify response status
            statusIs 201

            -- Verify trip request was created in database
            maybeTrip <- runDB $ selectFirst [TripRequestRider ==. riderId] []
            liftIO $ case maybeTrip of
                Nothing -> assertFailure "Trip request not found in database"
                Just (Entity _ tripRequest) -> do
                    tripRequestRider tripRequest `shouldBe` riderId
 
            -- Verify locations were created
            maybeStartLoc <- runDB $ selectFirst [LocationAddress ==. "123 Start St"] []
            liftIO $ case maybeStartLoc of
                Nothing -> assertFailure "Start location not found in database"
                Just _ -> pure ()

            maybeEndLoc <- runDB $ selectFirst [LocationAddress ==. "456 End Ave"] []
            liftIO $ case maybeEndLoc of
                Nothing -> assertFailure "End location not found in database"
                Just _ -> pure ()
 
            -- Verify trip was created and assigned to a driver
            maybeTripEntity <- runDB $ do
                tripReq <- selectFirst [] [Desc TripRequestId]
                case tripReq of
                    Nothing -> pure Nothing
                    Just (Entity tripReqId _) -> selectFirst [TripTripRequest ==. tripReqId] []

            liftIO $ case maybeTripEntity of
                Nothing -> assertFailure "Trip not created in database"
                Just (Entity _ trip) -> do
                    tripDriver trip `shouldBe` driverId
                    isNothing (tripCompletedAt trip) `shouldBe` True
                    isNothing (tripRating trip) `shouldBe` True

    describe "GET /api/trip-requests/:id" $ do
        it "returns trip request and trip details when trip request exists" $ do
            time <- liftIO getCurrentTime
            
            -- Create a rider
            riderId <- runDB $ insert $ User
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

            -- Create start and end locations
            startLocId <- runDB $ insert $ Location
                { locationAddress = "123 Start St"
                , locationCity = "Unknown"
                , locationState = "Unknown"
                , locationPosition = Point 0 0
                , locationCreatedAt = time
                , locationUpdatedAt = time
                }

            endLocId <- runDB $ insert $ Location
                { locationAddress = "456 End Ave"
                , locationCity = "Unknown"
                , locationState = "Unknown"
                , locationPosition = Point 0 0
                , locationCreatedAt = time
                , locationUpdatedAt = time
                }

            -- Create trip request
            tripRequestId <- runDB $ insert $ TripRequest
                { tripRequestRider = riderId
                , tripRequestStartLocation = startLocId
                , tripRequestEndLocation = endLocId
                , tripRequestCreatedAt = time
                , tripRequestUpdatedAt = time
                }

            -- Create associated trip
            tripId <- runDB $ insert $ Trip
                { tripTripRequest = tripRequestId
                , tripDriver = riderId  -- Using rider as driver for simplicity
                , tripCompletedAt = Nothing
                , tripRating = Nothing
                , tripCreatedAt = time
                , tripUpdatedAt = time
                }

            -- Generate auth token for rider
            token <- runHandler $ createAuthToken (fromSqlKey riderId)

            -- Make request
            request $ do
                setMethod "GET"
                setUrl $ ShowTripRequestR tripRequestId
                addRequestHeader ("Authorization", "Bearer " <> encodeUtf8 token)

            -- Verify response
            statusIs 200
            
            let expectedResponse = object
                    [ "tripRequestId" .= tripRequestId
                    , "tripId" .= tripId
                    ]
            bodyEquals $ (unpack . decodeUtf8 . toStrict . encode) expectedResponse

        it "returns 422 when trip request doesn't exist" $ do
            time <- liftIO getCurrentTime

            -- Create a rider
            riderId <- runDB $ insert $ User
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

            -- Create a non-existent trip request ID
            let nonExistentId = toSqlKey 999999

            -- Make request
            request $ do
                setMethod "GET"
                setUrl $ ShowTripRequestR nonExistentId
                addRequestHeader ("Authorization", "Bearer " <> encodeUtf8 token)

            -- Verify response
            statusIs 422
            
            let expectedResponse = object
                    [ "error" .= ("Trip request not found" :: Text)
                    ]
            bodyEquals $ (unpack . decodeUtf8 . toStrict . encode) expectedResponse