{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Api.TripRequestsSpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy (toStrict)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (addUTCTime)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Handler.Api.Auth (generateJWT)
import Test.HUnit (assertFailure)
import TestImport hiding (decodeUtf8, toStrict, unpack, (.))

createAuthToken :: Int64 -> Handler Text
createAuthToken userId = do
    time <- liftIO getCurrentTime
    let expTime = addUTCTime (24 * 3600) time -- token expires in 24 hours
    generateJWT userId expTime

spec :: Spec
spec = withApp $ do
    describe "POST /api/trip_requests" $ do
        it "successfully creates a trip request for authenticated rider" $ do
            time <- liftIO getCurrentTime

            -- Insert rider and driver directly into database
            riderId <-
                runDB $
                    createUser
                        "rider@example.com"
                        "hashedpass"
                        "Test"
                        "Rider"
                        "rider"
                        time
                        Nothing

            driverId <-
                runDB $
                    createUser
                        "driver@example.com"
                        "hashedpass"
                        "Test"
                        "Driver"
                        "driver"
                        time
                        (Just "DL123456")

            -- Generate auth token for rider
            token <- runHandler $ createAuthToken (fromSqlKey riderId)

            -- Create trip request payload
            let tripRequestPayload =
                    object
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
            riderId <-
                runDB $
                    createUser
                        "rider@example.com"
                        "hashedpass"
                        "Test"
                        "Rider"
                        "rider"
                        time
                        Nothing

            -- Create start and end locations
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

            -- Create associated trip
            tripId <-
                runDB $
                    createTrip
                        tripRequestId
                        riderId
                        Nothing
                        Nothing
                        time

            -- Generate auth token for rider
            token <- runHandler $ createAuthToken (fromSqlKey riderId)

            -- Make request
            request $ do
                setMethod "GET"
                setUrl $ ShowTripRequestR tripRequestId
                addRequestHeader ("Authorization", "Bearer " <> encodeUtf8 token)

            -- Verify response
            statusIs 200

            let expectedResponse =
                    object
                        [ "tripRequestId" .= tripRequestId
                        , "tripId" .= tripId
                        ]
            bodyEquals $ (unpack . decodeUtf8 . toStrict . encode) expectedResponse

        it "returns 422 when trip request doesn't exist" $ do
            time <- liftIO getCurrentTime

            -- Create a rider
            riderId <-
                runDB $
                    createUser
                        "rider@example.com"
                        "hashedpass"
                        "RiderFirstName"
                        "RiderLastName"
                        "rider"
                        time
                        Nothing

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

            let expectedResponse =
                    object
                        [ "error" .= ("Trip request not found" :: Text)
                        ]
            bodyEquals $ (unpack . decodeUtf8 . toStrict . encode) expectedResponse
