{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Api.TripRequestsSpec (spec) where

import TestImport
import Test.HUnit (assertFailure)
import Data.Aeson (object, (.=), encode)
import Database.Persist.Sql (fromSqlKey)
import Data.Time.Clock (addUTCTime)
import Handler.Api.Auth (generateJWT)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

createAuthToken :: Int64 -> Handler Text
createAuthToken userId = do
    time <- liftIO getCurrentTime
    let expTime = addUTCTime (24 * 3600) time  -- token expires in 24 hours
    generateJWT userId expTime

spec :: Spec
spec = withApp $ do
    describe "POST /api/trip-requests" $ do
        it "successfully creates a trip request for authenticated rider" $ do
                        -- Insert rider and driver directly into database
            time <- liftIO getCurrentTime
            
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
                setUrl TripRequestsR
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