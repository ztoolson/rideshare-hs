{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Api.TripRequests
    ( postTripRequestsR
    ) where

import Import
import qualified Data.Aeson as A
import Database.Esqueleto.Experimental as E


data TripRequestPayload = TripRequestPayload
    { tripRequestRiderId      :: UserId
    , tripRequestStartAddress :: Text
    , tripRequestEndAddress   :: Text
    } deriving (Show)

instance FromJSON TripRequestPayload where
    parseJSON = A.withObject "TripRequestPayload" $ \v -> TripRequestPayload
        <$> (toSqlKey <$> v .: "riderId")
        <*> v .: "startAddress"
        <*> v .: "endAddress"

newtype TripResponse = TripResponse
    { tripRequestId :: TripRequestId
    } deriving (Show)

instance ToJSON TripResponse where
    toJSON TripResponse{..} = object
        [ "trip_request_id" .= fromSqlKey tripRequestId
        ]

-- | Create a new trip request
postTripRequestsR :: Handler A.Value
postTripRequestsR = do
    payload <- requireCheckJsonBody :: Handler TripRequestPayload
    tripReqId <- runDB $ do
        -- Verify rider exists
        void $ get404 (tripRequestRiderId payload)
        
        -- Find or create locations
        startLoc <- findOrCreateLocation (tripRequestStartAddress payload)
        endLoc <- findOrCreateLocation (tripRequestEndAddress payload)
        
        -- Create trip request
        now <- liftIO getCurrentTime
        tripReqId <- insert TripRequest
            { tripRequestRider = tripRequestRiderId payload
            , tripRequestStartLocation = entityKey startLoc
            , tripRequestEndLocation = entityKey endLoc
            , tripRequestCreatedAt = now
            , tripRequestUpdatedAt = now
            }
            
        -- Create associated trip
        void $ createTrip tripReqId

        pure tripReqId
        
    -- Return response
    sendResponseStatus created201 $ toJSON $ TripResponse tripReqId

-- | Helper to find or create a location
findOrCreateLocation :: Text -> ReaderT SqlBackend Handler (Entity Location)
findOrCreateLocation addr = do
    now <- liftIO getCurrentTime
    mLoc <- getBy $ UniqueAddress addr
    case mLoc of
        Just loc -> pure loc
        Nothing -> do
            let newLoc = Location
                    { locationAddress = addr
                    , locationCity = "Unknown"
                    , locationState = "Unknown"
                    , locationPosition = defaultPoint -- You'll need to define this
                    , locationCreatedAt = now
                    , locationUpdatedAt = now
                    }
            insertEntity newLoc

-- | Create a trip with the best available driver
createTrip :: TripRequestId -> ReaderT SqlBackend Handler (Entity Trip)
createTrip reqId = do
    now <- liftIO getCurrentTime
    driver <- findBestAvailableDriver >>= maybe notFound pure
    insertEntity Trip
        { tripTripRequest = reqId
        , tripDriver = entityKey driver
        , tripCompletedAt = Nothing
        , tripRating = Nothing
        , tripCreatedAt = now
        , tripUpdatedAt = now
        }

-- | Define a default point (you'll need to implement this based on your Point type)
-- This should be replaced once geocoding is added in
defaultPoint :: Point
defaultPoint = Point 0 0

findBestAvailableDriver :: ReaderT SqlBackend Handler (Maybe (Entity User))
findBestAvailableDriver = fmap listToMaybe $ E.select $ do
    u <- E.from $ E.table @User
    E.where_ (u ^. UserType E.==. E.val "driver")
    E.orderBy [E.rand]
    E.limit 1
    return u
