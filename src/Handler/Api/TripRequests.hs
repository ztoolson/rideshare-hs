{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Api.TripRequests
    ( postCreateTripRequestsR
    , getShowTripRequestR
    ) where

import Import
import qualified Data.Aeson as A
import Database.Esqueleto.Experimental as E


data CreateTripRequestPayload = CreateTripRequestPayload
    { tripRequestRiderId      :: UserId
    , tripRequestStartAddress :: Text
    , tripRequestEndAddress   :: Text
    } deriving (Show)

instance FromJSON CreateTripRequestPayload where
    parseJSON = A.withObject "TripRequestPayload" $ \v -> CreateTripRequestPayload
        <$> (toSqlKey <$> v .: "riderId")
        <*> v .: "startAddress"
        <*> v .: "endAddress"

newtype CreateTripResponse = CreateTripResponse
    { tripRequestId :: TripRequestId
    } deriving (Show)

instance ToJSON CreateTripResponse where
    toJSON CreateTripResponse{..} = object
        [ "tripRequestId" .= fromSqlKey tripRequestId
        ]

-- postCreateTripRequestR is the handler for a rider requesting a ride. It will:
-- 1. create a record for the trip_request
-- 2. create locations for startAddress and endAddress in location
-- 3. assign a driver to create a trip record
--
-- TODO: add more robust security to ensure the rider requesting the ride is the one that is auth'd in the jwt
postCreateTripRequestsR :: Handler A.Value
postCreateTripRequestsR = do
    payload <- requireCheckJsonBody :: Handler CreateTripRequestPayload
    -- runDB will run all actions in a db transaction so this flow is all or nothing
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
        
    sendResponseStatus created201 $ toJSON $ CreateTripResponse tripReqId


-- getTripRequestR will check if the trip id exists, if it does it will return the trip request id and the associated trip id.
getShowTripRequestR:: TripRequestId -> Handler A.Value
getShowTripRequestR tripRequestId = do
    result <- runDB $ do
        -- Verify trip_request exist
        mTripRequest <- get tripRequestId
        case mTripRequest of
            Nothing -> return Nothing
            Just _tr -> do
                mTrip <- selectFirst [TripTripRequest Import.==. tripRequestId] []
                return $ Just $ object
                    [ "tripRequestId" .= tripRequestId
                    , "tripId" .= fmap entityKey mTrip
                    ]

    case result of
        Nothing -> 
            sendResponseStatus status422 $ 
                object ["error" .= ("Trip request not found" :: Text)]
        Just response -> 
            sendResponseStatus status200 response


-- createTrip will find a driver and assign them to the trip request by creating a record in the trip table
createTrip :: TripRequestId -> ReaderT SqlBackend Handler (Entity Trip)
createTrip reqId = do
    now <- liftIO getCurrentTime
    mDriver <- findBestAvailableDriver
    driver <- case mDriver of
        Nothing -> lift $ sendStatusJSON status404 $
            object ["error" .= ("No available drivers found" :: Text)]
        Just d -> return d
    insertEntity Trip
        { tripTripRequest = reqId
        , tripDriver = entityKey driver
        , tripCompletedAt = Nothing
        , tripRating = Nothing
        , tripCreatedAt = now
        , tripUpdatedAt = now
        }

-- findOrCreateLocation will check a given address in the location table. If it does, it will use that address otherwise it will create a record
findOrCreateLocation :: Text -> ReaderT SqlBackend Handler (Entity Location)
findOrCreateLocation addr = do
    now <- liftIO getCurrentTime
    mLoc <- getBy $ UniqueAddress addr
    -- TODO: this is where to make a call to geocode address if either address exists and hasn't been geocode yet or if address doesn't exist
    case mLoc of
        Just loc -> pure loc
        Nothing -> do
            let newLoc = Location
                    { locationAddress = addr
                    , locationCity = "Unknown"
                    , locationState = "Unknown"
                    , locationPosition = defaultPoint 
                    , locationCreatedAt = now
                    , locationUpdatedAt = now
                    }
            insertEntity newLoc

-- findBestAvailableDriver will select a random driver from the user table and assign to the request
-- TODO: if you want, this could be a place to improve the drive selection algorithm
findBestAvailableDriver :: ReaderT SqlBackend Handler (Maybe (Entity User))
findBestAvailableDriver = do
    driver <- E.select $ do
        u <- E.from $ E.table @User
        E.where_ (u ^. UserType E.==. E.val "driver")
        E.orderBy [E.rand]
        E.limit 1
        return u
    return $ listToMaybe driver

-- | Define a default point (you'll need to implement this based on your Point type)
-- TODO This should be replaced once geocoding is added in
defaultPoint :: Point
defaultPoint = Point 0 0
