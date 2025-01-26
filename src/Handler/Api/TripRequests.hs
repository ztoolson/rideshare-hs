{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Api.TripRequests (
    postCreateTripRequestsR,
    getShowTripRequestR,
)
where

import qualified Data.Aeson as A
import Control.Monad.Extra (fromMaybeM)
import Database.Esqueleto.Experimental as E
import qualified Database.Esqueleto.PostgreSQL as EP
import Import

data CreateTripRequestPayload = CreateTripRequestPayload
    { tripRequestStartAddress :: Text
    , tripRequestEndAddress :: Text
    }
    deriving (Show)

instance FromJSON CreateTripRequestPayload where
    parseJSON = A.withObject "TripRequestPayload" $ \v ->
        CreateTripRequestPayload
            <$> v .: "startAddress"
            <*> v .: "endAddress"

newtype CreateTripResponse = CreateTripResponse
    { tripRequestId :: TripRequestId
    }
    deriving (Show)

instance ToJSON CreateTripResponse where
    toJSON CreateTripResponse{..} =
        object
            [ "tripRequestId" .= fromSqlKey tripRequestId
            ]

-- postCreateTripRequestR is the handler for a rider requesting a ride. It will:
-- 1. create a record for the trip_request
-- 2. create locations for startAddress and endAddress in location
-- 3. assign a driver to create a trip record
--
postCreateTripRequestsR :: Handler A.Value
postCreateTripRequestsR = do
    payload <- requireCheckJsonBody :: Handler CreateTripRequestPayload
    -- Check that use in auth is the same as the user in the request. TODO - maybe better to not user id from payload and only user id from auth?
    jwtUserId <- requireJWTAuthId

    -- runDB will run all actions in a db transaction so this flow is all or nothing
    tripReqId <- runDB $ do
        -- Verify rider exists
        void $ get404 jwtUserId

        -- Find or create locations
        startLoc <- findOrCreateLocation (tripRequestStartAddress payload)
        endLoc <- findOrCreateLocation (tripRequestEndAddress payload)

        -- Create trip request
        now <- liftIO getCurrentTime
        tripReqId <-
            insert
                TripRequest
                    { tripRequestRider = jwtUserId
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
getShowTripRequestR :: TripRequestId -> Handler A.Value
getShowTripRequestR tripRequestId = do
    result <- runDB $ getTripWithRequest tripRequestId

    case result of
        [] ->
            sendResponseStatus status422 $
                object ["error" .= ("Trip request not found" :: Text)]
        ((_, trip) : _) ->
            sendResponseStatus status200 $
                object
                    [ "tripRequestId" .= tripRequestId
                    , "tripId" .= fromSqlKey (entityKey trip)
                    ]

-- createTrip will find a driver and assign them to the trip request by creating a record in the trip table
createTrip :: TripRequestId -> ReaderT SqlBackend Handler (Entity Trip)
createTrip tripReqId = do
    now <- liftIO getCurrentTime
    mDriver <- findBestAvailableDriver
    driver <- case mDriver of
        Nothing ->
            lift $
                sendStatusJSON status404 $
                    object ["error" .= ("No available drivers found" :: Text)]
        Just d -> return d
    insertEntity
        Trip
            { tripTripRequest = tripReqId
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
    fromMaybeM
      (insertEntity
          Location
              { locationAddress = addr
              , locationCity = "Unknown"
              , locationState = "Unknown"
              , locationPosition = defaultPoint  -- TODO: Geocode
              , locationCreatedAt = now
              , locationUpdatedAt = now
              })
      (return mLoc)

-- findBestAvailableDriver will select a random driver from the user table and assign to the request
-- TODO: if you want, this could be a place to improve the drive selection algorithm
findBestAvailableDriver :: ReaderT SqlBackend Handler (Maybe (Entity User))
findBestAvailableDriver = do
    driver <- E.select $ do
        u <- E.from $ E.table @User
        E.where_ (u ^. UserType E.==. E.val "driver")
        E.orderBy [E.asc $ EP.random_ @Double]
        E.limit 1
        return u
    return $ listToMaybe driver

{- | Define a default point (you'll need to implement this based on your Point type)
 TODO This should be replaced once geocoding is added in
-}
defaultPoint :: Point
defaultPoint = Point 0 0

-- getTripWithRequest does a db query to join trip_result and trip for the given trip request id.
getTripWithRequest ::
    (MonadIO m) =>
    TripRequestId ->
    SqlPersistT m [(Entity TripRequest, Entity Trip)]
getTripWithRequest tripRequestId = do
    select $ do
        (tripRequest :& trip) <-
            from
                $ table @TripRequest
                    `innerJoin` table @Trip
                `E.on` ( \(tr :& t) ->
                            t ^. TripTripRequest E.==. tr ^. TripRequestId
                       )
        where_ (tripRequest ^. TripRequestId E.==. val tripRequestId)
        pure (tripRequest, trip)
