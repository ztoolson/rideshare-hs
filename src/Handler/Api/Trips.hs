{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}


module Handler.Api.Trips (
    getMyTripsR,
)
where

import Import hiding ((==.), on, isNothing, (.))
import Database.Esqueleto.Experimental
import qualified Data.Aeson as A

data TripResponse = TripResponse
    { tripDetails :: Entity Trip
    , riderDetails :: Entity User
    , requestDetails :: Entity TripRequest
    , driverDetails :: Maybe (Entity User)
    } deriving (Generic)

-- Manual ToJSON instance
instance ToJSON TripResponse where
    toJSON TripResponse{..} = object
        [ "trip" .= tripDetails
        , "rider" .= riderDetails
        , "request" .= requestDetails
        , "driver" .= driverDetails
        ]

getMyTripsR :: Handler A.Value
getMyTripsR = do
    userId <- requireJWTAuthId
    trips <- runDB $ getCompletedTripsForUser userId
    return $ object ["trips" .=  Import.map (\(t, r, req, d) -> TripResponse t r req d) trips]

getCompletedTripsForUser :: UserId -> ReaderT SqlBackend Handler [(Entity Trip, Entity User, Entity TripRequest, Maybe (Entity User))]
getCompletedTripsForUser userId = do
    select $ do
        (trip :& tripRequest :& rider :& driver) <-
            from $ table @Trip
            `innerJoin` table @TripRequest
            `on` (\(t :& tr) -> 
                t ^. TripTripRequest ==. tr ^. TripRequestId)
            `innerJoin` table @User
            `on` (\(_ :& tr :& r) -> 
                tr ^. TripRequestRider ==. r ^. UserId)
            `leftJoin` table @User
            `on` (\(t :& _ :& _ :& d) -> 
                just (t ^. TripDriver) ==. d ?. UserId)
        where_ $ rider ^. UserId ==. val userId
        where_ $ not_ . isNothing $ (trip ^. TripCompletedAt)
        pure (trip, rider, tripRequest, driver)