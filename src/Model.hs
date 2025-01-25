{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model where

import ClassyPrelude.Yesod
import Control.Monad (fail)
import Data.Aeson ()
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Quasi
import Database.Persist.Sql (PersistFieldSql (..))
import Text.Printf (printf)
import Text.Read (reads)

-- Define VehicleStatus
data VehicleStatus = Draft | Published
    deriving (Show, Read, Eq, Enum, Bounded)

-- Implement PersistField instance
instance PersistField VehicleStatus where
    toPersistValue Draft = PersistText "Draft"
    toPersistValue Published = PersistText "Published"

    fromPersistValue (PersistText "Draft") = Right Draft
    fromPersistValue (PersistText "Published") = Right Published
    fromPersistValue _ = Left "Invalid VehicleStatus"

-- Implement PersistFieldSql instance
instance PersistFieldSql VehicleStatus where
    sqlType _ = SqlString

-- Implement ToJSON and FromJSON instances
instance ToJSON VehicleStatus where
    toJSON Draft = String "Draft"
    toJSON Published = String "Published"

instance FromJSON VehicleStatus where
    parseJSON (String "Draft") = pure Draft
    parseJSON (String "Published") = pure Published
    parseJSON _ = fail "Invalid VehicleStatus"

-- Define Point type
data Point = Point
    { pointLon :: Double
    , pointLat :: Double
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkPoint :: (Double, Double) -> Point
mkPoint (lon, lat) = Point lon lat

toTuple :: Point -> (Double, Double)
toTuple (Point lon lat) = (lon, lat)

-- PersistField instance for Point
instance PersistField Point where
    toPersistValue (Point lon lat) = PersistLiteral_ Escaped $ encodePoint lon lat
    fromPersistValue (PersistLiteral_ _ bs) = decodePoint bs
    fromPersistValue _ = Left "Expected PersistLiteral_ for Point"

-- PersistFieldSql instance for Point
instance PersistFieldSql Point where
    sqlType _ = SqlOther "point"

encodePoint :: Double -> Double -> ByteString
encodePoint lon lat = TE.encodeUtf8 $ T.pack $ printf "(%f,%f)" lon lat

decodePoint :: ByteString -> Either Text Point
decodePoint bs = case T.splitOn "," $ T.init $ T.tail $ TE.decodeUtf8 bs of
    [lonText, latText] -> do
        lon <- parseDouble lonText
        lat <- parseDouble latText
        Right $ Point lon lat
    _ -> Left "Invalid Point format"

parseDouble :: Text -> Either Text Double
parseDouble t = case reads (T.unpack t) of
    [(d, "")] -> Right d
    _ -> Left "Invalid double"

-- Share the persistent models
share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

instance ToJSON User where
    toJSON user =
        object
            [ "email" .= userEmail user
            , "firstName" .= userFirstName user
            , "lastName" .= userLastName user
            , "type" .= userType user
            , "createdAt" .= userCreatedAt user
            , "updatedAt" .= userUpdatedAt user
            , "tripsCount" .= userTripsCount user
            , "driversLicenseNumber" .= userDriversLicenseNumber user
            ]

instance ToJSON Trip where
    toJSON trip =
        object
            [ "tripRequest" .= tripTripRequest trip
            , "driver" .= tripDriver trip
            , "completedAt" .= tripCompletedAt trip
            , "rating" .= tripRating trip
            , "createdAt" .= tripCreatedAt trip
            , "updatedAt" .= tripUpdatedAt trip
            ]

instance ToJSON TripRequest where
    toJSON req =
        object
            [ "rider" .= tripRequestRider req
            , "startLocation" .= tripRequestStartLocation req
            , "endLocation" .= tripRequestEndLocation req
            , "createdAt" .= tripRequestCreatedAt req
            , "updatedAt" .= tripRequestUpdatedAt req
            ]

-- Entity instances
instance ToJSON (Entity User) where
    toJSON (Entity userId user) = Object $
        KeyMap.insert "id" (toJSON userId) $
            case toJSON user of
                Object obj -> obj
                _ -> error "Unexpected non-object User JSON"

instance ToJSON (Entity Trip) where
    toJSON (Entity tripId trip) = Object $
        KeyMap.insert "id" (toJSON tripId) $
            case toJSON trip of
                Object obj -> obj
                _ -> error "Unexpected non-object Trip JSON"

instance ToJSON (Entity TripRequest) where
    toJSON (Entity reqId req) = Object $
        KeyMap.insert "id" (toJSON reqId) $
            case toJSON req of
                Object obj -> obj
                _ -> error "Unexpected non-object TripRequest JSON"
