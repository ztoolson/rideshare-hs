{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestImport (
    module TestImport,
    module X,
)
where

import Application (makeFoundation, makeLogWare)
import ClassyPrelude as X hiding (Handler, delete, deleteBy)
import Database.Persist as X hiding (get)
import Database.Persist.Sql (SqlPersistM, rawExecute, rawSql, runSqlPersistMPool, unSingle)
import Database.Persist.SqlBackend (getEscapedRawName)
import Foundation as X
import Model as X
import Test.Hspec as X
import Text.Shakespeare.Text (st)
import Yesod.Auth as X
import Yesod.Core.Unsafe (fakeHandlerGetLogger)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)
import Yesod.Test as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <-
        loadYamlSettings
            ["config/test-settings.yml", "config/settings.yml"]
            []
            useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    -- TRUNCATEing all tables is the simplest approach to wiping the database.
    -- Should your application grow to hundreds of tables and tests,
    -- switching to DELETE could be a substantial speedup.
    -- See: https://github.com/yesodweb/yesod-scaffold/issues/201

    -- If there are no tables, return immediately
    unless (null tables) $ do
        let escapedTables = map (\t -> getEscapedRawName t sqlBackend) tables
            query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
        rawExecute query []

getTables :: DB [Text]
getTables = do
    tables <-
        rawSql
            [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_type = 'BASE TABLE';
    |]
            []

    return $ map unSingle tables

-- Helper functions for creating test data
createUser :: Text -> Text -> Text -> Text -> Text -> UTCTime -> Maybe Text -> SqlPersistM (Key User)
createUser email password firstName lastName userType time mLicense =
    insert $
        User
            { userEmail = email
            , userPasswordHash = encodeUtf8 password
            , userFirstName = firstName
            , userLastName = lastName
            , userType = userType
            , userCreatedAt = time
            , userUpdatedAt = time
            , userTripsCount = Nothing
            , userDriversLicenseNumber = mLicense
            }

createLocation :: Text -> Text -> Text -> Point -> UTCTime -> SqlPersistM (Key Location)
createLocation address city state position time =
    insert $
        Location
            { locationAddress = address
            , locationCity = city
            , locationState = state
            , locationPosition = position
            , locationCreatedAt = time
            , locationUpdatedAt = time
            }

createTripRequest :: Key User -> Key Location -> Key Location -> UTCTime -> SqlPersistM (Key TripRequest)
createTripRequest rider startLocation endLocation time =
    insert $
        TripRequest
            { tripRequestRider = rider
            , tripRequestStartLocation = startLocation
            , tripRequestEndLocation = endLocation
            , tripRequestCreatedAt = time
            , tripRequestUpdatedAt = time
            }

createTrip :: Key TripRequest -> Key User -> Maybe UTCTime -> Maybe Int -> UTCTime -> SqlPersistM (Key Trip)
createTrip tripRequest driver completedAt rating time =
    insert $
        Trip
            { tripTripRequest = tripRequest
            , tripDriver = driver
            , tripCompletedAt = completedAt
            , tripRating = rating
            , tripCreatedAt = time
            , tripUpdatedAt = time
            }
