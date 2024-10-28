{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import Import.NoFoundation
import Control.Monad.Logger (LogSource)
import Data.Kind (Type)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Web.JWT as JWT
import qualified Yesod.Core.Unsafe as Unsafe

data App = App
    { appSettings    :: AppSettings
    , appConnPool    :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

instance Yesod App where
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    errorHandler :: ErrorResponse -> Handler TypedContent
    errorHandler errorResponse = do
        let (status, message) = case errorResponse of
                NotFound -> (status404, "Not Found" :: Text)
                NotAuthenticated -> (status401, "Unauthorized" :: Text)
                PermissionDenied msg -> (status401, msg)
                BadMethod _ -> (status404, "Method Not Allowed" :: Text)
                InternalError msg -> (status500, msg)
                InvalidArgs _ -> (status400, "Invalid Arguments" :: Text)
        sendStatusJSON status $ object ["error" .= message]

    authRoute :: App -> Maybe (Route App)
    authRoute _ = Nothing  -- This disables redirects on auth failure

    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

    isAuthorized :: Route App -> Bool -> Handler AuthResult
    -- routes requiring authentication
    isAuthorized HelloWorldR _ = isAuthenticated
    isAuthorized CreateTripRequestsR _ = isAuthenticated
    isAuthorized (ShowTripRequestR tripId) _ = isAuthenticated

    -- routes not requiring authentication
    isAuthorized _ _ = return Authorized

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

isAuthenticated :: Handler AuthResult
isAuthenticated = do
    mAuth <- parseBearerToken
    case mAuth of
        Nothing -> return (Unauthorized "Missing Authorization Header")
        Just token -> do
            isValid <- liftIO $ validateJWT token
            if isValid
                then return Authorized
                else return (Unauthorized "Invalid Token")


parseBearerToken :: Handler (Maybe Text)
parseBearerToken = do
    mAuthHeader <- lookupHeader "Authorization"
    return $ mAuthHeader >>= extractToken

extractToken :: BS.ByteString -> Maybe Text
extractToken header =
    case T.words $ TE.decodeUtf8 header of
        ("Bearer":token:_) -> Just token
        _ -> Nothing

validateJWT :: Text -> IO Bool
validateJWT token = do
    currentTime <- getPOSIXTime
    let secret = "your-secret-key" :: Text
        secretKey = JWT.hmacSecret secret
        verifySigner = JWT.toVerify secretKey
    case JWT.decodeAndVerifySignature verifySigner token of
        Nothing -> return False
        Just verifiedToken -> do
            let claimsSet = JWT.claims verifiedToken
            case JWT.exp claimsSet of
                Nothing -> return False
                Just expTime -> return (JWT.secondsSinceEpoch expTime > currentTime)