{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foundation where

import Control.Monad ()
import Control.Monad.Logger (LogSource)
import Control.Monad.Trans.Maybe ()
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.HashMap.Strict ()
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.Persist.Sql (ConnectionPool, runSqlPool, toSqlKey)
import Import.NoFoundation
import qualified Web.JWT as JWT
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

-- define datatype for jwt token
data JWTClaims = JWTClaims
  { jwtUserId :: Key User, -- Using Key User instead of UserId
    jwtExp :: Integer
  }
  deriving (Show)

instance A.FromJSON JWTClaims where
  parseJSON = A.withObject "JWTClaims" $ \v ->
    JWTClaims
      <$> (toSqlKey <$> v .: "user_id") -- Parse as Int64 and convert to Key User
      <*> v
      .: "exp"

data App = App
  { appSettings :: AppSettings,
    appConnPool :: ConnectionPool,
    appHttpManager :: Manager,
    appLogger :: Logger
  }

mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

type DB a =
  forall (m :: Type -> Type).
  (MonadUnliftIO m) =>
  ReaderT SqlBackend m a

instance Yesod App where
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  yesodMiddleware :: (ToTypedContent res) => Handler res -> Handler res
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
  authRoute _ = Nothing -- This disables redirects on auth failure

  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return
      $ appShouldLogAll (appSettings app)
      || level
      == LevelWarn
      || level
      == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

  isAuthorized :: Route App -> Bool -> Handler AuthResult
  -- routes requiring authentication
  isAuthorized HelloWorldR _ = isAuthenticated
  isAuthorized CreateTripRequestsR _ = isAuthenticated
  isAuthorized (ShowTripRequestR _) _ = isAuthenticated
  isAuthorized MyTripsR _ = isAuthenticated
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
      mClaims <- liftIO $ validateJWT token
      case mClaims of
        Nothing -> return (Unauthorized "Invalid Token")
        Just _ -> return Authorized

parseBearerToken :: Handler (Maybe Text)
parseBearerToken = do
  mAuthHeader <- lookupHeader "Authorization"
  return $ mAuthHeader >>= extractToken

extractToken :: BS.ByteString -> Maybe Text
extractToken header =
  case T.words $ TE.decodeUtf8 header of
    ("Bearer" : token : _) -> Just token
    _ -> Nothing

requireJWTAuthId :: Handler (Key User)
requireJWTAuthId = do
  token <- requireBearerToken
  claims <- liftIO (validateJWT token) >>= maybe (permissionDenied "Invalid token") return
  return $ jwtUserId claims

requireBearerToken :: Handler Text
requireBearerToken = do
  mToken <- parseBearerToken
  maybe (permissionDenied "Missing Authorization header") return mToken

validateJWT :: Text -> IO (Maybe JWTClaims)
validateJWT token = do
  currentTime <- getPOSIXTime
  let secret = "your-secret-key" :: Text
      secretKey = JWT.hmacSecret secret
      verifySigner = JWT.toVerify secretKey
  return $ do
    verifiedToken <- JWT.decodeAndVerifySignature verifySigner token
    let claimsSet = JWT.claims verifiedToken
        expTime = JWT.secondsSinceEpoch <$> JWT.exp claimsSet
        JWT.ClaimsMap customClaims = JWT.unregisteredClaims claimsSet

        -- Extract user_id from the claims using pattern matching
        userId = case Map.lookup "user_id" customClaims of
          Just (Number n) -> Just (round n) -- Convert Scientific to Integer
          _ -> Nothing

    -- Check if token is expired
    guard $ maybe False (> currentTime) expTime

    -- Get the actual userId or fail
    actualUserId <- userId

    -- Construct our claims
    return
      $ JWTClaims
        { jwtUserId = toSqlKey actualUserId,
          jwtExp = maybe 0 round expTime
        }