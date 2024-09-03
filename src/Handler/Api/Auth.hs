{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Api.Auth where

import Import
import Data.Aeson (withObject)
import Database.Persist.Sql(fromSqlKey)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Web.JWT as JWT (encodeSigned, hmacSecret, JWTClaimsSet(..), stringOrURI, numericDate)
import qualified Data.Text as T


newtype HashedPassword = HashedPassword ByteString deriving Show
newtype PlainTextPassword = PlainTextPassword Text deriving Show

data LoginRequest = LoginRequest
    { loginEmail :: Text
    , loginPassword :: PlainTextPassword
    } deriving Show

instance FromJSON LoginRequest where
    parseJSON = withObject "LoginRequest" $ \v -> LoginRequest
        <$> v .: "email"
        <*> (PlainTextPassword <$>v .: "password")

postAuthLoginR :: Handler Value
postAuthLoginR = do
    loginReq <- requireCheckJsonBody :: Handler LoginRequest

    mUser <- runDB $ getBy $ UniqueUserEmail (loginEmail loginReq)
    case mUser of
        Nothing -> sendStatusJSON status401 $ object ["error" .= ("Invalid email or password" :: Text)]
        Just (Entity userId user) -> do
            let storedHash = HashedPassword $ userPasswordHash user
            if checkPassword storedHash (loginPassword loginReq)
            then do
                time <- liftIO getCurrentTime
                let expTime = addUTCTime(24 * 3600) time -- token expires in 24 hours
                token <- generateJWT (fromSqlKey userId) expTime

                return $ object
                    [ "token" .= token
                    , "exp" .= formatTime defaultTimeLocale "$m-$d-$Y %H:%M" expTime
                    , "userId" .= userId
                    ]
            else
                sendStatusJSON status401 $ object ["error" .= ("Invalid email or password" :: Text)]



data SignupRequest = SignupRequest
    { email :: Text
    , password :: PlainTextPassword
    } deriving Show

instance FromJSON SignupRequest where
    parseJSON = withObject "SignupRequest" $ \v -> SignupRequest
        <$> v .: "email"
        <*> (PlainTextPassword <$> v .: "password")

postAuthSignupR :: Handler Value
postAuthSignupR = do
    signupReq <- requireCheckJsonBody :: Handler SignupRequest

    -- check if user already exists
    mExistingUser <- runDB $ getBy $ UniqueUserEmail (email signupReq)
    case mExistingUser of
        Just _ -> sendStatusJSON status400 $ object ["error" .= ("Email already in use" :: Text)]
        Nothing -> do
            mHashedPass <- liftIO $ hashPassword (password signupReq)
            case mHashedPass of 
                Nothing -> sendStatusJSON status500 $ object ["error" .= ("Failed to hash password" :: Text)]
                Just (HashedPassword hashedPass) -> do
                    userId <- runDB $ insert $ User (email signupReq) hashedPass

                    -- generate JWT token
                    time <- liftIO getCurrentTime
                    let expTime = addUTCTime(24 * 3600) time -- token expires in 24 hours
                    token <- generateJWT (fromSqlKey userId) expTime

                    returnJson $ object
                        [ "token" .= token
                        , "exp" .= formatTime defaultTimeLocale "$m-$d-$Y %H:%M" expTime
                        , "userId" .= userId
                        ]


generateJWT :: Int64 -> UTCTime -> Handler Text
generateJWT userId expirationTime = do
    let secret = "your-secret-key" -- This is not secure, if you want this to be a real app then don't hard code this
    let cs = mempty { JWT.sub = JWT.stringOrURI (T.pack $ show userId)
                    , JWT.exp = JWT.numericDate (utcTimeToPOSIXSeconds expirationTime)
                    }
    return $ JWT.encodeSigned (JWT.hmacSecret secret) mempty cs

hashPassword :: PlainTextPassword -> IO (Maybe HashedPassword)
hashPassword (PlainTextPassword pass) = 
    fmap HashedPassword <$> hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 pass)

checkPassword :: HashedPassword -> PlainTextPassword -> Bool
checkPassword (HashedPassword hashedPassword) (PlainTextPassword pass) = validatePassword hashedPassword (encodeUtf8 pass)