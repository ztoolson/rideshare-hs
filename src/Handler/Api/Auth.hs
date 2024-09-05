{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Api.Auth where

import Import
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.Aeson (withObject, (.:?))
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.Persist.Sql(fromSqlKey)
import Text.Email.Validate (isValid)
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

data SignupRequest = SignupRequest
    { signupEmail :: Text
    , signupPassword :: PlainTextPassword
    , signupFirstName :: Text
    , signupLastName :: Text
    , signupType :: Text
    , signupDriversLicenseNumber :: Maybe Text
    } deriving Show

instance FromJSON SignupRequest where
    parseJSON = withObject "SignupRequest" $ \v -> SignupRequest
        <$> v .: "email"
        <*> (PlainTextPassword <$> v .: "password")
        <*>  v .: "firstName"
        <*>  v .: "lastName"
        <*>  v .: "type"
        <*>  v .:? "driversLicenseNumber"

postAuthLoginR :: Handler Value
postAuthLoginR = do
    loginReq <- requireCheckJsonBody

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



postAuthSignupR :: Handler Value
postAuthSignupR = do
    signupReq <- requireCheckJsonBody :: Handler SignupRequest

    case validateSignupRequest signupReq of
        Left errors -> sendResponseStatus status400 $ object ["errors" .= errors]
        Right _validRequest -> do

            -- check if user already exists
            mExistingUser <- runDB $ getBy $ UniqueUserEmail (signupEmail signupReq)
            case mExistingUser of
                Just _ -> sendStatusJSON status400 $ object ["error" .= ("Email already in use" :: Text)]
                Nothing -> do
                    mHashedPass <- liftIO $ hashPassword (signupPassword signupReq)
                    case mHashedPass of 
                        Nothing -> sendStatusJSON status500 $ object ["error" .= ("Failed to hash password" :: Text)]
                        Just (HashedPassword hashedPass) -> do
                            currentTime <- liftIO getCurrentTime
                            userId <- runDB $ insert $ User
                                { userEmail = signupEmail signupReq
                                , userPasswordHash = hashedPass
                                , userFirstName = signupFirstName signupReq
                                , userLastName = signupLastName signupReq
                                , userType = signupType signupReq
                                , userCreatedAt = currentTime
                                , userUpdatedAt = currentTime
                                , userTripsCount = Nothing
                                , userDriversLicenseNumber = signupDriversLicenseNumber signupReq
                                }

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

validateSignupRequest :: SignupRequest -> Either [Text] SignupRequest
validateSignupRequest req =
    case validateFields req of
        [] -> Right req
        errors -> Left errors

validateFields :: SignupRequest -> [Text]
validateFields signupReq = catMaybes [ validateEmail $ signupEmail signupReq
                                     , validateSignupPassword $ signupPassword signupReq
                                     , validateNonEmptyField "First name" $ signupFirstName signupReq
                                     , validateNonEmptyField "Last name" $ signupLastName signupReq
                                     , validateNonEmptyField "Type" $ signupType signupReq
                                     , validateDriversLicense $ signupDriversLicenseNumber signupReq
                                     ]
    where
        validateEmail :: Text -> Maybe Text
        validateEmail email
            | T.null (T.strip email) = Just "Email is required"
            | not (isValid $ encodeUtf8 email) = Just "Invalid email format"
            | T.length email > 100 = Just "Email must be at most 100 characters"
            | otherwise = Nothing

        validateSignupPassword :: PlainTextPassword -> Maybe Text
        validateSignupPassword (PlainTextPassword pass)
            | T.null (T.strip pass) = Just "Password is required"
            | T.length pass < 8 = Just "Password must be at least 8 characters long"
            | otherwise = Nothing

        validateNonEmptyField :: Text -> Text -> Maybe Text
        validateNonEmptyField fieldName name
            | T.null (T.strip name) = Just $ fieldName <> " is required"
            | otherwise = Nothing

        validateDriversLicense :: Maybe Text -> Maybe Text
        validateDriversLicense maybeDriversLicense =
            case maybeDriversLicense of
                Just dl | T.length dl > 100 -> Just "Driver's license number must be at most 100 characters"
                _ -> Nothing