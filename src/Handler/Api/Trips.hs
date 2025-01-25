{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Api.Trips (
    getMyTripsR,
)
where

import Import

getMyTripsR :: Handler Value
getMyTripsR = do
    return $ object ["message" .= ("Hello world! from my trips" :: Text)]
