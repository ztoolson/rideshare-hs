{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import Import

getHomeR :: Handler Value
getHomeR = do
    -- Return a hard-coded JSON message
    returnJson $ object ["message" .= ("Welcome to Yesod" :: Text)]
