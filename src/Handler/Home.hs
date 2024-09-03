{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import

getHomeR :: Handler Value
getHomeR = do
    -- Insert static text into the test table
    insertedId <- runDB $ insert $ TestTable "Hello to Yesod"
    
    -- Retrieve the inserted ID and text
    retrievedRow <- runDB $ get insertedId
    
    -- Return the ID and text as JSON
    case retrievedRow of
        Just (TestTable text) -> return $ object ["id" .= insertedId, "message" .= text]
        Nothing -> return $ object ["error" .= ("Row not found" :: Text)]