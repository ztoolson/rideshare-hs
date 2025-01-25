{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Api.HelloWorld where

import Import

getHelloWorldR :: Handler Value
getHelloWorldR = do
    return $ object ["message" .= ("Hello world!" :: Text)]
