{-# LANGUAGE OverloadedStrings #-}
module Vindinium.Api
        ( startTraining
        , startArena
        , request
        )
    where

import Vindinium.Types

import Network.HTTP.Client
import Network.HTTP.Types

import Data.Text (Text, unpack)
import Data.Aeson
import Data.Monoid ((<>))

import Control.Monad (liftM)

startTraining :: Settings -> Maybe Int -> Maybe Board -> IO State
startTraining settings mi mb = do
    let url = startUrl settings "training"
        obj = object ( maybe [] (\i -> [("turns", toJSON i)]) mi
                    <> maybe [] (\b -> [("map",  toJSON b)]) mb
                     )
    request settings url obj

startArena :: Settings -> IO State
startArena settings = do
    let url = startUrl settings "arena"
    request settings url (object [])

startUrl :: Settings -> Text -> Text
startUrl settings v = (\x -> x <> "/api/" <> v) . settingsUrl $ settings

request :: Settings -> Text -> Value -> IO State
request settings url val = do
    let key = settingsKey settings

    initReq <- parseUrl $ unpack url
    let req = initReq
                { method = "POST"
                , requestHeaders =
                    [ (hContentType, "application/json")
                    , (hAccept,      "application/json")
                    , (hUserAgent,   "vindinium-starter-haskell")
                    ]
                , requestBody = jsonBody (injectKey val key)
                , responseTimeout = Just 1000000000
                }

    withManager defaultManagerSettings $ \mgr ->
        liftM (decodeBody . responseBody) $ httpLbs req mgr

  where
    jsonBody = RequestBodyLBS . encode
    decodeBody body = case eitherDecode body of
            Left e  -> error $ "request: unable to decode state: " ++ e
            Right s -> s
    injectKey (Object a) k =
        let
            (Object b) = object [("key", toJSON k)]
        in
            Object (a <> b)
    injectKey _ _ = error "invalid argument to injectKey" -- avoid warning
