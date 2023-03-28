{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens

import           Data.Text

import           Network.HTTP.Client     hiding ( Proxy )
import           Network.HTTP.Client.TLS ( tlsManagerSettings )

import qualified OpenAI.Api              as OpenAI
import           OpenAI.Api              ( mModels )
import           OpenAI.Resources

import           Servant.Client

import System.Environment

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  api <- OpenAI.mkApi . pack <$> getEnv "OPENAI_API_KEY"
  let clientEnv = mkClientEnv mgr (BaseUrl Https "api.openai.com" 443 "v1")
  result <- runClientM (api ^. mModels) clientEnv
  case result of
    Left err -> print err
    Right response -> print $ fmap (view mId) response
  putStrLn "woof"

