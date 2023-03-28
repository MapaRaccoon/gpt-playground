{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module OpenAI.Api ( mkApi, Api(..), HasMModels(..), HasMModel(..) ) where

import           Control.Lens
import           Control.Lens.TH     ()

import           Data.Proxy
import           Data.Text
import           Data.Text.Encoding  ( encodeUtf8 )

import           OpenAI.Resources

import           Servant.API
import           Servant.Auth        as SA
import           Servant.Auth.Client ( Bearer, Token(..) )
import           Servant.Client

type PreAuthApiSpec = "models" :> Get '[ JSON ] ModelsResponse
  :<|> "models" :> Capture "model" Text :> Get '[ JSON ] Model

type ApiSpec = Auth '[ Bearer ] Text :> PreAuthApiSpec

data Api =
  Api { _apiMModels :: ClientM [Model], _apiMModel :: Text -> ClientM Model }

makeFields ''Api

mkApi :: Text -> Api
mkApi tok =
  let fModels :<|> fModel =
        client (Proxy :: Proxy ApiSpec) (Token (encodeUtf8 tok))
  in
    Api { _apiMModels = fmap (view mData) fModels, _apiMModel = fModel }
