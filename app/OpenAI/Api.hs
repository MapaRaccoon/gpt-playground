{-# LANGUAGE DataKinds #-}

{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module OpenAI.Api ( mkApi, Api(..), models ) where

import           Control.Lens
import           Control.Lens.TH()

import           Data.Proxy
import           Data.Text
import           Data.Text.Encoding  ( encodeUtf8 )

import           OpenAI.Resources

import           Servant.API
import           Servant.Auth        as SA
import           Servant.Auth.Client ( Bearer, Token(..) )
import           Servant.Client

type ApiSpec = "models" :> Auth '[ Bearer ] Text :> Get '[ JSON ] ModelsResponse

newtype Api = Api { _models :: ClientM [Model] }
makeLenses ''Api

mkApi :: Text -> Api
mkApi tok = let modelsC = client (Proxy :: Proxy ApiSpec) (Token (encodeUtf8 tok))
            in
              Api { _models = fmap (view mr_data) modelsC }
