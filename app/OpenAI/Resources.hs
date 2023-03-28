{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}

module OpenAI.Resources
  ( Model
  , ModelPermissionSet
  , ModelsResponse
  , HasMId(..)
  , HasMCreated(..)
  , HasMAllowCreateEngine(..)
  , HasMAllowSampling(..)
  , HasMAllowLogprobs(..)
  , HasMAllowSearchIndices(..)
  , HasMAllowView(..)
  , HasMAllowFineTuning(..)
  , HasMOrganization(..)
  , HasMGroup(..)
  , HasMIsBlocking(..)
  , HasMOwnedBy(..)
  , HasMPermission(..)
  , HasMData(..)
  ) where

import           Control.Lens.TH
import           Control.Monad

import           Data.Aeson
import           Data.Aeson.Types ( Parser )
import           Data.Text

import           GHC.Generics

data ModelPermissionSet =
  ModelPermissionSet { _modelPermissionSetMId :: Text
                     , _modelPermissionSetMCreated :: Integer
                     , _modelPermissionSetMAllowCreateEngine :: Bool
                     , _modelPermissionSetMAllowSampling :: Bool
                     , _modelPermissionSetMAllowLogprobs :: Bool
                     , _modelPermissionSetMAllowSearchIndices :: Bool
                     , _modelPermissionSetMAllowView :: Bool
                     , _modelPermissionSetMAllowFineTuning :: Bool
                     , _modelPermissionSetMOrganization :: String
                     , _modelPermissionSetMGroup :: Maybe Text
                     , _modelPermissionSetMIsBlocking :: Bool
                     }
  deriving ( Show, Eq )

makeFields ''ModelPermissionSet

data Model = Model { _modelMId         :: Text
                   , _modelMCreated    :: Integer
                   , _modelMOwnedBy   :: Text
                   , _modelMPermission :: [ModelPermissionSet]
                   }
  deriving ( Show, Generic )

makeFields ''Model

newtype ModelsResponse = ModelsResponse { _modelsResponseMData :: [Model] }
  deriving ( Show )

makeFields ''ModelsResponse

guardType :: Text -> Object -> Parser ()
guardType expectedType obj = do
  actualType :: Text <- obj .: "object"
  when (actualType /= expectedType) $ fail $
    unpack ("Expected object to be of type " <> expectedType <> ", but got "
            <> actualType)

instance FromJSON ModelPermissionSet where
  parseJSON = withObject "ModelPermissionSet" $ \root -> do
    guardType "model_permission" root
    ModelPermissionSet 
        <$> (root .: "id") 
        <*> (root .: "created")
        <*> (root .: "allow_create_engine") 
        <*> (root .: "allow_sampling")
        <*> (root .: "allow_logprobs") 
        <*> (root .: "allow_search_indices")
        <*> (root .: "allow_view") 
        <*> (root .: "allow_fine_tuning")
        <*> (root .: "organization") 
        <*> (root .: "group")
        <*> (root .: "is_blocking")

instance FromJSON Model where
  parseJSON = withObject "Model" $ \root -> do
    guardType "model" root
    Model 
        <$> (root .: "id") 
        <*> (root .: "created") 
        <*> (root .: "owned_by")
        <*> (root .: "permission")

instance FromJSON ModelsResponse where
  parseJSON = withObject "ModelsResponse" $ \root -> do
    guardType "list" root
    ModelsResponse <$> root .: "data"
