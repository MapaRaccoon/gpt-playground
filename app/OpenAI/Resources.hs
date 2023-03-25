{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module OpenAI.Resources
  ( Model
  , m_id
  , m_created
  , m_owned_by
  , m_permission
  , ModelPermissionSet
  , mps_id
  , mps_created
  , mps_allow_create_engine
  , mps_allow_sampling
  , mps_allow_logprobs
  , mps_allow_search_indices
  , mps_allow_view
  , mps_allow_fine_tuning
  , mps_organization
  , mps_group
  , mps_is_blocking
  , ModelsResponse
  , mr_data
  ) where

import           Control.Lens.TH
import           Control.Monad

import           Data.Aeson
import           Data.Aeson.Types ( Parser )
import           Data.Text

import           GHC.Generics

data ModelPermissionSet =
  ModelPermissionSet { _mps_id :: Text
                     , _mps_created :: Integer
                     , _mps_allow_create_engine :: Bool
                     , _mps_allow_sampling :: Bool
                     , _mps_allow_logprobs :: Bool
                     , _mps_allow_search_indices :: Bool
                     , _mps_allow_view :: Bool
                     , _mps_allow_fine_tuning :: Bool
                     , _mps_organization :: String
                     , _mps_group :: Maybe Text
                     , _mps_is_blocking :: Bool
                     }
  deriving ( Show, Eq )

makeLenses ''ModelPermissionSet

data Model = Model { _m_id         :: Text
                   , _m_created    :: Integer
                   , _m_owned_by   :: Text
                   , _m_permission :: [ModelPermissionSet]
                   }
  deriving ( Show, Generic )

makeLenses ''Model

newtype ModelsResponse = ModelsResponse { _mr_data :: [Model] }
  deriving ( Show )

makeLenses ''ModelsResponse

guardType :: Text -> Object -> Parser ()
guardType expectedType obj = do
  actualType :: Text <- obj .: "object"
  when (actualType /= expectedType) $ fail $ unpack ("Expected object to be of type " <> expectedType <> ", but got " <> actualType)

instance FromJSON ModelPermissionSet where
  parseJSON = withObject "ModelPermissionSet" $ \root -> do
    guardType "model_permission" root
    ModelPermissionSet <$> (root .: "id") <*> (root .: "created")
    <*> (root .: "allow_create_engine") <*> (root .: "allow_sampling")
    <*> (root .: "allow_logprobs") <*> (root .: "allow_search_indices")
    <*> (root .: "allow_view") <*> (root .: "allow_fine_tuning")
    <*> (root .: "organization") <*> (root .: "group")
    <*> (root .: "is_blocking")

instance FromJSON Model where
  parseJSON = withObject "Model" $ \root -> do
    guardType "model" root
    Model <$> (root .: "id")
    <*> (root .: "created") <*> (root .: "owned_by") <*> (root .: "permission")

instance FromJSON ModelsResponse where
  parseJSON = withObject "ModelsResponse" $ \root -> do
    guardType "list" root
    ModelsResponse <$> root .: "data"
