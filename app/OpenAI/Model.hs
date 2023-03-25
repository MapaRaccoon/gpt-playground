module OpenAI.Model ( Model(..) ) where

import           Data.Text

import           GHC.Generics

data Model =
  Model { id :: Text, object :: Text, owned_by :: Text, permission :: Text }
  deriving ( Show, Generic )
