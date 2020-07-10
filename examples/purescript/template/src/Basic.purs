module Basic where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Promise (Promise, fromAff)

handler :: Effect (Promise String)
handler = do
  liftEffect $ fromAff $ pure "Hello World!"
