module SimpleJson where

import Prelude
import Control.Monad.Except (throwError)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Promise (Promise, fromAff)
import Foreign (Foreign)
import Effect.Aff.Compat (mkEffectFn1)
import Effect.Uncurried (EffectFn1)
import Effect.Exception (error)
import Data.Either (Either(..))
import Simple.JSON (class ReadForeign, read, write, class WriteForeign)
import Data.Maybe (Maybe)

withEvent :: forall a b. ReadForeign a => WriteForeign b => Foreign -> (a -> b) -> Effect (Promise Foreign)
withEvent f fn = case read f of
  Right (r :: a) -> liftEffect $ fromAff $ pure $ write $ fn r
  Left e -> liftEffect $ fromAff $ throwError $ error $ show e

type Input
  = { id :: Number, message :: String, nested :: Maybe Nested }

type Nested
  = { value :: String }

type Output
  = { text :: String }

handler' :: Foreign -> Effect (Promise Foreign)
handler' f =
  withEvent f
    ( \(input :: Input) ->
      { text: "You said: " <> input.message } :: Output
    )

handler :: EffectFn1 Foreign (Promise Foreign)
handler = mkEffectFn1 handler'
