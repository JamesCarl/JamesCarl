module S3EventSource where

import Prelude
import Control.Monad.Except (throwError, runExcept)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Promise (Promise, fromAff)
import Effect.Aff.Compat (mkEffectFn1)
import Effect.Uncurried (EffectFn1)
import Effect.Exception (error)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Foreign (F, Foreign, MultipleErrors, readString, readArray, unsafeToForeign, renderForeignError)
import Foreign.Index ((!))
import Debug.Trace (traceM)

readS3Object :: Foreign -> F S3Object
readS3Object value = do
  key <- value ! "key" >>= readString
  pure { key }

readS3Bucket :: Foreign -> F S3Bucket
readS3Bucket value = do
  name <- value ! "name" >>= readString
  pure { name }

readS3 :: Foreign -> F S3
readS3 value = do
  bucket <- value ! "bucket" >>= readS3Bucket
  object <- value ! "object" >>= readS3Object
  pure { bucket, object }

readS3Record :: Foreign -> F S3Record
readS3Record value = do
  s3 <- value ! "s3" >>= readS3
  pure { s3 }

readS3Event :: Foreign -> F S3Event
readS3Event value = do
  records <- value ! "Records" >>= readArray >>= traverse readS3Record
  pure { records }

-- Not including all the fields for now
type S3Object
  = { key :: String }

type S3Bucket
  = { name :: String }

type S3
  = { bucket :: S3Bucket, object :: S3Object }

type S3Record
  = { s3 :: S3 }

type S3Event
  = { records :: Array S3Record }

renderForeignErrors :: MultipleErrors -> String
renderForeignErrors e =
  show $ renderForeignError <$> e

handleEvent :: forall a b. Foreign -> (Foreign -> F a) -> (a -> b) -> Effect (Promise Foreign)
handleEvent value reader fn = case (runExcept $ reader value) of
  Right r -> liftEffect $ fromAff $ pure $ unsafeToForeign $ fn r
  Left e -> liftEffect $ fromAff $ throwError $ error $ renderForeignErrors e

handler' :: Foreign -> Effect (Promise Foreign)
handler' event = do
  traceM event
  handleEvent event readS3Event
    ( \s3Event ->
        "s3 events: " <> show s3Event.records
    )

handler :: EffectFn1 Foreign (Promise Foreign)
handler = mkEffectFn1 handler'
