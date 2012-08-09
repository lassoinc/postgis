{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Database.PostgreSQL.Postgis (
	Geography(..),
	fromLatLon
	) where
import Database.Persist.Store
import Blaze.ByteString.Builder
import Blaze.Text
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8
import Data.Monoid
import Data.Typeable
import Data.ByteString
import qualified Data.Text as T

newtype Geography = Geography ByteString deriving (Show, Read, Eq, Typeable)

fromLatLon :: Double -> Double -> Geography
fromLatLon lat lon = Geography $ toByteString $ fromByteString "ST_GeographyFromText('SRID=4326;POINT("
                    `mappend` double lat
                    `mappend` Utf8.fromChar ' '
                    `mappend` double lon
                    `mappend` fromByteString ")')" 

instance PersistField Geography where
    toPersistValue (Geography l) = PersistSpecific l 
    fromPersistValue (PersistSpecific l) = Right (Geography l)
    fromPersistValue x = Left $ "PersistField " `mappend` T.pack (show x) `mappend` " is not a PersistSpecific"
    sqlType _ = SqlOther "geography(POINT,4326)"
