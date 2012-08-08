{-# LANGUAGE DeriveDataTypeable #-}
module Database.PostgreSQL.Postgis (
	Geography(..),
	fromLatLon
	) where
import Database.Persist.Store
import Blaze.ByteString.Builder
import Blaze.Text
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8

newtype Geography = Geography ByteString deriving (Show, Read, Eq, Typeable)

fromLatLon :: Double -> Double -> Geography
fromLatLon lat lon = Geography $ toByteString $ fromByteString "ST_GeographyFromText('SRID=4326;POINT("
                    `mappend` double lat
                    `mappend` Utf8.fromChar ' '
                    `mappend` double lon
                    `mappend` fromByteString ")')" 

instance PersistField Geography where
    toPersistValue (Geography l) = PersistSpecific "geography(POINT,4326)" l
    fromPersistValue (PersistSpecific "geography(POINT,4326)" l) = Right (SRID l)
    fromPersistValue _ = Left "PersistField is not a PersistSpecific of geography"
    sqlType _ = SqlOther "geography(POINT,4326)"