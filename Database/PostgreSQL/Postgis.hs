{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Database.PostgreSQL.Postgis (
	Geography(..),
	fromLonLat
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

fromLonLat :: Double -> Double -> Geography
fromLonLat lon lat = if lat <= 90 && lat >= -90 && lon <= 180 && lon >= -180
                        then Geography $ toByteString $ fromByteString "ST_GeographyFromText('SRID=4326;POINT("
                            `mappend` double lon
                            `mappend` Utf8.fromChar ' '
                            `mappend` double lat
                            `mappend` fromByteString ")')" 
                        else error $ "Coordinates (" ++ show lon ++ ", " ++ show lat ++ ") are invalid"

instance PersistField Geography where
    toPersistValue (Geography l) = PersistSpecific l 
    fromPersistValue (PersistSpecific l) = Right (Geography l)
    fromPersistValue x = Left $ "PersistField " `mappend` T.pack (show x) `mappend` " is not a PersistSpecific"
    sqlType _ = SqlOther "geography(POINT,4326)"
