{-# OPTIONS -Wall #-}

module Git.SHA (
    showDigestBS,
    readDigestBS,
    objectDigest,
    objectId
) where

import Crypto.Hash.SHA1 as SHA1
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char
import Data.List (unfoldr)
import Numeric

import Git.Pack

------------------------------------------------------------
-- From Data.Digest.Pure.SHA

-- |Prints out a bytestring in hexadecimal. Just for convenience.
showDigestBS :: ByteString -> String
showDigestBS bs = foldr paddedShowHex [] (BS.unpack bs)
 where
   paddedShowHex x xs = intToDigit (fromIntegral (x `shiftR` 4))
                      : intToDigit (fromIntegral (x .&. 0xf))
                      : xs


------------------------------------------------------------
-- Read a string as a hex bytestring

readDigestBS :: String -> ByteString
readDigestBS = BS.pack . map (fst . head . readHex) . takeWhile (not . null) . unfoldr (Just . splitAt 2)


------------------------------------------------------------
-- | Compute the object digest for an object.
objectDigest :: PackObjectType -> C.ByteString -> ByteString
objectDigest objType object = SHA1.hashlazy $ C.append prefix object
 where
  lengthStr = (show . C.length) object
  prefix = C.pack $ objectTypeToString objType ++ " " ++ lengthStr ++ "\0"


-- | Compute the object ID for an object.
objectId :: PackObjectType -> C.ByteString -> String
objectId objType object =  showDigestBS $ objectDigest objType object
