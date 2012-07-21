{-# OPTIONS -Wall #-}

module Git.SHA (
    showDigestBS,
    readDigestBS,
    sha1Blob
) where

import Crypto.Hash.SHA1 as SHA1
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char
import Data.List (unfoldr)
import Numeric

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


-- | Compute the SHA1 hash of a blob.
sha1Blob :: LC.ByteString -> ByteString
sha1Blob blob = SHA1.hashlazy $ LC.append prefix blob
 where
  lengthStr = (show . LC.length) blob
  prefix = LC.pack $ "blob " ++ lengthStr ++ "\0"
