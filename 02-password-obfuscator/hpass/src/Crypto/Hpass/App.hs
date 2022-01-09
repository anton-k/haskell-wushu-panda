module Crypto.Hpass.App(
  Hpass(..),
  runHpass,
) where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.List.Split (splitOn)
import Data.Text.Short (ShortText, toByteString, unpack)
import qualified Data.List as L
import Crypto.Argon2 (hashEncoded, HashOptions)
import System.Hclip (setClipboard)

data Hpass = Hpass
  { hpass'masterPassword  :: ShortText
  , hpass'secret          :: ShortText
  , hpass'options         :: HashOptions
  }
  deriving (Show)

deriveKey :: Hpass -> Either String ShortText
deriveKey Hpass{..} = bimap show id $
  hashEncoded hpass'options (toByteString hpass'masterPassword) (toByteString hpass'secret)

stripParams :: String -> String
stripParams str = L.intercalate "$" $ drop 5 $ splitOn "$" str

runHpass :: Hpass -> IO ()
runHpass cfg =
  case deriveKey cfg of
    Right key -> setClipboard $ stripParams $ unpack key
    Left err  -> putStrLn err

