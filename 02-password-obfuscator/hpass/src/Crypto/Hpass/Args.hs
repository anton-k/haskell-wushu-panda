module Crypto.Hpass.Args(
  readHpass,
) where

import Options.Applicative
import Crypto.Argon2 (defaultHashOptions)
import qualified Data.Text.Short as T
import Crypto.Hpass.App (Hpass(..))
import System.IO
import Control.Exception

data Args = Args
  { args'secret  :: String
  , args'options :: Maybe FilePath
  }

fromArgs :: Args -> IO Hpass
fromArgs Args{..} = do
  hpass'options <- maybe (pure defaultHashOptions) (fmap read . readFile) args'options
  let hpass'secret = T.pack args'secret
  hpass'masterPassword <- T.pack <$> getPassword
  pure Hpass{..}

readHpass :: IO Hpass
readHpass = fromArgs =<< execParser opts
  where
    opts = info (preArgs <**> helper)
      ( fullDesc
     <> progDesc "Password obfuscator"
     <> header "Hpass generates strong password out of single master password" )

preArgs :: Parser Args
preArgs = Args
  <$> (fromSecret <$> getSite)
  <*> getOptions
  where
    getSite = argument str
          ( metavar "SITE"
         <> help "Web site")

    getOptions = optional $ strOption
          ( long "hash-config"
         <> short 'h'
         <> metavar "STRING"
         <> help "Path to hash config file")

fromSecret :: String -> String
fromSecret site = take 100 $ cycle site

-------------------------------------------------------------------------

-- | Read the password without echoing the input
getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
