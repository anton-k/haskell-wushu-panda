module Exam.Utils.Aeson(
  dropPrefixOptions
) where

import Data.Aeson.TH

dropPrefixOptions :: Options
dropPrefixOptions = defaultOptions { fieldLabelModifier = dropTick }
  where
    dropTick name =
      case dropWhile (/= '\'') name of
        []     -> name
        _:rest -> rest
