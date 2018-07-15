# Example

```
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.OrgMode.Print
import qualified Data.Attoparsec.Text as APS
import qualified Data.Text.IO as TIO
import Data.OrgMode.Parse.Attoparsec.Document
import Text.Pretty.Simple (pPrint)
import Control.Monad

main :: IO ()
main = do
  f <- TIO.readFile "example.org"
  case APS.parseOnly (parseDocument [""]) (f) of
    Right doc -> do
      pPrint doc
      forM_ (formatDoc doc) TIO.putStrLn
    Left e -> error e
```
