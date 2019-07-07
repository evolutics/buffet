module Main
  ( main
  ) where

import qualified Data.Text.IO as T.IO
import qualified Lib (dockerfile)
import Prelude (IO)

main :: IO ()
main = T.IO.putStrLn Lib.dockerfile
