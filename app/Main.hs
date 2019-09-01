module Main
  ( main
  ) where

import qualified Data.Text.IO as T.IO
import qualified Lib
import Prelude (IO, (>>=))

main :: IO ()
main = Lib.dockerfile "dockerfiles" >>= T.IO.putStrLn
