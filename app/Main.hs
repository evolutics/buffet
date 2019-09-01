module Main
  ( main
  ) where

import qualified Data.Text.IO as T.IO
import qualified Lib
import Prelude (IO, (>>=))

main :: IO ()
main = Lib.build "dockerfiles" >>= T.IO.putStrLn
