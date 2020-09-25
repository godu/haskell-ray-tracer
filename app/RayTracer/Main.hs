module Main
  ( main,
  )
where

import qualified RayTracer.Chapter1 as Chapter1
  ( main,
  )
import qualified RayTracer.Chapter2 as Chapter2
  ( main,
  )
import qualified RayTracer.Chapter3 as Chapter3
  ( main,
  )
import qualified RayTracer.Chapter4 as Chapter4
  ( main,
  )
import qualified RayTracer.Chapter5 as Chapter5
  ( main,
  )
import qualified RayTracer.Chapter6 as Chapter6
  ( main,
  )
import System.Environment (getArgs)
import Prelude
  ( IO,
    String,
    print,
    show,
    ($),
    (<>),
    (=<<),
  )

main :: IO ()
main = run =<< getArgs

run :: [String] -> IO ()
run ("1" : _) = Chapter1.main
run ("2" : _) = Chapter2.main
run ("3" : _) = Chapter3.main
run ("4" : _) = Chapter4.main
run ("5" : _) = Chapter5.main
run ("6" : _) = Chapter6.main
run args = print $ "Chapter not found: " <> show args
