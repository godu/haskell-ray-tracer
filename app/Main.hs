module Main (main) where

import Data.Foldable (traverse_)
import qualified RayTracer.Chapter1 as Chapter1 (main)
import qualified RayTracer.Chapter10 as Chapter10 (main)
import qualified RayTracer.Chapter2 as Chapter2 (main)
import qualified RayTracer.Chapter3 as Chapter3 (main)
import qualified RayTracer.Chapter4 as Chapter4 (main)
import qualified RayTracer.Chapter5 as Chapter5 (main)
import qualified RayTracer.Chapter6 as Chapter6 (main)
import qualified RayTracer.Chapter7 as Chapter7 (main)
import qualified RayTracer.Chapter8 as Chapter8 (main)
import qualified RayTracer.Chapter9 as Chapter9 (main)
import System.Environment (getArgs)

main :: IO ()
main = run =<< getArgs

run :: [String] -> IO ()
run ("1" : _) = traverse_ print Chapter1.main
run ("2" : _) = writeFile "./.output/chapter-2.ppm" $ unlines Chapter2.main
run ("3" : _) = traverse_ print Chapter3.main
run ("4" : _) = writeFile "./.output/chapter-4.ppm" $ unlines Chapter4.main
run ("5" : _) = writeFile "./.output/chapter-5.ppm" $ unlines Chapter5.main
run ("6" : _) = writeFile "./.output/chapter-6.ppm" $ unlines Chapter6.main
run ("7" : _) = writeFile "./.output/chapter-7.ppm" $ unlines Chapter7.main
run ("8" : _) = writeFile "./.output/chapter-8.ppm" $ unlines Chapter8.main
run ("9" : _) = writeFile "./.output/chapter-9.ppm" $ unlines Chapter9.main
run ("10" : _) = writeFile "./.output/chapter-10.ppm" $ unlines Chapter10.main
run args = print $ "Chapter not found: " <> show args
