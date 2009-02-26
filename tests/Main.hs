module Main (main) where

import Properties (tests)

import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain tests