{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Print
import Protolude hiding (Show, show, print)

data Animal 
  = Dog
  | Cat
  deriving (Generic, Show)

data T1 
  = T1 Int Bool
  deriving (Generic, Show)

data T2 
  = T2 Int Bool
  | T3 { x :: Bool, y :: Int }
  deriving (Generic, Show)

data B a = B
  { first :: Int
  , second :: a
  } deriving (Generic, Show)

data I a b  = a :. b 
  deriving (Generic, Show)

main :: IO ()
main = do
  print [Cat, Dog]
  print [0 :: Int,5..100]
  print (T1 42 False)
  print (T2 1 True, B 1 (T3 False 3))
  print (B 3 [Cat, Dog])
  print (show (0.5 :: Double) == show ((1/2) :: Double))
  print ("継続は力なり" :: Text)
  print ("To be or not to be.\nThat is the question." :: Text)
  putStrLn (show (3.1415926535 :: Double))
