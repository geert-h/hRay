module Linearisible where

class Linearisible obj where
  (.*) :: obj -> Double -> obj
  (./) :: obj -> Double -> obj
  size :: obj -> Double
  normalize :: obj -> obj
  dot :: obj -> obj -> Double
  maxL :: obj -> Double
  cross :: obj -> obj -> obj