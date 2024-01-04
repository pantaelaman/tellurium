{-# LANGUAGE TemplateHaskell #-}

module Cooldown where

import Lens.Micro.Platform

data Cooldown = Cooldown {_active :: Bool, _counter :: Int, _max :: Int}
makeLenses ''Cooldown

newCD :: Int -> Cooldown
newCD = Cooldown False 0
