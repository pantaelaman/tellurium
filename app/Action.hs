{-# LANGUAGE TemplateHaskell #-}

module Action where

import Data.Maybe
import Brick.Types
import Lens.Micro.Platform

import MageState
import Cooldown

type ActionEffect = EventM Name MageState ()
type ActionCondition = EventM Name MageState Bool

data Action = NoOp | GCDAction ActionCondition ActionEffect | CDAction Int ActionCondition ActionEffect | InstantAction ActionCondition ActionEffect | MultiAction (MageState -> Action)

doAction :: Action -> EventM Name MageState ()
doAction (GCDAction condition effect) = do
  willWork <- condition
  if willWork
  then do
    curCDs <- use cds
    if fromJust $ curCDs^?_head.active
    then return ()
    else do
      effect
      zoom cds $ do
        _head.counter .= 0
        _head.active .= True
  else return ()
doAction (CDAction cdId condition effect) = do
  curCDs <- use cds
  if fromJust $ curCDs^?(ix cdId).active
  then return ()
  else do
    willWork <- condition
    if willWork
    then do
      effect
      zoom cds $ do
        (ix cdId).counter .= 0
        (ix cdId).active .= True
    else return ()
doAction (InstantAction condition effect) = do
  willWork <- condition
  if willWork
  then effect
  else return ()
doAction (MultiAction decide) = do
  (doAction . decide) =<< get
doAction NoOp = return ()
