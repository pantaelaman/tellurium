{-# LANGUAGE TemplateHaskell #-}

module Boss.Bosses where

import Lens.Micro.Platform
import Data.Maybe
import Brick.Types
import Data.Dynamic
import Data.Dynamic.Lens
import MageState
import Status.Statuses
import System.Random.Stateful
import Control.Monad.IO.Class (liftIO)

cast :: Cast -> EventM Name MageState ()
cast c = bossState.bossCast ?= c

trainingDummy :: BossSetup
trainingDummy = BossSetup "Training Dummy" 1000000 mechs init
  where
    init = do
      return $ toDyn ""
    mechs = do
      return ()

elementalAdept :: BossSetup
elementalAdept = BossSetup "Elemental Adept" 25000 (mechs) (init)
  where
    init = do
      return $ toDyn (0 :: Int)
    mechs = do
      totalTicks <- use elapsedTicks

      if totalTicks `rem` 600 == 300
      then castShield
      else return ()
      
      damage <- use totalDamage 
      return ()
    castShield = do
      randElement <- (toEnum . (`rem` 6)) <$> (liftIO $ (uniformM globalStdGen :: IO Int))
      bossState.bossInternalState._Dynamic .= (0 :: Int)
      cast $ shield randElement 
      return ()
    shield :: Element -> Cast
    shield el = newCast ("Shielding " ++ show el) 180 $ do
      bossState.bossStatuses %= (:) (shielded el)
    shielded :: Element -> Status
    shielded el = TimedStatus 400 ("Shielded: " ++ show el) (cast $ elementalAttack el) $ do
      primary <- use $ actionState.primaryElement 
      if primary == el
      then do
        damage <- use $ tickState.tickDamage
        bossState.bossInternalState._Dynamic += damage
      else if getAspect primary == getAspect el
      then do
        tickState.tickDamage *= 2
      else return ()
    elementalAttack :: Element -> Cast
    elementalAttack el = newCast (show el ++ " Counterattack") 20 $ do
      damage <- fromJust <$> (preuse $ bossState.bossInternalState._Dynamic) :: EventM Name MageState Int
      if damage > 0
      then do
        defended <- isJust <$> isStatusActive (== ("Exalted " ++ show el))
        if not defended
        then strikes += 1
        else return ()
      else return ()

