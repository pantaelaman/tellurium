module Status.Statuses where

import Data.Maybe
import Data.List
import Brick.Types
import Lens.Micro.Platform

import MageState
import Util

type Effect = EventM Name MageState ()

incrementElement :: Element -> Int -> Effect
incrementElement element amount = do
  actionState.elementLevels.(ix $ fromEnum element) %= (min 100 . max 0 . (+ amount))

incrementPrimaryElement :: Int -> Effect
incrementPrimaryElement amount = do
  primaryAmount <- getPrimaryElement
  if primaryAmount == 100 && amount > 0
  then stall
  else do
    primary <- use $ actionState.primaryElement
    incrementElement primary amount

incrementSecondaryElement :: Int -> Effect
incrementSecondaryElement amount = do
  secondary <- use $ actionState.secondaryElement
  incrementElement secondary amount

decrementTrigonalElements :: Element -> Int -> Effect
decrementTrigonalElements element amount = do
  incrementElement (transposeElement element) (-amount)
  incrementElement (revtransposeElement element) (-amount)

decrementPrimaryTrigonalElements :: Int -> Effect
decrementPrimaryTrigonalElements amount = do
  primary <- use $ actionState.primaryElement
  decrementTrigonalElements primary amount

incrementAspectedTrigonal :: Aspect -> Int -> Effect
incrementAspectedTrigonal aspect amount = do
  mapM_ (flip incrementElement amount)
    [toEnum ix | ix <- [0..5], (getAspect $ toEnum ix) == aspect]

opposeAspect :: Aspect -> Int -> Effect
opposeAspect Astral amount = do
  actionState.aspectLevels.(ix 0) %= (max 0 . (subtract amount))
opposeAspect Umbral amount = do
  actionState.aspectLevels.(ix 1) %= (min 1000 . (+ amount))

opposePrimaryAspect :: Int -> Effect
opposePrimaryAspect amount = do
  aspect <- getAspect <$> (use $ actionState.primaryElement)
  opposeAspect aspect amount

flagElement :: Element -> Effect
flagElement element =
  actionState.flaggedElements.(ix $ fromEnum element) .= True 

flagPrimaryElement :: Effect
flagPrimaryElement =
  flagElement =<< (use $ actionState.primaryElement)

flagSecondaryElement :: Effect
flagSecondaryElement =
  flagElement =<< (use $ actionState.secondaryElement)

getPrimaryElement :: EventM n MageState Int
getPrimaryElement = do
  primary <- fromEnum <$> (use $ actionState.primaryElement)
  st <- get 
  return $ fromJust $ st^?actionState.elementLevels.(ix primary) 

getSecondaryElement :: EventM n MageState Int
getSecondaryElement = do
  secondary <- fromEnum <$> (use $ actionState.secondaryElement)
  st <- get
  return $ fromJust $ st^?actionState.elementLevels.(ix secondary)

focusesAlign :: EventM n MageState Bool
focusesAlign = do
  primary <- use $ actionState.primaryElement
  secondary <- use $ actionState.secondaryElement
  return $ areTrigonallyAligned primary secondary 

--

isStatusActive :: ([Char] -> Bool) -> EventM Name MageState (Maybe Int)
isStatusActive isMatch = do
  findIndex (isMatch . view name) <$> use statuses

stall :: Effect
stall = do
  prevStalled <- isStatusActive (== "Stalled")
  case prevStalled of
    Just i -> statuses.(ix i).timeLeft .= 100
    Nothing -> statuses %= (:) stalled

---- Statuses

damageUp :: Float -> Int -> Status
damageUp percent time = TimedStatus time ("Damage ⇑ " ++ showFloatPretty (percent * 100) ++ "%") (return ()) $ do
  tickState.tickDamage %= floor . (* percent) . fromIntegral

damageDown :: Float -> Int -> Status
damageDown percent time = TimedStatus time ("Damage ⇓ " ++ showFloatPretty (percent * 100) ++ "%") (return ()) $ do
  tickState.tickDamage %= floor . (* percent) . fromIntegral

mpRegenUp :: Int -> Int -> Status
mpRegenUp amount time = TimedStatus time ("MP Regen ⇑") (return ()) $ do
  tickState.tickMPGain += amount

mpRegenDown :: Int -> Int -> Status
mpRegenDown amount time = TimedStatus time ("MP Regen ⇓") (return ()) $ do
  tickState.tickMPGain -= amount

stalled :: Status
stalled = TimedStatus 100 ("Stalled") (return ()) $ do
  tickState.mpGainLock .= True
  tickState.tickDamage %= floor . (* 0.1) . fromIntegral

lightSuffused :: Status
lightSuffused = TimedStackedStatus 300 5 ("Light Suffused") (return ()) $ do
  tickState.mpGainLock .= True
  primary <- use $ actionState.primaryElement
  if getAspect primary /= Astral
  then tickState.damageLock .= True
  else do
    damage <- use $ tickState.tickDamage
    if damage > 0
    then zoom (actionState.aspectLevels) $ do
      (ix $ fromEnum Astral) %= (min 1000 . (+ 5))
    else return ()

darkSuffused :: Status
darkSuffused = TimedStackedStatus 300 5 ("Dark Suffused") (return ()) $ do
  tickState.mpGainLock .= True
  primary <- use $ actionState.primaryElement
  if getAspect primary /= Umbral
  then tickState.damageLock .= True
  else do
    damage <- use $ tickState.tickDamage
    if damage > 0
    then zoom (actionState.aspectLevels) $ do
      (ix $ fromEnum Umbral) %= (max 0 . (subtract 5))
    else return ()

exaltedElement :: Element -> Status
exaltedElement element = TimedStatus 100 ("Exalted " ++ show element) (return ()) $ do
    tickState.tickDamage += 2

exaltedAspect :: Aspect -> Status
exaltedAspect aspect = TimedStatus 100 ("Exalted " ++ show aspect) expire $ do
    opposeAspect aspect 10
    tickState.tickDamage += 4
  where
    expire = do
      case aspect of
        Astral -> statuses %= (:) lightSuffused
        Umbral -> statuses %= (:) darkSuffused
