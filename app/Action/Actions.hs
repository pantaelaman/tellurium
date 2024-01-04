module Action.Actions where

import Data.Bool
import Data.Maybe
import Data.List
import Brick.Types
import Lens.Micro.Platform

import MageState
import Action
import Status.Statuses
import qualified Status.Statuses as S

noCondition :: ActionCondition
noCondition = return True

mpCost :: Int -> ActionCondition
mpCost cost = do
  curMP <- use $ actionState.mp
  if curMP >= cost
  then do
    actionState.mp -= cost
    return True
  else return False

elementCost :: Element -> Int -> ActionCondition
elementCost element cost = do
  curEl <- (!! (fromEnum element)) <$> (use $ actionState.elementLevels) 
  if curEl >= cost
  then do
    incrementElement element $ -cost
    return True
  else return False

primaryElementCost :: Int -> ActionCondition
primaryElementCost cost = do
  primaryAmount <- getPrimaryElement
  if primaryAmount >= cost
  then do
    incrementPrimaryElement $ (-cost)
    return True
  else return False

secondaryElementCost :: Int -> ActionCondition
secondaryElementCost cost = do
  secondaryAmount <- getSecondaryElement
  if secondaryAmount >= cost
  then do
    incrementSecondaryElement $ -cost
    return True
  else return False

stackCost :: [Char] -> Int -> ActionCondition
stackCost targetName cost = do
  statusIx <- (findIndex (\status -> status^.name == targetName)) <$> use statuses
  case statusIx of
    Just i -> do
      stackAmount <- fromJust <$> (preuse $ statuses.(ix i).stacksLeft )
      if stackAmount >= cost
      then do
        statuses.(ix i).stacksLeft -= cost
        return True
      else return False
    Nothing -> return False

---- Abilities, listed in order of the keymap

aegis :: Action
aegis = GCDAction (mpCost 100) $ do
  tickState.tickDamage += 100
  actionState.comboAction .= Aegis
  opposePrimaryAspect 10
  incrementPrimaryElement 5

phrygis :: Action
phrygis = GCDAction (mpCost 200) $ do
  curCombo <- use $ actionState.comboAction
  if curCombo == Aegis
  then do
    tickState.tickDamage += 150
    actionState.comboAction .= Phrygis
    opposePrimaryAspect 10
    incrementPrimaryElement 5
    decrementPrimaryTrigonalElements 5
  else do
    tickState.tickDamage += 15
    actionState.comboAction .= NoCombo

solis :: Action
solis = GCDAction (mpCost 300) $ do
  curCombo <- use $ actionState.comboAction
  if curCombo == Phrygis
  then do
    tickState.tickDamage += 200
    actionState.comboAction .= Solis
    opposePrimaryAspect 10
    incrementPrimaryElement 10
    decrementPrimaryTrigonalElements 10
  else do
    tickState.tickDamage += 20
    actionState.comboAction .= NoCombo

zephis :: Action
zephis = GCDAction (mpCost 400) $ do
  curCombo <- use $ actionState.comboAction
  if curCombo == Solis
  then do
    tickState.tickDamage += 400
    opposePrimaryAspect 10
    incrementPrimaryElement 20
    decrementPrimaryTrigonalElements 10
  else tickState.tickDamage += 40
  actionState.comboAction .= NoCombo

cardios :: Action
cardios = CDAction 1 (primaryElementCost 70) $ do
  tickState.tickDamage += 1000
  flagPrimaryElement

surgis :: Action
surgis = CDAction 2 (secondaryElementCost 50) $ do
  alignment <- focusesAlign
  if alignment
  then statuses %= (:) (S.damageUp 0.15 200)
  else statuses %= (:) (S.mpRegenUp 50 200)
  flagSecondaryElement

transpose :: Action
transpose = MultiAction $ \st ->
  if st^.actionState.secondaryTransposition
  then duotranspose
  else protranspose 

protranspose :: Action
protranspose = CDAction 5 noCondition $ do
  transposeOp <- (bool (transposeElement) (revtransposeElement)) <$> (use $ actionState.reverseTransposition)
  actionState.primaryElement %= transposeOp
  actionState.reverseTransposition .= False

duotranspose :: Action
duotranspose = CDAction 7 noCondition $ do
  transposeOp <- (bool (transposeElement) (revtransposeElement)) <$> (use $ actionState.reverseTransposition)
  actionState.secondaryElement %= transposeOp
  actionState.reverseTransposition .= False
  actionState.secondaryTransposition .= False

semitranspose :: Action
semitranspose = MultiAction $ \st ->
  if st^.actionState.secondaryTransposition
  then duosemitranspose
  else prosemitranspose

prosemitranspose :: Action
prosemitranspose = CDAction 6 noCondition $ do
  transposeOp <- (bool (semitransposeElement) (revsemitransposeElement)) <$> (use $ actionState.reverseTransposition)
  actionState.primaryElement %= transposeOp
  actionState.reverseTransposition .= False

duosemitranspose :: Action
duosemitranspose = CDAction 8 noCondition $ do
  transposeOp <- (bool (semitransposeElement) (revsemitransposeElement)) <$> (use $ actionState.reverseTransposition)
  actionState.secondaryElement %= transposeOp
  actionState.reverseTransposition .= False
  actionState.secondaryTransposition .= False

reversal :: Action
reversal = InstantAction noCondition $ do
  actionState.reverseTransposition %= not

secondary :: Action
secondary = InstantAction noCondition $ do
  actionState.secondaryTransposition %= not

sol :: Action
sol = CDAction 3 (stackCost "Light Suffused" 1) $ do
  incrementAspectedTrigonal Astral 5
  actionState.aspectLevels.(ix 0) %= (max 0 . (subtract 200))

nox :: Action
nox = CDAction 3 (stackCost "Dark Suffused" 1) $ do
  incrementAspectedTrigonal Umbral 5
  actionState.aspectLevels.(ix 1) %= (min 1000 . (+ 200))

impose :: Action
impose = CDAction 4 imposeCondition $ do
  secondary <- use $ actionState.secondaryElement
  actionState.imposeQueue %= (++) [secondary]
  where
    imposeCondition :: ActionCondition
    imposeCondition = do
      numImposed <- length <$> (use $ actionState.imposeQueue)
      if numImposed < 3
      then secondaryElementCost 25
      else (S.stall) >> return False

exult :: Action
exult = InstantAction exultCondition $ do
  imposed <- use $ actionState.imposeQueue
  mapM (flagElement) imposed 
  let elements = nub imposed
  let aspects = nub $ (getAspect <$> imposed)
  if length elements == 1
  then do
    statuses %= (:) (S.exaltedElement (head imposed))
    actionState.elementLevels.(ix $ fromEnum $ head imposed) .= 100
  else if length aspects == 1 && length elements == 3
  then do
    opposeAspect (head aspects) 200
    tickState.tickDamage += 1000
  else (S.stall) >> return ()
  actionState.imposeQueue .= []
  where
    exultCondition :: ActionCondition
    exultCondition = do
      numImposed <- length <$> (use $ actionState.imposeQueue)
      if numImposed == 3
      then return True
      else (S.stall) >> return False

lucidity :: Action
lucidity = CDAction 9 noCondition $ do
  statuses %= (:) (S.mpRegenUp 50 200)

invigorate :: Action
invigorate = CDAction 10 (primaryElementCost 50) $ do
  secondaryAspect <- getAspect <$> (use $ actionState.secondaryElement)
  actionState.aspectLevels.(ix $ fromEnum secondaryAspect) %= (min 1000 . (+ 300))
  flagPrimaryElement

enervate :: Action
enervate = CDAction 10 (primaryElementCost 50) $ do
  secondaryAspect <- getAspect <$> (use $ actionState.secondaryElement)
  actionState.aspectLevels.(ix $ fromEnum secondaryAspect) %= (max 0 . (subtract 300))
  flagPrimaryElement
  
refulgence :: Action
refulgence = InstantAction condition $ do
  actionState.flaggedElements %= (False <$)
  opposeAspect Astral 500
  opposeAspect Umbral 500
  elements <- use $ actionState.elementLevels
  tickState.tickDamage += 2000 + (10 * sum elements)
  actionState.elementLevels %= (0 <$)
  where
    condition = do
      flagged <- use $ actionState.flaggedElements
      return $ foldl (&&) True flagged

revival :: Action
revival = InstantAction condition $ do
  actionState.flaggedElements %= (False <$)
  opposeAspect Astral 500
  opposeAspect Umbral 500
  strikes -= 1 
  where
    condition = do
      flagged <- use $ actionState.flaggedElements
      return $ foldl (&&) True flagged
