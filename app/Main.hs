{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}
import Data.Dynamic

import Data.Dynamic
import Prelude hiding (gcd, max)
import qualified Prelude
import Data.List
import Data.Bool
import Data.Maybe
import qualified Data.Map.Strict as Mp
import qualified Data.Matrix as Mx
import Data.Matrix ((!))
import Brick.Main
import Brick.Types
import Brick.BChan
import Brick.AttrMap
import Brick.Util (fg, bg, on)
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Table
import Brick.Widgets.ProgressBar
import qualified Graphics.Vty.CrossPlatform (mkVty)
import qualified Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Lens.Micro.Platform
import Control.Concurrent.Timer
import Control.Concurrent.Suspend
import System.Exit

import Util
import Action
import qualified Action.Actions as A
import qualified Status.Statuses as S
import MageState
import Cooldown
import Menu

keymap :: Mp.Map Char Action
keymap = Mp.fromList $ zip (Mx.toList keyset)
  [ A.aegis
  , A.phrygis
  , A.solis
  , A.zephis
  , A.cardios
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , A.surgis
  , A.sol
  , A.nox
  , A.impose
  , NoOp
  , NoOp
  , NoOp
  , A.exult
  , A.transpose
  , A.reversal
  , A.secondary
  , A.semitranspose
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , A.lucidity
  , A.invigorate
  , A.enervate
  , A.refulgence
  , NoOp
  , NoOp
  , NoOp
  , A.revival
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  , NoOp
  ]

keyset :: Mx.Matrix Char
keyset = Mx.fromList 3 16
  [ 'q'
  , 'w'
  , 'f'
  , 'p'
  , 'Q'
  , 'W'
  , 'F'
  , 'P'
  , 'l'
  , 'u'
  , 'y'
  , ';'
  , 'L'
  , 'U'
  , 'Y'
  , ':'
  , 'a'
  , 'r'
  , 's'
  , 't'
  , 'A'
  , 'R'
  , 'S'
  , 'T'
  , 'n'
  , 'e'
  , 'i'
  , 'o'
  , 'N'
  , 'E'
  , 'I'
  , 'O'
  , 'z'
  , 'x'
  , 'c'
  , 'v'
  , 'Z'
  , 'X'
  , 'C'
  , 'V'
  , 'm'
  , ','
  , '.'
  , '/'
  , 'M'
  , '<'
  , '>'
  , '?'
  ]

data TickEvent = TickEvent

draw :: MageState -> Widget Name
draw st = center $
  hCenter boss <=>
  hCenter (gcdBar <+> (damageCounter <=> dpsDisplay) <+> elementGauges) <=>
  hCenter 
    ((statusWindow $ st^.statuses) <+>
    ((mpBar <+> reverseIndicator <+> secondaryIndicator) <=> (imposed <+> strikeMeter)) <+>
    aspectGauges) <=>
  hCenter (renderTable $ table [topRow, midRow, botRow])
  where
    boss = borderWithLabel (str $ st^.bossState.currentBossName) $ 
      (cast $ st^.bossState.bossCast) <=>
      (hpbar) <=>
      (statusWindow $ st^.bossState.bossStatuses)
      where
        cast (Just (Cast name cur most _)) =
          border $ padLeftRight 1 $ vLimit 1 $ hLimit 30 $
          updateAttrMap (mapAttrNames [(attrName "castbar:done", progressCompleteAttr), (attrName "castbar:todo", progressIncompleteAttr)]) $
          progressBar (Just name) $ getProgress cur most
        cast Nothing =
          border $ padLeftRight 1 $ vLimit 1 $ hLimit 30 $
          progressBar (Nothing) $ 0.0
        hpbar = do
          let hpPercent = 1.0 - (getProgress (st^.bossState.bossDamage) (st^.bossState.bossMaxHealth))
          border $ padLeftRight 1 $ vLimit 1 $ hLimit 30 $
            updateAttrMap (mapAttrNames [(attrName "hpbar:done", progressCompleteAttr), (attrName "hpbar:todo", progressIncompleteAttr)]) $
            progressBar (Just $ ((showFloatPretty $ hpPercent * 100.0) ++ "%")) hpPercent
    strikeMeter =
      renderTable $ table $
      [[drawStrike i | i <- [0..2]]]
      where
        drawStrike i = padLeftRight 1 $ 
          bool (id) (withAttr (attrName "strike")) ((i <) $ st^.strikes) $ (str " ")
    statusWindow ss =
      renderTable $ table $
      ((:) $ str <$> ["Status Name         ", "Time", "Stacks"]) $
      map status $ ss
      where
        status (TimedStatus timeLeft name _ _) = str <$> [name, show $ ticksToSecs timeLeft, ""]
        status (StackedStatus stacksLeft name _ _) = str <$> [name, "", show stacksLeft]
        status (TimedStackedStatus timeLeft stacksLeft name _ _) = str <$> [name, show $ ticksToSecs timeLeft, show stacksLeft]
    mpBar = 
      border $ padLeftRight 1 $ vLimit 1 $ hLimit 15 $
      updateAttrMap (mapAttrNames [(attrName "mpbar:done", progressCompleteAttr), (attrName "defbar:todo", progressIncompleteAttr)]) $
      progressBar (Just $ show $ st^.actionState.mp) $ (fromIntegral $ st^.actionState.mp) / 10000.0
    imposed =
      renderTable $ table $
      [[imposeIx i | i <- [0..2]]]
      where
        imposeIx i =
          padLeftRight 1 $
          withAttr (maybe (attrName "empty") (attrName . ("element" ++) . show . fromEnum) $ st^?actionState.imposeQueue.(ix i)) $
          str " "
    reverseIndicator = borderWithLabel (str "R") $ padLeftRight 1 $
      withAttr (bool (attrName "reverseindicator:off") (attrName "reverseindicator:on") $ st^.actionState.reverseTransposition) $
      str " "
    secondaryIndicator = borderWithLabel (str "S") $ padLeftRight 1 $
      withAttr (bool (attrName "secondaryindicator:off") (attrName "secondaryindicator:on") $ st^.actionState.secondaryTransposition) $
      str " "
    gcdBar = 
      border $ padLeftRight 1 $ vLimit 1 $ hLimit 10 $
      updateAttrMap (mapAttrNames [(attrName "defbar:done", progressCompleteAttr), (attrName "defbar:todo", progressIncompleteAttr)]) $
      progressBar (Just $ show $ st^?!cds._head.counter) $ getProgress (st^?!cds._head.counter) (st^?!cds._head.max)
    damageCounter =
      borderWithLabel (str "Damage") $ str $ padOutStrLeft 10 $ show $ st^.totalDamage 
    dpsDisplay = 
      borderWithLabel (str "DPS") $ str $ padOutStrLeft 10 $
      showFloatPretty $ averageDamages $ st^.prevTickDamages
    elementGauges = renderTable $ table [elementGauge elId | elId <- [0..5]]
      where
        elementGauge elId =
          (flip (++)) (bool (id) (withAttr (attrName ("element" ++ show elId)) <$>) (st^?!actionState.flaggedElements.(ix elId)) [str " "])$
          (flip (++)) (bool ([str " "]) ([str "S"]) ((fromEnum $ st^.actionState.secondaryElement) == elId)) $
          (flip (++)) (bool ([str " "]) ([str "P"]) ((fromEnum $ st^.actionState.primaryElement) == elId)) $
          return $ padLeftRight 1 $ vLimit 1 $ hLimit 10 $
          updateAttrMap (mapAttrNames [(attrName ("element" ++ show elId), progressCompleteAttr), (attrName ("empty"), progressIncompleteAttr)]) $
          progressBar (Just $ show $ st^?!actionState.elementLevels.(ix elId)) $ ((fromIntegral $ st^?!actionState.elementLevels.(ix elId)) / 100.0)
    aspectGauges = renderTable $ table [aspectGauge acId | acId <- [0..1]]
      where
        aspectGauge acId =
          (flip (++)) (bool ([str " "]) ([str "S"]) ((fromEnum $ getAspect $ st^.actionState.secondaryElement) == acId)) $
          (flip (++)) (bool ([str " "]) ([str "P"]) ((fromEnum $ getAspect $ st^.actionState.primaryElement) == acId)) $
          return $ padLeftRight 1 $ vLimit 1 $ hLimit 10 $
          updateAttrMap (mapAttrNames [(attrName ("aspect" ++ show acId), progressCompleteAttr), (attrName ("empty"), progressIncompleteAttr)]) $
          progressBar (Just $ show $ st^?!actionState.aspectLevels.(ix acId)) $ ((fromIntegral $ st^?!actionState.aspectLevels.(ix acId)) / 1000.0)
    topRow = [padLeftRight 1 $ showAction $ keyset ! (1,c) | c <- [1..16]]
    midRow = [padLeftRight 1 $ showAction $ keyset ! (2,c) | c <- [1..16]]
    botRow = [padLeftRight 1 $ showAction $ keyset ! (3,c) | c <- [1..16]]
    showAction k = (flip $ maybe (str "?")) (Mp.lookup k keymap) $ display
      where
        display (GCDAction _ _) = str [k]
        display (CDAction cdId _ _) = do
          let cd = fromJust $ st^?cds.(ix cdId) :: Cooldown
          bool (str [k]) (str $ show $ ticksToSecs (cd^.max - cd^.counter)) $ cd^.active
        display (InstantAction _ _) = str [k]
        display (MultiAction decide) = display $ decide st
        display NoOp = str "*"

handleEvent :: BrickEvent Name TickEvent -> EventM Name MageState ()
handleEvent (VtyEvent (EvKey (KEsc) _)) = halt
handleEvent (VtyEvent (EvKey (KChar k) _)) = do
  maybe (return ()) (doAction) $ Mp.lookup k keymap
handleEvent (AppEvent TickEvent) = do
  use statuses >>= fmap catMaybes . mapM (upkeepStatus) >>= assign statuses

  cds %= over traverse (handleCD)
  damage <- use $ tickState.tickDamage
  bool
    (totalDamage += damage >> bossState.bossDamage += damage)
    (return ())
    =<< (use $ tickState.damageLock)
  bool
    ((actionState.mp +=) =<< (use $ tickState.tickMPGain))
    (return ())
    =<< (use $ tickState.mpGainLock)

  overlight <- (isJust . find (\status -> status^.name == "Light Suffused")) <$> use statuses
  overdark <- (isJust . find (\status -> status^.name == "Dark Suffused")) <$> use statuses
  
  zoom (actionState.aspectLevels) $ do
    if overlight then return ()
    else do
      (ix 0) += 1
      (ix 0) %= min 1000
    if overdark then return ()
    else do
      (ix 1) -= 1
      (ix 1) %= Prelude.max 0

  bool
    (return ())
    (statuses %= (:) S.lightSuffused)
    =<< ((&& not overlight) . (== 1000) . fromJust) <$> (preuse $ actionState.aspectLevels.(ix 0))
  bool
    (return ())
    (statuses %= (:) S.darkSuffused)
    =<< ((&& not overdark) . (== 0) . fromJust) <$> (preuse $ actionState.aspectLevels.(ix 1))

  tickDamages <- use $ prevTickDamages
  if length tickDamages == 300
  then
    prevTickDamages .= damage:(init tickDamages)
  else
    prevTickDamages .= damage:tickDamages

  use (bossState.bossStatuses) >>= fmap catMaybes . mapM (upkeepStatus) >>= assign (bossState.bossStatuses)

  b <- use $ bossState.currentBoss
  b
  bd <- use $ bossState.bossDamage
  bmh <- use $ bossState.bossMaxHealth

  if bd >= bmh
  then do
    bosses <- use bossList
    case bosses of
      (nb:nbs) -> do
        bossState .= initBossState nb
        bossList .= nbs
      [] -> halt
  else return ()

  bossState.bossCast._Just.castTicksPassed += 1
  curCast <- use $ bossState.bossCast
  case curCast of
    Just (Cast _ cticks ctickcap effect) -> 
      if cticks >= ctickcap
      then do
        effect
        bossState.bossCast .= Nothing
      else return ()
    Nothing -> return ()

  numStrikes <- use strikes
  if numStrikes >= 3
  then halt
  else return ()
  
  actionState.mp %= min 10000
  tickState .= initTickState
  elapsedTicks += 1
  where
    handleCD :: Cooldown -> Cooldown
    handleCD cd@(Cooldown curActive curCD curMax) = do
      if curActive
      then do
        if curCD == curMax
        then (Cooldown False 0 curMax)
        else (Cooldown curActive (curCD + 1) curMax)
      else cd
handleEvent _ = return ()

app :: BossSetup -> App MageState TickEvent Name
app firstBossSetup =
  App { appDraw = (:[]) . draw
      , appChooseCursor = const $ const Nothing
      , appHandleEvent = handleEvent
      , appStartEvent = setup firstBossSetup
      , appAttrMap = attrs
      }

setup :: BossSetup -> EventM Name MageState ()
setup (BossSetup _ _ _ firstBossSetup) = do
  internalState <- firstBossSetup
  bossState.bossInternalState .= internalState

attrs :: MageState -> AttrMap
attrs _ = attrMap defAttr
  [ (attrName "defbar:done", black `on` blue)
  , (attrName "mpbar:done", bg blue)
  , (attrName "castbar:done", black `on` blue)
  , (attrName "hpbar:done", bg green)
  , (attrName "reverseindicator:on", bg red)
  , (attrName "secondaryindicator:on", bg magenta)
  , (attrName "element0", bg red)
  , (attrName "element1", bg yellow)
  , (attrName "element2", bg blue)
  , (attrName "element3", bg cyan)
  , (attrName "element4", bg green)
  , (attrName "element5", bg magenta)
  , (attrName "aspect0", black `on` white)
  , (attrName "aspect1", white `on` black)
  , (attrName "strike", bg red)
  , (attrName "empty", defAttr)
  ]

main :: IO ()
main = do
  config <- runMenu

  if isNothing config
  then exitSuccess
  else return ()

  let (Config bossSelection) = fromJust config

  tickChan <- newBChan 10 :: IO (BChan TickEvent)
  let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  initialVty <- buildVty

  timer <- repeatedTimer (sendTick tickChan) (msDelay 100)
  finalState <- customMain initialVty buildVty (Just tickChan) (app $ head bossSelection) (initState bossSelection)
  stopTimer timer

  putStrLn $ "Finished in: " ++ show ((fromIntegral $ finalState^.elapsedTicks) / 10.0) ++ " ticks"
  putStrLn $ "Final DPS: " ++ show (averageDamages $ finalState^.prevTickDamages)

  return ()
  where
    sendTick :: BChan TickEvent -> IO ()
    sendTick chan = writeBChan chan TickEvent
