{-# LANGUAGE TemplateHaskell #-}

module Menu (runMenu, Config (..)) where

import Data.Bool
import Data.Vector (fromList)
import Brick.Main
import Brick.AttrMap
import Brick.Types
import Brick.Util (on)
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.List
import Lens.Micro.Platform
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

import MageState (Boss, BossSetup (..))
import qualified Boss.Bosses as B
import Util

data Name = StartRaid | BossSelect | Quit | BossList
  deriving (Ord, Eq, Show, Enum)

data MenuState = MenuState
  { _selected :: Name
  , _bossList :: List Name BossSetup
  , _config :: Maybe Config
  }
data Config = Config
  { _bossSelection :: [BossSetup]
  }

makeLenses ''MenuState
makeLenses ''Config

draw :: MenuState -> [Widget Name]
draw st@(MenuState {_selected = BossList}) = return $ center $ border $ vLimit 10 $ hLimit 20 $
  renderList bossEntry True (st^.bossList)
  where
    bossEntry active (BossSetup name _ _ _) = bool (id) (withAttr (attrName "selected")) active $ str $ padOutStrRight 20 $ name
draw st = return $ center $
  (border $ named StartRaid $ str $ padOutStrRight 30 $ "Start Raid") <=>
  (border $ named BossSelect $ str $ padOutStrRight 30 "Boss Select") <=>
  (border $ named Quit $ str $ padOutStrRight 30 "Quit")
  where
    named :: Name -> Widget Name -> Widget Name
    named name
      | name == st^.selected = withAttr (attrName "selected")
      | otherwise = id

handleEvent :: BrickEvent Name () -> EventM Name MenuState ()
handleEvent (VtyEvent (EvKey (KEsc) _)) = config .= Nothing >> halt
handleEvent (VtyEvent (EvKey (KEnter) _)) = do
  curselect <- use selected
  case curselect of
    StartRaid -> do
      config ?= (Config $ tail allBosses)
      halt
    BossSelect -> selected .= BossList
    Quit -> config .= Nothing >> halt
    BossList -> do
      sel <- preuse $ bossList.listSelectedElementL
      config .= (Config . return <$> sel)
      halt
handleEvent (VtyEvent ev@(EvKey (KUp) _)) = do
  curselect <- use selected
  case curselect of
    StartRaid -> return ()
    BossList -> zoom bossList $ handleListEvent ev
    _ -> selected %= pred
handleEvent (VtyEvent ev@(EvKey (KDown) _)) = do
  curselect <- use selected
  case curselect of
    Quit -> return ()
    BossList -> zoom bossList $ handleListEvent ev
    _ -> selected %= succ
handleEvent _ = return ()

attrs :: MenuState -> AttrMap
attrs _ = attrMap defAttr
  [ (attrName "selected", black `on` blue)
  ]

initState :: MenuState
initState = MenuState
  { _config = Just defaultConfig
  , _selected = StartRaid
  , _bossList = list BossList (fromList allBosses) 1  
  }

defaultConfig :: Config
defaultConfig = Config
  { _bossSelection = [head allBosses]
  }

allBosses :: [BossSetup]
allBosses =
  [ B.trainingDummy
  , B.elementalAdept
  ]

menuApp :: App MenuState () Name  
menuApp = App
  { appDraw = draw
  , appChooseCursor = showFirstCursor  
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = attrs
  }

runMenu :: IO (Maybe Config)
runMenu = do
  finalState <- defaultMain menuApp initState 
  return $ finalState^.config
