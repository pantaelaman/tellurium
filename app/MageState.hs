{-# LANGUAGE TemplateHaskell #-}

module MageState where

import Data.Dynamic
import Brick.Types
import Lens.Micro.Platform

import Cooldown

type Boss = EventM Name MageState ()
data BossSetup = BossSetup [Char] Int Boss (EventM Name MageState Dynamic)

type CastEffect = EventM Name MageState ()
data Cast =
  Cast  { _castName :: [Char]
        , _castTicksPassed :: Int
        , _castMaxTicks :: Int
        , _castEffect :: CastEffect
        }

newCast :: [Char] -> Int -> CastEffect -> Cast
newCast name maxTicks effect = Cast name 0 maxTicks effect

data Element = Fire | Earth | Ice | Water | Wind | Lightning
  deriving (Eq, Show)
instance Enum Element where
  toEnum 0 = Fire
  toEnum 1 = Earth
  toEnum 2 = Ice
  toEnum 3 = Water
  toEnum 4 = Wind
  toEnum 5 = Lightning
  toEnum n = toEnum $ n `mod` 6
  fromEnum Fire = 0
  fromEnum Earth = 1
  fromEnum Ice = 2
  fromEnum Water = 3
  fromEnum Wind = 4
  fromEnum Lightning = 5

revtransposeElement :: Element -> Element
revtransposeElement = pred . pred

revsemitransposeElement :: Element -> Element
revsemitransposeElement = pred

transposeElement :: Element -> Element
transposeElement = succ . succ

semitransposeElement :: Element -> Element
semitransposeElement = succ

areTrigonallyAligned :: Element -> Element -> Bool
areTrigonallyAligned e1 e2 = fromEnum e1 `mod` 2 == fromEnum e2 `mod` 2

data Aspect = Astral | Umbral
  deriving (Enum, Eq, Show)

getAspect :: Element -> Aspect
getAspect Fire = Astral
getAspect Ice = Astral
getAspect Wind = Astral
getAspect Lightning = Umbral
getAspect Earth = Umbral
getAspect Water = Umbral

matchesAspect :: Element -> Aspect -> Bool
matchesAspect element aspect = getAspect element == aspect

type StatusEffect = EventM Name MageState ()

data Status =
  TimedStatus {_timeLeft :: Int, _name :: [Char], _expireEffect :: StatusEffect, _statusEffect :: StatusEffect} |
  StackedStatus {_stacksLeft :: Int, _name :: [Char], _expireEffect :: StatusEffect, _statusEffect :: StatusEffect} |
  TimedStackedStatus {_timeLeft :: Int, _stacksLeft :: Int, _name :: [Char], _expireEffect :: StatusEffect, _statusEffect :: StatusEffect}

upkeepStatus :: Status -> EventM Name MageState (Maybe Status)
upkeepStatus (TimedStatus timeLeft name expire effect) = do
  effect
  if timeLeft == 0
  then expire >> return Nothing
  else return $ Just $ TimedStatus (timeLeft - 1) name expire effect
upkeepStatus (StackedStatus stacksLeft name expire effect) = do
  effect
  if stacksLeft == 0
  then expire >> return Nothing
  else return $ Just $ StackedStatus stacksLeft name expire effect
upkeepStatus (TimedStackedStatus timeLeft stacksLeft name expire effect) = do
  effect
  if timeLeft == 0
  then expire >> return Nothing
  else if stacksLeft == 0
  then return Nothing
  else return $ Just $ TimedStackedStatus (timeLeft - 1) stacksLeft name expire effect

data ComboAction = NoCombo | Aegis | Phrygis | Solis | Zephis 
  deriving (Ord, Eq)

data Name = None
  deriving (Ord, Eq)

data TickState = 
  TickState { _tickDamage :: Int
            , _tickMPGain :: Int
            , _damageLock :: Bool
            , _mpGainLock :: Bool
            }

data ActionState = ActionState
  { _comboAction :: ComboAction
  , _primaryElement :: Element
  , _secondaryElement :: Element
  , _reverseTransposition :: Bool
  , _secondaryTransposition :: Bool
  , _imposeQueue :: [Element]
  , _elementLevels :: [Int]
  , _aspectLevels :: [Int]
  , _flaggedElements :: [Bool]
  , _mp :: Int
  }

data BossState = BossState
  { _currentBossName :: [Char]
  , _currentBoss :: Boss
  , _bossCast :: Maybe Cast
  , _bossStatuses :: [Status]
  , _bossInternalState :: Dynamic
  , _bossDamage :: Int
  , _bossMaxHealth :: Int
  }

data MageState = MageState
  { _cds :: [Cooldown]
  , _tickState :: TickState
  , _actionState :: ActionState
  , _statuses :: [Status]
  , _totalDamage :: Int
  , _prevTickDamages :: [Int]
  , _elapsedTicks :: Int
  , _bossState :: BossState
  , _bossList :: [BossSetup]
  , _strikes :: Int
  }

initTickState :: TickState
initTickState = TickState
  { _tickDamage = 0
  , _tickMPGain = 10
  , _damageLock = False
  , _mpGainLock = False
  }

initActionState :: ActionState
initActionState = ActionState
  { _comboAction = NoCombo
  , _primaryElement = Fire
  , _secondaryElement = Fire
  , _elementLevels = 0 <$ [0..5]
  , _aspectLevels = 500 <$ [0..1]
  , _flaggedElements = False <$ [0..5]
  , _imposeQueue = []
  , _reverseTransposition = False
  , _secondaryTransposition = False
  , _mp = 10000
  }

initBossState :: BossSetup -> BossState
initBossState (BossSetup name health boss _) = BossState
  { _currentBossName = name
  , _currentBoss = boss
  , _bossCast = Nothing
  , _bossStatuses = []
  , _bossInternalState = toDyn ""
  , _bossDamage = 0
  , _bossMaxHealth = health
  }

initState :: [BossSetup] -> MageState
initState bosses = MageState
  { _cds = allCDs
  , _tickState = initTickState
  , _actionState = initActionState
  , _statuses = []
  , _totalDamage = 0
  , _prevTickDamages = []
  , _elapsedTicks = 0
  , _bossState = initBossState $ head bosses
  , _bossList = tail bosses
  , _strikes = 0
  }

allCDs :: [Cooldown]
allCDs =
  [ newCD 10     -- GCD
  , newCD 300    -- Cardios
  , newCD 600    -- Surgis
  , newCD 30     -- Sol/Nox
  , newCD 100    -- Impose
  , newCD 200    -- Protranspose
  , newCD 200    -- Prosemitranspose
  , newCD 30     -- Duotranspose
  , newCD 30     -- Duosemitranspose
  , newCD 600    -- Lucidity
  , newCD 300    -- Invigorate/Enervate
  ]

makeLenses ''Cast
makeLenses ''Status
makeLenses ''TickState
makeLenses ''ActionState
makeLenses ''BossState
makeLenses ''MageState
