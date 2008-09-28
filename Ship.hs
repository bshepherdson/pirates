{-# LANGUAGE BangPatterns #-}

module Ship where

import qualified Data.Map as M
import Control.Arrow

import Base

type Class   = Int
type Heading = Int
type Speed   = Int -- measured in yards per tick

-- transforms wind heading and speed, given ships' heading, 
-- into a new heading and speed for the ship at the specified sail setting
type SailFunc = Heading -> Speed -> Heading -> (Heading, Speed)

-- maps user sail setting commands to functions that embody them for this ship
type Sails = Poss SailFunc

-- just a name, for now.
type Captain = String

-- a turn rate factor
type Rudder = Int

data TurnRate = Slow | Normal | Hard
  deriving (Read, Show)

data Ship = Ship {
      shipClass    :: !Class
    , name         :: !String
    , captain      :: !Captain
    , course       :: !Heading
    , orCourse     :: !(Maybe Heading)
    , sails        :: !Sails
    , sail         :: !SailFunc
    , rudder       :: !Rudder
    }


-- a starting ship

tpFrigate :: Ship
tpFrigate = Ship {
              shipClass = 5
            , name      = "Black Utopia"
            , captain   = "Myron Scrant"
            , course    = 0
            , orCourse  = Nothing
            , sails     = ssFrigate
            , sail      = furled
            , rudder    = 0
            }




ssFrigate :: Sails
ssFrigate = [(sLargeFastSquare    , ["full"])
            ,(sq3 sLargeFastSquare, ["drawn"])
            ,(sq2 sLargeFastSquare, ["half"])
            ,(sq1 sLargeFastSquare, ["light"])
            ,(steerage            , ["steerage"])
            ,(furled              , ["furled"])
            ]



-- extremely crude: 
-- 0-22.5* -> 0
-- 22.5-45 -> 1/4
-- 45-90   -> 1/2
-- 90-120  -> 3/4
-- 120-180 -> 1
-- unlike SMP, ships here are limited to the wind speed.
sLargeFastSquare :: SailFunc
sLargeFastSquare wh ws sh | dh < 23   = (sh,0)
                          | dh < 45   = (sh,q1 ws)
                          | dh < 90   = (sh,q2 ws)
                          | dh < 120  = (sh,q3 ws)
                          | otherwise = (sh,ws)
    where dh = windDiff wh sh

-----------------------------
-- ship-independent speeds --
-----------------------------

steerage :: SailFunc
steerage _ _ sh = (sh, 1)


-- moves one degree closer to the wind, either with or into it.
furled :: SailFunc
furled wh ws sh | windDiff wh sh == 90  = (sh,0)
                | otherwise             = (sh + signum (wh-sh), 0)








-----------------------------
----- report functions ------
-----------------------------

rudderReport :: Rudder -> String
rudderReport 0    = "amidships"
rudderReport 1    = "slow to starboard"
rudderReport 2    = "astarboard"
rudderReport 3    = "hard astarboard"
rudderReport (-1) = "slow to port"
rudderReport (-2) = "aport"
rudderReport (-3) = "hard aport"


turnReport :: Ship -> String
turnReport (Ship { rudder=r, orCourse=(Just c) }) = 
    "Turn " ++ rudderReport r ++ " to " ++ show c ++ ", aye."
turnReport (Ship { rudder=r}) = "Rudder " ++ rudderReport r ++ ", aye."



-----------------------------
----- utility functions  ----
-----------------------------

-- returns a value 0 <= x <= 180 describing the absolute difference between
-- the wind and the ship.
windDiff :: Heading -> Heading -> Heading
windDiff wh sh = norm180 $ abs $ sh - wh
    where norm180 x | x >= 360 = norm180 $ x - 360
                    | x <  0   = norm180 $ x + 360
                    | x >  180 = 180 - (x - 180)     -- weird but correct


sq3,sq2,sq1 :: SailFunc -> SailFunc
sq3 f wh ws sh = second q3 $ f wh ws sh
sq2 f wh ws sh = second q2 $ f wh ws sh
sq1 f wh ws sh = second q1 $ f wh ws sh


q3,q2,q1 :: Speed -> Speed
q3 x = (x * 3) `div` 4
q2 = (`div`2)
q1 = (`div`4)


