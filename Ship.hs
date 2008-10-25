{-# LANGUAGE BangPatterns #-}

module Ship where

import qualified Data.Map as M
import Control.Arrow

import Base

type Class   = Int
type Heading = Double
type Speed   = Double -- measured in yards per minute

-- 10 6-second ticks to a minute, a minute being the unit of Heading change, and Speed
tickRate = 10

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
    , sx           :: !Double
    , sy           :: !Double
    , turnRate     :: !Double
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



-----------------------------------
-------- sailing functions --------
-----------------------------------

tickShip :: Speed -> Heading -> ClientId -> Ship -> P Ship
tickShip ws wh cid s@(Ship { course = c, orCourse = oc, rudder = r, turnRate = dc }) = do
  let nc  = c + (r * dc / tickRate)
      oc' = maybe c id oc 
      overshoot = compare c oc' /= compare nc oc'
  when overshoot $ to c ("Steady on course " ++ roundShow nc ++ ", Cap'n")
  let s'  = if overshoot then s { course = nc } else s { course = oc', orCourse = Nothing, rudder = 0 }
      s'' = moveShip ws wh s'
  return s''

moveShip :: Speed -> Heading -> Ship -> Ship
moveShip ws wh s@(Ship { sail = f, course = c, sx = x, sy = y }) = s { course = c', sx = x', sy = y' }
    where (c',ss) = f ws wh c  -- apply the sailing function
          x'      = x + ss * cos (comp2cart c')
          y'      = y + ss * sin (comp2cart c')

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

roundShow = show . round

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

-- converts compass headings into left-hand 3 o'clock radians
comp2cart :: Heading -> Heading
comp2cart h = toRadians $ normalize $ (360 - h) + 90

cart2comp :: Heading -> Heading
cart2comp h = normalize $ toDegrees $ 360 - (h - 90)

toRadians,toDegrees :: (Floating a) => a -> a
toRadians x = 2 * pi * (x / 360)
toDegrees x = 360 * (x / (2*pi))

normalize x | x >= 360  = normalize $ x - 360
            | x <    0  = normalize $ x + 360
            | otherwise = x

