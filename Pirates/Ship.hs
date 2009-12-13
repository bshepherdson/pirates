{-# LANGUAGE BangPatterns #-}

module Pirates.Ship where

import qualified Data.Map as M
import Control.Arrow
import Control.Monad

import Pirates.Base


-- a starting ship

tpFrigate :: Ship
tpFrigate = Ship {
              shipClass_ = 5
            , name_      = "Black Utopia"
            , captain_   = "Myron Scrant"
            , course_    = 120
            , orCourse_  = Nothing
            , sails_     = ssFrigate
            , sail_      = sLargeFastSquare
            , rudder_    = 0
            , sx_        = 0
            , sy_        = 0
            , turnRate_  = 30
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
sLargeFastSquare ws wh sh | dh < 23   = (sh,0)
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
furled ws wh sh | windDiff wh sh == 90  = (sh,0)
                | otherwise             = (sh + signum (wh-sh), 0)



-----------------------------------
-------- sailing functions --------
-----------------------------------

tickShip :: Speed -> Heading -> ClientId -> Ship -> P Ship
tickShip ws wh cid s@(Ship { course_ = c, orCourse_ = oc, rudder_ = r, turnRate_ = dc }) = do
  let nc  = c + (fi r * dc / fi tickRate)
      (overshoot,oc') = case oc of
          Just x  -> (compare c x /= compare nc x || c == x, x)
          Nothing -> (False, 0)
  when overshoot $ to cid ("Steady on course " ++ roundShow oc' ++ ", Cap'n")
  let s'  = if overshoot then s { course_ = oc', orCourse_ = Nothing, rudder_ = 0 } else s { course_ = nc }
      s'' = moveShip ws wh s'
  to cid $ showShip s'' ws wh
  return s''

moveShip :: Speed -> Heading -> Ship -> Ship
moveShip ws wh s@(Ship { sail_ = f, course_ = c, sx_ = x, sy_ = y }) = s { course_ = normalize c', sx_ = x', sy_ = y' }
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
rudderReport _    = "ERROR: UNKNOWN"


turnReport :: Ship -> String
turnReport (Ship { rudder_=r, orCourse_=(Just c) }) = 
    "Turn " ++ rudderReport r ++ " to " ++ show c ++ ", aye."
turnReport (Ship { rudder_=r}) = "Rudder " ++ rudderReport r ++ ", aye."

roundShow = show . round


showShip :: Ship -> Speed -> Heading -> String
showShip (Ship { course_ = c, orCourse_ = oc, sails_ = s, rudder_ = r, sx_ = x, sy_ = y }) ws wh = "Ship: ("++ show x ++","++ show y++"), course "++ show c ++", ordered course "++show oc++", rudder " ++ rudderReport r ++ ", wind " ++ show wh ++ " at " ++ show ws

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
                    | otherwise = x


sq3,sq2,sq1 :: SailFunc -> SailFunc
sq3 f wh ws sh = second q3 $ f wh ws sh
sq2 f wh ws sh = second q2 $ f wh ws sh
sq1 f wh ws sh = second q1 $ f wh ws sh


q3,q2,q1 :: Speed -> Speed
q3 x = (x * 3) / 4
q2 = (/2)
q1 = (/4)

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

