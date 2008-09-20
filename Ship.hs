{-# LANGUAGE BangPatterns #-}

module Ship where

import qualified Data.Map as M
import Control.Arrow


type Class   = Int
type Heading = Int
type Speed   = Int -- measured in yards per tick

-- transforms wind heading and speed, given ships' heading, 
-- into a new heading and speed for the ship at the specified sail setting
type SailFunc = Heading -> Speed -> Heading -> (Heading, Speed)

-- maps user sail setting commands to functions that embody them for this ship
type Sails = M.Map String SailFunc

-- just a name, for now.
type Captain = String

data Ship = Ship {
      shipClass    :: !Class
    , name         :: !String
    , captain      :: !Captain
    , course       :: !Heading
    , sails        :: !Sails
    , sail         :: !SailFunc
    }



-- a starting ship

tpFrigate :: Ship
tpFrigate = Ship {
              shipClass = 5
            , name      = "Black Utopia"
            , captain   = "Myron Scrant"
            , course    = 0
            , sails     = ssFrigate
            , sail      = furled
            }


ssFrigate :: Sails
ssFrigate = M.fromList [
             ("full"    , sLargeFastSquare)
            ,("drawn"   , sq3 sLargeFastSquare)
            ,("half"    , sq2 sLargeFastSquare)
            ,("light"   , sq1 sLargeFastSquare)
            ,("steerage", steerage)
            ,("furled"  , furled)
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


