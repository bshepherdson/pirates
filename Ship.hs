{-# LANGUAGE BangPatterns #-}

module Ship where

import qualified Data.Map as M
import Control.Arrow


type Class   = Int
type Heading = Int
type Speed   = Int -- measured in yards per tick

-- transforms wind heading and speed, given ships' heading, 
-- into a new heading and speed for the ship at the specified sail setting
type SailFunc :: Heading -> Speed -> Heading -> (Heading, Speed)

-- maps user sail setting commands to functions that embody them for this ship
data Sail = M.Map String SailFunc

-- just a name, for now.
type Captain = String

data Ship = Ship {
      shipClass    :: !Class
    , name         :: !String
    , captain      :: !Captain
    , course       :: !Heading
    , sails        :: !Sail
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
            , sail      = sFurled
            }


ssFrigate :: Sails
ssFrigate = M.fromList [
             ("full"    , sLargeSquare)
            ,("drawn"   , second q3 . sLargeSquare)
            ,("half"    , second q2 . sLargeSquare)
            ,("steerage", steerage)
            ,("furled"  , furled)
            ]



q3 :: Speed -> Speed
q3 x = (x*3) `div` 4

q2 :: Speed -> Speed
q2 x = x `div` 2

q1 :: Speed -> Speed
q1 x = x `div` 4

steerage :: SailFunc
steerage _ _ sh = (sh, 1)


-- moves one degree closer to the wind, either with or into it.
furled :: SailFunc
furled wh ws sh | an == 90  = (sh,0)
                | otherwise = (sh + signum (wh-sh), 0)

