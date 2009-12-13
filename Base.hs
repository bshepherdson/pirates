{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

module Base where

import System.IO
import Control.Monad
import Control.Monad.STM
import Control.Monad.State
import Control.Monad.Error
import Control.Concurrent
import Control.Concurrent.STM.TChan

import Data.List
import Data.Char (isSpace)
import Data.Maybe
import Control.Arrow
import System.Exit


type Synonyms   = [String]
type Poss a = [(a,Synonyms)]

type ClientId = Int
type MasterChannel = TChan Event

data Event = Message (ClientId,String)
           | NewClient Client
           | Disconnect ClientId
           | NewReader ClientId ThreadId
           | Tick Int

data Client = Client {
      cid        :: !ClientId
    , socket     :: !Handle
    , chan       :: TChan String
    , reader     :: !ThreadId
    , writer     :: !ThreadId
    , ship       :: Ship
    , watching   :: ![ClientId]
    , reporting  :: ![ClientId] -- reporting C= watching
    , targeting  :: ![ClientId] -- targeting C= reporting
    }

--------------------------------------------------------------
----------------------- P monad --------------------------
--------------------------------------------------------------


data PState = PState { clients  :: [Client]
                     , inChan   :: MasterChannel
                     , wind     :: (Speed, Heading)
                     }

newtype P a = P (StateT PState IO a)
    deriving (Functor, Monad, MonadIO, MonadState PState)

--------------------------------------------------------------
----------------------- parse monad --------------------------
--------------------------------------------------------------

newtype Parse a = Parse (ErrorT String P a)
    deriving (Functor, Monad, MonadIO, MonadState PState)

runParse (Parse x) = runErrorT x

type Command = ClientId -> String -> String -> Parse ()


liftP :: P a -> Parse a
liftP = Parse . lift


-- moved from Ship.hs to avoid cyclic deps.
type Class   = Int
type Heading = Double
type Speed   = Double -- measured in yards per minute

-- 10 6-second ticks to a minute, a minute being the unit of Heading change, and Speed
tickRate = 10

-- transforms wind heading and speed, given ships' heading, 
-- into a new heading and speed for the ship at the specified sail setting
type SailFunc = Speed -> Heading -> Heading -> (Heading, Speed)

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


------------------------------------------
------- utility sending functions --------
------------------------------------------

io :: (MonadIO m) => IO a -> m a
io = liftIO

send :: String -> Client -> P ()
send s c = io $ atomically $ writeTChan (chan c) s

to :: ClientId -> String -> P ()
to c s = findClient c >>= send s

toAll :: String -> P ()
toAll s = gets clients >>= mapM_ (send s)

toAllBut :: ClientId -> String -> P ()
toAllBut c s = gets clients >>= mapM_ (send s) . filter ((/=c).cid)



--------------- helper functions ------------------------

findClient :: ClientId -> P Client
findClient c = do
  cs <- gets clients
  case find ((==c).cid) cs of
    Nothing -> do
      io $ putStrLn $ "The impossible just happened: ClientId " ++ show c ++ " does not exist."
      io $ exitWith (ExitFailure 1)
    Just c' -> return c'



-- warning: partial function
updateMatching :: (a -> Bool) -> (a -> a) -> [a] -> [a]
updateMatching p f xs = pre ++ f x' : xs'
    where (pre, (x':xs')) = break p xs

updateClient :: ClientId -> (Client -> Client) -> P ()
updateClient c f = modify (\st -> st { clients = updateMatching ((==c).cid) f (clients st) })


withShip :: ClientId -> (Ship -> Ship) -> P ()
withShip cid f = updateClient cid (\c -> c { ship = f (ship c) })


maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
                [(y,"")] -> Just y
                _        -> Nothing



namesMatch :: (String -> Bool) -> Poss a -> Poss a
namesMatch p = filter (not . null . snd) . map (second (filter p))


-- given an input and a Poss of commands:
-- * if there is a unique poss. that is a prefix of the input, returns:
--     the Poss value, the matched portion, and the remainder of the string (leading whitespace trimmed)
tryParse :: String -> Poss a -> Parse (a, String, String)
tryParse s cs = case namesMatch (`isPrefixOf` s) cs of
                  [(a,[x])] -> return (a, x, dropWhile isSpace $ drop (length x) s)
                  []      -> fail $ "Unknown term \"" ++ s ++ "\""
                  xs      -> fail $ "Ambiguous command. Did you mean: "
                             ++ (intercalate ", " . concat . map snd) xs


maybeParse :: String -> Poss a -> Maybe a
maybeParse s ps = case namesMatch (s `isPrefixOf`) ps of
                    [(a,_)] -> Just a
                    _       -> Nothing
                  

-- returns the /last/ Just value, or Nothing.
collapseMaybes :: [Maybe a] -> Maybe a
collapseMaybes = listToMaybe . reverse . catMaybes

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

