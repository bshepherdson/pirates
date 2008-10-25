{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

module Base where

import System.IO
import Control.Monad
import Control.Monad.STM
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM.TChan

import Ship

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
    }


data PState = PState { clients  :: [Client]
                     , inChan   :: MasterChannel
                     , wind     :: (Speed, Heading)
                     }

newtype P a = P (StateT PState IO a)
    deriving (Functor, Monad, MonadIO, MonadState PState)

