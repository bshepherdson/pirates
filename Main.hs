{-# LANGUAGE 
BangPatterns,
GeneralizedNewtypeDeriving #-}

module Main where

import System.IO
import System.Exit
import System.Random
import System.Environment
import Network

import Control.Monad
import Control.Monad.STM
import Control.Monad.State hiding (modify, get)
import Control.Monad.Trans
import Control.Monad.Error
import Control.Applicative
import Control.Arrow hiding (loop)

import Control.Concurrent
import Control.Concurrent.STM.TChan

import Data.List
import Data.Char
import Data.Maybe

import Pirates.Base
import Pirates.Ship

import Data.Accessor hiding ((%=),(%:))
import Data.Accessor.Monad.MTL.State hiding (lift)


readerThread :: ClientId -> Handle -> MasterChannel -> IO ()
readerThread c h chan = forever $ do
                          catch (hGetLine h >>= \s ->
                                     atomically $ writeTChan chan (Message (c,s)))
                                (const $ hClose h >> putDisconnect c chan)


writerThread :: ClientId -> Handle -> TChan String -> MasterChannel -> IO ()
writerThread c h chan mchan 
    = forever $ do
        s <- atomically $ readTChan chan
        catch (hPutStrLn h s >> hFlush h)
              (const $ hClose h >> putDisconnect c mchan)


putDisconnect :: ClientId -> MasterChannel -> IO ()
putDisconnect c chan = do
  putStrLn $ "Disconnecting client " ++ show c
  atomically $ writeTChan chan (Disconnect c)



-----------------------------------------
-------------   commands    -------------
-----------------------------------------

commands :: Poss Command
commands = [
            -- ship control
            (cmd_turn     , ["turn","come"])
           ,(cmd_rudder   , ["rudder"])
           -- targeting commands
           ,(cmd_watch    , ["watch"])
           ,(cmd_report   , ["report", "reports", "report on", "reports on"])
           ,(cmd_target   , ["target"])
           -- viewing the target lists
           ,(cmd_visible  , ["ships in view", "visible", "list visible", "show visible"])
           ,(cmd_watching , ["watching", "list watching", "show watching"])
           ,(cmd_reporting, ["reporting", "list reporting", "show reporting"])
           ,(cmd_targeting, ["targeting", "list targeting", "show targeting", "targets", "list targets", "show targets"])
           ]


parser :: (ClientId,String) -> P ()
parser (c,[]) = return ()  -- do nothing on empty string
parser (c,s)  = case words (map toLower s) of
                  [] -> return () -- do nothing again
                  _  -> do
                    x <- runParse (tryParse s commands >>= \(f,cmd,as) -> f c cmd as)
                    case x of
                      Left e  -> to c $ "Error: " ++ e
                      Right _ -> return ()


-- takes a default, possibilities, and the arguments
parseArg :: a -> Poss a -> [String] -> a
parseArg d p xs = maybe d id $ collapseMaybes $ map (flip maybeParse p) xs


dirs  = [(   -1, ["left","port","aport"])
        ,(    1, ["right","starboard","stbd","astarboard","astbd"])]

rates = [(    1, ["soft","gentle","softly","gently","slow","slowly","light","slight"])
        ,(    3, ["hard","fast","quick","quickly"])]
          

cmd_turn :: Command
cmd_turn c cmd as = do
  liftP $ turn c dir rate hdg absol
  s <- (^. ship) <$> liftP (findClient c)
  liftP $ to c $ turnReport s
    where absls = [(True , ["to"]), (False, ["by"])]
          as'   = words as
          dir   = parseArg 0 dirs  as'
          rate  = parseArg 2 rates as'
          hdg   = maybe 0 id $ collapseMaybes $ (map maybeRead as' :: [Maybe Int])
          absol = parseArg (cmd=="turn") absls as'

turn :: ClientId -> Int -> Rudder -> Int -> Bool -> P ()
turn c d r h a = withShip c $ \s -> s { rudder_  = d*r
                                      , orCourse_ = if a 
                                                    then Just $ fi h 
                                                    else Just $ course_ s + (fi $ h * d )
                                      }


cmd_rudder :: Command
cmd_rudder c cmd as = do
  liftP $ withShip c $ (rudder ^= r) . (orCourse ^= Nothing)
  liftP $ to c $ "Rudder " ++ rudderReport r ++", aye Cap'n"
    where as'  = words as
          dir  = parseArg 0 dirs  as'
          rate = parseArg 2 rates as'
          r    = dir*rate


cmd_watch :: Command
cmd_watch c cmd as = do
  cs     <- get clients
  target <- matchShipName cs as
  me     <- liftP $ findClient c
  when (target == me) $ fail "Can't watch yourself."

  if target^.cid `elem` me^.watching 
    then liftP $ to c $ "Already watching " ++ target^.ship^.name
    else do
      liftP $ updateClient c $ watching ^: (target^.cid :)
      liftP $ to c $ "Now watching " ++ target^.ship^.name




cmd_report :: Command
cmd_report = error "cmd_report"

cmd_target :: Command
cmd_target = error "cmd_target"

cmd_watching :: Command
cmd_watching = error "cmd_watching"

cmd_reporting :: Command
cmd_reporting = error "cmd_reporting"

cmd_targeting :: Command
cmd_targeting = error "cmd_targeting"

cmd_visible :: Command
cmd_visible = error "cmd_visible"

------------------------------------------------
-------------- server functions ----------------
------------------------------------------------


runP :: PState -> P a -> IO (a, PState)
runP st (P a) = runStateT a st


tick :: Int -> P ()
tick x = do
    -- advance all ships by the right amount of movement
    cs <- get clients
    (ws,wh) <- get wind
    cs' <- mapM (\c -> tickShip ws wh (c ^. cid) (c ^. ship) >>= \s -> return c { ship_ = s }) cs
    clients %= cs'
    tickWind

tickWind :: P ()
tickWind = return ()   -- TODO: to be defined
  


loop :: MasterChannel -> P ()
loop chan = forever $ do
              e <- io $ atomically $ readTChan chan
              case e of
                Message cs -> parser cs
                NewClient c -> clients %: (c:)
                Disconnect c -> do
                        cs <- get clients
                        case break ((==c). (^. cid)) cs of
                          (_,[]) -> return () -- do nothing, client already gone
                          (pre,d:post) -> do
                                  io $ killThread $ d ^. writer
                                  tid <- io $ myThreadId
                                  if d ^. reader == tid
                                    then return () -- do nothing.
                                    else io $ killThread $ d ^. reader
                                  clients %= pre ++ post
                                  toAll $ "System: Client Disconnected: " ++ show (d ^. cid)
                NewReader c t -> updateClient c $ reader ^= t
                Tick x -> tick x


main = do
  args <- getArgs
  case args of
    (port:_) -> do
              chan <- atomically newTChan
              tid  <- forkIO $ runP (PState [] chan (10,0)) (loop chan) >> return ()
              sock <- listenOn (PortNumber (fromIntegral ((read port)::Int)))
              tickt<- forkIO $ tickerThread 0 chan
              serverLoop chan sock tid 0
    [] -> putStrLn "Usage: pirateserver <port>"


tickerThread :: Int -> MasterChannel -> IO ()
tickerThread !x chan = do
  atomically $ writeTChan chan (Tick x)
  threadDelay $ (60 `div` fi tickRate) * 1000000
  tickerThread (x+1) chan


serverLoop :: MasterChannel -> Socket -> ThreadId -> ClientId -> IO ()
serverLoop chan s tid c = do
  (h,_,_) <- accept s
  nc <- atomically newTChan
  putStrLn $ "New client! Assigning cid "++ show c
  wid <- forkIO $ writerThread c h nc chan
  atomically $ writeTChan chan $ NewClient (Client {
                                              cid_    = c
                                            , socket_ = h
                                            , chan_   = nc
                                            , reader_ = tid
                                            , writer_ = wid
                                            , ship_   = tpFrigate
                                            , watching_ = []
                                            , reporting_ = []
                                            , targeting_ = []
                                            })
  rid <- forkIO $ readerThread c h chan
  atomically $ writeTChan chan (NewReader c rid)
  serverLoop chan s tid $! c+1



