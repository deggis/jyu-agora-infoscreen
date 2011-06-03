{-#LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards, TupleSections,
            GeneralizedNewtypeDeriving, DeriveDataTypeable#-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.Console.Haskeline
import System.Locale
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import VMonad
import Utils
import qualified Korppi

import Text.Blaze
import Text.Blaze.Renderer.Utf8 (renderHtml)

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (class_)

import System.Console.CmdArgs
import System.Exit

data Format = Plain | Html deriving (Data,Typeable,Eq)

data Fetch = Fetch {account ::String
                   , n :: Int
                   , format :: Format
                   , filtering :: Maybe RoughTime } deriving (Data,Typeable)

arguments = cmdArgsMode $ Fetch {account = ""  &= help "account"
                                ,n      = 5    &= help "output N next events"
                                ,format = Html &= help "output format (plain | html)"
                                ,filtering = Nothing
                                               &= help "filter events ( ended | ongoing | starting | upcoming)"
                                } &= summary "Fetch classroom reservations from Korppi"

main = do
    Fetch{..} <- liftIO $ cmdArgsRun arguments
    
    -- Set up time related stuff
    time <- getCurrentTime 
    timeZone <- liftIO getCurrentTimeZone
    let currentLocalTime = localTimeOfDay . utcToLocalTime timeZone $ time

    -- Fetch stuff from korppi
    (r,w) <- executeV $ do
        Just pass <- liftIO $ runInputT defaultSettings $ getPassword (Just '*') "password: "
        cookie <- Korppi.login account pass
        rooms  <- Korppi.reservations cookie time
        return rooms

    -- Handle errors
    case r of
        Left  err   -> print "poks"
        Right rooms -> output currentLocalTime format
                         . take n 
                         . sortBy (compare`on` Korppi.time) 
                         . filt currentLocalTime filtering
                         $ rooms
 where
    error Plain (e,w) = do 
                         putStrLn $ "ERROR: "++show e++" | "++show w 
                         exitWith (ExitFailure 1)
    error Html  (e,w) = do
                         B.putStrLn . renderHtml $ 
                          H.div ! class_ "error-msg" $ (H.p (H.toHtml . T.pack $ show e) >> 
                                                        H.p (H.toHtml . T.pack $ show w))

                         exitWith (ExitFailure 1)
                            
    output _   Plain = mapM_ print
    output clt Html  = B.putStrLn . renderHtml 
                              . H.table 
                              . (tableHeader `mappend`) 
                              . H.tbody 
                              . mconcat 
                              . map (row clt)
    row clt x = htmlFormat (toValue . show . classifyTime clt $ x) x
                                                                
    filt clt Nothing  = id
    filt clt (Just r) = filter (\evt -> classifyTime clt evt == r)


data RoughTime = Ended | Ongoing | Starting | Upcoming deriving (Eq,Ord,Show,Enum,Data,Typeable)

classifyTime currentLocalTime e
    | start < currentLocalTime && currentLocalTime < end  
        = Ongoing
    | start > currentLocalTime && start < addMinutes 15 currentLocalTime = Starting
    | start >= currentLocalTime = Upcoming
    | end < currentLocalTime = Ended
    | otherwise = Upcoming
        where (start,end) = Korppi.time e

tableHeader = H.thead . H.tr $ do
            cell "room"    "Sali/Room" 
            cell "time"    "Aika/Time" 
            cell "course"  "Koodi/Code" 
            cell "event"   "Tapahtuma/Event"
    where 
        cell label elem = H.th ! class_ label $ elem


htmlFormat cls (Korppi.EVT{..}) = H.tr ! class_ cls $ do
            cell "room"   $ room
            cell "time"   $ st (fst time) ++ " - " ++ st (snd time)
            cell "course" $ fromMaybe "" course
            cell "event"  $ fromMaybe "" event
    where 
        cell label elem = H.td ! class_ label $ H.toHtml elem
        s :: Show a => a -> T.Text
        s = T.pack . show
        st = formatTime defaultTimeLocale "%R"



