{-#LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, RecordWildCards, TupleSections,
            GeneralizedNewtypeDeriving, DeriveDataTypeable#-}
module Main where
import System.Console.Haskeline
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B
import Data.List (sortBy)
import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Clock
import Data.Monoid
import System.Locale
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline

import VMonad
import Utils
import qualified Korppi

import Text.Blaze
import Text.Blaze.Renderer.Utf8 (renderHtml)

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (class_)

import System.Console.CmdArgs

data Format = Plain | Html deriving (Data,Typeable,Eq)
--data Filter  = All | Starting | Ongoing deriving (Data,Typeable,Eq) 

data Fetch = Fetch {account ::String
                   , n :: Int
                   , format :: Format
                   , filtering :: Maybe RoughTime } deriving (Data,Typeable)

arguments = cmdArgsMode $ Fetch {account = "" &= help "account"
                                ,n      = 5  &= help "output N next events"
                                ,format = Html &= help "output format (plain | html)"
                                ,filtering = Nothing
                                    &= help "filter events ( ended | ongoing | starting | upcoming)"
                                } &= summary "Fetch classroom reservations from Korppi"

main = runV $ do
    Fetch{..} <- liftIO $ cmdArgsRun arguments
    time <- liftIO $ getCurrentTime 
    timeZone <- liftIO $ getCurrentTimeZone
    Just pass <- liftIO $ runInputT defaultSettings $ getPassword (Just '*') "password: "
    cookie <- Korppi.login account pass
    rooms  <- Korppi.reservations cookie time
    let currentLocalTime = localTimeOfDay . utcToLocalTime timeZone $ time
        output Plain = mapM_ print 
        output Html  = B.putStrLn . renderHtml . H.table . mconcat . map ((\x ->  htmlFormat (toValue . show $ classifyTime currentLocalTime x) x)) -- This is slightly ugly
        filt Nothing  = id
        filt (Just r) = filter (\evt -> classifyTime currentLocalTime evt == r)
    liftIO $ output format
             . take n 
             . sortBy (compare`on` Korppi.time) 
             . filt filtering
             $ rooms

data RoughTime = Ended | Ongoing | Starting | Upcoming deriving (Eq,Ord,Show,Enum,Data,Typeable)

classifyTime currentLocalTime e
    | start < currentLocalTime && currentLocalTime < end  
        = Ongoing
    | start > currentLocalTime && start < addMinutes 15 currentLocalTime = Starting
    | start >= currentLocalTime = Upcoming
    | end < currentLocalTime = Ended
    | otherwise = Upcoming
        where (start,end) = Korppi.time e

htmlFormat cls (Korppi.EVT{..}) = H.tr ! class_ cls $ do
            H.td ! class_ "room"  $ H.toHtml room
            H.td ! class_ "time"  $ H.toHtml (st (fst time) ++ " - " ++ st (snd time))
            H.td ! class_ "event" $ H.toHtml (T.intercalate " " . catMaybes $ [course , event])
    where 
        s :: Show a => a -> T.Text
        s = T.pack . show
        st = formatTime defaultTimeLocale "%R"



