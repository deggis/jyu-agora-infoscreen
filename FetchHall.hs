{-#LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, RecordWildCards, TupleSections,
            GeneralizedNewtypeDeriving#-}
module FetchHalls where
import Network.Curl
import Network.Curl.Easy
import Network.Curl.Opts
import System.Console.Haskeline
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Data.List (sortBy)
import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import System.Locale
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline

import VMonad
import Utils
import qualified Korppi

main = runV $ do
    time <- liftIO $ getCurrentTime 
    Just pass <- liftIO $ runInputT defaultSettings $ getPassword (Just '*') "password: "
    cookie <- Korppi.login "aleator" pass
    rooms  <- Korppi.reservations cookie time
    liftIO $ mapM_ print $ sortBy (compare`on` Korppi.time) rooms

format (Korppi.EVT{..}) = T.intercalate " " $ 
     [room,s date,s time,T.intercalate " " $ catMaybes [course , event]]
    where 
        s :: Show a => a -> T.Text
        s = T.pack . show



