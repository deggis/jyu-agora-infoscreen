{-#LANGUAGE ScopedTypeVariables, ViewPatterns, RecordWildCards, OverloadedStrings#-}
-- | This module provides means to log into the Korppi system and fetch classroom reservations
module Korppi (login, reservations, Event(..)) where

import System.IO.Unsafe
import Network.Curl
import Network.Curl.Easy
import Network.Curl.Opts
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import System.Locale
import Control.Monad
import Control.Monad.Trans
import Data.Char

import Utils
import VMonad

-- |Url where you get the session cookie
loginURL = "https://korppi.jyu.fi/kotka/servlet/authentication/checkLogin"

-- |Url where you get the list of classrooms
classroomListURL :: FormatTime t => t -> t -> String
classroomListURL f t = "https://korppi.jyu.fi/kotka/reservation/reportOrgShow.jsp?order=code,begintime&organizationName=&spaceGroup=4&spaceGroup=896&spaceGroup=897&spaceGroupSearch=agora&courseName=&code=&confirmed=true&role4=true&role3=true&role2=true&role1=true&role0=true&requests=true&personName=&endDate=" ++format t++ "&beginDate="++format f++"&oldSearch=%3B%3Bagora%3B%3B&freeSearchWord"
    where 
        format = formatTime defaultTimeLocale "%d.%m.%Y"

-- |Shorthand for when we want ByteStrings out of Curl.
type ByteStringResponse = CurlResponse_ [(String,String)] B.ByteString

curling :: (MonadIO m) => IO a -> m a
curling = liftIO . withCurlDo

-- |Get a list of reservations in agora at a given day.
reservations
  :: (FormatTime t, MonadIO m, MonadPlus m, Functor m) => String -> t -> m [Event]
reservations sessionCookie time = do 
    t :: ByteStringResponse <- curling $ curlGetResponse_ (classroomListURL time time) 
                                          [CurlHttpHeaders ["Cookie:"++sessionCookie]]
    let rooms = T.decodeUtf8 . respBody $ t
    readColumns rooms >>= mapM Korppi.parseEvent

stripP = dropWhile isSpace

-- |Exhange account and password for a session cookie.
login :: String -> String -> VaksiMonad String
-- FIXME: For some odd reason "curling $ curlGetResponse .."
--        segfaults -> unsafePerformIO
login account password = return $ unsafePerformIO $ do
    -- | Begin login from frontpage, put SessionID aside
    r1 <- curlGetResponse "https://korppi.jyu.fi/kotka/servlet/authentication/openIdLogin" []
    let head1 = respHeaders r1
        Just korppicookie = lookup "Set-Cookie" head1
        -- | For some reason Location fields contain extra prefix space that breaks curl.
        Just manageURL = fmap stripP $ lookup "Location" head1

    -- | Continue to OpenID manager.
    r2 <- curlGetResponse (tail manageURL) []
    let postLoginFields = CurlPostFields ["username="++account, "password="++password, "login=Login"]

    r3 <- curlGetResponse manageURL [postLoginFields]
    let head3 = respHeaders r3
        Just cookie1 = lookup "Set-Cookie" head3
        Just return2KorppiURL = fmap stripP $ lookup "Location" head3

    -- | Let's visit the returnto address once to say hi.
    _ <- curlGetResponse return2KorppiURL [CurlHttpHeaders ["Cookie:"++korppicookie]]

    -- | For non-JS agents this call must be made once:
    let disableJSURL = "https://korppi.jyu.fi/kotka/noticeboard/notices.jsp?fromLogin=true&javascript-enabled=false"
    _ <- curlGetResponse disableJSURL [CurlHttpHeaders ["Cookie:"++korppicookie]]

    return korppicookie


-- Parsing
-- |Data type for class room reservations
data Event = EVT {room :: T.Text
              --   ,date :: Day
                 ,time :: (TimeOfDay,TimeOfDay)
                 ,course :: Maybe T.Text
                 ,event  :: Maybe T.Text} 
            deriving (Eq,Ord,Show)

parseEvent e = do
      room <- look "Sali" 
--      date <- look "Pvm"  >>= (parseDay `tag` ("parsing " ++ show e))
      time <- look "Klo"  >>= (parseTimeRange `tag` ("parsing "++show e))
      let course = look "Kurssi" 
      let event  = look "Tapahtuma" 
      return EVT{..}
    where look a = m2e ("Missing field: "++T.unpack a) . lookup a $ e

readColumns :: (Monad m) => T.Text -> m [[(T.Text, T.Text)]]
readColumns x = do 
            let hd:dash:body = T.lines x
                c = chnk hd
            return $ map (rechunk c) (filter (not . T.null) body)

parseDay :: (Monad m) => T.Text -> m Day
parseDay = parseTimeM "%d.%m" 

parseTimeRange :: (Functor m, Monad m, ParseTime t) => T.Text -> m (t, t)
parseTimeRange t =  case T.split (=='-') t of
                      [s, e] -> (,) `fmap` read s `ap` read e
                      _      -> fail $ "no time range in string"++T.unpack t
                where 
                    read = parseTimeM "%H:%M"

parseTimeM s (T.unpack->t) = m2e ("Failed to parse time: "++t) 
                              . parseTime defaultTimeLocale s 
                              $ t
--
