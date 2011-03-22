module Utils where
import qualified Data.Text as T
import Control.Monad
import Data.Time.LocalTime

-- |Add an hour to localtime
addHour tod | todHour tod  == 23 = tod{todHour=0}
            | otherwise          = tod{todHour=todHour tod + 1}

-- |Add minutes to local time
addMinutes n tod 
    | n >= 60  = addMinutes (n-60) (addHour tod)
    | remainder >= 0 = (addHour tod){todMin = remainder }
    | otherwise      = tod{todMin = todMin tod + n}
    where
     remainder = todMin tod + n - 60

-- |Convert Maybe into a Monad
m2e :: (Monad m) => String -> Maybe a -> m a
m2e _ (Just a) = return a
m2e e Nothing  = fail e


-- |Replace fail on a monad by a different one. (Ie, if your monad
--  logs errors, you can use this to supply context for it)
tag :: MonadPlus m => (t -> m a) -> String -> t -> m a
tag a p = \x -> a x `mplus` (fail p) 

-- Why oh why doesn't Data.Text already have these?

-- | Split a Text into fields specified by field length and a key.
rechunk [] t = []
rechunk ((l,w):cs) t = let
    e = T.take (l) t
    in (w,T.strip e):rechunk cs (T.drop l t)

-- | Break a string into words while keeping track of total length of word and whitespace
--   before the next word. 
chnk t | T.null t  = []
        | otherwise = let
               (w,r)  = T.span (/=' ') t
               (s,r') = T.span (==' ') r 
               l = T.length w + T.length s
             in (l, w):chnk r'
