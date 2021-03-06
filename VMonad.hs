{-#LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, RecordWildCards, TupleSections,
            GeneralizedNewtypeDeriving#-}
module VMonad where
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Trans

-- newtype VaksiMonadPure a = WP (VaksiMonad a) deriving (Monad,MonadPlus,Functor)

newtype VaksiMonad a = W (ErrorT String (WriterT [String] IO) a) deriving (MonadIO,MonadPlus,Functor)
instance Monad VaksiMonad where
    return a = W $ return a
    (W a) >>= b = W $ do
                       ra <- a
                       let (W br) = b ra
                       br
    fail str = W $ lift (tell [str]) >> (throwError str)

--note :: String -> VaksiMonad ()
note = W . lift . tell . box


box :: t -> [t]
box x = [x]

--executeV :: VaksiMonad a ->  (Either String a, [String])
executeV (W a) = runWriterT . runErrorT $ a

runV :: VaksiMonad a -> IO a
runV (W a) = do
         (r,w) <- runWriterT . runErrorT $ a 
         case r of
            Left e -> fail ("caught: "++e++show w)
            Right v -> return v

--runV (WP a) = do
--         (r,w) <- runErrorT $ a 
