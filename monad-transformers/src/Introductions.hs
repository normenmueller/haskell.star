{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Introductions where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

-- | Angenommen man will eine 'IO' Monade und einer 'Maybe' Monade kombinieren,
-- wobei die Kombination auch eine Instanz der CC of Monads darstellen soll.
-- Sprich, wir möchten, dass wir Werte vom Typ 'IO (Maybe a)' monadisch
-- behandeln können.
--
-- Hier kommen monad transformers zur Hilfe!
--
-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
type Eff a = MaybeT IO a

-- | 'succ' kann mit der neuen Monade 'Eff :: * -> *' umgehen, als ob es die
-- Monade 'IO Maybe :: * -> *' wäre.
--
-- 'MaybeT' tritt dabei nicht direkt in Betracht!
--
--    !!! 'MaybeT' binds over both bits of structure.
--
succ :: Num a => Eff a -> Eff a
succ = fmap (1+)

succ' :: Num a => Eff a -> Eff a
succ' = ((\a -> return $ 1 + a) =<<)

succ'' :: Num a => Eff a -> Eff a
succ'' lma = do
    a <- lma
    return $ 1 + a

-- | 'run' lässt, wenn man dann mit allen *effectful computations* fertig ist,
-- den Transformer (Stack) "laufen", sprich führt die kombinierte Berechnung
-- aus,  extrahiert den resultierenden Wert und liefert dieses zurück.
--
-- Man muss sich vorstellen, die ganzen Berechnungen über wurde ausschlißlich
-- *direkt* mit Monad Transformer handtiert, jedoch keine Tranformation
-- durchgeführt.  Man hat sozusagen mit Monad Transformer als *first class
-- citizen* operiert...  und zwar monadisch[^1].
--
-- Nun, nach getaner Arbeit, möchte man schlussendlich die Transformationen
-- durchführen... man möchte die Transformationen laufen
-- lassen. In diesem Beispiel hier, man transformiert 'IO a' hinzu 'IO (Maybe
-- a)'... und genau dies liefert die 'run' Operation.
--
-- [1]: Remember, wir wollen ja, dass sich die Kombination
-- zweier Monaden wieder wie eine Monad verhält; das funktioniert aber nicht
-- immer so ohne weiteres... und da kamen die Monad Transformer ins Spiel. Ein
-- Monad Transform stellt sozusagen sicher, dass sich wir mit der Kombination
-- monadisch hantieren können. Der Monad Transformer selbst, bleibt dabei im
-- Hintergrund, sprich, ist nicht "spürbar" ... bis zum Schluss ... dann muss
-- der Transformer (Stack) ausgeführt werden.
run :: Eff a -> IO (Maybe a)
run = runMaybeT

-- | Wenn man nun noch mehrere Monaden kombinieren möchte, sprich, wenn man nun
-- noch mehrere Monad Transformer mit einander kombinieren möchte, dann ist mtl
-- äusserst *convient*, damit nicht das ganze *lifting* händisch ausführen muss.
type Env = Integer
type ST = Integer

type Eff2 a = ReaderT Env (StateT ST (MaybeT IO)) a

inc :: Num a => Eff2 a -> Eff2 a
inc = ((\a -> lift . state $ \s -> (a, s + 1)) =<<)
--            ^^^^^^^^^^^^
--            Cf. 'ReaderT r m' instance of 'MonadState'.
--            With mtl 'lift' is done under the hood.
