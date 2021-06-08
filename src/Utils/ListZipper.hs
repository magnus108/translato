module Utils.ListZipper
    ( ListZipper(..)
    , fromList
    , bextend
    , remove
    , setter
    , toList
    , zipperL
    , forward
    , backward
    , add
    )
where

import           Control.Lens                   ( Lens'
                                                , lens
                                                )

import           Prelude                 hiding ( fromList
                                                , toList
                                                )
import           Control.Comonad
import           Data.Aeson

import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( (<|) )

data ListZipper a = ListZipper [a] a [a]
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


setter :: (ListZipper a) -> a -> (ListZipper a)
setter (ListZipper ls _ rs) new = ListZipper ls new rs


add :: a -> (ListZipper a) -> (ListZipper a)
add new (ListZipper ls x rs) = ListZipper ls new (x:rs)

remove :: ListZipper a -> ListZipper a
remove (ListZipper [] x []) = ListZipper [] x []
remove (ListZipper [] x (r:rs)) = ListZipper [] r rs
remove (ListZipper (l:ls) x rs) = ListZipper ls l rs


toList :: ListZipper a -> [a]
toList (ListZipper ls x rs) = (reverse ls) ++ (x : rs)

iterate' :: (a -> Maybe a) -> a -> NonEmpty a
iterate' f x = case f x of
    Just x' -> x <| (iterate' f x')
    Nothing -> x :| []

forward' :: ListZipper a -> Maybe (ListZipper a)
forward' (ListZipper ls a (r : rs)) = Just (ListZipper (a : ls) r rs)
forward' (ListZipper _  _ []      ) = Nothing

forward :: ListZipper a -> ListZipper a
forward a = fromMaybe a (forward' a)

backward :: ListZipper a -> ListZipper a
backward a = fromMaybe a (backward' a)


backward' :: ListZipper a -> Maybe (ListZipper a)
backward' (ListZipper (l : ls) a rs) = Just (ListZipper ls l (a : rs))
backward' (ListZipper []       _ _ ) = Nothing

fromList :: [a] -> Maybe (ListZipper a)
fromList []       = Nothing
fromList (x : xs) = Just $ ListZipper [] x xs


bextend :: (Bool -> ListZipper a -> b) -> ListZipper a -> ListZipper b
bextend f l =
    let (ListZipper xs _  ys) = extend (f False) l
        (ListZipper _  y' _ ) = extend (f True) l
        l'                    = ListZipper xs y' ys
    in  l'

instance Functor ListZipper where
    fmap f (ListZipper ls a rs) = ListZipper (fmap f ls) (f a) (fmap f rs)

instance Comonad ListZipper where
    extract (ListZipper _ a _) = a
    duplicate a = ListZipper (shift backward') a (shift forward')
        where shift move = NE.tail $ iterate' move a

instance Foldable ListZipper where
    foldMap f (ListZipper l x r) = foldMap f (reverse l) <> f x <> foldMap f r

instance Traversable ListZipper where
    traverse f (ListZipper l x r) =
        ListZipper <$> traverse f l <*> f x <*> traverse f r


zipperL :: Lens' (ListZipper a) a
zipperL = lens getter setter
  where
    getter :: ListZipper a -> a
    getter = extract

    setter :: (ListZipper a) -> a -> (ListZipper a)
    setter (ListZipper ls _ rs) new = ListZipper ls new rs
