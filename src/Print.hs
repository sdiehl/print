{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Print (
  Show(showsPrec, show),
  ShowS,
  showChar,
  showText,
  showList,
  showList__,
  showParen,
  shows,

  con0,
  con1,

  print,
) where

import Data.Int
import Data.Word
import Data.Char
import Data.Monoid
import Data.Either
import Data.Function
import Data.Foldable

import Data.Text.Lazy (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Data.Text.Buildable (build)
import qualified Data.Text.Lazy.Builder as TB

import GHC.Num
import GHC.Exts hiding (build)
import GHC.Base hiding (build)
import GHC.Generics

default (Text)


type ShowS = TB.Builder -> TB.Builder

data Type = Rec | Pref | Inf Text

appPrec, appPrec1 :: Int
appPrec = 10
appPrec1 = 11

showText :: Text -> ShowS
showText = (<>) . TB.fromLazyText
{-# INLINABLE showText #-}

-- For compatability with GHC.Generics
showString :: String -> ShowS
showString s = (TB.fromString s <>)
{-# INLINABLE showString #-}

showChar :: Char -> ShowS
showChar c = (TB.singleton c <>)
{-# INLINABLE showChar #-}

showSpace :: ShowS
showSpace = showChar ' '

showParen :: Bool -> ShowS -> ShowS
showParen True p = showChar '(' . p . showChar ')'
showParen False p = p

showBraces :: ShowS -> ShowS
showBraces p = showChar '{' . p . showChar '}'

showQuotes :: ShowS -> ShowS
showQuotes p = showChar '"' . p . showChar '"'

showList__ :: (a -> ShowS) -> [a] -> ShowS
showList__ _     []     s = "[]" <> s
showList__ showx (x:xs) s = "[" <> showx x (showl xs)
  where
    showl []     = "]" <> s
    showl (y:ys) = "," <> showx y (showl ys)

showList :: Show a => [a] -> TB.Builder -> TB.Builder
showList = showList__ shows 

shows :: (Show a) => a -> ShowS
shows = showsPrec 0
{-# INLINABLE shows #-}

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> TL.Text

  default showsPrec 
    :: (Generic a, GShow (Rep a))
    => Int -> a -> ShowS
  showsPrec i x = gshowsPrec Pref i (from x)

  show x = TB.toLazyText (shows x "")

class GShow f where
  gshowsPrec :: Type -> Int -> f a -> ShowS
  basic :: f a -> Bool
  basic _ = False

instance  GShow U1 where
  gshowsPrec _ _ _ = id
  basic _ = True

instance Show c => GShow (K1 i c) where
  gshowsPrec _ i (K1 fp) = showsPrec i fp

instance (GShow f, Constructor c) => GShow (M1 C c f) where
  gshowsPrec t i c@(M1 fp) 
    | conIsRecord c = 
          showString (conName c)
        . showChar ' '
        . showBraces (gshowsPrec Rec i fp)

    | otherwise = case conFixity c of
      Prefix -> showParen (i > appPrec && not (basic fp)) $
          showString (conName c)
        . if basic fp then id else showChar ' '
        . gshowsPrec t appPrec1 fp

      Infix _ m -> showParen (i > m) $ 
        showBraces (gshowsPrec t m fp)

instance (GShow f, Selector c) => GShow (M1 S c f) where
  gshowsPrec t i c@(M1 fp) = case t of
    Pref    -> gshowsPrec t i fp
    Inf _   -> gshowsPrec t i fp
    Rec     ->
        showString (selName c)
      . showText " = "
      . gshowsPrec t i fp

instance (GShow f) => GShow (M1 D c f) where
  gshowsPrec t i (M1 fp) = gshowsPrec t i fp

instance (GShow f, GShow g) => GShow (f :+: g) where
  gshowsPrec t i (L1 fp) = gshowsPrec t i fp
  gshowsPrec t i (R1 fp) = gshowsPrec t i fp

instance (GShow a, GShow b) => GShow (a :*: b) where
  gshowsPrec t@Rec n (a :*: b) =
      gshowsPrec t n a 
    . showText ", "
    . gshowsPrec t n b

  gshowsPrec t@Pref n (a :*: b) =
      gshowsPrec t (n+1) a 
    . showChar ' '
    . gshowsPrec t (n+1) b

  gshowsPrec t@(Inf s) n (a :*: b) =
      gshowsPrec t n a 
    . showText s 
    . gshowsPrec t n b

-- Buildable instances

instance Show Int where
  showsPrec _ n f = build n <> f

instance Show Int8 where
  showsPrec _ n f = build n <> f

instance Show Int16 where
  showsPrec _ n f = build n <> f

instance Show Int32 where
  showsPrec _ n f = build n <> f

instance Show Int64 where
  showsPrec _ n f = build n <> f

instance Show Integer where
  showsPrec _ n f = build n <> f

instance Show Word where
  showsPrec _ n f =  build n <> f

instance Show Word16 where
  showsPrec _ n f =  build n <> f

instance Show Word32 where
  showsPrec _ n f =  build n <> f

instance Show Word64 where
  showsPrec _ n f =  build n <> f

instance Show Float where
  showsPrec _ n f = build n <> f

instance Show Double where
  showsPrec _ n f = build n <> f

instance Show T.Text where
  showsPrec _ n f = build n <> f

instance Show TL.Text where
  showsPrec _ n f = build n <> f

instance Show TB.Builder where
  showsPrec _ n f = build n <> f

instance Show Char where
  showsPrec _ '\'' = showText "'\\''"
  showsPrec _ c    = showChar '\'' . showChar c . showChar '\''

instance Show a => Show [a]  where
  showsPrec _ = showList

{-# INLINE con0 #-}
con0 :: Text -> ShowS
con0 = showText

{-# INLINE con1 #-}
con1 :: Show a => Text -> Int -> a -> ShowS
con1 con p x = 
  showParen (p > appPrec) $
    showText con .
    showsPrec appPrec1 x

instance Show () where
  showsPrec _ () = showText "()"

instance Show Bool where
  showsPrec _ True = showText "True"
  showsPrec _ False = showText "False"

instance Show Ordering where
  showsPrec _ LT = showText "LT"
  showsPrec _ EQ = showText "EQ"
  showsPrec _ GT = showText "GT"

instance Show a => Show (Maybe a) where
  showsPrec _  Nothing s = con0 "Nothing" s
  showsPrec p (Just x) s = con1 "Just " p x s

instance (Show a, Show b) => Show (Either a b) where
  showsPrec p (Left x) s = con1 "Left " p x s
  showsPrec p (Right x) s = con1 "Right " p x s

instance (Show a, Show b) => Show (a,b)  where
  showsPrec _ (a,b) = showTuple [shows a, shows b]

instance (Show a, Show b, Show c) => Show (a,b,c)  where
  showsPrec _ (a,b,c) = showTuple [shows a, shows b, shows c]

instance (Show a, Show b, Show c, Show d) => Show (a,b,c,d)  where
  showsPrec _ (a,b,c,d) = showTuple [shows a, shows b, shows c, shows d]

showTuple :: [ShowS] -> ShowS
showTuple ss = 
    showChar '('
  . foldr1 (\s r -> s . showChar ',' . r) ss
  . showChar ')'

print :: Show a => a -> IO ()
print = TL.putStrLn . show
