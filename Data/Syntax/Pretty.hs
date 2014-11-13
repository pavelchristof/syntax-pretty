{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module      :  Data.Syntax.Pretty
Description :  Syntax instance for Text.PrettyPrint.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental

Provides a Syntax instance for Text.PrettyPrint.

-}
module Data.Syntax.Pretty (
    Printer(..)
    ) where

import           Control.Applicative
import           Control.Lens.SemiIso
import           Control.Monad
import           Data.Monoid
import           Data.SemiIsoFunctor
import           Data.Syntax
import           Data.Syntax.Char
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Text.PrettyPrint as P

-- | A printer is a function @a -> Either String Doc@.
newtype Printer a = Printer { 
    -- | Runs the printer.
    runPrinter :: a -> Either String P.Doc
}

instance SemiIsoFunctor Printer where
    simap ai (Printer f) = Printer (apply ai >=> f)

instance SemiIsoApply Printer where
    sipure ai = Printer (\a -> either Left (\_ -> Right mempty) (apply ai a))
    (Printer f) /*/ (Printer g) = Printer (\(a, b) -> (<>) <$> f a <*> g b)

instance SemiIsoAlternative Printer where
    siempty = Printer (\_ -> Left "error")
    (Printer f) /|/ (Printer g) = Printer (\a -> f a <|> g a)

instance SemiIsoMonad Printer where
    (Printer f) //= g = Printer (\(a, b) -> (<>) <$> f a <*> runPrinter (g a) b)

instance SemiIsoFix Printer where
    sifix f = Printer $ \a -> runPrinter (f a) a

printText :: Text -> Either String P.Doc
printText = Right . P.text . T.unpack

instance Syntax Printer Text where
    anyChar = Printer (Right . P.char)
    take n = Printer (printText . T.take n)
    takeWhile p = Printer (printText . T.takeWhile p)
    takeWhile1 p = Printer (printText <=< notNull . T.takeWhile p)
      where notNull t | T.null t  = Left "takeWhile1: failed"
                      | otherwise = Right t
    takeTill1 p = Printer (printText <=< notNull . T.takeWhile p)
      where notNull t | T.null t  = Left "takeTill1: failed"
                      | otherwise = Right t

instance SyntaxChar Printer Text where
    decimal = Printer (format . toInteger)
      where format i | i < 0 = Left "decimal: negative number"
                     | otherwise = Right (P.integer i)
    scientific = Printer (Right . P.text . show)
