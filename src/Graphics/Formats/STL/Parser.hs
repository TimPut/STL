{-# LANGUAGE OverloadedStrings #-}

module Graphics.Formats.STL.Parser where

import           Prelude                    hiding (takeWhile)

import           Data.Attoparsec.Text
import           Data.Text                  (Text)
import           Graphics.Formats.STL.Types
import           Linear.V3                  hiding (triple)

-- | A parser from 'Text' to the @STL@ type.  The parser should be
-- fairly permissive about whitespace, but has not been tested enough
-- against STL files in the wild.
stlParser :: Parser STL
stlParser = STL <$> nameParser <*> many' triangle

nameParser :: Parser Text
nameParser = text "solid" *> takeWhile (inClass " -~") <* skipSpace

triangle :: Parser Triangle
triangle = Triangle <$> ss normalParser <*> loop <* text "endfacet"

loop :: Parser ((V3 Float), (V3 Float), (V3 Float))
loop = triple <$> (text "outer loop" *> ss vertex) <*> ss vertex <*> ss vertex <* text "endloop"

normalParser :: Parser (Maybe (V3 Float))
normalParser = text "facet" *> text "normal" *> do
    n <- v3
    return $ case n of
        (V3 0 0 0) -> Nothing
        _          -> Just n

vertex :: Parser (V3 Float)
vertex = text "vertex" *> v3

v3 :: Parser (V3 Float)
v3 = V3 <$> ss float <*> ss float <*> ss float

ss :: Parser a -> Parser a
ss p = p <* skipSpace

text :: Text -> Parser Text
text t = string t <* skipSpace

float :: Parser Float
float = realToFrac <$> double
