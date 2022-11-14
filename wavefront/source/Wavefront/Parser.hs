{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- A parser for the specific subset of @OBJ@ that we're likely to care about.
-- Open to extension later. The supported specification is a subset of the
-- specification given here: http://paulbourke.net/dataformats/obj/.
module Wavefront.Parser where

import Control.Monad (guard)
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Linear (M33, V2 (V2), V3 (V3))
import Numeric.Natural (Natural)
import Text.Parsec hiding (Line, State, parse, space)
import Text.Parsec.Number (floating, int, sign)

-- | Commands that we're interested in supporting.
type Command ∷ Type
data Command
  = GeometricVertex (V3   Double) -- ^ A point in 3D space.
  | TextureVertex   (V2   Double) -- ^ A 2D texture coordinate.
  | VertexNormal    (V3   Double) -- ^ A 3D surface normal vector.
  | Triangle        (M33 Natural) -- ^ Three triples of position/texture/normal.
  deriving stock (Eq, Ord, Show)

-- | Parse an @OBJ@ file into a list of 'Command' values. Note that lines that
-- fail to parse will be ignored: parse errors will only occur for lines that
-- begin with one of our recognised commands.
parser ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m [Command]
parser = do
  let ignore ∷ ParsecT s u m String
      ignore = manyTill anyChar $ try (lookAhead endOfLine)

      command ∷ ParsecT s u m (Maybe Command)
      command = choice
        [ Just    <$> choice [ v, vn, vt, f ]
        , Nothing <$  ignore
        ]

  commands ← command `sepEndBy` endOfLine
  pure (catMaybes commands)

-- | Parse a geometric vertex: an @x y z@ position in space.
v ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m Command
v = do
  _ ← try $ char 'v' *> notFollowedBy (oneOf "npt")

  x ← many1 space *> sign <*> floating
  y ← many1 space *> sign <*> floating
  z ← many1 space *> sign <*> floating

  pure $ GeometricVertex (V3 x y z)

-- | Parse a vertex normal: an @i j k@ vector.
vn ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m Command
vn = do
  _ ← try $ string "vn"

  x ← many1 space *> sign <*> floating
  y ← many1 space *> sign <*> floating
  z ← many1 space *> sign <*> floating

  pure $ VertexNormal (V3 x y z)

-- | Parse a texture coordinate: a @u v@ coordinate.
vt ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m Command
vt = do
  _ ← try $ string "vt"

  x ← many1 space *>          floating
  y ← many1 space *> option 0 floating

  pure $ TextureVertex (V2 x y)

-- | Parse a face: three vertex/texture/normal triples.
f ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m Command
f = do
  _ ← try $ char 'f'

  let index ∷ Stream s m Char ⇒ ParsecT s u m Natural
      index = do
        value ← int @Int

        guard (value > 0) <?> "positive index"
        pure (fromIntegral value)

      triple ∷ ParsecT s u m (V3 Natural)
      triple = do
        x ←             index
        y ← char '/' *> index
        z ← char '/' *> index

        pure (V3 x y z)

  a ← many1 space *> triple
  b ← many1 space *> triple
  c ← many1 space *> triple

  pure $ Triangle (V3 a b c)

-- | A space or a tab.
space ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m Char
space = oneOf "\t "
