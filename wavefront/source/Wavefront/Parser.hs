{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- A parser based on http://paulbourke.net/dataformats/obj/ for /polygonal/
-- geometry. I've tried to remove references to free-form geometry and
-- Wavefront-specific products where possible.
module Wavefront.Parser where

import Control.Applicative (liftA2)
import Control.Monad (ap, guard)
import Data.Char (isSpace)
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.OneOf (type (∈) (..), OneOf)
import Text.Parsec hiding (Line)
import Text.Parsec.Number (floating, int, sign)

parser
  ∷ ∀ s m u xs
  . ( GeometricVertex ∈ xs
    , VertexNormal    ∈ xs
    , TextureVertex   ∈ xs
    , Points          ∈ xs
    , Line            ∈ xs
    , Face            ∈ xs
    , GroupNames      ∈ xs
    , SmoothingGroup  ∈ xs
    , ObjectName      ∈ xs
    , Comment         ∈ xs

    , Stream s m Char
    )
  ⇒ ParsecT s u m [OneOf xs]
parser = do
  commands ← many $ choice
    [ Nothing <$ many1 space

    , fmap (Just . inj) vertexNormal
    , fmap (Just . inj) textureVertex
    , fmap (Just . inj) geometricVertex
    , fmap (Just . inj) points
    , fmap (Just . inj) line
    , fmap (Just . inj) face
    , fmap (Just . inj) groupNames
    , fmap (Just . inj) smoothingGroup
    , fmap (Just . inj) objectName
    , fmap (Just . inj) comment
    ]

  catMaybes commands <$ eof

-- * Vertex data

type GeometricVertex ∷ Type
data GeometricVertex
  = GeometricVertex { x ∷ Double, y ∷ Double, z ∷ Double }
  deriving stock (Eq, Ord, Show)

-- | Parse a 'GeometricVertex'.
geometricVertex ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m GeometricVertex
geometricVertex = command do
  _ ← char 'v'

  x ← many1 lineSpace *> ap sign floating
  y ← many1 lineSpace *> ap sign floating
  z ← many1 lineSpace *> ap sign floating

  pure GeometricVertex{..}

-- ** Vertex normals
--
--     vn i j k
--
-- Specifies a normal vector with components @i@, @j@, and @k@.
--
-- Vertex normals affect the smooth-shading and rendering of geometry. For
-- polygons, vertex normals are used in place of the actual facet normals.
--
-- When vertex normals are present, they supersede smoothing groups.
--
-- @i@ @j@ @k@ are the @i@, @j@, and @k@ coordinates for the vertex normal.
-- They are floating point numbers.
type VertexNormal ∷ Type
data VertexNormal
  = VertexNormal { i ∷ Double, j ∷ Double, k ∷ Double }
  deriving stock (Eq, Ord, Show)

-- | Parse a 'VertexNormal'.
vertexNormal ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m VertexNormal
vertexNormal = command do
  _ ← try (string "vn")

  i ← many1 lineSpace *> floating
  j ← many1 lineSpace *> floating
  k ← many1 lineSpace *> floating

  pure VertexNormal{..}

-- ** Texture vertices
--
--     vt u v w
--
-- Specifies a texture vertex and its coordinates. A @1D@ texture requires only
-- @u@ texture coordinates, a @2D@ texture requires both @u@ and @v@ texture
-- coordinates, and a @3D@ texture requires all three coordinates.
--
-- @u@ is the value for the horizontal direction of the texture.
--
-- @v@ is the value for the vertical direction of the texture. The default is
-- @0@.
--
-- @w@ is a value for the depth of the texture. The default is @0@.
type TextureVertex ∷ Type
data TextureVertex
  = TextureVertex { u ∷ Double, v ∷ Double, w ∷ Double }
  deriving stock (Eq, Ord, Show)

-- | Parse a 'TextureVertex'.
textureVertex :: ∀ s m u. Stream s m Char ⇒ ParsecT s u m TextureVertex
textureVertex = command do
  _ ← try (string "vt")

  u ← many1 lineSpace *> floating
  v ← many1 lineSpace *> option 0 floating
  w ← many1 lineSpace *> option 0 floating

  pure TextureVertex{..}

-- * Elements

-- ** Points
--
--     p  v1 v2 v3 . . .
--
-- Specifies a point element and its vertex. You can specify multiple points
-- with this statement. Although points cannot be shaded or rendered, they are
-- used by other Advanced Visualizer programs.
--
-- @v@ is the vertex reference number for a point element. Each point element
-- requires one vertex. Positive values indicate absolute vertex numbers.
-- Negative values indicate relative vertex numbers.
type Points ∷ Type
newtype Points = Points [Index GeometricVertex]
  deriving stock (Eq, Ord, Show)

points ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m Points
points = command do
  _ ← char 'p'

  let point ∷ ParsecT s u m (Index GeometricVertex)
      point = do
        index ← many1 lineSpace *> int

        guard (index /= 0) <?> "non-zero index"
        pure (Index index)

  fmap Points (many1 point)

-- ** Lines
--
--     l  v1/vt1   v2/vt2   v3/vt3 . . .
--
-- Specifies a line and its vertex reference numbers. You can optionally
-- include the texture vertex reference numbers. Although lines cannot be
-- shaded or rendered, they are used by other Advanced Visualizer programs.
--
-- The reference numbers for the vertices and texture vertices must be
-- separated by a slash (@/@). There is no space between the number and the
-- slash.
--
-- @v@ is a reference number for a vertex on the line. A minimum of two vertex
-- numbers are required. There is no limit on the maximum. Positive values
-- indicate absolute vertex numbers. Negative values indicate relative vertex
-- numbers.
--
-- @vt@ is an optional argument. @vt@ is the reference number for a texture
-- vertex in the line element. It must always follow the first slash.
type Line ∷ Type
newtype Line = Line [(Index GeometricVertex, Maybe (Index TextureVertex))]
  deriving stock (Eq, Ord, Show)

-- | Parse a 'Line'.
line ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m Line
line = command do
  _ ← char 'l'

  fmap Line $ atLeast 2 do
    vertex ← many1 lineSpace *> int
    guard (vertex /= 0) <?> "non-zero vertex index"

    texture ← optionMaybe do
      texture ← char '/' *> int
      guard (texture /= 0) <?> "non-zero texture index"

      pure texture

    pure (Index vertex, fmap Index texture)

-- ** Faces
--
--     f  v1/vt1/vn1   v2/vt2/vn2   v3/vt3/vn3 . . .
--
-- Specifies a face element and its vertex reference number. You can optionally
-- include the texture vertex and vertex normal reference numbers.
--
-- The reference numbers for the vertices, texture vertices, and vertex normals
-- must be separated by slashes (@/@). There is no space between the number and
-- the slash.
--
-- @v@ is the reference number for a vertex in the face element. A minimum of
-- three vertices are required.
--
-- @vt@ is an optional argument. @vt@ is the reference number for a texture
-- vertex in the face element. It always follows the first slash.
--
-- @vn@ is an optional argument. @vn@ is the reference number for a vertex
-- normal in the face element. It must always follow the second slash.
--
-- Face elements use surface normals to indicate their orientation. If
-- vertices are ordered counterclockwise around the face, both the
-- face and the normal will point toward the viewer. If the vertex
-- ordering is clockwise, both will point away from the viewer. If
-- vertex normals are assigned, they should point in the general
-- direction of the surface normal, otherwise unpredictable results
-- may occur.
--
-- If a face has a texture map assigned to it and no texture vertices
-- are assigned in the @f@ statement, the texture map is ignored when
-- the element is rendered.
type Face ∷ Type
data Face = Face
  [ ( Index GeometricVertex
    , Maybe (Index TextureVertex)
    , Maybe (Index VertexNormal)
    )
  ]
  deriving stock (Eq, Ord, Show)

-- | Parse a 'Face'.
face ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m Face
face = command do
  _ ← char 'f'

  vertices ← atLeast 3 do
    vertex ← many1 lineSpace *> int
    guard (vertex /= 0) <?> "non-zero geometric vertex index"

    (texture, normal) ← option (Nothing, Nothing) do
      _ ← char '/'

      texture ← optionMaybe do
        texture ← int
        guard (texture /= 0) <?> "non-zero texture vertex index"

        pure texture

      normal ← option Nothing do
        _ ← char '/'

        normal ← optionMaybe do
          normal ← int
          guard (normal /= 0) <?> "non-zero normal vector index"

          pure normal

        pure normal

      pure (texture, normal)

    pure (Index vertex, fmap Index texture, fmap Index normal)

  pure (Face vertices)

-- * Grouping

-- ** Group names
--
--     g group_name1 group_name2 . . .
--
-- Specifies the group name for the elements that follow it. You can have
-- multiple group names. If there are multiple groups on one line, the data
-- that follows belong to all groups. Group information is optional.
--
-- @group_name@ is the name for the group. Letters, numbers, and combinations
-- of letters and numbers are accepted for group names. The default group name
-- is @default@.
newtype GroupNames = GroupNames [String]
  deriving stock (Eq, Ord, Show)

-- | Parse some 'GroupNames'.
groupNames ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m GroupNames
groupNames = command do
  _ ← char 'g'

  names ← many1 do
    _ ← many1 lineSpace
    many1 alphaNum

  pure (GroupNames names)

-- ** Smoothing groups
--
--     s group_number
--
-- Sets the smoothing group for the elements that follow it. If you do not want
-- to use a smoothing group, specify @off@ or a value of @0@.
--
-- @group_number@ is the smoothing group number. To turn off smoothing groups,
-- use a value of @0@ or @off@.
newtype SmoothingGroup = SmoothingGroup (Maybe Int)
  deriving stock (Eq, Ord, Show)

-- | Parse a 'SmoothingGroup'.
smoothingGroup ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m SmoothingGroup
smoothingGroup = command do
  _ ← char 's' *> many1 lineSpace

  group ←
    choice
      [ string "off" *> pure Nothing
      , char    '0'  *> pure Nothing
      , fmap Just int
      ]

  pure (SmoothingGroup group)

-- ** Object names
--
--     o object_name
--
-- Optional statement. It specifies a user-defined object name for the elements
-- defined after this statement.
--
-- @object_name@ is the user-defined object name. There is no default.
newtype ObjectName = ObjectName { getObjectName ∷ String }
  deriving newtype (Eq, Ord, Show)

-- | Parse an 'ObjectName'.
objectName ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m ObjectName
objectName = command do
  _ ← char 'o'

  name ← many1 lineSpace *> manyTill anyChar (try endOfLine)
  pure (ObjectName name)

-- * Comments
--
--     # this is a comment
--
-- Comments can appear anywhere in an @.obj@ file. They are used to annotate the
-- file; they are not processed.
newtype Comment = Comment { getComment ∷ String }
  deriving newtype (Eq, Ord, Show)

-- | Parse a 'Comment'.
comment ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m Comment
comment = command do
  _ ← char '#'

  text ← many1 lineSpace *> manyTill anyChar (try $ lookAhead endOfLine)
  pure (Comment text)

-- * Helpers

-- | Parse a space that isn't a newline.
lineSpace ∷ ∀ s m u. Stream s m Char ⇒ ParsecT s u m Char
lineSpace = satisfy \c → isSpace c && c /= '\n' && c /= '\r'

-- | Run a parser at least the given number of times.
atLeast ∷ ∀ s m t u x. Stream s m t ⇒ Int → ParsecT s u m x → ParsecT s u m [x]
atLeast n xs = liftA2 (<>) (count n xs) (many xs)

-- | Define a command parser. Commands are trimmed of space on either side, and
-- assumed to end in an EOL.
command ∷ ∀ s m u x. Stream s m Char ⇒ ParsecT s u m x → ParsecT s u m x
command inner = inner <* manyTill space (try endOfLine)

-- | A newtype for keeping track of /what/ we're indexing.
type Index ∷ Type → Type
data Index x = Index { getIndex ∷ Int }
  deriving stock (Eq, Functor, Ord, Show)
