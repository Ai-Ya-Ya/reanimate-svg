{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Graphics.SvgTree.Types.Hashable where

import Codec.Picture (PixelRGBA8 (..))
import Control.Lens
import Data.Hashable
import GHC.Generics (Generic)
import Graphics.SvgTree.Types.Internal

-- ----------------------------------------------------------------------
-- 1. Heavy Leafs & Recursion Breakers
-- ----------------------------------------------------------------------

instance Hashable PixelRGBA8 where
  hashWithSalt s (PixelRGBA8 r g b a) = 
    s `hashWithSalt` r `hashWithSalt` g `hashWithSalt` b `hashWithSalt` a

-- Massive record (26 fields). Manual instance prevents large Product tree.
instance Hashable DrawAttributes where
  hashWithSalt s da = s
    `hashWithSalt` _strokeWidth da
    `hashWithSalt` _strokeColor da
    `hashWithSalt` _strokeOpacity da
    `hashWithSalt` _strokeLineCap da
    `hashWithSalt` _strokeLineJoin da
    `hashWithSalt` _strokeMiterLimit da
    `hashWithSalt` _fillColor da
    `hashWithSalt` _fillOpacity da
    `hashWithSalt` _groupOpacity da
    `hashWithSalt` _transform da
    `hashWithSalt` _fillRule da
    `hashWithSalt` _maskRef da
    `hashWithSalt` _clipPathRef da
    `hashWithSalt` _clipRule da
    `hashWithSalt` _attrClass da
    `hashWithSalt` _attrId da
    `hashWithSalt` _strokeOffset da
    `hashWithSalt` _strokeDashArray da
    `hashWithSalt` _fontSize da
    `hashWithSalt` _fontFamily da
    `hashWithSalt` _fontStyle da
    `hashWithSalt` _textAnchor da
    `hashWithSalt` _markerStart da
    `hashWithSalt` _markerMid da
    `hashWithSalt` _markerEnd da
    `hashWithSalt` _filterRef da

-- Moderate sum type (10 constructors), frequently used.
instance Hashable PathCommand where
  hashWithSalt s pc = case pc of
    MoveTo o p           -> s `hashWithSalt` (0::Int) `hashWithSalt` o `hashWithSalt` p
    LineTo o p           -> s `hashWithSalt` (1::Int) `hashWithSalt` o `hashWithSalt` p
    HorizontalTo o c     -> s `hashWithSalt` (2::Int) `hashWithSalt` o `hashWithSalt` c
    VerticalTo o c       -> s `hashWithSalt` (3::Int) `hashWithSalt` o `hashWithSalt` c
    CurveTo o c          -> s `hashWithSalt` (4::Int) `hashWithSalt` o `hashWithSalt` c
    SmoothCurveTo o c    -> s `hashWithSalt` (5::Int) `hashWithSalt` o `hashWithSalt` c
    QuadraticBezier o c  -> s `hashWithSalt` (6::Int) `hashWithSalt` o `hashWithSalt` c
    SmoothQuadraticBezierCurveTo o p -> s `hashWithSalt` (7::Int) `hashWithSalt` o `hashWithSalt` p
    EllipticalArc o c    -> s `hashWithSalt` (8::Int) `hashWithSalt` o `hashWithSalt` c
    EndPath              -> s `hashWithSalt` (9::Int)

-- Recursive Sum: FilterElement <-> Merge/ComponentTransfer <-> FilterElement
instance Hashable FilterElement where
  hashWithSalt s fe = case fe of
    FEBlend v             -> s `hashWithSalt` (0::Int)  `hashWithSalt` v
    FEColorMatrix v       -> s `hashWithSalt` (1::Int)  `hashWithSalt` v
    FEComponentTransfer v -> s `hashWithSalt` (2::Int)  `hashWithSalt` v
    FEComposite v         -> s `hashWithSalt` (3::Int)  `hashWithSalt` v
    FEConvolveMatrix v    -> s `hashWithSalt` (4::Int)  `hashWithSalt` v
    FEDiffuseLighting v   -> s `hashWithSalt` (5::Int)  `hashWithSalt` v
    FEDisplacementMap v   -> s `hashWithSalt` (6::Int)  `hashWithSalt` v
    FEDropShadow v        -> s `hashWithSalt` (7::Int)  `hashWithSalt` v
    FEFlood v             -> s `hashWithSalt` (8::Int)  `hashWithSalt` v
    FEFuncA v             -> s `hashWithSalt` (9::Int)  `hashWithSalt` v
    FEFuncB v             -> s `hashWithSalt` (10::Int) `hashWithSalt` v
    FEFuncG v             -> s `hashWithSalt` (11::Int) `hashWithSalt` v
    FEFuncR v             -> s `hashWithSalt` (12::Int) `hashWithSalt` v
    FEGaussianBlur v      -> s `hashWithSalt` (13::Int) `hashWithSalt` v
    FEImage v             -> s `hashWithSalt` (14::Int) `hashWithSalt` v
    FEMerge v             -> s `hashWithSalt` (15::Int) `hashWithSalt` v
    FEMergeNode v         -> s `hashWithSalt` (16::Int) `hashWithSalt` v
    FEMorphology v        -> s `hashWithSalt` (17::Int) `hashWithSalt` v
    FEOffset v            -> s `hashWithSalt` (18::Int) `hashWithSalt` v
    FESpecularLighting v  -> s `hashWithSalt` (19::Int) `hashWithSalt` v
    FETile v              -> s `hashWithSalt` (20::Int) `hashWithSalt` v
    FETurbulence v        -> s `hashWithSalt` (21::Int) `hashWithSalt` v
    FENone                -> s `hashWithSalt` (22::Int)

-- Recursive Sum: TextSpan <-> TextSpanContent <-> TextSpan
instance Hashable TextSpanContent where
  hashWithSalt s c = case c of
    SpanText t    -> s `hashWithSalt` (0::Int) `hashWithSalt` t
    SpanTextRef r -> s `hashWithSalt` (1::Int) `hashWithSalt` r
    SpanSub sub   -> s `hashWithSalt` (2::Int) `hashWithSalt` sub

instance Hashable TextSpan where
  hashWithSalt s (TextSpan info attrs content) = 
    s `hashWithSalt` info `hashWithSalt` attrs `hashWithSalt` content

-- Recursive Sum: Element contains Tree.
instance Hashable Element where
  hashWithSalt s e = case e of
    ElementLinearGradient v -> s `hashWithSalt` (0::Int) `hashWithSalt` v
    ElementRadialGradient v -> s `hashWithSalt` (1::Int) `hashWithSalt` v
    ElementMeshGradient v   -> s `hashWithSalt` (2::Int) `hashWithSalt` v
    ElementGeometry v       -> s `hashWithSalt` (3::Int) `hashWithSalt` v -- RECURSION
    ElementPattern v        -> s `hashWithSalt` (4::Int) `hashWithSalt` v
    ElementMarker v         -> s `hashWithSalt` (5::Int) `hashWithSalt` v
    ElementMask v           -> s `hashWithSalt` (6::Int) `hashWithSalt` v
    ElementClipPath v       -> s `hashWithSalt` (7::Int) `hashWithSalt` v

-- ----------------------------------------------------------------------
-- 2. Tree Containers (The Spine)
-- ----------------------------------------------------------------------

instance Hashable Group where
  hashWithSalt s g = s 
    `hashWithSalt` _groupDrawAttributes g
    `hashWithSalt` _groupChildren g
    `hashWithSalt` _groupViewBox g
    `hashWithSalt` _groupAspectRatio g

instance Hashable Marker where
  hashWithSalt s m = s
    `hashWithSalt` _markerDrawAttributes m
    `hashWithSalt` _markerRefPoint m
    `hashWithSalt` _markerWidth m
    `hashWithSalt` _markerHeight m
    `hashWithSalt` _markerOrient m
    `hashWithSalt` _markerUnits m
    `hashWithSalt` _markerViewBox m
    `hashWithSalt` _markerOverflow m
    `hashWithSalt` _markerAspectRatio m
    `hashWithSalt` _markerElements m -- RECURSION

instance Hashable Pattern where
  hashWithSalt s p = s
    `hashWithSalt` _patternDrawAttributes p
    `hashWithSalt` _patternViewBox p
    `hashWithSalt` _patternWidth p
    `hashWithSalt` _patternHeight p
    `hashWithSalt` _patternPos p
    `hashWithSalt` _patternHref p
    `hashWithSalt` _patternElements p -- RECURSION
    `hashWithSalt` _patternUnit p
    `hashWithSalt` _patternAspectRatio p
    `hashWithSalt` _patternTransform p

instance Hashable Mask where
  hashWithSalt s m = s
    `hashWithSalt` _maskDrawAttributes m
    `hashWithSalt` _maskContentUnits m
    `hashWithSalt` _maskUnits m
    `hashWithSalt` _maskPosition m
    `hashWithSalt` _maskWidth m
    `hashWithSalt` _maskHeight m
    `hashWithSalt` _maskContent m -- RECURSION

instance Hashable ClipPath where
  hashWithSalt s c = s
    `hashWithSalt` _clipPathDrawAttributes c
    `hashWithSalt` _clipPathUnits c
    `hashWithSalt` _clipPathContent c -- RECURSION

instance Hashable Document where
  hashWithSalt s d = s
    `hashWithSalt` _documentViewBox d
    `hashWithSalt` _documentWidth d
    `hashWithSalt` _documentHeight d
    `hashWithSalt` _documentElements d -- RECURSION
    `hashWithSalt` _documentDescription d
    `hashWithSalt` _documentLocation d
    `hashWithSalt` _documentAspectRatio d

-- ----------------------------------------------------------------------
-- 3. Standard Derivations (Non-recursive / Small)
-- ----------------------------------------------------------------------

deriving instance Hashable CoordinateUnits
deriving instance Hashable PreserveAspectRatio
deriving instance Hashable Alignment
deriving instance Hashable MeetSlice
deriving instance Hashable LinearGradient
deriving instance Hashable Spread
deriving instance Hashable Transformation
deriving instance Hashable GradientStop
deriving instance Hashable GradientPathCommand
deriving instance Hashable Origin
deriving instance Hashable Use
deriving instance Hashable Filter
deriving instance Hashable FilterAttributes
deriving instance Hashable Blend
deriving instance Hashable BlendMode
deriving instance Hashable ConvolveMatrix
deriving instance Hashable Morphology
deriving instance Hashable OperatorType
deriving instance Hashable NumberOptionalNumber
deriving instance Hashable SpecularLighting
deriving instance Hashable DropShadow
deriving instance Hashable DiffuseLighting
deriving instance Hashable Flood
deriving instance Hashable Tile
deriving instance Hashable Offset
deriving instance Hashable Merge
deriving instance Hashable MergeNode
deriving instance Hashable ImageF
deriving instance Hashable ComponentTransfer
deriving instance Hashable FuncType
deriving instance Hashable FuncA
deriving instance Hashable FuncR
deriving instance Hashable FuncG
deriving instance Hashable FuncB
deriving instance Hashable ColorMatrix
deriving instance Hashable FilterSource
deriving instance Hashable ColorMatrixType
deriving instance Hashable Composite
deriving instance Hashable CompositeOperator
deriving instance Hashable DisplacementMap
deriving instance Hashable ChannelSelector
deriving instance Hashable GaussianBlur
deriving instance Hashable EdgeMode
deriving instance Hashable Turbulence
deriving instance Hashable StitchTiles
deriving instance Hashable TurbulenceType
deriving instance Hashable Path
deriving instance Hashable Circle
deriving instance Hashable PolyLine
deriving instance Hashable Polygon
deriving instance Hashable Ellipse
deriving instance Hashable Line
deriving instance Hashable Rectangle
deriving instance Hashable TextPath
deriving instance Hashable TextPathMethod
deriving instance Hashable TextPathSpacing
deriving instance Hashable Text
deriving instance Hashable TextAdjust
deriving instance Hashable TextInfo
deriving instance Hashable Image
deriving instance Hashable RadialGradient
deriving instance Hashable MeshGradient
deriving instance Hashable MeshGradientType
deriving instance Hashable MeshGradientRow
deriving instance Hashable MeshGradientPatch
deriving instance Hashable MarkerOrientation
deriving instance Hashable MarkerUnit
deriving instance Hashable Overflow
deriving instance Hashable Texture
deriving instance Hashable Cap
deriving instance Hashable LineJoin
deriving instance Hashable FillRule
deriving instance Hashable ElementRef
deriving instance Hashable FontStyle
deriving instance Hashable TextAnchor

-- ----------------------------------------------------------------------
-- 4. Tree Branch Manual Derivation (Crucial)
-- ----------------------------------------------------------------------

instance Hashable Tree where
  {-# NOINLINE hashWithSalt #-}
  hashWithSalt s = hashWithSalt s . _treeHash

treeBranch :: Lens' Tree TreeBranch
treeBranch = lens _treeBranch $ const Tree

instance Hashable TreeBranch where
  hashWithSalt s branch = case branch of 
    NoNode -> label 0 s
    UseNode i n -> label 1 s `hashWithSalt` (i, n)
    GroupNode g -> label 2 s `hashWithSalt` g
    SymbolNode g -> label 3 s `hashWithSalt` g
    DefinitionNode g -> label 4 s `hashWithSalt` g
    FilterNode f -> label 5 s `hashWithSalt` f
    PathNode p -> label 6 s `hashWithSalt` p
    CircleNode c -> label 7 s `hashWithSalt` c
    PolyLineNode p -> label 8 s `hashWithSalt` p
    PolygonNode p -> label 9 s `hashWithSalt` p
    EllipseNode e -> label 10 s `hashWithSalt` e
    LineNode l -> label 11 s `hashWithSalt` l
    RectangleNode r -> label 12 s `hashWithSalt` r
    TextNode m t -> label 13 s `hashWithSalt` (m, t)
    ImageNode i -> label 14 s `hashWithSalt` i
    LinearGradientNode l -> label 15 s `hashWithSalt` l
    RadialGradientNode r -> label 16 s `hashWithSalt` r
    MeshGradientNode m -> label 17 s `hashWithSalt` m
    PatternNode p -> label 18 s `hashWithSalt` p
    MarkerNode m -> label 19 s `hashWithSalt` m
    MaskNode m -> label 20 s `hashWithSalt` m
    ClipPathNode c -> label 21 s `hashWithSalt` c
    SvgNode d -> label 22 s `hashWithSalt` d
    where label n = (`hashWithSalt` (n :: Int))
  {-# NOINLINE hashWithSalt #-}

instance WithDefaultSvg Tree where
  defaultSvg = Tree NoNode

unpack :: Tree -> TreeBranch
unpack = _treeBranch

pattern Tree :: TreeBranch -> Tree
pattern Tree branch <-
  CachedTree {_treeBranch = branch}
  where
    Tree branch =
      CachedTree
        { _treeBranch = branch,
          _treeHash = hash branch
        }

pattern GroupTree :: Group -> Tree
pattern GroupTree g = Tree (GroupNode g)

pattern SymbolTree :: Group -> Tree
pattern SymbolTree g = Tree (SymbolNode g)

pattern DefinitionTree :: Group -> Tree
pattern DefinitionTree g = Tree (DefinitionNode g)

pattern None :: Tree
pattern None = Tree NoNode

pattern UseTree :: Use -> Maybe Tree -> Tree
pattern UseTree info sub = Tree (UseNode info sub)

pattern FilterTree :: Filter -> Tree
pattern FilterTree f = Tree (FilterNode f)

pattern PathTree :: Path -> Tree
pattern PathTree f = Tree (PathNode f)

pattern CircleTree :: Circle -> Tree
pattern CircleTree f = Tree (CircleNode f)

pattern PolyLineTree :: PolyLine -> Tree
pattern PolyLineTree f = Tree (PolyLineNode f)

pattern PolygonTree :: Polygon -> Tree
pattern PolygonTree f = Tree (PolygonNode f)

pattern EllipseTree :: Ellipse -> Tree
pattern EllipseTree f = Tree (EllipseNode f)

pattern LineTree :: Line -> Tree
pattern LineTree f = Tree (LineNode f)

pattern RectangleTree :: Rectangle -> Tree
pattern RectangleTree f = Tree (RectangleNode f)

pattern TextTree :: Maybe TextPath -> Text -> Tree
pattern TextTree p t = Tree (TextNode p t)

pattern ImageTree :: Image -> Tree
pattern ImageTree n = Tree (ImageNode n)

pattern LinearGradientTree :: LinearGradient -> Tree
pattern LinearGradientTree n = Tree (LinearGradientNode n)

pattern RadialGradientTree :: RadialGradient -> Tree
pattern RadialGradientTree n = Tree (RadialGradientNode n)

pattern MeshGradientTree :: MeshGradient -> Tree
pattern MeshGradientTree n = Tree (MeshGradientNode n)

pattern PatternTree :: Pattern -> Tree
pattern PatternTree n = Tree (PatternNode n)

pattern MarkerTree :: Marker -> Tree
pattern MarkerTree n = Tree (MarkerNode n)

pattern MaskTree :: Mask -> Tree
pattern MaskTree n = Tree (MaskNode n)

pattern ClipPathTree :: ClipPath -> Tree
pattern ClipPathTree n = Tree (ClipPathNode n)

pattern SvgTree :: Document -> Tree
pattern SvgTree n = Tree (SvgNode n)
