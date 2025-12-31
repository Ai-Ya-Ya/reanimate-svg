{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Graphics.SvgTree.Types.Hashable where

import Codec.Picture (PixelRGBA8 (..))
import Control.Lens
import Data.Hashable 
import Data.Hashable.Generic (genericHashWithSalt)
import GHC.Generics (Generic)
import Graphics.SvgTree.Types.Internal

-- Orphan instances :(

deriving instance Generic PixelRGBA8
instance Hashable PixelRGBA8 where
  hashWithSalt = genericHashWithSalt

instance Hashable DrawAttributes where
  hashWithSalt = genericHashWithSalt

instance Hashable Pattern where
  hashWithSalt = genericHashWithSalt

instance Hashable Element where
  hashWithSalt = genericHashWithSalt

instance Hashable ClipPath where
  hashWithSalt = genericHashWithSalt

instance Hashable Mask where
  hashWithSalt = genericHashWithSalt

instance Hashable CoordinateUnits where
  hashWithSalt = genericHashWithSalt

instance Hashable TreeBranch where
  hashWithSalt = genericHashWithSalt

instance Hashable Group where
  hashWithSalt = genericHashWithSalt

instance Hashable PreserveAspectRatio where
  hashWithSalt = genericHashWithSalt

instance Hashable Alignment where
  hashWithSalt = genericHashWithSalt

instance Hashable MeetSlice where
  hashWithSalt = genericHashWithSalt

instance Hashable LinearGradient where
  hashWithSalt = genericHashWithSalt

instance Hashable Spread where
  hashWithSalt = genericHashWithSalt

instance Hashable Transformation where
  hashWithSalt = genericHashWithSalt

instance Hashable GradientStop where
  hashWithSalt = genericHashWithSalt

instance Hashable GradientPathCommand where
  hashWithSalt = genericHashWithSalt

instance Hashable Origin where
  hashWithSalt = genericHashWithSalt

instance Hashable Use where
  hashWithSalt = genericHashWithSalt

instance Hashable Filter where
  hashWithSalt = genericHashWithSalt

instance Hashable FilterAttributes where
  hashWithSalt = genericHashWithSalt

instance Hashable FilterElement where
  hashWithSalt = genericHashWithSalt

instance Hashable Blend where
  hashWithSalt = genericHashWithSalt

instance Hashable BlendMode where
  hashWithSalt = genericHashWithSalt

instance Hashable ConvolveMatrix where
  hashWithSalt = genericHashWithSalt

instance Hashable Morphology where
  hashWithSalt = genericHashWithSalt

instance Hashable OperatorType where
  hashWithSalt = genericHashWithSalt

instance Hashable NumberOptionalNumber where
  hashWithSalt = genericHashWithSalt

instance Hashable SpecularLighting where
  hashWithSalt = genericHashWithSalt

instance Hashable DropShadow where
  hashWithSalt = genericHashWithSalt

instance Hashable DiffuseLighting where
  hashWithSalt = genericHashWithSalt

instance Hashable Flood where
  hashWithSalt = genericHashWithSalt

instance Hashable Tile where
  hashWithSalt = genericHashWithSalt

instance Hashable Offset where
  hashWithSalt = genericHashWithSalt

instance Hashable Merge where
  hashWithSalt = genericHashWithSalt

instance Hashable MergeNode where
  hashWithSalt = genericHashWithSalt

instance Hashable ImageF where
  hashWithSalt = genericHashWithSalt

instance Hashable ComponentTransfer where
  hashWithSalt = genericHashWithSalt

instance Hashable FuncType where
  hashWithSalt = genericHashWithSalt

instance Hashable FuncA where
  hashWithSalt = genericHashWithSalt

instance Hashable FuncR where
  hashWithSalt = genericHashWithSalt

instance Hashable FuncG where
  hashWithSalt = genericHashWithSalt

instance Hashable FuncB where
  hashWithSalt = genericHashWithSalt

instance Hashable ColorMatrix where
  hashWithSalt = genericHashWithSalt

instance Hashable FilterSource where
  hashWithSalt = genericHashWithSalt

instance Hashable ColorMatrixType where
  hashWithSalt = genericHashWithSalt

instance Hashable Composite where
  hashWithSalt = genericHashWithSalt

instance Hashable CompositeOperator where
  hashWithSalt = genericHashWithSalt

instance Hashable DisplacementMap where
  hashWithSalt = genericHashWithSalt

instance Hashable ChannelSelector where
  hashWithSalt = genericHashWithSalt

instance Hashable GaussianBlur where
  hashWithSalt = genericHashWithSalt

instance Hashable EdgeMode where
  hashWithSalt = genericHashWithSalt

instance Hashable Turbulence where
  hashWithSalt = genericHashWithSalt

instance Hashable StitchTiles where
  hashWithSalt = genericHashWithSalt

instance Hashable TurbulenceType where
  hashWithSalt = genericHashWithSalt

instance Hashable Path where
  hashWithSalt = genericHashWithSalt

instance Hashable PathCommand where
  hashWithSalt = genericHashWithSalt

instance Hashable Circle where
  hashWithSalt = genericHashWithSalt

instance Hashable PolyLine where
  hashWithSalt = genericHashWithSalt

instance Hashable Polygon where
  hashWithSalt = genericHashWithSalt

instance Hashable Ellipse where
  hashWithSalt = genericHashWithSalt

instance Hashable Line where
  hashWithSalt = genericHashWithSalt

instance Hashable Rectangle where
  hashWithSalt = genericHashWithSalt

instance Hashable TextPath where
  hashWithSalt = genericHashWithSalt

instance Hashable TextPathMethod where
  hashWithSalt = genericHashWithSalt

instance Hashable TextPathSpacing where
  hashWithSalt = genericHashWithSalt

instance Hashable Text where
  hashWithSalt = genericHashWithSalt

instance Hashable TextAdjust where
  hashWithSalt = genericHashWithSalt

instance Hashable TextSpan where
  hashWithSalt = genericHashWithSalt

instance Hashable TextInfo where
  hashWithSalt = genericHashWithSalt

instance Hashable TextSpanContent where
  hashWithSalt = genericHashWithSalt

instance Hashable Image where
  hashWithSalt = genericHashWithSalt

instance Hashable RadialGradient where
  hashWithSalt = genericHashWithSalt

instance Hashable MeshGradient where
  hashWithSalt = genericHashWithSalt

instance Hashable MeshGradientType where
  hashWithSalt = genericHashWithSalt

instance Hashable MeshGradientRow where
  hashWithSalt = genericHashWithSalt

instance Hashable MeshGradientPatch where
  hashWithSalt = genericHashWithSalt

instance Hashable Marker where
  hashWithSalt = genericHashWithSalt

instance Hashable MarkerOrientation where
  hashWithSalt = genericHashWithSalt

instance Hashable MarkerUnit where
  hashWithSalt = genericHashWithSalt

instance Hashable Overflow where
  hashWithSalt = genericHashWithSalt

instance Hashable Document where
  hashWithSalt = genericHashWithSalt

instance Hashable Texture where
  hashWithSalt = genericHashWithSalt

instance Hashable Cap where
  hashWithSalt = genericHashWithSalt

instance Hashable LineJoin where
  hashWithSalt = genericHashWithSalt

instance Hashable FillRule where
  hashWithSalt = genericHashWithSalt

instance Hashable ElementRef where
  hashWithSalt = genericHashWithSalt

instance Hashable FontStyle where
  hashWithSalt = genericHashWithSalt

instance Hashable TextAnchor where
  hashWithSalt = genericHashWithSalt

instance Hashable Tree where
  hashWithSalt s = hashWithSalt s . _treeHash

treeBranch :: Lens' Tree TreeBranch
treeBranch = lens _treeBranch $ const Tree

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
