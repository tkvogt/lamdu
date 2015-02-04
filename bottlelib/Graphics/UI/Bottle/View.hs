{-# LANGUAGE RecordWildCards, RankNTypes, OverloadedStrings #-}
module Graphics.UI.Bottle.View
  ( View(..)
  , empty
  , size, animFrame
  , width, height
  , Size
  , augmentAnimId
  , backgroundColor, backgroundColorShadowed
  , scale
  ) where

import           Control.Lens (Lens')
import           Control.Lens.Operators
import           Control.Lens.Tuple
import qualified Data.ByteString.Char8 as SBS8
import           Data.Monoid (Monoid(..))
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, Layer)
import qualified Graphics.UI.Bottle.Animation as Anim

type Size = Anim.Size

data View = View
    { _size :: Size
    , _animFrame :: Anim.Frame
    }

empty :: View
empty = View 0 mempty

size :: Lens' View Size
size f View{..} = f _size <&> \_size -> View{..}

animFrame :: Lens' View Anim.Frame
animFrame f View{..} = f _animFrame <&> \_animFrame -> View{..}

width :: Lens' View Anim.R
width = size . _1

height :: Lens' View Anim.R
height = size . _2

augmentAnimId :: Show a => AnimId -> a -> AnimId
augmentAnimId animId = Anim.joinId animId . (:[]) . SBS8.pack . show

backgroundColor :: AnimId -> Layer -> Draw.Color -> View -> View
backgroundColor animId layer color view =
    view
    & animFrame %~ Anim.backgroundColor bgAnimId layer color (view ^. size)
    where
        bgAnimId = animId ++ ["bg"]

backgroundColorShadowed :: AnimId -> Layer -> Draw.Color -> View -> View
backgroundColorShadowed animId layer color view =
    view
    & animFrame <>~
      Anim.coloredSquare (bgAnimId "front") layer color (view ^. size - 3)
    & animFrame <>~
      ( Anim.coloredSquare (bgAnimId "back") (layer+1) (halfColor color)
        (view ^. size - 3)
        & Anim.translate 3
      )
    where
        halfColor (Draw.Color r g b a) = Draw.Color (r/2) (g/2) (b/2) a
        bgAnimId x = animId ++ ["bg", x]

scale :: Vector2 Draw.R -> View -> View
scale ratio (View sz frm) =
    View (sz*ratio) (Anim.scale ratio frm)
