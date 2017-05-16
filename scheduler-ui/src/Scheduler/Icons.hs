{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Scheduler.Icons where

import ClassyPrelude
import Diagrams
import Diagrams.Backend.SVG
import Graphics.Svg.Core
import Data.Colour.Names
import qualified Data.ByteString.Lazy as BL
import Servant
import Network.HTTP.Media ((//), (/:))

arrUp :: Diagram B
arrUp = (((triangle 0.6 # translateY 0.5 # scaleY 1.1) === (rect 0.25 0.5)) # fc black # lw none) <> square 1 # lw none

arrDown :: Diagram B
arrDown = arrUp # rotate (180 @@ deg)

removeIcon :: Diagram B
removeIcon = d # fc red # lw none
  where
    d = (r # rotate (45 @@ deg)) <> (r # rotate (-45 @@ deg))
    r = rect 0.25 1

renderIcon :: Diagram B -> BL.ByteString
renderIcon d = renderBS $ renderDia SVG (SVGOptions (mkWidth 12.5) Nothing "" [] True) d

data SvgDiagram
newtype SvgDiagram' = SvgDiagram' {unSvgDiagram' :: Diagram B}

instance Accept SvgDiagram where
  contentType _ = "image" // "svg+xml"

instance MimeRender SvgDiagram SvgDiagram' where
  mimeRender _ = renderIcon . unSvgDiagram'
