{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Plot where

import ClassyPrelude as CP
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time hiding (readTime)
import Data.Colour.SRGB

readTime :: TimeZone -> (String, String) -> Either String (LocalTime, Double)
readTime tz (t, v) = do
  time <- parseTimeM True defaultTimeLocale "%e %b %Y %X %Z" t
  case readMay v of
    Nothing -> Left $ "Could not read " <> show v
    Just dbl -> Right $ (utcToLocalTime tz time, dbl)

valLines :: TimeZone -> [(String, String)] -> Either String (PlotLines LocalTime Double)
valLines tz vals = do
  times <- mapM (readTime tz) vals
  return $ plot_lines_values .~ [times] $ def

layout :: TimeZone -> [(String, String)] -> Either String (Layout LocalTime Double)
layout tz vals = do
  ls <- valLines tz vals
  return
    $ layout_plots .~ [toPlot ls]
    $ def

indexIt :: Int -> [a] -> Maybe (a, a)
indexIt idx l = do
  time <- CP.index l 0
  val <- CP.index l idx
  return (time, val)

plotIt :: (PlotValue x0, PlotValue y0) => [([(x0, y0)], String)] -> Layout x0 y0
plotIt vals = layout_plots .~ fmap (toPlot . toLines) valColours $ def
  where
    toLines ((v, label), colour) =
        plot_lines_values .~ [v]
      $ plot_lines_style . line_color .~ opaque colour
      $ plot_lines_title .~ label
      $ def
    valColours = zip vals $ colours ^.. cycled traverse

-- colours = [dodgerblue, burlywood, slategray, yellowgreen, firebrick, purple, palevioletred, mediumaquamarine]

colours = [ sRGB 0.27450980392156865 0.5254901960784314 0.7686274509803922
         , sRGB 0.7803921568627451 0.30196078431372547 0.26666666666666666
         , sRGB 0.6196078431372549 0.7372549019607844 0.3254901960784314
         , sRGB 0.5686274509803921 0.4666666666666667 0.7019607843137254
         , sRGB 0.22745098039215686 0.6901960784313725 0.8
         , sRGB 0.9647058823529412 0.5843137254901961 0.18823529411764706
         , sRGB 0.615686274509804 0.7058823529411765 0.8352941176470589
         , sRGB24read "4D4D4D"
         ]

