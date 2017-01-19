module Palette (Colors, fromImage) where

import Codec.Picture
import qualified Data.List as L


type Colors = [PixelRGB8]

rgb :: Pixel8 -> Pixel8 -> Pixel8 -> PixelRGB8
rgb = PixelRGB8

-- white, gray and black colors 
monochrome :: Colors
monochrome = [rgb 0 0 0, rgb 127 127 127, rgb 255 255 255]

extended :: Colors
extended = monochrome
  -- green
  ++ [rgb 194 229 160, rgb 171 218 136, rgb 128 194 65, rgb 73 159 0, rgb 0 116 24, rgb 2 92 27]
  -- blue-green
  ++ [rgb 138 229 220, rgb 37 200 174, rgb 4 185 164, rgb 0 135 106, rgb 0 88 94, rgb 0 61 73]
  -- blue
  ++ [rgb 169 219 255, rgb 105 189 248, rgb 0 159 232, rgb 0 92 187, rgb 0 63 120, rgb 0 32 88]
  -- ...
  -- TODO: finish

fromImage :: DynamicImage -> Colors
fromImage (ImageRGB8 image@(Image w h _)) = map (head . fst) mainColors ++ monochrome
  where
    mainColors = L.filter hardcodedFrequencies colorsWithCounts
    hardcodedFrequencies (_, x) = (x > 3000) && (x < 30000) -- for wheel.png
    colorsWithCounts = countItems allPixels
    allPixels = map (uncurry (pixelAt image)) allCoords
    allCoords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
fromImage _ = error "Unsupported image type"


countItems :: (Ord a) => [a] -> [([a], Int)]
countItems = fmap counter . L.group . L.sort
  where
    counter x = ([head x], length x)
