module Palette (Colors, fromImage, extended) where

import Codec.Picture
import qualified Data.List as L


type Colors = [PixelRGB8]

rgb :: Pixel8 -> Pixel8 -> Pixel8 -> PixelRGB8
rgb = PixelRGB8

monochrome :: Colors
monochrome = [rgb 0 0 0, rgb 64 64 64, rgb 127 127 127, rgb 190 190 190, rgb 255 255 255]

extended :: Colors
extended = monochrome
  -- green
  ++ [rgb 194 229 160, rgb 171 218 136, rgb 128 194 65, rgb 73 159 0, rgb 0 116 24, rgb 2 92 27]
  -- green-blue
  ++ [rgb 138 229 220, rgb 37 200 174, rgb 4 185 164, rgb 0 135 106, rgb 0 88 94, rgb 0 61 73]
  -- blue
  ++ [rgb 169 219 255, rgb 105 189 248, rgb 0 159 232, rgb 0 92 187, rgb 0 63 120, rgb 0 32 88]
  -- purple-blue
  ++ [rgb 188 200 239, rgb 132 158 216, rgb 86 110 181, rgb 48 84 159, rgb 11 55 131, rgb 0 11 75]
  -- purple
  ++ [rgb 221 200 237, rgb 202 174 222, rgb 202 174 222, rgb 132 64 140, rgb 89 45 122, rgb 49 25 91]
  -- purple-red
  ++ [rgb 231 194 227, rgb 216 144 189, rgb 200 106 159, rgb 169 65 127, rgb 128 2 73, rgb 61 17 44]
  -- red
  ++ [rgb 248 202 189, rgb 238 167 144, rgb 224 113 85, rgb 189 13 1, rgb 141 2 1, rgb 82 0 1]
  -- orange-red
  ++ [rgb 245 196 148, rgb 238 154 115, rgb 230 128 75, rgb 207 78 2, rgb 127 51 2, rgb 60 22 1]
  -- orange
  ++ [rgb 251 215 154, rgb 246 185 118, rgb 239 152 73, rgb 224 117 0, rgb 144 83 0, rgb 92 60 5]
  -- orange-yellow
  ++ [rgb 254 227 148, rgb 250 195 82, rgb 245 175 50, rgb 239 152 2, rgb 166 121 0, rgb 116 93 5]
  -- yellow
  ++ [rgb 254 246 172, rgb 253 237 113, rgb 253 227 44, rgb 250 216 2, rgb 188 170 1, rgb 118 114 17]
  -- green-yellow
  ++ [rgb 219 225 90, rgb 193 210 2, rgb 167 194 0, rgb 149 184 0, rgb 102 140 0, rgb 85 103 3]

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
