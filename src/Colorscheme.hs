{-# LANGUAGE OverloadedStrings #-}
module Colorscheme (
  processImage
) where

import qualified Data.ByteString as B
import Codec.Picture
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V
import Data.Colour.SRGB
import Data.Colour.CIE
import qualified Data.Colour.CIE.Illuminant
import Data.Word (Word8)
import qualified Data.List as L
import Data.Ord (comparing)

type ColorWheelPalette = [PixelRGB8]

processImage :: FilePath -> IO ()
processImage path = do
  wheel <- loadImage "wheel.png"
  -- let palette = preparePalette wheel
  palette <- preparePalette' wheel
  image <- loadImage path
  -- print $ nearestPaletteColor palette (PixelRGB8 0 42 0)
  let result = mapDynamicImage palette image
  savePngImage "out.png" result
  putStrLn "done"

loadImage :: FilePath -> IO DynamicImage
loadImage path = do
  eimg <- readImage path
  case eimg of
    Left err -> error ("Could not read image: " ++ err)
    Right img -> return img

preparePalette' :: DynamicImage -> IO ColorWheelPalette
preparePalette' (ImageRGB8 image@(Image w h _)) = do
  let mainColors = L.filter (\(_, x) -> (x > 3000) && (x < 30000)) colorsWithCounts
  let palette = (map (head . fst) mainColors) ++ [PixelRGB8 0 0 0, PixelRGB8 127 127 127, PixelRGB8 255 255 255] -- adding black, grey, and grey
  print palette
  return palette
  where
    colorsWithCounts = map (\x->([head x], length x)) . L.group . L.sort $ allPixels
    allPixels = map (\(x, y) -> pixelAt image x y) allCoords :: ColorWheelPalette
    allCoords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]

preparePalette :: DynamicImage -> ColorWheelPalette
preparePalette (ImageRGB8 image@(Image w h _)) =
  Set.toList $ Set.fromList allPixels
    where
      allPixels = map (\(x, y) -> pixelAt image x y) allCoords
      allCoords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]

preparePalette _ = error "Unsupported image format"

colorDistance :: PixelRGB8 -> PixelRGB8 -> Double
colorDistance a b = cie76 (convertCCtoJC a) (convertCCtoJC b)

cie76 :: Colour Double -> Colour Double -> Double
cie76 c1 c2 = sqrt $ (l2 - l1)^2 + (a2 - a1)^2 + (b2 - b1)^2
  where
    (l1, a1, b1) = toLAB c1
    (l2, a2, b2) = toLAB c2

toLAB :: Colour Double -> (Double, Double, Double)
toLAB = cieLABView Data.Colour.CIE.Illuminant.d65

convertCCtoJC :: PixelRGB8 -> Colour Double
convertCCtoJC pixel@(PixelRGB8 r g b) = sRGB24 r g b

mapDynamicImage :: ColorWheelPalette -> DynamicImage -> DynamicImage
mapDynamicImage palette source = ImageRGB8 $ pixelMap (nearestPaletteColor palette) rgb8image
  where
    rgb8image = case source of
      (ImageRGB8 image) -> image
      otherImage        -> convertRGB8 otherImage

nearestPaletteColor :: ColorWheelPalette -> PixelRGB8 -> PixelRGB8
nearestPaletteColor palette c = snd $ L.minimumBy (comparing fst) distances
  where
    distances = map (\p -> (colorDistance c p, p)) palette