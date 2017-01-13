{-# LANGUAGE OverloadedStrings #-}
module Colorscheme (processImage) where

import Codec.Picture
import Data.Colour.CIE
import Data.Colour.SRGB
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.Colour.CIE.Illuminant
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V
import qualified Palette

processImage :: FilePath -> IO ()
processImage path = do
  wheel <- loadImage "wheel.png"
  let palette = Palette.fromImage wheel
  print palette
  image <- loadImage path
  let palette' = fmap convertJCtoCC palette
  print palette'
  let result = mapDynamicImage' palette' image
  -- let result = mapDynamicImage palette image
  savePngImage "out.png" result
  putStrLn "done"

loadImage :: FilePath -> IO DynamicImage
loadImage path = do
  eimg <- readImage path
  case eimg of
    Left err -> error ("Could not read image: " ++ err)
    Right img -> return . ImageRGB8 $ enforceRGB8 img

-- forces DynamicImage to ImageRGB8
enforceRGB8 :: DynamicImage -> Image PixelRGB8
enforceRGB8 (ImageRGB8 image) = image
enforceRGB8 otherFormat       = convertRGB8 otherFormat

colorDistance :: PixelRGB8 -> PixelRGB8 -> Double
colorDistance a b = cie76 (convertJCtoCC a) (convertJCtoCC b)

type CColor = Colour Double

cie76 :: CColor -> CColor -> Double
cie76 c1 c2 = sqrt $ (l2 - l1)^2 + (a2 - a1)^2 + (b2 - b1)^2
  where
    (l1, a1, b1) = toLAB c1
    (l2, a2, b2) = toLAB c2

toLAB :: CColor -> (Double, Double, Double)
toLAB = cieLABView Data.Colour.CIE.Illuminant.d65

convertJCtoCC :: PixelRGB8 -> CColor
convertJCtoCC pixel@(PixelRGB8 r g b) = sRGB24 r g b

mapDynamicImage :: Palette.Colors -> DynamicImage -> DynamicImage
mapDynamicImage palette source = ImageRGB8 $ pixelMap (nearestPaletteColor palette) rgb8image
  where
    rgb8image = case source of
      (ImageRGB8 image) -> image
      otherImage        -> convertRGB8 otherImage

nearestPaletteColor :: Palette.Colors -> PixelRGB8 -> PixelRGB8
nearestPaletteColor palette c = snd $ L.minimumBy (comparing fst) distances
  where
    distances = map (\p -> (colorDistance c p, p)) palette


-- Attempt #2. Precalculating palette in CColor

convertCCtoJC :: CColor -> PixelRGB8
convertCCtoJC c = PixelRGB8 r g b
  where
    (RGB r g b) = toSRGB24 c

mapDynamicImage' :: [CColor] -> DynamicImage -> DynamicImage
mapDynamicImage' palette source = ImageRGB8 $ pixelMap (nearestPaletteColor' palette) rgb8image
  where
    rgb8image = case source of
      (ImageRGB8 image) -> image
      otherImage        -> convertRGB8 otherImage

nearestPaletteColor' :: [CColor] -> PixelRGB8 -> PixelRGB8
nearestPaletteColor' palette c = convertCCtoJC . snd $ L.minimumBy (comparing fst) distances
  where
    distances = map (\p -> (colorDistance' c p, p)) palette

colorDistance' :: PixelRGB8 -> CColor-> Double
colorDistance' p c = cie76 (convertJCtoCC p) c
