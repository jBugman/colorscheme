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
  image <- loadImage path
  let palette = Palette.fromImage wheel
  -- print palette
  let palette' = fmap convertJCtoCC palette
  -- print palette'
  let colorDistance' = colorDistance convertJCtoCC id
  let nearestPaletteColor' = nearestPaletteColor convertCCtoJC colorDistance'
  let result = mapDynamicImage nearestPaletteColor' palette' image
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

type Ccolor = Colour Double
type JCcolor = PixelRGB8
type LABcolor = (Double, Double, Double)

cie76 :: Ccolor -> Ccolor -> Double
cie76 c1 c2 = sqrt $ (l2 - l1)^2 + (a2 - a1)^2 + (b2 - b1)^2
  where
    (l1, a1, b1) = toLAB c1
    (l2, a2, b2) = toLAB c2

toLAB :: Ccolor -> LABcolor
toLAB = cieLABView Data.Colour.CIE.Illuminant.d65

convertJCtoCC :: JCcolor -> Ccolor
convertJCtoCC pixel@(PixelRGB8 r g b) = sRGB24 r g b

convertCCtoJC :: Ccolor -> JCcolor
convertCCtoJC c = PixelRGB8 r g b
  where
    (RGB r g b) = toSRGB24 c

mapDynamicImage :: ([p] -> JCcolor -> JCcolor) -> [p] -> DynamicImage -> DynamicImage
mapDynamicImage paletteSearch palette (ImageRGB8 image) = ImageRGB8 $ pixelMap (paletteSearch palette) image

nearestPaletteColor :: (p -> JCcolor) -> (JCcolor -> p -> Double) -> [p] -> JCcolor -> JCcolor
nearestPaletteColor conversionToJC distanceF palette c = conversionToJC pixel
    where
      pixel = snd $ L.minimumBy (comparing fst) distances
      distances = map (\x -> (distanceF c x, x)) palette

colorDistance :: (a -> Ccolor)-> (b -> Ccolor) -> a -> b -> Double
colorDistance convX convY x y = cie76 (convX x) (convY y)
