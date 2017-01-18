{-# LANGUAGE OverloadedStrings #-}
module Colorscheme (processImage) where

import Codec.Picture
import Data.Colour.CIE (cieLABView, cieLAB)
import Data.Colour.SRGB (Colour, RGB(..), sRGB24, toSRGB24)
import Data.Ord (comparing)
import qualified Data.Colour.CIE.Illuminant
import qualified Data.List as L
import qualified Palette

processImage :: FilePath -> IO ()
processImage path = do
  wheel <- loadImage "wheel.png"
  image <- loadImage path
  
  let convertJCtoLAB = convertCCtoLAB . convertJCtoCC
  let convertLABtoJC = convertCCtoJC . convertLABtoCC
  
  let paletteJC = Palette.fromImage wheel
  let palette = fmap convertJCtoLAB paletteJC

  let distanceF = colorDistance convertJCtoLAB id
  let searchF = nearestPaletteColor convertLABtoJC distanceF
  let paletteSearch = searchF palette
  let result = mapDynamicImage paletteSearch image
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

cie76 :: LABcolor -> LABcolor -> Double
cie76 (l1, a1, b1) (l2, a2, b2) = sqrt $ sq (l2 - l1) + sq (a2 - a1) + sq (b2 - b1)
  where
    sq x =  x ^ (2::Int)

convertCCtoLAB :: Ccolor -> LABcolor
convertCCtoLAB = cieLABView Data.Colour.CIE.Illuminant.d65

convertLABtoCC :: LABcolor -> Ccolor
convertLABtoCC (l, a, b) = cieLAB Data.Colour.CIE.Illuminant.d65 l a b

convertJCtoCC :: JCcolor -> Ccolor
convertJCtoCC (PixelRGB8 r g b) = sRGB24 r g b

convertCCtoJC :: Ccolor -> JCcolor
convertCCtoJC c = PixelRGB8 r g b
  where
    (RGB r g b) = toSRGB24 c

mapDynamicImage :: (JCcolor -> JCcolor) -> DynamicImage -> DynamicImage
mapDynamicImage paletteSearch (ImageRGB8 image) = ImageRGB8 $ pixelMap paletteSearch image
mapDynamicImage _ _ = error "Unsupported image type" -- should not match due to enforceRGB8

nearestPaletteColor :: (a -> JCcolor) -> (JCcolor -> a -> Double) -> [a] -> JCcolor -> JCcolor
nearestPaletteColor conversionToJC distanceF palette c = conversionToJC pixel
    where
      pixel = snd $ L.minimumBy (comparing fst) distances
      distances = map (\x -> (distanceF c x, x)) palette

colorDistance :: (a -> LABcolor)-> (b -> LABcolor) -> a -> b -> Double
colorDistance convX convY x y = cie76 (convX x) (convY y)
