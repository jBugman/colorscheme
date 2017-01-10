#!/usr/bin/env stack
-- stack --install-ghc runghc --package JuicyPixels --package colour

{-# LANGUAGE OverloadedStrings #-}

import System.Environment
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


main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> processImage path
    _ -> putStrLn "Please provide a path"

processImage :: FilePath -> IO ()
processImage path = do
  wheel <- loadImage "wheel.png"
  let palette = preparePalette wheel
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

type ColorWheelPalette = [PixelRGB8]

-- preparePalette' :: Image PixelRGB8 -> Int -> Int -> [PixelRGB8]
preparePalette :: DynamicImage -> ColorWheelPalette
preparePalette (ImageRGB8 image@(Image w h _)) =
  Set.toList $ Set.fromList allPixels
    where
      allCoords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
      allPixels = map (\(x, y) -> pixelAt image x y) allCoords

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
mapDynamicImage palette source = ImageRGB8 $ pixelMap (nearestPaletteColor $ take 3 palette) rgb8image
  where
    rgb8image = case source of
      (ImageRGB8 image) -> image
      otherImage        -> convertRGB8 otherImage

nearestPaletteColor :: ColorWheelPalette -> PixelRGB8 -> PixelRGB8
nearestPaletteColor palette c = snd $ L.minimumBy (comparing fst) distances
  where
    distances = map (\p -> (colorDistance c p, p)) palette
