{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module RayTracer.App (
    start
) where

import qualified GI.Gtk as Gtk
import GI.Gtk.Enums (WindowType(..))
import GI.Cairo
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Graphics.Rendering.Cairo.Internal (Render(..))
import Graphics.Rendering.Cairo
import Foreign.Ptr (Ptr, castPtr)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad
import System.Random(randomIO)
import Control.Parallel.Strategies
import Codec.BMP

import Data.Word(Word8(..))
import qualified Data.ByteString as ByteString
import qualified Data.List as List

import RayTracer.Scene
import RayTracer.SceneParser
import RayTracer.Vector

failOrReturn :: Monad m => Show a => m (Either a b) -> m b
failOrReturn = flip (>>=) aux
    where aux (Right r) = return r
          aux (Left l) = fail . show $ l

randomPair :: IO (Scalar, Scalar)
randomPair = randomIO >>= \a -> randomIO >>= \b -> return (a, b)

vectorToRGBA :: Vector -> [Word8]
vectorToRGBA (Vector r g b) = [
                                floor $ r * 255.0,
                                floor $ g * 255.0,
                                floor $ b * 255.0,
                                255
                              ]

start :: String -> IO ()
start filename = do
    Gtk.init Nothing

    scene <- failOrReturn $ loadSceneFromFile filename
    let (width, height) = getDimensions scene
    window <- Gtk.windowNew WindowTypeToplevel
    Gtk.onWidgetDestroy window Gtk.mainQuit

    Gtk.windowSetDefaultSize window (fromIntegral width) (fromIntegral height)
    Gtk.windowSetResizable window False

    canvas <- Gtk.drawingAreaNew
    Gtk.setWidgetWidthRequest canvas (fromIntegral width)
    Gtk.setWidgetHeightRequest canvas (fromIntegral height)
    Gtk.containerAdd window canvas

    let sampleNum = 16
    samplesDeviation <- mapM (\_ -> randomPair) [1..sampleNum]

    let pixels = concat $ flip (parMap rdeepseq) [0..height-1] $ \y ->
                            flip map [0..width-1] $ \x ->
                                (<**>) (1.0/(fromIntegral sampleNum)) $ foldl (<+>) (Vector 0.0 0.0 0.0) $ map (\(dx, dy) -> renderPixel scene (dx + fromIntegral x) (dy + fromIntegral y)) samplesDeviation

    let rgba = ByteString.pack $ List.concat $ map vectorToRGBA (List.reverse pixels)
    let bmp = packRGBA32ToBMP24 width height rgba
    writeBMP "rendered.bmp" bmp

    Gtk.onWidgetDraw canvas $ \(Context fp) -> withManagedPtr fp $ \p -> (`runReaderT` Cairo (castPtr p)) $ runRender $ do
        forM_ (zip [0..] pixels) $ \(i, (Vector r g b)) -> do
            let x = i `mod` width
            let y = i `div` width
            setSourceRGB r g b
            rectangle (fromIntegral x) (fromIntegral y) 1.0 1.0
            fill

        return True

    Gtk.widgetShowAll window
    Gtk.main

