module LambdaCube.GL.Data where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List as L
import Data.Maybe
import Foreign 
--import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

--import Control.DeepSeq

import Graphics.GL.Core33
import Data.Word
import Codec.Picture
import Codec.Picture.Types

import LambdaCube.GL.Type
import LambdaCube.GL.Util

-- Buffer
disposeBuffer :: Buffer -> IO ()
disposeBuffer (Buffer _ bo) = withArray [bo] $ glDeleteBuffers 1

compileBuffer :: [Array] -> IO Buffer
compileBuffer arrs = do
    let calcDesc (offset,setters,descs) (Array arrType cnt setter) =
          let size = cnt * sizeOfArrayType arrType
          in (size + offset, (offset,size,setter):setters, ArrayDesc arrType cnt offset size:descs)
        (bufSize,arrSetters,arrDescs) = foldl' calcDesc (0,[],[]) arrs
    bo <- alloca $! \pbo -> glGenBuffers 1 pbo >> peek pbo
    glBindBuffer GL_ARRAY_BUFFER bo
    glBufferData GL_ARRAY_BUFFER (fromIntegral bufSize) nullPtr GL_STATIC_DRAW
    forM_ arrSetters $! \(offset,size,setter) -> setter $! glBufferSubData GL_ARRAY_BUFFER (fromIntegral offset) (fromIntegral size)
    glBindBuffer GL_ARRAY_BUFFER 0
    return $! Buffer (V.fromList $! reverse arrDescs) bo

updateBuffer :: Buffer -> [(Int,Array)] -> IO ()
updateBuffer (Buffer arrDescs bo) arrs = do
    glBindBuffer GL_ARRAY_BUFFER bo
    forM arrs $ \(i,Array arrType cnt setter) -> do
        let ArrayDesc ty len offset size = arrDescs V.! i
        when (ty == arrType && cnt == len) $
            setter $! glBufferSubData GL_ARRAY_BUFFER (fromIntegral offset) (fromIntegral size)
    glBindBuffer GL_ARRAY_BUFFER 0

bufferSize :: Buffer -> Int
bufferSize = V.length . bufArrays

arraySize :: Buffer -> Int -> Int
arraySize buf arrIdx = arrLength $! bufArrays buf V.! arrIdx

arrayType :: Buffer -> Int -> ArrayType
arrayType buf arrIdx = arrType $! bufArrays buf V.! arrIdx

-- Texture
disposeTexture :: TextureData -> IO ()
disposeTexture (TextureData to) = withArray [to] $ glDeleteTextures 1

-- FIXME: Temporary implemenation
uploadTexture2DToGPU :: DynamicImage -> IO TextureData
uploadTexture2DToGPU = uploadTexture2DToGPU' False True False

uploadTexture2DToGPU' :: Bool -> Bool -> Bool -> DynamicImage -> IO TextureData
uploadTexture2DToGPU' isSRGB isMip isClamped bitmap' = do
    let bitmap = case bitmap' of
          ImageRGB8 i@(Image w h _)   -> bitmap' -- pixelFoldMap (\(PixelRGB8 r g b) -> [PixelRGBA8 r g b maxBound]) i
          ImageRGBA8 i@(Image w h _)  -> bitmap' -- pixelFoldMap (\(PixelRGBA8 r g b a) -> [PixelRGBA8 r g b a]) i
          ImageYCbCr8 i@(Image w h _) -> ImageRGB8 $ convertImage i -- $ Image w h $ SV.fromList $ pixelFoldMap (\p -> let PixelRGB8 r g b = convertPixel p in [PixelRGBA8 r g b maxBound]) i
          ImageCMYK16 _   -> error "uploadTexture2DToGPU: ImageCMYK16"
          ImageCMYK8 _    -> error "uploadTexture2DToGPU: ImageCMYK8"
          ImageRGBA16 _   -> error "uploadTexture2DToGPU: ImageRGBA16"
          ImageRGBF _     -> error "uploadTexture2DToGPU: ImageRGBF"
          ImageRGB16 _    -> error "uploadTexture2DToGPU: ImageRGB16"
          ImageYA16 _     -> error "uploadTexture2DToGPU: ImageYA16"
          ImageYA8 _      -> error "uploadTexture2DToGPU: ImageYA8"
          ImageYF _       -> error "uploadTexture2DToGPU: ImageYF"
          ImageY16 _      -> error "uploadTexture2DToGPU: ImageY16"
          ImageY8 _       -> error "uploadTexture2DToGPU: ImageY8"
          _ -> error "uploadTexture2DToGPU: unknown image"

    glPixelStorei GL_UNPACK_ALIGNMENT 1
    to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
    glBindTexture GL_TEXTURE_2D to
    let (width,height) = bitmapSize bitmap
        bitmapSize (ImageRGB8 (Image w h _)) = (w,h)
        bitmapSize (ImageRGBA8 (Image w h _)) = (w,h)
        bitmapSize _ = error "unsupported image type :("
        withBitmap (ImageRGB8 (Image w h v)) f = SV.unsafeWith v $ f (w,h) 3 0
        withBitmap (ImageRGBA8 (Image w h v)) f = SV.unsafeWith v $ f (w,h) 4 0
        withBitmap _ _ = error "unsupported image type :("
        wrapMode = case isClamped of
            True    -> GL_CLAMP_TO_EDGE
            False   -> GL_REPEAT
        (minFilter,maxLevel) = case isMip of
            False   -> (GL_LINEAR,0)
            True    -> (GL_LINEAR_MIPMAP_LINEAR, floor $ log (fromIntegral $ max width height) / log 2)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S $ fromIntegral wrapMode
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T $ fromIntegral wrapMode
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $ fromIntegral minFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $ fromIntegral GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL $ fromIntegral maxLevel
    withBitmap bitmap $ \(w,h) nchn 0 ptr -> do
        let internalFormat  = fromIntegral $ if isSRGB then (if nchn == 3 then GL_SRGB8 else GL_SRGB8_ALPHA8) else (if nchn == 3 then GL_RGB8 else GL_RGBA8)
            dataFormat      = fromIntegral $ case nchn of
                3   -> GL_RGB
                4   -> GL_RGBA
                _   -> error "unsupported texture format!"
        glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat GL_UNSIGNED_BYTE $ castPtr ptr
    when isMip $ glGenerateMipmap GL_TEXTURE_2D
    return $ TextureData to
