{-# LANGUAGE TupleSections, RecordWildCards #-}
module LambdaCube.GL.Mesh (
    addMeshToObjectArray,
    uploadMeshToGPU,
    disposeMesh,
    updateMesh,
    Mesh(..),
    MeshPrimitive(..),
    MeshAttribute(..),
    GPUMesh,
    meshData
) where

import Data.Maybe
import Control.Applicative
import Control.Monad
import Foreign.Ptr
import Data.Int
import Foreign.Storable
import Foreign.Marshal.Utils
import System.IO.Unsafe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB

import LambdaCube.GL
import LambdaCube.GL.Type as T
import LambdaCube.IR as IR
import LambdaCube.Linear as IR
import LambdaCube.Mesh

data GPUData
    = GPUData
    { dPrimitive    :: Primitive
    , dStreams      :: Map String (Stream Buffer)
    , dIndices      :: Maybe (IndexStream Buffer)
    , dBuffers      :: [Buffer]
    }

data GPUMesh
  = GPUMesh
  { meshData  :: Mesh
  , gpuData   :: GPUData
  }

addMeshToObjectArray :: GLStorage -> String -> [String] -> GPUMesh -> IO Object
addMeshToObjectArray input slotName objUniNames (GPUMesh _ (GPUData prim streams indices _)) = do
    -- select proper attributes
    let (ObjectArraySchema slotPrim slotStreams) = fromMaybe (error $ "addMeshToObjectArray - missing object array: " ++ slotName) $ Map.lookup slotName $! objectArrays $! schema input
        filterStream n _ = Map.member n slotStreams
    addObject input slotName prim indices (Map.filterWithKey filterStream streams) objUniNames

withV w a f = w a (\p -> f $ castPtr p)

meshAttrToArray :: MeshAttribute -> Array
meshAttrToArray (A_Float v) = Array ArrFloat  (1 *  V.length v) $ withV SV.unsafeWith $ V.convert v
meshAttrToArray (A_V2F   v) = Array ArrFloat  (2 *  V.length v) $ withV SV.unsafeWith $ V.convert v
meshAttrToArray (A_V3F   v) = Array ArrFloat  (3 *  V.length v) $ withV SV.unsafeWith $ V.convert v
meshAttrToArray (A_V4F   v) = Array ArrFloat  (4 *  V.length v) $ withV SV.unsafeWith $ V.convert v
meshAttrToArray (A_M22F  v) = Array ArrFloat  (4 *  V.length v) $ withV SV.unsafeWith $ V.convert v
meshAttrToArray (A_M33F  v) = Array ArrFloat  (9 *  V.length v) $ withV SV.unsafeWith $ V.convert v
meshAttrToArray (A_M44F  v) = Array ArrFloat  (16 * V.length v) $ withV SV.unsafeWith $ V.convert v
meshAttrToArray (A_Int   v) = Array ArrInt32  (1 *  V.length v) $ withV SV.unsafeWith $ V.convert v
meshAttrToArray (A_Word  v) = Array ArrWord32 (1 *  V.length v) $ withV SV.unsafeWith $ V.convert v

meshAttrToStream :: Buffer -> Int -> MeshAttribute -> Stream Buffer
meshAttrToStream b i (A_Float v) = Stream Attribute_Float b i 0 (V.length v)
meshAttrToStream b i (A_V2F   v) = Stream Attribute_V2F b i 0 (V.length v)
meshAttrToStream b i (A_V3F   v) = Stream Attribute_V3F b i 0 (V.length v)
meshAttrToStream b i (A_V4F   v) = Stream Attribute_V4F b i 0 (V.length v)
meshAttrToStream b i (A_M22F  v) = Stream Attribute_M22F b i 0 (V.length v)
meshAttrToStream b i (A_M33F  v) = Stream Attribute_M33F b i 0 (V.length v)
meshAttrToStream b i (A_M44F  v) = Stream Attribute_M44F b i 0 (V.length v)
meshAttrToStream b i (A_Int   v) = Stream Attribute_Int b i 0 (V.length v)
meshAttrToStream b i (A_Word  v) = Stream Attribute_Word b i 0 (V.length v)

updateMesh :: GPUMesh -> [(String,MeshAttribute)] -> Maybe MeshPrimitive -> IO ()
updateMesh (GPUMesh (Mesh dMA dMP) (GPUData _ dS dI _)) al mp = do
  -- check type match
  let arrayChk (Array t1 s1 _) (Array t2 s2 _) = t1 == t2 && s1 == s2
      ok = and [Map.member n dMA && arrayChk (meshAttrToArray a1) (meshAttrToArray a2) | (n,a1) <- al, let a2 = fromMaybe (error $ "missing mesh attribute: " ++ n) $ Map.lookup n dMA]
  if not ok then putStrLn "updateMesh: attribute mismatch!"
    else do
      forM_ al $ \(n,a) -> do
        case Map.lookup n dS of
          Just (Stream _ b i _ _) -> updateBuffer b [(i,meshAttrToArray a)]
          _ -> return ()
{-
      case mp of
        Nothing -> return ()
        Just p -> do
          let ok2 = case (dMP,p) of
                (Just (P_TriangleStripI v1, P_TriangleStripI v2) -> V.length v1 == V.length v2
                (P_TrianglesI v1, P_TrianglesI v2) -> V.length v1 == V.length v2
                (a,b) -> a == b
-}

uploadMeshToGPU :: Mesh -> IO GPUMesh
uploadMeshToGPU mesh@(Mesh attrs mPrim) = do
    let mkIndexBuf v = do
            iBuf <- compileBuffer [Array ArrWord32 (V.length v) $ withV SV.unsafeWith $ V.convert v]
            return $! Just $! IndexStream iBuf 0 0 (V.length v)
    vBuf <- compileBuffer [meshAttrToArray a | a <- Map.elems attrs]
    (indices,prim) <- case mPrim of
        P_Points            -> return (Nothing,PointList)
        P_TriangleStrip     -> return (Nothing,TriangleStrip)
        P_Triangles         -> return (Nothing,TriangleList)
        P_TriangleStripI v  -> (,TriangleStrip) <$> mkIndexBuf v
        P_TrianglesI v      -> (,TriangleList) <$> mkIndexBuf v
    let streams = Map.fromList $! zipWith (\i (n,a) -> (n,meshAttrToStream vBuf i a)) [0..] (Map.toList attrs)
    return $! GPUMesh mesh (GPUData prim streams indices (vBuf:[iBuf | IndexStream iBuf _ _ _ <- maybeToList indices]))

disposeMesh :: GPUMesh -> IO ()
disposeMesh (GPUMesh _ GPUData{..}) = mapM_ disposeBuffer dBuffers
