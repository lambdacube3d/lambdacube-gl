module LambdaCube.GL.Input where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Vector (Vector,(//),(!))
import Data.Word
import Foreign
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB

import Graphics.GL.Core33

import IR as IR
import Linear as IR
import LambdaCube.GL.Type as T
import LambdaCube.GL.Util

import qualified IR as IR

schemaFromPipeline :: IR.Pipeline -> PipelineSchema
schemaFromPipeline a = PipelineSchema (Map.fromList sl) (foldl Map.union Map.empty ul)
  where
    (sl,ul) = unzip [( (sName,ObjectArraySchema sPrimitive (fmap cvt sStreams))
                     , sUniforms
                     )
                    | IR.Slot sName sStreams sUniforms sPrimitive _ <- V.toList $ IR.slots a
                    ]
    cvt a = case toStreamType a of
        Just v  -> v
        Nothing -> error "internal error (schemaFromPipeline)"

mkUniform :: [(String,InputType)] -> IO (Map GLUniformName InputSetter, Map String GLUniform)
mkUniform l = do
    unisAndSetters <- forM l $ \(n,t) -> do
        (uni, setter) <- mkUniformSetter t
        return ((n,uni),(n,setter))
    let (unis,setters) = unzip unisAndSetters
    return (Map.fromList setters, Map.fromList unis)

allocStorage :: PipelineSchema -> IO GLStorage
allocStorage sch = do
    let sm  = Map.fromList $ zip (Map.keys $ objectArrays sch) [0..]
        len = Map.size sm
    (setters,unis) <- mkUniform $ Map.toList $ uniforms sch
    seed <- newIORef 0
    slotV <- V.replicateM len $ newIORef (GLSlot IM.empty V.empty Ordered)
    size <- newIORef (0,0)
    ppls <- newIORef $ V.singleton Nothing
    return $ GLStorage
        { schema        = sch
        , slotMap       = sm
        , slotVector    = slotV
        , objSeed       = seed
        , uniformSetter = setters
        , uniformSetup  = unis
        , screenSize    = size
        , pipelines     = ppls
        }

disposeStorage :: GLStorage -> IO ()
disposeStorage = error "not implemented: disposeStorage"

-- object
addObject :: GLStorage -> String -> Primitive -> Maybe (IndexStream Buffer) -> Map String (Stream Buffer) -> [String] -> IO Object
addObject input slotName prim indices attribs uniformNames = do
    let sch = schema input
    forM_ uniformNames $ \n -> case Map.lookup n (uniforms sch) of
        Nothing -> throw $ userError $ "Unknown uniform: " ++ show n
        _ -> return ()
    case Map.lookup slotName (objectArrays sch) of
        Nothing -> throw $ userError $ "Unknown slot: " ++ show slotName
        Just (ObjectArraySchema sPrim sAttrs) -> do
            when (sPrim /= (primitiveToFetchPrimitive prim)) $ throw $ userError $
                "Primitive mismatch for slot (" ++ show slotName ++ ") expected " ++ show sPrim  ++ " but got " ++ show prim
            let sType = fmap streamToStreamType attribs
            when (sType /= sAttrs) $ throw $ userError $ unlines $ 
                [ "Attribute stream mismatch for slot (" ++ show slotName ++ ") expected "
                , show sAttrs
                , " but got "
                , show sType
                ]
                
    let slotIdx = case slotName `Map.lookup` slotMap input of
            Nothing -> error $ "internal error (slot index): " ++ show slotName
            Just i  -> i
        seed = objSeed input
    order <- newIORef 0
    enabled <- newIORef True
    index <- readIORef seed
    modifyIORef seed (1+)
    (setters,unis) <- mkUniform [(n,t) | n <- uniformNames, let Just t = Map.lookup n (uniforms sch)]
    cmdsRef <- newIORef (V.singleton V.empty)
    let obj = Object
            { objSlot       = slotIdx
            , objPrimitive  = prim
            , objIndices    = indices
            , objAttributes = attribs
            , objUniSetter  = setters
            , objUniSetup   = unis
            , objOrder      = order
            , objEnabled    = enabled
            , objId         = index
            , objCommands   = cmdsRef
            }

    modifyIORef (slotVector input ! slotIdx) $ \(GLSlot objs _ _) -> GLSlot (IM.insert index obj objs) V.empty Generate

    -- generate GLObjectCommands for the new object
    {-
        foreach pipeline:
            foreach realted program:
                generate commands
    -}
    ppls <- readIORef $ pipelines input
    let topUnis = uniformSetup input
    cmds <- V.forM ppls $ \mp -> case mp of
        Nothing -> return V.empty
        Just p  -> do
            Just ic <- readIORef $ glInput p
            case icSlotMapInputToPipeline ic ! slotIdx of
                Nothing         -> do
                    putStrLn $ " ** slot is not used!"
                    return V.empty   -- this slot is not used in that pipeline
                Just pSlotIdx   -> do
                    putStrLn "slot is used!" 
                    --where
                    let emptyV = V.replicate (V.length $ glPrograms p) []
                    return $ emptyV // [(prgIdx,createObjectCommands (glTexUnitMapping p) topUnis obj (glPrograms p ! prgIdx))| prgIdx <- glSlotPrograms p ! pSlotIdx]
    writeIORef cmdsRef cmds
    return obj

removeObject :: GLStorage -> Object -> IO ()
removeObject p obj = modifyIORef (slotVector p ! objSlot obj) $ \(GLSlot objs _ _) -> GLSlot (IM.delete (objId obj) objs) V.empty Generate

enableObject :: Object -> Bool -> IO ()
enableObject obj b = writeIORef (objEnabled obj) b

setObjectOrder :: GLStorage -> Object -> Int -> IO ()
setObjectOrder p obj i = do
    writeIORef (objOrder obj) i
    modifyIORef (slotVector p ! objSlot obj) $ \(GLSlot objs sorted _) -> GLSlot objs sorted Reorder

objectUniformSetter :: Object -> Map GLUniformName InputSetter
objectUniformSetter = objUniSetter

setScreenSize :: GLStorage -> Word -> Word -> IO ()
setScreenSize p w h = writeIORef (screenSize p) (w,h)

sortSlotObjects :: GLStorage -> IO ()
sortSlotObjects p = V.forM_ (slotVector p) $ \slotRef -> do
    GLSlot objMap sortedV ord <- readIORef slotRef
    let cmpFun (a,_) (b,_) = a `compare` b
        doSort objs = do
            ordObjsM <- V.thaw objs
            I.sortBy cmpFun ordObjsM
            ordObjs <- V.freeze ordObjsM
            writeIORef slotRef (GLSlot objMap ordObjs Ordered)
    case ord of
        Ordered -> return ()
        Generate -> do
            objs <- V.forM (V.fromList $ IM.elems objMap) $ \obj -> do
                ord <- readIORef $ objOrder obj
                return (ord,obj)
            doSort objs
        Reorder -> do
            objs <- V.forM sortedV $ \(_,obj) -> do
                ord <- readIORef $ objOrder obj
                return (ord,obj)
            doSort objs

createObjectCommands :: Map String (IORef GLint) -> Map String GLUniform -> Object -> GLProgram -> [GLObjectCommand]
createObjectCommands texUnitMap topUnis obj prg = objUniCmds ++ objStreamCmds ++ [objDrawCmd]
  where
    -- object draw command
    objDrawCmd = case objIndices obj of
        Nothing -> GLDrawArrays prim 0 (fromIntegral count)
        Just (IndexStream (Buffer arrs bo) arrIdx start idxCount) -> GLDrawElements prim (fromIntegral idxCount) idxType bo ptr
          where
            ArrayDesc arrType arrLen arrOffs arrSize = arrs ! arrIdx
            idxType = arrayTypeToGLType arrType
            ptr    = intPtrToPtr $! fromIntegral (arrOffs + start * sizeOfArrayType arrType)
      where
        objAttrs = objAttributes obj
        prim = primitiveToGLType $ objPrimitive obj
        count = head [c | Stream _ _ _ _ c <- Map.elems objAttrs]

    -- object uniform commands
    -- texture slot setup commands
    objUniCmds = uniCmds ++ texCmds
      where
        uniCmds = [GLSetUniform i u | (n,i) <- uniMap, let u = Map.findWithDefault (topUni n) n objUnis]
        uniMap  = Map.toList $ inputUniforms prg
        topUni n = Map.findWithDefault (error $ "internal error (createObjectCommands): " ++ show n) n topUnis
        objUnis = objUniSetup obj
        texUnis = S.toList $ inputTextureUniforms prg
        texCmds = [ GLBindTexture (inputTypeToTextureTarget $ uniInputType u) texUnit u
                  | n <- texUnis
                  , let u = Map.findWithDefault (topUni n) n objUnis
                  , let texUnit = Map.findWithDefault (error $ "internal error (createObjectCommands - Texture Unit): " ++ show n) n texUnitMap
                  ]
        uniInputType (GLUniform ty _) = ty

    -- object attribute stream commands
    objStreamCmds = [attrCmd i s | (i,name) <- Map.elems attrMap, let Just s = Map.lookup name objAttrs]
      where 
        attrMap = inputStreams prg
        objAttrs = objAttributes obj
        attrCmd i s = case s of
            Stream ty (Buffer arrs bo) arrIdx start len -> case ty of
                Attribute_Word   -> setIntAttrib 1
                Attribute_V2U    -> setIntAttrib 2
                Attribute_V3U    -> setIntAttrib 3
                Attribute_V4U    -> setIntAttrib 4
                Attribute_Int    -> setIntAttrib 1
                Attribute_V2I    -> setIntAttrib 2
                Attribute_V3I    -> setIntAttrib 3
                Attribute_V4I    -> setIntAttrib 4
                Attribute_Float  -> setFloatAttrib 1
                Attribute_V2F    -> setFloatAttrib 2
                Attribute_V3F    -> setFloatAttrib 3
                Attribute_V4F    -> setFloatAttrib 4
                Attribute_M22F   -> setFloatAttrib 4
                Attribute_M23F   -> setFloatAttrib 6
                Attribute_M24F   -> setFloatAttrib 8
                Attribute_M32F   -> setFloatAttrib 6
                Attribute_M33F   -> setFloatAttrib 9
                Attribute_M34F   -> setFloatAttrib 12
                Attribute_M42F   -> setFloatAttrib 8
                Attribute_M43F   -> setFloatAttrib 12
                Attribute_M44F   -> setFloatAttrib 16
              where
                setFloatAttrib n = GLSetVertexAttribArray i bo n glType (ptr n)
                setIntAttrib n = GLSetVertexAttribIArray i bo n glType (ptr n)
                ArrayDesc arrType arrLen arrOffs arrSize = arrs ! arrIdx
                glType = arrayTypeToGLType arrType
                ptr compCnt   = intPtrToPtr $! fromIntegral (arrOffs + start * fromIntegral compCnt * sizeOfArrayType arrType)

            -- constant generic attribute
            constAttr -> GLSetVertexAttrib i constAttr

nullSetter :: GLUniformName -> String -> a -> IO ()
--nullSetter n t _ = return () -- Prelude.putStrLn $ "WARNING: unknown uniform: " ++ SB.unpack n ++ " :: " ++ t
nullSetter n t _ = Prelude.putStrLn $ "WARNING: unknown uniform: " ++ show n ++ " :: " ++ t

uniformBool  :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun Bool
uniformV2B   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V2B
uniformV3B   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V3B
uniformV4B   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V4B

uniformWord  :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun Word32
uniformV2U   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V2U
uniformV3U   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V3U
uniformV4U   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V4U

uniformInt   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun Int32
uniformV2I   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V2I
uniformV3I   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V3I
uniformV4I   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V4I

uniformFloat :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun Float
uniformV2F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V2F
uniformV3F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V3F
uniformV4F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun V4F

uniformM22F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun M22F
uniformM23F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun M23F
uniformM24F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun M24F
uniformM32F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun M32F
uniformM33F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun M33F
uniformM34F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun M34F
uniformM42F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun M42F
uniformM43F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun M43F
uniformM44F   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun M44F

uniformFTexture2D   :: GLUniformName -> Map GLUniformName InputSetter -> SetterFun TextureData

uniformBool n is = case Map.lookup n is of
    Just (SBool fun)    -> fun
    _   -> nullSetter n "Bool"

uniformV2B n is = case Map.lookup n is of
    Just (SV2B fun)    -> fun
    _   -> nullSetter n "V2B"

uniformV3B n is = case Map.lookup n is of
    Just (SV3B fun)    -> fun
    _   -> nullSetter n "V3B"

uniformV4B n is = case Map.lookup n is of
    Just (SV4B fun)    -> fun
    _   -> nullSetter n "V4B"

uniformWord n is = case Map.lookup n is of
    Just (SWord fun)    -> fun
    _   -> nullSetter n "Word"

uniformV2U n is = case Map.lookup n is of
    Just (SV2U fun)    -> fun
    _   -> nullSetter n "V2U"

uniformV3U n is = case Map.lookup n is of
    Just (SV3U fun)    -> fun
    _   -> nullSetter n "V3U"

uniformV4U n is = case Map.lookup n is of
    Just (SV4U fun)    -> fun
    _   -> nullSetter n "V4U"

uniformInt n is = case Map.lookup n is of
    Just (SInt fun)    -> fun
    _   -> nullSetter n "Int"

uniformV2I n is = case Map.lookup n is of
    Just (SV2I fun)    -> fun
    _   -> nullSetter n "V2I"

uniformV3I n is = case Map.lookup n is of
    Just (SV3I fun)    -> fun
    _   -> nullSetter n "V3I"

uniformV4I n is = case Map.lookup n is of
    Just (SV4I fun)    -> fun
    _   -> nullSetter n "V4I"

uniformFloat n is = case Map.lookup n is of
    Just (SFloat fun)    -> fun
    _   -> nullSetter n "Float"

uniformV2F n is = case Map.lookup n is of
    Just (SV2F fun)    -> fun
    _   -> nullSetter n "V2F"

uniformV3F n is = case Map.lookup n is of
    Just (SV3F fun)    -> fun
    _   -> nullSetter n "V3F"

uniformV4F n is = case Map.lookup n is of
    Just (SV4F fun)    -> fun
    _   -> nullSetter n "V4F"

uniformM22F n is = case Map.lookup n is of
    Just (SM22F fun)    -> fun
    _   -> nullSetter n "M22F"

uniformM23F n is = case Map.lookup n is of
    Just (SM23F fun)    -> fun
    _   -> nullSetter n "M23F"

uniformM24F n is = case Map.lookup n is of
    Just (SM24F fun)    -> fun
    _   -> nullSetter n "M24F"

uniformM32F n is = case Map.lookup n is of
    Just (SM32F fun)    -> fun
    _   -> nullSetter n "M32F"

uniformM33F n is = case Map.lookup n is of
    Just (SM33F fun)    -> fun
    _   -> nullSetter n "M33F"

uniformM34F n is = case Map.lookup n is of
    Just (SM34F fun)    -> fun
    _   -> nullSetter n "M34F"

uniformM42F n is = case Map.lookup n is of
    Just (SM42F fun)    -> fun
    _   -> nullSetter n "M42F"

uniformM43F n is = case Map.lookup n is of
    Just (SM43F fun)    -> fun
    _   -> nullSetter n "M43F"

uniformM44F n is = case Map.lookup n is of
    Just (SM44F fun)    -> fun
    _   -> nullSetter n "M44F"

uniformFTexture2D n is = case Map.lookup n is of
    Just (SFTexture2D fun)    -> fun
    _   -> nullSetter n "FTexture2D"
