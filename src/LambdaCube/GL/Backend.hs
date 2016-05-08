{-# LANGUAGE TupleSections, MonadComprehensions, RecordWildCards, LambdaCase #-}
module LambdaCube.GL.Backend where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Bits
import Data.IORef
import Data.IntMap (IntMap)
import Data.Maybe (isNothing,fromJust)
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector,(!),(//))
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import Graphics.GL.Core33
import Foreign
import Foreign.C.String

-- LC IR imports
import LambdaCube.PipelineSchema
import LambdaCube.Linear
import LambdaCube.IR hiding (streamType)
import qualified LambdaCube.IR as IR

import LambdaCube.GL.Type
import LambdaCube.GL.Util

import LambdaCube.GL.Data
import LambdaCube.GL.Input

setupRasterContext :: RasterContext -> IO ()
setupRasterContext = cvt
  where
    cff :: FrontFace -> GLenum
    cff CCW = GL_CCW
    cff CW  = GL_CW

    setProvokingVertex :: ProvokingVertex -> IO ()
    setProvokingVertex pv = glProvokingVertex $ case pv of
        FirstVertex -> GL_FIRST_VERTEX_CONVENTION
        LastVertex  -> GL_LAST_VERTEX_CONVENTION

    setPointSize :: PointSize -> IO ()
    setPointSize ps = case ps of
        ProgramPointSize    -> glEnable GL_PROGRAM_POINT_SIZE
        PointSize s         -> do
            glDisable GL_PROGRAM_POINT_SIZE
            glPointSize $ realToFrac s

    cvt :: RasterContext -> IO ()
    cvt (PointCtx ps fts sc) = do
        setPointSize ps
        glPointParameterf GL_POINT_FADE_THRESHOLD_SIZE (realToFrac fts)
        glPointParameterf GL_POINT_SPRITE_COORD_ORIGIN $ realToFrac $ case sc of
            LowerLeft   -> GL_LOWER_LEFT
            UpperLeft   -> GL_UPPER_LEFT

    cvt (LineCtx lw pv) = do
        glLineWidth (realToFrac lw)
        setProvokingVertex pv

    cvt (TriangleCtx cm pm po pv) = do
        -- cull mode
        case cm of
            CullNone    -> glDisable GL_CULL_FACE
            CullFront f -> do
                glEnable    GL_CULL_FACE
                glCullFace  GL_FRONT
                glFrontFace $ cff f
            CullBack f -> do
                glEnable    GL_CULL_FACE
                glCullFace  GL_BACK
                glFrontFace $ cff f

        -- polygon mode
        case pm of
            PolygonPoint ps -> do
                setPointSize ps
                glPolygonMode GL_FRONT_AND_BACK GL_POINT
            PolygonLine lw  -> do
                glLineWidth (realToFrac lw)
                glPolygonMode GL_FRONT_AND_BACK GL_LINE
            PolygonFill  -> glPolygonMode GL_FRONT_AND_BACK GL_FILL

        -- polygon offset
        glDisable GL_POLYGON_OFFSET_POINT
        glDisable GL_POLYGON_OFFSET_LINE
        glDisable GL_POLYGON_OFFSET_FILL
        case po of
            NoOffset -> return ()
            Offset f u -> do
                glPolygonOffset (realToFrac f) (realToFrac u)
                glEnable $ case pm of
                    PolygonPoint _  -> GL_POLYGON_OFFSET_POINT
                    PolygonLine  _  -> GL_POLYGON_OFFSET_LINE
                    PolygonFill     -> GL_POLYGON_OFFSET_FILL

        -- provoking vertex
        setProvokingVertex pv

setupAccumulationContext :: AccumulationContext -> IO ()
setupAccumulationContext (AccumulationContext n ops) = cvt ops
  where
    cvt :: [FragmentOperation] -> IO ()
    cvt (StencilOp a b c : DepthOp f m : xs) = do
        -- TODO
        cvtC 0 xs
    cvt (StencilOp a b c : xs) = do
        -- TODO
        cvtC 0 xs
    cvt (DepthOp df dm : xs) = do
        -- TODO
        glDisable GL_STENCIL_TEST
        case df == Always && dm == False of
            True    -> glDisable GL_DEPTH_TEST
            False   -> do
                glEnable GL_DEPTH_TEST
                glDepthFunc $! comparisonFunctionToGLType df
                glDepthMask (cvtBool dm)
        cvtC 0 xs
    cvt xs = do 
        glDisable GL_DEPTH_TEST
        glDisable GL_STENCIL_TEST
        cvtC 0 xs

    cvtC :: Int -> [FragmentOperation] -> IO ()
    cvtC i (ColorOp b m : xs) = do
        -- TODO
        case b of
            NoBlending -> do
                -- FIXME: requires GL 3.1
                --glDisablei GL_BLEND $ fromIntegral GL_DRAW_BUFFER0 + fromIntegral i
                glDisable GL_BLEND -- workaround
                glDisable GL_COLOR_LOGIC_OP
            BlendLogicOp op -> do
                glDisable   GL_BLEND
                glEnable    GL_COLOR_LOGIC_OP
                glLogicOp $ logicOperationToGLType op
            Blend cEq aEq scF dcF saF daF (V4 r g b a) -> do
                glDisable GL_COLOR_LOGIC_OP
                -- FIXME: requires GL 3.1
                --glEnablei GL_BLEND $ fromIntegral GL_DRAW_BUFFER0 + fromIntegral i
                glEnable GL_BLEND -- workaround
                glBlendEquationSeparate (blendEquationToGLType cEq) (blendEquationToGLType aEq)
                glBlendFuncSeparate (blendingFactorToGLType scF) (blendingFactorToGLType dcF)
                                    (blendingFactorToGLType saF) (blendingFactorToGLType daF)
                glBlendColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
        let cvt True    = 1
            cvt False   = 0
            (mr,mg,mb,ma) = case m of
                VBool r             -> (cvt r, 1, 1, 1)
                VV2B (V2 r g)       -> (cvt r, cvt g, 1, 1)
                VV3B (V3 r g b)     -> (cvt r, cvt g, cvt b, 1)
                VV4B (V4 r g b a)   -> (cvt r, cvt g, cvt b, cvt a)
                _           -> (1,1,1,1)
        glColorMask mr mg mb ma
        cvtC (i + 1) xs
    cvtC _ [] = return ()

    cvtBool :: Bool -> GLboolean
    cvtBool True  = 1
    cvtBool False = 0

clearRenderTarget :: [ClearImage] -> IO ()
clearRenderTarget values = do
    let setClearValue (m,i) value = case value of
            ClearImage Depth (VFloat v) -> do
                glDepthMask 1
                glClearDepth $ realToFrac v
                return (m .|. GL_DEPTH_BUFFER_BIT, i)
            ClearImage Stencil (VWord v) -> do
                glClearStencil $ fromIntegral v
                return (m .|. GL_STENCIL_BUFFER_BIT, i)
            ClearImage Color c -> do
                let (r,g,b,a) = case c of
                        VFloat r            -> (realToFrac r, 0, 0, 1)
                        VV2F (V2 r g)       -> (realToFrac r, realToFrac g, 0, 1)
                        VV3F (V3 r g b)     -> (realToFrac r, realToFrac g, realToFrac b, 1)
                        VV4F (V4 r g b a)   -> (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                        _                   -> (0,0,0,1)
                glColorMask 1 1 1 1
                glClearColor r g b a
                return (m .|. GL_COLOR_BUFFER_BIT, i+1)
            _ -> error "internal error (clearRenderTarget)"
    (mask,_) <- foldM setClearValue (0,0) values
    glClear $ fromIntegral mask


printGLStatus = checkGL >>= print
printFBOStatus = checkFBO >>= print

compileProgram :: Program -> IO GLProgram
compileProgram p = do
    po <- glCreateProgram
    --putStrLn $ "compile program: " ++ show po
    let createAndAttach src t = do
            o <- glCreateShader t
            compileShader o [src]
            glAttachShader po o
            --putStr "    + compile shader source: " >> printGLStatus
            return o

    objs <- sequence $ createAndAttach (vertexShader p) GL_VERTEX_SHADER : createAndAttach (fragmentShader p) GL_FRAGMENT_SHADER : case geometryShader p of
        Nothing -> []
        Just s  -> [createAndAttach s GL_GEOMETRY_SHADER]

    forM_ (zip (V.toList $ programOutput p) [0..]) $ \(Parameter n t,i) -> withCString n $ \pn -> do
        --putStrLn ("variable " ++ show n ++ " attached to color number #" ++ show i)
        glBindFragDataLocation po i $ castPtr pn
    --putStr "    + setup shader output mapping: " >> printGLStatus

    glLinkProgram po
    log <- printProgramLog po

    -- check link status
    status <- glGetProgramiv1 GL_LINK_STATUS po
    when (status /= fromIntegral GL_TRUE) $ fail $ unlines ["link program failed:",log]

    -- check program input
    (uniforms,uniformsType) <- queryUniforms po
    (attributes,attributesType) <- queryStreams po
    --print uniforms
    --print attributes
    let lcUniforms = (programUniforms p) `Map.union` (programInTextures p)
        lcStreams = fmap ty (programStreams p)
        check a m = and $ map go $ Map.toList m
          where go (k,b) = case Map.lookup k a of
                  Nothing -> False
                  Just x -> x == b
    unless (check lcUniforms uniformsType) $ fail $ unlines
      [ "shader program uniform input mismatch!"
      , "expected: " ++ show lcUniforms
      , "actual: " ++ show uniformsType
      ]
    unless (check lcStreams attributesType) $ fail $ "shader program stream input mismatch! " ++ show (attributesType,lcStreams)
    -- the public (user) pipeline and program input is encoded by the objectArrays, therefore the programs does not distinct the render and slot textures input
    let inUniNames = programUniforms p
        inUniforms = L.filter (\(n,v) -> Map.member n inUniNames) $ Map.toList $ uniforms
        inTextureNames = programInTextures p
        inTextures = L.filter (\(n,v) -> Map.member n inTextureNames) $ Map.toList $ uniforms
        texUnis = [n | (n,_) <- inTextures, Map.member n (programUniforms p)]
    let prgInTextures = Map.keys inTextureNames
        uniInTextures = map fst inTextures
    {-
    unless (S.fromList prgInTextures == S.fromList uniInTextures) $ fail $ unlines
      [ "shader program uniform texture input mismatch!"
      , "expected: " ++ show prgInTextures
      , "actual: " ++ show uniInTextures
      , "vertex shader:"
      , vertexShader p
      , "geometry shader:"
      , fromMaybe "" (geometryShader p)
      , "fragment shader:"
      , fragmentShader p
      ]
    -}
    --putStrLn $ "uniTrie: " ++ show (Map.keys uniTrie)
    --putStrLn $ "inUniNames: " ++ show inUniNames
    --putStrLn $ "inUniforms: " ++ show inUniforms
    --putStrLn $ "inTextureNames: " ++ show inTextureNames
    --putStrLn $ "inTextures: " ++ show inTextures
    --putStrLn $ "texUnis: " ++ show texUnis
    let valA = Map.toList $ attributes
        valB = Map.toList $ programStreams p
    --putStrLn "------------"
    --print $ Map.toList $ attributes
    --print $ Map.toList $ programStreams p
    let lcStreamName = fmap name (programStreams p)
    return $ GLProgram
        { shaderObjects         = objs
        , programObject         = po
        , inputUniforms         = Map.fromList inUniforms
        , inputTextures         = Map.fromList inTextures
        , inputTextureUniforms  = S.fromList $ texUnis
        , inputStreams          = Map.fromList [(n,(idx, attrName)) | (n,idx) <- Map.toList $ attributes, let attrName = fromMaybe (error $ "missing attribute: " ++ n) $ Map.lookup n lcStreamName]
        }

compileRenderTarget :: Vector TextureDescriptor -> Vector GLTexture -> RenderTarget -> IO GLRenderTarget
compileRenderTarget texs glTexs (RenderTarget targets) = do
    let isFB (Framebuffer _)    = True
        isFB _                  = False
        images = [img | TargetItem _ (Just img) <- V.toList targets]
    case all isFB images of
        True -> do
            let bufs = [cvt img | TargetItem Color img <- V.toList targets]
                cvt a = case a of
                    Nothing                     -> GL_NONE
                    Just (Framebuffer Color)    -> GL_BACK_LEFT
                    _                           -> error "internal error (compileRenderTarget)!"
            return $ GLRenderTarget
                { framebufferObject         = 0
                , framebufferDrawbuffers    = Just bufs
                }
        False -> do
            when (any isFB images) $ fail "internal error (compileRenderTarget)!"
            fbo <- alloca $! \pbo -> glGenFramebuffers 1 pbo >> peek pbo
            glBindFramebuffer GL_DRAW_FRAMEBUFFER fbo
            {-
                void glFramebufferTexture1D(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level);
                    GL_TEXTURE_1D
                void glFramebufferTexture2D(GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level);
                    GL_TEXTURE_2D
                    GL_TEXTURE_RECTANGLE
                    GL_TEXTURE_CUBE_MAP_POSITIVE_X
                    GL_TEXTURE_CUBE_MAP_POSITIVE_Y
                    GL_TEXTURE_CUBE_MAP_POSITIVE_Z
                    GL_TEXTURE_CUBE_MAP_NEGATIVE_X
                    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
                    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
                    GL_TEXTURE_2D_MULTISAMPLE
                void glFramebufferTextureLayer(GLenum target, GLenum attachment, GLuint texture, GLint level, GLint layer);
                void glFramebufferRenderbuffer(GLenum target, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer);
                void glFramebufferTexture(GLenum target, GLenum attachment, GLuint texture, GLint level);
            -}
            let attach attachment (TextureImage texIdx level (Just layer)) =
                    glFramebufferTextureLayer GL_DRAW_FRAMEBUFFER attachment (glTextureTarget $ glTexs ! texIdx) (fromIntegral level) (fromIntegral layer)
                attach attachment (TextureImage texIdx level Nothing) = do
                    let glTex = glTexs ! texIdx
                        tex = texs ! texIdx
                        txLevel = fromIntegral level
                        txTarget = glTextureTarget glTex
                        txObj = glTextureObject glTex
                        attachArray = glFramebufferTexture GL_DRAW_FRAMEBUFFER attachment txObj txLevel
                        attach2D    = glFramebufferTexture2D GL_DRAW_FRAMEBUFFER attachment txTarget txObj txLevel
                    case textureType tex of
                        Texture1D     _ n
                            | n > 1             -> attachArray
                            | otherwise         -> glFramebufferTexture1D GL_DRAW_FRAMEBUFFER attachment txTarget txObj txLevel
                        Texture2D     _ n
                            | n > 1             -> attachArray
                            | otherwise         -> attach2D
                        Texture3D     _         -> attachArray
                        TextureCube   _         -> attachArray
                        TextureRect   _         -> attach2D
                        Texture2DMS   _ n _ _
                            | n > 1             -> attachArray
                            | otherwise         -> attach2D
                        TextureBuffer _         -> fail "internalError (compileRenderTarget/TextureBuffer)!"
            
                go a (TargetItem Stencil (Just img)) = do
                    fail "Stencil support is not implemented yet!"
                    return a
                go a (TargetItem Depth (Just img)) = do
                    attach GL_DEPTH_ATTACHMENT img
                    return a
                go (bufs,colorIdx) (TargetItem Color (Just img)) = do
                    let attachment = GL_COLOR_ATTACHMENT0 + fromIntegral colorIdx
                    attach attachment img
                    return (attachment : bufs, colorIdx + 1)
                go (bufs,colorIdx) (TargetItem Color Nothing) = return (GL_NONE : bufs, colorIdx + 1)
                go a _ = return a
            (bufs,_) <- foldM go ([],0) targets
            withArray (reverse bufs) $ glDrawBuffers (fromIntegral $ length bufs)
            return $ GLRenderTarget
                { framebufferObject         = fbo
                , framebufferDrawbuffers    = Nothing
                }

compileStreamData :: StreamData -> IO GLStream
compileStreamData s = do
  let withV w a f = w a (\p -> f $ castPtr p)
  let compileAttr (VFloatArray v) = Array ArrFloat (V.length v) (withV (SV.unsafeWith . V.convert) v)
      compileAttr (VIntArray v) = Array ArrInt32 (V.length v) (withV (SV.unsafeWith . V.convert) v)
      compileAttr (VWordArray v) = Array ArrWord32 (V.length v) (withV (SV.unsafeWith . V.convert) v)
      --TODO: compileAttr (VBoolArray v) = Array ArrWord32 (length v) (withV withArray v)
      (indexMap,arrays) = unzip [((n,i),compileAttr d) | (i,(n,d)) <- zip [0..] $ Map.toList $ streamData s]
      getLength n = l `div` c
        where
          l = case Map.lookup n $ IR.streamData s of
            Just (VFloatArray v) -> V.length v
            Just (VIntArray v) -> V.length v
            Just (VWordArray v) -> V.length v
            _ -> error "compileStreamData - getLength"
          c = case Map.lookup n $ IR.streamType s of
            Just Bool   -> 1
            Just V2B    -> 2
            Just V3B    -> 3
            Just V4B    -> 4
            Just Word   -> 1
            Just V2U    -> 2
            Just V3U    -> 3
            Just V4U    -> 4
            Just Int    -> 1
            Just V2I    -> 2
            Just V3I    -> 3
            Just V4I    -> 4
            Just Float  -> 1
            Just V2F    -> 2
            Just V3F    -> 3
            Just V4F    -> 4
            Just M22F   -> 4
            Just M23F   -> 6
            Just M24F   -> 8
            Just M32F   -> 6
            Just M33F   -> 9
            Just M34F   -> 12
            Just M42F   -> 8
            Just M43F   -> 12
            Just M44F   -> 16
            _ -> error "compileStreamData - getLength element count"
  buffer <- compileBuffer arrays
  cmdRef <- newIORef []
  let toStream (n,i) = (n,Stream
        { streamType    = fromMaybe (error $ "missing attribute: " ++ n) $ toStreamType =<< Map.lookup n (IR.streamType s)
        , streamBuffer  = buffer
        , streamArrIdx  = i
        , streamStart   = 0
        , streamLength  = getLength n
        })
  return $ GLStream
    { glStreamCommands    = cmdRef
    , glStreamPrimitive   = case streamPrimitive s of
        Points              -> PointList
        Lines               -> LineList
        Triangles           -> TriangleList
        LinesAdjacency      -> LineListAdjacency
        TrianglesAdjacency  -> TriangleListAdjacency
    , glStreamAttributes  = Map.fromList $ map toStream indexMap
    , glStreamProgram     = V.head $ streamPrograms s
    }

createStreamCommands :: Map String (IORef GLint) -> Map String GLUniform -> Map String (Stream Buffer) -> Primitive -> GLProgram -> [GLObjectCommand]
createStreamCommands texUnitMap topUnis attrs primitive prg = streamUniCmds ++ streamCmds ++ [drawCmd]
  where
    -- object draw command
    drawCmd = GLDrawArrays prim 0 (fromIntegral count)
      where
        prim = primitiveToGLType primitive
        count = head [c | Stream _ _ _ _ c <- Map.elems attrs]

    -- object uniform commands
    -- texture slot setup commands
    streamUniCmds = uniCmds ++ texCmds
      where
        uniCmds = [GLSetUniform i u | (n,i) <- uniMap, let u = topUni n]
        uniMap  = Map.toList $ inputUniforms prg
        topUni n = Map.findWithDefault (error "internal error (createStreamCommands)!") n topUnis
        texUnis = S.toList $ inputTextureUniforms prg
        texCmds = [ GLBindTexture (inputTypeToTextureTarget $ uniInputType u) texUnit u
                  | n <- texUnis
                  , let u = topUni n
                  , let texUnit = Map.findWithDefault (error "internal error (createStreamCommands - Texture Unit)") n texUnitMap
                  ]
        uniInputType (GLUniform ty _) = ty

    -- object attribute stream commands
    streamCmds = [attrCmd i s | (i,name) <- Map.elems attrMap, let s = fromMaybe (error $ "missing attribute: " ++ name) $ Map.lookup name attrs]
      where 
        attrMap = inputStreams prg
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

allocRenderer :: Pipeline -> IO GLRenderer
allocRenderer p = do
    smps <- V.mapM compileSampler $ samplers p
    texs <- V.mapM compileTexture $ textures p
    trgs <- V.mapM (compileRenderTarget (textures p) texs) $ targets p
    prgs <- V.mapM compileProgram $ programs p
    -- texture unit mapping ioref trie
    -- texUnitMapRefs :: Map UniformName (IORef TextureUnit)
    texUnitMapRefs <- Map.fromList <$> mapM (\k -> (k,) <$> newIORef 0) (S.toList $ S.fromList $ concat $ V.toList $ V.map (Map.keys . programInTextures) $ programs p)
    let (cmds,st) = runState (mapM (compileCommand texUnitMapRefs smps texs trgs prgs) $ V.toList $ commands p) initCGState
    input <- newIORef Nothing
    -- default Vertex Array Object
    vao <- alloca $! \pvao -> glGenVertexArrays 1 pvao >> peek pvao
    strs <- V.mapM compileStreamData $ streams p
    return $ GLRenderer
        { glPrograms        = prgs
        , glTextures        = texs
        , glSamplers        = smps
        , glTargets         = trgs
        , glCommands        = cmds
        , glSlotPrograms    = V.map (V.toList . slotPrograms) $ IR.slots p
        , glInput           = input
        , glSlotNames       = V.map slotName $ IR.slots p
        , glVAO             = vao
        , glTexUnitMapping  = texUnitMapRefs
        , glStreams         = strs
        }

disposeRenderer :: GLRenderer -> IO ()
disposeRenderer p = do
    setStorage' p Nothing
    V.forM_ (glPrograms p) $ \prg -> do
        glDeleteProgram $ programObject prg
        mapM_ glDeleteShader $ shaderObjects prg
    let targets = glTargets p
    withArray (map framebufferObject $ V.toList targets) $ (glDeleteFramebuffers $ fromIntegral $ V.length targets)
    let textures = glTextures p
    withArray (map glTextureObject $ V.toList textures) $ (glDeleteTextures $ fromIntegral $ V.length textures)
    let samplers = glSamplers p
    withArray (map glSamplerObject $ V.toList samplers) $ (glDeleteSamplers . fromIntegral . V.length $ glSamplers p)
    with (glVAO p) $ (glDeleteVertexArrays 1)

{-
data ObjectArraySchema
    = ObjectArraySchema
    { primitive     :: FetchPrimitive
    , attributes    :: Trie StreamType
    }
    deriving Show

data PipelineSchema
    = PipelineSchema
    { objectArrays  :: Trie ObjectArraySchema
    , uniforms      :: Trie InputType
    }
    deriving Show
-}
isSubTrie :: (a -> a -> Bool) -> Map String a -> Map String a -> Bool
isSubTrie eqFun universe subset = and [isMember a (Map.lookup n universe) | (n,a) <- Map.toList subset]
  where
    isMember a Nothing  = False
    isMember a (Just b) = eqFun a b

-- TODO: if there is a mismatch thow detailed error message in the excoeption, containing the missing attributes and uniforms
{-
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
-}

setStorage :: GLRenderer -> GLStorage -> IO (Maybe String)
setStorage p input' = setStorage' p (Just input')

setStorage' :: GLRenderer -> Maybe GLStorage -> IO (Maybe String)
setStorage' p@GLRenderer{..} input' = do
    -- TODO: check matching input schema
    {-
    case input' of
        Nothing     -> return ()
        Just input  -> schemaFromPipeline p
    -}
    {-
        deletion:
            - remove pipeline's object commands from used objectArrays
            - remove pipeline from attached pipelines vector
    -}
    readIORef glInput >>= \case
        Nothing -> return ()
        Just InputConnection{..} -> do
            let slotRefs = slotVector icInput
            modifyIORef (pipelines icInput) $ \v -> v // [(icId,Nothing)]
            V.forM_ icSlotMapPipelineToInput $ \slotIdx -> do
                slot <- readIORef (slotRefs ! slotIdx)
                forM_ (IM.elems $ objectMap slot) $ \obj -> do
                    modifyIORef (objCommands obj) $ \v -> v // [(icId,V.empty)]
    {-
        addition:
            - get an id from pipeline input
            - add to attached pipelines
            - generate slot mappings
            - update used objectArrays, and generate object commands for objects in the related objectArrays
    -}
    case input' of
        Nothing -> writeIORef glInput Nothing >> return Nothing
        Just input -> do
            let pipelinesRef = pipelines input
            oldPipelineV <- readIORef pipelinesRef
            (idx,shouldExtend) <- case V.findIndex isNothing oldPipelineV of
                Nothing -> do
                    -- we don't have empty space, hence we double the vector size
                    let len = V.length oldPipelineV
                    modifyIORef pipelinesRef $ \v -> (V.concat [v,V.replicate len Nothing]) // [(len,Just p)]
                    return (len,Just len)
                Just i  -> do
                    modifyIORef pipelinesRef $ \v -> v // [(i,Just p)]
                    return (i,Nothing)
            -- create input connection
            let sm      = slotMap input
                pToI    = [i | n <- glSlotNames, let i = fromMaybe (error $ "missing object array: " ++ n) $ Map.lookup n sm]
                iToP    = V.update (V.replicate (Map.size sm) Nothing) (V.imap (\i v -> (v, Just i)) pToI)
            writeIORef glInput $ Just $ InputConnection idx input pToI iToP

            -- generate object commands for related objectArrays
            {-
                for each slot in pipeline:
                    map slot name to input slot name
                    for each object:
                        generate command program vector => for each dependent program:
                            generate object commands
            -}
            let slotV = slotVector input
                progV = glPrograms
                --texUnitMap = glTexUnitMapping p
                topUnis = uniformSetup input
                emptyV  = V.replicate (V.length progV) []
                extend v = case shouldExtend of
                    Nothing -> v
                    Just l  -> V.concat [v,V.replicate l V.empty]
            V.forM_ (V.zip pToI glSlotPrograms) $ \(slotIdx,prgs) -> do
                slot <- readIORef $ slotV ! slotIdx
                forM_ (IM.elems $ objectMap slot) $ \obj -> do
                    let cmdV = emptyV // [(prgIdx,createObjectCommands glTexUnitMapping topUnis obj (progV ! prgIdx)) | prgIdx <- prgs]
                    modifyIORef (objCommands obj) $ \v -> extend v // [(idx,cmdV)]
            -- generate stream commands
            V.forM_ glStreams $ \s -> do
              writeIORef (glStreamCommands s) $ createStreamCommands glTexUnitMapping topUnis (glStreamAttributes s) (glStreamPrimitive s) (progV ! glStreamProgram s)
            return Nothing
{-
  track state:
    - render target
    - binded textures
-}

{-
  render steps:
    - update uniforms
        - per uniform setup
        - buffer setup (one buffer per object, which has per at least one object uniform)
    - new command: set uniform buffer (binds uniform buffer to program's buffer slot)
    - render slot steps:
        - set uniform buffer or set uniforms separately
        - set vertex and index array
        - call draw command
-}
{-
  storage alternatives:
    - interleaved / separated
    - VAO or VBOs
-}
    {-
      strategy:
        step 1: generate commands for an object
        step 2: sort object merge and do optimization by filtering redundant commands
    -}
{-
  design:
    runtime eleminiation of redundant buffer bind commands and redundant texture bind commands
-}
{-
  track:
    buffer binding on various targets: GL_ARRAY_BUFFER, GL_ELEMENT_ARRAY_BUFFER
    glEnable/DisableVertexAttribArray
-}
renderSlot :: [GLObjectCommand] -> IO ()
renderSlot cmds = forM_ cmds $ \cmd -> do
    case cmd of
        GLSetVertexAttribArray idx buf size typ ptr     -> do
                                                            glBindBuffer GL_ARRAY_BUFFER buf
                                                            glEnableVertexAttribArray idx
                                                            glVertexAttribPointer idx size typ (fromIntegral GL_FALSE) 0 ptr
        GLSetVertexAttribIArray idx buf size typ ptr    -> do
                                                            glBindBuffer GL_ARRAY_BUFFER buf
                                                            glEnableVertexAttribArray idx
                                                            glVertexAttribIPointer idx size typ 0 ptr
        GLDrawArrays mode first count                   -> glDrawArrays mode first count
        GLDrawElements mode count typ buf indicesPtr    -> do
                                                            glBindBuffer GL_ELEMENT_ARRAY_BUFFER buf
                                                            glDrawElements mode count typ indicesPtr
        GLSetUniform idx (GLUniform ty ref)             -> setUniform idx ty ref
        GLBindTexture txTarget tuRef (GLUniform _ ref)  -> do
                                                            txObjVal <- readIORef ref
                                                            -- HINT: ugly and hacky
                                                            with txObjVal $ \txObjPtr -> do
                                                                txObj <- peek $ castPtr txObjPtr :: IO GLuint
                                                                texUnit <- readIORef tuRef
                                                                glActiveTexture $ GL_TEXTURE0 + fromIntegral texUnit
                                                                glBindTexture txTarget txObj
                                                                --putStrLn $ "to texture unit " ++ show texUnit ++ " texture object " ++ show txObj
        GLSetVertexAttrib idx val                       -> do
                                                            glDisableVertexAttribArray idx
                                                            setVertexAttrib idx val
    --isOk <- checkGL
    --putStrLn $ isOk ++ " - " ++ show cmd

renderFrame :: GLRenderer -> IO ()
renderFrame glp = do
    glBindVertexArray (glVAO glp)
    forM_ (glCommands glp) $ \cmd -> do
        case cmd of
            GLSetRasterContext rCtx         -> setupRasterContext rCtx
            GLSetAccumulationContext aCtx   -> setupAccumulationContext aCtx
            GLSetRenderTarget rt bufs       -> do
                                                -- set target viewport
                                               --when (rt == 0) $ do -- screen out
                                                ic' <- readIORef $ glInput glp
                                                case ic' of
                                                    Nothing -> return ()
                                                    Just ic -> do
                                                                let input = icInput ic
                                                                (w,h) <- readIORef $ screenSize input
                                                                glViewport 0 0 (fromIntegral w) (fromIntegral h)
                                                -- TODO: set FBO target viewport
                                                glBindFramebuffer GL_DRAW_FRAMEBUFFER rt
                                                case bufs of
                                                    Nothing -> return ()
                                                    Just bl -> withArray bl $ glDrawBuffers (fromIntegral $ length bl)
            GLSetProgram p                  -> glUseProgram p
            GLSetSamplerUniform i tu ref    -> glUniform1i i tu >> writeIORef ref tu
            GLSetTexture tu target tx       -> glActiveTexture tu >> glBindTexture target tx
            GLSetSampler tu s               -> glBindSampler tu s
            GLClearRenderTarget vals        -> clearRenderTarget vals
            GLGenerateMipMap tu target      -> glActiveTexture tu >> glGenerateMipmap target
            GLRenderStream streamIdx progIdx  -> do
                                                renderSlot =<< readIORef (glStreamCommands $ glStreams glp ! streamIdx)
            GLRenderSlot slotIdx progIdx    -> do
                                                input <- readIORef (glInput glp)
                                                case input of
                                                    Nothing -> putStrLn "Warning: No pipeline input!" >> return ()
                                                    Just ic -> do
                                                        GLSlot _ objs _ <- readIORef (slotVector (icInput ic) ! (icSlotMapPipelineToInput ic ! slotIdx))
                                                        --putStrLn $ "Rendering " ++ show (V.length objs) ++ " objects"
                                                        V.forM_ objs $ \(_,obj) -> do
                                                            enabled <- readIORef $ objEnabled obj
                                                            when enabled $ do
                                                                cmd <- readIORef $ objCommands obj
                                                                --putStrLn "Render object"
                                                                renderSlot ((cmd ! icId ic) ! progIdx)
            {-
            GLSetSampler
            GLSaveImage
            GLLoadImage
            -}
        --isOk <- checkGL
        --putStrLn $ isOk ++ " - " ++ show cmd

data CGState
    = CGState
    { currentProgram    :: ProgramName
    , textureBinding    :: IntMap GLTexture
    }

initCGState = CGState
    { currentProgram    = error "CGState: empty currentProgram"
    , textureBinding    = IM.empty
    }

type CG a = State CGState a

compileCommand :: Map String (IORef GLint) -> Vector GLSampler -> Vector GLTexture -> Vector GLRenderTarget -> Vector GLProgram -> Command -> CG GLCommand
compileCommand texUnitMap samplers textures targets programs cmd = case cmd of
    SetRasterContext rCtx       -> return $ GLSetRasterContext rCtx
    SetAccumulationContext aCtx -> return $ GLSetAccumulationContext aCtx
    SetRenderTarget rt          -> let GLRenderTarget fbo bufs = targets ! rt in return $ GLSetRenderTarget fbo bufs
    SetProgram p                -> do
                                    modify (\s -> s {currentProgram = p})
                                    return $ GLSetProgram $ programObject $ programs ! p
    SetSamplerUniform n tu      -> do
                                    p <- currentProgram <$> get
                                    case Map.lookup n (inputTextures $ programs ! p) of
                                        Nothing -> return (GLSetProgram (programObject $ programs ! p) {-HACK!!! we have to emit something-}) -- TODO: some drivers does heavy cross stage (vertex/fragment) dead code elimination; fail $ "internal error (SetSamplerUniform)! - " ++ show cmd
                                        Just i  -> case Map.lookup n texUnitMap of
                                            Nothing -> fail $ "internal error (SetSamplerUniform - IORef)! - " ++ show cmd
                                            Just r  -> return $ GLSetSamplerUniform i (fromIntegral tu) r
    SetTexture tu t             -> do
                                    let tex = textures ! t
                                    modify (\s -> s {textureBinding = IM.insert tu tex $ textureBinding s})
                                    return $ GLSetTexture (GL_TEXTURE0 + fromIntegral tu) (glTextureTarget tex) (glTextureObject tex)
    SetSampler tu s             -> return $ GLSetSampler (GL_TEXTURE0 + fromIntegral tu) (maybe 0 (glSamplerObject . (samplers !)) s)
    RenderSlot slot             -> do
                                    p <- currentProgram <$> get
                                    return $ GLRenderSlot slot p
    RenderStream stream         -> do
                                    p <- currentProgram <$> get
                                    return $ GLRenderStream stream p
    ClearRenderTarget vals      -> return $ GLClearRenderTarget $ V.toList vals
    GenerateMipMap tu           -> do
                                    tb <- textureBinding <$> get
                                    case IM.lookup tu tb of
                                        Nothing     -> fail "internal error (GenerateMipMap)!"
                                        Just tex    -> return $ GLGenerateMipMap (GL_TEXTURE0 + fromIntegral tu) (glTextureTarget tex)
{-
    SaveImage _ _               -> undefined
    LoadImage _ _               -> undefined
-}
