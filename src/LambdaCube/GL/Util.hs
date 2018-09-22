{-# LANGUAGE RecordWildCards #-}
module LambdaCube.GL.Util (
    queryUniforms,
    queryStreams,
    mkUniformSetter,
    setUniform,
    setVertexAttrib,
    compileShader,
    printProgramLog,
    glGetShaderiv1,
    glGetProgramiv1,
    Buffer(..),
    ArrayDesc(..),
    StreamSetter,
    streamToInputType,
    arrayTypeToGLType,
    comparisonFunctionToGLType,
    logicOperationToGLType,
    blendEquationToGLType,
    blendingFactorToGLType,
    checkGL,
    textureDataTypeToGLType,
    textureDataTypeToGLArityType,
    glGetIntegerv1,
    setSampler,
    checkFBO,
    compileSampler,
    compileTexture,
    primitiveToFetchPrimitive,
    primitiveToGLType,
    inputTypeToTextureTarget
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List as L
import Foreign
import Foreign.C.String
import qualified Data.Vector as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.GL.Core33
import LambdaCube.Linear
import LambdaCube.IR
import LambdaCube.PipelineSchema
import LambdaCube.GL.Type

setSampler :: GLint -> Int32 -> IO ()
setSampler i v = glUniform1i i $ fromIntegral v

z2 = V2 0 0 :: V2F
z3 = V3 0 0 0 :: V3F
z4 = V4 0 0 0 0 :: V4F

-- uniform functions
queryUniforms :: GLuint -> IO (Map String GLint, Map String InputType)
queryUniforms po = do
    ul <- getNameTypeSize po glGetActiveUniform glGetUniformLocation GL_ACTIVE_UNIFORMS GL_ACTIVE_UNIFORM_MAX_LENGTH
    let uNames = [n | (n,_,_,_) <- ul]
        uTypes = [fromGLType (e,s) | (_,_,e,s) <- ul]
        uLocation = [i | (_,i,_,_) <- ul]
    return $! (Map.fromList $! zip uNames uLocation, Map.fromList $! zip uNames uTypes)

b2w :: Bool -> GLuint
b2w True = 1
b2w False = 0

mkUniformSetter :: InputType -> IO (GLUniform, InputSetter)
mkUniformSetter t@Bool  = do {r <- newIORef 0;                            return $! (GLUniform t r, SBool $!  writeIORef r . b2w)}
mkUniformSetter t@V2B   = do {r <- newIORef (V2 0 0);                     return $! (GLUniform t r, SV2B $!   writeIORef r . fmap b2w)}
mkUniformSetter t@V3B   = do {r <- newIORef (V3 0 0 0);                   return $! (GLUniform t r, SV3B $!   writeIORef r . fmap b2w)}
mkUniformSetter t@V4B   = do {r <- newIORef (V4 0 0 0 0);                 return $! (GLUniform t r, SV4B $!   writeIORef r . fmap b2w)}
mkUniformSetter t@Word  = do {r <- newIORef 0;                            return $! (GLUniform t r, SWord $!  writeIORef r)}
mkUniformSetter t@V2U   = do {r <- newIORef (V2 0 0);                     return $! (GLUniform t r, SV2U $!   writeIORef r)}
mkUniformSetter t@V3U   = do {r <- newIORef (V3 0 0 0);                   return $! (GLUniform t r, SV3U $!   writeIORef r)}
mkUniformSetter t@V4U   = do {r <- newIORef (V4 0 0 0 0);                 return $! (GLUniform t r, SV4U $!   writeIORef r)}
mkUniformSetter t@Int   = do {r <- newIORef 0;                            return $! (GLUniform t r, SInt $!   writeIORef r)}
mkUniformSetter t@V2I   = do {r <- newIORef (V2 0 0);                     return $! (GLUniform t r, SV2I $!   writeIORef r)}
mkUniformSetter t@V3I   = do {r <- newIORef (V3 0 0 0);                   return $! (GLUniform t r, SV3I $!   writeIORef r)}
mkUniformSetter t@V4I   = do {r <- newIORef (V4 0 0 0 0);                 return $! (GLUniform t r, SV4I $!   writeIORef r)}
mkUniformSetter t@Float = do {r <- newIORef 0;                            return $! (GLUniform t r, SFloat $! writeIORef r)}
mkUniformSetter t@V2F   = do {r <- newIORef (V2 0 0);                     return $! (GLUniform t r, SV2F $!   writeIORef r)}
mkUniformSetter t@V3F   = do {r <- newIORef (V3 0 0 0);                   return $! (GLUniform t r, SV3F $!   writeIORef r)}
mkUniformSetter t@V4F   = do {r <- newIORef (V4 0 0 0 0);                 return $! (GLUniform t r, SV4F $!   writeIORef r)}
mkUniformSetter t@M22F  = do {r <- newIORef (V2 z2 z2);                   return $! (GLUniform t r, SM22F $!  writeIORef r)}
mkUniformSetter t@M23F  = do {r <- newIORef (V3 z2 z2 z2);                return $! (GLUniform t r, SM23F $!  writeIORef r)}
mkUniformSetter t@M24F  = do {r <- newIORef (V4 z2 z2 z2 z2);             return $! (GLUniform t r, SM24F $!  writeIORef r)}
mkUniformSetter t@M32F  = do {r <- newIORef (V2 z3 z3);                   return $! (GLUniform t r, SM32F $!  writeIORef r)}
mkUniformSetter t@M33F  = do {r <- newIORef (V3 z3 z3 z3);                return $! (GLUniform t r, SM33F $!  writeIORef r)}
mkUniformSetter t@M34F  = do {r <- newIORef (V4 z3 z3 z3 z3);             return $! (GLUniform t r, SM34F $!  writeIORef r)}
mkUniformSetter t@M42F  = do {r <- newIORef (V2 z4 z4);                   return $! (GLUniform t r, SM42F $!  writeIORef r)}
mkUniformSetter t@M43F  = do {r <- newIORef (V3 z4 z4 z4);                return $! (GLUniform t r, SM43F $!  writeIORef r)}
mkUniformSetter t@M44F  = do {r <- newIORef (V4 z4 z4 z4 z4);             return $! (GLUniform t r, SM44F $!  writeIORef r)}
mkUniformSetter t@FTexture2D = do {r <- newIORef (TextureData 0);         return $! (GLUniform t r, SFTexture2D $! writeIORef r)}

-- sets value based uniforms only (does not handle textures)
setUniform :: Storable a => GLint -> InputType -> IORef a -> IO ()
setUniform i ty ref = do
    v <- readIORef ref
    let false = fromIntegral GL_FALSE
    with v $ \p -> case ty of
        Bool        -> glUniform1uiv i 1 (castPtr p)
        V2B         -> glUniform2uiv i 1 (castPtr p)
        V3B         -> glUniform3uiv i 1 (castPtr p)
        V4B         -> glUniform4uiv i 1 (castPtr p)
        Word        -> glUniform1uiv i 1 (castPtr p)
        V2U         -> glUniform2uiv i 1 (castPtr p)
        V3U         -> glUniform3uiv i 1 (castPtr p)
        V4U         -> glUniform4uiv i 1 (castPtr p)
        Int         -> glUniform1iv i 1 (castPtr p)
        V2I         -> glUniform2iv i 1 (castPtr p)
        V3I         -> glUniform3iv i 1 (castPtr p)
        V4I         -> glUniform4iv i 1 (castPtr p)
        Float       -> glUniform1fv i 1 (castPtr p)
        V2F         -> glUniform2fv i 1 (castPtr p)
        V3F         -> glUniform3fv i 1 (castPtr p)
        V4F         -> glUniform4fv i 1 (castPtr p)
        M22F        -> glUniformMatrix2fv   i 1 false (castPtr p)
        M23F        -> glUniformMatrix2x3fv i 1 false (castPtr p)
        M24F        -> glUniformMatrix2x4fv i 1 false (castPtr p)
        M32F        -> glUniformMatrix3x2fv i 1 false (castPtr p)
        M33F        -> glUniformMatrix3fv   i 1 false (castPtr p)
        M34F        -> glUniformMatrix3x4fv i 1 false (castPtr p)
        M42F        -> glUniformMatrix4x2fv i 1 false (castPtr p)
        M43F        -> glUniformMatrix4x3fv i 1 false (castPtr p)
        M44F        -> glUniformMatrix4fv   i 1 false (castPtr p)
        FTexture2D  -> return () --putStrLn $ "TODO: setUniform FTexture2D"
        _   -> fail $ "internal error (setUniform)! - " ++ show ty

-- attribute functions
queryStreams :: GLuint -> IO (Map String GLuint, Map String InputType)
queryStreams po = do
    al <- getNameTypeSize po glGetActiveAttrib glGetAttribLocation GL_ACTIVE_ATTRIBUTES GL_ACTIVE_ATTRIBUTE_MAX_LENGTH
    let aNames = [n | (n,_,_,_) <- al]
        aTypes = [fromGLType (e,s) | (_,_,e,s) <- al]
        aLocation = [fromIntegral i | (_,i,_,_) <- al]
    return $! (Map.fromList $! zip aNames aLocation, Map.fromList $! zip aNames aTypes)

arrayTypeToGLType :: ArrayType -> GLenum
arrayTypeToGLType a = case a of
    ArrWord8    -> GL_UNSIGNED_BYTE
    ArrWord16   -> GL_UNSIGNED_SHORT
    ArrWord32   -> GL_UNSIGNED_INT
    ArrInt8     -> GL_BYTE
    ArrInt16    -> GL_SHORT
    ArrInt32    -> GL_INT
    ArrFloat    -> GL_FLOAT
    ArrHalf     -> GL_HALF_FLOAT

setVertexAttrib :: GLuint -> Stream Buffer -> IO ()
setVertexAttrib i val = case val of
    ConstWord v     -> with v $! \p -> glVertexAttribI1uiv i $! castPtr p
    ConstV2U v      -> with v $! \p -> glVertexAttribI2uiv i $! castPtr p
    ConstV3U v      -> with v $! \p -> glVertexAttribI3uiv i $! castPtr p
    ConstV4U v      -> with v $! \p -> glVertexAttribI4uiv i $! castPtr p
    ConstInt v      -> with v $! \p -> glVertexAttribI1iv i $! castPtr p
    ConstV2I v      -> with v $! \p -> glVertexAttribI2iv i $! castPtr p
    ConstV3I v      -> with v $! \p -> glVertexAttribI3iv i $! castPtr p
    ConstV4I v      -> with v $! \p -> glVertexAttribI4iv i $! castPtr p
    ConstFloat v    -> setAFloat i v
    ConstV2F v      -> setAV2F i v
    ConstV3F v      -> setAV3F i v
    ConstV4F v      -> setAV4F i v
    ConstM22F (V2 x y)      -> setAV2F i x >> setAV2F (i+1) y
    ConstM23F (V3 x y z)    -> setAV2F i x >> setAV2F (i+1) y >> setAV2F (i+2) z
    ConstM24F (V4 x y z w)  -> setAV2F i x >> setAV2F (i+1) y >> setAV2F (i+2) z >> setAV2F (i+3) w
    ConstM32F (V2 x y)      -> setAV3F i x >> setAV3F (i+1) y
    ConstM33F (V3 x y z)    -> setAV3F i x >> setAV3F (i+1) y >> setAV3F (i+2) z
    ConstM34F (V4 x y z w)  -> setAV3F i x >> setAV3F (i+1) y >> setAV3F (i+2) z >> setAV3F (i+3) w
    ConstM42F (V2 x y)      -> setAV4F i x >> setAV4F (i+1) y
    ConstM43F (V3 x y z)    -> setAV4F i x >> setAV4F (i+1) y >> setAV4F (i+2) z
    ConstM44F (V4 x y z w)  -> setAV4F i x >> setAV4F (i+1) y >> setAV4F (i+2) z >> setAV4F (i+3) w
    _ -> fail "internal error (setVertexAttrib)!"

setAFloat :: GLuint -> Float -> IO ()
setAV2F   :: GLuint -> V2F -> IO ()
setAV3F   :: GLuint -> V3F -> IO ()
setAV4F   :: GLuint -> V4F -> IO ()
setAFloat i v = with v $! \p -> glVertexAttrib1fv i $! castPtr p
setAV2F i v   = with v $! \p -> glVertexAttrib2fv i $! castPtr p
setAV3F i v   = with v $! \p -> glVertexAttrib3fv i $! castPtr p
setAV4F i v   = with v $! \p -> glVertexAttrib4fv i $! castPtr p

-- result list: [(name string,location,gl type,component count)]
getNameTypeSize :: GLuint -> (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
                   -> (GLuint -> Ptr GLchar -> IO GLint) -> GLenum -> GLenum -> IO [(String,GLint,GLenum,GLint)]
getNameTypeSize o f g enum enumLen = do
    nameLen <- glGetProgramiv1 enumLen o
    allocaArray (fromIntegral nameLen) $! \namep -> alloca $! \sizep -> alloca $! \typep -> do
        n <- glGetProgramiv1 enum o
        forM [0..n-1] $! \i -> f o (fromIntegral i) (fromIntegral nameLen) nullPtr sizep typep namep >>
            (,,,) <$> peekCString (castPtr namep) <*> g o namep <*> peek typep <*> peek sizep

fromGLType :: (GLenum,GLint) -> InputType
fromGLType (t,1)
    | t == GL_BOOL              = Bool
    | t == GL_BOOL_VEC2         = V2B
    | t == GL_BOOL_VEC3         = V3B
    | t == GL_BOOL_VEC4         = V4B
    | t == GL_UNSIGNED_INT      = Word
    | t == GL_UNSIGNED_INT_VEC2 = V2U
    | t == GL_UNSIGNED_INT_VEC3 = V3U
    | t == GL_UNSIGNED_INT_VEC4 = V4U
    | t == GL_INT               = Int
    | t == GL_INT_VEC2          = V2I
    | t == GL_INT_VEC3          = V3I
    | t == GL_INT_VEC4          = V4I
    | t == GL_FLOAT             = Float
    | t == GL_FLOAT_VEC2        = V2F
    | t == GL_FLOAT_VEC3        = V3F
    | t == GL_FLOAT_VEC4        = V4F
    | t == GL_FLOAT_MAT2        = M22F
    | t == GL_FLOAT_MAT2x3      = M23F
    | t == GL_FLOAT_MAT2x4      = M24F
    | t == GL_FLOAT_MAT3x2      = M32F
    | t == GL_FLOAT_MAT3        = M33F
    | t == GL_FLOAT_MAT3x4      = M34F
    | t == GL_FLOAT_MAT4x2      = M42F
    | t == GL_FLOAT_MAT4x3      = M43F
    | t == GL_FLOAT_MAT4        = M44F
    | t == GL_SAMPLER_1D_ARRAY_SHADOW                   = STexture1DArray
    | t == GL_SAMPLER_1D_SHADOW                         = STexture1D
    | t == GL_SAMPLER_2D_ARRAY_SHADOW                   = STexture2DArray
    | t == GL_SAMPLER_2D_RECT_SHADOW                    = STexture2DRect
    | t == GL_SAMPLER_2D_SHADOW                         = STexture2D
    | t == GL_SAMPLER_CUBE_SHADOW                       = STextureCube
    | t == GL_INT_SAMPLER_1D                            = ITexture1D
    | t == GL_INT_SAMPLER_1D_ARRAY                      = ITexture1DArray
    | t == GL_INT_SAMPLER_2D                            = ITexture2D
    | t == GL_INT_SAMPLER_2D_ARRAY                      = ITexture2DArray
    | t == GL_INT_SAMPLER_2D_MULTISAMPLE                = ITexture2DMS
    | t == GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY          = ITexture2DMSArray
    | t == GL_INT_SAMPLER_2D_RECT                       = ITexture2DRect
    | t == GL_INT_SAMPLER_3D                            = ITexture3D
    | t == GL_INT_SAMPLER_BUFFER                        = ITextureBuffer
    | t == GL_INT_SAMPLER_CUBE                          = ITextureCube
    | t == GL_SAMPLER_1D                                = FTexture1D
    | t == GL_SAMPLER_1D_ARRAY                          = FTexture1DArray
    | t == GL_SAMPLER_2D                                = FTexture2D
    | t == GL_SAMPLER_2D_ARRAY                          = FTexture2DArray
    | t == GL_SAMPLER_2D_MULTISAMPLE                    = FTexture2DMS
    | t == GL_SAMPLER_2D_MULTISAMPLE_ARRAY              = FTexture2DMSArray
    | t == GL_SAMPLER_2D_RECT                           = FTexture2DRect
    | t == GL_SAMPLER_3D                                = FTexture3D
    | t == GL_SAMPLER_BUFFER                            = FTextureBuffer
    | t == GL_SAMPLER_CUBE                              = FTextureCube
    | t == GL_UNSIGNED_INT_SAMPLER_1D                   = UTexture1D
    | t == GL_UNSIGNED_INT_SAMPLER_1D_ARRAY             = UTexture1DArray
    | t == GL_UNSIGNED_INT_SAMPLER_2D                   = UTexture2D
    | t == GL_UNSIGNED_INT_SAMPLER_2D_ARRAY             = UTexture2DArray
    | t == GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE       = UTexture2DMS
    | t == GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = UTexture2DMSArray
    | t == GL_UNSIGNED_INT_SAMPLER_2D_RECT              = UTexture2DRect
    | t == GL_UNSIGNED_INT_SAMPLER_3D                   = UTexture3D
    | t == GL_UNSIGNED_INT_SAMPLER_BUFFER               = UTextureBuffer
    | t == GL_UNSIGNED_INT_SAMPLER_CUBE                 = UTextureCube
    | otherwise = error "Failed fromGLType"
fromGLUniformType _ = error "Failed fromGLType"

printShaderLog :: GLuint -> IO String
printShaderLog o = do
  i <- glGetShaderiv1 GL_INFO_LOG_LENGTH o
  case (i > 0) of
    False -> return ""
    True -> do
      alloca $ \sizePtr -> allocaArray (fromIntegral i) $! \ps -> do
        glGetShaderInfoLog o (fromIntegral i) sizePtr ps
        size <- peek sizePtr
        log <- peekCStringLen (castPtr ps, fromIntegral size)
        putStrLn log
        return log

glGetShaderiv1 :: GLenum -> GLuint -> IO GLint
glGetShaderiv1 pname o = alloca $! \pi -> glGetShaderiv o pname pi >> peek pi

glGetProgramiv1 :: GLenum -> GLuint -> IO GLint
glGetProgramiv1 pname o = alloca $! \pi -> glGetProgramiv o pname pi >> peek pi

printProgramLog :: GLuint -> IO String
printProgramLog o = do
  i <- glGetProgramiv1 GL_INFO_LOG_LENGTH o
  case (i > 0) of
    False -> return ""
    True -> do
      alloca $ \sizePtr -> allocaArray (fromIntegral i) $! \ps -> do
        glGetProgramInfoLog o (fromIntegral i) sizePtr ps
        size <- peek sizePtr
        log <- peekCStringLen (castPtr ps, fromIntegral size)
        unless (null log) $ putStrLn log
        return log

compileShader :: GLuint -> [String] -> IO ()
compileShader o srcl = withMany withCString srcl $! \l -> withArray l $! \p -> do
    glShaderSource o (fromIntegral $! length srcl) (castPtr p) nullPtr
    glCompileShader o
    log <- printShaderLog o
    status <- glGetShaderiv1 GL_COMPILE_STATUS o
    when (status /= fromIntegral GL_TRUE) $ fail $ unlines ["compileShader failed:",log]

checkGL :: IO String
checkGL = do
    let f e | e == GL_INVALID_ENUM                  = "INVALID_ENUM"
            | e == GL_INVALID_VALUE                 = "INVALID_VALUE"
            | e == GL_INVALID_OPERATION             = "INVALID_OPERATION"
            | e == GL_INVALID_FRAMEBUFFER_OPERATION = "INVALID_FRAMEBUFFER_OPERATION"
            | e == GL_OUT_OF_MEMORY                 = "OUT_OF_MEMORY"
            | e == GL_NO_ERROR                      = "OK"
            | otherwise                             = "Unknown error"
    e <- glGetError
    return $ f e

streamToInputType :: Stream Buffer -> InputType
streamToInputType s = case s of
    ConstWord  _    -> Word
    ConstV2U   _    -> V2U
    ConstV3U   _    -> V3U
    ConstV4U   _    -> V4U
    ConstInt   _    -> Int
    ConstV2I   _    -> V2I
    ConstV3I   _    -> V3I
    ConstV4I   _    -> V4I
    ConstFloat _    -> Float
    ConstV2F   _    -> V2F
    ConstV3F   _    -> V3F
    ConstV4F   _    -> V4F
    ConstM22F  _    -> M22F
    ConstM23F  _    -> M23F
    ConstM24F  _    -> M24F
    ConstM32F  _    -> M32F
    ConstM33F  _    -> M33F
    ConstM34F  _    -> M34F
    ConstM42F  _    -> M42F
    ConstM43F  _    -> M43F
    ConstM44F  _    -> M44F
    Stream t (Buffer a _) i _ _
        | 0 <= i && i < V.length a &&
          if elem t integralTypes then elem at integralArrTypes else True
        -> fromStreamType t
        | otherwise -> error "streamToInputType failed"
      where
        at = arrType $! (a V.! i)
        integralTypes    = [Attribute_Word, Attribute_V2U, Attribute_V3U, Attribute_V4U, Attribute_Int, Attribute_V2I, Attribute_V3I, Attribute_V4I]
        integralArrTypes = [ArrWord8, ArrWord16, ArrWord32, ArrInt8, ArrInt16, ArrInt32]

comparisonFunctionToGLType :: ComparisonFunction -> GLenum
comparisonFunctionToGLType a = case a of
    Always      -> GL_ALWAYS
    Equal       -> GL_EQUAL
    Gequal      -> GL_GEQUAL
    Greater     -> GL_GREATER
    Lequal      -> GL_LEQUAL
    Less        -> GL_LESS
    Never       -> GL_NEVER
    Notequal    -> GL_NOTEQUAL

logicOperationToGLType :: LogicOperation -> GLenum
logicOperationToGLType a = case a of
    And             -> GL_AND
    AndInverted     -> GL_AND_INVERTED
    AndReverse      -> GL_AND_REVERSE
    Clear           -> GL_CLEAR
    Copy            -> GL_COPY
    CopyInverted    -> GL_COPY_INVERTED
    Equiv           -> GL_EQUIV
    Invert          -> GL_INVERT
    Nand            -> GL_NAND
    Noop            -> GL_NOOP
    Nor             -> GL_NOR
    Or              -> GL_OR
    OrInverted      -> GL_OR_INVERTED
    OrReverse       -> GL_OR_REVERSE
    Set             -> GL_SET
    Xor             -> GL_XOR

blendEquationToGLType :: BlendEquation -> GLenum
blendEquationToGLType a = case a of
    FuncAdd             -> GL_FUNC_ADD
    FuncReverseSubtract -> GL_FUNC_REVERSE_SUBTRACT
    FuncSubtract        -> GL_FUNC_SUBTRACT
    Max                 -> GL_MAX
    Min                 -> GL_MIN

blendingFactorToGLType :: BlendingFactor -> GLenum
blendingFactorToGLType a = case a of
    ConstantAlpha           -> GL_CONSTANT_ALPHA
    ConstantColor           -> GL_CONSTANT_COLOR
    DstAlpha                -> GL_DST_ALPHA
    DstColor                -> GL_DST_COLOR
    One                     -> GL_ONE
    OneMinusConstantAlpha   -> GL_ONE_MINUS_CONSTANT_ALPHA
    OneMinusConstantColor   -> GL_ONE_MINUS_CONSTANT_COLOR
    OneMinusDstAlpha        -> GL_ONE_MINUS_DST_ALPHA
    OneMinusDstColor        -> GL_ONE_MINUS_DST_COLOR
    OneMinusSrcAlpha        -> GL_ONE_MINUS_SRC_ALPHA
    OneMinusSrcColor        -> GL_ONE_MINUS_SRC_COLOR
    SrcAlpha                -> GL_SRC_ALPHA
    SrcAlphaSaturate        -> GL_SRC_ALPHA_SATURATE
    SrcColor                -> GL_SRC_COLOR
    Zero                    -> GL_ZERO

-- XXX: we need to extend IR.TextureDescriptor to carry component bit depth
--      if we want to avoid making arbitrary decisions here
textureDataTypeToGLType :: ImageSemantic -> TextureDataType -> GLenum
textureDataTypeToGLType Color a = case a of
    FloatT Red  -> GL_R32F
    IntT   Red  -> GL_R32I
    WordT  Red  -> GL_R32UI
    FloatT RG   -> GL_RG32F
    IntT   RG   -> GL_RG32I
    WordT  RG   -> GL_RG32UI
    FloatT RGBA -> GL_RGBA32F
    IntT   RGBA -> GL_RGBA8I
    WordT  RGBA -> GL_RGBA8UI
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a
textureDataTypeToGLType Depth a = case a of
    FloatT Red  -> GL_DEPTH_COMPONENT32F
    WordT  Red  -> GL_DEPTH_COMPONENT32
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a
textureDataTypeToGLType Stencil a = case a of
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a

textureDataTypeToGLArityType :: ImageSemantic -> TextureDataType -> GLenum
textureDataTypeToGLArityType Color a = case a of
    FloatT Red  -> GL_RED
    IntT   Red  -> GL_RED_INTEGER
    WordT  Red  -> GL_RED_INTEGER
    FloatT RG   -> GL_RG
    IntT   RG   -> GL_RG_INTEGER
    WordT  RG   -> GL_RG_INTEGER
    FloatT RGBA -> GL_RGBA
    IntT   RGBA -> GL_RGBA_INTEGER
    WordT  RGBA -> GL_RGBA_INTEGER
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a
textureDataTypeToGLArityType Depth a = case a of
    FloatT Red  -> GL_DEPTH_COMPONENT
    WordT  Red  -> GL_DEPTH_COMPONENT
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a
textureDataTypeToGLArityType Stencil a = case a of
    a           -> error $ "FIXME: This texture format is not yet supported" ++ show a
{-
Texture and renderbuffer color formats (R):
    R11F_G11F_B10F
    R16
    R16F
    R16I
    R16UI
    R32F
    R32I
    R32UI
    R8
    R8I
    R8UI
    RG16
    RG16F
    RG16I
    RG16UI
    RG32F
    RG32I
    RG32UI
    RG8
    RG8I
    RG8UI
    RGB10_A2
    RGB10_A2UI
    RGBA16
    RGBA16F
    RGBA16I
    RGBA16UI
    RGBA32F
    RGBA32I
    RGBA32UI
    RGBA8
    RGBA8I
    RGBA8UI
    SRGB8_ALPHA8
-}

glGetIntegerv1 :: GLenum -> IO GLint
glGetIntegerv1 e = alloca $ \pi -> glGetIntegerv e pi >> peek pi

checkFBO :: IO String
checkFBO = do
    let f e | e == GL_FRAMEBUFFER_UNDEFINED                 = "FRAMEBUFFER_UNDEFINED"
            | e == GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT     = "FRAMEBUFFER_INCOMPLETE_ATTACHMENT"
            | e == GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER    = "FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER"
            | e == GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER    = "FRAMEBUFFER_INCOMPLETE_READ_BUFFER"
            | e == GL_FRAMEBUFFER_UNSUPPORTED               = "FRAMEBUFFER_UNSUPPORTED"
            | e == GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE    = "FRAMEBUFFER_INCOMPLETE_MULTISAMPLE"
            | e == GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS  = "FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS"
            | e == GL_FRAMEBUFFER_COMPLETE                  = "FRAMEBUFFER_COMPLETE"
            | otherwise                                     = "Unknown error"
    e <- glCheckFramebufferStatus GL_DRAW_FRAMEBUFFER
    return $ f e

filterToGLType :: Filter -> GLenum
filterToGLType a = case a of
    Nearest                 -> GL_NEAREST
    Linear                  -> GL_LINEAR
    NearestMipmapNearest    -> GL_NEAREST_MIPMAP_NEAREST
    NearestMipmapLinear     -> GL_NEAREST_MIPMAP_LINEAR
    LinearMipmapNearest     -> GL_LINEAR_MIPMAP_NEAREST
    LinearMipmapLinear      -> GL_LINEAR_MIPMAP_LINEAR

edgeModeToGLType :: EdgeMode -> GLenum
edgeModeToGLType a = case a of
    Repeat          -> GL_REPEAT
    MirroredRepeat  -> GL_MIRRORED_REPEAT
    ClampToEdge     -> GL_CLAMP_TO_EDGE
    ClampToBorder   -> GL_CLAMP_TO_BORDER

data ParameterSetup
  = ParameterSetup
  { setParameteri     :: GLenum -> GLint -> IO ()
  , setParameterfv    :: GLenum -> Ptr GLfloat -> IO ()
  , setParameterIiv   :: GLenum -> Ptr GLint -> IO ()
  , setParameterIuiv  :: GLenum -> Ptr GLuint -> IO ()
  , setParameterf     :: GLenum -> GLfloat -> IO ()
  }

setTextureSamplerParameters :: GLenum -> SamplerDescriptor -> IO ()
setTextureSamplerParameters target = setParameters $ ParameterSetup
  { setParameteri     = glTexParameteri target
  , setParameterfv    = glTexParameterfv target
  , setParameterIiv   = glTexParameterIiv target
  , setParameterIuiv  = glTexParameterIuiv target
  , setParameterf     = glTexParameterf target
  }

setSamplerParameters :: GLuint -> SamplerDescriptor -> IO ()
setSamplerParameters samplerObj = setParameters $ ParameterSetup
  { setParameteri     = glSamplerParameteri samplerObj
  , setParameterfv    = glSamplerParameterfv samplerObj
  , setParameterIiv   = glSamplerParameterIiv samplerObj
  , setParameterIuiv  = glSamplerParameterIuiv samplerObj
  , setParameterf     = glSamplerParameterf samplerObj
  }

setParameters :: ParameterSetup -> SamplerDescriptor -> IO ()
setParameters ParameterSetup{..} s = do
    setParameteri GL_TEXTURE_WRAP_S $ fromIntegral $ edgeModeToGLType $ samplerWrapS s
    case samplerWrapT s of
        Nothing -> return ()
        Just a  -> setParameteri GL_TEXTURE_WRAP_T $ fromIntegral $ edgeModeToGLType a
    case samplerWrapR s of
        Nothing -> return ()
        Just a  -> setParameteri GL_TEXTURE_WRAP_R $ fromIntegral $ edgeModeToGLType a
    setParameteri GL_TEXTURE_MIN_FILTER $ fromIntegral $ filterToGLType $ samplerMinFilter s
    setParameteri GL_TEXTURE_MAG_FILTER $ fromIntegral $ filterToGLType $ samplerMagFilter s

    let setBColorV4F a = with a $ \p -> setParameterfv GL_TEXTURE_BORDER_COLOR $ castPtr p
        setBColorV4I a = with a $ \p -> setParameterIiv GL_TEXTURE_BORDER_COLOR $ castPtr p
        setBColorV4U a = with a $ \p -> setParameterIuiv GL_TEXTURE_BORDER_COLOR $ castPtr p
    case samplerBorderColor s of
        -- float, word, int, red, rg, rgb, rgba
        VFloat a        -> setBColorV4F $ V4 a 0 0 0
        VV2F (V2 a b)   -> setBColorV4F $ V4 a b 0 0
        VV3F (V3 a b c) -> setBColorV4F $ V4 a b c 0
        VV4F a          -> setBColorV4F a

        VInt a          -> setBColorV4I $ V4 a 0 0 0
        VV2I (V2 a b)   -> setBColorV4I $ V4 a b 0 0
        VV3I (V3 a b c) -> setBColorV4I $ V4 a b c 0
        VV4I a          -> setBColorV4I a

        VWord a         -> setBColorV4U $ V4 a 0 0 0
        VV2U (V2 a b)   -> setBColorV4U $ V4 a b 0 0
        VV3U (V3 a b c) -> setBColorV4U $ V4 a b c 0
        VV4U a          -> setBColorV4U a
        _ -> fail "internal error (setTextureSamplerParameters)!"

    case samplerMinLod s of
        Nothing -> return ()
        Just a  -> setParameterf GL_TEXTURE_MIN_LOD $ realToFrac a
    case samplerMaxLod s of
        Nothing -> return ()
        Just a  -> setParameterf GL_TEXTURE_MAX_LOD $ realToFrac a
    setParameterf GL_TEXTURE_LOD_BIAS $ realToFrac $ samplerLodBias s
    case samplerCompareFunc s of
        Nothing -> setParameteri GL_TEXTURE_COMPARE_MODE $ fromIntegral GL_NONE
        Just a  -> do
            setParameteri GL_TEXTURE_COMPARE_MODE $ fromIntegral GL_COMPARE_REF_TO_TEXTURE
            setParameteri GL_TEXTURE_COMPARE_FUNC $ fromIntegral $ comparisonFunctionToGLType a

compileSampler :: SamplerDescriptor -> IO GLSampler
compileSampler s = do
  so <- alloca $! \po -> glGenSamplers 1 po >> peek po
  setSamplerParameters so s
  return $ GLSampler
    { glSamplerObject = so
    }

compileTexture :: TextureDescriptor -> IO GLTexture
compileTexture txDescriptor = do
    to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
    let TextureDescriptor
            { textureType       = txType
            , textureSize       = txSize
            , textureSemantic   = txSemantic
            , textureSampler    = txSampler
            , textureBaseLevel  = txBaseLevel
            , textureMaxLevel   = txMaxLevel
            } = txDescriptor

        txSetup txTarget dTy = do
            let internalFormat  = fromIntegral $ textureDataTypeToGLType txSemantic dTy
                dataFormat      = fromIntegral $ textureDataTypeToGLArityType txSemantic dTy
            glBindTexture txTarget to
            glTexParameteri txTarget GL_TEXTURE_BASE_LEVEL $ fromIntegral txBaseLevel
            glTexParameteri txTarget GL_TEXTURE_MAX_LEVEL $ fromIntegral txMaxLevel
            setTextureSamplerParameters txTarget txSampler
            return (internalFormat,dataFormat)

        mipSize 0 x = [x]
        mipSize n x = x : mipSize (n-1) (x `div` 2)
        mipS = mipSize (txMaxLevel - txBaseLevel)
        levels = [txBaseLevel..txMaxLevel]
    target <- case txType of
        Texture1D dTy layerCnt -> do
            let VWord txW = txSize
                txTarget = if layerCnt > 1 then GL_TEXTURE_1D_ARRAY else GL_TEXTURE_1D
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip levels (mipS txW)) $ \(l,w) -> case layerCnt > 1 of
                True    -> glTexImage2D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral layerCnt) 0 dataFormat GL_UNSIGNED_BYTE nullPtr
                False   -> glTexImage1D txTarget (fromIntegral l) internalFormat (fromIntegral w) 0 dataFormat GL_UNSIGNED_BYTE nullPtr
            return txTarget
        Texture2D dTy layerCnt -> do
            let VV2U (V2 txW txH) = txSize
                txTarget = if layerCnt > 1 then GL_TEXTURE_2D_ARRAY else GL_TEXTURE_2D
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip3 levels (mipS txW) (mipS txH)) $ \(l,w,h) -> case layerCnt > 1 of
                True    -> glTexImage3D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) (fromIntegral layerCnt) 0 dataFormat GL_UNSIGNED_BYTE nullPtr
                False   -> glTexImage2D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat GL_UNSIGNED_BYTE nullPtr
            return txTarget
        Texture3D dTy -> do
            let VV3U (V3 txW txH txD) = txSize
                txTarget = GL_TEXTURE_3D
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip4 levels (mipS txW) (mipS txH) (mipS txD)) $ \(l,w,h,d) ->
                glTexImage3D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) (fromIntegral d) 0 dataFormat GL_UNSIGNED_BYTE nullPtr
            return txTarget
        TextureCube dTy -> do
            let VV2U (V2 txW txH) = txSize
                txTarget = GL_TEXTURE_CUBE_MAP
                targets =
                    [ GL_TEXTURE_CUBE_MAP_POSITIVE_X 
                    , GL_TEXTURE_CUBE_MAP_NEGATIVE_X
                    , GL_TEXTURE_CUBE_MAP_POSITIVE_Y
                    , GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
                    , GL_TEXTURE_CUBE_MAP_POSITIVE_Z
                    , GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
                    ]
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip3 levels (mipS txW) (mipS txH)) $ \(l,w,h) -> 
                forM_ targets $ \t -> glTexImage2D t (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat GL_UNSIGNED_BYTE nullPtr
            return txTarget
        TextureRect dTy -> do
            let VV2U (V2 txW txH) = txSize
                txTarget = GL_TEXTURE_RECTANGLE
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            forM_ (zip3 levels (mipS txW) (mipS txH)) $ \(l,w,h) -> 
                glTexImage2D txTarget (fromIntegral l) internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat GL_UNSIGNED_BYTE nullPtr
            return txTarget
        Texture2DMS dTy layerCnt sampleCount isFixedLocations -> do
            let VV2U (V2 w h)   = txSize
                txTarget        = if layerCnt > 1 then GL_TEXTURE_2D_MULTISAMPLE_ARRAY else GL_TEXTURE_2D_MULTISAMPLE
                isFixed         = fromIntegral $ if isFixedLocations then GL_TRUE else GL_FALSE
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            case layerCnt > 1 of
                True    -> glTexImage3DMultisample txTarget (fromIntegral sampleCount) internalFormat (fromIntegral w) (fromIntegral h) (fromIntegral layerCnt) isFixed
                False   -> glTexImage2DMultisample txTarget (fromIntegral sampleCount) internalFormat (fromIntegral w) (fromIntegral h) isFixed
            return txTarget
        TextureBuffer dTy -> do
            fail "internal error: buffer texture is not supported yet"
            -- TODO
            let VV2U (V2 w h)   = txSize
                txTarget        = GL_TEXTURE_2D
            (internalFormat,dataFormat) <- txSetup txTarget dTy
            glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat GL_UNSIGNED_BYTE nullPtr
            return txTarget
    return $ GLTexture
        { glTextureObject   = to
        , glTextureTarget   = target
        }

primitiveToFetchPrimitive :: Primitive -> FetchPrimitive
primitiveToFetchPrimitive prim = case prim of
    TriangleStrip           -> Triangles
    TriangleList            -> Triangles
    TriangleFan             -> Triangles
    LineStrip               -> Lines
    LineList                -> Lines
    PointList               -> Points
    TriangleStripAdjacency  -> TrianglesAdjacency
    TriangleListAdjacency   -> TrianglesAdjacency
    LineStripAdjacency      -> LinesAdjacency
    LineListAdjacency       -> LinesAdjacency

primitiveToGLType :: Primitive -> GLenum
primitiveToGLType p = case p of
    TriangleStrip           -> GL_TRIANGLE_STRIP
    TriangleList            -> GL_TRIANGLES
    TriangleFan             -> GL_TRIANGLE_FAN
    LineStrip               -> GL_LINE_STRIP
    LineList                -> GL_LINES
    PointList               -> GL_POINTS
    TriangleStripAdjacency  -> GL_TRIANGLE_STRIP_ADJACENCY
    TriangleListAdjacency   -> GL_TRIANGLES_ADJACENCY
    LineStripAdjacency      -> GL_LINE_STRIP_ADJACENCY
    LineListAdjacency       -> GL_LINES_ADJACENCY

inputTypeToTextureTarget :: InputType -> GLenum
inputTypeToTextureTarget ty = case ty of
    STexture1D          -> GL_TEXTURE_1D
    STexture2D          -> GL_TEXTURE_2D
    STextureCube        -> GL_TEXTURE_CUBE_MAP
    STexture1DArray     -> GL_TEXTURE_1D_ARRAY
    STexture2DArray     -> GL_TEXTURE_2D_ARRAY
    STexture2DRect      -> GL_TEXTURE_RECTANGLE

    FTexture1D          -> GL_TEXTURE_1D
    FTexture2D          -> GL_TEXTURE_2D
    FTexture3D          -> GL_TEXTURE_3D
    FTextureCube        -> GL_TEXTURE_CUBE_MAP
    FTexture1DArray     -> GL_TEXTURE_1D_ARRAY
    FTexture2DArray     -> GL_TEXTURE_2D_ARRAY
    FTexture2DMS        -> GL_TEXTURE_2D_MULTISAMPLE
    FTexture2DMSArray   -> GL_TEXTURE_2D_MULTISAMPLE_ARRAY
    FTextureBuffer      -> GL_TEXTURE_BUFFER
    FTexture2DRect      -> GL_TEXTURE_RECTANGLE

    ITexture1D          -> GL_TEXTURE_1D
    ITexture2D          -> GL_TEXTURE_2D
    ITexture3D          -> GL_TEXTURE_3D
    ITextureCube        -> GL_TEXTURE_CUBE_MAP
    ITexture1DArray     -> GL_TEXTURE_1D_ARRAY
    ITexture2DArray     -> GL_TEXTURE_2D_ARRAY
    ITexture2DMS        -> GL_TEXTURE_2D_MULTISAMPLE
    ITexture2DMSArray   -> GL_TEXTURE_2D_MULTISAMPLE_ARRAY
    ITextureBuffer      -> GL_TEXTURE_BUFFER
    ITexture2DRect      -> GL_TEXTURE_RECTANGLE

    UTexture1D          -> GL_TEXTURE_1D
    UTexture2D          -> GL_TEXTURE_2D
    UTexture3D          -> GL_TEXTURE_3D
    UTextureCube        -> GL_TEXTURE_CUBE_MAP
    UTexture1DArray     -> GL_TEXTURE_1D_ARRAY
    UTexture2DArray     -> GL_TEXTURE_2D_ARRAY
    UTexture2DMS        -> GL_TEXTURE_2D_MULTISAMPLE
    UTexture2DMSArray   -> GL_TEXTURE_2D_MULTISAMPLE_ARRAY
    UTextureBuffer      -> GL_TEXTURE_BUFFER
    UTexture2DRect      -> GL_TEXTURE_RECTANGLE

    _ -> error "internal error (inputTypeToTextureTarget)!"
