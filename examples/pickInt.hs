{-# LANGUAGE PackageImports, LambdaCase, OverloadedStrings, StandaloneDeriving, ViewPatterns #-}
import           Control.Monad
import           Data.Aeson
import           Data.Vect                            (Mat4(..), Vec3(..), Vec4(..))
import           Graphics.GL.Core33                as GL
import           LambdaCube.GL                     as LambdaCubeGL
import           LambdaCube.GL.Mesh                as LambdaCubeGL
import           Text.Printf
import "GLFW-b"  Graphics.UI.GLFW                  as GLFW
import qualified Data.ByteString                   as SB
import qualified Data.Map                          as Map
import qualified Data.Vect                         as Vc
import qualified Data.Vector                       as V
import qualified Foreign                           as F
import qualified Foreign.C.Types                   as F
import qualified LambdaCube.GL.Type                as LC
import qualified LambdaCube.Linear                 as LCLin

----------------------------------------------------
--  See:  http://lambdacube3d.com/getting-started
----------------------------------------------------

screenDim :: (Int, Int)
screenDim = (800, 600)
(screenW, screenH) = screenDim

main :: IO ()
main = do
    Just pipePickDesc <- decodeStrict <$> SB.readFile "pickInt.json"
    Just pipeDrawDesc <- decodeStrict <$> SB.readFile "pickIntDraw.json"

    win <- initWindow "LambdaCube 3D integer picking" 800 600

    -- setup render data
    let inputSchema = makeSchema $ do
          defObjectArray "objects" Triangles $ do
            "position"  @: Attribute_V2F
            "id"        @: Attribute_Int
            "color"     @: Attribute_V4F
          defUniforms $ do
            "viewProj"  @: M44F

    storage <- LambdaCubeGL.allocStorage inputSchema

    -- upload geometry to GPU and add to pipeline input
    LambdaCubeGL.uploadMeshToGPU triangleA >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []
    LambdaCubeGL.uploadMeshToGPU triangleB >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []

    -- allocate GL pipeline
    pipePick <- LambdaCubeGL.allocRenderer pipePickDesc
    pipeDraw <- LambdaCubeGL.allocRenderer pipeDrawDesc
    errPick  <- LambdaCubeGL.setStorage pipePick storage
    errDraw  <- LambdaCubeGL.setStorage pipeDraw storage
    case (errPick, errDraw) of -- check schema compatibility
      (Just err, _) -> putStrLn err
      (_, Just err) -> putStrLn err
      (Nothing, Nothing) -> loop
        where loop = do
                -- update graphics input
                GLFW.getWindowSize win >>= \(w,h) -> LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
                LambdaCubeGL.updateUniforms storage $ do
                  let (x, y)    = (,) 0 0
                      cvpos     = Vec3 x (-y) 0
                      toScreen  = screenM screenW screenH
                  "viewProj" @= pure (mat4ToM44F $! (Vc.fromProjective $! Vc.translation cvpos) Vc..*. toScreen)

                let pickPoints =   -- should be fb 0            fb 1 (pick)
                      [ (0,   0)   --           black              0
                      , (200, 200) --         ..blue, ffff0000     2
                      , (600, 400) --         ..red,  ff0000ff     1
                      ] :: [(Int, Int)]

                -- render to render texture
                LambdaCubeGL.renderFrame pipePick
                case LC.glOutputs pipePick of
                  [LC.GLOutputRenderTexture (fromIntegral -> fbo) _rendTex] -> do
                    rtexPicks <- collectPicks fbo pickPoints
                    printPicks pickPoints rtexPicks
                  x -> error $ "Unexpected outputs: " <> show x

                -- render to framebuffer & pick
                LambdaCubeGL.renderFrame pipeDraw
                colorPicks <- collectPicks 0 pickPoints
                printPicks pickPoints colorPicks

                GLFW.swapBuffers win
                GLFW.pollEvents

                let keyIsPressed k = fmap (==KeyState'Pressed) $ GLFW.getKey win k
                escape <- keyIsPressed Key'Escape
                if escape then return () else loop
              collectPicks :: Int -> [(Int, Int)] -> IO [Int]
              collectPicks fb picks =
                forM picks $ (fromIntegral <$>) . pickFrameBuffer fb screenDim
              printPicks pickPoints colorPicks = do
                forM_ (zip pickPoints colorPicks) $ \((x,y), col)-> do
                  printf "%d:%d: %x   " x y col
                putStrLn ""

    LambdaCubeGL.disposeRenderer pipePick
    LambdaCubeGL.disposeRenderer pipeDraw
    LambdaCubeGL.disposeStorage storage
    GLFW.destroyWindow win
    GLFW.terminate

deriving instance Show (LC.GLOutput)
deriving instance Show (LC.GLTexture)

-- geometry data: triangles
scale = 250.0
s     = scale

triangleA :: LambdaCubeGL.Mesh
triangleA = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V2F $ V.fromList [V2 s s, V2 s (-s), V2 (-s) (-s)])
        , ("color",     A_V4F $ V.fromList $ take 4 $ repeat $ V4 1 0 0 1)
        , ("id",        A_Int $ V.fromList [1, 1, 1])
        ]
    , mPrimitive    = P_Triangles
    }

triangleB :: LambdaCubeGL.Mesh
triangleB = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V2F $ V.fromList [V2 s s, V2 (-s) (-s), V2 (-s) s])
        , ("color",     A_V4F $ V.fromList $ take 4 $ repeat $ V4 0 0 1 1)
        , ("id",        A_Int $ V.fromList [2, 2, 2])
        ]
    , mPrimitive    = P_Triangles
    }

vec4ToV4F :: Vec4 -> LCLin.V4F
vec4ToV4F (Vc.Vec4 x y z w) = LCLin.V4 x y z w

mat4ToM44F :: Mat4 -> LCLin.M44F
mat4ToM44F (Mat4 a b c d) = LCLin.V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

screenM :: Int -> Int -> Mat4
screenM w h = scaleM
  where (fw, fh) = (fromIntegral w, fromIntegral h)
        scaleM = Vc.Mat4 (Vc.Vec4 (1/fw)  0     0 0)
                         (Vc.Vec4  0     (1/fh) 0 0)
                         (Vc.Vec4  0      0     1 0)
                         (Vc.Vec4  0      0     0 0.5)

pickFrameBuffer
  :: Int         -- ^ framebuffer
  -> (Int, Int)  -- ^ FB dimensions
  -> (Int, Int)  -- ^ pick coordinates
  -> IO F.Word32 -- ^ resultant pixel value
pickFrameBuffer fb (w, h) (x, y) = do
  glFinish
  glBindFramebuffer GL_READ_FRAMEBUFFER $ fromIntegral fb
  let (fbmode, format) =
        if fb == 0
        then (GL_BACK_LEFT,         GL_RGBA)
        else (GL_COLOR_ATTACHMENT0, GL_RGBA_INTEGER)
  glReadBuffer fbmode
  withFrameBuffer w format x (h - y - 1) 1 1 $ \p -> fromIntegral <$> F.peek (F.castPtr p :: F.Ptr F.Word32)

withFrameBuffer :: Int -> GLenum -> Int -> Int -> Int -> Int -> (F.Ptr F.Word8 -> IO a) -> IO a
withFrameBuffer rowLen format x y w h fn = F.allocaBytes (w*h*4) $ \p -> do
  glPixelStorei GL_UNPACK_LSB_FIRST    0
  glPixelStorei GL_UNPACK_SWAP_BYTES   0
  glPixelStorei GL_UNPACK_ROW_LENGTH   $ fromIntegral rowLen
  glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
  glPixelStorei GL_UNPACK_SKIP_ROWS    0
  glPixelStorei GL_UNPACK_SKIP_PIXELS  0
  glPixelStorei GL_UNPACK_SKIP_IMAGES  0
  glPixelStorei GL_UNPACK_ALIGNMENT    1
  glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) format GL_UNSIGNED_BYTE $ F.castPtr p
  glPixelStorei GL_UNPACK_ROW_LENGTH   0
  fn p

initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
    GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win
