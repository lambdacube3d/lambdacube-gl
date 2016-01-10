{-# LANGUAGE PackageImports, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Data.Map as Map
import qualified Data.Vector.Storable as SV

import "lambdacube-gl-ir" LambdaCube.GL as LambdaCubeGL -- renderer
import "lambdacube-gl-ir" LambdaCube.GL.Mesh as LambdaCubeGL

import Codec.Picture as Juicy

import LambdaCube.Compiler.Driver as LambdaCube -- compiler

----
import Control.Monad.Writer

{-
data ObjectArraySchema
    = ObjectArraySchema
    { primitive     :: FetchPrimitive
    , attributes    :: Map String StreamType
    }
    deriving Show

data PipelineSchema
    = PipelineSchema
    { objectArrays  :: Map String ObjectArraySchema
    , uniforms      :: Map String InputType
    }
-}


a @: b = tell [(a,b)]
defObjectArray n p m = tell [PipelineSchema (Map.singleton n $ ObjectArraySchema p $ Map.fromList $ execWriter m) mempty]
defUniforms m = tell [PipelineSchema mempty $ Map.fromList $ execWriter m]
makeSchema a = head $ execWriter a :: PipelineSchema

sch = makeSchema $ do
  defObjectArray "objects" Triangles $ do
    "position"  @: Attribute_V4F
    "uv"        @: Attribute_V2F
  defUniforms $ do
    "time"           @: Float
    "diffuseTexture" @: FTexture2D

-----
main :: IO ()
main = do
    -- compile hello.lc to graphics pipeline description
    pipelineDesc <- LambdaCube.compileMain ["."] OpenGL33 "hello" >>= \case
      Left err  -> fail $ "compile error:\n" ++ err
      Right pd  -> return pd

    win <- initWindow "LambdaCube 3D DSL Hello World" 640 480

    -- setup render data
    let inputSchema = makeSchema $ do
          defObjectArray "objects" Triangles $ do
            "position"  @: Attribute_V4F
            "uv"        @: Attribute_V2F
          defUniforms $ do
            "time"           @: Float
            "diffuseTexture" @: FTexture2D

    storage <- LambdaCubeGL.allocStorage inputSchema

    -- upload geometry to GPU and add to pipeline input
    LambdaCubeGL.uploadMeshToGPU triangleA >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []
    LambdaCubeGL.uploadMeshToGPU triangleB >>= LambdaCubeGL.addMeshToObjectArray storage "objects" []

    -- load image and upload texture
    Right img <- Juicy.readImage "Panels_Diffuse.png"
    textureData <- LambdaCubeGL.uploadTexture2DToGPU img

    -- allocate GL pipeline
    renderer <- LambdaCubeGL.allocRenderer pipelineDesc
    LambdaCubeGL.setStorage renderer storage >>= \case -- check schema compatibility
      Just err -> putStrLn err
      Nothing  -> loop
        where loop = do
                -- update graphics input
                GLFW.getWindowSize win >>= \(w,h) -> LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
                {-
                LambdaCubeGL.updateUniforms storage $ do
                  "time" @= fromJust <$> GLFW.getTime
                  "diffuseTexture" @= return textureData
                -}
                -- render
                LambdaCubeGL.renderFrame renderer
                GLFW.swapBuffers win
                GLFW.pollEvents

                let keyIsPressed k = fmap (==KeyState'Pressed) $ GLFW.getKey win k
                escape <- keyIsPressed Key'Escape
                if escape then return () else loop

    LambdaCubeGL.disposeRenderer renderer
    LambdaCubeGL.disposeStorage storage
    GLFW.destroyWindow win
    GLFW.terminate

-- geometry data: triangles
triangleA :: LambdaCubeGL.Mesh
triangleA = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V2F $ SV.fromList [V2 1 1, V2 1 (-1), V2 (-1) (-1)])
        , ("uv",        A_V2F $ SV.fromList [V2 0 0, V2 0 1, V2 1 1])
        ]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }

triangleB :: LambdaCubeGL.Mesh
triangleB = Mesh
    { mAttributes   = Map.fromList
        [ ("position",  A_V2F $ SV.fromList [V2 1 1, V2 (-1) (-1), V2 (-1) 1])
        , ("uv",        A_V2F $ SV.fromList [V2 0 0, V2 1 1, V2 1 0])
        ]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }

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
