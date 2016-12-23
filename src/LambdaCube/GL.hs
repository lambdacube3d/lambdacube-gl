module LambdaCube.GL (
    -- Schema
    module LambdaCube.PipelineSchema,
    -- IR
    V2(..),V3(..),V4(..),
    -- Array, Buffer, Texture
    Array(..),
    ArrayType(..),
    Buffer,
    BufferSetter,
    IndexStream(..),
    Stream(..),
    StreamSetter,
    FetchPrimitive(..),
    InputType(..),
    Primitive(..),
    SetterFun,
    TextureData,
    InputSetter(..),
    fromStreamType,
    sizeOfArrayType,
    toStreamType,
    compileBuffer,
    disposeBuffer,
    updateBuffer,
    bufferSize,
    arraySize,
    arrayType,
    uploadTexture2DToGPU,
    uploadTexture2DToGPU',
    disposeTexture,

    -- GL: Renderer, Storage, Object
    GLUniformName,
    GLRenderer,
    GLStorage,
    Object,
    schema,
    schemaFromPipeline,
    allocRenderer,
    disposeRenderer,
    setStorage,
    renderFrame,
    allocStorage,
    disposeStorage,
    uniformSetter,
    addObject,
    removeObject,
    enableObject,
    setObjectOrder,
    objectUniformSetter,
    setScreenSize,
    sortSlotObjects,

    uniformBool,
    uniformV2B,
    uniformV3B,
    uniformV4B,

    uniformWord,
    uniformV2U,
    uniformV3U,
    uniformV4U,

    uniformInt,
    uniformV2I,
    uniformV3I,
    uniformV4I,

    uniformFloat,
    uniformV2F,
    uniformV3F,
    uniformV4F,

    uniformM22F,
    uniformM23F,
    uniformM24F,
    uniformM32F,
    uniformM33F,
    uniformM34F,
    uniformM42F,
    uniformM43F,
    uniformM44F,

    uniformFTexture2D,

    -- schema builder utility functions
    (@:),
    defObjectArray,
    defUniforms,
    makeSchema,

    (@=),
    updateUniforms,
    updateObjectUniforms
) where

import LambdaCube.GL.Type
import LambdaCube.GL.Backend
import LambdaCube.GL.Data
import LambdaCube.GL.Input
import LambdaCube.IR
import LambdaCube.Linear
import LambdaCube.PipelineSchema
import LambdaCube.PipelineSchemaUtil
