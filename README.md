# lambdacube-gl
[![Gitter chat](https://badges.gitter.im/lambdacube3d/lambdacube3d.png)](https://gitter.im/LambdaCube-3D/Lobby)

This is a fork of the Haskell OpenGL backend for LambdaCube 3D, that uses the OpenGL 4.6 API instead of OpenGL 3.3. This is by no means a huge fork of the code; there are basically no differences between the 3.3 and 4.6 APIs in the package that lambdacube-gl uses for OpenGl bindings (OpenGLRaw). Nonetheless, you should expect some differences in how your use behaves in comparison to regular LambdaCube 3D code.

## Building instructions

0. On **Linux** install the following libraries.
   i.e. on Ubuntu:
   ```
   sudo apt install libgl1-mesa-dev libxi-dev libxcursor-dev libxinerama-dev libxrandr-dev zlib1g-dev
   ```
   For other Linux distributions make sure the corresponing packages are installed.

   *These libraries required for OpenGL development.*

1. Install Haskell [Stack](http://www.haskellstack.org) by following it's simple [install manual](https://docs.haskellstack.org/en/stable/README/#how-to-install).

2. Checkout the this repository then run the following commands.
```
stack setup
stack build
```
3. Run the examples.
```
cd examples
stack exec lambdacube-gl-hello
stack exec lambdacube-gl-hello-obj
```

## Tutorials and Examples

- [Getting started](http://lambdacube3d.com/getting-started)
- [Workshop material](https://github.com/csabahruska/lambdacube-workshop)
