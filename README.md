# lambdacube-gl

This is the Haskell OpenGL backend for LambdaCube. It depends on the [lambdacube-ir](https://github.com/lambdacube3d/lambdacube-ir) package.

## Building instructions

Probably the easiest way to build this project is using [stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md).

1. Clone the [lambdacube-ir](https://github.com/lambdacube3d/lambdacube-ir) and [lambdacube-gl](https://github.com/lambdacube3d/lambdacube-gl) repositories under a common root (from now on `$LC_ROOT`). If you want to run the hello world example, clone [lambdacube-compiler](https://github.com/lambdacube3d/lambdacube-compiler) as well.

2. If you are a Windows user, you must enable symlink support for `lambdacube-ir`. Start your Git shell as an administrator and run the following commands:

   ```
   git config core.symlinks true
   git reset --hard HEAD
   ```

3. If you just want to build this package, simply execute `stack build` in `$LC_ROOT/lambdacube-gl`. If you want to run the hello world example, continue with the steps below

4. Set up a local stack config in `$LC_ROOT`:

   1. Execute `stack init --resolver lts-4.1` to generate `stack.yaml`.
   2. Copy the `extra-deps` section from the stack config of `lambdacube-compiler` into the above file. The resulting file should look something like this (plus auto-generated comments):

      ```
      resolver: lts-4.1
      packages:
      - lambdacube-ir\lambdacube-ir.haskell\
      - lambdacube-gl\
      - lambdacube-compiler\
      extra-deps:
      - indentation-0.2.1.1
      - pretty-compact-1.0 
      flags: {}
      extra-package-dbs: []
      ```

5. Execute `stack install GLFW-b` in `$LC_ROOT`.

6. Copy `lambdacube-gl/examples` into `$LC_ROOT/examples` (so it's not part of the `lambdacube-gl` local stack, which doesn't see `lambdacube-compiler`).

7. Copy `lambdacube-compiler/testdata/accept/{Prelude|Builtins|Internals}.lc` into `$LC_ROOT/examples` (sadly we don't have a standard way to handle these dependencies yet).

8. In `$LC_ROOT/examples` execute `stack ghc -- --make Hello`.

9. Run the resulting executable and enjoy the view.
