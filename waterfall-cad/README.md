# Waterfall CAD 

![Waterfall CAD](https://raw.githubusercontent.com/joe-warren/opencascade-hs/main/images/logo/waterfall-cad-logo-name.svg)

Waterfall CAD is a declarative CAD/Solid Modeling library.

This uses [opencascade-hs](https://hackage.haskell.org/package/opencascade-hs) as the kernel, but provides a "more functional" API over it.

## Dependencies 

You'll need the main OpenCASCADE libraries/header files installed to use this.

Please see the [main readme](https://github.com/joe-warren/opencascade-hs/#installing-dependencies) on Github for more information.

## Examples

There are examples of how to use the library in [waterfall-cad-examples](https://hackage.haskell.org/package/waterfall-cad-examples).

You can see images of these examples in the [main readme](https://github.com/joe-warren/opencascade-hs/#examples) on Github.

## Imports 

This library is recommended to used by importing the root `Waterfall` module, qualified, which includes the entire public API, e.g:

```haskell
import qualified Waterfall
```

It should also be possible to import the library unqualified, or to import the individual modules.

The examples use qualified imports from the individual modules, in order to communicate the module structure.
However this can be a little tedious for regular use. 
