### 0.9.1.3

- Internal changes to make `createStage` errors more verbose about the type of the stage.

### 0.9.1.2

- Support for `contravariant-1.4`.
- Support for GL_ARB_separate_shader_objects for `gl33` backend (shader pragma).

### 0.9.1.1

- Several minor changes (clean up mostly).

## 0.9.1

- Made `Program` an instance of `Eq` and `Show`.

# 0.9

- Dropped `gl32` for `gl33`.

## 0.8.2.1

- Removed the `MonadIO` constraint on `createGeometry` as it’s already brought by `MonadResource`.
- Clean some code about `createGeometry` conditional (gl32 / gl45).

## 0.8.2

- Exposed `SomeUniformName`.

## 0.8.1

- Exposed `UniformName` – forgotten in 0.8 release.

# 0.8

#### Breaking changes

- `createProgram` now has a new rank 2 type parameter to map uniform values. It uses the newly
  added first-class `UniformName` type to select which kind of uniform is wanted – up to now, simple
  uniforms or uniform block. That change is great because it unifies everything under the same type
  and future adds won’t break the existing code – the signature of `createProgram` for instance.

## 0.7.2

- Made `UniformInterface` visible (type only).

## 0.7.1

- Added `Graphics.Luminance.Pixel` in the export-list of `Graphics.Luminance.Texture`.

# 0.7

#### Breaking changes

- Shader creation can fail with the `UnsupportedStage` error, holding the stage (and not a `String`)
  as it used to.
- Fixed cubemap size interface.
- Shader interface now uses the type `StageType` and `createStage` to create new shader stages.

#### Minor changes

- Several internal architectural changes.
- Added `gl45-bindless-textures` caps.
- Added `gl32` and `gl45` backends. The default backend is `gl32` and backends can be selected via
  compilation flags.

#### Patch changes

- Simplified and fixed UBO implementation.
- Added more debug symbols.
- OpenGL debugging now shows the callstack in a fancy way.
- Several internal architectural changes.

### 0.6.0.5

- semigroups-0.18 support.

### 0.6.0.4

- Extensions are now set per module.

### 0.6.0.3

- Updated .cabal documentation.
- Support for `semigroups-0.17`.

### 0.6.0.2

- Fixed typo in the hackage documentation.

### 0.6.0.1

- Fixed `(:*:)` for `UniformBlock`.
- Dumped the `Storable` constraint in the `uniBlock` function (rank2 function passed to build
  uniform interfaces to `createProgram`).
- Added `(:.)`, `(,)`, `(,,)` and `(,,,)` into `UniformBlock`.

# 0.6

#### Breaking changes

- Added a new function to the `createProgram` and `createProgram_` uniform interface builder
  argument. That function can now be used to retrieve `U (Region rw (UB a))`, which is a *UBO*.
- The uniform interface creation is not performed in a arbitrary, user-defined monad anymore. A
  dedicated type was introduced for that very purpose – `UniformInterface` – constraining the user
  to only use the uniformize functions to map semantics to `U` values.

#### Non-breaking changes

- Added `UB`, which can be used along with `Buffer` to create *UBO* buffers and pass them to
  shaders.

### 0.5.2.1

- Relaxed lower bound of `linear` to accept `linear-1.19.*`. That changes should enable `lumimance`
  to be included into stackage.
- Changed internal representation of `Region`.

## 0.5.2

#### Non-breaking changes

- Added texture arrays:
    + `Texture1DArray`
    + `Texture2DArray`
    + `CubemapArray`

## 0.5.1

#### Non-breaking changes

- Added several `Uniform` instances for [linear](http://hackage.haskell.org/package/linear).

#### Patch changes

- Fixed the indexed render.
- Fixed the vertex attributes being ignored.

### 0.5

#### Breaking changes

- Changed the interface of texels transfer and filling. We dropped the `Foldable` instance and now
  require a `Data.Vector.Storable.Vector` for performance purposes.

#### Non-breaking changes

- Added `MirrorRepeat` constructor to `Wrap`.

#### Patch changes

- Fixed prerequisites in README.

## 0.4.1

- Fixed the `sizeOf` implementation of `a :. b`.
- Added `nubDirect`, which can be used to turn *direct geometry* into *indirect geometry*.

# 0.4

#### Non-breaking changes

- Added .gitignore.

#### Breaking changes

- `V2`, `V3` and `V4` replaced by `vec2`, `vec3` and `vec4`.
- `V` is not anymore luminance’s. We use linear’s one, because it already has all the instances we
  need and is more generic. The interface is then impacted.

## 0.3.2

- Added Core.Tuple into the export liste of Luminance for easier uses in client code space.

### 0.3.1.2

- Fixed Geometry haddock documentation.

### 0.3.1.1

- Fixed haddock escaping issues.

## 0.3.1

- Added `CubeFace` in the interface.

### 0.3.0.1

- Enhanced Texture documentation.

# 0.3

- All textures can now be used in shaders.
- Added support for more OpenGL textures – though, framebuffers are not impacted yet.
    - `Texture1D`
    - `Texture2D`
    - `Texture3D`
    - `Cubemap`
- Changed the texture interface with type families so that we can add more in an
  easier way!

# 0.2

#### Breaking changes

- Automatically insert GLSL pragmas in shaders.

#### Non-breaking changes

- Added documentation for RenderCmd.
- Added stdRenderCmd_.
- Added shaderProgramBatch_.

### 0.1.1.1

- Fixed a typo in the Graphics.Luminance documentation.

## 0.1.1

- Added a tutoral in Graphics.Luminance.

# 0.1

- Initial revision. Do not consider this revision as a stable release. It’s experimental. The
  first stable release will be **1.0**.
