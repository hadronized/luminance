## 0.5.1

#### Minor changes

- Added several `Uniform` instances for [linear](http://hackage.haskell.org/package/linear).

#### Patch changes

- Fixed the vertex attributes being ignored.

### 0.5

#### Major changes

- Changed the interface of texels transfer and filling. We dropped the `Foldable` instance and now
  require a `Data.Vector.Storable.Vector` for performance purposes.

#### Minor changes

- Added `MirrorRepeat` constructor to `Wrap`.

#### Patch changes

- Fixed prerequisites in README.

## 0.4.1

- Fixed the `sizeOf` implementatiof of `a :. b`.
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
