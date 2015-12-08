# What is this?

![Hackage](https://img.shields.io/badge/hackage-0.8.2-orange.svg?style=flat)
![License](https://img.shields.io/badge/license-BSD3-blue.svg?style=flat)

`luminance` is an effort to make graphics rendering simple and elegant in **Haskell**. It’s forked
from a larger project which was closed because way too complex and bloated. The aims of `luminance`
are:

  - providing a simple API ;
  - abstract over the trending hardware interfaces (i.e. **OpenGL** up to now) and approach
    as close as possible a stateless design ;
  - being a tiny API, so that new comers don’t have to learn a lot of new concepts to get
    their feet wet.

The first version of `luminance` will be released of [hackage](https://hackage.haskell.org) and
[stackage](https://www.stackage.org) as a *BSD3* library.

# What’s included?

`luminance` is a rendering framework, not a 3D engine. As so, it doesn’t include stuff like
lights, materials, asset management nor scene description. It only provides a rendering framework
you can plug in whatever libraries you want to. The single restriction – yet – is that you must have
an operating system with **OpenGL** installed.

## Features set

- **buffers**: **buffers** are way to communicate with the *GPU*; they represent regions of memory
  you can write to and read from. There’re several kinds of buffers you can create, among *vertex
  and index buffers*, *shader buffer*, *compute buffer*, and so on and so forth… ;
- **framebuffers**: **framebuffers** are used to hold *renders*. Each time you want to perform a
  render, you need to perform it into a framebuffer. Framebuffers can then be combined with each
  other to produce nice effects ;
- **shaders**: `luminance` support six kinds of shader stages:
  + tessellation control shaders ;
  + tessellation evaluation shaders ;
  + vertex shaders ;
  + geometry shaders ;
  + fragment shaders ;
  + compute shaders ;
- **vertices, indices, primitives and shapes**: those are used to define a shape you can render into
  a framebuffer
- **textures**: **textures** represent information packed into arrays on the GPU, and can be used
  to customize a visual aspect or pass information around ;
- **blending**: **blending** is the process of taking two colors from two framebuffers and mix them
  between each other ;
- and a lot of other cool things

# What are the prerequisites?

In order to use `luminance`, you need several prerequisites to be fulfilled. Those prerequisites
determine which backend you will be able to use. Currently, two backends are available: **gl32**
and **gl45**.

- a library to setup an *OpenGL* context.
- **gl32**
  - a decent *OpenGL 3.2* implementation
  - support for extension *GL_ARB_texture_cube_map*
- **gl45**
  - a decent *OpenGL 4.5* implementation
- **gl-bindless-textures** (bonus)
  - a decent *OpenGL 4.5* implementation
  - support for extension *GL_ARB_bindless_texture* ;

The backends can be enabled through **compilation flags**. You should only pick one as the more
recent will be preferred over others. However, some of the flags can be combined. If you want
**bindless textures**, you should enable the flags `gl45` and `gl-bindless-textures`.

`luminance` does not provide point a way to create windows because it’s important that it not depend
on windowing libraries so that end-users can use whatever they like. Furthermore, such libraries
typically implement windowing and events features, which have nothing to do with our initial
purposes. However, a few packages might come up, like `luminance-glfw` if such libraries are judged
useful.

## Important point about OpenGL compatibility

Recently, a lot of people asked whether older version of **OpenGL** are planned to be supported.
Even though I decided not to support them, I’ve changed my mind. A support for **OpenGL 3.2+**
might be planned as well as a version for **OpenGL ES**. The specification is yet to set but it will
eventually be a thing.

# How to dig in?

`luminance` is written to be fairly simple. The documentation – on hackage – is very transparent
about what the library does and several articles will appear as the development goes on. Keep tuned!
