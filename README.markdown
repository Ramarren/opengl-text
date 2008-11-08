# Overview

This is a Common Lisp library for texture mapped fonts in OpenGL.

# Dependencies

- cl-opengl
- iterate
- alexandria
- cl-vectors
- zpb-ttf
- ffa
- trivial-garbage

# Usage

Note: the OpenGL textures are managed using finalizers. It is possible that they might run after OpenGL has stopped and started again, and free a texture in a new session. Deleting a texture when OpenGL is not running doesn't seem to hurt, as far as I can tell.

Another note: none of below methods handle small fonts size very well. Class
`packed-mipmap-opengl-text` should in theory be best, with emsquare equal to displayed size, but
unfortunately cl-vectors/zpb-ttf provides no hinting. It should be relatively easy to replace them
with Freetype by replacing draw-char method, but I would prefer not to introduce non-Lisp
dependencies (besides OpenGL of course), and there seem to be no established Freetype bindings,
anyway.

## Basic Textured Font

Font and other information is stored in an instance of `opengl-text` class. Important initargs are:

- `:font` a zpb-ttf font loader
- `:emsquare` somewhat misnamed, more or less the side of a square containing a single glyph.
- `:length` length of string there are preallocated vertex buffers for. Default is 20.

Their respective accessors are `font-loader-of`, `emsquare-of` and `length-of`, which are all setfable.

Only the characters actually used are drawn on the texture. It might be useful to use `ensure-characters (sequence opengl-text-object)` function to avoid pauses when characters are first used.

Dynamic variable `*auto-extend-buffers*` controls whether vertex/texture coordinates buffers are automatically extended when string is drawn longer that `length`. Setting it to `nil` might be useful if a longer string is only rarely drawn.

The function `draw-gl-string (string opengl-text-object &key kerning)` renders a series of rectangles textured with proper letters in the current RGBA color. No newlines are allowed. The size is normalized so that a font em square (~font size) is one unit in OpenGL coordinate system.

## Mipmapped font

This is essentially identical to the one above, but draws mipmaps for smaller emsquares. This
requires more memory and time, as each glyph has to be drawn (log emsquare 2) times. 

The class name is `mipmap-opengl-text`.

## Packed texture

This is similar to the basic font, but uses different way of packing glyphs into the texture. The two preceding classes draw glyphs scaled to square cells on the texture. This one maintains aspect ratio. The glyphs are packed into texture using very simple rectangle packing algorithm, but it seems to work reasonably well. Also automatic mipmaps are generated.

The class name is `packed-mipmap-opengl-text`.

## Polygonal

The class `polygonal-opengl-text` uses cl-vectors and GLU tesselator to transform glyphs into
triangles, and draws them as such. Initargs `:bezier-distance-tolerance` and
`:bezier-angle-tolerance` are passed to cl-vectors and control in how many segments curves are
split.

## Outline

The class `outline-opengl-text` is like the above, but draws glyphs as outline lines, rather than triangles. Slot `line-width` controls width of the line in pixels.
