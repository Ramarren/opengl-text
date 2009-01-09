# Overview

This is a Common Lisp library for texture mapped fonts in OpenGL. The actual glyph rendering is a pluggable subsystem, right now there is only one based on cl-vectors ttf rendering. Unfortunately, there is no hinting, so small font sizes don't work very well, and there is no pixel-texel positioning anyway.

There is polygonal font subsystem, but it is completely broken.

# Dependencies

- cl-opengl
- iterate
- alexandria
- ffa
- trivial-garbage

and for TTF rendering subsystem:

- cl-vectors
- zpb-ttf

# Usage

Note: the OpenGL textures are managed using finalizers. It is possible that they might run after
OpenGL has stopped and started again, and free a texture in a new session. Deleting a texture when
OpenGL is not running doesn't seem to hurt, as far as I can tell.

## Basic Textured Font

Font object is usually created by glyph rendering subsystem. Right now only one exists:

`opengl-text-vector:make-vector-gl-text (font &key (mipmapped t) (internal-size 32) (length 20))`

 - `font` is a path to ttf font, or zpb-ttf font loader
 - `mipmapped` sets whether mipmaps are used
 - `internal-size` sets how many pixels the side of em-square of the font is in the texture
 - `length` sets the size of vertex and texture coordinates buffers, special variable `opengl-text:*auto-extend-buffers*` controls whether drawing a longer string will enlarge them, or will allocate one shot larger buffers

Returned object is a valid argument to:

`opengl-text:draw-gl-string (string gl-text &key kerning depth-shift)`

 - `string` is a string to be printed, newlines are ignored and it doesn't cause GL matrix changes
 - `gl-text` is a aforementioned object
 - `kerning` controls kerning
 - `depth-shift` allows each letter to be translated in the z direction
