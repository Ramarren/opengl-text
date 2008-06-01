# Overview

This is a Common Lisp library for texture mapped fonts in OpenGL.

# Dependencies

- cl-opengl
- iterate
- alexandria
- cl-vectors
- zpb-ttf
- ffa
- affi
- trivial-garbage

# Usage

Font and other information is stored in an instance of `opengl-text` class. Important initargs are:

- `:font` a zpb-ttf font loader
- `:emsquare` somewhat misnamed, more or less the side of a square containing a single glyph.
- `:color` list of three integers between 0 and 255, noting an RGB color

Their respective accessors are `font-loader-of`, `emsquare-of` and `color-of`, which are all setfable.

Only the characters actually used are drawn on the texture. It might be useful to use `ensure-characters (sequence opengl-text-object)` function to avoid pauses when characters are first used.

The function `draw-gl-string (string opengl-text-object &key kerning)` renders a series of rectangles textured with proper letters. No newlines are allowed. The size is normalized so that a font em square (~font size) is one unit in OpenGL coordinate system.