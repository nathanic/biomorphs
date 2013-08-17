# Biomorphs

This is a simple implementation of vaguely-Dawkins-style Biomorphs.  The primary reason it exists is to give me an excuse to tinker on ClojureScript, play with canvas, and other cool tech.

It will show you a grid of 9 line-drawing creatures.  You pick the one you like best, and it will be moved to the upper left square.  The remaining creatures will be deleted and replaced with random variants on the new parent creature of choice.  In this way you can sort of "breed" asexual creatures into [hopefully cool-looking] shapes.

The line drawings are binary trees, the shape, depth, and color of which are controlled by a simple genome vector.

There might be a demo running at [nathanic.org/biomorphs](http://nathanic.org/biomorphs).
