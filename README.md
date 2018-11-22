Minimal
=======

### What's That? ###

Minimal is a very basic software synthesizer controllable via
[golfscript](http://www.golfscript.com/golfscript/index.html).

Try it out:

	$ make
	$ ./minimal tunes/esteem.gs

Btw, you need `gdc` and `libsdl-dev` for this to compile.


### Built-ins ###

Most of golfscript's default built-ins are there. The interpreter, however, does
not always behave like the original golfscript interpreter.
No big integers. No single quotation mark strings.

Of course, there are also additional built-ins to play notes and stuff. Please,
look at the source code. It's not that much. I might list them here if I get to
it.


### Known Bugs ###

- Stuff seems to get gc'd for some strage reason, causing seg faults. For now,
  gc is disabled (which is really bad).


### Unknown Bugs ###

- There are many...

