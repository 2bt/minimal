Minimal
=======

Minimal is a very basic software synthesizer controllable via [gofscript]
(http://www.golfscript.com/golfscript/index.html).

### Built-ins ###

Most of golfscript's default built-ins are there. However, the interpreter does
not always behave like the original golfscript interpreter.
Of course, there are also additional built-ins to play notes and stuff. Please,
look at the source code. It's not that much.

### Known Bugs ###

- Stuff seems to get gc'd for some strage reason, causing seg faults. For now,
  gc is disabled (this is bad).
