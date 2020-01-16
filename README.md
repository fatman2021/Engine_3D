# Engine_3D

Engine_3D is a real-time 3D engine project grabbed from my old data.
The full rendering is pure software: no OpenGL or the like, no 3D graphics card acceleration!
And: Engine_3D is completely programmed in Ada.

Some parts of the 3D engine, like the animated bump mapping, are still waiting to be ported to my newer 3D project, GLOBE_3D !...

DOS and Linux versions available.

Remarks for the DOS version:

It is also a funny example of a game system almost fully in Ada, including sound driver, graphics driver, keyboard driver. The only things not in Ada there are the file system (MS-DOS), the 32-bit DOS extender (CWSDPMI) and the mouse driver.

For running, I recommend DOSBox.
For building from sources, you'll need the DJGPP compiler, which contains a version of GNAT - seems to be still updated as in June 2016 !
