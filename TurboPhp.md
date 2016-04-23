# Introduction #

After numerous requests we have decided to release the source code to our TurboPhp project for Delphi (Windows).

You may grab up the code from the [Downloads](http://code.google.com/p/turbophp/downloads/list) page or from the Google SVN server. There are a lot of goodies there, including component and framework code and alpha work on an unreleased ajax IDE.

Binaries (including EXEs, DCPs, BPLs) were removed from the SVN repo. You can grab those up as separate [downloads](http://code.google.com/p/turbophp/downloads/list).

# Details #

You **will have trouble** compiling the code because the original projects used several commercial Delphi libraries, including LMD, Dream Controls, and AQ Docking. These systems will have to be replaced with custom or open source variations before the projects are buildable.

The biggest issues there are the docking system and the designer. Copious work was done on writing a custom designer implementation (most of which was also licensed to the JVCL), most of that work can be found in the Turbo5 folders. The docking system can likely be replaced by other objects in the JVCL.

There is a lot of potential here, but also perhaps a lot of work to get this project buildable. I hope the community can make use of this code.