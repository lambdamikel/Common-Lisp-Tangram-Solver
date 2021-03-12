# Common-Lisp-Tangram-Solver
A Tangram Puzzle Solver in Common Lisp that is capable of solving arbitrary geometric tiling problems, utilizing CLIM (Common Lisp Interface Manager) for its GUI. 

![tiles](pics/tiles.png)

Check out [the YouTube video to see it in action!](https://www.youtube.com/watch?v=UUn_np8I3zg) 

This was written in Common Lisp in 2003, using CLIM (Common Lisp
Interface Manager) for the GUI. It compiles with LispWorks 6.1 & CLIM.

Here is a [2003 poster about the project, in German](tangram-poster.pdf). 

![poster](pics/poster.jpg)

In 2021 I resurrected the code and fixed some major bugs that were
responsible for some severe incompleteness. It is performing much
better now!

I was intruiged to find that I was still able to run a CLIM
application from 2003 in 2021, but it required me to install a number
of 32bit legacy libraries (32bit Motif etc.) to get is going again on
my 64bit Ubuntu Xenial PC.

On my 2012 iCore 7 2.4 GZ 8 GB Ubuntu Xenial PC, thanks to its
geometric heuristics, it takes ~ 3 minutes and 54 seconds to solve the
following problem:

![editor1](pics/editor1.png)
![editor2](pics/editor2.png)
![thinking](pics/showthinking.png)
![solutions](pics/editor.jpg)

A YouTube video showing the solver in action will be published soon.

AFAIK, this is the only Tangram solver in existence (there are very
few out there!) capable of solving arbitary geometric tiling problems
purely by means of geometrical search / computational geometry. 

![search](pics/search.jpg)

The geometric substrate support functions were also written by the
author, as part of his computer science master and PhD endeavors from
1996 to 2003. The solver was written in 2003. 



