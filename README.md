# Common-Lisp-Tangram-Solver
A Tangram Puzzle Solver in Common Lisp that is capable of solving arbitrary geometric tiling problems

![tiles](pics/tiles.png)

This was writting in Common Lisp in 2003, using CLIM for the GUI (Common Lisp Interface Manager). It compiles with LispWorks 6.1 & CLIM. 

Here is a [2003 poster about the project, in German](tangram-poster.pdf). 

![poster](pics/poster.jpg)

In to 2021 I resurrected the code and fixed some major bugs that
caused the solver to be severely incomplete. It is performing much better now. 

I was intruiged to find that I was still able to run an old 2003 CLIM
applications, but it required me to install a number of 32bit legacy
libraries (32bit Motif etc.) to get LispWorks 6.1 with CLIM running again. 

On 2012 iCore 7 2.4 GZ 8 GB Ubuntu Xenial PC, thanks to its geometric
heuristics, it takes 3 minutes and 54 seconds to solve the following
problem:

![editor1](pics/editor1.png)
![editor2](pics/editor2.png)
![solution](pics/solution.png)

A YouTube video showing the solver in action will be published soon.

AFAIK, this is the only Tangram solver capable of solving arbitary
geometric tiling problems that does it complete in terms of
computational geometry.

![search](pics/search.jpg)

The geometric substrate support functions were written by the author, 
as part of his computer science master and PhD theses from 1996 to
2003. The solver was written in 2003. 



