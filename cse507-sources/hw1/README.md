# Source code for [CSE 507 Homework 1](tex/hw1.pdf)

This directory contains the LaTeX sources and supporting code for Homework 1, as well as the supporting code for Problems 1, 5, and 7.  

The following instructions assume that you are using Linux or macOS as your operating system.  We will provide tech support only for these two OSes.  You are welcome to use Windows, but you will need to get everything working on your own.  

To complete the homework:

- Install [Racket](http://racket-lang.org) and [Rosette](https://github.com/emina/rosette#installing-rosette).

- Clone the homework repository:  
  `$ git clone git@gitlab.cs.washington.edu:cse507/hw19wi.git`

- You can find the LaTeX and Racket sources in the `hw1` directory:   
  `$ cd hw1`  
  `$ ls`  
  `README.md graph-coloring logic sat tex`

- The directory is organized as follows:
	- `tex` contains the LaTeX sources.
	- `logic` contains the solution skeleton for Problem 1.
	- `sat` contains the sample CNF and GraphViz files for Problem 5.
	- `graph-coloring` contains the solution skeleton for Problem 7.
		- See [examples.rkt](graph-coloring/examples.rkt) for a quick tour of the types and procedures to use in your implementation.
		- The`graph-coloring/data/` directory contains a set of graph coloring benchmarks from the [Graph Coloring Benchmarks](https://sites.google.com/site/graphcoloring/) page.  You will be testing your implementation on this data set.

- Your completion of the solution skeletons must be **entirely contained** in your copy of the [classify.rkt](logic/classify.rkt) and [k-coloring.rkt](graph-coloring/k-coloring.rkt) files. You may not change any of the interfaces or the supporting code that we provide.  We must be able to run your code simply by replacing our copies of [classify.rkt](logic/classify.rkt) and [k-coloring.rkt](graph-coloring/k-coloring.rkt) with yours.



