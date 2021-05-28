# Source code for [CSE 507 Homework 2](tex/hw2.pdf)

This directory contains the LaTeX sources for Homework 2 and solution skeletons for Problems 6, 7, and 9--11.

To complete the homework:

- Clone the homework repository:

```
$ git clone git@gitlab.cs.washington.edu:cse507/hw19wi.git
```

- You can find the sources and solution skeletons in the `hw2` directory:

```
$ cd hw2
$ ls
README.md	bvv nnf tex uf
```
## BV verifier (Problem 6)

- Install [Racket](http://racket-lang.org) and [Rosette](https://github.com/emina/rosette#installing-rosette).

- The `bvv` directory contains the solution skeleton for this problem:
  - See [examples.rkt](bvv/examples.rkt) for a quick tour of the types and procedures to use in your implementation.
  - See [tests.rkt](bvv/tests.rkt) for a suite of initial tests for your verifier. We will also test your code on additional benchmarks that are not included here.  To make sure that your verifier works correctly, you will need to write your own tests, eespecially for corner cases.
  - Your implementation must be **entirely contained** in your copy of [verifier.rkt](bvv/verifier.rkt).  This is the only file that you are going to submit.  You may not change any of the interfaces or the supporting code that we provide.  In particular, we must be able to run your code simply by placing your [verifier.rkt](bvv/verifier.rkt) file into our copy of the `hw/hw2/bvv` directory.

## Verification with UFs (Problem 7)

- The `uf` directory contains the solution skeleton for this (Rosette) problem:
  - See [matrix.rkt](uf/matrix.rkt) for the program and the verification queries you will be working to speed up.
  - See [uf.rkt](uf/uf.rkt) for instructions on how to replace a procedure with a UF.
  - Your implementation must be **entirely contained** in your copy of [uf.rkt](uf/uf.rkt), which is the only file you will submit for this problem.

## Finite model finding with Alloy (Problems 9--11)

- Download [Alloy-5.0.0.1.jar](https://github.com/AlloyTools/org.alloytools.alloy/releases/download/v5.0.0.1/Alloy-5.0.0.1.jar) and launch the tool.  
  - We strongly recommend that you use either Linux or macOS.  The Windows version of Alloy may lack support for state-of-the-art SAT solvers (such as Glucose), which are recommended for this assignment.
  - We have tested this with Java 8. If you are unable to launch Alloy, please make sure you have Java 8 installed and selected on your machine.

```
$ java -version
java version "1.8.0_202"
Java(TM) SE Runtime Environment (build 1.8.0_202-b08)
Java HotSpot(TM) 64-Bit Server VM (build 25.202-b08, mixed mode)
```

- Open [nnf.als](nnf/nnf.als) in Alloy and complete the missing definitions. Your implementation must be **entirely contained** in your copy of [nnf.als](nnf/nnf.als), which is the only file you will submit for this problem.


