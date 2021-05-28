# Source code for [CSE 507 Homework 3](tex/hw3.pdf)

This directory contains the LaTeX sources for Homework 3 and solution skeletons
for Problems 4, 6-7, and 9-11.

To complete the homework:

- Clone the homework repository:

```
$ git clone git@gitlab.cs.washington.edu:cse507/hw19wi.git
```

- You can find the sources and code in the `hw3` directory:

```
$ cd hw3
$ ls
README.md imp seq tex
```

## Verifying IMP Programs (Problems 4, 6, and 7)

- Install [Racket](http://racket-lang.org) and [Rosette](https://github.com/emina/rosette#installing-rosette).

- The `imp` directory contains the solution skeleton for Problems 4, 6, and 7:
  - [imp.rkt](imp/imp.rkt) contains the IMP interpreter code.
  - [examples.rkt](imp/examples.rkt) has examples of IMP programs and shows how
    to run them.
  - [hoare.rkt](imp/hoare.rkt) contains the verifier for proof outlines in Hoare
    Logic. 
  - [proofs.rkt](imp/proofs.rkt) has two example proof outlines that pass the
    Hoare verifier; **Problem 4** asks you to complete the proofs of the
    remaining programs to also pass the Hoare verifier. 
  - [tools.rkt](imp/tools.rkt) contains the WP/SP verifiers and the SE bounded
    verifier for annotated IMP programs. 
  - [ivl.rkt](imp/ivl.rkt) contains the procedures for translating IMP programs
    to the intermediate verification language for IMP; **Problems 6 and 7** ask
    you to implement these procedures.
  - [verified.rkt](imp/verified.rkt) has examples of annotated IMP programs that
    use the verifiers from [tools.rkt](imp/tools.rkt). This code will work as
    expected once you implement the translation procedures.

- Submit your copy of [proofs.rkt](imp/proofs.rkt) and [ivl.rkt](imp/ivl.rkt) as
  the answer to Problems 4, 6, and 7. Your implementation must be **entirely
  contained** in these two files.  These are the only files that you are going
  to submit.  You may not change any of the interfaces or the supporting code
  that we provide.  In particular, we must be able to run your code simply by
  placing your [proofs.rkt](imp/proofs.rkt) and [ivl.rkt](imp/ivl.rkt) files
  into our copy of the `hw/hw3/imp` directory.

## Verifying Programs with Dafny (Problems 9-11)

The file [seqlib.dfy](seq/seqlib.dfy) in the `seq` directory contains a partial
implementation and specification of a small library for operating on sequences
and arrays. **Problems 9-11** ask you to complete this implementation and add
enough annotations to get Dafny to verify all included postconditios, lemmas,
and client procedures. To get started, install the [Visual Studio Code
IDE](https://code.visualstudio.com) and follow the instructions for installing
the [Dafny extension](https://github.com/Microsoft/dafny/wiki/INSTALL).
You can then open [seqlib.dfy](seq/seqlib.dfy) in the IDE and verify the code!