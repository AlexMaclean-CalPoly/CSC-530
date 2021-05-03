# Program Verification as Constraint Solving
**805 Kitchen:** Alex MacLean, Bailey Wickham

## Progress
For this project we built a full implementation of the pipeline described in Section 2 for program verification. Given a program in an s-expression syntax (similar to the paper, but with more parentheses) we generate the second-order contrains from the control flow graph, then generate the first order constraints based on command line specifications, then we simplify to conjunctive normal from before applying Farka's Lemma, satifiablity is then checked with Z3 and if a model is generated it is parsed back in and the final invariant is displayed.


## Example
```bash
>> cat examples/figure1.cexp
```
```lisp
(
(x := -50)
(while (x < 0) (
    (x := (x + y))
    (y := (y + 1))
))
(assert(y > 0))
)
```

```bash
>> ./verifier.exe examples/figure1.cexp 2 1
```
```lisp
Second Order Constraints:
((=>
   (and I (and (not (>= x 0)) (subst (>= (+ y x) 0) x (subst (>= (+ 1 y) 0) y true))))
   (subst (>= (+ y x) 0) x (subst (>= (+ 1 y) 0) y I)))
 (=> (and I (and (not (not (>= x 0))) true)) (and (>= (+ -1 y) 0) I))
 (=> (and true (subst (>= -50 0) x true)) (subst (>= -50 0) x I)))

Hypothesized Invariant:
(or (and (>= (+ u506 (* y u507) (* x u508)) 0)) (and (>= (+ u509 (* y u510) (* x u511)) 0)))

First Order Constraints:
((and (or (>= x 0)
           (>= (+ (+ u506 u507) (* y (+ u507 u508)) (* x u508)) 0)
           (>= (+ (+ u509 u510) (* y (+ u510 u511)) (* x u511)) 0)
           (>= (+ (+ -1 (* u506 -1)) (* y (* u507 -1)) (* x (* u508 -1))) 0))
       (or (>= x 0)
           (>= (+ (+ u506 u507) (* y (+ u507 u508)) (* x u508)) 0)
           (>= (+ (+ u509 u510) (* y (+ u510 u511)) (* x u511)) 0)
           (>= (+ (+ -1 (* u509 -1)) (* y (* u510 -1)) (* x (* u511 -1))) 0)))
  (and (or (>= (+ -1 y) 0)
           (>= (+ -1 (* x -1)) 0)
           (>= (+ (+ -1 (* u506 -1)) (* y (* u507 -1)) (* x (* u508 -1))) 0))
       (or (>= (+ -1 y) 0)
           (>= (+ -1 (* x -1)) 0)
           (>= (+ (+ -1 (* u509 -1)) (* y (* u510 -1)) (* x (* u511 -1))) 0))
       (or (>= (+ -1 (* x -1)) 0)
           (>= (+ u506 (* y u507) (* x u508)) 0)
           (>= (+ u509 (* y u510) (* x u511)) 0)
           (>= (+ (+ -1 (* u506 -1)) (* y (* u507 -1)) (* x (* u508 -1))) 0))
       (or (>= (+ -1 (* x -1)) 0)
           (>= (+ u506 (* y u507) (* x u508)) 0)
           (>= (+ u509 (* y u510) (* x u511)) 0)
           (>= (+ (+ -1 (* u509 -1)) (* y (* u510 -1)) (* x (* u511 -1))) 0)))
  (and (or (>= (+ (+ u506 (* u508 -50)) (* y u507)) 0) (>= (+ (+ u509 (* u511 -50)) (* y u510)) 0))))

Z3 Program:
(declare-const lambda*516 Int)
(declare-const lambda*521 Int)
(declare-const lambda*525 Int)
(declare-const lambda*529 Int)
(declare-const lambda*534 Int)
(declare-const lambda*539 Int)
(declare-const lambda*542 Int)
(declare-const lambda514 Int)
(declare-const lambda512 Int)
(declare-const lambda513 Int)
(declare-const lambda515 Int)
(declare-const lambda517 Int)
(declare-const lambda518 Int)
(declare-const lambda519 Int)
(declare-const lambda520 Int)
(declare-const lambda522 Int)
(declare-const lambda523 Int)
(declare-const lambda524 Int)
(declare-const lambda526 Int)
(declare-const lambda527 Int)
(declare-const lambda528 Int)
(declare-const lambda530 Int)
(declare-const lambda532 Int)
(declare-const lambda533 Int)
(declare-const lambda531 Int)
(declare-const lambda535 Int)
(declare-const lambda536 Int)
(declare-const lambda537 Int)
(declare-const lambda538 Int)
(declare-const lambda540 Int)
(declare-const lambda541 Int)
(declare-const u506 Int)
(declare-const u509 Int)
(declare-const u510 Int)
(declare-const u507 Int)
(declare-const u511 Int)
(declare-const u508 Int)
(assert (= (+ 0 (* lambda515 (+ 0 u506)) (* lambda514 (+ -1 (* u509 -1) (* u510 -1))) (* lambda512 -1) (* lambda513 (+ -1 (* u506 -1) (* u507 -1)))) (* -1 lambda*516)))
(assert (= (+ 0 (* lambda515 u507) (* lambda514 (+ (* u510 -1) (* u511 -1))) (* lambda513 (+ (* u507 -1) (* u508 -1)))) 0))
(assert (= (+ 0 (* lambda515 u508) (* lambda514 (* u511 -1)) (* lambda512 -1) (* lambda513 (* u508 -1))) 0))
(assert (= (+ 0 (* lambda517 -1) (* lambda518 (+ -1 (* u506 -1) (* u507 -1))) (* lambda519 (+ -1 (* u509 -1) (* u510 -1))) (* lambda520 (+ 0 u509))) (* -1 lambda*521)))
(assert (= (+ 0 (* lambda518 (+ (* u507 -1) (* u508 -1))) (* lambda519 (+ (* u510 -1) (* u511 -1))) (* lambda520 u510)) 0))
(assert (= (+ 0 (* lambda517 -1) (* lambda518 (* u508 -1)) (* lambda519 (* u511 -1)) (* lambda520 u511)) 0))
(assert (= (+ 0 (* lambda522 0) (* lambda523 0) (* lambda524 (+ 0 u506))) (* -1 lambda*525)))
(assert (= (+ 0 (* lambda522 -1) (* lambda524 u507)) 0))
(assert (= (+ 0 lambda523 (* lambda524 u508)) 0))
(assert (= (+ 0 (* lambda526 0) (* lambda527 0) (* lambda528 (+ 0 u509))) (* -1 lambda*529)))
(assert (= (+ 0 (* lambda526 -1) (* lambda528 u510)) 0))
(assert (= (+ 0 lambda527 (* lambda528 u511)) 0))
(assert (= (+ 0 (* lambda531 (+ -1 (* u506 -1))) (* lambda530 0) (* lambda532 (+ -1 (* u509 -1))) (* lambda533 (+ 0 u506))) (* -1 lambda*534)))
(assert (= (+ 0 (* lambda531 (* u507 -1)) (* lambda532 (* u510 -1)) (* lambda533 u507)) 0))
(assert (= (+ 0 (* lambda531 (* u508 -1)) lambda530 (* lambda532 (* u511 -1)) (* lambda533 u508)) 0))
(assert (= (+ 0 (* lambda535 0) (* lambda536 (+ -1 (* u506 -1))) (* lambda537 (+ -1 (* u509 -1))) (* lambda538 (+ 0 u509))) (* -1 lambda*539)))
(assert (= (+ 0 (* lambda536 (* u507 -1)) (* lambda537 (* u510 -1)) (* lambda538 u510)) 0))
(assert (= (+ 0 lambda535 (* lambda536 (* u508 -1)) (* lambda537 (* u511 -1)) (* lambda538 u511)) 0))
(assert (= (+ 0 (* lambda540 (+ -1 (* u506 -1) (* u508 50))) (* lambda541 (+ -1 (* u509 -1) (* u511 50)))) (* -1 lambda*542)))
(assert (= (+ 0 (* lambda540 (* u507 -1)) (* lambda541 (* u510 -1))) 0))
(assert (>= lambda514 0))
(assert (>= lambda512 0))
(assert (>= lambda513 0))
(assert (>= lambda515 0))
(assert (>= lambda517 0))
(assert (>= lambda518 0))
(assert (>= lambda519 0))
(assert (>= lambda520 0))
(assert (>= lambda522 0))
(assert (>= lambda523 0))
(assert (>= lambda524 0))
(assert (>= lambda526 0))
(assert (>= lambda527 0))
(assert (>= lambda528 0))
(assert (>= lambda530 0))
(assert (>= lambda532 0))
(assert (>= lambda533 0))
(assert (>= lambda531 0))
(assert (>= lambda535 0))
(assert (>= lambda536 0))
(assert (>= lambda537 0))
(assert (>= lambda538 0))
(assert (>= lambda540 0))
(assert (>= lambda541 0))
(assert (> lambda*516 0))
(assert (> lambda*521 0))
(assert (> lambda*525 0))
(assert (> lambda*529 0))
(assert (> lambda*534 0))
(assert (> lambda*539 0))
(assert (> lambda*542 0))
(check-sat)
(get-model)

Final Invariant:
(or (and (>= (+ -50 (* y 0) (* x -1)) 0)) (and (>= (+ -17 (* y 16) (* x 0)) 0)))
```

## Reference

- https://www.brinckerhoff.org/clements/2214-csc530/Files/gsv-constraints-2008.pdf
