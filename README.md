# Program Verification as Constraint Solving
**805 Kitchen:** Alex MacLean, Bailey Wickham

## Progress
For this project we built a partial implementation of the pipeline described in Section 2 for program verification. Given a program in an s-expression syntax (similar to the paper, but with more parentheses) we generate the second-order contrains from the control flow graph, then generate the first order constraints based on command line specifications. 


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
I ∧ (x < 0) => ((I)[(y + 1)/y])[(x + y)/x]
I ∧ ¬((x < 0)) => (y > 0) ∧ I
true => (I)[-50/x]

Hypothesized Invariant:
(((y * u507) + (x * u508)) >= 0) ∨ (((y * u509) + (x * u510)) >= 0)

First Order Contraints:
¬((((y * u507) + (x * u508)) >= 0) ∨ (((y * u509) + (x * u510)) >= 0) ∧ 
   (x < 0)) ∨ ((((y + 1) * u507) + ((x + y) * u508)) >= 0) ∨ ((((y + 1) * u509) + ((x + y) * u510)) >= 0)
¬((((y * u507) + (x * u508)) >= 0) ∨ (((y * u509) + (x * u510)) >= 0) ∧ 
   ¬((x < 0))) ∨ (y > 0) ∧ (((y * u507) + (x * u508)) >= 0) ∨ (((y * u509) + (x * u510)) >= 0)
¬(true) ∨ (((y * u507) + (-50 * u508)) >= 0) ∨ (((y * u509) + (-50 * u510)) >= 0)
```

## Reference

- https://www.brinckerhoff.org/clements/2214-csc530/Files/gsv-constraints-2008.pdf
