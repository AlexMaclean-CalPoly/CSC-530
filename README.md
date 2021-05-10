# Autograder with Sketch
**805 Kitchen:** Alex MacLean, Bailey Wickham

## Progress

We partially implemented the pipeline described in the paper for a smaller subset of python. Given a student program, a teacher program, and a set of rewrite rules (all in a version of python with extra parentheses for easy s-expression parseing), we rewrite the student program into a mPy~ then generate a sketch file. This file can then be plugged into sketch to generate an `xml` file with the list of rules applied with the minimized weight.




## Example
Student program `examples/student.sexpy`
```py
(
    (x = (3 + 5))
    (y = (80 / 2))
    (return ((x * y) * 1))
)
```
Teacher program `examples/teacher.sexpy`
```py
(
    (x = (3 - 5))
    (y = (80 / 2))
    (return (0 - (x * y)))
)
```
Error model `examples/error-model.sexpy`
```py
(
    ((($ a) + ($ b)) -> (($ a) (? * -) ($ b)))
    (1 -> -1)
    ((($ a) / ($ b)) -> (($ b) / ($ a)))
)
```
In the error model syntax `($ id)` indicates that this element can be anything and it will be taken from the before tree and substituted into the after tree.
`(? terms ...)` indicates a choice.

### Demo
```bash
>> racket main.rkt examples/student.sexpy examples/teacher.sexpy examples/error-model.sexpy > out.sk
>> cat out.sk
```
```java
// much more boiler plate above ...

harness void main() {
    int totalCost = 0;

    MultiType result1 = teacher();
    MultiType result2 = student();

    if(choice585) totalCost++;
    if(choice581) totalCost++;
    if(choice583) totalCost++;
    if(choice587) totalCost++;

    assert MTEquals(result1, result2);
    minimize(totalCost);
}

bit choice585 = 0;
bit choice581 = 0;
bit choice583 = 0;
bit choice587 = 0;

MultiType student() {
    MultiType x = choices580();
    MultiType y = choices584(x);
    return binOpMT(binOpMT(x, y, MULT_OP), choices586(y, x), MULT_OP);
}

MultiType choices584(MultiType x) {
    if (??) {
        return binOpMT(new MultiType(val=80, flag=INTEGER), new MultiType(val=2, flag=INTEGER), DIV_OP);
    } else {
        choice585 = 1;
        return binOpMT(new MultiType(val=2, flag=INTEGER), new MultiType(val=80, flag=INTEGER), DIV_OP);
    }
}

MultiType choices580() {
    if (??) {
        return binOpMT(new MultiType(val=3, flag=INTEGER), new MultiType(val=5, flag=INTEGER), ADD_OP);
    } else {
        choice581 = 1;
        return binOpMT(new MultiType(val=3, flag=INTEGER), new MultiType(val=5, flag=INTEGER), choices582());
    }
}

MultiType choices582() {
    if (??) {
        return MULT_OP;
    } else {
        choice583 = 1;
        return SUB_OP;
    }
}

MultiType choices586(MultiType y, MultiType x) {
    if (??) {
        return new MultiType(val=1, flag=INTEGER);
    } else {
        choice587 = 1;
        return new MultiType(val=-1, flag=INTEGER);
    }
}

MultiType teacher() {
    MultiType x = binOpMT(new MultiType(val=3, flag=INTEGER), new MultiType(val=5, flag=INTEGER), SUB_OP);
    MultiType y = binOpMT(new MultiType(val=80, flag=INTEGER), new MultiType(val=2, flag=INTEGER), DIV_OP);
    return binOpMT(new MultiType(val=0, flag=INTEGER), binOpMT(x, y, MULT_OP), SUB_OP);
}
```
*Indentation in here was added by me, but otherwise the code is unchanged.*
```bash
>>  ~/sketch-1.7.6/sketch-frontend/sketch --fe-output-xml -- -n out.sk
```
```xml
SKETCH version 1.7.6
Benchmark = out.sk
=== BEGIN XML OUTPUT ===
<?xml version="1.0"?>
<hole_values>
    <hole_value line="94" col="-1"  name="H__1" type="int" value="0" />
    <hole_value line="103" col="-1"  name="H__2" type="int" value="0" />
    <hole_value line="85" col="-1"  name="H__0" type="int" value="1" />
    <hole_value line="112" col="-1"  name="H__3" type="int" value="0" />
</hole_values>
[SKETCH] DONE
Total time = 683
```
