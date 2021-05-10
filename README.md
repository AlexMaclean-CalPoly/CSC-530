# Autograder

We wrote a translator for the sketch language. Given an sexp style input, we take a student program, a teacher program, and a set of transformation rules and transform them into a mPy~ program. We then transform this into sketch format which is plugged into sketch itself. The output is an `xml` file with the list of rules applied with the minimized weight.


## Example
Student program:
```py
(
    (x = (3 + 5))
    (y = (80 / 2))
    (return ((x * y) * 1))
)
```
Teacher program:
```py
(
    (x = (3 - 5))
    (y = (80 / 2))
    (return (0 - (x * y)))
)
```
Error model:
```py
(
    ((($ a) + ($ b)) -> (($ a) (? * -) ($ b)))
    (1 -> -1)
)
```
A hole value 0 implies the path was taken, meaning three rules have been applied here.
Output:
```xml
=== BEGIN XML OUTPUT ===
<?xml version="1.0"?>
<hole_values>
    <hole_value line="101" col="-1"  name="H__2" type="int" value="0" />
    <hole_value line="83" col="-1"  name="H__0" type="int" value="0" />
    <hole_value line="92" col="-1"  name="H__1" type="int" value="0" />
</hole_values>
```
