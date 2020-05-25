# ComplexNumbers

### Description

Class to represent complex numbers in Scala.

### About

This class was implemented using a generic type, which was implemented [here](https://github.com/JoaoVitorLeite/ComplexNumbers/blob/master/src/main/scala/complexNumbers/Num.scala), which supports some basic algebraic operations and is defined for types Int, Float, Double, Long. The tests made are of simple character, and for this the [Scala Test](https://www.scalatest.org/) was used. In total 38 tests were performed. 

###Project Structure

```bash
src
|   
|   
\---main
|   \---scala
|       \---complexNumbers
|               Complex.scala
|               Num.scala
|               
\---test
|    \---scala
|        \---complexNumbers
|                ComplexNumbersSuite.scala
|                
|
+---.gitignore
    build.sbt
    LICENSE
    README.md
```
### Test

To perform the tests it is necessary to be in the root directory(where the build.sbt file is), then use the `sbt test` command. The tests used FunSuite and assert's.

### IDE

The IDE used was [Intellij Idea](https://www.jetbrains.com/idea/).

### References

* [Link](https://www.mathsisfun.com/numbers/complex-numbers.html)
* [Link](brilliant.org/wiki/complex-numbers/)
* [Link](https://mathworld.wolfram.com/ComplexNumber.html)

### License

The LICENSE used is [Apache-2.0](https://github.com/JoaoVitorLeite/Rational/blob/master/LICENSE).

