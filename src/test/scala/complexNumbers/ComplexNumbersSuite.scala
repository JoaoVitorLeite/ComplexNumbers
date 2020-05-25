package complexNumbers

import org.scalatest.FunSuite

class ComplexNumbersSuite extends FunSuite {

  test("Positive values"){
    val a = new Complex(2,7)
    assert(a == new Complex(2,7))
  }
  test("Negative values 1"){
    val a = new Complex(-2,7)
    assert(a == new Complex(-2,7))
  }
  test("Negative values 2"){
    val a = new Complex(-2,7)
    assert(a == new Complex(-2,7))
  }
  test("Equals"){
    val a = new Complex(Double.MaxValue, Double.MinValue)
    assert(a == new Complex(Double.MaxValue, Double.MinValue))
  }
  test("Neg"){
    val a = new Complex(-2, -17)
    assert(a == -new Complex(2,17))
  }
  test("Conjugate 1"){
    val a = new Complex(5,20)
    assert(a.conjungate == new Complex(5,-20))
  }
  test("Conjugate 2"){
    val a = new Complex(5,-20)
    assert(a.conjungate == new Complex(5,20))
  }
  test("Add 1"){
    val a = new Complex(1,2)
    val b = new Complex(5 , 4)
    assert(a+b == new Complex(6,6))
  }
  test("Add 2"){
    val a = new Complex(-1,2)
    val b = new Complex(5 , 4)
    assert(a+b == new Complex(4,6))
  }
  test("Add 3"){
    val a = new Complex(1,-2)
    val b = new Complex(5 , 4)
    assert(a+b == new Complex(6,2))
  }
  test("Add 4"){
    val a = new Complex(1,2)
    val b = new Complex(-5 , 4)
    assert(a+b == new Complex(-4,6))
  }
  test("Add 5"){
    val a = new Complex(1,2)
    val b = new Complex(5 , -4)
    assert(a+b == new Complex(6,-2))
  }
  test("Sub 1"){
    val a = new Complex(1,2)
    val b = new Complex(5 , 4)
    assert(a-b == new Complex(-4,-2))
  }
  test("Sub 2"){
    val a = new Complex(-1,2)
    val b = new Complex(5 , 4)
    assert(a-b == new Complex(-6,-2))
  }
  test("Sub 3"){
    val a = new Complex(1,-2)
    val b = new Complex(5 , 4)
    assert(a-b == new Complex(-4,-6))
  }
  test("Sub 4"){
    val a = new Complex(1,2)
    val b = new Complex(-5 , 4)
    assert(a-b == new Complex(6,-2))
  }
  test("Sub 5"){
    val a = new Complex(1,2)
    val b = new Complex(5 , -4)
    assert(a-b == new Complex(-4,6))
  }
  test("Mul 1"){
    val a = new Complex(1,2)
    val b = new Complex(5 , 4)
    assert(a*b == new Complex(-3,14))
  }
  test("Mul 2"){
    val a = new Complex(-1,2)
    val b = new Complex(5 , 4)
    assert(a*b == new Complex(-13,6))
  }
  test("Mul 3"){
    val a = new Complex(1,-2)
    val b = new Complex(5 , 4)
    assert(a*b == new Complex(13,-6))
  }
  test("Mul 4"){
    val a = new Complex(1,2)
    val b = new Complex(-5 , 4)
    assert(a*b == new Complex(-13,-6))
  }
  test("Mul 5"){
    val a = new Complex(1,2)
    val b = new Complex(5 , -4)
    assert(a*b == new Complex(13,6))
  }
  test("Div 1"){
    val a = new Complex(1,2)
    val b = new Complex(5 , 4)
    assert(a/b == new Complex(13D/41D,6D/41D))
  }
  test("Div 2"){
    val a = new Complex(-1,2)
    val b = new Complex(5 , 4)
    assert(a/b == new Complex(3D/41D,14D/41D))
  }
  test("Div 3"){
    val a = new Complex(1,-2)
    val b = new Complex(5 , 4)
    assert(a/b == new Complex(-3D/41D,-14D/41D))
  }
  test("Div 4"){
    val a = new Complex(1,2)
    val b = new Complex(-5 , 4)
    assert(a/b == new Complex(3D/41D,-14D/41D))
  }
  test("Div 5"){
    val a = new Complex(1,2)
    val b = new Complex(5 , -4)
    assert(a/b == new Complex(-3D/41D,14D/41D))
  }
  test("Properties conjugate 1"){
    val a = new Complex(-2 ,7)
    val b = new Complex(-20,17)
    assert((a+b).conjungate == (a.conjungate + b.conjungate))
  }
  test("Properties conjugate 2"){
    val a = new Complex(-2 ,7)
    val b = new Complex(-20,17)
    assert((a*b).conjungate == (a.conjungate * b.conjungate))
  }
  test("Properties absolute value 1"){
    val a = new Complex(5,-7)
    assert(a.conjungate.modulus == a.modulus)
  }
  test("Properties absolute value 2"){
    val a = new Complex(20,95)
    val b = new Complex(12,105)
    assert((a*b).modulus == a.modulus * b.modulus)
  }
  test("Properties absolute value 3"){
    val a = new Complex(2,7)
    assert(a.realPart.modulus <= a.modulus)
  }
  test("Properties absolute value 4(Cosine rule)"){
    val a = new Complex(5,67)
    val b = new Complex(-7, -59)
    assert((a+b).absoluteSquare == a.absoluteSquare + b.absoluteSquare + 2 * (a*b.conjungate).real)
  }
  test("Properties absolute value 5(Parallelogram law)"){
    val a = new Complex(5,28)
    val b = new Complex(4,2)
    assert(((a+b).absoluteSquare + (a-b).absoluteSquare) == 2 * (a.absoluteSquare + b.absoluteSquare))
  }
  test("Arg"){
    val a = new Complex(math.sqrt(3), 1)
    assert(a.arg - math.Pi/6 < 1E-25)
  }
  test("Cartesian form"){
    val a = new Complex(5,27)
    assert(a.cartesionForm == (5,27))
  }
  test("Polar form"){
    val a = new Complex(2,3)
    val p = a.modulus
    val arg = a.arg
    val s = s"$p * ( cos($arg) + i * sen($arg) )"
    assert(a.polarForm == s)
  }
  test("Inverse"){
    assert(new Complex(2,4).inverse == new Complex(1D/10D, -2D/10D))
  }
}
