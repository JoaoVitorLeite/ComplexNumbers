package complexNumbers

import Num.Implicits._


/***
 * @author João Vitor Leite
 *
 * */

/***
 * Class to represent complex numbers
 * <p>
 *   Note: The default values of the parameters is zero
 * </p>
 *
 * More informations about complex numbers:
 * <p>
 *   https://www.mathsisfun.com/numbers/complex-numbers.html
 *   brilliant.org/wiki/complex-numbers/
 *   https://mathworld.wolfram.com/ComplexNumber.html
 * </p>
 *
 * @param real Real part of a complex number
 * @param img Imaginary part of a complex number
 */
class Complex[A](val real: A, val img: A)(implicit op: Num[A]){

  /***
   * Method for applying the addition of two complex numbers
   * @param other Receive other complex number to apply addition
   * @return Complex number as result of operation
   */
  def + (other: Complex[A]): Complex[A] =
    new Complex(real+other.real, img+other.img)

  /***
   * Auxiliary method for applying subtraction
   * @return A complex number with negative parameters
   */
  def unary_- :Complex[A]= new Complex(-real, -img)

  /***
   * Method for applying the subtraction of two complex numbers
   * @param other Recieve other complex number to apply subtraction
   * @return Complex number as result of operation
   */
  def - (other: Complex[A]): Complex[A] =
    this + -other

  /***
   * Method for applying the multiplication of two complex numbers
   * @param other Receive other complex number to apply multiplication
   * @return Complex number as result of operation
   */
  def * (other: Complex[A]): Complex[A] =
    new Complex(real*other.real - img*other.img, real*other.img + img*other.real)

  /***
   * Method for returning the conjugate of a complex number
   * @return The conjugate of a complex number
   */
  def conjungate: Complex[A] = new Complex(real,-img)

  /***
   * Method for applying the division of two complex numbers
   * @param other Receive other complex number to apply division
   * @return Complex number as result of operation
   */
  def / (other: Complex[A]): Complex[A] = {
    val num = this * other.conjungate
    val den = other * other.conjungate
    new Complex(num.real/den.real, num.img/den.real)
  }

  /***
   * Method for returning the absolute value of a complex number
   * @return The absolute value of a complex number
   */
  def modulus: A = op.sqroot(real*real + img*img)

  /***
   * Method for returning the real part of a complex number
   * @return Real part of a complex number
   */
  def realPart: Complex[A] = new Complex(real, op.zero)

  /***
   * Method for returning the imaginary part of a complex number
   * @return Imaginary part of a complex number
   */
  def imgPart: Complex[A] = new Complex(op.zero, img)

  /***
   * Method for returning the squre of the absolute value of a complex number
   * @return Square of the absolute value
   */
  def absoluteSquare: A = (this * this.conjungate).real

  /***
   * Method for returning the argument of a complex number
   * @return Arg of a complex number
   */
  def arg: A = op.acos(real/this.modulus)

  /***
   * Method for returnig the cartesion coordenates of a complex number
   * @return Cartesian coordenates of a complex number
   */
  def cartesionForm: (A, A) = (real, img)

  /***
   * Method for returning the polar form of a complex number
   * <p>
   *   Note: This method return the polar form as a string
   * </p>
   * @return Polar form of a complex number
   */
  def polarForm: String = s"$modulus * ( cos($arg) + i * sen($arg) )"

  /***
   * Method for returning the multiplicative inverse of a complex number
   * @return Multiplicative inverse of a complex number
   */
  def inverse: Complex[A] = new Complex(op.one,op.zero) / this

  /***
   * Method to compare two complex number
   * @param obj Any object
   * @return Return true if the complex numbers are equal, otherwise false
   */
  override def equals(obj: Any): Boolean = obj match {
    case cp: Complex[A] => if((this.real == cp.real) && (this.img == cp.img)) true else false
    case _ => false
  }

  /***
   * Method for returning the algebric form of a complex number
   * <p>
   *   Note: This method return the algebric form as a string
   * </p>
   * @return Algebric form of a complex number
   */
  override def toString: String = real + (if (img < op.zero) "-" + -img else "+" + img) + "*i"
}


