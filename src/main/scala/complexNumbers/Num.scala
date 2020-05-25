package complexNumbers

trait Num[A]{
  def plus (x:A, y:A): A
  def sub(x:A, y:A): A
  def mul(x:A, y:A): A
  def div(x:A, y:A): A
  def rm(x:A, y:A): A
  def zero: A
  def one: A
  def neg(x:A): A
  def sqroot(x:A): A
  def abs(x:A): A
  def pow(x:A, y:A): A
  def ltn(x:A, y:A): Boolean
  def ltneq(x:A, y:A): Boolean
  def gtn(x:A, y:A): Boolean
  def gtneq(x:A, y:A): Boolean
  def acos(x:A): A

  class Ops(lhs: A){

    def + (rhs: A): A = plus(lhs, rhs)
    def unary_- : A = neg(lhs)
    def - (rhs: A): A = sub(lhs, rhs)
    def * (rhs: A): A = mul(lhs, rhs)
    def / (rhs: A): A = div(lhs, rhs)
    def % (rhs: A): A = rm(lhs, rhs)
    def < (rhs: A): Boolean = ltn(lhs, rhs)
    def <= (rhs: A): Boolean = ltneq(lhs, rhs)
    def > (rhs: A): Boolean = gtn(lhs, rhs)
    def >= (rhs: A): Boolean = gtneq(lhs, rhs)
  }

  implicit def mkNumOps(lhs: A): Ops = new Ops(lhs)

}


object Num {

  trait ExtraImplicits{

    implicit def infixNumOps[A](x:A)(implicit num: Num[A]): Num[A]#Ops = new num.Ops(x)

  }

  object Implicits extends ExtraImplicits { }

  implicit object IntNum extends Num[Int]{
    override def plus(x: Int, y: Int): Int = x+y

    override def sub(x: Int, y: Int): Int = x-y

    override def mul(x: Int, y: Int): Int = x*y

    override def div(x: Int, y: Int): Int = x/y

    override def zero: Int = 0

    override def one: Int = 1

    override def rm(x: Int, y: Int): Int = x%y

    override def neg(x: Int): Int = -x

    override def sqroot(x: Int): Int = math.sqrt(x).toInt

    override def abs(x: Int): Int = if(x < 0) -x else x

    override def pow(x: Int, y: Int): Int = math.pow(x, y).toInt

    override def ltn(x: Int, y: Int): Boolean = if(x < y) true else false

    override def ltneq(x: Int, y: Int): Boolean = if(x <= y) true else false

    override def gtn(x: Int, y: Int): Boolean = if(x > y) true else false

    override def gtneq(x: Int, y: Int): Boolean = if(x >= y) true else false

    override def acos(x: Int): Int = math.acos(x).toInt
  }

  //implicit object IntNum extends IntNum with Ordering.IntOrdering

  implicit object FloatNum extends Num[Float]{
    override def plus(x: Float, y: Float): Float = x+y

    override def sub(x: Float, y: Float): Float = x-y

    override def mul(x: Float, y: Float): Float = x*y

    override def div(x: Float, y: Float): Float = x/y

    override def zero: Float = 0f

    override def one: Float = 1f

    override def rm(x: Float, y: Float): Float = x%y

    override def neg(x: Float): Float = -x

    override def sqroot(x: Float): Float = math.sqrt(x).toFloat

    override def abs(x: Float): Float = if(x < 0) -x else x

    override def pow(x: Float, y: Float): Float = math.pow(x, y).toFloat

    override def ltn(x: Float, y: Float): Boolean = if(x < y) true else false

    override def ltneq(x: Float, y: Float): Boolean = if(x <= y) true else false

    override def gtn(x: Float, y: Float): Boolean = if(x > y) true else false

    override def gtneq(x: Float, y: Float): Boolean = if(x >= y) true else false

    override def acos(x: Float): Float = math.acos(x).toFloat
  }

  //implicit object FloatNum extends FloatNum with Ordering.Float.TotalOrdering

  implicit object DoubleNum extends Num[Double]{
    override def plus(x: Double, y: Double): Double = x+y

    override def sub(x: Double, y: Double): Double = x-y

    override def mul(x: Double, y: Double): Double = x*y

    override def div(x: Double, y: Double): Double = x/y

    override def zero: Double = 0D

    override def one: Double = 1D

    override def rm(x: Double, y: Double): Double = x%y

    override def neg(x: Double): Double = -x

    override def sqroot(x: Double): Double = math.sqrt(x)

    override def abs(x: Double): Double = if(x < 0) -x else x

    override def pow(x: Double, y: Double): Double = math.pow(x,y)

    override def ltn(x: Double, y: Double): Boolean = if(x < y) true else false

    override def ltneq(x: Double, y: Double): Boolean = if(x <= y) true else false

    override def gtn(x: Double, y: Double): Boolean = if(x > y) true else false

    override def gtneq(x: Double, y: Double): Boolean = if(x >= y) true else false

    override def acos(x: Double): Double = math.acos(x)
  }

  //implicit object DoubleNum extends DoubleNum with Ordering.Double.TotalOrdering

  implicit object LongNum extends Num[Long]{
    override def plus(x: Long, y: Long): Long = x+y

    override def sub(x: Long, y: Long): Long = x-y

    override def mul(x: Long, y: Long): Long = x*y

    override def div(x: Long, y: Long): Long = x/y

    override def zero: Long = 0L

    override def one: Long = 1L

    override def rm(x: Long, y: Long): Long = x%y

    override def neg(x: Long): Long = -x

    override def sqroot(x: Long): Long = math.sqrt(x.toDouble).toLong

    override def abs(x: Long): Long = if(x < 0) -x else x

    override def pow(x: Long, y: Long): Long = math.pow(x,y).toLong

    override def ltn(x: Long, y: Long): Boolean = if(x < y) true else false

    override def ltneq(x: Long, y: Long): Boolean = if(x <= y) true else false

    override def gtn(x: Long, y: Long): Boolean = if(x > y) true else false

    override def gtneq(x: Long, y: Long): Boolean = if(x >= y) true else false

    override def acos(x: Long): Long = math.acos(x).toLong
  }

  //implicit object LongNum extends LongNum with Ordering.LongOrdering
}