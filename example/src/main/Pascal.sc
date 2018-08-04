def sum1(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}
println(sum1(x=>x)(2,4))
println("hello")

def fact(n:Int): Int = if (n <= 1) 1  else n*fact(n-1)

def product(f: Int => Int) (a:Int, b:Int): Int = {
  if (a >b) 1 else f(a)* product(f)(a+1,b)
}

def sum(f: Int=>Int)(a:Int, b:Int):Int = {
  if (a>b) 0 else f(a) + sum(f)(a+1,b)
}

def Reduce(f:Int=>Int, combine:(Int,Int)=>Int,zero:Int)(a:Int, b:Int):Int = {
  if (a >b) zero else combine(f(a), Reduce(f,combine,zero)(a+1,b))
}

def product2(f:Int=>Int)(a:Int,b:Int):Int = Reduce(f,(x,y)=>x*y,1)(a,b)
println(product(x=>x)(2,4))
println(fact(5))
println(product(x=>x)(2,5))

def fact2(c:Int): Int = product(x=>x)(2,c)

println(fact(8))
println(fact2(8))

class Rational(x:Int, y:Int) {
  def numer:Int = x
  def denom:Int  = y

  def add(that:Rational) = {
    new Rational(numer*that.denom + that.numer * denom, denom*that.denom)
  }

  def neg:Rational = {
    new Rational(numer * -1 , denom)
  }

  def sub(that: Rational) = {
    new Rational(numer*that.denom - that.numer * denom, denom*that.denom)
  }
   def print = {
     println(s"${numer}/${denom}")
   }
}

object Rationals {
  val first = new Rational(1,2)
  val second = new Rational(3,4)
  val third = new Rational(2,3)
  first.sub(second).sub(third).print
}
Rationals



