import java.security.KeyStore.TrustedCertificateEntry

import com.sun.org.apache.xpath.internal.functions.FuncFalse

def abs(x:Double) = if (x < 0) -x else x


def sqrt(x:Double) = {
  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double, x: Double) = (guess + x / guess) / 2

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  sqrtIter(1.0, x)
}


def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)

def factorial(n: Int): Int = if(n == 0) 1 else n * factorial(n-1)

gcd(14,21)
factorial(5)

def pascal(c: Int, r: Int): Int =
  if (c==0 | c==r) 1 else (pascal(c-1,r-1)+pascal(c,r-1))
pascal(2,4)


def balance(chars: List[Char]): Boolean = {
  def balance_inner(chars: List[Char], sum : Int): Boolean =
    if (chars.isEmpty && sum == 0) true
    else if (chars.isEmpty && sum != 0) false
    else if (sum < 0) false
    else {
      if (chars.head == ')')  balance_inner(chars.tail, sum-1)
      else if (chars.head == '(') balance_inner(chars.tail, sum+1)
      else balance_inner(chars.tail, sum)
    }
  balance_inner(chars, 0)
    }

val abc = "(just an) example".toList

balance(abc)

def countChange(money: Int, coins: List[Int]): Int = {
  if (coins.isEmpty) 0
  else if (money - coins.head == 0) 1
  else if (money - coins.head < 0) 0
  else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}

val list = List(1, 2, 5)
countChange(7, list)
