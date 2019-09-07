

class S99Int(val start: Int) {
  import S99Int._

  def isPrime: Boolean =  S99Int.isPrime(start)

  def isCoprimeTo(b: Int): Boolean = gcd(start, b) == 1

  def totient: Int =
    if (start == 1)
      1
    else
      (1 until start).count(isCoprimeTo)

  def primeFactors: List[Int] = S99Int.primeFactors(start)

  def primeFactorMultiplicity: List[(Int, Int)] =
    primeFactors
      .groupBy(x => x)
      .mapValues(_.size)
      .toList
      .sortBy(_._1)
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  // using the Euclidean algorithm
  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0)
      a
    else
      gcd(b, a % b)

  private val primeGenerator = new Primes
  import primeGenerator._

  def isPrime(v: Int): Boolean = {
    if (primes.contains(v))
      true
    else if (v < primes.last)
      false
    else {
      while (v > primes.last)
        primesIterator.next()
      if (v == primes.last)
        true
      else
        false
    }
  }

  def primeFactors(v: Int, primesPosition: Int = 1): List[Int] = {
    if (isPrime(v)) List(v)
    else {
      val prime = primes(primesPosition)
      if (v % prime == 0)
        List(prime) ++ primeFactors(v / prime, primesPosition = 1)
      else
        primeFactors(v, primesPosition = primesPosition + 1)
    }
  }

  @scala.annotation.tailrec
  def listPrimesinRange(r: Range): List[Int] = {
    if (primes.last > r.last)
      primes.filter(x => r.head <= x && x <= r.last)
    else {
      primesIterator.next()
      listPrimesinRange(r)
    }
  }
}


class Primes {
  // could have used hashset for earlier questions but primefactors needs them in order
  var primes: List[Int] = List(1, 2, 3)

  // This iterator keeps calculating the next prime, and also updates the primes var list
  val primesIterator: Iterator[Int] = (Stream(1, 2, 3) ++
    Stream.from(3)
      .map(_ + 2)
      .flatMap { x =>
        if (!primes.tail.exists(x % _ == 0)) {
          primes ++= List(x)
          Some(x)
        } else
          None
    }
    ).iterator
}
