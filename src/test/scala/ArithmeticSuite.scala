import org.scalatest.FunSuite
import Utils._

class ArithmeticSuite extends FunSuite {
  import S99Int._

  test("P31 - isPrime"){
    assert(6.isPrime == false)
    assert(new S99Int(4).isPrime == false)
    assert(new S99Int(5).isPrime == true)
  }

  test("P32 - gcd"){
    assert(gcd(270, 192) == 6)
    assert(gcd(36, 63) == 9)
    assert(gcd(192, 270) == 6)
    assert(gcd(63, 36) == 9)
  }

  test("P33 - isCoprimeTo"){
    assert(35.isCoprimeTo(64) == true)
    assert(64.isCoprimeTo(35) == true)
    assert(35.isCoprimeTo(65) == false)
    assert(65.isCoprimeTo(35) == false)
  }

  test("P34 - totient"){
    assert(10.totient == 4)
    assert(5.totient == 4)
    assert(11.totient == 10)
    assert(1.totient == 1)  // This is correct, the s-99 answer is wrong and doesn't handle this case
    assert(2.totient == 1)
  }

  test("prime stream memoization test") {
    val (_, firstRun) = time(S99Int.isPrime(100))
    val (_, secondRun) = time(S99Int.isPrime(100))
    assert(firstRun > (secondRun / 5))
  }

  test("P35 - primeFactors"){
    assert(315.primeFactors == List(3, 3, 5, 7))
    assert(13.primeFactors == List(13))
  }

  test("P36 - primeFactorMultiplicity"){
    assert(315.primeFactorMultiplicity == List((3,2), (5,1), (7,1)))
  }

  test("P37 - phi"){
    assert(10.phi == 4)
  }

  test("P38 - comparison of phi and totient"){
    // The question says, if the prime factors of a number is known, then phi is much more efficient
    // To test this the prime factors are pre-calculated
    val v = 10000
    val primeFactorCounts = v.primeFactorMultiplicity
    println(primeFactorCounts)

    val (totientResult, totientTime) = time(v.totient)
    val (phiResult, phiTime) = time(v.phi(primeFactorCounts))

    assert(totientResult == phiResult)
    assert(totientTime > phiTime * 2) // at least twice as fast
  }

  test("P39 - listPrimesinRange"){
    assert(listPrimesinRange(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31))
  }

  test("P40 - goldbach"){
    assert(28.goldbach == (5, 23) || 28.goldbach == (23, 5))
  }


//  test("P41 - print goldbach list"){
//    //TODO: come up with a test for the values and not the print statement
//  }

  test("P41a - goldbachList") {
    // The question asks to print them, I am going to return tuples instead
    assert(goldbachList(9 to 20) ==
      List((10, 3, 7), (12, 5, 7), (14, 3, 11), (16, 3, 13), (18, 5, 13), (20, 3, 17)))
  }

  test("P41b - goldbachListLimited") {
    // The question asks to print them, I am going to return tuples instead
    assert(goldbachListLimited(1 to 2000, 50) ==
      List((992, 73, 919), (1382, 61, 1321), (1856,67,1789), (1928,61,1867)))
  }
}