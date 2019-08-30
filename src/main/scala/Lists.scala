object Lists {

  def last[T](list: List[T]): T = list.last
  def penultimate[T](list: List[T]): T = list(list.length - 2)
  def nth[T](n: Int, list: List[T]): T = list(n)
  def length[T](list: List[T]): Int = list.length
  def reverse[T](list: List[T]): List[T] = list.reverse

  def isPalindrome[T](list: List[T]): Boolean = {
    val output = list.zip(list.reverse).takeWhile(x => x._1 == x._2)
    output.length == list.length
  }

  def flatten[T](list: List[T]): List[Any] = {
    list.flatMap{
      case x : List[T] => flatten(x)
      case x => List(x)
    }
  }

  def compress[T](list: List[T]): List[T] =
    List(list.head) ++ list.tail
      .zip(list.dropRight(1))
      .filter{ case (ths, lst) => ths != lst }
      .map(_._1)

  def pack[T](list: List[T]): List[List[T]] = {
    // TODO - not happy with this one, too much mutability
    var output = List[List[T]]()
    var i = 0
    while (i < list.length) {
      var intermediateList = List(list(i))
      i += 1

      while (i < list.length && list(i) == intermediateList.last){
        intermediateList ++= List(list(i))
        i += 1
      }
      output ++= List(intermediateList)
    }
    output
  }

  def encode[T](list: List[T]): List[(Int, T)] =
    pack(list)
      .map{ sublist => (sublist.length, sublist.head) }

  def encodeModified[T](list: List[T]): List[Any] =
    encode(list)
      .map(x => if(x._1 > 1) x else x._2)

  def decode[T](encodedList: List[(Int, T)]): List[T] =
    encodedList
      .flatMap { case (count, value) => List.fill(count)(value) }

  def duplicate[T](list: List[T]): List[T] =
    list.flatMap(List.fill(2)(_))

  def duplicateN[T](n: Int, list: List[T]): List[T] =
    list.flatMap(List.fill(n)(_))

  def drop[T](n: Int, list: List[T]): List[T] =
    list
      .zip(Stream.from(1))
      .filter{ case (value, i) => i % n != 0 }
      .map(_._1)

  def split[T](n: Int, list: List[T]): (List[T], List[T]) =
    (list.slice(0, n), list.slice(n, list.length))

  def slice[T](i: Int, k: Int, list: List[T]): List[T] =
    list.slice(i, k)

  def rotate[T](n: Int, list: List[T]): List[T] = {
    val len = list.length
    (n until (n + len))
      .map(x =>
        if (x < 0) x + len
        else if (x >= len) x - len
        else x
      ).map(list(_))
        .toList
  }

  def removeAt[T](k: Int, list: List[T]): (List[T], T) =
    (list.indices
      .filter(_ != k)
      .map(list(_)).toList,
    list(k))

  def insertAt[T](newItem: T, k: Int, list: List[T]): List[T] =
    list.slice(0, k) ++ List(newItem) ++ list.slice(k, list.length)

  def range(start: Int, end: Int): List[Int] = (start to end).toList

  def randomSelect[T](n: Int, list: List[T]): List[T] = {
    // TODO - test missing
    val rng = new scala.util.Random(10)
    (1 to n)
      .map(_ => rng.nextInt(list.length))
      .map(list)
      .toList
  }

//  def combinations[T](k: Int, list: List[T]): List[List[T]] = {

  def lsort[T](list: List[List[T]]): List[List[T]] =
    list
      .map(x => (x.length, x))
      .sortBy(_._1)
      .map(_._2)

  def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
    val lengthFrequencyOrder = list
      .map(x => (x.length, 1))
      .groupBy(_._1)
      .mapValues(_.length)
      .toList
      .sortBy(_._2)
      .map(_._1)

    val listWithLengths = list.map(x => (x, x.length))

    lengthFrequencyOrder
      .flatMap{ len =>
        listWithLengths
          .filter(_._2 == len)
          .map(_._1)
      }
  }
}
