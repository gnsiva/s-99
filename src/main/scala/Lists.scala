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

  def compress[T](list: List[T]): List[T] = {
    List(list.head) ++ list.tail
      .zip(list.dropRight(1))
      .filter{ case (ths, lst) => ths != lst }
      .map(_._1)
  }

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

  def encode[T](list: List[T]): List[(Int, T)] = {
    pack(list).map{ sublist =>
      (sublist.length, sublist.head)
    }
  }

  def encodeModified[T](list: List[T]): List[Any] = {
    encode(list)
      .map(x => if(x._1 > 1) x else x._2)
  }

  def decode[T](encodedList: List[(Int, T)]): List[T] = {
    encodedList
      .flatMap { case (count, value) => List.fill(count)(value) }
  }
}