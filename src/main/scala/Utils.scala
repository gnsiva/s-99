object Utils {
  def time[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block
    (result, System.nanoTime() - t0)
  }

  def printTime[R](block: => R): R = {
    val (result, nanoseconds) = time(block)
    println(f"Elapsed time: $nanoseconds%,d ns")
    result
  }
}
