package fp


object Main extends App {
  println("hello, fp")
  //val fibs: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

  val fib: LazyList[Long] = 0L #:: 1L #:: fib.zip(fib.tail).map(n => n._1 + n._2)

  fib take 64 foreach println
}
