object Test extends App {
  def join[T, U](x: T, y: U): Any = macro ???
  val result = join(new { val x = 2 }, new { val y = 3 })
  println((result.x, result.y))
}