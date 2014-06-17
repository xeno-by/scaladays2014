import scala.language.experimental.macros
import scala.meta._
import scala.meta.semantic._
import errors.throwExceptions

object Test extends App {
  def join[T, U](x: T, y: U): Any = macro {
    val xfields = x.tpe.vals.map(f => f -> q"xtemp")
    val yfields = y.tpe.vals.map(f => f -> q"ytemp")
    val getters = (xfields ++ yfields).map{ case (f, ref) => q"val ${f.name} = $ref.${f.name}" }
    c.whitebox(q"""
      val xtemp = $x
      val ytemp = $y
      new { ..$getters }
    """)
  }
  val result2 = join(new { val x = 2 }, new { val y = 3 })
  println((result2.x, result2.y))
}
