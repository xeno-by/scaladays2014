import scala.language.experimental.macros
import scala.reflect.core._
import scala.reflect.semantic._
import scala.reflect.semantic.errors.throwExceptions

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
  val result = join(new { val x = 2 }, new { val y = 2 })
  println((result.x, result.y))
}