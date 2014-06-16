package tests

import scala.reflect.core._
import scala.reflect.semantic._
import errors.throwExceptions

object Test extends App {
  import scala.language.reflectiveCalls
  val x = new { val x = 2 }
  val y = new { val y = 3 }
  val result = macros.Join(x, y)
  println(result.x)
  println(result.y)
  println(macros.Macros.add(2, 3))  

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
  val result2 = join(new { val x = 2 }, new { val y = 2 })
  println((result2.x, result2.y))
 }