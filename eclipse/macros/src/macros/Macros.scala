package macros

import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

object Macros {
  def add_impl(c: Context)(n: c.Tree, m: c.Tree) = {
    import c.universe._
    val q"${nval: Int}" = n
    val q"${mval: Int}" = m
    if (mval == 0) q"$nval"
    else q"_root_.macros.Macros.add(${nval + 1}, ${mval - 1})"
  }

  def add(n: Int, m: Int): Int = macro add_impl
}

object Join {
  def impl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    def fields(tree: Tree) = tree.tpe.members.collect{ case m: TermSymbol if m.isGetter => m }
    val xfields = fields(x).map(f => f -> q"xtemp")
    val yfields = fields(y).map(f => f -> q"ytemp")
    val getters = (xfields ++ yfields).map{ case (f, ref) => q"val ${f.name} = $ref.${f.name}" }
    q"""
      val xtemp = $x
      val ytemp = $y
      new { ..$getters }
    """
  }

  def apply[T, U](x: T, y: U): Any = macro impl
}