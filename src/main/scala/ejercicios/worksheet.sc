import ejercicios.cap1.Capitulo1
import ejercicios.cap2.Capitulo2.parallel
object prueba {
  val f: Int => Double = (x) => x + 2
  val g: Double => Boolean = (y) => 2 * y > 6
  val comp: Int => Boolean = Capitulo1.compose1(g, f)
  val res: Boolean = comp(4)
  val res2: Option[(Int,Int)] = Capitulo1.fuse(Some(3), None)
  val res3: Boolean = Capitulo1.check(0	until	10)(40	/	_	>	0)



  val op1: Int = 3*3
  val op2: Double = 3.0 * 3.0

  val ejer22 = parallel(op1,op2)
}