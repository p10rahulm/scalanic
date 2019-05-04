
object tru extends Bool {
  def ifthenelse[T](t: => T, e: => T) = t
}

object fal extends Bool {
  def ifthenelse[T](t: => T, e: => T) = e
}


abstract class Bool {

  def ifthenelse[T](thn : => T,els: => T): T

  //  call as A && B
  def &&(B: => Bool):Bool = ifthenelse(B,fal)

  //  call as A || B
  def ||(B: => Bool):Bool = ifthenelse(tru,B)

  //  call as ! Ax
  def unary_! : Bool = ifthenelse(fal,tru)

  def ==(x:Bool):Bool = ifthenelse(x,x.unary_!)

  def !=(x:Bool):Bool = ifthenelse(x.unary_!,x)

}

