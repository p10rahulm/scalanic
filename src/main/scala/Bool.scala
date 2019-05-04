package idealized.scala

abstract class Boolean {

  def ifthenelse[T](thn : => T,els: => T): T

  //  call as A && B
  def &&(B: => Boolean):Boolean = ifthenelse(B,false)

  //  call as A || B
  def ||(B: => Boolean):Boolean = ifthenelse(true,B)

  //  call as ! Ax
  def unary_! : Boolean = ifthenelse(false,true)

  def ==(x:Boolean):Boolean = ifthenelse(x,x.unary_!)

  def !=(x:Boolean):Boolean = ifthenelse(x.unary_!,x)



}
