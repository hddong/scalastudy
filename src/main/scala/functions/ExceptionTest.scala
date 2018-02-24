package functions

import scala.{Option => _, Either => _, _}
object ExceptionTest {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    //
    // B:>A 表示 B类型是A的父类,A是B的子类型
    // default: => B 表示参数类型是B但是不是立刻求值(解释后续补上)
    def getOrElse[B>:A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    //
    // 这里先将Some[A]转为Some[Some[A]] 然后调用getOrElse 类型匹配
    def orElse[B>:A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => this
      case _ => None
    }

    def flatMap[B](f: A => Option[B]):Option[B] = this.map(f).getOrElse(None)

    //
    // f aa,bb => C
    // b map f => C
    // flat map aa => Option[C]
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a flatMap (aa => b map (bb => f(aa, bb)))
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap(hh => sequence(t) map(hh :: _))
    }

    def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

    //
    // for yield
    // 对最后一个做吗map之前的做flatmap
    def map_2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def main(args: Array[String]): Unit = {
    val a = Array(2, 3)
    val b = Array(5, 8)
    val res = for {
      aa <- a
      bb <- b
    } yield aa * bb
    println(res.mkString(", "))
  }
}
