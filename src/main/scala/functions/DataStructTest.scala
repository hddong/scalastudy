package functions

/**
  * function:
  *    模式匹配和函数数据结构的学习 p24
  *    https://github.com/fpinscala/fpinscala
  */
object DataStructTest {
  //
  // +A表示协变参数,子类可通用
  //
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A] (head: A, tail: List[A]) extends List[A]
  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(d, ds) => d * product(ds)
    }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def dropOne[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n-1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case Cons(h, t) => Cons(h, dropWhile(t, f))
      case _ => l
    }
    def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case Cons(h, t) => Cons(h, dropWhile(t, f))
      case _ => l
    }

    //
    // foldRight 不是尾递归调用,会有爆内存隐患
    // foldLeft 采用尾递归
    def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
    @scala.annotation.tailrec
    def foldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(h, z))(f)
    }

    // 对所有的进行+1
    def add1(l: List[Int]): List[Int] = {
      foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
    }
    def map[A,B](l: List[A])(f: A => B): List[B] = {
      foldLeft(l, Nil: List[B])((h, t) => Cons(f(h), t))
    }
    def double2String(l: List[Double]): List[String] =
      map(l)(_.toString)
  }

  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4, 5, 6) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println(x)

    println(List.drop(List(1, 2, 3, 4, 5, 6), 3))

    //
    // dropWhile函数f必须申明参数类型
    // dropWhile2使用柯里化将参数分组,这时参数类型从左往右推导,f可以不用定义类型
    //
    println(List.dropWhile(List(1, 2, 3, 4, 5, 6), (x:Int) => x==3))
    println(List.dropWhile2(List(1, 2, 3, 4, 5, 6))(_==3))

  }
}
