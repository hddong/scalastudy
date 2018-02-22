package functions

import scala.annotation.tailrec
/**
  * function:
  *    泛型函数测试
  * tip:
  *    同名函数由于类型不同并不影响函数运行, 应该是多态性质
  *    泛函数在scala中也成为多态函数
  *    泛型函数在比较时会自动校验类型是否匹配,类似as和key必须是同类型的
  *    匿名函数 f 传递执行逻辑
  */
object PantypeTest {
  def main(args: Array[String]): Unit = {
    val fields = Array("hello", "world", "this", "is", "my", "first", "test")
    val key = "this"
    println("查找字符串的结果是:" + findFirst(fields, key))

    val intField = Array(1, 2, 3, 4, 5, 6, 7)
    val intKey = 6
    println("查找泛型函数的结果是:" + findFirst(intField, (key:Int) => key == intKey))
  }

  // 原始函数
  def findFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def _loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else _loop(n+1)
    }
    _loop(0)
  }

  // 泛型函数
  def findFirst[A](as: Array[A], f: A => Boolean): Int = {
    @tailrec
    def _loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (f(as(n))) n
      else _loop(n+1)
    }
    _loop(0)
  }
}
