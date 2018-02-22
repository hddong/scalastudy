/*
 * scala 测试代码
 */
package annotation
import scala.annotation
/**
  * function:
  *   尾递归测试
  *      递归算法需要保持调用堆栈，效率较低，如果调用次数较多，会耗尽内存或栈溢出。然而，尾递归可以克服这一缺点。
  *      尾递归是指递归调用是函数的最后一个语句，而且其结果被直接返回，这是一类特殊的递归调用。由于递归结果总是直接返回，
  *      尾递归比较方便转换为循环，因此编译器容易对它进行优化。
  * tips:
  *   尾递归版本最重要的就是找到合适的累加器，该累加器可以保留最后一次递归调用留在堆栈中的数据，积累之前调用的结果，
  *   这样堆栈数据就可以被丢弃，当前的函数栈可以被重复利用。
  */
object TailrecTest {
  def main(args: Array[String]): Unit = {
    println("----阶乘求解----")
    println("非尾递归:" + factorial(4))
    println("尾递归:" + factorialTailRecursive(4))

    println("----菲波那切数列----")
    println("非尾递归:" + fibonacci(4))
    println("尾递归:" + fibonacciTailRecursive(4))
  }

  //
  // 阶乘求解
  // tips:
  //    在这个例子中，变量acc就是累加器，每次递归调用都会更新该变量，直到递归边界条件满足时返回该值。
  //
  // 非尾递归的递归方式
  def factorial(n: BigInt): BigInt = {
    if (n <= 1) 1 else n * factorial(n-1)
  }
//  factorial(4)
//  --------------
//  4 * factorial(3)
//  4 * (3 * factorial(2))
//  4 * (3 * (2 * factorial(1)))
//  4 * (3 * (2 * 1)

  // 尾递归调用
  def factorialTailRecursive(n: BigInt): BigInt = {
    //标注, 编译时强制校准是否符合尾调用
    @annotation.tailrec
    def _loop(acc: BigInt, n: BigInt): BigInt = {
      if (n <= 1) acc else _loop(acc*n, n-1)
    }
    _loop(1, n)
  }
//  factorialTailRecursive(4)
//  --------------------------
//  _loop(1, 4)
//  _loop(4, 3)
//  _loop(12, 2)
//  _loop(24, 1)
  // tips:
  //   尾递归结尾不可以再做操作 类似 1 + _loop(1, n) 是不符合尾递归标准的

  //
  // 菲波那切数列
  // tips:
  //    1 1 2 3 5 8 13
  //  计算最后的一个数
  //
  // 非尾递归
  def fibonacci(n: Int): Int = {
    if (n <= 2) 1 else fibonacci(n-1) + fibonacci(n-2)
  }

  // 尾递归
  def fibonacciTailRecursive(n: Int): Int = {
    def _loop(acc1: Int, acc2: Int, n: Int): Int = {
      if (n <= 2) acc2 else _loop(acc2, acc1+acc2, n-1)
    }
    _loop(1, 1, n)
  }
}
