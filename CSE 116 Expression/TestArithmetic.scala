package tests

import org.scalatest._
import expression.Expressions._

import scala.collection.mutable.Map


class TestArithmetic extends FunSuite {
  test("expression") {

    var case1 = evaluateArithmetic("3 * 5 ^ 1 ")
    assert (case1 == 15)
    var case2 = evaluateArithmetic("10 - (8/12.0*6)/2-1")
    assert (case2 == 7.0)
    var case4 = evaluateArithmetic("3  +  4  *  2   /    (1 - 5)   ^ 2 ")
    assert (case4 == 3.5)
    var case5 = evaluateArithmetic("9 +   6 / 4 ")
    assert (case5 == 10.5)
    var case7 = evaluateArithmetic("(2 ^ 3 +(3 - 4))")
    assert (case7== 7)
    var case8 = evaluateArithmetic("2.5 + 8.0")
    assert (case8== 10.5)
    var case10 = evaluateArithmetic("(2^3+(3-4))")
    assert (case10== 7)
    var case11 = evaluateArithmetic("    7.2 + 1.2    ")
    assert (case11== 8.4)
    var case12 = evaluateArithmetic("2^3+(3-4)")
    assert (case12== 7)
    var case13 = evaluateArithmetic("2 - 3 + 4")
    assert (case13== 3)
    var case14 = evaluateArithmetic("6 / 2 * 4")
    assert (case14== 12)
    var case15 = evaluateArithmetic("2 + 4 - 3")
    assert (case15== 3)
    var case16 = evaluateArithmetic("4* 2 /2")
    assert (case16== 4)
    var case17 = evaluateArithmetic("1*(3+(5*3)-20)-1")
    assert (case17== -3)
    var case18 = evaluateArithmetic("(1/3)+1")
    assert (case18== 1.3333333333333333)



  }
}
