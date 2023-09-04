package expression
import scala.collection.mutable.Map


object Expressions {

  def expand(order: List[List[String]]): Map[String, Int] = {
    var res: Map[String, Int] = Map()
    var priority: Int = 1
    for (lst <- order) {
      for (i <- lst) {
        res(i) = priority
      }
      priority += 1
    }
    res
  }

  def tokenize(exp: String,
               symbols: Map[String, Int],
               record: String): List[String] = {
    if (exp.isEmpty) {
      if (record.isEmpty) {
        return List[String]()
      } else {
        return List[String](record)
      }
    }
    for ((i, _) <- symbols) {
      var tmp: String = new String("")
      if (exp.startsWith(i)) {
        tmp = i
      } else if (exp.startsWith("(")) {
        tmp = "("
      } else if (exp.startsWith(")")) {
        tmp = ")"
      }
      if (!tmp.isEmpty) {
        if (record.isEmpty) {
          return tmp :: tokenize(exp.substring(tmp.length), symbols, "")
        } else {
          return record :: tmp :: tokenize(exp.substring(tmp.length), symbols, "")
        }
      }
    }
    if (exp.apply(0) != ' ') {
      return tokenize(exp.substring(1), symbols, record + exp.apply(0))
    } else {
      return tokenize(exp.substring(1), symbols, record)
    }
  }


  def evaluate[A](exp: String,
                  toA: String => A,
                  op: Map[String, (A, A) => A],
                  order: List[List[String]]): A = {

    val priorities: Map[String, Int] = expand(order)
    val tokens = tokenize(exp, priorities, "")
    println(tokens)
    val postfix_stk: Stack[String] = new Stack()
    val eval_stk: Stack[A] = new Stack()
    var postfix: List[String] = List()

    for (token <- tokens) {
      if (priorities.keySet.exists(_ == token)) {
        if (postfix_stk.isEmpty() || postfix_stk.top() == "(" || priorities(token) < priorities(postfix_stk.top())) {
          postfix_stk.push(token)
        } else {
          while (!postfix_stk.isEmpty() && postfix_stk.top() != "(" && priorities(token) >= priorities(postfix_stk.top())) {
            var v2 = eval_stk.pop()
            var v1 = eval_stk.pop()
            eval_stk.push(op(postfix_stk.pop)(v1, v2))
          }
          postfix_stk.push(token)
        }
      } else if (token == "(") {
        postfix_stk.push("(")
      } else if (token == ")") {
        while (!postfix_stk.isEmpty() && postfix_stk.top() != "(") {
          var v2 = eval_stk.pop()
          var v1 = eval_stk.pop()
          eval_stk.push(op(postfix_stk.pop)(v1, v2))
        }
        if (postfix_stk.isEmpty()) {
          println("EXCEPTION: Unbalanced Parentheness")
        }
        postfix_stk.pop()
      } else {
        eval_stk.push(toA(token))
      }
    }
    while (!postfix_stk.isEmpty()) {
      var v2 = eval_stk.pop()
      var v1 = eval_stk.pop()
      eval_stk.push(op(postfix_stk.pop)(v1, v2))
    }

    eval_stk.top()
  }

  def evaluateArithmetic(expression: String): Double = {
    val pow = (a: Double, b: Double) => Math.pow(a, b)
    val mul = (a: Double, b: Double) => a * b
    val div = (a: Double, b: Double) => a / b
    val add = (a: Double, b: Double) => a + b
    val sub = (a: Double, b: Double) => a - b
    val operatorTable: Map[String, (Double, Double) => Double] = Map(
      "^" -> pow,
      "*" -> mul,
      "/" -> div,
      "+" -> add,
      "-" -> sub
    )
    val order = List(List("^"), List("*", "/"), List("+", "-"))
    evaluate(expression, (s: String) => s.toDouble, operatorTable, order)
  }

  def evaluateBoolean(expression: String): Boolean = {
    val and = (a: Boolean, b: Boolean) => a && b
    val or = (a: Boolean, b: Boolean) => a || b
    val xor = (a: Boolean, b: Boolean) => (a || b) && !(a && b)
    val implies = (a: Boolean, b: Boolean) => !(a && !b)
    val iff = (a: Boolean, b: Boolean) => (a && b) || (!a && !b)
    val operatorTable: Map[String, (Boolean, Boolean) => Boolean] = Map(
      "&&" -> and,
      "||" -> or,
      "xor" -> xor,
      "->" -> implies,
      "<>" -> iff
    )
    val order = List(List("&&"), List("||", "xor"), List("->", "<>"))
    evaluate(expression, (s: String) => s.toBoolean, operatorTable, order)
  }

  /*def main(args: Array[String]) = {
    println(evaluateBoolean("(true -> false)"))
  }
}*/



  def main(args: Array[String]) = {
    println(1)
    //println(tokenize("12+3&&   33( 42  34+2  34)&&  ++55", Map[String, Int](")" -> 0, "(" -> 0, "+" -> 1, "&&" -> 2), ""))

    println(evaluateArithmetic( " (1/3)+1"))
  }
}
