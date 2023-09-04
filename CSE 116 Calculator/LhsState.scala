package calculator.model

class LhsState(ccc : Calculator) extends CalculatorState{
  var a:Int = 10
  var b:Double = 1.0
  var c:Int = 1
  var d:Double = 0.9

  override def pressNb(i: Int): Unit = {
    ccc.lhs = ccc.lhs * a + i * b
    b = b / c
    ccc.showNumber = ccc.lhs
  }

  override def pressAdd(): Unit = {
    ccc.op = (a , b) => a + b
    ccc.state = new OpState(ccc)
  }

  override def pressSub(): Unit = {
    ccc.op = (a , b) => a - b
    ccc.state = new OpState(ccc)
  }

  override def pressMul(): Unit = {
    ccc.op = (a, b) => a * b
    ccc.state = new OpState(ccc)
  }

  override def pressDiv(): Unit = {
    ccc.op = (a, b) => a / b
    ccc.state = new OpState(ccc)
  }

  override def pressDot(): Unit = {
    a = 1
    b = b - d
    c = 10
    d = 0
  }

  override def pressEqual(): Unit = {

  }
}
