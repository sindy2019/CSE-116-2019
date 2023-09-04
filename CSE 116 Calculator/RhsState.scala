package calculator.model

class RhsState(ccc:Calculator) extends  CalculatorState {
  var a:Int = 10
  var b:Double = 1
  var c:Int = 1
  var d:Double = 0.9

  override def pressNb(i: Int): Unit = {
    ccc.rhs = ccc.rhs * a + i * b
    b = b / c
    ccc.showNumber = ccc.rhs
  }

  override def pressAdd(): Unit = {
    ccc.lhs = ccc.op(ccc.lhs ,ccc.rhs)
    ccc.state = new OpState(ccc)
    ccc.pressAdd()
    ccc.showNumber = ccc.lhs
  }

  override def pressSub(): Unit = {
    ccc.lhs = ccc.op(ccc.lhs ,ccc.rhs)
    ccc.state = new OpState(ccc)
    ccc.pressSub()
    ccc.showNumber = ccc.lhs
  }

  override def pressMul(): Unit = {
    ccc.lhs = ccc.op(ccc.lhs ,ccc.rhs)
    ccc.state = new OpState(ccc)
    ccc.pressMul()
    ccc.showNumber = ccc.lhs
  }

  override def pressDiv(): Unit = {
    ccc.lhs = ccc.op(ccc.lhs ,ccc.rhs)
    ccc.state = new OpState(ccc)
    ccc.pressDiv()
    ccc.showNumber = ccc.lhs
  }

  override def pressDot(): Unit = {
    a = 1
    b = b - d
    c = 10
    d = 0.0
  }

  override def pressEqual(): Unit = {
    ccc.state = new EqualState(ccc)
    ccc.pressEqual()
  }
}
