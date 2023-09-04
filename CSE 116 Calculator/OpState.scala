package calculator.model

class OpState(ccc: Calculator) extends CalculatorState {

  override def pressNb(i: Int): Unit = {
    ccc.state = new RhsState(ccc)
    ccc.pressNb(i)
  }

  override def pressAdd(): Unit = {
    ccc.op = (a, b) => a + b
    ccc.rhs = 0.0
  }

  override def pressSub(): Unit = {
    ccc.op = (a, b) => a - b
    ccc.rhs = 0.0
  }

  override def pressMul(): Unit = {
    ccc.op = (a, b) => a * b
    ccc.rhs = 0.0
  }

  override def pressDiv(): Unit = {
    ccc.op = (a, b) => a / b
    ccc.rhs = 0.0
  }

  override def pressDot(): Unit = {
    ccc.state = new RhsState(ccc)
    ccc.pressDot()
  }

  override def pressEqual(): Unit = {

  }
}
