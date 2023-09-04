package calculator.model

class EqualState(ccc:Calculator) extends CalculatorState {
  override def pressNb(i: Int): Unit = {
    ccc.state = new LhsState(ccc)
    ccc.pressClear()
    ccc.pressNb(i)
  }

  override def pressAdd(): Unit = {
    ccc.state = new OpState(ccc)
    ccc.pressAdd()
  }

  override def pressSub(): Unit = {
    ccc.state = new OpState(ccc)
    ccc.pressSub()
  }

  override def pressMul(): Unit = {
    ccc.state = new OpState(ccc)
    ccc.pressMul()
  }

  override def pressDiv(): Unit = {
    ccc.state = new OpState(ccc)
    ccc.pressDiv()
  }

  override def pressDot(): Unit = {

  }

  override def pressEqual(): Unit = {
    ccc.lhs = ccc.op(ccc.lhs, ccc.rhs)
    ccc.showNumber = ccc.lhs
  }
}
