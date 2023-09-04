package calculator.model


class Calculator() {
  var showNumber = 0.0
  var rhs = 0.0
  var lhs = 0.0
  var op = (a:Double, b:Double) => a + b
  var state: CalculatorState = new LhsState(this)

  def displayNumber(): Double = {
    this.showNumber
  }

  def pressNb(i:Int):Unit = {
    this.state.pressNb(i)
  }

  def pressDot():Unit = {
    this.state.pressDot()
  }

  def pressAdd():Unit = {
    this.state.pressAdd()
  }

  def pressSub():Unit = {
    this.state.pressSub()
  }

  def pressMul():Unit = {
    this.state.pressMul()
  }

  def pressDiv():Unit = {
    this.state.pressDiv()
  }

  def pressEqual():Unit = {
    this.state.pressEqual()
  }

  def pressClear():Unit = {
    showNumber = 0.0
    lhs = 0.0
    rhs = 0.0
    this.state = new LhsState(this)
  }


  //

}
