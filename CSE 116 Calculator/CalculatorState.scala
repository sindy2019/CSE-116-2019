package calculator.model

trait CalculatorState {
  def pressNb(i:Int):Unit
  def pressAdd():Unit
  def pressSub():Unit
  def pressMul():Unit
  def pressDiv():Unit
  def pressDot():Unit
  def pressEqual():Unit
}
