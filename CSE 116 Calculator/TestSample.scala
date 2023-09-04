package calculator.tests

import calculator.controller.{AdditionAction, EqualAction, NumberAction}
import calculator.model.Calculator
import org.scalatest._

class TestSample extends FunSuite {

  val EPSILON: Double = 0.000001

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  test("Sample testing of the calculator") {
    val calculator: Calculator = new Calculator()

    // Since we don't need event information f0r (edit to avoid keyword) this program, setting the event to null
    // to test is fine. This allows us to automate testing without using the GUI
    new NumberAction(calculator, 1).handle(null)
    new NumberAction(calculator, 2).handle(null)

    new AdditionAction(calculator).handle(null)

    new NumberAction(calculator, 5).handle(null)

    new EqualAction(calculator).handle(null)

    val expected: Double = 17.0
    val actual: Double = calculator.displayNumber()
    assert(equalDoubles(actual, expected), actual)
  }

}
