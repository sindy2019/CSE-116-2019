package tests

import clicker._
import clicker.equipment._
import org.scalatest._

class TestEquipment extends FunSuite{
  test("Equipment"){
    var case1 = new Excavators()
    case1.numberOwned = 4
    val a =case1.costOfNextPurchase()
    val b = case1.goldPerClick()
    val c = case1.goldPerSecond()


    assert(a == 292.8200000000001)
    assert (b == 80.0)
    assert (c == 40.0)

    var case2 = new Excavators()
    case2.numberOwned = 0
    val m = case2.costOfNextPurchase()
    val n = case2.goldPerClick()
    val o = case2.goldPerSecond()


    assert (m == 200)
    assert (n == 0)
    assert (o == 0)

    var case3 = new Shovels()
    case3.numberOwned = 3
    val d = case2.costOfNextPurchase()
    assert(d == 200)
    val e = case2.goldPerClick()
    assert (e == 0.0)
    val f = case2.goldPerSecond()
    assert (f == 0.0)

    var case4 = new Shovels()

    case4.numberOwned = 0
    val aa = case4.costOfNextPurchase()
    assert(aa == 10.0)
    val ab = case4.goldPerSecond()
    assert(ab == 0)
    val ac = case4.goldPerClick()
    assert(ac == 0)


    var case5 = new GoldMines()
    case5.numberOwned = 0
    val g = case5.costOfNextPurchase()
    assert (g == 1000.0)
    val h = case5.goldPerClick()
    assert (h == 0)
    val i = case5.goldPerSecond()
    assert (i == 0.0)

    var case6 = new GoldMines()
    case6.numberOwned = 4
    val ad = case6.costOfNextPurchase()
    assert(ad == 1464.1000000000004)
    val ae = case6.goldPerSecond()
    assert(ae == 400)
    val af = case6.goldPerClick()
    assert(af == 0)
  }
}


