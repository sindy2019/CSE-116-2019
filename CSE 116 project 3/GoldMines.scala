package clicker.equipment

class GoldMines extends Equipment{

  this.name = "Gold Mine"

  override def goldPerClick(): Double = {
    0.0
  }

  override def goldPerSecond(): Double = {
    100.0 * this.numberOwned
  }

  override def costOfNextPurchase(): Double = {
    1000.0* Math.pow(1.10, numberOwned)
  }

}
