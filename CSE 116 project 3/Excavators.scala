package clicker.equipment

class Excavators extends Equipment{

  this.name = "Excavator"

  override def goldPerClick(): Double = {
    20.0 * this.numberOwned
  }

  override def goldPerSecond(): Double = {
    10.0 * this.numberOwned
  }

  override def costOfNextPurchase(): Double = {
    200 * Math.pow(1.10, numberOwned)
  }

}
