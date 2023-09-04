package clicker.equipment

class Shovels extends Equipment {

  this.name = "Shovel"

  override def goldPerClick(): Double = {
    1.0 * this.numberOwned
  }

  override def goldPerSecond(): Double = {
    0.0
  }

  override def costOfNextPurchase(): Double = {
    10 * Math.pow(1.05, numberOwned)
  }
}
