package Comparator

object SelectionSort {

  def intSelectionSort(inputData: List[Int],
                       comparator: (Int, Int)
                         => Boolean):List[Int] = {
    var data: List[Int] = inputData

    for(i <- data.indices) {
      var minFound = data.apply(i)
      var minIndex = i
      for(j <- i until data.size) {
        var currentValue = data.apply(j)

        if (comparator(currentValue, minFound)) {
          minFound = currentValue
          minIndex = j
        }
      }

      data = data.updated(minIndex, data.apply(i))
      data = data.updated(i, minFound)
    }

    data
  }

  def selectionSort[Type](inputData: List[Type],
                          comparator: (Type, Type)
                          => Boolean) : List[Type] = {
    var data: List[Type] = inputData
    for (i <- data.indices) {
      var minFound = data.apply(i)
      var minIndice = i
      for (j <- i until data.length) {
        val curValue = data.apply(j)
        if( comparator(curValue, minFound)) {
          minFound = curValue
          minIndice = j
        }
      }

      data = data.updated(minIndice, data.apply(i))
      data = data.updated(i, minFound)
    }

    data
  }

  def GeneticAlgorithm[T](fitness : T => Double,
                            func2 : List[Double] => T,
                            genes : List[Double])
  : T = {
    func2(genes)
  }


  def main(args:Array[String]) = {
    val numbers = List(5, -23, -8, 7, -4, 10)
    val numbersSorted = selectionSort(numbers, (a: Int, b: Int) => a < b)
    println(numbersSorted)
  }
}
