package genetics
import genes._
import genetics.geometry._

object GeneticAlgorithm {

  def genRandomList(len : Int):List[Gene] = {
    var lst:List[Gene] = List()
    for(i <- 1 to len) {
      lst = lst :+ new Gene(scala.util.Random.nextDouble())
    }
    lst
  }

  def mergeSort[T](inputData: List[T], comparator:(T,T) => Boolean):List[T] = {
    if(inputData.length < 2) {
      inputData
    } else {
      val mid: Int = inputData.length / 2
      val (left, right) = inputData.splitAt(mid)
      val leftSorted = mergeSort(left, comparator)
      val rightSorted = mergeSort(right, comparator)

      merge(leftSorted, rightSorted, comparator)
    }
  }

  def merge[T](left: List[T], right: List[T], comparator:(T,T) => Boolean) = {

    var leftPointer = 0
    var rightPointer = 0

    var sortedList: List[T] = List()

    while(leftPointer < left.length && rightPointer < right.length) {
      if(comparator(left.apply(leftPointer),right.apply(rightPointer))) {
        sortedList = sortedList :+ left.apply(leftPointer)
        leftPointer += 1
      } else {
        sortedList = sortedList :+ right.apply(rightPointer)
        rightPointer += 1
      }
    }

    while (leftPointer < left.length) {
      sortedList = sortedList :+ left.apply(leftPointer)
      leftPointer += 1
    }
    while (rightPointer < right.length) {
      sortedList = sortedList :+ right.apply(rightPointer)
      rightPointer += 1
    }

    sortedList
  }

  def mutate(geneList: List[Gene]) = {
    var res = geneList
    for(i <- 0 until geneList.length) {
      var rand : Double = Math.random()
      var op : Int = scala.util.Random.nextInt(2)
      var changed : Double = geneList.apply(i).getValue()
      if(op == 2){
        println("mutate: exception")
      } else if (op == 1){
        changed -= rand / 100
      } else {
        changed += rand / 100
      }
      if(changed >= 1.0) {
        res
      } else {}
      if (changed < 0.0) {
        changed = 0.0
      } else {}

      res = res.updated(i, new Gene(changed))
    }
    res
  }

  def mate(male: List[Gene], female : List[Gene]) = {
    if(male.length != female.length){
      println("mate : exception")
    }
    var res = List[Gene]()
    for(i <- 0 until male.length) {
      val newGeneValue : Double = (male.apply(i).getValue() + female.apply(i).getValue()) / 2
      res :+= new Gene(newGeneValue)
    }
    res
  }

  def geneticAlgorithm[T](fitness :T => Double, animal: List[Gene] => T, genes: List[Gene]): T ={
    val nGene:Int = genes.length
    val nAnimal = 40
    var animalList : List[List[Gene]] = List[List[Gene]]()
    val fitnessList : List[Double] = List[Double]()

    for(i <- 0 to 20) {
      animalList :+= genRandomList(nGene)
    }

    for(n <- 1 to 10000) {
      var sortedList = mergeSort(animalList,(a:List[Gene], b:List[Gene]) => fitness(animal(a)) > fitness(animal(b)))

      if(n == 10000){
        return animal(sortedList.apply(0))
      }
      for(i <- 0 to 3) {
        animalList = animalList.updated(i, sortedList.apply(i))
      }

      animalList = animalList.updated(1, mutate(sortedList.apply(0)))
      animalList = animalList.updated(2, mutate(sortedList.apply(1)))
      animalList = animalList.updated(2, mutate(sortedList.apply(1)))


      var count = 4
      for(i <- 0 to 2) {
        for(j <- i + 1 to 3) {
          animalList = animalList.updated(count, mate(sortedList.apply(i), sortedList.apply(j)))
          count += 1
        }
      }
      if(count != 10){
        println("Algo: count wrong:" + count.toString)
      }



      for(i <- 10 to 19) {
        animalList = animalList.updated(i, genRandomList(nGene))
      }



    }


    animal(animalList.apply(0))
  }

  def normalize(input: Double) : Double = {
    if(input < 0){
      println("Normalize: negative")
    }
    //Math.atan(input) * 2 / Math.PI
    println("wrong branch")
    0
  }

  def denormalize(input : Double) : Double = {
    if(input < 0.0 || input >= 1.0){
      println("Denormalize: negative or larger than 1 : " + input.toString)
    }
    Math.tan((input - 0.5) * Math.PI)
  }


  def linearRegression(ps : List[Point]):Line = {
    var fitness = ( ln: Line) => {
      var cum : Double = 0.0
      for (p <- ps){
        cum += Math.abs(p.y - ln.evaluate(p.x))
      }
      -cum
    }

    var geneToLine = (lst : List[Gene]) => {
      val a : Double = denormalize(lst.apply(0).geneValue)
      val b : Double = denormalize(lst.apply(1).geneValue)
      new Line(a, b)
    }

    val predict: Line = geneticAlgorithm[Line](fitness, geneToLine, List(new Gene(0.0), new Gene(0.0)))

    return predict
  }

  def main(args: Array[String]): Unit = {

    val answer = new Line(1.0,0.0)
    val testPoints : List[Point]= List(
      new Point(1.0, answer.evaluate(1.0)),
      new Point(4.0, answer.evaluate(4.0)),
      new Point(8.0, answer.evaluate(8.0)),
      new Point(-5.0, answer.evaluate(-5.0)),
      new Point(100.0, answer.evaluate(100.0)),
      new Point(-45.0, answer.evaluate(-45.0))
    )

    val predict = linearRegression(testPoints)
    print(predict)
  }
}
