package towers.model.ai


import play.api.libs.json.{JsValue, Json}
import towers.model.GridLocation
import towers.model.physics.PhysicsVector

import scala.collection.mutable.Queue



class AIPlayer(id: String) {

  def computeMovement(jsonGameState: String): PhysicsVector = {
    // Do not edit this method. It will not be called during grading
    var path = getPath(jsonGameState)
    path = smoothPath(jsonGameState, path)
    pathToDirection(jsonGameState, path)
  }

  def getPath(jsonGameState: String): List[GridLocation] = {
    var walls : Set[Int] = Set()
    var explored: Map[Int, GridLocation] = Map()
    val gs : JsValue = Json.parse(jsonGameState)
    val wall_list = (gs \ "walls").as[List[JsValue]]
    val start_point = (gs \ "players").as[List[JsValue]]
    val gridsize = (gs \ "gridSize").as[Map[String, Int]]
    val base = (gs \ "base").as[Map[String, Int]]
    val grid_x = gridsize("x")
    val grid_y = gridsize("y")
    var start_x = 0
    var start_y = 0
    for (s <- start_point) {
      val cur_id = (s \ "id").as[String]
      if (cur_id == id) {
        start_x = (s \ "x").as[Double].toInt
        start_y = (s \ "y").as[Double].toInt
      }
    }
    val start_num : Int = start_y * grid_x + start_x
    val base_x = base("x")
    val base_y = base("y")
    var count = 0
    for (w <- wall_list) {
      val w_x = (w \ "x").as[Int]
      val w_y = (w \ "y").as[Int]
      walls = walls + (w_y * grid_x + w_x)
    }

    val toExplore: Queue[GridLocation] = new Queue()
    toExplore.enqueue(new GridLocation(start_x, start_y))
    explored = explored + (start_num->new GridLocation(start_x, start_y))

    while (!toExplore.isEmpty) {
      val nodeToExplore = toExplore.dequeue()
      val node_x = nodeToExplore.x
      val node_y = nodeToExplore.y
      if (node_x + 1 < grid_x) {
        val tmp = node_y * grid_x + (node_x + 1)
        if(!explored.contains(tmp) && !walls.contains(tmp)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x + 1, node_y))
        }
      }
      if (node_x - 1 >= 0) {
        val tmp = node_y * grid_x + (node_x - 1)
        if(!explored.contains(tmp) && !walls.contains(tmp)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x - 1, node_y))
        }
      }
      if (node_y + 1 < grid_y) {
        val tmp = (node_y + 1) * grid_x + node_x
        if(!explored.contains(tmp) && !walls.contains(tmp)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x, node_y + 1))
        }
      }
      if (node_y - 1 >= 0) {
        val tmp = (node_y - 1) * grid_x + node_x
        if(!explored.contains(tmp) && !walls.contains(tmp)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x, node_y - 1))
        }
      }
      count += 1
    }

    var res : List[GridLocation] = List(new GridLocation(base_x, base_y))
    var cur_num = base_y * grid_x + base_x

    while(cur_num != start_num) {
      var tmp_location : GridLocation = explored(cur_num)
      res = tmp_location::res
      cur_num = tmp_location.y * grid_x + tmp_location.x
    }
    res
  }

  def pathToDirection(jsonGameState: String, path: List[GridLocation]): PhysicsVector = {
    val gs : JsValue = Json.parse(jsonGameState)
    val cur_point = (gs \ "players").as[List[JsValue]]
    var cur_x = 0.0
    var cur_y = 0.0
    for (s <- cur_point) {
      val cur_id = (s \ "id").as[String]
      if (cur_id == id) {
        cur_x = (s \ "x").as[Double]
        cur_y = (s \ "y").as[Double]
      }
    }
    val base = (gs \ "base").as[Map[String, Int]]
    val base_x = base("x")
    val base_y = base("y")

    if (path.length <= 1) {
      new PhysicsVector(cur_x.toInt + 0.5 - cur_x, cur_y.toInt + 0.5 - cur_y)
    } else {
      new PhysicsVector(path(1).x + 0.5 - cur_x, path(1).y + 0.5 - cur_y)
    }
  }


  def smoothPath(jsonGameState: String, path: List[GridLocation]): List[GridLocation] = {
    var walls : Set[Int] = Set()
    var explored: Map[Int, GridLocation] = Map()
    val gs : JsValue = Json.parse(jsonGameState)
    val wall_list = (gs \ "walls").as[List[JsValue]]
    val start_point = (gs \ "players").as[List[JsValue]]
    val gridsize = (gs \ "gridSize").as[Map[String, Int]]
    val base = (gs \ "base").as[Map[String, Int]]
    val grid_x = gridsize("x")
    val grid_y = gridsize("y")
    var start_x = 0
    var start_y = 0
    for (s <- start_point) {
      val cur_id = (s \ "id").as[String]
      if (cur_id == id) {
        start_x = (s \ "x").as[Double].toInt
        start_y = (s \ "y").as[Double].toInt
      }
    }
    val start_num : Int = start_y * grid_x + start_x
    val base_x = base("x")
    val base_y = base("y")
    var count = 0
    for (w <- wall_list) {
      val w_x = (w \ "x").as[Int]
      val w_y = (w \ "y").as[Int]
      walls = walls + (w_y * grid_x + w_x)
    }

    val toExplore: Queue[GridLocation] = new Queue()
    toExplore.enqueue(new GridLocation(start_x, start_y))
    explored = explored + (start_num->new GridLocation(start_x, start_y))

    while (!toExplore.isEmpty) {
      val nodeToExplore = toExplore.dequeue()
      val node_x = nodeToExplore.x
      val node_y = nodeToExplore.y
      if (node_x + 1 < grid_x) {
        val tmp = node_y * grid_x + (node_x + 1)
        if(!explored.contains(tmp) && !walls.contains(tmp)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x + 1, node_y))
        }
      }
      if (node_x - 1 >= 0) {
        val tmp = node_y * grid_x + (node_x - 1)
        if(!explored.contains(tmp) && !walls.contains(tmp)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x - 1, node_y))
        }
      }
      if (node_y + 1 < grid_y) {
        val tmp = (node_y + 1) * grid_x + node_x
        if(!explored.contains(tmp) && !walls.contains(tmp)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x, node_y + 1))
        }
      }
      if (node_y - 1 >= 0) {
        val tmp = (node_y - 1) * grid_x + node_x
        if(!explored.contains(tmp) && !walls.contains(tmp)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x, node_y - 1))
        }
      }
      if (node_x + 1 < grid_x && node_y + 1 < grid_y) {
        val tmp = (node_y + 1) * grid_x + (node_x + 1)
        val tmp2 = node_y * grid_x + (node_x + 1)
        val tmp3 = (node_y + 1) * grid_x + node_x
        if(!explored.contains(tmp) && !walls.contains(tmp) && !walls.contains(tmp2) && !walls.contains(tmp3)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x + 1, node_y + 1))
        }
      }
      if (node_x - 1 >= 0 && node_y + 1 < grid_y) {
        val tmp = (node_y + 1) * grid_x + (node_x - 1)
        val tmp2 = node_y * grid_x + (node_x - 1)
        val tmp3 = (node_y + 1) * grid_x + node_x
        if(!explored.contains(tmp) && !walls.contains(tmp) && !walls.contains(tmp2) && !walls.contains(tmp3)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x - 1, node_y + 1))
        }
      }
      if (node_x + 1 < grid_x && node_y - 1 >= 0) {
        val tmp = (node_y - 1) * grid_x + (node_x + 1)
        val tmp2 = node_y * grid_x + (node_x + 1)
        val tmp3 = (node_y - 1) * grid_x + node_x
        if(!explored.contains(tmp) && !walls.contains(tmp) && !walls.contains(tmp2) && !walls.contains(tmp3)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x + 1, node_y - 1))
        }
      }
      if (node_x - 1 >= 0 && node_y - 1 >= 0) {
        val tmp = (node_y - 1) * grid_x + (node_x - 1)
        val tmp2 = node_y * grid_x + (node_x - 1)
        val tmp3 = (node_y - 1) * grid_x + node_x
        if(!explored.contains(tmp) && !walls.contains(tmp) && !walls.contains(tmp2) && !walls.contains(tmp3)) {
          explored = explored + (tmp->nodeToExplore)
          toExplore.enqueue(new GridLocation(node_x - 1, node_y - 1))
        }
      }
      count += 1
    }

    var res : List[GridLocation] = List(new GridLocation(base_x, base_y))
    var cur_num = base_y * grid_x + base_x

    while(cur_num != start_num) {
      var tmp_location : GridLocation = explored(cur_num)
      res = tmp_location::res
      cur_num = tmp_location.y * grid_x + tmp_location.x
    }
    res
    // TODO
    //path
  }

}
