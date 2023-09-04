package towers.model.game_objects

import play.api.libs.json.{JsValue, Json}
import towers.model.genetics.genes.Gene
import towers.model.genetics.GeneticAlgorithm
import towers.model.physics.PhysicsVector

class DodgeBallTower(val x: Int, val y: Int) extends GameObject {

  // The height at which projectiles are fired
  val height = 3.0

  // Towers can only fire at players closer than this distance from the tower
  val sightRange = 5.0

  // The magnitude of the velocity at which projectiles are fired
  val projectileVelocity = 5.0


  def findClosestPlayer(players: Map[String, Map[String, Double]]) = {
    var min_dis_square = Double.MaxValue
    var min_player: Map[String, Double] = null
    for (k <- players.keySet) {
      val player_x : Double = players(k)("x")
      val player_y : Double = players(k)("y")
      val tower_x : Double = x + 0.5
      val tower_y : Double = y + 0.5
      val cur_dis_square = math.pow(player_x - tower_x, 2) + math.pow(player_y - tower_y, 2)
      if (cur_dis_square < min_dis_square) {
        min_dis_square = cur_dis_square
        min_player = players(k)
      }
    }
    min_player
  }



  def fire(jsonGameState: String): List[Projectile] = {
    // TODO: Objective 2
    var gs : JsValue = Json.parse(jsonGameState)

    val tower_x = x + 0.5
    val tower_y = y + 0.5

    val players = (gs \ "players").as[List[JsValue]]
    if (players.isEmpty){
      List()
    } else {
      var min_dis_square = Double.MaxValue
      var closest_player_x : Double = 0.0
      var closest_player_y : Double = 0.0
      for (k <- players) {
        val player_x : Double = (k \ "x").as[Double]
        val player_y : Double = (k \ "y").as[Double]
        val tower_x : Double = x + 0.5
        val tower_y : Double = y + 0.5
        val cur_dis_square = math.pow(player_x - tower_x, 2) + math.pow(player_y - tower_y, 2)
        if (cur_dis_square < math.pow(sightRange, 2) && cur_dis_square < min_dis_square) {
          min_dis_square = cur_dis_square
          closest_player_x = player_x
          closest_player_y = player_y
        }
      }
      if (min_dis_square == Double.MaxValue) {
        List()
      } else {
        var projectile_velocity: PhysicsVector = new PhysicsVector(closest_player_x - tower_x, closest_player_y - tower_y)
        projectile_velocity = projectile_velocity.normal2d()
        projectile_velocity = new PhysicsVector(projectileVelocity * projectile_velocity.x, projectileVelocity * projectile_velocity.y)
        val p: Projectile = new Projectile(new PhysicsVector(tower_x, tower_y, height), projectile_velocity)
        List(p)
      }
    }
  }


  def aimFire(jsonGameState: String): List[Projectile] = {
    // TODO: Bonus Objective
    return List()
    var gs : JsValue = Json.parse(jsonGameState)

    val tower_x = x + 0.5
    val tower_y = y + 0.5

    val players = (gs \ "players").as[List[JsValue]]
    if (players.isEmpty){
      List()
    } else {
      var min_dis_square = Double.MaxValue
      var closest_player : Player = null
      for (k <- players) {
        val player_x : Double = (k \ "x").as[Double]
        val player_y : Double = (k \ "y").as[Double]
        val tower_x : Double = x + 0.5
        val tower_y : Double = y + 0.5
        val cur_dis_square = math.pow(player_x - tower_x, 2) + math.pow(player_y - tower_y, 2)
        if (cur_dis_square < math.pow(sightRange, 2) && cur_dis_square < min_dis_square) {
          min_dis_square = cur_dis_square
          closest_player = new Player(new PhysicsVector(player_x, player_y), new PhysicsVector((k \ "v_x").as[Double], (k \ "v_y").as[Double]))
        }
      }
      if (closest_player == null) {
        List()
      } else {
        if (closest_player.velocity.x == 0 && closest_player.velocity.y == 0) {
          var projectile_velocity = new PhysicsVector(closest_player.location.x - tower_x, closest_player.location.y - tower_y)
          projectile_velocity = projectile_velocity.normal2d()
          projectile_velocity = new PhysicsVector(projectileVelocity * projectile_velocity.x, projectileVelocity * projectile_velocity.y)
          val p: Projectile = new Projectile(new PhysicsVector(tower_x, tower_y, height), projectile_velocity)
          List(p)
        } else {
          val distance: Double = GeneticAlgorithm.geneticAlgorithm[Double](
            getFitnessFunction(closest_player),
            vectorIncubator,
            List(new Gene(0.0))
          )
          val dir_edge = math.sqrt(math.pow(closest_player.velocity.x, 2) + math.pow(closest_player.velocity.y, 2))
          val intersection_x = closest_player.location.x + distance / dir_edge * closest_player.velocity.x
          val intersection_y = closest_player.location.y + distance / dir_edge * closest_player.velocity.y
          val intersection = new PhysicsVector(intersection_x, intersection_y)
          var projectile_velocity: PhysicsVector = new PhysicsVector(intersection.x - tower_x, intersection.y - tower_y)
          projectile_velocity = projectile_velocity.normal2d()
          projectile_velocity = new PhysicsVector(projectileVelocity * projectile_velocity.x, projectileVelocity * projectile_velocity.y)
          val p: Projectile = new Projectile(new PhysicsVector(tower_x, tower_y, height), projectile_velocity)
          List(p)
        }
      }
    }
  }


  // Suggested Genetic Algorithm setup
  def getFitnessFunction(targetPlayer: Player): Double => Double = {
    val tower_location = new PhysicsVector(x + 0.5, y + 0.5)

    (distance : Double) => {
      //TODO: velocity = 0
      if (distance < 0) {
        Double.MinValue
      } else {
        val dir_edge = math.sqrt(math.pow(targetPlayer.velocity.x, 2) + math.pow(targetPlayer.velocity.y, 2))
        val intersection_x = targetPlayer.location.x + distance / dir_edge * targetPlayer.velocity.x
        val intersection_y = targetPlayer.location.y + distance / dir_edge * targetPlayer.velocity.y
        val intersection = new PhysicsVector(intersection_x, intersection_y)
        val projectile_move_distance = intersection.distance2d(tower_location)
        val player_move_distance = intersection.distance2d(targetPlayer.location)
        -math.abs(projectile_move_distance * targetPlayer.speed - player_move_distance * projectileVelocity)
      }
    }
  }

  def vectorIncubator(genes: List[Gene]): Double = {
    math.tan(genes(0).geneValue * math.Pi / 2)
  }

}
