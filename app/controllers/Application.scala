package controllers

import javax.inject._
import play.api._
import play.api.db.Database
import play.api.mvc._
import scala.math._
import scala.collection.mutable.ArrayBuffer

class Complex(val x: Double, val y: Double) {
  def getX: Double = x
  def getY: Double = y
  def mul(second: Complex): Complex = {
    val xb = second.getX
    val yb = second.getY
    val xc = x * xb - y * yb
    val yc = x * yb + y * xb
    new Complex(xc, yc)
  }
}

@Singleton
class Application @Inject()(val controllerComponents: ControllerComponents, val database: Database) extends BaseController {

  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def results(path: String) = Action {
    Ok(views.html.results(path))
  }

  def grid(size: Double, nxOverTwo: Int): Array[Complex] = {
    val dxTimesTwo = size / nxOverTwo
    val dy = dxTimesTwo * sqrt(3) / 2
    val ny = floor(size / dy)
    var iy = -ny
    val dx = dxTimesTwo / 2
    val points = ArrayBuffer()
    while (iy <= ny) {
      val isEven = iy % 2 == 0
      val y = iy * dy
      var nx = floor(sqrt(size * size - y * y) / dx)
      if ((ix % 2 == 0) != isEven) {
        nx =- 1
      }
      var ix = -nx
      while (ix <= nx) {
        val x = ix * dx
        points += new Complex(x, y)
        ix += 2
      }
      iy += 1
    }
    points
  }

  def db(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    // In this getting started app, we don't use a custom execution context to keep the code and configuration simple.
    // For real-world apps, consult the Play documentation on how to configure custom contexts and how to use them:
    // https://www.playframework.com/documentation/2.8.19/AccessingAnSQLDatabase#Using-a-CustomExecutionContext
    database.withConnection { connection =>
      val statement = connection.createStatement()
      statement.executeUpdate("CREATE TABLE IF NOT EXISTS ticks (tick timestamp)")
      statement.executeUpdate("INSERT INTO ticks VALUES (now())")

      val output = new StringBuilder();
      val resultSet = statement.executeQuery("SELECT tick FROM ticks")
      while (resultSet.next()) {
        output.append("Read from DB: " + resultSet.getTimestamp("tick") + "\n")
      }

      Ok(output.toString())
    }
  }
}
