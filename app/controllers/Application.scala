package controllers

import javax.inject._
import play.api._
import play.api.db.Database
import play.api.mvc._
import scala.math._
import scala.collection.mutable.ArrayBuffer

class Complex(val x: Double, val y: Double) {
  val magSq = x * x + y * y
  def calcIterNo(maxIter: Int): Int = {
    var n = 0
    var z = new Complex(0, 0)
    while (n < maxIter && z.magSq < 4) {
      z = iter(z)
      n += 1
    }
    if (n == maxIter) 0 else n
  }
  def mul(second: Complex): Complex = {
    val xb = second.x
    val yb = second.y
    val xc = x * xb - y * yb
    val yc = x * yb + y * xb
    new Complex(xc, yc)
  }
  def iter(second: Complex): Complex = {
    val xNew = second.x * second.x - second.y * second.y + x
    val yNew = (second.x + second.x) * second.y + y
    new Complex(xNew, yNew)
  }
}

class ComplexWithIterNo(x: Double, y: Double, val iterNo: Int) extends Complex(x, y) {
  def color(maxIterNo: Int): String = {
    if (iterNo == 0) {
      "#000000"
    } else {
      val n = floor(255 * iterNo / maxIterNo).toInt
      var hexR = (if (n < 16) "0" else "") + Integer.toString(n, 16)
      var hexB = (if (255 - n < 16) "0" else "") + Integer.toString(255 - n, 16)
      s"#${hexR}00${hexB}"
    }
  }
}

class Grid(val size: Double, val nxOverTwo: Int, val maxIter: Int) {
  val points = {
    val dxTimesTwo = 2.0 / nxOverTwo
    val dy = dxTimesTwo * sqrt(3) / 2
    val ny = floor(2.0 / dy)
    var iy = -ny
    val dx = dxTimesTwo / 2
    val points: ArrayBuffer[ComplexWithIterNo] = ArrayBuffer()
    while (iy <= ny) {
      val isEven = iy % 2 == 0
      val y = iy * dy
      var nx = floor(sqrt(4.0 - y * y) / dx)
      if ((nx % 2 == 0) != isEven) nx -= 1
      var ix = -nx
      while (ix <= nx) {
        val x = ix * dx
        val iterNo = (new Complex(x, y)).calcIterNo(maxIter)
        points += new ComplexWithIterNo(x, y, iterNo)
        ix += 2
      }
      iy += 1
    }
    points.toArray
  }
  val maxIterNo = {
    var maxIterNo = 0
    for (point <- points) {
      if (maxIterNo < point.iterNo) maxIterNo = point.iterNo
    }
    maxIterNo
  }
}

@Singleton
class Application @Inject()(val controllerComponents: ControllerComponents, val database: Database) extends BaseController {

  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def results(path: String, nxOverTwo: Int, maxIter: Int) = Action {
    Ok(views.html.results(new Grid(400.0, nxOverTwo, maxIter)))
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
