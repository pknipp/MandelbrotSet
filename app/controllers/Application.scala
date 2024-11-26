package controllers

import javax.inject._
import play.api._
import play.api.db.Database
import play.api.mvc._
import scala.math._
import scala.collection.mutable.ArrayBuffer

class Complex(val x: Double, val y: Double) {
  val magSq = x * x + y * y
  def iter(second: Complex): Complex = {
    val xNew = second.x * second.x - second.y * second.y + x
    val yNew = (second.x + second.x) * second.y + y
    new Complex(xNew, yNew)
  }
  def calcIterNo(maxIter: Int): Int = {
    var n = 0
    var z = new Complex(0, 0)
    while (n < maxIter && z.magSq < 4) {
      z = iter(z)
      n += 1
    }
    if (n == maxIter) 0 else n
  }
}

class ComplexWithIterNo(override val x: Double, override val y: Double, val iterNo: Int) extends Complex(x, y) {
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
  def add(second: Complex): ComplexWithIterNo = new ComplexWithIterNo(x + second.x, y + second.y, iterNo)
  def mul(second: Complex): ComplexWithIterNo = {
    val xb = second.x
    val yb = second.y
    new ComplexWithIterNo(x * xb - y * yb, x * yb + y * xb, iterNo)
  }
}

class Grid(val size: Double, val nxOverTwo: Int, val maxIter: Int, val mag: Int, val c: Complex) {
  val rows = {
    val dxTimesTwo = 2.0 / nxOverTwo
    val dy = dxTimesTwo * sqrt(3) / 2
    val ny = floor(2.0 / dy)
    var iy = -ny
    val dx = dxTimesTwo / 2
    val rows: ArrayBuffer[Array[ComplexWithIterNo]] = ArrayBuffer()
    while (iy <= ny) {
      val row: ArrayBuffer[ComplexWithIterNo] = ArrayBuffer()
      val isEven = iy % 2 == 0
      val y = iy * dy
      println(iy, y)
      var nx = floor(sqrt(4.0 - y * y) / dx)
      if ((nx % 2 == 0) != isEven) nx -= 1
      var ix = -nx
      while (ix <= nx) {
        val x = ix * dx
        println(iy, ix, x)
        val z = (new Complex(x, y))
        var zWithIterNo = new ComplexWithIterNo(z.x, z.y, z.calcIterNo(maxIter))
        zWithIterNo = zWithIterNo.mul(new Complex(pow(2, -mag).toDouble, 0.0)).add(c)
        row += complexToDom(zWithIterNo)
        ix += 2
      }
      rows += row.toArray
      iy += 1
    }
    rows.toArray
  }
  val maxIterNo = {
    var maxIterNo = 0
    for (row <- rows) {
      for (point <- row) {
        if (maxIterNo < point.iterNo) maxIterNo = point.iterNo
      }
    }
    maxIterNo
  }
  def complexToDom(z: ComplexWithIterNo): ComplexWithIterNo = {
    z.add(new Complex(-c.x, -c.y)).mul(new Complex(pow(2, mag).toDouble, 0.0)).add(new Complex(2.0, 0)).mul(new Complex(size, 0))
  }
  def domToComplex(z: ComplexWithIterNo): ComplexWithIterNo = {
    z.mul(new Complex(1.0 / size, 0.0)).add(new Complex(-2.0, 0.0)).mul(new Complex(pow(2, -mag).toDouble, 0)).add(c)
  }
}

@Singleton
class Application @Inject()(val controllerComponents: ControllerComponents, val database: Database) extends BaseController {

  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def results(nxOverTwoStr: String, maxIterStr: String, magStr: String, cStr: String) = Action {
    val nxOverTwo = try {
      nxOverTwoStr.toInt
    } catch {
      case e: NumberFormatException => 0 // Or handle the error differently
    }
    val maxIter = try {
      maxIterStr.toInt
    } catch {
      case e: NumberFormatException => 0 // Or handle the error differently
    }
    val mag = try {
      magStr.toInt
    } catch {
      case e: NumberFormatException => 0 // Or handle the error differently
    }
    val cArr = cStr.replaceAll("\\s+", "").split(",")
    val xStr = cArr(0)
    val yStr = cArr(1)
    val x = try {
      xStr.substring(1).toDouble
    } catch {
      case e: NumberFormatException => 0.0 // Or handle the error differently
    }
    val y = try {
      yStr.substring(0, yStr.length - 1).toDouble
    } catch {
      case e: NumberFormatException => 0.0 // Or handle the error differently
    }
    Ok(views.html.results(new Grid(
      400.0,
      nxOverTwo,
      maxIter,
      mag,
      new Complex(x, y),
    )))
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
