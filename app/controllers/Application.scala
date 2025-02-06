package controllers

import javax.inject._
import play.api._
import play.api.db.Database
import play.api.mvc._
import play.api.libs.json._
import scala.math._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

class Complex(val x: Double, val y: Double) { // val neighbors: Array[(Int, Int)]
  val neighbors: mutable.Set[Complex] = mutable.Set.empty[Complex]
  val magSq = x * x + y * y
  var iterNo: Int = 0
  var hasEscaped: Boolean = false
  def add(second: Complex): Complex = {
    val z = new Complex(x + second.x, y + second.y)
    z.iterNo = iterNo
    z.hasEscaped = hasEscaped
    z
  }
  def mul(second: Complex): Complex = {
    val xb = second.x
    val yb = second.y
    val z = new Complex(x * xb - y * yb, x * yb + y * xb)
    z.iterNo = iterNo
    z.hasEscaped = hasEscaped
    z
  }
  def iter(second: Complex): Complex = {
    val xNew = second.x * second.x - second.y * second.y + x
    val yNew = (second.x + second.x) * second.y + y
    new Complex(xNew, yNew)
  }
  def calcIterNo(maxIter: Int): (Int, Boolean) = {
    var (n, hasEscaped) = (0, false)
    var z = new Complex(0, 0)
    while (!(n >= maxIter || hasEscaped)) {
      if (z.magSq >= 4) hasEscaped = true
      z = iter(z)
      n += 1
    }
    (n, hasEscaped)
  }
  def color(maxIterNo: Int, hasEscaped: Boolean): String = {
    if (!hasEscaped) {
      "#000000"
    } else {
      val n = floor(255 * iterNo / maxIterNo).toInt
      var hexR = (if (n < 16) "0" else "") + Integer.toString(n, 16)
      var hexB = (if (255 - n < 16) "0" else "") + Integer.toString(255 - n, 16)
      s"#${hexR}00${hexB}"
    }
  }
}

class Grid(
  val size: Double,
  val nxOverTwo: Int,
  val maxIter: Int,
  val mag: Int,
  val c: Complex,
)
{
  var potentialEscapers: mutable.Set[Complex] = mutable.Set.empty[Complex]
  val mag2 = {
    pow(2, mag)
  }
  val xMin = {
    val number = c.x - 2.0 / mag2
    f"$number%.5f"
  }
  val xMax = {
    val number = c.x + 2.0 / mag2
    f"$number%.5f"
  }
  val yMin = {
    val number = c.y - 2.0 / mag2
    f"$number%.5f"
  }
  val yMax = {
    val number = c.y + 2.0 / mag2
    f"$number%.5f"
  }
  val rows = {
    var dxTimesTwo = 2.0 / nxOverTwo
    var dy = dxTimesTwo * sqrt(3) / 2
    val ny = floor(2.0 / dy).toInt
    var iy = -ny
    dxTimesTwo /= pow(2, mag).toDouble
    val dx = dxTimesTwo / 2
    val nx0 = floor(2.0 / pow(2, mag) / dx).toInt
    dy /= pow(2, mag).toDouble
    val rows: ArrayBuffer[Array[Complex]] = ArrayBuffer()
    var numCells = 0
    while (iy <= ny) {
      val row: ArrayBuffer[Complex] = ArrayBuffer()
      val isEven = iy % 2 == 0
      var y = iy * dy
      y += c.y
      val nx = nx0 - (if ((nx0 % 2 == 0) != isEven) 1 else 0)
      var ix = -nx
      while (ix <= nx) {
        numCells += 1
        val x = ix * dx + c.x
        var z = new Complex(x, y)
        if (iy == -ny || iy == ny || ix == -nx || ix == nx) potentialEscapers += z
        row += toDom(z)
        ix += 2
      }
      rows += row.toArray
      iy += 1
    }
    for ((row, i) <- rows.zipWithIndex) {
      for ((z, j) <- row.zipWithIndex) {
        if (j > 0) z.neighbors += row(j - 1)
        if (j < row.length - 1) z.neighbors += row(j + 1)
        if (i > 0) {
          if (j < rows(i - 1).length) z.neighbors += rows(i - 1)(j)
          val otherJ = j + (if (row.length < rows(i - 1).length) 1 else -1)
          if (otherJ < rows(i - 1).length && otherJ >= 0) {
            z.neighbors += rows(i - 1)(otherJ)
          }
        }
        if (i < rows.length - 1) {
          if (j < rows(i + 1).length) z.neighbors += rows(i + 1)(j)
          val otherJ = j + (if (row.length < rows(i + 1).length) 1 else -1)
          if (otherJ < rows(i + 1).length && otherJ >= 0) {
            z.neighbors += rows(i + 1)(otherJ)
          }
        }
      }
    }
    rows.toArray
  }
  val numberOfCells = {
    rows.map(_.length).sum
  }
  var maxIterNo = 0

  def setIterNo(maxIter: Int) = {
    var nextPE: mutable.Set[Complex] = mutable.Set.empty[Complex]
    while (potentialEscapers.size > 0) {
      println("size = ", potentialEscapers.size)
      for (z <- potentialEscapers) {
        val result = fromDom(z).calcIterNo(maxIter)
        // potentialEscapers -= z
        z.iterNo = result._1
        z.hasEscaped = result._2
        if (maxIterNo < z.iterNo) maxIterNo = z.iterNo
        if (z.hasEscaped) {
          for (zNn <- z.neighbors) {
            println("top of for over neighbors")
            if (zNn.iterNo == 0) nextPE += zNn
          }
        }
      }
      potentialEscapers = nextPE
    }


    for (row <- rows) {
      for (z <- row) {
        val result = fromDom(z).calcIterNo(maxIter)
        z.iterNo = result._1
        z.hasEscaped = result._2
        if (maxIterNo < z.iterNo) maxIterNo = z.iterNo
      }
    }
  }
  def toDom(z: Complex): Complex = {
    z.add(new Complex(-c.x, -c.y)).mul(new Complex(pow(2, mag).toDouble, 0.0)).add(new Complex(2.0, 2.0)).mul(new Complex(size / 2, 0))
  }
  def fromDom(z: Complex): Complex = {
    z.mul(new Complex(2 / size, 0)).add(new Complex(-2.0, -2.0)).mul(new Complex(pow(2, -mag).toDouble, 0.0)).add(new Complex(c.x, c.y))
  }
}

class Url(val nxOverTwoStr: String, val maxIterStr: String, val magStr: String, val cStr: String) {
  var nxOverTwo = 0
  var maxIter = 0
  var mag = 0
  var x = 0.0
  var y = 0.0
  private val error0 = "The url fragment "
  private val error1 = " cannot be parsed as "
  private val errorInteger = error1 + " an integer."
  private val errorNumber = error1 + " a number."
  private var messages: ArrayBuffer[String] = ArrayBuffer()
  def getMessages(): Array[String] = {
    try {
      nxOverTwo = nxOverTwoStr.toInt
    } catch {
      case e: NumberFormatException => messages += error0 + nxOverTwoStr + errorInteger
    }
    try {
      maxIter = maxIterStr.toInt
    } catch {
      case e: NumberFormatException => messages += error0 + maxIterStr + errorInteger
    }
    try {
      mag = magStr.toInt
    } catch {
      case e: NumberFormatException => messages += error0 + magStr + errorInteger
    }
    if (messages.isEmpty) {
      val cArr = cStr.replaceAll("\\s+", "").split(",")
      val xStr = cArr(0)
      if (cArr.length != 2) {
        messages += "The center " + cStr + " seems to have " + cArr.length.toString + " coordinate(s) instead of 2."
      } else {
        try {
          x = xStr.toDouble
        } catch {
          case e: NumberFormatException => messages += error0 + xStr + errorNumber
        }
        val yStr = cArr(1)
        try {
          y = yStr.toDouble
        } catch {
          case e: NumberFormatException => messages += error0 + yStr + errorNumber
        }
      }
    }
    messages.toArray
  }
}

@Singleton
class Application @Inject()(val controllerComponents: ControllerComponents, val database: Database) extends BaseController {

  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def results(nxOverTwoStr: String, maxIterStr: String, magStr: String, cStr: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val url = new Url(nxOverTwoStr, maxIterStr, magStr, cStr)
    val messages = url.getMessages()
    if (!messages.isEmpty) {
      BadRequest(views.html.error(messages))
    } else {
      val grid = new Grid(
        340.0,
        url.nxOverTwo,
        url.maxIter,
        url.mag,
        new Complex(url.x, url.y),
      )
      grid.setIterNo(grid.maxIter)
      Ok(views.html.results(grid))
    }
  }

  def jsonResults(nxOverTwoStr: String, maxIterStr: String, magStr: String, cStr: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    val url = new Url(nxOverTwoStr, maxIterStr, magStr, cStr)
    val messages = url.getMessages()
    if (!messages.isEmpty) {
      BadRequest(Json.toJson(Map("errors" -> messages)))
    } else {
      // Even those following is in px rather than arbs, this
      // is undone immediately by invocation of fromDom.
      val grid = (new Grid(
        340.0,
        url.nxOverTwo,
        url.maxIter,
        url.mag,
        new Complex(url.x, url.y),
      ))
      Ok(Json.obj("rows" -> grid.rows.map(_.map(z => {
        val zNonDom = grid.fromDom(z)
        Json.obj(
          "x" -> zNonDom.x,
          "y" -> zNonDom.y,
          "magSq" -> zNonDom.magSq,
          "iterNo" -> z.iterNo,
          "color" -> z.color(url.maxIter, z.hasEscaped),
          "hasEscaped" -> z.hasEscaped,
        )
      }))))
    }
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
