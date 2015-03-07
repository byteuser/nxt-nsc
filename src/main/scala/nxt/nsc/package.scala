package nxt

import play.api.libs.json.Json

import scala.collection.immutable.HashMap
import scala.reflect.io.File
import scala.util.Try

package object nsc {
  val peersDatabaseFilename = "peers.nsc.db.json"
  val forgersDatabaseFilename = "forgers.nsc.db.json"
  val payoutsDatabaseFilename = "payouts.nsc.db.json"

  implicit class MapExtensions[A, B](map: Map[A, B]) {
    def toHashMap = HashMap(map.toSeq: _*)
  }

  def loadDatabase(filename: String): Try[HashMap[String, Double]] = {
    Try(Json.parse(scala.io.Source.fromFile(filename).mkString).as[Map[String, Double]].toHashMap)
  }

  def saveDatabase(filename: String)(data: String) {
    File(filename).writeAll(data)
  }

  def convertDatabase(database: HashMap[String, Double]): String = {
    Json.prettyPrint(Json.toJson[Map[String, Double]](database))
  }

  def mergeDatabases(first: HashMap[String, Double], second: HashMap[String, Double]): HashMap[String, Double] = {
    first.merged(second)({ case ((k, v1), (_, v2)) => (k, v1 + v2)})
  }
}
