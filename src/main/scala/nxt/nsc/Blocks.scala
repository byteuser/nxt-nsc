package nxt.nsc

import play.api.libs.json.Json

import scala.util.{Success, Try}
import scalaj.http._

object Blocks {
  // Pass start and end block numbers as params
  def main(args: Array[String]) {
    require(Try(args(0).toLong).isSuccess && Try(args(1).toLong).isSuccess)
    case class Forgers(pools: Map[String, Int], users: Map[String, Set[Long]]) {
      def aggregate: Map[String, Double] = pools.mapValues(_.toDouble) ++ users.mapValues(_.size.toDouble)
    }
    val blocksInfo = (args(0).toLong until args(1).toLong).map(getBlockInfo)
    val forgersInfo = blocksInfo.foldLeft[Try[Forgers]] {
      Try(Forgers(
        Seq("NXT-K5KL-23DJ-3XLK-22222", "NXT-8N9W-TN4F-YA2S-H5B7R", "NXT-TMVC-69YC-SJB4-8YCH7").map(_ -> 0).toMap,
        Map[String, Set[Long]]().withDefault(_ => Set[Long]())))
    } { (f, b) =>
      for {
        forgers <- f
        blockInfo <- b()
      } yield {
        if (forgers.pools.contains(blockInfo.generatorRS))
          forgers.copy(pools = forgers.pools.updated(blockInfo.generatorRS, forgers.pools(blockInfo.generatorRS) + 1))
        else
          forgers.copy(users = forgers.users.updated(blockInfo.generatorRS, forgers.users(blockInfo.generatorRS) + (blockInfo.timestamp / (24 * 60 * 60))))
      }
    }
    val newForgersEarnings = forgersInfo.map(_.aggregate)
    val newForgersEarningsAsDatabase = newForgersEarnings.map(x => Json.toJson[Map[String, Double]](x)).map(Json.prettyPrint)
    newForgersEarningsAsDatabase.map(saveDatabase(forgersDatabaseFilename)) match {
      case Success(_) => println("Completed successfully")
      case e => println(e)
    }
  }

  def getBlockInfo(blockHeight: Long): () => Try[BlockInfo] = {
    lazy val response = Try(Http("http://localhost:7876/nxt").params(Map("requestType" -> "getBlock", "height" -> blockHeight.toString)).asString.body)
    implicit val blockInfoReads = Json.reads[BlockInfo]
    () => response.map(Json.parse).map(_.as[BlockInfo])
  }

  case class BlockInfo(generatorRS: String, timestamp: Long)

}
