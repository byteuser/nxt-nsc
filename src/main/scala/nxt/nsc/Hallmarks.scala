package nxt.nsc

import play.api.libs.json.Json

import scala.util.{Success, Try}
import scalaj.http.Http

object Hallmarks {
  // Pass accepted versions as params
  // Run hourly
  def main(args: Array[String]) {
    val peersAccounts = getPeersHallmarks(args.toList).map(_.map(getAccountByHallmark).flatMap(_.toOption))
    val peersAccountToReps = peersAccounts.map(_.groupBy(identity).mapValues(_.length))
    case class PeerAccountInfo(account: String, balance: Long, reps: Int) {
      def earnings = reps * (
        if (balance > 9999) 3.5
        else if (balance > 999) 0.5
        else 0.02
        ) / 24
    }
    val peersAccountInfo = peersAccountToReps.map(_.map { case (account, reps) => PeerAccountInfo(account, getBalance(account).getOrElse(0), reps)})
    val peersEarnings = peersAccountInfo.map(x => x.map(pai => pai.account -> pai.earnings).toMap.toHashMap)
    val previousPeersEarnings = loadDatabase(peersDatabaseFilename)
    val newPeersEarnings = for {pe <- peersEarnings; ppe <- previousPeersEarnings} yield mergeDatabases(pe, ppe)
    val newPeersEarningsAsDatabase = newPeersEarnings.map(convertDatabase)
    newPeersEarningsAsDatabase.map(saveDatabase(peersDatabaseFilename)) match {
      case Success(_) => println("Completed successfully")
      case e => println(e)
    }
  }

  def getBalance(account: String): Try[Long] = {
    val response = Try(Http("http://localhost:7876/nxt").params(Map("requestType" -> "getBalance", "account" -> account)).asString.body)
    response.map(Json.parse).map(_ \ "balanceNQT").map(_.as[String]).map(_.toLong / 100000000)
  }

  def getAccountByHallmark(hallmark: String): Try[String] = {
    val response = Try(Http("http://localhost:7876/nxt").params(Map("requestType" -> "decodeHallmark", "hallmark" -> hallmark)).asString.body)
    response.map(Json.parse).map(_ \ "accountRS").map(_.as[String])
  }

  def getPeersHallmarks(acceptedVersions: List[String]): Try[List[String]] = {
    val response = Try(Http("http://localhost:7876/nxt").params(Map("requestType" -> "getPeers", "includePeerInfo" -> "true")).asString.body)
    implicit val peerInfoReads = Json.reads[PeerInfo]
    val peers = response.map(Json.parse).map(_ \ "peers").map(_.as[List[PeerInfo]])
    val recentPeers = peers.map(_.filter(_.version match {
      case Some(v) =>
        acceptedVersions.contains(v)
      case _ =>
        false
    }))
    recentPeers.map(_.flatMap(_.hallmark))
  }

  case class PeerInfo(hallmark: Option[String], version: Option[String])

}
