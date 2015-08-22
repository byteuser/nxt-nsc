package nxt.nsc

import scala.util.{Success, Try}
import scalaj.http.{Http, HttpResponse}

object Payouts {
  // Pass secretPhrase as param
  def main(args: Array[String]) {
    require(args.length == 1)
    val secretPhrase = args(0)
    calculate().map {
      case (payouts, newPeersEarnings) =>
        saveDatabase(peersDatabaseFilename)(convertDatabase(newPeersEarnings.toHashMap))
        saveDatabase(payoutsDatabaseFilename)(convertDatabase(payouts.mapValues(_.toDouble).toHashMap))
        payouts.map { case (account, sum) => pay(account, sum, secretPhrase) }.foreach(println)
    } match {
      case Success(_) => println("Completed successfully")
      case e => println(e)
    }
  }

  def calculate(): Try[(Map[String, Long], Map[String, Double])] = {
    val earnings = loadDatabase(peersDatabaseFilename)
    val lowerBound = 20.0
    val payouts = earnings.map(_.filter(_._2 > lowerBound).mapValues(_.toLong))
    val newPeersEarnings = earnings.map(_.mapValues {
      case d if d > lowerBound => d - Math.floor(d)
      case d => d
    })
    for {p <- payouts; npe <- newPeersEarnings} yield (p, npe.filter(_._2 > 0.0))
  }

  def pay(account: String, sum: Long, secretPhrase: String): Try[HttpResponse[String]] = {
    Try(Http("http://localhost:7876/nxt").postForm(Seq(
      "requestType" -> "transferCurrency",
      "secretPhrase" -> secretPhrase,
      "recipient" -> account,
      "currency" -> "1294380573514520412",
      "units" -> (sum * 10000L).toString,
      "feeNQT" -> "100000000",
      "deadline" -> "1440"
    )).asString)
  }
}
