package y2015

import scala.io.Source

object Day19 extends App {

  val data = Source.fromResource("2015/19.data").getLines().toSeq

  val Replacement = raw"""(\w+) => (\w+)""".r

  val replacements = data.foldLeft(Map.empty[String, Seq[String]]) {
    case (acc, str) =>
      str match {
        case Replacement(k, v) =>
          acc + (k -> (acc.getOrElse(k, Seq.empty) :+ v))
      }

  }

  val molecule =
    "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr"

  val distinctMolecules = replacements.foldLeft(Set.empty[String]) {
    case (acc, (k, v)) =>
      val matches = k.r.findAllMatchIn(molecule).map(x => x.start)

      matches.foldLeft(acc) { case (acc2, i) =>
        acc2 ++ v.map(x => molecule.patch(i, x, k.length))

      }
  }

  println(distinctMolecules.size)

}
