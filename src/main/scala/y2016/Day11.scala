package y2016

import scala.collection.mutable.ListBuffer

object Day11 extends App {


  /*val starting = Seq(
    Lab(
      0,
      0,
      Vector(
        Floor(Set.empty, Set(Microchip("Hydrogen"), Microchip("Lithium"))),
        Floor(Set(Generator("Hydrogen")), Set.empty),
        Floor(Set(Generator("Lithium")), Set.empty),
        Floor(Set.empty, Set.empty)
      )
    )
  )*/

  val starting = Seq(
    Lab(
      0,
      0,
      Vector(
        Floor(Set(Generator("Promethium")), Set(Microchip("Promethium"))),
        Floor(Set(Generator("Cobalt"), Generator("Curium"), Generator("Ruthium"), Generator("Plutonium")), Set.empty),
        Floor(Set.empty, Set(Microchip("Cobalt"), Microchip("Curium"), Microchip("Ruthium"), Microchip("Plutonium"))),
        Floor(Set.empty, Set.empty)
      )
    )
  )

  val winner = exec(starting, Set(starting.hashCode()))
  println(s"winner: $winner")

  def exec(labs: Seq[Lab], seen : Set[Int]) : Int = {

    val withoutBad = labs.filter(_.isValid)
    println(s"${withoutBad.size} -- ${seen.size}")
    withoutBad.find(_.isDone) match{
      case Some(winner) =>
        println(winner)
        winner.moves
      case None => {
        val next = withoutBad.flatMap(_.next)
        val withoutDupes = next.filterNot(x => seen.contains(x.hashCode()))
        val ids = withoutDupes.map(_.hashCode()).toSet
        exec(next, seen ++ ids)
      }
    }

  }


  case class Lab( elevator: Int, moves: Int, floors: Vector[Floor]) {
    override def toString: String = {
      s"""
        |E: $elevator
        |Moves: $moves
        |${floors.zipWithIndex.map{case(f, i) =>s"$i: $f"}.reverse.mkString("\n")}
        |""".stripMargin
    }

    def isValid : Boolean = {
      floors.forall(_.isValid)
    }
    def isDone: Boolean = floors.init.forall(_.isEmpty)

    def next: ListBuffer[Lab] = {
      val f = floors(elevator)
      val validFloors = getValidFloors

      val chips : Set[Set[Component]] = f.chips.map(Set(_))
      val generators : Set[Set[Component]] = f.generators.map(Set(_))
      val x: Set[Set[Component]] = (f.chips ++ f.generators).toSeq.combinations(2).map(_.toSet).toSet
      val pairs  = chips ++ generators ++ x
      val combos = for {
        floor <- validFloors
        pair <- pairs

      } yield {
        (floor, pair)
      }
      val b = combos.foldLeft(ListBuffer.empty[Lab]){ case (acc, x) =>
        val l = Lab(x._1,
          moves + 1,
          floors.zipWithIndex.map{case(f, i) =>
            if(i == x._1){
              f.add(x._2)
            }else if(i == elevator) {
              f.remove(x._2)
            }else{
              f
            }
          }
        )
        acc.addOne(l)
        }
      b
    }

    def getValidFloors : Seq[Int] = {
      val above = elevator + 1
      val below = elevator - 1
      val allEmptyUnderneath = (0 to below).forall(floors(_).isEmpty)
      Seq(above, below).filter{ x =>
        x >= 0 && x <= 3 && !(allEmptyUnderneath && x < elevator)
      }
    }
  }

  trait Component extends Product with Serializable{
    def element: String
  }

  case class Floor(generators: Set[Generator], chips: Set[Microchip]) {

    override val toString: String = {
      generators.mkString(" ") + " " + chips.mkString(" ")
    }

    lazy val isValid : Boolean = {
      generators.isEmpty || chips.forall(x => generators.exists(_.element == x.element))
    }

    def add(comps : Set[Component]) = {
      val (newG, newC) = comps.foldLeft((generators, chips)){ case((gs, cs), i) =>
        i match{
        case g : Generator => (gs + g, cs)
        case c : Microchip => (gs, cs + c )
      }
      }
      Floor(newG, newC)
    }

    def remove(comps :Set[Component]) = {
      val (newG, newC) = comps.foldLeft((generators, chips)){ case((gs, cs), i) =>
        i match {
          case g : Generator => (gs - g, cs)
          case c : Microchip => (gs, cs - c )
        }
      }
      Floor(newG, newC)
    }

    lazy val isEmpty = generators.isEmpty && chips.isEmpty



  }
  case class Generator(element: String) extends Component {
    override def toString: String = s"${element.substring(0, 2)}G"
  }

  case class Microchip(element: String) extends Component {
    override def toString: String = s"${element.substring(0, 2)}M"
  }

}
