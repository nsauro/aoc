package y2023

import scala.io.Source

object Day19 extends App{

  val ShapeParameters = raw"""\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}""".r
  val FlowParameters = raw"""(.+)\{(.+)\}""".r

  val XLess = raw"""x<(\d+):(.+)""".r
  val XGreater = raw"""x>(\d+):(.+)""".r
  val MLess = raw"""m<(\d+):(.+)""".r
  val MGreater = raw"""m>(\d+):(.+)""".r
  val ALess = raw"""a<(\d+):(.+)""".r
  val AGreater = raw"""a>(\d+):(.+)""".r
  val SLess = raw"""s<(\d+):(.+)""".r
  val SGreater = raw"""s>(\d+):(.+)""".r

  val Des = raw"""([a-z]+)""".r

  val data = Source.fromResource("2023/19.data").getLines().toSeq

  val(flows, shapeParameters) = data.splitAt(data.indexOf(""))

  val shapes = shapeParameters.tail.map{
      case ShapeParameters(x,m,a,s) => Shape(x.toInt, m.toInt, a.toInt,s.toInt)
  }

  val flowMap = flows.foldLeft(Map.empty[String, Workflow]){
    case (acc, s) => {
      val workflow = s match{
        case FlowParameters(name, rawSteps) => {
          val steps = rawSteps.split(",").map{ rawStep =>
            if(rawStep.contains(":")){
              rawStep match{
                case XGreater(amt, des) => Step.xGreater(amt.toInt, Result(des))
                case XLess(amt, des) => Step.xLess(amt.toInt, Result(des))

                case MGreater(amt, des) => Step.mGreater(amt.toInt, Result(des))
                case MLess(amt, des) => Step.mLess(amt.toInt, Result(des))

                case AGreater(amt, des) => Step.aGreater(amt.toInt, Result(des))
                case ALess(amt, des) => Step.aLess(amt.toInt, Result(des))

                case SGreater(amt, des) => Step.sGreater(amt.toInt, Result(des))
                case SLess(amt, des) => Step.sLess(amt.toInt, Result(des))
              }
            }else{
              Step.identity(Result(rawStep))
            }
          }
          Workflow(name, steps)
        }
      }
      acc + (workflow.name -> workflow)
    }
  }


  println(shapes.map(exec(_, "in")).sum)

  def exec(s : Shape, workflow: String) : Int = {
    flowMap(workflow).exec(s) match{
      case Result.ACCEPTED => s.x + s.m + s.a + s.s
      case Result.REJECTED => 0
      case Result.Redirect(name) => exec(s, name)
    }
  }

  case class Shape(x:Int, m:Int, a:Int, s:Int)

  case class Workflow(name: String, steps:Seq[Step]){
    def exec(shape: Shape) : Result = {
      steps.collectFirst{
        case s if s.exec(shape).isDefined => s.exec(shape).get
      }.get
    }
  }

  trait Step {
    def exec(shape: Shape) : Option[Result]
  }

  object Step{

    def identity(r: Result) = new Step {
      override def exec(shape: Shape): Option[Result] = Some(r)
    }
    def aLess(a : Int, res: Result) = new Step {
      override def exec(shape: Shape): Option[Result] = Option.when(shape.a < a)(res)
    }

    def aGreater(a: Int, res: Result) = new Step {
      override def exec(shape: Shape): Option[Result] = Option.when(shape.a > a)(res)
    }

    def xLess(x: Int, res: Result) = new Step {
      override def exec(shape: Shape): Option[Result] = Option.when(shape.x < x)(res)
    }

    def xGreater(x: Int, res: Result) = new Step {
      override def exec(shape: Shape): Option[Result] = Option.when(shape.x > x)(res)
    }

    def mLess(m: Int, res: Result) = new Step {
      override def exec(shape: Shape): Option[Result] = Option.when(shape.m < m)(res)
    }

    def mGreater(m: Int, res: Result) = new Step {
      override def exec(shape: Shape): Option[Result] = Option.when(shape.m > m)(res)
    }

    def sLess(s: Int, res: Result) = new Step {
      override def exec(shape: Shape): Option[Result] = Option.when(shape.s < s)(res)
    }

    def sGreater(s: Int, res: Result) = new Step {
      override def exec(shape: Shape): Option[Result] = Option.when(shape.s > s)(res)
    }

  }
  sealed trait Result

  object Result{

    def apply(str : String) = str match{
      case "R" =>REJECTED
      case "A" => ACCEPTED
      case other => Redirect(other)
    }

    case object REJECTED extends Result
    case object ACCEPTED extends Result
    case class Redirect(flow: String) extends Result
  }

  ////////// part 2



  case class Range(xStart : Int, xEnd:Int, mStart:Int, mEnd:Int, aStart: Int, aEnd:Int, sStart: Int, sEnd: Int) {
    def envelopes(r: Range) = {
      xStart <= r.xStart &&
        xEnd >= r.xEnd &&
        mStart <= r.mStart &&
        mEnd >= r.mEnd &&
        aStart <= r.aStart &&
        aEnd >= r.aEnd &&
        sStart <= r.sStart &&
        sEnd >= r.sEnd
    }
  }

  val rawFlows = flows.foldLeft(Map.empty[String, String]) {
    case (acc, s) => {
      s match {
        case FlowParameters(name, rawSteps) => {
          acc + (name -> rawSteps)
        }
      }
    }
  }

  val allRanges = findRanges("in", Range(1,4000,1,4000,1,4000,1,4000)).distinct


  val filtered = allRanges.foldLeft(Seq.empty[Range]){ case(acc, r) =>
    if(allRanges.filterNot(_ == r).exists(_.envelopes(r))){
      println(s"removing $r")
      acc
    }else{
      acc :+ r
    }

  }


  filtered.foreach(println)

  val res = filtered.map { x =>
    (x.xEnd - x.xStart + 1).toLong * (x.mEnd - x.mStart + 1).toLong * (x.aEnd - x.aStart + 1).toLong * (x.sEnd - x.sStart + 1).toLong
  }.sum

  println(res)

  def findRanges(name : String, range: Range) : Seq[Range] = {

    val (_, acc) = rawFlows(name).split(",").foldLeft(range, Seq.empty[Range]) { case((temp, acc), rawString) =>
      rawString match {
        case XGreater(amt, "R") => {
          (temp.copy(xEnd = amt.toInt), acc)
        }
        case XGreater(amt, "A") => {
          (temp.copy(xEnd = amt.toInt), acc :+ (temp.copy(xStart = amt.toInt + 1)))
        }
        case XGreater(amt, des) => {
          (temp.copy(xEnd = amt.toInt), acc ++ findRanges(des, temp.copy(xStart = amt.toInt + 1)))
        }

        case XLess(amt, "R") => {
          (temp.copy(xStart = amt.toInt), acc)
        }
        case XLess(amt, "A") => {
          (temp.copy(xStart = amt.toInt), acc :+ (temp.copy(xEnd = amt.toInt - 1)))
        }
        case XLess(amt, des) => {
          (temp.copy(xStart = amt.toInt), acc ++ findRanges(des, temp.copy(xEnd = amt.toInt - 1)))
        }

        case MGreater(amt, "R") => {
          (temp.copy(mEnd = amt.toInt), acc)
        }
        case MGreater(amt, "A") => {
          (temp.copy(mEnd = amt.toInt), acc :+ (temp.copy(mStart = amt.toInt + 1)) )
        }
        case MGreater(amt, des) => {
          (temp.copy(mEnd = amt.toInt), acc ++ findRanges(des, temp.copy(mStart = amt.toInt + 1)))
        }

        case MLess(amt, "R") => {
          (temp.copy(mStart = amt.toInt), acc)
        }
        case MLess(amt, "A") => {
          (temp.copy(mStart = amt.toInt), acc :+ (temp.copy(mEnd = amt.toInt - 1)))
        }
        case MLess(amt, des) => {
          (temp.copy(mStart = amt.toInt), acc ++ findRanges(des, temp.copy(mEnd = amt.toInt - 1)))
        }

        case AGreater(amt, "R") => {
          (temp.copy(aEnd = amt.toInt), acc)
        }
        case AGreater(amt, "A") => {
          (temp.copy(aEnd = amt.toInt), acc :+ temp.copy(aStart = amt.toInt + 1))
        }
        case AGreater(amt, des) => {
          (temp.copy(aEnd = amt.toInt), acc ++ findRanges(des, temp.copy(aStart = amt.toInt + 1)))
        }

        case ALess(amt, "R") => {
          (temp.copy(aStart = amt.toInt), acc)
        }
        case ALess(amt, "A") => {
          (temp.copy(aStart = amt.toInt), acc :+ temp.copy(aEnd = amt.toInt - 1))
        }
        case ALess(amt, des) => {
          (temp.copy(aStart = amt.toInt), acc ++ findRanges(des, temp.copy(aEnd = amt.toInt - 1)))
        }

        case SGreater(amt, "R") => {
          (temp.copy(sEnd = amt.toInt), acc)
        }
        case SGreater(amt, "A") => {
          (temp.copy(sEnd = amt.toInt), acc :+ temp.copy(sStart = amt.toInt + 1))
        }
        case SGreater(amt, des) => {
          (temp.copy(sEnd = amt.toInt), acc ++ findRanges(des, temp.copy(sStart = amt.toInt + 1)))
        }

        case SLess(amt, "R") => {
          (temp.copy(sStart = amt.toInt), acc)
        }
        case SLess(amt, "A") => {
          (temp.copy(sStart = amt.toInt), acc :+ (temp.copy(sEnd = amt.toInt - 1)))
        }
        case SLess(amt, des) => {
          (temp.copy(sStart = amt.toInt), acc ++ findRanges(des, temp.copy(sEnd = amt.toInt - 1)))
        }

        case "R" => {
          (temp, acc)
        }
        case "A" => {
          (temp, acc :+ temp)
        }
        case Des(des) => {
          (temp, acc ++ findRanges(des, temp))
        }
      }
    }
    acc
  }
}
