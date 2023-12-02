package y2016

import scala.io.Source

object Day22 extends App{
    val NodeR = """/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T.*""".r
    
    val data = Source.fromResource("2016/22.data").getLines().toSeq.map{
        case NodeR(x, y, size, used, avail) => Node(x.toInt, y.toInt, size.toInt, used.toInt, avail.toInt)
    }

    val viable = (for {
        a <- data
        b <- data
    } yield {
        Option.when(a.used > 0 && a != b && a.used <= b.free)((a,b))
    }).flatten
    
    println(viable.size)



    val gridView = for{
        i <- 0 to 29
    } yield {
        data.filter(x => x.y == i).sortBy(_.x)
    }


    gridView.foreach{ row =>
        println(row.mkString(" "))
    }



    case class Node(x: Int, y: Int, size : Int, used:Int, free:Int){
        val usePercent = Math.round((used.toFloat / size.toFloat) * 100f)
        
        override def toString = {
            if(size > 100){
                "|"
            }else if(used == 0){
                "O"
            }else{
                 s"$used/$size"
            }
        } 
    }
  
}
