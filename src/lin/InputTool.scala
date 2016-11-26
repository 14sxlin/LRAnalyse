package lin

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by LinSixin on 2016/11/23.
  */
object InputTool {

  def readDataStructFromFile(filePath:String,seperate:String) = {
    var mode = 0

    var vt = ArrayBuffer[Char]()
    var vn = ArrayBuffer[Char]()
    var rule = ArrayBuffer[(Char,String)]()

    val source = Source.fromFile(filePath)
    var index = 0
    for(line <- source.getLines()){
      index += 1
      index match {
        case 1 =>
          for( str <- line.split(seperate)) vn += str.charAt(0)
        case 2 =>
          for( str <- line.split(seperate)) vt += str.charAt(0)
        case _ =>
          val array = line.split(seperate)
          rule += ((array(0).charAt(0),array(1)))
      }

    }
    (vt,vn,rule)
  }
}
