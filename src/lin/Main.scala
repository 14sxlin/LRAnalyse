package lin

/**
  * Created by LinSixin on 2016/11/23.
  */
object Main extends App{
//  val vn = Array('S','E','A','B')
//  val vt = Array('a','b','c','d')
//  val rule = Array(
//    ('S' , "E") ,
//    ('E' , "aA"),
//    ('E' , "bB"),
//    ('A' , "cA"),
//    ('A' , "d" ),
//    ('B' , "cB"),
//    ('B' , "d")
//  )

  println("use: \"java -jar LR.jar inputFilePath outputFilePath\" to generate table")
  val hasParams = if(args.length==2) true else false
  var inputPath = "C:\\Users\\LinSixin\\Desktop\\LR\\input1.txt"
  var outputPath = "C:\\Users\\LinSixin\\Desktop\\LR\\output1.txt"
  if(hasParams)
  {
    inputPath = args(0)
    outputPath = args(1)
  }


  val data = InputTool.readDataStructFromFile(inputPath,LR.seperate)

  val vt = data._1.toArray
  val vn = data._2.toArray
  val rule = data._3.toArray

  val lr = new LR(vt,vn,rule,'.')
  val items = lr.genItemSet()
  lr.genClosures()

  var content = ""
  var rawContent = ""
  val info = lr.checkIsLR()
  info match{
    case Some(msg) =>
      println(msg)
      content+= msg
      rawContent += "不是 LR(0) 文法"
    case None =>
      content += lr.printVt()
      content += lr.printVn()
      content += "\n"
      println
      for( i <- lr.closures.indices)
      {
        content += "%5s".format("I"+i)
        val line = lr.printState(i) + "\r\n"
        rawContent += line
        content += line
        println()
      }
  }


//  lr.printRule()
  OutputTool.printToFile(outputPath,rawContent)
  OutputTool.printToTemp(lr.tempContent + "\n" +content)

}
