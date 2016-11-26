package lin

import java.io.{File, FileOutputStream, OutputStream, PrintWriter}

/**
  * Created by LinSixin on 2016/11/23.
  */
object OutputTool {

  def printToFile(outputPath:String,content:String): Unit ={
    val file = new File(outputPath)
    if(!file.exists())
      file.createNewFile()

    val pw = new PrintWriter(new FileOutputStream(file,false))
    pw.print(content)
    pw.flush()
    pw.close()
  }

  def printToTemp(content:String) {
    printToFile("temp.txt",content)
  }

  def appendToTemp(content:String): Unit =
  {
    val file = new File("temp.txt")
    if(!file.exists())
      file.createNewFile()

    val pw = new PrintWriter(new FileOutputStream(file,true))
    pw.append(content)
    pw.flush()
    pw.close()
  }
}
