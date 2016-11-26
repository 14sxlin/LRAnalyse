package lin


import scala.collection.mutable.ArrayBuffer

/**
  * Created by LinSixin on 2016/11/23.
  */
class LR(val vt:Array[Char],
         val vn:Array[Char],
         val rule : Array[(Char,String)],
         val dot : Char) {


  var newVt:ArrayBuffer[Char] = new ArrayBuffer[Char]()
  for( c <- vt) newVt += c
  newVt += '#'
  val newVn = vn.filter( _!='S')

  var allV = newVt ++ newVn

  var firstItem :(Char,String) = _
  var closures = new ArrayBuffer[ArrayBuffer[(Char,String)]]() /*闭包集合 也就是状态*/
  var itemSet = scala.collection.mutable.ArrayBuffer[(Char,String)]()  /*规范项目族*/

  val move = "移进"
  val reduce = "规约"
  val succ = "可接受"
  var action = new ArrayBuffer[Array[(String,Int)]]/*第一个表示动作,第二表示输入*/

  /** 表示 状态1 from  状态2 to  输入符号 动作 */
  var moveInRelation = new ArrayBuffer[(Int,Int,Char,String)]()

  /** 状态  非终结符下标 跳转状态 **/
  var reduceRelation = new ArrayBuffer[(Int,Int,Int)]()

  var acc = (1,'#',0)

  var tempContent = ""

  def genItemSet()={
    for((k,v) <- rule) {
      val newMap = for( i <- 0 to v.length)
        yield (k, v.substring(0, i) + dot + v.substring(i))

      for((k:Char ,v:String) <- newMap) itemSet += ((k,v))
//      for((k:Char ,v:String) <- newMap) println(""+k+" "+v)
    }

//    for((k,v) <- itemSet) println(k+"  "+v)
    firstItem = itemSet(0)
    itemSet
  }

  def genClosures(): Unit ={

    /*生成第一个状态(闭包)*/
    var currentClosure = completeClosure(firstItem)
    closures += currentClosure
    var lastLength = -1
    while(lastLength!=closures.length){
      lastLength = closures.length
      for( i <- closures.indices){     // i 表示状态号
        for((k,v) <- closures(i)) { //对于每个闭包中的每个产生式 k -> v
        val nextChar:Char = dotNext(v).getOrElse('#')  //dot下一个符号
          if (vt.contains(nextChar)) {//下一个符号是终结符Vt E-> .aA
            val newValue = completeClosure((k,dotMove(v).get))
            if(AddToClosures(newValue)) //是否添加了新的状态
            {
              AddToMoveInRelation((i,closures.length-1,nextChar,move))
            }
            else{                   //前面有过了 查找一下在哪里
              AddToMoveInRelation((i,closures.indexOf(newValue),nextChar,move))
            }
          }
          else if(vn.contains(nextChar)){//下一个符号是非终结符Vn S->.E
              val newValue = completeClosure((k,dotMove(v).get))
              if(AddToClosures(newValue)) //添加新的状态
              {
                  AddToMoveInRelation((i,closures.length-1,nextChar,move))
              }
              else{               //前面有过了 查找一下在哪里
                AddToMoveInRelation((i,closures.indexOf(newValue),nextChar,move))
              }
          }else{//没有下一个符号了 要规约
            val reduceUseIndex = rule.indexOf((k,v.substring(0,v.length-1)))
            if(k=='S')
              acc  = (i, '#',reduceUseIndex)
            else
              AddToReduceRelation((i,-1,reduceUseIndex))
          }

        }

      }

    }
    tempContent += printAllClosure()+"\n"
    tempContent += printMoveInRelation() +"\n"
    tempContent += printlnReduce() +"\n"

  }

  def checkIsLR():Option[String] ={
    for( i <- moveInRelation.indices)
      for( j <- i+1 until moveInRelation.length)
        {
          if(moveInRelation(i)._1 == moveInRelation(j)._1
            && moveInRelation(i)._3 == moveInRelation(j)._3)
            {
              return Some(
                "I%d meet \"%c\" may goto I%d or I%d  : Not LR(0)".format(
                  moveInRelation(i)._1,
                  moveInRelation(i)._3,
                  moveInRelation(i)._2,
                  moveInRelation(j)._2
                )
              )
            }
        }
    None
  }


  def AddToClosures(closure:ArrayBuffer[(Char,String)]): Boolean ={
    if(!closures.contains(closure))
    {
      closures += closure
      return true
    }
    false
  }

  def AddToMoveInRelation(newRelation:(Int,Int,Char,String)) ={
    if(!moveInRelation.contains(newRelation))
     {
       moveInRelation += newRelation
       true
     }else false
  }

  def AddToReduceRelation(newRelation:(Int,Int,Int)) ={
    if(!reduceRelation.contains(newRelation))
     {
       reduceRelation += newRelation
       true
     }else false
  }


  def dotMove(right:String)={

    val indexOfDot = right.indexOf(dot)
    if(indexOfDot == right.length)
      None
    else{
      Some(""+right.substring(0,indexOfDot)
        +right.substring(indexOfDot+1,indexOfDot+2)+dot
        +right.substring(indexOfDot+2))
    }

  }

  /**点后面的下一个字符*/
  def dotNext(right:String) = {
    if(right.indexOf(dot)+1>=right.length)
      None
    else Some(right.charAt(right.indexOf(dot)+1))
  }


  /**
    * 以开始项目求单个完整闭包
    * @param firstItem 开始的项目
    */
  def completeClosure(firstItem:(Char,String))={
    var currentClosure = new ArrayBuffer[(Char,String)]()
    currentClosure += firstItem
    var lastLength = -1
    while(lastLength!=currentClosure.length){
      lastLength = currentClosure.length
      currentClosure = collectClosure(currentClosure)
    }
    currentClosure
  }

  /**收集闭包的项目**/
  def collectClosure(closure:ArrayBuffer[(Char,String)]) ={
    for((k,v) <- closure
        if vn.contains(dotNext(v).getOrElse('*')); //如果下一个是非终结符
        (newkey,newItem) <- itemSet
        if newkey == dotNext(v).get){//则把左部为该非终结符且 点是在一开始 的产生式加入
      if(newItem.indexOf(dot) == 0 && !closure.contains((newkey,newItem)))
        closure += ((newkey,newItem))
    }
    closure
  }

  def printAllClosure() = {
    var content = ""
    for( i <- closures.indices) {
      content += "I"+i+"  :  "
      for ((k, v) <- closures(i))
        content += ""+k + " -> " + v + "  "
      content+= "\r\n"
    }
    println(content)
    content

  }


  def printMoveInRelation() = {
    var content = ""
    for((from,to,value,action) <- moveInRelation)
      {
        content += "from: %3d to:%3d input:%3s do %5s\n".format(from,to,value,action)
      }
    println(content)
    content
  }

  def printVt()={
    var content = ""
    content += "%5s".format(" ")
    for( x <- newVt){
      content += "%5s".format(x)
    }
    print(content)
    content
  }

  def printVn() ={
    var content = ""
    for( x <- newVn) content += "%5s".format(x)
    print(content)
    content
  }

  def printlnReduce() ={
    var content = ""
    for((from,to,use) <- reduceRelation)
      content += "from : %3d  to: %3d  use : %3d\n".format(from,to,use)
    println(content)
    content
  }



  def printState(from:Int) ={
    var content = ""
    val mRelation = moveInRelation.filter( (r) => r._1 == from)
//    for(( from , to, input, action) <- mRelation)
//      println(from + " " + to + " " + input +" " +action)
    print("%5s".format("I"+from))

    var reduceMap = scala.collection.mutable.Map[Int,Int]()
    for((from,to,use) <- reduceRelation)
      reduceMap += (from -> use)


    for( i<- allV.indices)
    {

      findInMoveInRelation(allV(i),mRelation) match {
        case Some(index: Int) =>
          content += printTableUnit(mRelation(index))
        case _ =>
          if (reduceMap.contains(from) && i<newVt.length) {
            content += "%5s".format("r" + reduceMap(from) + LR.seperate)
            printf("%5s", "r" + reduceMap(from))
          }
          else if(from==acc._1 && (allV.indexOf('#') == i ))
          {
            content += "%5s".format(LR.accChar+LR.seperate)  //acc
            printf("%5s", "acc")
          }
          else {
            content += "%5s".format("@" + LR.seperate)
            printf("%5s", "@")
          }
      }
    }
    content

  }

  def findInMoveInRelation(target:Char, mrelation: ArrayBuffer[(Int,Int,Char,String)]) :Option[Int]={
    for( i <- mrelation.indices)
      if(target == mrelation(i)._3 )
        return Some(i)
    None

  }

  def findInReduceRelation(from:Int,mrelation:ArrayBuffer[(Int,Int,Int)]): Boolean ={
    for( i <- mrelation.indices)
      if(from == mrelation(i)._1 )
        return true
    false
  }

  def printTableUnit(r:(Int,Int,Char,String)) ={
    printf("%5s","s"+r._2)
    "%5s".format("s"+r._2 + LR.seperate)
  }

  def printRule() {
    for(i <- rule.indices)
      {
        println(""+i+" "+rule(i)._1+" -> "+rule(i)._2)
      }
  }



}

object LR{
  val seperate = "~"
  val accChar = "A"
}

