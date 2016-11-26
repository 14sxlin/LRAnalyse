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
  var closures = new ArrayBuffer[ArrayBuffer[(Char,String)]]() /*�հ����� Ҳ����״̬*/
  var itemSet = scala.collection.mutable.ArrayBuffer[(Char,String)]()  /*�淶��Ŀ��*/

  val move = "�ƽ�"
  val reduce = "��Լ"
  val succ = "�ɽ���"
  var action = new ArrayBuffer[Array[(String,Int)]]/*��һ����ʾ����,�ڶ���ʾ����*/

  /** ��ʾ ״̬1 from  ״̬2 to  ������� ���� */
  var moveInRelation = new ArrayBuffer[(Int,Int,Char,String)]()

  /** ״̬  ���ս���±� ��ת״̬ **/
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

    /*���ɵ�һ��״̬(�հ�)*/
    var currentClosure = completeClosure(firstItem)
    closures += currentClosure
    var lastLength = -1
    while(lastLength!=closures.length){
      lastLength = closures.length
      for( i <- closures.indices){     // i ��ʾ״̬��
        for((k,v) <- closures(i)) { //����ÿ���հ��е�ÿ������ʽ k -> v
        val nextChar:Char = dotNext(v).getOrElse('#')  //dot��һ������
          if (vt.contains(nextChar)) {//��һ���������ս��Vt E-> .aA
            val newValue = completeClosure((k,dotMove(v).get))
            if(AddToClosures(newValue)) //�Ƿ�������µ�״̬
            {
              AddToMoveInRelation((i,closures.length-1,nextChar,move))
            }
            else{                   //ǰ���й��� ����һ��������
              AddToMoveInRelation((i,closures.indexOf(newValue),nextChar,move))
            }
          }
          else if(vn.contains(nextChar)){//��һ�������Ƿ��ս��Vn S->.E
              val newValue = completeClosure((k,dotMove(v).get))
              if(AddToClosures(newValue)) //����µ�״̬
              {
                  AddToMoveInRelation((i,closures.length-1,nextChar,move))
              }
              else{               //ǰ���й��� ����һ��������
                AddToMoveInRelation((i,closures.indexOf(newValue),nextChar,move))
              }
          }else{//û����һ�������� Ҫ��Լ
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

  /**��������һ���ַ�*/
  def dotNext(right:String) = {
    if(right.indexOf(dot)+1>=right.length)
      None
    else Some(right.charAt(right.indexOf(dot)+1))
  }


  /**
    * �Կ�ʼ��Ŀ�󵥸������հ�
    * @param firstItem ��ʼ����Ŀ
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

  /**�ռ��հ�����Ŀ**/
  def collectClosure(closure:ArrayBuffer[(Char,String)]) ={
    for((k,v) <- closure
        if vn.contains(dotNext(v).getOrElse('*')); //�����һ���Ƿ��ս��
        (newkey,newItem) <- itemSet
        if newkey == dotNext(v).get){//�����Ϊ�÷��ս���� ������һ��ʼ �Ĳ���ʽ����
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

