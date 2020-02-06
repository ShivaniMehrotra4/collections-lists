//Q1.
def sumConditional ( map1 : Map[String,Int],str :String) : Int= {

  @scala.annotation.tailrec
  def sumRecursive(originalMap: Map[String,Int], sum : Int) :Int = {

    if (originalMap.isEmpty)
      sum
    else
    {
      if(originalMap.head._1.contains(str))
        sumRecursive(originalMap .- (originalMap.head._1),sum.+(originalMap.head._2))
      else
        sumRecursive(originalMap .- (originalMap.head._1),sum)
    }
  }
  sumRecursive(map1,0)
}
val map = Map("anurag" -> 24, "daniel" -> 23, "anushka" -> 30)
val str = "anu"
val result = sumConditional(map,str)

// Q2.
def check(map1 : Map[Int,List[String]]): Map[Int,List[String]] =
{
  @scala.annotation.tailrec
  def updateList(key: Int, oldList: List[String], newList : List[String]) : List[String] = {
    oldList match {
      case Nil => newList
      case head :: rest =>
        if(key %2 ==0)
        {
          updateList(key,rest,newList ::: List(head(0)+"even"))
        }
        else
        {
          updateList(key,rest,newList ::: List(head(0)+"odd"))
        }
    }
  }

  @scala.annotation.tailrec
  def updateMap(oldMap:Map[Int,List[String]], newMap : Map[Int,List[String]]) : Map[Int,List[String]] ={
    if(oldMap.isEmpty)
      newMap
    else
      updateMap(oldMap.-(oldMap.head._1),newMap.++(Map(oldMap.head._1 ->updateList(oldMap.head._1,oldMap.head._2,List()))))
  }

  updateMap(map1,Map())
}

val map1 = Map(1 -> List("Sunil","Laxmi"),2 -> List("Bhavya", "Sangeeta"),3 ->List("Arun","Sushmita"),4 -> List("Jamwant") )
check(map1)


def last(list: List[Int]):Int
= {
  list.foldLeft(0) { (acc,element) => element }
}
val l = List(1,2,5,7,2,8)
last(l)


def checkMap(map:Map[String,Int])