//kth element
@scala.annotation.tailrec
def kth(list1: List[Int], position: Int): Int = {
  val counterArr = new Array[Int](1)
  counterArr(0) = 0
  list1 match {
    case Nil => -1
    case head :: rest =>
      counterArr(0) = counterArr(0) + 1
      if (position != counterArr(0)) kth(rest, position - 1) else head
  }
}
kth(List(14, 21, 30, 34, 85, 65), 4)

//Q. Check Palindrome.

@scala.annotation.tailrec
def isPalindrome(list2: List[Int]): Boolean = list2 match {
  case Nil => true
  case _ :: Nil => true
  case _ :: _ => list2.head == list2.last && isPalindrome(list2.tail.init)

}

isPalindrome(List(1, 2, 3))

//Q. REVERSE OF A LIST

def reverse(givenList: List[Int]): List[Int] = {

  @scala.annotation.tailrec
  def innerReverse(givenList: List[Int], newList: List[Int]): List[Int] = givenList match {
    case Nil => givenList
    case head :: Nil => head :: newList
    case first :: second :: rest =>
      innerReverse(second :: rest, first :: newList)
  }

  innerReverse(givenList, List.empty[Int])
}

reverse(List(4, 6, 10, 0))

//also, with one def

def reverseList(givenInputList: List[Int]): List[Int] = {
  givenInputList match {
    case Nil => givenInputList
    case head :: rest => reverseList(rest) ::: List(head)
  }
}

reverseList(List(5, 8, 2, 4, 9, 1))

//Q. First Even Number

@scala.annotation.tailrec
def firstEven(givenInputList: List[Int]): Int = {

  //givenInputList find(_%2==0)

  givenInputList match {
    case Nil => -1
    //case head :: Nil if head % 2 == 0 => head
    case head :: rest =>
      if (head % 2 == 0)
        head
      else
        firstEven(rest)
  }
}

firstEven(List(21, 5, 29, 7, 6))

//remove duplicates

def compress(givenList: List[Int]): List[Int] = {
  @scala.annotation.tailrec
  def compressRecursive(givenList: List[Int], newList: List[Int]): List[Int] = givenList match {
    case List() => givenList
    case first :: Nil => newList :+ first
    case first :: second :: rest => if (first == second) compressRecursive(second :: rest, newList) else compressRecursive(second :: rest, newList :+ first)
  }

  compressRecursive(givenList, List.empty[Int])
}
compress(List(1, 1, 2, 2, 4, 5, 5))

// find Second Maximum element.

def findSecondMax(givenList: List[Int], maximum: Int, secondMaximum: Int): Int = {
  val secondMaxValue = givenList match {
    case Nil => secondMaximum
    case head :: Nil => secondMaximum
    case first :: second :: rest =>
      if (first > second) {
        if (second > secondMaximum) findSecondMax(List(first) ::: rest, first, second)
        else
          findSecondMax(List(first) ::: rest, first, secondMaximum)
      }
      else if (first > secondMaximum)
             findSecondMax(List(second) ::: rest, second, first)
      else
        findSecondMax(List(second) ::: rest, second, secondMaximum)

    case _ => secondMaximum
  }
  secondMaxValue

}

val min = findSecondMax(List(2, 4, 1, 6), 0, 0)
println(min)

//Q. Add duplicate elements to list

def duplicate(givenList: List[Int]): List[Int] = {
  givenList match {
    case Nil => givenList
    case head :: rest => head :: head :: duplicate(rest)
  }
}
duplicate(List(2, 3, 5, 1, 8, 8))

//Drop Nth Element

def drop(n: Int, givenList: List[Int]): List[Int] = {

  @scala.annotation.tailrec
  def dropRecursive(newList: List[Int], givenList: List[Int]): List[Int] = {
    givenList match {
      case Nil => newList
      case head :: Nil => List(head)
      case head :: rest =>
        if (newList.length != n - 1) {
          dropRecursive(head :: newList, rest)
        }
        else
          newList.reverse ::: rest
    }

  }

  dropRecursive(List.empty[Int], givenList)
}

drop(3, List(1, 2, 3, 4, 5, 6))

//with using splitAt

def dropNth(n: Int, givenList: List[Int]): List[Int] = {

  givenList match {
    case Nil => givenList
    case _ :: _ =>
      val (list2, list3) = givenList.splitAt(n)
      val (list4, _) = list2.splitAt(n - 1)
      list4 ::: list3
  }
}
dropNth(3, List(3, 5, 1, 7))

