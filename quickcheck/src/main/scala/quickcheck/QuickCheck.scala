package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      e <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(e, h)
  )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val min = if a < b then a else b
    val h = insert(a, insert(b, empty))
    findMin(h) == min
  }

  property("delete1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h)) == true
  }

  property("sorting") = forAll { (a: Int, b: Int, c: Int) =>
    val list = List(a, b, c)
    val h = insertAll(list, empty)
    val elements = deleteRecur(h, List())
    elements == list.sorted
  }

  property("melding") = forAll { (h1: H, h2: H) =>
    if isEmpty(h1) && isEmpty(h2)
    then true
    else if isEmpty(h1)
    then findMin(meld(h1, h2)) == findMin(h2)
    else if isEmpty(h2)
    then findMin(meld(h1, h2)) == findMin(h1)
    else {
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val min = if min1 < min2 then min1 else min2
      findMin(meld(h1, h2)) == min
    }
  }

  property("contains all inserted elements") = forAll { (a: Int, b: Int) =>
    val list = List(a, b)
    val h = insertAll(list, empty)
    val elements = deleteRecur(h, List())
    list.toSet == elements.toSet
  }

  def insertAll(elements: List[Int], h: H): H =
    elements match {
      case Nil => h
      case e :: es => insertAll(es, insert(e, h))
    }

  def deleteRecur(h: H, acc: List[A]): List[A] =
    if isEmpty(h) then acc
    else {
      val min = findMin(h)
      deleteRecur(deleteMin(h), acc :+ min)
    }
