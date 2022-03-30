package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.{forAll, forAllShrink}

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap :
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[A]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(i, heap)
  )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of two elem heap") = forAll { (m: A, n: A) =>
    val minElem = m min n
    val heap = insert(m, insert(n, empty))
    findMin(heap) == minElem
  }

  property("insert into empty and delete min") = forAll { (n: A) =>
    val heap = insert(n, empty)
    val afterRemoveHeap = deleteMin(heap)
    isEmpty(afterRemoveHeap)
  }

  property("heap is sorted after finding and deleting") = forAll { (h: H) =>
    @tailrec
    def isOrdered(heap: H, min: A): Boolean =
      isEmpty(heap) || findMin(heap) < min || isOrdered(deleteMin(heap), findMin(heap))

    isEmpty(h) || isOrdered(deleteMin(h), findMin(h))
  }

  property("minimum of two melded heaps") = forAll { (h1: H, h2: H) =>
    val meldHeap = meld(h1, h2)

    def compareMin: Boolean =
      val meldMin = findMin(meldHeap)
      !isEmpty(h1) && meldMin == findMin(h1) || !isEmpty(h2) && meldMin == findMin(h2)

    isEmpty(meldHeap) || compareMin
  }

  property("1 elem heap became empty after delete") = forAll { (h: H) =>
    isEmpty(h) || isEmpty(deleteMin(h)) || findMin(h) != findMin(deleteMin(h))
  }

