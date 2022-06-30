import org.scalatest._
import flatspec._
import matchers._
import Heap._
import cats.data._
import cats._
import cats.implicits._
import scala.util.Random

class ExplitMinTest extends AnyFlatSpec with should.Matchers {

  val heap = BinominalHeap.heap
  val exp = ExplicitMin[BinominalHeap.HeapT]
  val h = heap.empty[Int]
  val target = exp.empty[Int]

  type H1 = ExplicitMin[BinominalHeap.HeapT, Int]
  type H2 = BinominalHeap.HeapT[Int]

  def generateHeap: (H1, H2) =
    (1 to Random.nextInt(10000).abs.toInt)
      .map(_ => Random.nextInt)
      .foldLeft((target, h)) { case ((tt, hh), n) =>
        (tt.insert(n), hh.insert(n))
      }

  def emptyHeap[F[_]: Heap, A: Ordering](f: F[A]): List[(F[A], A)] = {
    def go[F[_]: Heap, A: Ordering](
        f: F[A],
        acc: List[(F[A], A)]
    ): Option[List[(F[A], A)]] = f.deleteMin
      .flatMap { case (ff, v) =>
        go(ff, acc :+ ((ff, v)))
      }
    go(f, List.empty).getOrElse(List.empty)
  }

  "findMin" should "returns same value of wrapee" in {
    val (a, b) = generateHeap
    a.findMin shouldBe b.findMin
  }

  "merge" should "returns same result of wrappee" in {
    val (a, b) = generateHeap
    val (aa, bb) = generateHeap
    emptyHeap(a merge aa) shouldBe emptyHeap(
      Heap[BinominalHeap.HeapT].merge(b, bb)
    )
  }

  "#deleteMin" should "returns same result of wrapee" in {

    val (hh, tt) = generateHeap
    emptyHeap(hh) shouldBe emptyHeap(tt)
  }

}
