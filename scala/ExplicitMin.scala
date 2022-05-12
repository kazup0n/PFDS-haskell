import scala.language.higherKinds
import scala.math.Ordering._

sealed trait ExplicitMin[F[_], A]
object ExplicitMin {
  final case class T[F[_]: Heap, A](a: A, h: F[A]) extends ExplicitMin[F, A]
  final case class E[F[_]: Heap, A]() extends ExplicitMin[F, A]
  implicit def apply[F[_]](implicit heap: Heap[F]): Heap[ExplicitMin[F, *]] =
    new Heap[ExplicitMin[F, *]] {
      type G[A] = ExplicitMin[F, A]

      override def empty[A]: G[A] = E[F, A]()

      override def isEmpty[A](h: ExplicitMin[F, A]): Boolean = h match {
        case E() => true
        case _   => false
      }

      override def findMin[A: Ordering](h: ExplicitMin[F, A]): Option[A] =
        h match {
          case E()     => None
          case T(a, h) => Some(a)
        }

      override def insert[A: Ordering](
          a: A,
          h: ExplicitMin[F, A]
      ): ExplicitMin[F, A] = h match {
        case E() => T(a, heap.empty)
        // aの方が小さいときはaを保存する
        case T(aa, hh) if Ordering[A].lt(a, aa) => T(a, heap.insert(a, hh))
        case T(aa, hh)                          => T(aa, heap.insert(a, hh))
      }

      override def merge[A: Ordering](
          h1: ExplicitMin[F, A],
          h2: ExplicitMin[F, A]
      ): ExplicitMin[F, A] = (h1, h2) match {
        case (_, E()) => h1
        case (E(), _) => h2
        case (T(a1, hh1), T(a2, hh2)) if Ordering[A].lt(a1, a2) =>
          T(a1, heap.merge(hh1, hh2))
        case (T(a1, hh1), T(a2, hh2)) =>
          T(a2, heap.merge(hh1, hh2))

      }

      override def deleteMin[A: Ordering](
          h: ExplicitMin[F, A]
      ): Option[(ExplicitMin[F, A], A)] = h match {
        case E() => None
        case T(a, hh) =>
          heap.deleteMin(hh).map { case (hhh, min) =>
            (T(min, hhh), a)
          }
      }
    }
}
