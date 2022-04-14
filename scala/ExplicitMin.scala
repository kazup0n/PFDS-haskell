trait Heap[F[_]] {
  def empty[A]: F[A]
  def isEmpty[A](h: F[A]): Boolean
  def findMin[A](h: F[A]): Option[A]
  def insert[A](a: A, h: F[A]): F[A]
  def merge[A](h1: F[A], h2: F[A]): F[A]
  def deleteMin[A](h: F[A]): Option[(F[A], A)]
}
sealed trait ExplicitMin[F[_]]
object ExplicitMin {
  final case class T[F[_]: Heap, A](a: A, h: F[A]) extends ExplicitMin[F]
  final case class E[F[_]]() extends ExplictMin[F]

  implicit def explictMin[F[_]](implicit heap: Heap[F]): Heap[ExplicitMin[F]] = new Heap[ExplicitMin[F]] {
      type H[A] = ExplicitMin[F[A]]
      def empty[A]: H[A] = ExplicitMin.E()
      def isEmpty[A](h: H[A]) = h match {
          case ExplicitMin.E => true
          case _ => false
      }
      def findMin[A](h: H[A]): Option[A] = h match {
          case ExplicitMin.E => None
          case ExplicitMin.T(a, _) => Some(a)
      }

      def deleteMin[A](h: F[A]): Option[(H[A], A)] = h match {
          case ExplicitMin.E => None
          case ExplicitMin.T(a, hh) => hh.deleteMin.map {
              case (hhh, aa) => ???
          }
      }
  }

}
