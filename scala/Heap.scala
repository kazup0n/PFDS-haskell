trait Heap[F[_]] {
  def empty[A]: F[A]
  def isEmpty[A](h: F[A]): Boolean
  def findMin[A: Ordering](h: F[A]): Option[A]
  def insert[A: Ordering](a: A, h: F[A]): F[A]
  def merge[A: Ordering](h1: F[A], h2: F[A]): F[A]
  def deleteMin[A: Ordering](h: F[A]): Option[(F[A], A)]
}
