trait Heap[F[_]] {
  def empty[A]: F[A]
  def isEmpty[A](h: F[A]): Boolean
  def findMin[A: Ordering](h: F[A]): Option[A]
  def insert[A: Ordering](a: A, h: F[A]): F[A]
  def merge[A: Ordering](h1: F[A], h2: F[A]): F[A]
  def deleteMin[A: Ordering](h: F[A]): Option[(F[A], A)]
}

object Heap {
  def apply[F[_]](implicit h: Heap[F]): Heap[F] = h
  implicit class HeapOps[F[_]: Heap, A: Ordering](h: F[A]) {
    def insert(a: A) = Heap[F].insert(a, h)
    def deleteMin = Heap[F].deleteMin(h)
    def findMin = Heap[F].findMin(h)
    def merge(h2: F[A]): F[A] = Heap[F].merge(h, h2)
  }
}
