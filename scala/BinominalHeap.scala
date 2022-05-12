import scala.math.Ordering._

object BinominalHeap {
  sealed trait Tree[A]
  final case class Node[A](a: A, tree: List[Tree[A]]) extends Tree[A]
  type TreeR[A] = (Int, Tree[A])
  type HeapT[A] = List[TreeR[A]]

  def rank[A: Ordering](t: TreeR[A]): Int = t match {
    case (r, Node(_, _)) => r
  }

  def insTree[A: Ordering](t: TreeR[A], h: HeapT[A]): HeapT[A] = h match {
    case List() => List(t)
    case ::(t_, ts_) =>
      if (rank(t) < rank(t_)) t +: h else insTree(link(t, t_), ts_)
  }

  def link[A: Ordering](tree1: TreeR[A], tree2: TreeR[A]): TreeR[A] =
    (tree1, tree2) match {
      case ((r, t1 @ Node(x1, c1)), _, t2 @ (Node(x2, c2))) =>
        if (Ordering[A].leq(x1, x2)) (r + 1, Node(x1, t2 :: c1))
        else (r + 1, Node(x2, t1 :: c2))
    }

  implicit val heap: Heap[HeapT] = new Heap[HeapT] {
    type F[A] = HeapT[A]

    override def empty[A]: F[A] = List.empty

    override def isEmpty[A](h: HeapT[A]): Boolean = ???

    override def findMin[A: Ordering](h: HeapT[A]): Option[A] = ???

    override def insert[A: Ordering](a: A, h: HeapT[A]): HeapT[A] = ???

    override def merge[A: Ordering](h1: HeapT[A], h2: HeapT[A]): HeapT[A] = ???

    override def deleteMin[A: Ordering](h: HeapT[A]): Option[(HeapT[A], A)] =
      ???

  }

}
