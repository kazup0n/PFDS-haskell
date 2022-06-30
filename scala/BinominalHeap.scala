import scala.math.Ordering._

object BinominalHeap {
  sealed trait Tree[A]
  final case class Node[A](r: Int, a: A, tree: List[Tree[A]]) extends Tree[A]
  type HeapT[A] = List[Tree[A]]

  def rank[A: Ordering](t: Tree[A]): Int = t match {
    case Node(r, _, _) => r
  }

  def root[A](t: Tree[A]) = t match {
    case Node(_, a, _) => a
  }

  def insTree[A: Ordering](t: Tree[A], h: HeapT[A]): HeapT[A] = h match {
    case List() => List(t)
    case ::(t_, ts_) =>
      if (rank(t) < rank(t_)) t +: h else insTree(link(t, t_), ts_)
  }

  def link[A: Ordering](t1: Tree[A], t2: Tree[A]): Tree[A] =
    (t1, t2) match {
      case (Node(r, x1, c1), Node(_, x2, c2)) =>
        if (Ordering[A].lteq(x1, x2)) Node(r + 1, x1, t2 :: c1)
        else Node(r + 1, x2, t1 :: c2)
    }

  def removeMinTree[A: Ordering](
      t: HeapT[A]
  ): Option[(Tree[A], HeapT[A])] = t match {
    case List()  => None
    case List(t) => Some((t, List()))
    case h :: ts => {
      removeMinTree(ts).map { case (t_, ts_) =>
        if (Ordering[A].lteq(root(h), root(t_))) (h, ts) else (t_, h :: ts_)
      }
    }
  }

  implicit val heap: Heap[HeapT] = new Heap[HeapT] {
    type F[A] = HeapT[A]

    override def empty[A]: F[A] = List.empty

    override def isEmpty[A](h: HeapT[A]): Boolean = h match {
      case List() => true
      case _      => false
    }

    override def findMin[A: Ordering](h: HeapT[A]): Option[A] = h match {
      case List()  => None
      case List(t) => Some(root(t))
      case t :: ts => findMin(ts).map(Ordering[A].min(root(t), _))
    }

    override def insert[A: Ordering](x: A, h: HeapT[A]): HeapT[A] =
      insTree(Node(0, x, List.empty), h)

    override def merge[A: Ordering](ts1: HeapT[A], ts2: HeapT[A]): HeapT[A] =
      (ts1, ts2) match {
        case (_, List()) => ts1
        case (List(), _) => ts2
        case (t1 :: tts1, t2 :: tts2) =>
          if (rank(t1) < rank(t2)) t1 :: merge(tts1, ts2)
          else if (rank(t2) < rank(t1)) t2 :: merge(ts1, tts2)
          else insTree(link(t1, t2), merge(tts1, tts2))
      }

    override def deleteMin[A: Ordering](h: HeapT[A]): Option[(HeapT[A], A)] =
      removeMinTree(h).map { case (Node(r, a, ts), ts2) =>
        (merge(ts.reverse, ts2), a)
      }

  }

}
