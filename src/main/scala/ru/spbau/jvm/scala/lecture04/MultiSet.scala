package ru.spbau.jvm.scala.lecture04

trait MultiSet[+A] {
  def filter(f : A => Boolean) : MultiSet[A]

  def map[B](f : A => B) : MultiSet[B]

  def flatMap[B](f : A => MultiSet[B]) : MultiSet[B]

  def apply[B >: A](elem : B): Int

  def withFilter(f : A => Boolean) : MultiSet[A]

  def foreach(f : A => Unit) : Unit

  def &[B >: A](multiSet: MultiSet[B]): MultiSet[B]

  def |[B >: A](multiSet: MultiSet[B]): MultiSet[B]

  def -[B >: A](e : B): (Int, MultiSet[A])

  def *(times: Int): MultiSet[A]

  def find[B >: A](e : B) : Option[B]

  def +[B >: A](e : B): MultiSet[B]

}

object MultiSet {
  def apply[A](seq : A*): MultiSet[A] = seq.foldRight[MultiSet[A]](Nil)((elem : A, set : MultiSet[A]) => Cons(elem, 1, Nil) | set)

  def unapplySeq[A](s: MultiSet[A]): Option[Seq[A]] = s match {
    case Nil => None
    case Cons(elem, count, next) => Some(Seq.fill(count)(elem) ++ unapplySeq(next).getOrElse(Seq.empty[A]))
  }
}

case object Nil extends MultiSet[Nothing] {
  override def filter(f: (Nothing) => Boolean): MultiSet[Nothing] = Nil

  override def map[B](f: (Nothing) => B): MultiSet[B] = Nil

  override def apply[B](elem: B): Int = 0

  override def &[B](multiSet: MultiSet[B]): MultiSet[B] = Nil

  override def |[B](multiSet: MultiSet[B]): MultiSet[B] = multiSet

  override def foreach(f: (Nothing) => Unit): Unit = ()

  override def withFilter (f : Nothing => Boolean): MultiSet[Nothing] = Nil

  override def flatMap[B](f: (Nothing) => MultiSet[B]): MultiSet[Nothing] = Nil

  override def *(times: Int): MultiSet[Nothing] = Nil

  override def -[B >: Nothing](e: B): (Int, MultiSet[Nothing]) = (0, Nil)

  override def find[B >: Nothing](e: B): Option[B] = None

  override def +[B >: Nothing](e: B) = MultiSet(e)
}

case class Cons[A](elem : A, count : Int, next : MultiSet[A]) extends MultiSet[A] {
  override def filter(f: A => Boolean): MultiSet[A] =
    if (f(elem)) {
      Cons(elem, count, next.filter(f))
    }
    else {
      next.filter(f)
    }

  override def map[B](f: (A) => B) = Cons(f(elem), count, next.map(f))

  override def apply[B >: A](elem: B): Int =
    if (this.elem == elem) {
      count
    }
    else {
      next(elem)
    }

  override def &[B >: A](multiSet: MultiSet[B]): MultiSet[B] = {
    val num = multiSet(elem)
    if (num > 0) {
      Cons(elem, num.min(count), next & multiSet)
    }
    else {
      next & multiSet
    }
  }

  override def |[B >: A](multiSet: MultiSet[B]): MultiSet[B] =
    multiSet - elem match {
      case (n, s) => Cons(elem, n + count, next | s)
    }

  override def foreach(f: (A) => Unit): Unit = {
    for (_ <- Range(0, count)) {
      f(elem)
    }
    next.foreach(f)
  }

  override def withFilter(f: A => Boolean): MultiSet[A] =
    if (f(elem)) {
      Cons(elem, count, next.filter(f))
    }
    else {
      next.filter(f)
    }

  override def flatMap[B](f: (A) => MultiSet[B]): MultiSet[B] = f(elem) * count | next.flatMap(f)

  override def *(times: Int): MultiSet[A] = Cons(elem, count * times, next * times)

  override def -[B >: A](e: B): (Int, MultiSet[A]) =
    if (elem == e) {
      (count, next)
    }
    else {
      next - e match {
        case (n, tail) => (n, Cons(elem, count, tail))
      }
    }

  override def find[B >: A](e: B): Option[B] =
    if (elem == e) {
      Some(elem)
    }
    else {
      next.find(elem)
    }

  override def +[B >: A](e: B): MultiSet[B] = MultiSet(e) | this
}

