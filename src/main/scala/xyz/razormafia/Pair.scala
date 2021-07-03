package xyz.razormafia

case class Pair[A, B](
                     first: A,
                     second: B
                     ) {
  @inline final def mapFirst[C](f: A => C): Pair[C, B] =
    Pair(f(first), second)

  @inline final def mapSecond[C](f: B => C): Pair[A, C] =
    Pair(first, f(second))
}
