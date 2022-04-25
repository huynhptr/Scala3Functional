package rockthejvm

object SemigroupsMonoids {
  // type + a combination
  trait Semigroup[T] {
    def combine(a: T, b: T): T
  } 
  
  object Semigroup {
    def apply[T](using instance: Semigroup[T]): Semigroup[T] = instance
  }
  
  trait Monoid[T] extends Semigroup[T] {
    def empty: T // empty + x = x for all x in the type T
  }
  
  object Monoid {
    def apply[T](using monoid: Monoid[T]): Monoid[T] = monoid
  }
  
  object IntInstances {
    given intMonoid: Monoid[Int] with {
      def combine(a: Int, b: Int) = a + b
      def empty: Int = 0
    }
  }

  object StringInstances {
    given stringMonoid: Monoid[String] with {
      def combine(a: String, b: String): String = a + b
      def empty: String = ""
    }
  }
  import IntInstances.given
  import StringInstances.given
  val naturalIntSemiGroup = Semigroup[Int]
  val naturalStringSemiGroup = Semigroup[String]
  val meaningOfLife = naturalIntSemiGroup.combine(2, 40)
  val favLanguge = naturalStringSemiGroup.combine("Sca", "la")
  
  object SemiGroupSyntax {
    extension [T](a: T)
      def |+|(b: T)(using semiGroup: Semigroup[T]): T = semiGroup.combine(a, b)
  }
  
  import SemiGroupSyntax._
  val meaning2 = 2 |+| 40
  
  def reduceCompact[T : Semigroup](list: List[T]): T = list.reduce(_ |+| _)
}
