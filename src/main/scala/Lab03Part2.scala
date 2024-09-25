object Lab03Part2 extends App {

  println("implementazione punto 3")
  // Definizione del tipo Person
  sealed trait Person
  case class Student(name: String, courses: Sequence[String]) extends Person
  case class Teacher(name: String, subject: String) extends Person

  // Definizione del tipo Sequence
  sealed trait Sequence[+A]
  case class Cons[A](head: A, tail: Sequence[A]) extends Sequence[A]
  case class Nil[A]() extends Sequence[A]

  // Funzione concat (necessaria per flatMap)
  def concat[A](l: Sequence[A], r: Sequence[A]): Sequence[A] = l match {
    case Cons(head, tail) => Cons(head, concat(tail, r))
    case Nil() => r
  }

  // Funzione flatMap
  def flatMap[A, B](l: Sequence[A])(f: A => Sequence[B]): Sequence[B] = l match {
    case Cons(head, tail) => concat(f(head), flatMap(tail)(f))
    case Nil() => Nil()
  }

  // Funzione per filtrare e ottenere i corsi degli studenti
  def studentCourses(persons: Sequence[Person]): Sequence[String] = {
    flatMap(persons) {
      case Student(_, courses) => courses // Se è uno studente, restituisci i corsi
      case _ => Nil() // Se non è uno studente, restituisci una lista vuota
    }
  }

  // Funzione per stampare una Sequence
  def printSequence[A](l: Sequence[A]): Unit = l match {
    case Cons(head, tail) =>
      print(s"$head ")
      printSequence(tail)
    case Nil() => println()
  }

  // Esempio di utilizzo
  val lstPersons: Sequence[Person] = Cons(
    Student("Alice", Cons("Math", Cons("Physics", Nil()))),
    Cons(Teacher("Bob", "History"),
      Cons(Student("Charlie", Cons("Biology", Cons("Chemistry", Nil()))), Nil())
    )
  )

  val resultCourses = studentCourses(lstPersons)

  print("Corsi degli studenti: ")
  printSequence(resultCourses)
  // Output atteso: Math Physics Biology Chemistry

  println("implementazione punto 4")
  // Funzione foldLeft
  def foldLeft[A, B](l: Sequence[A])(acc: B)(f: (B, A) => B): B = l match {
    case Nil() => acc // Se la sequenza è vuota, ritorna l'accumulatore di default
    case Cons(head, tail) => foldLeft(tail)(f(acc, head))(f) // Applica la funzione e continua con la coda
  }

  // Esempio di utilizzo
  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  // Esempio di foldLeft con l'operatore -
  val result = foldLeft(lst)(0)(_ - _)

  println(s"Risultato di foldLeft con sottrazione: $result")
  // Output atteso: -16

  println("implementazione punto 5")

  // Implicit class per estendere il tipo Sequence con nuovi metodi
  object SequenceOps {
    implicit class SequenceExtensions[A](seq: Sequence[A]) {

      // Funzione take
      def take(n: Int): Sequence[A] = seq match {
        case Cons(head, tail) if n > 0 => Cons(head, tail.take(n - 1))
        case _ => Nil() // Quando n <= 0 o la lista è vuota
      }

      // Funzione zip
      def zip[B](other: Sequence[B]): Sequence[(A, B)] = (seq, other) match {
        case (Cons(headL, tailL), Cons(headR, tailR)) =>
          Cons((headL, headR), tailL.zip(tailR))
        case _ => Nil() // Quando una delle liste è vuota
      }

      // Funzione concat
      def concat(other: Sequence[A]): Sequence[A] = seq match {
        case Cons(head, tail) => Cons(head, tail.concat(other))
        case Nil() => other // Quando la lista 'seq' è vuota, ritorna 'other'
      }

      // Funzione flatMap
      def flatMap[B](f: A => Sequence[B]): Sequence[B] = seq match {
        case Cons(head, tail) => f(head).concat(tail.flatMap(f))
        case Nil() => Nil() // Lista vuota
      }

      // Funzione map (usando flatMap)
      def map[B](f: A => B): Sequence[B] = {
        seq.flatMap(v => Cons(f(v), Nil()))
      }

      // Funzione filter (usando flatMap)
      def filter(p: A => Boolean): Sequence[A] = {
        seq.flatMap(v => if (p(v)) Cons(v, Nil()) else Nil())
      }

      // Funzione foldLeft
      def foldLeft[B](acc: B)(f: (B, A) => B): B = seq match {
        case Nil() => acc // Se la sequenza è vuota, ritorna l'accumulatore di default
        case Cons(head, tail) => tail.foldLeft(f(acc, head))(f) // Applica la funzione e continua con la coda
      }
    }
  }

  // Funzione min come estensione
  object MinOps {

    import SequenceOps.*

    implicit class IntSequenceExtensions(seq: Sequence[Int]) {
      def min: Option[Int] = seq match {
        case Nil() => None // Sequenza vuota, ritorna None
        case Cons(head, tail) => Some(seq.foldLeft(head)(math.min))
      }
    }
  }

  // Import esplicito di SequenceOps per poter usare i metodi di estensione
  import SequenceOps.*
  // Import esplicito di MinOps per poter usare il metodo di estensione `min`
  import MinOps.*

  val lstMin = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  // Esempio di take
  println(s"Take 2: ${lst.take(2)}") // Output atteso: Cons(3, Cons(7, Nil()))

  // Esempio di zip
  val lst2 = Cons("a", Cons("b", Cons("c", Nil())))
  println(s"Zip: ${lst.zip(lst2)}") // Output atteso: Cons((3,a), Cons((7,b), Cons((1,c), Nil())))

  // Esempio di concat
  val lst3 = Cons(10, Nil())
  println(s"Concat: ${lst.concat(lst3)}") // Output atteso: Cons(3, Cons(7, Cons(1, Cons(5, Cons(10, Nil())))))

  // Esempio di flatMap
  println(s"FlatMap (add 1): ${lst.flatMap(x => Cons(x + 1, Nil()))}") // Output atteso: Cons(4, Cons(8, Cons(2, Cons(6, Nil()))))

  // Esempio di map
  println(s"Map (add 1): ${lst.map(_ + 1)}") // Output atteso: Cons(4, Cons(8, Cons(2, Cons(6, Nil()))))

  // Esempio di filter
  println(s"Filter (> 3): ${lst.filter(_ > 3)}") // Output atteso: Cons(7, Cons(5, Nil()))

  // Esempio di foldLeft
  println(s"FoldLeft (subtraction): ${lst.foldLeft(0)(_ - _)}") // Output atteso: -16

  // Esempio di min
  println(s"Min: ${lst.min}") // Output atteso: Some(1)
}