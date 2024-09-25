import scala.annotation.tailrec

object Lab03Part3 extends App {
  println("implementazione punto 6:")

  sealed trait Stream[A]

  object Stream {
    case class Cons[A](head: () => A, tail: () => Stream[A]) extends Stream[A]

    case class Empty[A]() extends Stream[A]

    // Funzione factory per creare uno stream vuoto
    def empty[A](): Stream[A] = Empty()

    // Funzione factory per creare uno stream
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    // Funzione per creare uno stream infinito che applica la funzione f ad ogni elemento
    def iterate[A](start: => A)(f: A => A): Stream[A] = cons(start, iterate(f(start))(f))

    // Funzione per trasformare uno stream in una lista
    def toList[A](stream: Stream[A]): List[A] = stream match {
      case Cons(h, t) => h() :: toList(t())
      case Empty() => List()
    }

    // Funzione takeWhile: restituisce i primi n elementi dello stream che soddisfano il predicato p
    def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A] = stream match {
      case Cons(h, t) if p(h()) => cons(h(), takeWhile(t())(p)) // Mantieni l'elemento se il predicato è vero
      case _ => empty() // Interrompi lo stream se il predicato è falso o lo stream è vuoto
    }
  }

  import Stream.*

  // Esempio di utilizzo
  val s = iterate(0)(_ + 1) // Stream che genera numeri interi a partire da 0

  // Prendi gli elementi minori di 5
  val resultTakeWhile = takeWhile(s)(_ < 5)

  // Converti lo stream in una lista per una più facile visualizzazione
  println(toList(resultTakeWhile)) // Output atteso: List(0, 1, 2, 3, 4)

  println("implementazione punto 7:")

  // Funzione factory per creare uno stream vuoto
  def empty[A](): Stream[A] = Empty()

  // Funzione factory per creare uno stream con un elemento e il resto dello stream
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // Funzione per trasformare uno stream in una lista
  def toList[A](stream: Stream[A]): List[A] = stream match {
    case Cons(h, t) => h() :: toList(t())
    case Empty() => List()
  }

  // Funzione fill: crea uno stream con n elementi, tutti uguali a k
  def fill[A](n: Int)(k: A): Stream[A] = {
    if (n <= 0) empty() // Se n è 0 o negativo, restituisci uno stream vuoto
    else cons(k, fill(n - 1)(k)) // Altrimenti, aggiungi k e continua con n-1
  }


  // Esempio di utilizzo
  val result = fill(3)("a") // Crea uno stream con 3 "a"

  // Converti lo stream in una lista per una più facile visualizzazione
  println(toList(result)) // Output atteso: List("a", "a", "a")

  println("implementazione punto 8:")

  // Funzione take: prende i primi n elementi dello stream
  def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match {
    case (Cons(h, t), n) if n > 0 => cons(h(), take(t())(n - 1))
    case _ => empty()
  }

  // Stream infinito dei numeri di Pell
  val pell: Stream[Int] = {
    // Funzione ausiliaria ricorsiva per calcolare i numeri di Pell
    def loop(p0: Int, p1: Int): Stream[Int] = {
      cons(p0, loop(p1, 2 * p1 + p0))
    }

    loop(0, 1) // Iniziamo con Pell(0) = 0 e Pell(1) = 1
  }

  // Esempio di utilizzo: prendiamo i primi 5 numeri di Pell e li trasformiamo in lista
  val resultPell = take(pell)(5)
  println(toList(resultPell)) // Output atteso: List(0, 1, 2, 5, 12)

}




