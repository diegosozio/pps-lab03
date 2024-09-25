object Lab03Part1 extends App {
  // Definizione del tipo Sequence
  sealed trait Sequence[+A]

  case class Cons[A](head: A, tail: Sequence[A]) extends Sequence[A]

  case class Nil[A]() extends Sequence[A]

  // Funzione take
  def take[A](l: Sequence[A], n: Int): Sequence[A] = l match {
    case Cons(head, tail) if n > 0 => Cons(head, take(tail, n - 1))
    case _ => Nil() // Quando n <= 0 o la lista è vuota
  }

  // Esempi di utilizzo
  val lst = Cons(10, Cons(20, Cons(30, Nil())))

  println("funzione Take:")
  println(take(lst, 2)) // Output: Cons(10, Cons(20, Nil()))
  println(take(lst, 0)) // Output: Nil()
  println(take(lst, 5)) // Output: Cons(10, Cons(20, Cons(30, Nil())))


  // Funzione zip
  def zip[A, B](l: Sequence[A], r: Sequence[B]): Sequence[(A, B)] = (l, r) match {
    case (Cons(headL, tailL), Cons(headR, tailR)) =>
      Cons((headL, headR), zip(tailL, tailR))
    case _ => Nil() // Quando una delle liste è vuota
  }

  // Esempi di utilizzo
  println("funzione Zip:")
  val lstZip1 = Cons(10, Cons(20, Cons(30, Nil())))
  val lstZip2 = Cons("a", Cons("b", Cons("c", Nil())))

  println(zip(lstZip1, lstZip2)) // Output: Cons((10,a), Cons((20,b), Cons((30,c), Nil())))

  // Funzione concat
  def concat[A](l: Sequence[A], r: Sequence[A]): Sequence[A] = l match {
    case Cons(head, tail) => Cons(head, concat(tail, r))
    case Nil() => r // Quando la lista 'l' è vuota, ritorna 'r'
  }

  // Esempi di utilizzo
  val lstConcat1 = Cons(10, Cons(20, Nil()))
  val lstConcat2 = Cons(30, Cons(40, Nil()))

  println("funzione Concat:")
  println(concat(lstConcat1, lstConcat2)) // Output: Cons(10, Cons(20, Cons(30, Cons(40, Nil()))))


  // Funzione flatMap
  def flatMap[A, B](l: Sequence[A])(f: A => Sequence[B]): Sequence[B] = l match {
    case Cons(head, tail) => concat(f(head), flatMap(tail)(f)) // Applica f e concatena
    case Nil() => Nil() // Lista vuota
  }

  println("funzione flatMap:")
  // Esempi di utilizzo
  val lstflatMap = Cons(10, Cons(20, Cons(30, Nil())))

  // Caso 1: Trasforma ogni elemento aggiungendo 1
  println(flatMap(lst)(v => Cons(v + 1, Nil())))
  // Output: Cons(11, Cons(21, Cons(31, Nil())))

  // Caso 2: Trasforma ogni elemento in una lista di due valori (v+1, v+2)
  println(flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))
  // Output: Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil()))))))

  println("funzione map e filter in flatMap:")
  def map[A, B](l: Sequence[A])(f: A => B): Sequence[B] = {
    flatMap(l)(v => Cons(f(v), Nil()))
  }

  def filter[A](l: Sequence[A])(p: A => Boolean): Sequence[A] = {
    flatMap(l)(v => if (p(v)) Cons(v, Nil()) else Nil())
  }


  def printSequence[A](l: Sequence[A]): Unit = l match {
    case Cons(head, tail) =>
      print(s"$head ")
      printSequence(tail)
    case Nil() => println()
  }

  // Esempio di map: aggiunge 1 a ogni elemento
  val mappedList = map(lst)(_ + 1)
  print("Risultato di map (aggiungi 1 a ogni elemento): ")
  printSequence(mappedList) // Output: 11 21 31

  // Esempio di filter: tiene solo gli elementi maggiori di 15
  val filteredList = filter(lst)(_ > 15)
  print("Risultato di filter (elementi > 15): ")
  printSequence(filteredList) // Output: 20 30

  println("funzione MIN")
  // Definizione di Optional
  sealed trait Optional[+A]

  case class Some[A](value: A) extends Optional[A]

  case object None extends Optional[Nothing]

  // Funzione min per Sequence[Int]
  def min(l: Sequence[Int]): Optional[Int] = l match {
    case Nil() => None // Sequenza vuota, ritorna None
    case Cons(head, tail) => minHelper(head, tail) // Trova il minimo nella lista
  }

  // Funzione helper per trovare il minimo
  def minHelper(currentMin: Int, l: Sequence[Int]): Optional[Int] = l match {
    case Nil() => Some(currentMin) // Se abbiamo finito la lista, restituisci il minimo corrente
    case Cons(head, tail) =>
      // Aggiorna il minimo se il nuovo elemento è più piccolo
      minHelper(math.min(currentMin, head), tail)
  }

  // Esempi di utilizzo
  val lstMin = Cons(10, Cons(25, Cons(20, Nil())))
  val resultMin = min(lst)

  resultMin match {
    case Some(value) => println(s"Il valore minimo è: $value") // Output atteso: Il valore minimo è: 10
    case None => println("La sequenza è vuota")
  }
}