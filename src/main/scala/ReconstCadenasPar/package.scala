import common._
import Oraculo._
import ArbolSufijos._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq
import scala.collection.parallel.immutable.ParRange

package object ReconstCadenasPar {
  // Versión paralela del método reconstruirCadenaIngenuo

  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def generarTodasSecuenciasRec(rango: Range): Set[Seq[Char]] = {
      if (rango.length <= umbral) {
        // Caso base: cuando el rango es igual o menor al umbral, realizar de manera secuencial
        rango.foldLeft(Set(Seq.empty[Char])) { (acc, _) =>
          for {
            seq <- acc
            caracter <- alfabeto
          } yield seq :+ caracter
        }
      } else {
        // Dividir el rango en dos partes y continuar de manera recursiva
        val m = rango.length / 2
        val (secuenciasPrimeraParte, secuenciasSegundaParte) = parallel(
          generarTodasSecuenciasRec(rango.take(m)),
          generarTodasSecuenciasRec(rango.drop(m))
        )
        secuenciasPrimeraParte.flatMap(seq1 => secuenciasSegundaParte.map(seq2 => seq1 ++ seq2))
      }
    }

    // Uso de la función recursiva
    val secuenciaResultante = generarTodasSecuenciasRec(1 to n).filter(o).head
    secuenciaResultante match {
      case Nil => Seq.empty[Char]
      case nonEmptySeq => secuenciaResultante
    }
  }

  // Versión paralela del método reconstruirCadenaMejorado
  def reconstruirCadenaMejoradoPar(n: Int, o: Seq[Char] => Boolean): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenas(k: Int, chars: Set[Seq[Char]]): Set[Seq[Char]] = {
      if (k <= 0) chars
      else generarCadenas(k - 1, chars.flatMap(s => alfabeto.par.map(c => s :+ c)).seq)
    }

    def filtrarCadenas(cadenas: Set[Seq[Char]]): Set[Seq[Char]] = {
      cadenas.par.filter(o).seq
    }

    def reconstruirRec(k: Int, subcadenas: Set[Seq[Char]]): Seq[Char] = {
      if (k > n) Seq.empty[Char]
      else {
        val nuevasSubcadenas = filtrarCadenas(generarCadenas(k, subcadenas))
        if (nuevasSubcadenas.isEmpty) reconstruirRec(k + 1, subcadenas)
        else nuevasSubcadenas.head
      }
    }

    val conjuntoInicial = Set(Seq.empty[Char])
    reconstruirRec(1, conjuntoInicial)
  }

  // Versión paralela del método reconstruirCadenaTurbo
  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenaTurbo(k: Int, conjuntoActual: Seq[Seq[Char]]): Seq[Char] = {
      val posiblesSecuencias = if (n <= umbral) {
        conjuntoActual.flatMap(seq1 => conjuntoActual.map(seq2 => seq1 ++ seq2)).filter(o)
      }
      else {
        val conjuntoActualPar = conjuntoActual.par
        val (conjuntoIzq, conjuntoDer) = conjuntoActualPar.splitAt(conjuntoActual.size / 2)
        val ((izqConjuntoIzq, derConjuntoIzq), (izqConjuntoDer, derConjuntoDer)) = (conjuntoIzq.splitAt(conjuntoIzq.size / 2), conjuntoDer.splitAt(conjuntoDer.size / 2))
        val ((conjuntoPar1, conjuntoPar2), (conjuntoPar3, conjuntoPar4)) = parallel(
          parallel(
            izqConjuntoIzq.flatMap(seq1 => conjuntoActualPar.map(seq2 => seq1 ++ seq2)).filter(o),
            derConjuntoIzq.flatMap(seq1 => conjuntoActualPar.map(seq2 => seq1 ++ seq2)).filter(o)),
          parallel(
            izqConjuntoDer.flatMap(seq1 => conjuntoActualPar.map(seq2 => seq1 ++ seq2)).filter(o),
            derConjuntoDer.flatMap(seq1 => conjuntoActualPar.map(seq2 => seq1 ++ seq2)).filter(o)
          )
        )
        (conjuntoPar1 ++ conjuntoPar2 ++ conjuntoPar3 ++ conjuntoPar4).seq
      }
      val secuenciaReconstruida = posiblesSecuencias.filter(subseq => subseq.length == n)
      if (secuenciaReconstruida.nonEmpty) {
        secuenciaReconstruida.head
      } else if (k > n) {
        Seq.empty[Char]
      } else {
        generarCadenaTurbo(k * 2, posiblesSecuencias)
      }
    }
    val conjuntoInicial: Seq[Seq[Char]] = alfabeto.map(Seq(_))
    generarCadenaTurbo(1, conjuntoInicial)
  }

  // Versión paralela del método reconstruirCadenaTurboMejorado
  def reconstruirCadenaTurboMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] =
  {

    val secuenciasIniciales: Seq[Seq[Char]] = alfabeto.flatMap(s1 => alfabeto.map(s2 => Seq(s1, s2))).filter(o)

    def Filtrar(ss: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k == n) ss.head
      else {
        if (k >= umbral) Filtrar(
          (
            for {
              seq1 <- ss
              seq2 <- ss
              if (secuenciasIniciales.contains(Seq(seq1.last, seq2.head)))
            } yield seq1 ++ seq2).filter(o), k * 2)
        else FiltrarPar(
          (
            for {
              seq1 <- ss
              seq2 <- ss
              if (secuenciasIniciales.contains(Seq(seq1.last, seq2.head)))
            } yield seq1 ++ seq2).filter(o).par.map(s => s.par), k * 2).to(Seq)
      }
    }

    def FiltrarPar(ss: ParSeq[ParSeq[Char]], k: Int): ParSeq[Char] = {
      if (k == n) ss.head
      else {
        FiltrarPar(
          (
            for {
              seq1 <- ss
              seq2 <- ss
              if (secuenciasIniciales.contains(Seq(seq1.last, seq2.head)))
            } yield seq1 ++ seq2).to(Seq).map(s => s.to(Seq)).filter(o).par.map(s => s.par), k * 2)
      }
    }

    Filtrar(secuenciasIniciales, 2)
  }
/*
  // Versión paralela del método reconstruirCadenaTurboAcelerada
  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
    // un umbral y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa el filtro para ir más rápido.
    // Usa árboles de sufijos para guardar Seq[Seq[Char]].
    // Usa paralelismo de tareas y/o datos.
    // ...
  }
   */
}

