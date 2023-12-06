import common._
import Oraculo._
import ArbolSufijos._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

package object ReconstCadenasPar {
  // Versión paralela del método reconstruirCadenaIngenuo

  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarTodasSecuencias(n: Int, alfabeto: Seq[Char]): Set[Seq[Char]] = {
      val initialSet: Set[Seq[Char]] = Set(Seq.empty[Char])
      if(n<=umbral){
        (1 to n).foldLeft(initialSet) { (acc, _) =>
          for {
            seq <- acc
            caracter <- alfabeto
          }
          yield {
            seq :+ caracter
          }
        }
      } else {
        (1 to n).par.foldLeft(initialSet) { (acc, _) =>
          for {
            seq <- acc
            caracter <- alfabeto
          }
          yield {
            seq :+ caracter
          }
        }
      }
    }

    val secuenciaResultante = generarTodasSecuencias(n, alfabeto).filter(o).head

    secuenciaResultante match {
      case Nil => Seq.empty[Char]
      case nonEmptySeq => secuenciaResultante
    }
  }
  /*
  // Versión paralela del método reconstruirCadenaMejorado
  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n),
    // un umbral y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa paralelismo de tareas y/o datos.
    // ...
  }
*/
  // Versión paralela del método reconstruirCadenaTurbo
  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa paralelismo de tareas y/o datos

    def generarCadenaTurbo(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      val newSC = if (n <= umbral) {
        SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).filter(o)
      }
      else {
        val SCPar = SC.par
        val (leftSC, rightSC) = SCPar.splitAt(SC.size / 2)
        val ((l1SC, l2SC), (r1SC, r2SC)) = (leftSC.splitAt(leftSC.size / 2), rightSC.splitAt(rightSC.size / 2))
        val ((l1newSC, l2newSC), (r1newSC, r2newSC)) = parallel(
          parallel(
            l1SC.flatMap(seq1 => SCPar.map(seq2 => seq1 ++ seq2)).filter(o),
            l2SC.flatMap(seq1 => SCPar.map(seq2 => seq1 ++ seq2)).filter(o)),
          parallel(
            r1SC.flatMap(seq1 => SCPar.map(seq2 => seq1 ++ seq2)).filter(o),
            r2SC.flatMap(seq1 => SCPar.map(seq2 => seq1 ++ seq2)).filter(o)
          )
        )
        (l1newSC ++ l2newSC ++ r1newSC ++ r2newSC).seq
      }
      val resultado = newSC.filter(subseq => subseq.length == n)
      if (resultado.nonEmpty) {
        resultado.head
      } else if (k > n) {
        Seq.empty[Char]
      } else {
        generarCadenaTurbo(k * 2, newSC)
      }
    }
    val conjuntoInicial: Seq[Seq[Char]] = alfabeto.map(Seq(_))
    generarCadenaTurbo(1, conjuntoInicial)
  }

  // Versión paralela del método reconstruirCadenaTurboMejorado
  def reconstruirCadenaTurboMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] =
  {
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
    // un umbral y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa el filtro para ir más rápido.
    // Usa paralelismo de tareas y/o datos.
    // ...
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
  // y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
  // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
  // Usa el filtro para ir más rápido.
  // ...

    val secuenciasIniciales: Seq[Seq[Char]] = alfabeto.flatMap(s1 => alfabeto.map(s2 => Seq(s1, s2))).filter(o)

    def filtro(ss: Seq[Seq[Char]], k: Int, s: Seq[Char]): Boolean =
    {
      //si ya no quedan cadenas en SC, s no es una cadena validad
      if (ss.isEmpty) false
      else
      //si aun quedan cadenas en SC, se sigue comprobando
      {
        if (ss.head.containsSlice(s.slice(k - 1, k + 1))) true
        else filtro(ss.tail, k, s)
      }
    }

    def Filtrar(ss: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] =
    {
      //se comprueba si ha alcanzado el tamaño maximo
      if (k == n) ss //se devuelve la cadena recibida en caso de que ya se haya alcanzado el tamaño maximo
      else //se crean cadenas de tamñaño 2k y se filtran, primero se filtran las obvias, y luego se filtran usando el oraculo
      {

        // se crean las secuencias
        val nuevasSecuencias = ss.flatMap(seq1 => ss.map(seq2 => seq1 ++ seq2))
        //se filtran
        val filtradas = nuevasSecuencias.filter(s => filtro(ss, k, s)).filter(o)
        if (k >= umbral) FiltrarParalelo(filtradas.par.map(s => s.par),k * 2).to(Seq).map(s => s.to(Seq))
        else Filtrar(filtradas,k * 2)//se llama recursivamente Filtrar
      }
    }

    def filtroParalelo(ss: ParSeq[ParSeq[Char]], k: Int, s: ParSeq[Char]): Boolean =
    {
      //si ya no quedan cadenas en SC, s no es una cadena validad
      if (ss.isEmpty) false
      else
      //si aun quedan cadenas en SC, se sigue comprobando
      {
        if (ss.head == s.slice(k - (k/2), k + (k/2))) true
        else filtroParalelo(ss.tail, k, s)
      }
    }

    def FiltrarParalelo(ss: ParSeq[ParSeq[Char]], k: Int): ParSeq[ParSeq[Char]] = {
      //se comprueba si ha alcanzado el tamaño maximo
      if (k == n) ss //se devuelve la cadena recibida en caso de que ya se haya alcanzado el tamaño maximo
      else //se crean cadenas de tamñaño 2k y se filtran, primero se filtran las obvias, y luego se filtran usando el oraculo
      {

        // se crean las secuencias
        val nuevasSecuencias = ss.flatMap(seq1 => ss.map(seq2 => seq1 ++ seq2))
        //se filtran
        val filtradas = (nuevasSecuencias.filter(s => filtroParalelo(ss, k, s))).to(Seq).map(s => s.to(Seq)).filter(o).par.map(s => s.par)
        FiltrarParalelo(filtradas, k * 2) //se llama recursivamente Filtrar
      }
    }

    Filtrar(secuenciasIniciales, 2).head
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

