import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')
    def generarTodasSecuencias(n: Int, alfabeto: Seq[Char]): Set[Seq[Char]] = {
      val initialSet: Set[Seq[Char]] = Set(Seq.empty[Char])

      (1 to n).foldLeft(initialSet) { (acc, _) =>
        for {
          seq <- acc
          caracter <- alfabeto
        }
        yield {
          seq :+ caracter
        }
      }
    }
    val secuenciaResultante = generarTodasSecuencias(n, alfabeto).find(o)

    secuenciaResultante match {
      case None => Seq.empty[Char]
      case Some(secuenciaIngenua) => secuenciaIngenua
    }
  }

  /*def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n),
    // y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // ...
  }
*/
  def reconstruirCadenaTurbo(alfabeto: Seq[Char], N: Int, oraculo: Oraculo): Seq[Char] = {
    val secuenciasIniciales: Set[Seq[Char]] = alfabeto.flatMap(char => Seq(Seq(char))).toSet

    def generarConjuntoSC(conjuntoActual: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      if (k > N) conjuntoActual
      else {
        val nuevasSecuencias = conjuntoActual.flatMap(seq1 => conjuntoActual.map(seq2 => seq1 ++ seq2))
        val filtradas = nuevasSecuencias.filter(oraculo)
        generarConjuntoSC(filtradas, k * 2)
      }
    }

    val conjuntoFinal = generarConjuntoSC(secuenciasIniciales, 2)
    conjuntoFinal.find(_.length == N).getOrElse(Seq.empty[Char])
  }
/*
  def reconstruirCadenaTurboMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
    // y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa el filtro para ir más rápido.
    // ...
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
    // y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa el filtro para ir más rápido.
    // Usa árboles de sufijos para guardar Seq[Seq[Char]].
    // ...
  }
  */
}
