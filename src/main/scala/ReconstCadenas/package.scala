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
    val secuenciaResultante = generarTodasSecuencias(n, alfabeto).filter(o).head

    secuenciaResultante match {
      case Nil => Seq.empty[Char]
      case nonEmptySeq => secuenciaResultante
    }
  }

  def reconstruirCadenaMejorado(n: Int, o: Seq[Char] => Boolean): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenas(k: Int, chars: Set[Seq[Char]]): Set[Seq[Char]] = {
      if (k <= 0) chars
      else generarCadenas(k - 1, chars.flatMap(s => alfabeto.map(c => s :+ c)))
    }

    def filtrarCadenas(cadenas: Set[Seq[Char]]): Set[Seq[Char]] = {
      cadenas.filter(o)
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
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    val secuenciasIniciales: Set[Seq[Char]] = alfabeto.flatMap(char => Seq(Seq(char))).toSet

    def generarConjuntoSC(conjuntoActual: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      if (k > n) conjuntoActual
      else {
        val nuevasSecuencias = conjuntoActual.flatMap(seq1 => conjuntoActual.map(seq2 => seq1 ++ seq2))
        val filtradas = nuevasSecuencias.filter(o)
        generarConjuntoSC(filtradas, k * 2)
      }
    }
    val conjuntoFinal = generarConjuntoSC(secuenciasIniciales, 2)
    if(conjuntoFinal.nonEmpty){
      conjuntoFinal.filter(seq=> seq.length == n).head
    }
    else {
      Seq.empty[Char]
    }
  }

  def reconstruirCadenaTurboMejorado(n: Int, o: Oraculo): Seq[Char] =
  {
    val secuenciasIniciales: Seq[Seq[Char]] = alfabeto.flatMap(s1 => alfabeto.map(s2 => Seq(s1, s2)))

    def Filtrar(ss: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k == n) ss.head
      else {
        Filtrar(
          (
            for {
              seq1 <- ss
              seq2 <- ss
              if (secuenciasIniciales.contains(Seq(seq1.last, seq2.head)))
            } yield seq1 ++ seq2).filter(o), k * 2)
      }
    }

    Filtrar(secuenciasIniciales.filter(o), 2)
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    //recibe la longitud de la secuencia que hay que reconstruir(n, potencia de 2)
    // y un oraculo para esa secuencia y devuelve la secuencia reconstruida
    //Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 tambien son subsecuencias de s
    //Usa arbol de sufijos para guardar Set[Seq[Char]]

    val secuenciasIniciales: Seq[Seq[Char]] = alfabeto.flatMap(s1 => alfabeto.map(s2 => Seq(s1, s2))).filter(o)
    def Filtrar(SC: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      if (k == n) SC
      else {
        val nuevasSecuencias = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
        val arbol = arbolDeSufijos(nuevasSecuencias.toSeq)
        val filtradas = nuevasSecuencias.filter(x => pertenece(x, arbol)).filter(o)
        Filtrar(filtradas, k * 2)
      }
    }

    Filtrar(secuenciasIniciales.toSet, 2).head

  }

}
