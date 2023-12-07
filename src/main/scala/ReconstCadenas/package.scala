import ArbolSufijos._
import Oraculo._

import scala.collection.View.Empty

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

  /*def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n),
    // y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // ...
  }
*/
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
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
    // y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa el filtro para ir más rápido.
    // ...

    val secuenciasIniciales: Seq[Seq[Char]] = alfabeto.flatMap(s1 => alfabeto.map(s2 => Seq(s1,s2))).filter(o)

    def filtro(ss: Seq[Seq[Char]], k:Int, s:Seq[Char]): Boolean =
    {
      //si ya no quedan cadenas en SC, s no es una cadena validad
      if(ss.isEmpty) false
      else
      //si aun quedan cadenas en SC, se sigue comprobando
      {
        if(ss.head.containsSlice(s.slice(k-1, k+1))) true
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
        val filtradas = nuevasSecuencias.filter(x => filtro(ss,k,x)).filter(o)
        Filtrar(filtradas, k * 2) //se llama recursivamente Filtrar
      }
    }

    Filtrar(secuenciasIniciales, 2).head
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
      //recibe la longitud de la secuencia que hay que reconstruir(n, potencia de 2)
      // y un oraculo para esa secuencia y devuelve la secuencia reconstruida
      //Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 tambien son subsecuencias de s
      //Usa arbol de sufijos para guardar Set[Seq[Char]]

      val secuenciasIniciales: Seq[Seq[Char]] = alfabeto.flatMap(s1 => alfabeto.map(s2 => Seq(s1,s2))).filter(o)
      def Filtrar(SC: Set[Seq[Char]], k: Int, arbol: Trie): Set[Seq[Char]] = {
        if(k == n) SC
        else {
          val nuevasSecuencias = SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
          val arbol = arbolDeSufijos(nuevasSecuencias.toSeq)
          val filtradas = nuevasSecuencias.filter(x=>pertenece(x,arbol)).filter(o)
          val arbolActual = arbolDeSufijos(filtradas.toSeq)
          Filtrar(filtradas,k*2,arbolActual)
        }
      }
      Filtrar(secuenciasIniciales.toSet, 2, Nodo('_', false, Nil)).head
    }

}
