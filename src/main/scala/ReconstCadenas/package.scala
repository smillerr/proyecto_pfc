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

def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
  val alfabeto = Seq('a', 'c', 'g', 't')

  // Función auxiliar recursiva para generar subcadenas candidatas
  def generarSubcadenas(longitud: Int): Seq[String] = {
    if (longitud == 0) Seq("")
    else {
      val subCadenas = generarSubcadenas(longitud - 1)
      alfabeto.flatMap(char => subCadenas.map(sub => char.toString + sub))
    }
  }

  // Generar subcadenas candidatas y consultar al oráculo
  val candidatos = generarSubcadenas(n)
  candidatos.find(o) match {
    case Some(cadenaEncontrada) => cadenaEncontrada.toList // Si se encuentra, devolver la cadena como lista de caracteres
    case None => Seq.empty // Si no se encuentra, devolver una lista vacía o manejar el caso según lo necesites
  }
}
  
  def reconstruirCadenaTurbo(alfabeto: Seq[Char], n: Int, o: Oraculo): Seq[Char] = {
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
    conjuntoFinal.find(_.length == n).getOrElse(Seq.empty[Char])
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
  /*
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
