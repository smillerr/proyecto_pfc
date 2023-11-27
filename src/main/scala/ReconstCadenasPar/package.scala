import common._
// import scala.collection.parallel.CollectionConverters._
import Oraculo._
import ArbolSufijos._

package object ReconstCadenasPar {
  // Versión paralela del método reconstruirCadenaIngenuo

  /*
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n),
    // un umbral y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa paralelismo de tareas.
    // ...
  }

  // Versión paralela del método reconstruirCadenaMejorado
  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n),
    // un umbral y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa paralelismo de tareas y/o datos.
    // ...
  }

  // Versión paralela del método reconstruirCadenaTurbo
  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
    // un umbral y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa paralelismo de tareas y/o datos.
    // ...
  }

  // Versión paralela del método reconstruirCadenaTurboMejorado
  def reconstruirCadenaTurboMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
    // un umbral y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa el filtro para ir más rápido.
    // Usa paralelismo de tareas y/o datos.
    // ...
  }

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

