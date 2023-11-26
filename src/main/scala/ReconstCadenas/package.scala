import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n),
    // y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // ...
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n),
    // y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // ...
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
    // y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // ...

  }

  def reconstruirCadenaTurboMejorado(n: Int, o: Oraculo): Seq[Seq[Char]] =
  {
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
    // y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa el filtro para ir más rápido.
    // ...
    def Filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      for {
        cadena1 <- SC
        cadena2 <- SC
        comprobante <- SC
        if ((cadena1 ++ cadena2).slice(k / 2, k + (k / 2)) == comprobante)
      } yield cadena1 ++ cadena2
    }
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
    // y un oráculo para esa secuencia, y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa el filtro para ir más rápido.
    // Usa árboles de sufijos para guardar Seq[Seq[Char]].
    // ...
  }
}
