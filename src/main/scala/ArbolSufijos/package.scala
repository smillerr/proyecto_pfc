package object ArbolSufijos {
  // Definición del árbol Trie
  abstract class Trie
  case class Nodo(char: Char, marcada: Boolean,
                  hijos: List[Trie]) extends Trie
  case class Hoja(char: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char =
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }

  def cabezas(t:Trie): Seq[Char] = {
    t match {
      case Nodo(_,_,lt) => lt.map(t=>raiz(t))
      case Hoja(c,_) => Seq[Char](c)
    }
  }


  def pertenece(secuencia: Seq[Char], arbol: Trie): Boolean = {
    def buscarRec(secuenciaRestante: Seq[Char], nodo: Trie): Boolean = {
      (secuenciaRestante, nodo) match {
        case (Nil, _) => true // Secuencia encontrada
        case (c +: cs, Nodo(char, _, hijos)) =>
          hijos.exists(subTrie => raiz(subTrie) == c && buscarRec(cs, subTrie))
        case (c +: _, Hoja(char, _)) => false
      }
    }
    buscarRec(secuencia, arbol)
  }


  def obtenerSufijos(cadena: Seq[Char]): Seq[Seq[Char]] = {
    cadena.tails.map(_.toList).toList
  }



  def insertarEnNodo(nodo: Nodo, sufijo: Seq[Char]): Nodo = {
    sufijo match {
      case Seq() => nodo.copy(marcada = true)
      case head +: tail =>
        nodo.hijos.find {
          case Nodo(c, _, _) if c == head => true
          case Hoja(c, _) if c == head => true
          case _ => false
        } match {
          case Some(existingNode: Nodo) => nodo.copy(hijos = nodo.hijos.updated(nodo.hijos.indexOf(existingNode), insertarEnNodo(existingNode, tail)))
          case Some(existingLeaf: Hoja) => nodo.copy(hijos = nodo.hijos.updated(nodo.hijos.indexOf(existingLeaf), insertarEnHoja(existingLeaf, tail)))
          case None => nodo.copy(hijos = nodo.hijos :+ insertarEnHoja(Hoja(head, false), tail))
        }
    }
  }



  def insertarEnHoja(hoja: Hoja, sufijo: Seq[Char]): Trie = {
    sufijo match {
      case Seq() => hoja.copy(marcada = true) // Cambiar a true cuando llegas al final del sufijo
      case head +: tail =>
        val nuevoHijo = insertarEnHoja(Hoja(head, false), tail)
        Nodo(hoja.char, false, List(nuevoHijo))
    }
  }


  def adicionar(s: Seq[Char], trie: Trie): Trie = {
    trie match {
      case nodo: Nodo => insertarEnNodo(nodo, s)
      case hoja: Hoja => insertarEnHoja(hoja, s)
    }
  }


  def arbolDeSufijos(secuencias: Seq[Seq[Char]]): Trie = {
    secuencias.foldLeft(Nodo('_', false, Nil)) { (trie, cadena) =>
      val sufijosCadena = obtenerSufijos(cadena)
      sufijosCadena.foldLeft(trie) { (subtrie, sufijo) =>
        insertarEnNodo(subtrie.asInstanceOf[Nodo], sufijo)
      }
    }
  }

}
