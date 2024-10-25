abstract class ArbolHuff {

  def peso(arbol: ArbolHuff): Int = arbol match
    case NodoHuff(caracter, frecuencia) => frecuencia
    case RamaHuff(nodoDch, nodoIzq) => peso(nodoIzq) + peso(nodoDch)

  def caracteres(arbol: ArbolHuff): List[Char] =
    def caracteresAux(arbol: ArbolHuff, Lista: List[Char]): List[Char] = arbol match
      case NodoHuff(caracter, frecuencia) => caracter :: Lista
      case RamaHuff(nodoDch, nodoIzq) => caracteresAux(nodoIzq, Lista) ::: caracteresAux(nodoDch, Lista)

    caracteresAux(arbol, Nil)

  def cadenaAListaChars(cadena: String): List[Char] =
    cadena.toList

  def listaCharsACadena(listaChar: List[Char]): String =
    listaChar.mkString
}


