def cadenaAListaChars(cadena: String): List[Char] =
  cadena.toList

def listaCharsACadena(listaChar: List[Char]): String =
  listaChar.mkString

def peso(arbol: ArbolHuff): Int = arbol match
  case NodoHuff(caracter, frecuencia) => frecuencia
  case RamaHuff(nodoDch, nodoIzq) => peso(nodoIzq) + peso(nodoDch)

def caracteres(arbol: ArbolHuff): List[Char] =
  def caracteresAux(arbol: ArbolHuff, Lista: List[Char]): List[Char] = arbol match
    case NodoHuff(caracter, frecuencia) => caracter :: Lista
    case RamaHuff(nodoDch, nodoIzq) => caracteresAux(nodoIzq, Lista) ::: caracteresAux(nodoDch, Lista)

  caracteresAux(arbol, Nil)


listaCharsACadena(cadenaAListaChars("Hola, que tal te va la vida?"))

val nodo1 = NodoHuff('a', 3)
val nodo2 = NodoHuff('b', 0)
val nodo3 = NodoHuff('c', 56)

val arbol1 = RamaHuff(nodo1,nodo2)
val arbol2 = RamaHuff(arbol1,nodo3)

caracteres(arbol2)
peso(arbol1)
peso(nodo3)

6+8

1+1