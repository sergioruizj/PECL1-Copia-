abstract class ArbolHuff {

  type Bit = 0 | 1

  def peso: Int =
    def pesoAux(arbol: ArbolHuff): Int = arbol match
      case NodoHuff(caracter, frecuencia) => frecuencia
      case RamaHuff(nodoDch, nodoIzq) => pesoAux(nodoIzq) + pesoAux(nodoDch)

    pesoAux(this)

  def caracteres: List[Char] =
    def caracteresAux(arbol: ArbolHuff, Lista: List[Char]): List[Char] = arbol match
      case NodoHuff(caracter, frecuencia) => caracter :: Lista
      case RamaHuff(nodoDch, nodoIzq) => caracteresAux(nodoIzq, Lista) ::: caracteresAux(nodoDch, Lista)

    caracteresAux(this, Nil)

  def cadenaAListaChars(cadena: String): List[Char] =
    cadena.toList

  def listaCharsACadena(listaChar: List[Char]): String =
    listaChar.mkString

  def descodificar(bits: List[Bit]): String =
//    def descodAux(arbol: ArbolHuff, bits: List[Bit]): (Char, List[Bit]) = bits match
//      case h :: t => if h == 0 then {
//        arbol match
//          case RamaHuff(nodoDch, nodoIzq) => descodAux(nodoIzq, t)
//          case NodoHuff(caracter, frecuencia) => (caracter, t)
//      } else {
//        arbol match
//          case RamaHuff(nodoDch, nodoIzq) => descodAux(nodoDch, t)
//          case NodoHuff(caracter, frecuencia) => (caracter, t)
//      }
    def descodAux(arbol: ArbolHuff, bits: List[Bit]): (Char, List[Bit]) = arbol match
      case RamaHuff(nodoDch, nodoIzq) => bits match
        case h :: t => if h == 0 then descodAux(nodoIzq,t) else descodAux(nodoDch,t)
      case NodoHuff(caracter, frecuencia) => (caracter,bits)




    var listaC: List[Char] = List();
    var listaB: List[Bit] = bits;
    while (listaB.nonEmpty) {
      val (caracter, restoBits) = descodAux(this, listaB)
      listaC = caracter :: listaC
      listaB = restoBits
    }

    listaCharsACadena(listaC.reverse)

}


object ArbolHuffTest {

  def main(args: Array[String]): Unit = {
    // Construir un árbol de Huffman simple para pruebas
    val nodoS = NodoHuff('S', 4)
    val nodoO = NodoHuff('O', 3)
    val nodoE = NodoHuff('E', 2)
    val nodoEsp = NodoHuff(' ', 2)
    val rama1 = RamaHuff(nodoEsp,nodoE)
    val rama2 = RamaHuff(rama1, nodoO)
    val arbol = RamaHuff(rama2, nodoS)

    // Probar el método peso
    println(s"Peso del árbol: ${arbol.peso}")  // Debe ser 5 + 7 + 10 = 22
    val listaB: List[arbol.Bit] = List(0,1,0,1,1,1,0,0,1,0)
    println(arbol.descodificar(listaB))
  }
}