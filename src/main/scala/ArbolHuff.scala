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
    def descodAux(arbol: ArbolHuff, bits: List[Bit]): (Char, List[Bit]) = bits match
      case h :: t => if h == 0 then {
        arbol match
          case RamaHuff(nodoDch, nodoIzq) => descodAux(nodoIzq, t)
          case NodoHuff(caracter, frecuencia) => (caracter, t)
      } else {
        arbol match
          case RamaHuff(nodoDch, nodoIzq) => descodAux(nodoDch, t)
          case NodoHuff(caracter, frecuencia) => (caracter, t)
      }

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
    val ramaAB = RamaHuff(nodoA, nodoB)
    val arbol = RamaHuff(ramaAB, nodoC)  // Árbol con A, B y C

    // Probar el método peso
    println(s"Peso del árbol: ${arbol.peso}")  // Debe ser 5 + 7 + 10 = 22

    // Probar el método caracteres
    println(s"Caracteres en el árbol: ${arbol.caracteres.mkString(", ")}")  // Debe ser A, B, C

    // Probar el método descodificar
    val bits: List[arbol.Bit] = List(0, 0, 1, 0, 1, 1)  // Representación de una secuencia de bits
    println(s"Descodificación de bits: ${arbol.descodificar(bits)}")
  }
}