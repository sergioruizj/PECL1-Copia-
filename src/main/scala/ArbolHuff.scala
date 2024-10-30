import scala.annotation.tailrec

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
    @tailrec
    def descodAux(arbol: ArbolHuff, bits: List[Bit]): (Char, List[Bit]) = arbol match
      case RamaHuff(nodoDch, nodoIzq) => bits match
        case h :: t => if h == 0 then descodAux(nodoIzq,t) else descodAux(nodoDch,t)
        case _ => throw new Error("La lista de Bits introducida no es posible en este árbol")
      case NodoHuff(caracter, frecuencia) => (caracter,bits)

    @tailrec
    def descodFinal(listaC: List[Char], listaB: List[Bit]): String = listaB match
      case Nil => listaCharsACadena(listaC.reverse)
      case _ => val (caracter, restoBits) = descodAux(this, listaB)
        descodFinal(caracter :: listaC, restoBits)

    descodFinal(Nil, bits)

//  def codificar(cadena: String): List[Bit] = Preguntar el tipo de búsqueda llegar a los caracteres y sacar los bits

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
    val listaB: List[arbol.Bit] = List(0, 1)
    println(arbol.descodificar(listaB))
  }
}