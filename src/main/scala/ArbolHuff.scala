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

  def codificar(cadena: String): List[Bit] =

    // Método para comprobar si un caracter se encuentra en un arbol
    def hay_caracter_en_arbol(arbol: ArbolHuff, caracter: Char): Boolean =
      def aux_caracter(caracter: Char, listaC: List[Char]): Boolean =
        if caracter == listaC.head then true
        else if listaC.tail.nonEmpty then aux_caracter(caracter, listaC.tail)
        else false

      aux_caracter(caracter, arbol.caracteres)


    def auxCodificar(lista_cod: List[Char], arbol: ArbolHuff, listaB: List[Bit]): List[Bit] = lista_cod match
      case Nil => listaB.reverse
      case h :: t => arbol match
        case RamaHuff(nodoDch, nodoIzq) =>
          if hay_caracter_en_arbol(nodoIzq, lista_cod.head) then auxCodificar(lista_cod, nodoIzq, 0 :: listaB)
          else if hay_caracter_en_arbol(nodoDch, lista_cod.head) then auxCodificar(lista_cod, nodoDch, 1 :: listaB)
          else throw new Error("Alguno de los caractéres introducidos no se encuentra en el árbol")
        case NodoHuff(caracter, frecuencia) => auxCodificar(t, this, listaB)


    auxCodificar(cadenaAListaChars(cadena), this, Nil)
}

case class NodoHuff(caracter: Char, frecuencia: Int) extends ArbolHuff

case class RamaHuff(nodoDch: ArbolHuff, nodoIzq: ArbolHuff) extends ArbolHuff
def main(args: Array[String]): Unit = {
  // Construir un árbol de Huffman simple para pruebas
  val nodoS = NodoHuff('S', 4)
  val nodoO = NodoHuff('O', 3)
  val nodoE = NodoHuff('E', 2)
  val nodoEsp = NodoHuff(' ', 2)
  val rama1 = RamaHuff(nodoEsp, nodoE)
  val rama2 = RamaHuff(rama1, nodoO)
  val arbol = RamaHuff(rama2, nodoS)

  // Probar el método peso
  println(s"Peso del árbol: ${arbol.peso}") // Debe ser 5 + 7 + 10 = 22
  val listaB: List[arbol.Bit] = List(0, 0)
  println(arbol.descodificar(listaB))
  println(arbol.codificar("SSSSO ES"))
  println(arbol.codificar("1"))

}
