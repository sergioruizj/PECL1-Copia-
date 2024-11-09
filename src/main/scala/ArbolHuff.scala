import scala.annotation.tailrec

abstract class ArbolHuff {
  
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
  
  def descodificar(bits: List[Bit]): String =
    @tailrec
    def descodAux(arbol: ArbolHuff, bits: List[Bit]): (Char, List[Bit]) = arbol match
      case RamaHuff(nodoDch, nodoIzq) => bits match
        case h :: t => if h == 0 then descodAux(nodoIzq, t) else descodAux(nodoDch, t)
        case _ => throw new Error("La lista de Bits introducida no es posible en este árbol")
      case NodoHuff(caracter, frecuencia) => (caracter, bits)

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

type Bit = 0 | 1

type TablaCodigos = List[(Char, List[Bit])]

def cadenaAListaChars(cadena: String): List[Char] =
  cadena.toList

def listaCharsACadena(listaChar: List[Char]): String =
  listaChar.mkString

/////////////////// CREACIÓN DEL ÁRBOL ////////////////////////
def crearArbolHuffman(cadena: String): ArbolHuff =
  // Generación de la lista de nodos ordenados
  val listaHojas = distribFrecAListaHojas(listaCharsAdistFrec(cadenaAListaChars(cadena)))

  // Creación del arbol
  val arbol: List[ArbolHuff] = repetirHasta(combinar, esListaSingleton)(listaHojas)

  arbol.head


// Generar una lista de tuplas (caracter, frecuencia) a partir de una lista de caracteres
private def listaCharsAdistFrec(listaChar: List[Char]): List[(Char, Int)] =

  // Método para meter un caracter a una lista caracter-frecuencia
  def actualizarFrec(c: Char, lista: List[(Char, Int)]): List[(Char, Int)] = lista match
    case Nil => List((c, 1))
    case (caracter, frecuencia) :: t =>
      if (c == caracter) then (c, frecuencia + 1) :: t else (caracter, frecuencia) :: actualizarFrec(c, t)

  // Método generador de la lista de tuplas
  def AuxListaCharsAdistFrec(listaChar: List[Char], listaFinal: List[(Char, Int)]): List[(Char, Int)] = listaChar match
    case Nil => listaFinal
    case head :: tail =>
      val resultado = actualizarFrec(head, listaFinal)
      AuxListaCharsAdistFrec(tail, resultado)

  AuxListaCharsAdistFrec(listaChar, List())


// Convertir una tupla en un nodo hoja
private def tuplaANodo(tupla: (Char, Int)): NodoHuff = tupla match {
  case (caracter, frecuencia) => NodoHuff(caracter, frecuencia)
}


// Convertir la lista de tuplas en una lista de hojas ordenada de forma creciente según el peso
private def distribFrecAListaHojas(frec: List[(Char, Int)]): List[NodoHuff] = {

  //Inserta el nodo ordenándolo dentro de la lista
  def insertaNodoOrdenado(nodo: NodoHuff, lista: List[NodoHuff]): List[NodoHuff] = lista match {
    case Nil => List(nodo)
    case head :: tail =>
      if (nodo.frecuencia <= head.frecuencia) nodo :: lista
      else head :: insertaNodoOrdenado(nodo, tail)
  }

  //Recorre la lista de tuplas y las ordena convirtiéndolas en nodos
  @tailrec
  def distribFrecAListaHojasAux(tuplas: List[(Char, Int)], listaOrdenada: List[NodoHuff] = Nil): List[NodoHuff] = tuplas match {
    case Nil => listaOrdenada
    case head :: tail =>
      val nodo = tuplaANodo(head) // Convertimos la tupla en un nodo
      distribFrecAListaHojasAux(tail, insertaNodoOrdenado(nodo, listaOrdenada))
  }

  distribFrecAListaHojasAux(frec)
}


// Crea un objeto RamaHuff integrando los dos ArbolHuff (izquierdo y derecho) que se le pasan como parámetros
private def creaRamaHuff(izq: ArbolHuff, dch: ArbolHuff): RamaHuff = {
  RamaHuff(izq, dch)
}


// Método auxiliar para crear una rama a partir de los dos nodos de menor peso e insertarla en la lista
private def combinar(nodos: List[ArbolHuff]): List[ArbolHuff] = {

  def insertarOrdenado(nodo: ArbolHuff, lista: List[ArbolHuff]): List[ArbolHuff] = lista match {
    case Nil => List(nodo)
    case head :: tail =>
      if (nodo.peso <= head.peso) nodo :: lista
      else head :: insertarOrdenado(nodo, tail)
  }

  nodos match {
    case izq :: dch :: tail =>
      val nuevaRama = creaRamaHuff(izq, dch)
      insertarOrdenado(nuevaRama, tail)
    case _ => nodos
  }
}


//Función booleana que evalúe que la lista resultante tenga solamente un elemento
private def esListaSingleton(lista: List[ArbolHuff]): Boolean = lista.length == 1


//Función currificada que reciba tres parámetros en dos pasos: por un lado, las funciones combinar y esListaSingleton, y, por otro lado, la lista de nodos hoja
private def repetirHasta[A](f: List[A] => List[A], condicion: List[A] => Boolean)(lista: List[A]): List[A] = {
  if (condicion(lista)) lista
  else repetirHasta(f, condicion)(f(lista))
}


//////////////////////  CREACIÓN DE UNA TABLA PARA LA CODIFICACIÓN DE MENSAJES ///////////////////////

def deArbolATabla(arbol: ArbolHuff): TablaCodigos =

  // Método para comprobar si un caracter se encuentra en un arbol
  def hay_caracter_en_arbol(arbol: ArbolHuff, caracter: Char): Boolean =
    def aux_caracter(caracter: Char, listaC: List[Char]): Boolean =
      if caracter == listaC.head then true
      else if listaC.tail.nonEmpty then aux_caracter(caracter, listaC.tail)
      else false

    aux_caracter(caracter, arbol.caracteres)

  // Generación de la cadena de bits a partir de un arbol y un caracter
  def generarCadenaBits(caracter: Char, arbolHuff: ArbolHuff, listaB: List[Bit]): List[Bit] = arbolHuff match
    case RamaHuff(nodoDch, nodoIzq) =>
      if hay_caracter_en_arbol(nodoDch, caracter) then generarCadenaBits(caracter, nodoDch, 1 :: listaB) else generarCadenaBits(caracter, nodoIzq, 0 :: listaB)
    case NodoHuff(caracter, frecuencia) => listaB.reverse

  // Generación de la tabla de códigos
  def auxdeArbolATabla(caracteres: List[Char], arbol: ArbolHuff, tablaCodigos: TablaCodigos): TablaCodigos = caracteres match
    case Nil => tablaCodigos.reverse
    case head :: tail =>
      auxdeArbolATabla(tail, arbol, (head, generarCadenaBits(head, arbol, List())) :: tablaCodigos)

  auxdeArbolATabla(arbol.caracteres, arbol, List())

def codificar(arbol: TablaCodigos)(cadena: String): List[Bit] =

  // Método para buscar el codigo binaro correspondiente a un caracter
  def buscarCodigo(caracter: Char, arbol: TablaCodigos): List[Bit] = arbol match
    case (car, listaB) :: tail => if car == caracter then listaB else buscarCodigo(caracter, tail)


  def auxCodificar(caracteres: List[Char], arbol: TablaCodigos, listaSalida: List[Bit]): List[Bit] = caracteres match
    case Nil => listaSalida.reverse
    case head :: tail =>
      val codigo: List[Bit] = buscarCodigo(head, arbol)
      auxCodificar(tail, arbol, codigo.reverse ::: listaSalida)

  auxCodificar(cadena.toList, arbol, List())
  
  

object ArbolHuff {
  def apply(cadena: String): ArbolHuff = crearArbolHuffman(cadena)
}

@main def main(): Unit = {
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
  val listaB: List[Bit] = List(0, 0)
  println(arbol.descodificar(listaB))
  println(arbol.codificar("SSSSO ES"))
  

  val frecuencias = List(('a', 5), ('b', 2), ('c', 7), ('d', 1))
  val listaHojas = arbol.distribFrecAListaHojas(frecuencias)
  println(listaHojas) // Debería imprimir los nodos ordenados: d, b, a, c

  // Probador del metodo deArbolATabla
  println(arbol.deArbolATabla(arbol))

  /***
   * 1. He seguido los pasos para en teoría acabar con la creación del árbol pero me da este error (como estoy en el avión no puedo meterme en ChatGPT para ver que coño pasa.
   *    Creo que el problema esta en la creación del "object ArbolHuff" no sé exactamente dónde hay que crearlo (yo lo he puesto dentro de la clase abstracta, no se si hay que ponerlo fuera, he pribado a hacerlo y tampoco va)
   *    Porfi cuando veas esto mira donde hacerlo. Yo voy a seguir con lo siguiente para ver si son cosas que si que puedo hacer, dejo este error en rojo para que puedas verlo.
   *    Cuando hagas esto comprueba que se haga bien (ahora mismo no se como podríamos comprobarlo pero de alguna forma se podrá hacer)
   *
   * 2. Hecho ya el metodo de crear una tabla a partir de un arbol, lo he comprobado con el arbolillo que tenemos hecho en el probador, creo que esta bien.
   *    De todas formas porfa cuando veas esto asegurate de que esté bien
   *
   * 3. Método para codificar strings con listas caracter-codigo hecho. Igual que antes, necesito probarlos
   */

  // Esto es lo que digo en el punto 1 del comentario grande
   val miArbol: ArbolHuff = ArbolHuff("Hola")


  // Construir un árbol de Huffman simple para pruebas
  val nodooA = NodoHuff('A', 5)
  val nodooB = NodoHuff('B', 2)
  val nodooC = NodoHuff('C', 1)
  val ramaa1 = RamaHuff(nodooC, nodooB) // Rama que contiene los nodos 'C' y 'B'
  val arbool = RamaHuff(ramaa1, nodooA) // Rama final que une 'A' con la rama que contiene 'C' y 'B'

  // Probador para el método deArbolATabla
  val tablaCodigos = arbool.deArbolATabla(arbool)
  println("Tabla de códigos generada a partir del árbol:")
  tablaCodigos.foreach { case (caracter, bits) =>
    println(s"Caracter: $caracter, Código: ${bits.mkString}")
  }

  // Ejemplo de salida esperada:
  //   - 'A' podría estar codificado como `1`
  //   - 'B' podría estar codificado como `01`
  //   - 'C' podría estar codificado como `00`
}