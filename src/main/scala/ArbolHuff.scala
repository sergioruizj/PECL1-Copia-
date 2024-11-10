import scala.annotation.tailrec
import scala.io.StdIn.readLine

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
      @tailrec
      def aux_caracter(caracter: Char, listaC: List[Char]): Boolean =
        if caracter == listaC.head then true
        else if listaC.tail.nonEmpty then aux_caracter(caracter, listaC.tail)
        else false

      aux_caracter(caracter, arbol.caracteres)

    @tailrec
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

def cadenaAListaChars(cadena: String): List[Char] = cadena.toList

def listaCharsACadena(listaChar: List[Char]): String = listaChar.mkString

/////////////////// CREACIÓN DEL ÁRBOL ////////////////////////

object ArbolHuff {
  def apply(cadena: String): ArbolHuff = crearArbolHuffman(cadena)

  private def crearArbolHuffman(cadena: String): ArbolHuff =
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
        if c == caracter then (c, frecuencia + 1) :: t else (caracter, frecuencia) :: actualizarFrec(c, t)

    // Método generador de la lista de tuplas
    @tailrec
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
  @tailrec
  private def repetirHasta[A](f: List[A] => List[A], condicion: List[A] => Boolean)(lista: List[A]): List[A] = {
    if (condicion(lista)) lista
    else repetirHasta(f, condicion)(f(lista))
  }
}

//////////////////////  CREACIÓN DE UNA TABLA PARA LA CODIFICACIÓN DE MENSAJES ///////////////////////

def deArbolATabla(arbol: ArbolHuff): TablaCodigos =

  // Método para comprobar si un caracter se encuentra en un arbol
  def hay_caracter_en_arbol(arbol: ArbolHuff, caracter: Char): Boolean =
    @tailrec
    def aux_caracter(caracter: Char, listaC: List[Char]): Boolean =
      if caracter == listaC.head then true
      else if listaC.tail.nonEmpty then aux_caracter(caracter, listaC.tail)
      else false

    aux_caracter(caracter, arbol.caracteres)

  // Generación de la cadena de bits a partir de un arbol y un caracter
  @tailrec
  def generarCadenaBits(caracter: Char, arbolHuff: ArbolHuff, listaB: List[Bit]): List[Bit] = arbolHuff match
    case RamaHuff(nodoDch, nodoIzq) =>
      if hay_caracter_en_arbol(nodoDch, caracter) then generarCadenaBits(caracter, nodoDch, 1 :: listaB) else generarCadenaBits(caracter, nodoIzq, 0 :: listaB)
    case NodoHuff(caracter, frecuencia) => listaB.reverse

  // Generación de la tabla de códigos
  @tailrec
  def auxdeArbolATabla(caracteres: List[Char], arbol: ArbolHuff, tablaCodigos: TablaCodigos): TablaCodigos = caracteres match
    case Nil => tablaCodigos.reverse
    case head :: tail =>
      auxdeArbolATabla(tail, arbol, (head, generarCadenaBits(head, arbol, List())) :: tablaCodigos)

  auxdeArbolATabla(arbol.caracteres, arbol, List())

def codificar(arbol: TablaCodigos)(cadena: String): List[Bit] =

  // Método para buscar el codigo binaro correspondiente a un caracter
  @tailrec
  def buscarCodigo(caracter: Char, arbol: TablaCodigos): List[Bit] = arbol match
    case (car, listaB) :: tail => if car == caracter then listaB else buscarCodigo(caracter, tail)
    case Nil => Nil

  @tailrec
  def auxCodificar(caracteres: List[Char], arbol: TablaCodigos, listaSalida: List[Bit]): List[Bit] = caracteres match
    case Nil => listaSalida.reverse
    case head :: tail =>
      val codigo: List[Bit] = buscarCodigo(head, arbol)
      if codigo == Nil then throw new Error("Alguno de los caractéres introducidos no se encuentra en el árbol") else auxCodificar(tail, arbol, codigo.reverse ::: listaSalida)

  auxCodificar(cadena.toList, arbol, List())

def decodificar(tabla: TablaCodigos)(lista: List[Bit]): String =
  @tailrec
  def decodificarAux(tabla: TablaCodigos, lista: List[Bit], cont: Int): (Char, List[Bit]) =
    val filter = tabla.filter((x, y) => y == lista.take(cont))
    if cont <= tabla.length then {
      if filter == Nil then decodificarAux(tabla, lista, cont + 1) else (filter.head._1, lista.drop(cont))
    }
    else throw new Error("La lista de Bits introducida no es posible en este árbol")

  @tailrec
  def decodificarFinal(tabla: TablaCodigos, lista: List[Bit], listaC: List[Char]): String = lista match
    case Nil => listaC.reverse.mkString
    case h :: t =>
      val (car, list) = decodificarAux(tabla, lista, 1)
      decodificarFinal(tabla, list, car :: listaC)

  decodificarFinal(tabla, lista, Nil)


//object miPrograma extends App{
//  // Construir un árbol de Huffman simple para pruebas
//  val nodoS = NodoHuff('S', 4)
//  val nodoO = NodoHuff('O', 3)
//  val nodoE = NodoHuff('E', 2)
//  val nodoEsp = NodoHuff(' ', 2)
//  val rama1 = RamaHuff(nodoEsp, nodoE)
//  val rama2 = RamaHuff(rama1, nodoO)
//  val arbol = RamaHuff(rama2, nodoS)
//
//  val tabla = deArbolATabla(arbol)
//  println(tabla)
//
//  val arbol2 = ArbolHuff("this is an example of a huffman tree")
//  val tabla2 = deArbolATabla(arbol2)
//  println(tabla2)
//  println(arbol2.caracteres)
//  val decodArbolP = decodificar(tabla2)
//
//  println(decodArbolP(codificar(tabla2)("aef i")))
//
//}

object Main extends App {

  def inicializarArbol(): ArbolHuff = {
    print("Bienvenido al probador de árboles Huffman!!\n" +
      "Empecemos primero creando tu árbol, que nos dará las claves para poder codificar y decodificar" +
      "todos los textos que se te ocurran.\n" +
      "Por favor, introduzca la cadena de caracteres que crearán tu árbol: ")
    val cadenaCreacionArbol = readLine()
    println("Creando árbol...")
    val miArbol = ArbolHuff(cadenaCreacionArbol)
    print("Árbol Huffman creado con éxito!!\n")
    miArbol
  }

  def inicializarTabla(arbolHuff: ArbolHuff): TablaCodigos = {
    deArbolATabla(arbolHuff)
  }

  def imprimirMenuPrincipal(): Unit = {
    println("Bienvenido al menú principal.")
    println("   1. Ver el peso del árbol")
    println("   2. Ver la lista de caracteres del árbol")
    println("   3. Codificar un texto recorriendo el árbol")
    println("   4. Decodificar una secuencia de bits recorriendo el árbol")
    println("   5. Codificar un texto usando la tabla de códigos")
    println("   6. Decodificar una secuencia de bits usando la tabla de códigos")
    println("   7. Comprobar si un carácter específico está en el árbol")
    println("   8. Mostrar las frecuencias de caracteres en el árbol")
    println("   9. Crear un nuevo árbol con otra cadena")
    println("   10. Salir del programa")
    print("Por favor, elige la opción deseada: ")
  }

  def seguirOFinalizar(arbolHuff: ArbolHuff, tablaCodigos: TablaCodigos): Unit = {
    println("Por favor elija entre las siguientes opciones: ")
    println("   1. Volver al menú principal")
    println("   2. Salir del programa")
    print("Por favor, elige la opción deseada: ")
    val opcion2 = readLine().toInt
    opcion2 match {
      case 1 => menuRecursivo(arbolHuff, tablaCodigos)
      case 2 => println("Saliendo del programa. Hasta luego!!!")
      case _ => println("Opción seleccionada no válida. Inténtalo de nuevo."); seguirOFinalizar(arbolHuff, tablaCodigos)
    }
  }

  def menuRecursivo(arbolHuff: ArbolHuff, tablaCodigos: TablaCodigos): Unit = {
    imprimirMenuPrincipal()
    val opcion = readLine().toInt

    opcion match {
      // 1. Ver el peso del árbol
      case 1 =>
        println(s"El peso del árbol es: ${arbolHuff.peso}")
        seguirOFinalizar(arbolHuff, tablaCodigos)

      // 2. Ver la lista de caracteres del árbol
      case 2 =>
        println(s"Lista de caracteres del árbol: ${arbolHuff.caracteres.mkString(", ")}")
        seguirOFinalizar(arbolHuff, tablaCodigos)

      // 3. Codificar un texto recorriendo el árbol
      case 3 =>
        println("Introduce el texto a codificar:")
        val texto = readLine()
        val bitsCodificados = arbolHuff.codificar(texto)
        println(s"Texto codificado: ${bitsCodificados.mkString}")
        seguirOFinalizar(arbolHuff, tablaCodigos)

      // 4. Decodificar una secuencia de bits recorriendo el árbol
      case 4 =>
        println("Introduce la secuencia de bits a decodificar (ejemplo: 010101):")
        val bits = readLine().toList.collect { case '0' => 0; case '1' => 1 }
        val textoDecodificado = arbolHuff.descodificar(bits) 
        println(s"Texto decodificado: $textoDecodificado")
        seguirOFinalizar(arbolHuff, tablaCodigos)

      // 5. Codificar un texto usando la tabla de códigos
      case 5 =>
        println("Introduce el texto a codificar usando la tabla:")
        val texto = readLine()
        val bitsCodificados = codificar(tablaCodigos)(texto)
        println(s"Texto codificado: ${bitsCodificados.mkString}")
        seguirOFinalizar(arbolHuff, tablaCodigos)

      // 6. Decodificar una secuencia de bits usando la tabla de códigos
      case 6 =>
        println("Introduce la secuencia de bits a decodificar (ejemplo: 010101):")
        val bits = readLine().toList.collect { case '0' => 0; case '1' => 1 }
        val textoDecodificado = arbolHuff.descodificar(bits)
        println(s"Texto decodificado: $textoDecodificado")
        seguirOFinalizar(arbolHuff, tablaCodigos)
        val textoDecodificado = decodificar(tablaCodigos)(bits)
        println(s"Texto decodificado: $textoDecodificado")
        seguirOFinalizar(arbolHuff, tablaCodigos)

      // 7. Comprobar si un carácter específico está en el árbol
      case 7 =>
        println("Introduce el carácter a comprobar:")
        val caracter = readLine().headOption.getOrElse(' ')
        println(s"El carácter '$caracter' ${if (arbolHuff.caracteres.contains(caracter)) "sí" else "no"} está en el árbol.")
        seguirOFinalizar(arbolHuff, tablaCodigos)

      // 8. Mostrar las frecuencias de caracteres en el árbol
      case 8 =>
        println("Frecuencias de caracteres en el árbol:")
        arbolHuff.caracteres.foreach { char =>
          val frecuencia = arbolHuff match {
            case NodoHuff(`char`, freq) => freq
            case _ => 0
          }
          println(s"$char -> $frecuencia")
        }
        seguirOFinalizar(arbolHuff, tablaCodigos)

      // 9. Crear un nuevo árbol con otra cadena
      case 9 =>
        val nuevoArbol = inicializarArbol()
        val nuevaTabla = inicializarTabla(nuevoArbol)
        menuRecursivo(nuevoArbol, nuevaTabla)

      // 10. Salir del programa
      case 10 =>
        println("Saliendo del programa. Hasta luego!!")

      // Opción no válida
      case _ =>
        println("Opción seleccionada no válida. Inténtalo de nuevo.")
        menuRecursivo(arbolHuff, tablaCodigos)
    }
  }

  val arbolInicial = inicializarArbol()
  val tablaInicial = inicializarTabla(arbolInicial)
  menuRecursivo(arbolInicial, tablaInicial)
}