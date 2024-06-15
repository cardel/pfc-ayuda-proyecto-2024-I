package proyecto

import scala.annotation.tailrec

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]
  type Itinerario = List[Vuelo]

  /*
    @param vuelos: List[Vuelo] recive una lista de vuelos
    @param aeropuertos: List[Aeropuerto] recive una lista de aeropuertos
    @return (String, String) => List[Itinerario] Retorna una función que recibe los codigos de dos aeropuertos, Retorna todos los itinerarios posibles de cod1 a cod2
  */
  def itinerarios(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // Función DFS para encontrar todas las rutas
    def dfs(inicio: String, fin: String): List[Itinerario] = {
      def dfsAux(itineAcc: Itinerario, visitado: Set[String]): List[Itinerario] = {
        val currentAirport = itineAcc.head.Dst // Aeropuerto de destino del vuelo actual
        if (currentAirport == fin) List(itineAcc) // Si llegamos al destino, devolvemos la ruta
        else if (visitado.contains(currentAirport)) Nil // Si ya hemos visitado el aeropuerto actual, no seguimos, evitamos ciclos
        else {
          val nextFlights = for { // Buscamos los vuelos que salen del aeropuerto actual y que no hemos visitado
            vuelo <- vuelos
            if vuelo.Org == currentAirport && !visitado.contains(vuelo.Dst)
          } yield vuelo

          nextFlights.flatMap(vuelo => dfsAux(vuelo :: itineAcc, visitado + currentAirport))
        }
      }

      val initialFlights = for { // Buscamos los vuelos que salen del aeropuerto de inicio
        vuelo <- vuelos
        if vuelo.Org == inicio
      } yield vuelo

      initialFlights.flatMap(vuelo => dfsAux(List(vuelo), Set(inicio)))
    }
    // Devolver la función, se le hace el reverse a las rutas para que estén en el orden inico => fin
    (cod1: String, cod2: String) => dfs(cod1, cod2).map(_.reverse)
  }

  /*
    @param vuelos: List[Vuelo] recive una lista de vuelos
    @param aeropuertos: List[Aeropuerto] recive una lista de aeropuertos
    @return (String, String) => List[Itinerario] Retorna una función que recibe los codigos de dos aeropuertos, Retorna todos los itinerarios posibles de cod1 a cod2
    que minimizan el tiempo total de viaje

  */
  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // Crear un mapa de aeropuertos para búsqueda rápida por código
    val aeropuertoMap: Map[String, Aeropuerto] = aeropuertos.map(a => a.Cod -> a).toMap
    // Función para calcular el tiempo de vuelo en minutos considerando GMT
    /*
      @param vuelo: Vuelo recive un vuelo
      @return Int Retorna el tiempo de vuelo en minutos
    */
    def calcularTiempoVuelo(vuelo: Vuelo): Int = {
      val aeropuertoOrigen = aeropuertoMap(vuelo.Org)
      val aeropuertoDestino = aeropuertoMap(vuelo.Dst)
      val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT)/100
      val salidaEnMinutos = vuelo.HS * 60 + vuelo.MS
      val llegadaEnMinutos = vuelo.HL * 60 + vuelo.ML + (diferenciaGMT * 60).toInt
      if (llegadaEnMinutos >= salidaEnMinutos) {
        llegadaEnMinutos - salidaEnMinutos
      } else {
        (1440 + llegadaEnMinutos) - salidaEnMinutos
      }
    }

    /*
      @param vuelo1: Vuelo recive un vuelo
      @param vuelo2: Vuelo recive un vuelo
      @return Int Retorna el tiempo de espera en minutos
    */
    def calcularTiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo): Int = {
      val aeropuertoDestinoVuelo1 = aeropuertoMap(vuelo1.Dst)
      val aeropuertoOrigenVuelo2 = aeropuertoMap(vuelo2.Org)
      val diferenciaGMT = (aeropuertoOrigenVuelo2.GMT - aeropuertoDestinoVuelo1.GMT)/100
      val llegadaVuelo1 = vuelo1.HL * 60 + vuelo1.ML + (diferenciaGMT * 60).toInt
      val salidaVuelo2 = vuelo2.HS * 60 + vuelo2.MS
      if (salidaVuelo2 >= llegadaVuelo1) {
        salidaVuelo2 - llegadaVuelo1
      } else {
        (1440 + salidaVuelo2) - llegadaVuelo1
      }
    }

    /*
      @param itinerario: Itinerario recive un itinerario
      @return Int Retorna el tiempo total de un itinerario en minutos
    */
    def calcularTiempoTotalItinerario(itinerario: Itinerario): Int = {
      val tiemposVuelo = itinerario.map(calcularTiempoVuelo)
      val tiemposEspera = itinerario.sliding(2).collect {
        case List(vuelo1, vuelo2) => calcularTiempoEspera(vuelo1, vuelo2)
      }.toList
      tiemposVuelo.sum + tiemposEspera.sum
    }
    // Función para obtener todos los itinerarios posibles de cod1 a cod2
    (cod1: String, cod2: String) => {
      val obtenerItinerarios = itinerarios(vuelos, aeropuertos)
      val todosLosItinerarios = obtenerItinerarios(cod1, cod2)
      val itinerariosConTiempo = for {
        itinerario <- todosLosItinerarios
        tiempoTotal = calcularTiempoTotalItinerario(itinerario)
      } yield (itinerario, tiempoTotal)

      val itinerariosOrdenados = itinerariosConTiempo.sortBy(_._2)
      val mejoresItinerarios = itinerariosOrdenados.take(3).map(_._1)
      mejoresItinerarios.sortBy(itinerario => calcularTiempoTotalItinerario(itinerario))
    }
  }


  /*
    @param vuelos: List[Vuelo] recive una lista de vuelos
    @param aeropuertos: List[Aeropuerto] recive una lista de aeropuertos
    @return (String, String) => List[Itinerario] Retorna una función que recibe los codigos de dos aeropuertos, Retorna todos los itinerarios posibles de cod1 a cod2
    que minimizan el número de escalas
  */
  def itinerariosEscalas(vuelos:List[Vuelo], aeropuertos:List[Aeropuerto]):(String, String)=>List[Itinerario]
  = {
    @tailrec
    def sumarEscalas(itinerario: Itinerario, acc:Int=0): Int = {
      itinerario match {
        case Nil => acc
        case h::Nil =>
          acc + h.Esc
        case h::t =>
          sumarEscalas(t, acc+h.Esc+1)
      }
    }
    (code1: String, code2:String) => {
      val itinerario = itinerarios(vuelos, aeropuertos)
      val sumaEscalas = for{
        i <- itinerario(code1, code2)
        sumas = sumarEscalas(i)
      } yield (i, sumas)
      sumaEscalas.sortBy(_._2).take(3).map(_._1)
    }
  }


  def itinerariosAire(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el tiempo en itinerarios
    (cod1:String, cod2:String)=> List[Itinerario]()
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos y dos enteros, que es la hora de la cita
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que permiten llegar a una hora de la cita
    (cod1:String, cod2:String, HC:Int, MC:Int)=> List[Itinerario]()
  }


}

object prueba{
  //Funcion para probar la fucnion itinerarios
  def main(args: Array[String]): Unit = {
        val itinerario = new Itinerario()
        val itsCurso = itinerario.itinerariosEscalas(datos.vuelosCurso, datos.aeropuertosCurso)
    //    val its1 = itsCurso("MID", "SVCS")
    //    val its2 = itsCurso("CLO", "SVCS")
          val its3 = itsCurso("CLO", "SVO")
    //    val its4 = itsCurso("CLO", "MEX")
    //    val its5 = itsCurso("CTG", "PTY")
          println(its3)

    //    val itsCurs = itinerario.itinerarios(datos.vuelosD1, datos.aeropuertos)
    //    val its11 = itsCurs("ORD", "LAX")
      //   print(its11)

  }
}
