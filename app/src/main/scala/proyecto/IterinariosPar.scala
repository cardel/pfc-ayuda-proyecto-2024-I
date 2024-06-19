package proyecto

import common._

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq


class ItinerariosPar() {
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]
  type Itinerario = List[Vuelo]

  def itinerariosPar(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
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

      initialFlights.par.flatMap(vuelo => dfsAux(List(vuelo), Set(inicio))).toList
    }
    // Devolver la función, se le hace el reverse a las rutas para que estén en el orden inico => fin
    (cod1: String, cod2: String) => dfs(cod1, cod2).par.map(_.reverse).toList
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
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
      val llegadaEnMinutos = vuelo.HL * 60 + vuelo.ML - (diferenciaGMT * 60).toInt
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
      val par_Vuelo_Espera = parallel(itinerario.map(calcularTiempoVuelo), itinerario.sliding(2).collect {
        case List(vuelo1, vuelo2) => calcularTiempoEspera(vuelo1, vuelo2)
      }.toList)
      par_Vuelo_Espera._1.sum + par_Vuelo_Espera._2.sum
    }
    // Función para obtener todos los itinerarios posibles de cod1 a cod2
    (cod1: String, cod2: String) => {
      val obtenerItinerarios = itinerariosPar(vuelos, aeropuertos)
      val todosLosItinerarios = obtenerItinerarios(cod1, cod2).par.toList
      val itinerariosConTiempo = for {
        itinerario <- todosLosItinerarios.par.toList
        tiempoTotal = calcularTiempoTotalItinerario(itinerario)
      } yield (itinerario, tiempoTotal)

      val itinerariosOrdenados = itinerariosConTiempo.sortBy(_._2)
      val mejoresItinerarios = itinerariosOrdenados.take(3).map(_._1)
      mejoresItinerarios.sortBy(itinerario => calcularTiempoTotalItinerario(itinerario))
    }
  }

  def itinerariosEscalasPar(vuelos:List[Vuelo], aeropuertos:List[Aeropuerto]):(String, String)=>List[Itinerario] = {
    def calcularEscalasPar(vuelos:List[Itinerario]):List[(Itinerario, Int)] = {
      val sumaEscalas = for{
        i <- vuelos
        sumas = sumarEscalas(i)
      } yield (i, sumas)
      sumaEscalas
    }

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
      val itinerario = itinerariosPar(vuelos, aeropuertos)
      val itinerarios = itinerario(code1, code2)
      val mitad = itinerarios.length/2
      val (primeraMitad, segundaMitad) = itinerarios.splitAt(mitad)
      //Se aplica paralelización de la lista luego de dividirla a la mitad
      val sumasEscalasPar = parallel(calcularEscalasPar(primeraMitad),
        calcularEscalasPar(segundaMitad)
      )
      //Se concatenan las dos listas y se ordenan por la cantidad de escalas
      (sumasEscalasPar._1 ++ sumasEscalasPar._2).sortBy(_._2).take(3).par.map(_._1).toList
    }
  }

  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val aeropuertoMap: Map[String, Aeropuerto] = aeropuertos.map(a => a.Cod -> a).toMap

    def calculoAirePar(vuelo: List[Itinerario]): List[(Itinerario, Int)] = {
      for{
        i <- vuelo
        sumas = sumarHoras(i)
      } yield(i, sumas)
    }

    @tailrec
    def sumarHoras(itinerario: Itinerario, acc:Int=0): Int = {
      itinerario match {
        case Nil => acc
        case h::t =>
          val diferenciaHoraria = (((aeropuertoMap(h.Dst).GMT - aeropuertoMap(h.Org).GMT)/100) * 60).toInt
          val salida = h.HS * 60 + h.MS
          val llegada = (h.HL * 60 + h.ML) - diferenciaHoraria
          val tiempoVuelo = if (llegada >= salida) {
            llegada - salida
          } else {
            (1440 + llegada) - salida
          }
          sumarHoras(t, acc + tiempoVuelo)
      }
    }

    (code1: String, code2:String) => {
      val itinerario = itinerariosPar(vuelos, aeropuertos)
      val itinerarios = itinerario(code1, code2)
      val mitad = itinerarios.length/2
      val (primeraMitad, segundaMitad) = itinerarios.splitAt(mitad)
      val horasVuelosPar = parallel(calculoAirePar(primeraMitad),
        calculoAirePar(segundaMitad)
      )
      //Se agrega paralelizacion de datos para mejorar el rendimiento
      (horasVuelosPar._1 ++ horasVuelosPar._2).sortBy(_._2).take(3).par.map(_._1).toList
    }
  }

  def itinerariosSalidaPar(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {

    //Función que permite la paralelización de los datos
    def calcularLlegadaPar(vuelos:List[Itinerario], code3:Int, code4:Int): List[(Itinerario, Int)] = {
      val mejoresSalidas = for{
        i <- vuelos
        horaPreferida = code3*60 + code4
        horarioLlegada = i.last.HL*60 + i.last.ML
        if horarioLlegada < horaPreferida
      } yield (i, i.head.HS*60 + i.head.MS)
      mejoresSalidas
    }
    (code1: String, code2:String, code3:Int, code4:Int) => {
      val itinerario = itinerariosPar(vuelos, aeropuertos)
      val itinerarios = itinerario(code1, code2)
      val mitad = itinerarios.length/2
      val (primeraMitad, segundaMitad) = itinerarios.splitAt(mitad)
      val mejoresSalidas = parallel(calcularLlegadaPar(primeraMitad, code3, code4),
        calcularLlegadaPar(segundaMitad, code3, code4)
      )
      (mejoresSalidas._1 ++ mejoresSalidas._2).sortBy(_._2).take(1).par.map(_._1).toList
    }
  }
}