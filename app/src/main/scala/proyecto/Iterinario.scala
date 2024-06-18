package proyecto

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def findItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      def search(current: String, destination: String, visited: Set[String]): List[List[Vuelo]] = {
        if (current == destination) List(List())
        else {
          for {
            vuelo <- vuelos if vuelo.Org == current && !visited.contains(vuelo.Dst)
            itinerario <- search(vuelo.Dst, destination, visited + current)
          } yield vuelo :: itinerario
        }
      }
      search(cod1, cod2, Set())
    }
    findItinerarios
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def minimoTiempo(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itsAll = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      itsAll.sortBy(it => calcularTiempoTotal(it, aeropuertos)).take(3)
    }
    minimoTiempo
  }

  def calcularDuracionVuelo(vuelo: Vuelo, aeropuertos: List[Aeropuerto]): Int = {
    val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
    val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

    val salidaEnMinutos = (vuelo.HS * 60) + vuelo.MS
    val llegadaEnMinutos = (vuelo.HL * 60) + vuelo.ML

    val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) / 100
    val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt

    val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)

    if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
  }

  def calcularTiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo): Int = {
    val llegadaEnMinutos = vuelo1.HL * 60 + vuelo1.ML
    val salidaEnMinutos = vuelo2.HS * 60 + vuelo2.MS

    val esperaEnMinutos = salidaEnMinutos - llegadaEnMinutos

    if (esperaEnMinutos < 0) esperaEnMinutos + 1440 else esperaEnMinutos
  }

  def calcularTiempoTotal(itinerario: List[Vuelo], aeropuertos: List[Aeropuerto]): Int = {
    val tiemposDeVuelo = itinerario.map(vuelo => calcularDuracionVuelo(vuelo, aeropuertos))
    val tiemposDeEspera = itinerario.zip(itinerario.tail).map { case (v1, v2) => calcularTiempoEspera(v1, v2) }
    tiemposDeVuelo.sum + tiemposDeEspera.sum
  }


  def itinerariosEscalas(vuelos:List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def minimoEscalas(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itsAll = itinerarios(vuelos, aeropuertos)(cod1, cod2)

      def calcularEscalas(itinerario: List[Vuelo]): Int = {
        val escExp = itinerario.count(v => v.Dst != cod2)
        val escTec = itinerario.map(v => v.Esc)
        escExp + escTec.sum
      }

      val itsAllEscalas = itsAll.map(it => (it, calcularEscalas(it)))
      itsAllEscalas.sortBy(_._2).map(_._1).take(3)
    }

    minimoEscalas
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def calcularDuracionVuelo(vuelo: Vuelo): Int = {
      val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
      val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

      val salidaEnMinutos = (vuelo.HS * 60) + vuelo.MS
      val llegadaEnMinutos = (vuelo.HL * 60) + vuelo.ML

      val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT)/100
      val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt

      val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)

      if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
    }

    def calcularTiempoTotal(itinerario: List[Vuelo]): Int = {
      itinerario match {
        case Nil => 0
        case _ :: Nil => 0
        case vuelo1 :: vuelo2 :: tail => calcularDuracionVuelo(vuelo1) + calcularTiempoTotal(vuelo2 :: tail)
      }
    }

    def minimoAire(cod1: String, cod2: String): List[List[Vuelo]] = {
      val istAll = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val itsAllTime = istAll.map(it => (it, calcularTiempoTotal(it)))
      itsAllTime.sortBy(_._2).map(_._1).take(3)
    }

    minimoAire
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[List[Vuelo]] = {
    def convertirAMinutos(hora: Int, minutos: Int): Int = {
      hora * 60 + minutos
    }
    def buscarItinerarios(origen: String, destino: String, visitados: Set[String], caminoActual: List[Vuelo], horaCita: Int): List[List[Vuelo]] = {
      if (origen == destino) {
        if (convertirAMinutos(caminoActual.last.HL, caminoActual.last.ML) <= horaCita) List(caminoActual)
        else List()
      } else {
        val vuelosDisponibles = vuelos.filter(v =>
          v.Org == origen &&
            !visitados.contains(v.Dst) &&
            convertirAMinutos(v.HS, v.MS) > convertirAMinutos(caminoActual.last.HL, caminoActual.last.ML) &&
            convertirAMinutos(v.HL, v.ML) <= horaCita
        )
        vuelosDisponibles.flatMap { vuelo =>
          val nuevosVisitados = visitados + origen
          buscarItinerarios(vuelo.Dst, destino, nuevosVisitados, caminoActual :+ vuelo, horaCita)
        }
      }
    }

    (origen: String, destino: String, horaCita: Int, minCita: Int) => {
      val tiempoCita = convertirAMinutos(horaCita, minCita)
      val itinerariosEncontrados = vuelos.filter(_.Org == origen).flatMap { vuelo =>
        buscarItinerarios(vuelo.Dst, destino, Set.empty, List(vuelo), tiempoCita)
      }

      if (itinerariosEncontrados.isEmpty) List()
      else {
        val salidaMasTarde = itinerariosEncontrados.map(it => convertirAMinutos(it.last.HS, it.last.MS)).max
        itinerariosEncontrados.filter(it => convertirAMinutos(it.last.HS, it.last.MS) == salidaMasTarde)
      }
    }
  }

}
