package proyecto

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los itinerarios posibles de cod1 a cod2
    def generarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      def buscar(actual: String, destino: String, visitados: Set[String] = Set()): List[List[Vuelo]] = {
        if (actual == destino) List(List())
        else {
          for {
            v <- vuelos if (v.Org == actual && !visitados.contains(v.Dst))
            i <- buscar(v.Dst, destino, visitados + actual)
          } yield v::i
        }
      }

      buscar(cod1, cod2)
    }

    generarItinerarios
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    //Recibe vuelos, una lista de vuelos y aeropuertos, una lista de aeropuertos y retorna una funcion que recibe dos strings y retorna una lista de itinerarios
    //Devuelve una función que recibe c1 y c2, códigos de aeropuertos
    //y devuelve una función que devuelve los tres (si los hay) itinerarios que minimizan el tiempo total de viaje
    (cod1:String, cod2:String)=> List[Itinerario]()
  }

  def itinerariosEscalas(vuelos:List[Vuelo], aeropuertos:List[Aeropuerto]):(String, String)=>List[Itinerario]
  = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el número de escalas
    (cod1:String, cod2:String)=> List[Itinerario]()
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el tiempo en itinerarios
    (cod1:String, cod2:String)=> List[Itinerario]()
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
