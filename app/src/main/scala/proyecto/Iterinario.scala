package proyecto

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]
/*
  def itinerarios(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los itinerarios posibles de cod1 a cod2
    (cod1:String, cod2:String)=> List[Itinerario]()
  }
*/
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
    def calcularDuracion(vuelo: Vuelo): Int = {
      val duracionHoras = vuelo.HL - vuelo.HS
      val duracionMinutos = vuelo.ML - vuelo.MS
      duracionHoras * 60 + duracionMinutos
    }

    def buscarItinerarios(origen: String, destino: String, visitados: Set[String]): List[List[Vuelo]] = {
      if (origen == destino) {
        List(List())
      } else {
        vuelos.filter(v => v.Org == origen && !visitados.contains(v.Dst)).flatMap { vuelo =>
          val itinerariosRestantes = buscarItinerarios(vuelo.Dst, destino, visitados + origen)
          itinerariosRestantes.map(it => vuelo :: it)
        }
      }
    }

    (cod1: String, cod2: String) => {
      val itinerarios = buscarItinerarios(cod1, cod2, Set.empty).map { it =>
        val tiempoTotal = it.map(calcularDuracion).sum
        (it, tiempoTotal)
      }.sortBy(_._2).map(_._1)

      itinerarios.take(3)
    }
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

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos y dos enteros, que es la hora de la cita
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que permiten llegar a una hora de la cita
    (cod1:String, cod2:String, HC:Int, MC:Int)=> List[Itinerario]()
  }

}
