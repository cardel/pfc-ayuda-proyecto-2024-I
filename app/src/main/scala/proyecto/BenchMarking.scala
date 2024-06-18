package proyecto

import org.scalameter.{Warmer, withWarmer}


class BenchMarking () {
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]
  val itinerarioSeq = new Itinerario
  val itinerarioPar = new ItinerariosPar

  def itinerarioBench (vuelo: List[Vuelo], aeropuerto: List[Aeropuerto], numRepe:Int): (String, String) => Any = {

    val itsSeq = itinerarioSeq.itinerarios(vuelo, aeropuerto)
    val itsPar = itinerarioPar.itinerariosPar(vuelo, aeropuerto)

    (salida:String, llegada:String) => {
      println("---ItinerariosSeq vs ItinerariosPar--")
      // Calentar el sistema con ejecuciones adicionales
      for (_ <- 1 to 5) {
        itsSeq(salida, llegada)
        itsPar(salida, llegada)
      }

      // Medir tiempos
      val timeFinalSeq = new Array[Double](numRepe)
      val timeFinalPar = new Array[Double](numRepe)

      for (i <- 0 until numRepe) {
        // Medir tiempo para itsSeq
        val timeSeq = withWarmer(new Warmer.Default) measure {
          itsSeq(salida, llegada)
        }
        timeFinalSeq(i) = timeSeq.value

        // Medir tiempo para itsPar
        val timePar = withWarmer(new Warmer.Default) measure {
          itsPar(salida, llegada)
        }
        timeFinalPar(i) = timePar.value
      }
      val promedioSeq = timeFinalSeq.sum / numRepe
      val promedioPar = timeFinalPar.sum / numRepe
      println(s"ItinerariosSeq promedio: $promedioSeq ms")
      println(s"ItinerariosPar promedio: $promedioPar ms" + "\n")
    }
  }

  def itinerariosTiempoBench (vuelo: List[Vuelo], aeropuerto: List[Aeropuerto], numRepe:Int): (String, String) => Any = {
    val itsSeq = itinerarioSeq.itinerariosTiempo(vuelo, aeropuerto)
    val itsPar = itinerarioPar.itinerariosTiempo(vuelo, aeropuerto)

    (salida:String, llegada:String) => {
      println("---ItinerariosTiempoSeq vs ItinerariosTiempoSeqPar--")
      // Calentar el sistema con ejecuciones adicionales
      for (_ <- 1 to 5) {
        itsSeq(salida, llegada)
        itsPar(salida, llegada)
      }

      // Medir tiempos
      val timeFinalSeq = new Array[Double](numRepe)
      val timeFinalPar = new Array[Double](numRepe)

      for (i <- 0 until numRepe) {
        // Medir tiempo para itsSeq
        val timeSeq = withWarmer(new Warmer.Default) measure {
          itsSeq(salida, llegada)
        }
        timeFinalSeq(i) = timeSeq.value

        // Medir tiempo para itsPar
        val timePar = withWarmer(new Warmer.Default) measure {
          itsPar(salida, llegada)
        }
        timeFinalPar(i) = timePar.value
      }
      val promedioSeq = timeFinalSeq.sum / numRepe
      val promedioPar = timeFinalPar.sum / numRepe
      println(s"ItinerariosSeq promedio: $promedioSeq ms")
      println(s"ItinerariosPar promedio: $promedioPar ms" + "\n")
    }
  }

  def itinerariosEscalasBench (vuelo: List[Vuelo], aeropuerto: List[Aeropuerto], numRepe:Int): (String, String) => Any = {
    val itsSeq = itinerarioSeq.itinerariosEscalas(vuelo, aeropuerto)
    val itsPar = itinerarioPar.itinerariosEscalasPar(vuelo, aeropuerto)

    (salida:String, llegada:String) => {
      println("---ItinerariosEscalasSeq vs ItinerariosEscalasPar--")
      // Calentar el sistema con ejecuciones adicionales
      for (_ <- 1 to 5) {
        itsSeq(salida, llegada)
        itsPar(salida, llegada)
      }

      // Medir tiempos
      val timeFinalSeq = new Array[Double](numRepe)
      val timeFinalPar = new Array[Double](numRepe)

      for (i <- 0 until numRepe) {
        // Medir tiempo para itsSeq
        val timeSeq = withWarmer(new Warmer.Default) measure {
          itsSeq(salida, llegada)
        }
        timeFinalSeq(i) = timeSeq.value

        // Medir tiempo para itsPar
        val timePar = withWarmer(new Warmer.Default) measure {
          itsPar(salida, llegada)
        }
        timeFinalPar(i) = timePar.value
      }
      val promedioSeq = timeFinalSeq.sum / numRepe
      val promedioPar = timeFinalPar.sum / numRepe
      println(s"ItinerariosSeq promedio: $promedioSeq ms")
      println(s"ItinerariosPar promedio: $promedioPar ms" + "\n")
    }
  }

  def itinerariosAireBench (vuelo: List[Vuelo], aeropuerto: List[Aeropuerto], numRepe:Int): (String, String) => Any = {
    val itsSeq = itinerarioSeq.itinerariosAire(vuelo, aeropuerto)
    val itsPar = itinerarioPar.itinerariosAirePar(vuelo, aeropuerto)

    (salida:String, llegada:String) => {
      println("---ItinerariosAireSeq vs ItinerariosAirePar--")
      // Calentar el sistema con ejecuciones adicionales
      for (_ <- 1 to 5) {
        itsSeq(salida, llegada)
        itsPar(salida, llegada)
      }

      // Medir tiempos
      val timeFinalSeq = new Array[Double](numRepe)
      val timeFinalPar = new Array[Double](numRepe)

      for (i <- 0 until numRepe) {
        // Medir tiempo para itsSeq
        val timeSeq = withWarmer(new Warmer.Default) measure {
          itsSeq(salida, llegada)
        }
        timeFinalSeq(i) = timeSeq.value

        // Medir tiempo para itsPar
        val timePar = withWarmer(new Warmer.Default) measure {
          itsPar(salida, llegada)
        }
        timeFinalPar(i) = timePar.value
      }
      val promedioSeq = timeFinalSeq.sum / numRepe
      val promedioPar = timeFinalPar.sum / numRepe
      println(s"ItinerariosSeq promedio: $promedioSeq ms")
      println(s"ItinerariosPar promedio: $promedioPar ms" + "\n")
    }
  }

  def itinerariosSalidaBench(vuelo: List[Vuelo], aeropuerto: List[Aeropuerto], numRepe:Int): (String, String,Int,Int) => Any = {
    val itsSeq = itinerarioSeq.itinerariosSalida(vuelo, aeropuerto)
    val itsPar = itinerarioPar.itinerariosSalidaPar(vuelo, aeropuerto)

    (salida:String, llegada:String, horaL:Int , minL:Int) => {
      println("---ItinerariosSalidaSeq vs ItinerariosSalidaPar--")
      // Calentar el sistema con ejecuciones adicionales
      for (_ <- 1 to 5) {
        itsSeq(salida, llegada, horaL, minL)
        itsPar(salida, llegada, horaL, minL)
      }

      // Medir tiempos
      val timeFinalSeq = new Array[Double](numRepe)
      val timeFinalPar = new Array[Double](numRepe)

      for (i <- 0 until numRepe) {
        // Medir tiempo para itsSeq
        val timeSeq = withWarmer(new Warmer.Default) measure {
          itsSeq(salida, llegada, horaL,minL)
        }
        timeFinalSeq(i) = timeSeq.value

        // Medir tiempo para itsPar
        val timePar = withWarmer(new Warmer.Default) measure {
          itsPar(salida, llegada, horaL, minL)
        }
        timeFinalPar(i) = timePar.value
      }
      val promedioSeq = timeFinalSeq.sum / numRepe
      val promedioPar = timeFinalPar.sum / numRepe
      println(s"ItinerariosSeq promedio: $promedioSeq ms")
      println(s"ItinerariosPar promedio: $promedioPar ms")
    }
  }
}
