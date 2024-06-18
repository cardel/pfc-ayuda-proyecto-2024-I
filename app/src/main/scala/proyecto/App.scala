/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package proyecto
import datos._

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object App {
  def main(args: Array[String]): Unit = {
    val bench = new BenchMarking()
    val salida = "MIA"
    val llegada = "ORD"
    val horaSal = 12
    val minSal = 30
    bench.itinerarioBench(vuelosC1, aeropuertos, 7)(salida, llegada)
    bench.itinerariosTiempoBench(vuelosC1, aeropuertos, 7)(salida, llegada)
    bench.itinerariosEscalasBench(vuelosC1, aeropuertos, 7)(salida, llegada)
    bench.itinerariosAireBench(vuelosC1, aeropuertos, 7)(salida, llegada)
    bench.itinerariosSalidaBench(vuelosC1, aeropuertos, 7)(salida, llegada,horaSal,minSal)
  }
}


