package proyecto

import org.scalameter
import org.scalameter.{Quantity, Warmer, measure, withWarmer}
import scala.concurrent.Future

class Benchmarking {
  def itinerarios(itinerarioSeq: (String, String) => List[List[Vuelo]], itinerarioPar: (String, String) => Future[List[List[Vuelo]]]): (Quantity[Double], Quantity[Double]) = {
    val seq = withWarmer(new Warmer.Default) measure {
      itinerarioSeq("DFW", "ATL")
    }

    val par = withWarmer(new Warmer.Default) measure {
      itinerarioPar("DFW", "ATL")
    }

    (seq, par)
  }
}
