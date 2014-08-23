import scala.math
import scalaz.{\/-, -\/, \/}

/**
 * Represents an object that could be represented in n-dimensional space
 * @param coordinates the coordinates
 */
class Point(val coordinates: Set[Int]) {

  // Syntactic sugar so we don't have to say Point(Set(1,2,...)) and can just go Point(1,2,..)
  def this(test: Int*) = this(test.toSet)

  val dimensions = coordinates.size

  /**
   * Computes the distance between two points using Pythagoras' theorem
   * @param neighbour The point to find the distance to
   * @return The distance from this point to the given point
   */
  def computeDistance(neighbour: Point): Double \/ InvalidDimensionException = neighbour.dimensions match {
    case `dimensions` => -\/({
      val delta: Set[Double] = for {(a,b) <- coordinates zip neighbour.coordinates} yield math.pow(a-b, dimensions)
      math.sqrt(delta.sum)
    })
    case _ => \/-(new InvalidDimensionException(s"Points must have the same number of dimensions - $this, $neighbour"))
  }

  override def toString: String = coordinates.toString()
}

class InvalidDimensionException(message: String) extends Exception(message)
