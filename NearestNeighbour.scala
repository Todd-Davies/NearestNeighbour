import scalaz.{-\/, \/-}

object NearestNeighbour extends App {

  /**
   * Find the nearest neighbour to a point in a set of points
   * @param input The point we're at
   * @param existing A list of points
   * @return The nearest point in the list to the input point
   */
  def computeNearestNeighbour[A <: Point](input: Point, existing: Set[A]): A = {
    implicit val nearestNeighbour = Ordering.by((p: A) => p.computeDistance(input) match {
      case \/-(exception) => throw exception
      case -\/(distance) => distance
    })
    existing.min
  }

  /**
   * A simple wrapper around Point to represent a person
   */
  class Person(val weight: Int, val height: Int, val name: String) extends Point(weight, height) {
    override def toString = s"$name ($weight, $height)"
  }

  val in: Person = new Person(60, 167, "Todd")

  val children = Set(new Person(40,140,"Jess"), new Person(34,123,"Mo"), new Person(38,146,"Dan"))
  val adults = Set(new Person(83,179,"Cynthia"), new Person(71, 168,"Terrence"), new Person(56, 160,"Sid"))

  // Filter not is union but without any duplicates
  val sample: Set[Person] = (children union adults) filterNot (children intersect adults)

  println(s"Input is: $in")
  val nearestNeighbour = computeNearestNeighbour(in, sample)
  println(s"Nearest neighbour is: $nearestNeighbour")
  val set = children contains nearestNeighbour match {
    case true  => "children"
    case false => "adults"
  }
  println(s"I think $in belongs to the group '$set'")

}