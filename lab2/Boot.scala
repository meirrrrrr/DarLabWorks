package lab2
import scala.math
object Boot {
  def main(args: Array[String]): Unit = {
    case class Film( name: String,
                     yearOfRelease: Int,
                     imdbRating: Double)
    case class Director( firstName: String,
                         lastName: String,
                         yearOfBirth: Int,
                         films: Seq[Film])

    val memento = Film("Memento", 2000, 8.5)
    val darkKnight = Film("Dark Knight", 2008, 9.0)
    val inception = Film("Inception", 2010, 8.8)
    val highPlainsDrifter = Film("High Plains Drifter", 1973, 7.7)
    val outlawJoseyWales = Film("The Outlaw Josey Wales", 1976, 7.9)
    val unforgiven = Film("Unforgiven", 1992, 8.3)
    val granTorino = Film("Gran Torino", 2008, 8.2)
    val invictus = Film("Invictus", 2009, 7.4)
    val predator = Film("Predator", 1987, 7.9)
    val dieHard = Film("Die Hard", 1988, 8.3)
    val huntForRedOctober = Film("The Hunt for Red October", 1990, 7.6)
    val thomasCrownAffair = Film("The Thomas Crown Affair", 1999, 6.8)
    val eastwood = Director("Clint", "Eastwood", 1930,
      Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))
    val mcTiernan = Director("John", "McTiernan", 1951,
      Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))
    val nolan = Director("Christopher", "Nolan", 1970,
      Seq(memento, darkKnight, inception))
    val someGuy = Director("Just", "Some Guy", 1990,
      Seq())
    val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

    //Task 1
    def beforeDir(year: Int): Director => Boolean = (x: Director) => {
      x.yearOfBirth<year
    }

    println("was born before 1970: ")
    directors.filter(beforeDir(1970)).foreach(dir=>println(dir.firstName))

    //Task 2
    def moreThan(numberOfFilms: Int): Director => Boolean = (x: Director) => {
      x.films.length>numberOfFilms
    }
    println("More Than 4 Films: ")
    directors.filter(moreThan(4)).foreach(dir=>println(dir.firstName))

    //Task 3
    def superDir(year: Int, numberOfFilms: Int): Director => Boolean = (x: Director) => {
      x.films.length>numberOfFilms && x.films.length>numberOfFilms
    }

    println("Super Directors who was born before 1950 and directed more than 3 films: ")
    directors.filter(superDir(1950, 3)).foreach(dir=>println(dir.firstName))

    //Task 4

    def sortedDirectors(ascending: Boolean = true) : Seq[Director] ={
      ascending match {
        case true => directors.sortWith(_.yearOfBirth < _.yearOfBirth)
        case false=> directors.sortWith(_.yearOfBirth > _.yearOfBirth)
      }
    }

    //Task 5
    def nolanFilms(): Seq[String] ={
      val films = for {
        i <- directors
        if i.firstName == "Christopher" && i.lastName == "Nolan"
          film <- i.films
      } yield film.name
      films
    }

    //Task 6
    def allFilms(): Seq[String] ={
      val films = for {
        i <- directors
        film <- i.films
      } yield film.name
      films
    }

    //Task 7
    def earlyMc(): Int ={
      val films = mcTiernan.films
      val year = for {
        i <- films
      } yield i.yearOfRelease
      year.sorted
      year.head
    }

    //Task 8
    def sortByIMDB(): Seq[Film] ={
      val films = for {
        i <- directors
        film <- i.films
      } yield film

      films.sortWith(_.imdbRating < _.imdbRating)
    }

    //Task 9
    def avgScore(): Double ={
      var score = 0.0
      var count = 0
      directors.foreach(
        dir=>dir.films.foreach(
          film=> {
            score+=film.imdbRating
            count+=1}
        )
      )
      score/count
    }

    //Task 10
    def tonights(): Unit = {
      directors.foreach(
        dir=>dir.films.foreach(
          film=> {
            println(s"Tonight only! ${film.name} by ${dir.firstName}!")
        }
      ))
    }

    //Task 11
    def byArchives(): Film ={
      val films = for {
        i <- directors
        film <- i.films
      } yield film

      films.sortWith(_.yearOfRelease < _.yearOfRelease)
      films.head
    }



  }
}
