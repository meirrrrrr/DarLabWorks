package firstscala

import java.time.LocalDate

object Boot {
  def main(args: Array[String]):Unit={
    val oswald =new Cat("Oswald","Black","Milk")
    val henderson = new Cat("Henderson", "Ginger", "Chips")
    val quentin = new Cat("Quentin","Tabby and white", "Curry")
    val eastwood = new Director("Clint","Eastwood", 1930)
    val mcTiernan = new Director("John","McTiernan", 1951)
    val nolan = new Director("Christopher","Nolan", 1970)
    val someBody = new Director("Just","SomeBody", 1990)
    val memento = new Film("Memento", 2000, 8.5, nolan)
    val darkKnight = new Film("DarkKnight", 2008, 9.0, nolan)
    val inception = new Film("Inception", 2010, 8.8, nolan)
    val highPlainsDrifter  = new Film("HighPlainsDrifter", 1973, 7.7, eastwood)
    val outlawJoseyWales   = new Film("TheOutlawJoseyWales", 1976, 7.9, eastwood)
    val unforgiven = new Film("Unforgiven", 1992, 8.3, eastwood)
    val granTorino = new Film("GranTorino", 2008, 8.2, eastwood)
    val invictus = new Film("Invictus", 2009, 7.4, eastwood)
    val predator = new Film("Predator", 1987, 7.9, mcTiernan)
    val dieHard = new Film("DieHard", 1988, 8.3, mcTiernan)
    val huntForRedOctober = new Film("TheHuntforRedOctober", 1990, 7.6, mcTiernan)
    val thomasCrownAffair = new Film("TheThomasCrownAffair", 1999, 6.8, mcTiernan)

    println(eastwood.yearOfBirth) //should be 1930
    println(dieHard.director.name) //should be "JohnMcTiernan"
    println(invictus.isDirectedBy(nolan)) // should be false
    println(highPlainsDrifter.copy(n ="L'hommedeshautesplaines")) // returns Film("L'hommedeshautesplaines",1973,7.7,/*etc*/)
    println(thomasCrownAffair.copy(y = 1968,dir = new Director("Norman","Jewison", 1926))) // returnsFilm("TheThomasCrownAffair",1926,/*etc*/)
    println(inception.copy().copy().copy()) // returns a new copy of`inception`
    println(new Counter(10).inc.dec.inc.inc.count)
    ChipShop.willServe(henderson)
    ChipShop.willServe(oswald)
  }
}
case class Cat(name:String , colour: String, food: String) {

}

case class Director(firstName:String, lastName:String, yearofbirth:Int){
  def name = firstName+lastName
  val yearOfBirth = yearofbirth
}

case class Film(name:String, yearOfRelease:Int, imbdRating:Double, dddirector: Director){
  val director = dddirector
  def directorsAge =LocalDate.now.getYear-director.yearOfBirth
  def isDirectedBy(ddirector: Director) = ddirector.equals(director)
  def copy(n: String=name,y:Int=yearOfRelease,imbd:Double=imbdRating,dir:Director=dddirector) = new Film(n,y,imbd,dir)
}
case class Counter(int:Int=0){
  def inc = new Counter(int+1)
  def dec = new Counter(int-1)
  def count = int
}
object ChipShop{
  def willServe(cat:Cat) = cat match {
    case Cat(_,_,food) => println(food.equals("Chips"))
  }

}

object Dad{
  def rate(film:Film) = film.dddirector.name match {
    case "JohnMcTiernan" => film.imbdRating(7.0)
    case "ClintEastwood" => film.imbdRating(10.0)
    case _ => film.imbdRating(3.0)

  }
}