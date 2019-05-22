package lab3

object Boot extends App{
  //Task 1
  def divide(operand1: Double, operand2: Double): Double ={
    if (operand2 == 0) 0
    else operand1/operand2
  }

  //Task 2
  def calculator(operand1: String, operator: String, operand2:String): Double ={
    val number2 = readInt(operand1).getOrElse(0)
    val number1 = readInt(operand2).getOrElse(0)
    operator match {
      case "+" => number2 + number1
      case "-" => number2 - number1
      case "*" => number2 * number1
      case "/" => divide(number2, number1)
      case _ => 0
    }
  }

  def readInt(str: String): Option[Int] =
    if(str matches "\\d+") Some(str.toInt) else None

 def sumOfOptionsUsingFor(operand1: Option[Int], operator :Option[String], operand2: Option[Int]): Option[Double] = {
   val opnd1 = operand1.getOrElse(0)
   val opnd2 = operand2.getOrElse(0)
   val oper = operator.getOrElse("This is operator")
   oper match {
     case "+"=> Some(opnd1 + opnd2)
     case "-"=> Some(opnd1 - opnd2)
     case "*"=> Some(opnd1 * opnd2)
     case "/"=> divide(opnd1, opnd2)
     case _ => None
   }
 }
  println(calculator("5","/","10"))


}
