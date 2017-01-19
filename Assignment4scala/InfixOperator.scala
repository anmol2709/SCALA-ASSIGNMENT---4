import java.util.Stack
class Expression(val expr:String) {
  def evaluateExp(op:Stack[Int],operator:Stack[Char]):Int={
    var fValue=1
    val exprArray = expr.toCharArray
    //println(exprArray)

    for(c <- 0 to exprArray.length-1) {

      if (exprArray(c) >= '0' && exprArray(c) <= '9') {
        val new1=exprArray(c).toInt-48
        //println(new1)
        op.push(new1)
      }
      else if (exprArray(c) == '+' || exprArray(c) == '-' || exprArray(c) == '*' || exprArray(c) == '/' || exprArray(c) == '%') {

        if (checkPrecendence(exprArray(c), operator) == true) {
          //println("exprArray(c)=> "+exprArray(c))
          operator.push(exprArray(c))
        }
        else {
          val opOnStack = operator.pop()
          //println("opOnStack: "+opOnStack)
          val op1: Int = op.pop()
          //println("op1: "+op1)
          val op2: Int = op.pop()
          //println("op2: "+op2)
          fValue = Operate(op2, op1, opOnStack)
          //println("fValue: "+fValue)
          operator.push(exprArray(c))
          //println("exprArray(c)=> "+exprArray(c))
          op.push(fValue)

        }
      }

    }
    while((!op.empty())&&(!operator.empty()))
    {
      val opOnStack = operator.pop()
      //println("opOnStack: "+opOnStack)
      val op1: Int = op.pop()
      //println("op1: "+op1)
      val op2: Int = op.pop()
      //println("op2: "+op2)
      fValue = Operate(op2, op1, opOnStack)
      //println("fValue: "+fValue)

      op.push(fValue)
    }


    fValue
  }
  def Operate(op1:Int , op2:Int, opOnStack : Char): Int =
    opOnStack match{
      case '+' => op1 + op2
      case '-' => op1 - op2
      case '*' => op1 * op2
      case '/' => op1 / op2
      case '%' => op1 % op2
      case _ => 0
    }


  def checkPrecendence(operator2:Char,operator:Stack[Char]):Boolean =
  {
    var f = false
    if(operator.empty()==true)
    {
      f = true
    }
    else if(operator.peek() == '+' || operator.peek() == '-')
    {
      if(operator2=='*' || operator2=='/'||operator2=='%')
      {
        f = true
      }
      else
      {
        f = false
      }
    }
    else if(operator.peek() == '*' || operator.peek() =='/' || operator.peek()=='%')
    {
      f =false
    }
    f

  }
}
object InfixOperator {
  def main(args: Array[String]) {
    val s1 = new Stack[Int]()
    val s2 = new Stack[Char]()

    val obj1=new Expression("3*6/2-1+2")
    println( "The Final Answer is: "+obj1.evaluateExp(s1,s2))
  }
}