import java.util.Stack
class Expression(val expr:String) {
def evaluateExp(op:Stack[Int],operator:Stack[Char]):Int={
var fValue=1
  val exprArray = expr.toCharArray


   for(c <- 0 to exprArray.length-1) {

     if (exprArray(c) >= '0' && exprArray(c) <= '9') {
       op.push(exprArray(c))
     }
     else if (exprArray(c) == '+' || exprArray(c) == '-' || exprArray(c) == '*' || exprArray(c) == '/' || exprArray(c) == '%') {

       if (checkPrecendence(exprArray(c), operator) == true) {
         operator.push(exprArray(c))
       }
       else {
         val opOnStack = operator.pop()
         val op1: Int = op.pop().toInt - 48
         val op2: Int = op.pop().toInt - 48
         fValue = Operate(op2, op1, opOnStack)
         operator.push(exprArray(c))
         op.push(fValue)

       }
     }

   }
  while((!op.empty())&&(!operator.empty()))
    {
      val opOnStack = operator.pop()
      val op1: Int = op.pop().toInt-48
      val op2: Int = op.pop().toInt-48
      fValue = Operate(op2, op1, opOnStack)
      op.push(fValue)
    }


      fValue+48
    }
  def Operate(op1:Int , op2:Int, opOnStack : Char): Int =
  {
    opOnStack match{
      case '+' => op1 + op2
      case '-' => op1 - op2
      case '*' => op1 * op2
      case '/' => op1 / op2
      case '%' => op1 % op2
    case _ => 0
    }
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
    val s1 = new Stack[Int]
    val s2 = new Stack[Char]

    val obj1=new Expression("1+2*3")
    println( obj1.evaluateExp(s1,s2))
  }
}
