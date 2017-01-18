case class Tiffin(method:String,payment:Double)
{
  def trial():Double=
  {
    method match
    {
      case "Paytm" => useWallets()
      case "FreeCharge" => useWallets()
      case "NetBanking" => useNetBanking()
      case "CardPayment" => useCardPayment()
      case "CashPayment" => payment
      case _ => 0
    }
  }
  def useWallets():Double=
  {
    payment+(payment*0.02)
  }
  def useNetBanking():Double=
  {
    payment+5
  }
  def useCardPayment():Double=
  {
    payment+1.5
  }

}

object TiffinClassProvider {
  def main(args: Array[String]) {
    val t = Tiffin("Paytm",100)
    val t1 = Tiffin("NetBanking",100)
    val t2 = Tiffin("CardPayment",100)
    val t3 = Tiffin("CashPayment",100)
    println(s"Paytm: The final amount is ${t.trial()}")
    println(s"NetBanking: The final amount is ${t1.trial()}")
    println(s"CardPayment: The final amount is ${t2.trial()}")
    println(s"CashPayment: The final amount is ${t3.trial()}")
  }

}
