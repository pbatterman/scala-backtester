import scala.io._
import breeze.linalg.DenseVector
import breeze.numerics._
import breeze.plot._

object Backtester {
  def getDatePriceData(stock: String): Seq[(Double,Double)] = {
    val stock_close = Source.fromFile(s"ScalaDs/${stock}_stocks.csv")
      .getLines.drop(1).map(_.split(",")(4).toDouble).toSeq
    val stock_days = (0 until stock_close.length).map( _.toDouble)
    stock_days.zip(stock_close)
  }
//  def getDatePriceData2(stock: String): (DenseVector[Double], DenseVector[Double]) = {
//    val stock_close = DenseVector( Source.fromFile(s"ScalaDs/${stock}_stocks.csv")
//      .getLines.drop(1).map(_.split(",")(4).toDouble).toSeq :_ * )
//    val stock_days = DenseVector.range(0,stock_close.length, 1).map( _.toDouble)
//    (stock_days, stock_close)
//  }

  type StockData = Map[String, Seq[(Double, Double)]]
  type Holdings = Map[String, Int]

  def plotStockPrices(rightData: StockData): Unit = {
    val day = rightData("sap")(1)._1 - rightData("ibm")(0)._1 // dynamically figure out whata  day is


    val fig = Figure()
    val plt = fig.subplot(0)

    for {
      (stock, daycloses) <- rightData
      (stockdays, stockcloses) = daycloses.unzip

    } plt += plot(DenseVector(stockdays :_*), DenseVector(stockcloses :_*))

    fig.refresh()
  }

  def getMoney(state: State, date: Int): Double = {
    val stockData = state.stockData
    state.holdings.map((secnum) => {
      val (sec, num): (String, Int) = secnum
      val price: Double = stockData.get(sec).map((timepricedata) => timepricedata(date)._2).getOrElse(-1) // should never be -1
      num * price
    }).sum + state.cash
  }

  def main(args: Array[String]): Unit = {
    val stocks = Seq("sap", "ibm")
    val rawData: Seq[Seq[(Double, Double)]] = stocks.map(getDatePriceData(_))
    val minLength = rawData.map((data) => data.length).min
    val trimmedData = rawData.map((data) => data.take(minLength))
    val stockData: StockData = stocks.zip(trimmedData.map(_.reverse)).toMap

//    Rule(security:String, sellThreshold:Double, buyThreshold:Double, buyCountDailyMax:Int, sellCountDailyMax:Int)

    val startDate = 0
    val endDate = minLength
    val initCash = 1000
    val rules = Map("sap" -> Rule("sap", 0, 0, 1000000, 0))
    val init = State(stockData, Map[String, Int](), initCash, startDate)
    val myhistory: Seq[State] = RuleInterpreter.runRules(init, stockData, rules, startDate, endDate)

    println(myhistory.take(10).map(state => (state.cash, state.holdings)))

    val mymoney: Seq[Double] = myhistory.zip(startDate until endDate).map(statedate => getMoney(statedate._1, statedate._2))
    val days = (startDate until endDate).map (_.toDouble)
    val timemoneydata: Seq[(Double, Double)] = days.zip(mymoney)

    plotStockPrices(stockData.updated("mydata", timemoneydata))

  }

  // should be string
  final case class State( stockData: StockData,
                          holdings: Holdings,
                          cash: Double,
                          date: Int)

  object RuleInterpreter {
//    def eval(state: State, rules: Map[String,Rule]): State = {
//      //      state.holdings.data.map((sec, amount) => {})
//      def datePrice(security: String, date: Int) = state.stockData.get(security).map((seq) => seq(date)._2)
//
//      for {
//        (security, num) <- state.holdings
//        todayPrice = datePrice(security, state.date)
//        priceDelta = todayPrice - datePrice(security, state.date - 1)
//        numBought = if (rules(security).buyThreshold > priceDelta) rules(security).buyCountDailyMax.min(state.cash / todayPrice) else 0
//        updatedState = state.copy(cash = (state.cash - numBought * todayPrice), holdings = state.holdings.updated(security, numBought + num))
//        numSold = if (rules(security).sellThreshold < priceDelta) rules(security).sellCountDailyMax.min(num) else 0
//        updatedState = state.copy(cash = (state.cash + numSold * todayPrice), holdings = state.holdings.updated(security, num - numSold))
//      }
//    }

    def eval(initstate: State, rules: Map[String,Rule]): State = {
      //      state.holdings.data.map((sec, amount) => {})
      def datePrice(state: State, security: String, date: Int): Double = state.stockData.get(security).map((seq) => seq(date)._2).getOrElse(0)
      println("prefold")
      initstate.stockData.foldLeft(initstate)((state, secdata) => {
        val (security, _) = secdata
        val num: Int = state.holdings.getOrElse(security, 0)
        val todayPrice: Double = datePrice(state, security, state.date)
        val priceDelta: Double = todayPrice - datePrice(state, security, state.date - 1)
        val numBought = rules.get(security).map(r =>
          if (r.buyThreshold > priceDelta)
            r.buyCountDailyMax.min( (state.cash / todayPrice).floor.toInt )
          else 0).getOrElse(0)
        val updatedBought = state.copy(holdings = state.holdings.
          updated(security, num + numBought), cash = state.cash - numBought * todayPrice)
        println(numBought)
        val numSold = rules.get(security).map(r =>
          if (r.sellThreshold < priceDelta)
            r.sellCountDailyMax.min(num)
          else 0).getOrElse(0)
        val updatedBoughtSold = updatedBought.copy(holdings = state.holdings.
          updated(security, num - numSold),cash = state.cash + numSold * todayPrice)

        updatedBoughtSold
      })
//      for {
      // (security, num) <- state.holdings
      // todayPrice = datePrice(security, state.date)
      // priceDelta = todayPrice - datePrice(security, state.date - 1)
      // numBought = if (rules(security).buyThreshold > priceDelta) rules(security).buyCountDailyMax.min(state.cash / todayPrice) else 0
      // updatedState = state.copy(cash = (state.cash - numBought * todayPrice), holdings = state.holdings.updated(security, numBought + num))
//        numSold = if (rules(security).sellThreshold < priceDelta) rules(security).sellCountDailyMax.min(num) else 0 updatedState = state.copy(cash = (state.cash + numSold * todayPrice), holdings = state.holdings.updated(security, num - numSold))
//      }


      }
      def runRules(init:State, stockData: StockData, rules: Map[String,Rule], startDate: Int, endDate: Int): Seq[State] =  {
        if (startDate > endDate) throw new Exception("stateDate > endDate")
        (startDate until endDate).foldLeft[Seq[State]](IndexedSeq(init))((states: Seq[State], date: Int) => {
//          println(eval(states.last, rules).holdings)
          states :+ eval(states.last, rules)
        })
      }






  }
  final case class Rule(security:String, sellThreshold:Double, buyThreshold:Double, buyCountDailyMax:Int, sellCountDailyMax:Int)

}
