import scala.io._
import breeze.linalg._
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

  def main(args: Array[String]): Unit = {
    val stocks = Seq("sap", "ibm")
    val stockData: Seq[Seq[(Double, Double)]] = stocks.map(getDatePriceData(_))
    val minLength = stockData.map((data) => data.length).min
    val trimmedData = stockData.map((data) => data.take(minLength))
    val rightData: StockData = stocks.zip(trimmedData.map(_.reverse)).toMap
    plotStockPrices(rightData)

  }

  // should be string
  final case class State( stockData: StockData,
                          holdings: Holdings,
                          cash: Double,
                          date: Int)

  object RuleInterpreter {
    def eval(state: State, rules: Map[String,Rule]): State = {
//      state.holdings.data.map((sec, amount) => {})
      def datePrice(security: String, date: Int) = state.stockData.get(security).map((seq) => seq(date)._2)
      for {
        (security, num) <- state.holdings
        todayPrice = datePrice(security, state.date)
        priceDelta = todayPrice - datePrice(security, state.date - 1)
        numBought = if (rules(security).buyThreshold > priceDelta) rules(security).buyCountDailyMax.min(state.cash / todayPrice) else 0
        updatedState = state.copy(cash = (state.cash - numBought * todayPrice), holdings = state.holdings.updated(security, numBought + num))
        numSold = if (rules(security).sellThreshold < priceDelta) rules(security).sellCountDailyMax.min(num) else 0
        updatedState = state.copy(cash = (state.cash + numSold * todayPrice), holdings = state.holdings.updated(security, num - numSold))
      }


    }
  }
  final case class Rule(security:String, sellThreshold:Double, buyThreshold:Double, buyCountDailyMax:Int, sellCountDailyMax:Int)

}
