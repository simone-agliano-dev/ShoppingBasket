import utilities.caseClasses.{Condition, ConditionalDiscount, Discount, GoodsBasket}
import utilities.discountFunctions.CalculateDiscounts.calculateOnSaleGoods
import utilities.logging.Logging.{outputNoOffers, outputTotalBasketCost}

import java.io.{BufferedInputStream, FileInputStream}
import scala.io.{BufferedSource, Source}


class ShoppingBasket() {

  def readCSVFromPrices(path: String) : Map[String,Double] = {
    val csvFile = Source.fromFile(path)
      csvFile
      .getLines()
      .foldLeft(Map.empty[String,Double]){
        (acc,line) =>
          val  col  = line.split(",").map(_.trim)
          acc + (col(0) -> col(1).toDouble)
      }
  }
  def readCSVForDiscount(path: String, itemsCounter :Map[String, Int] ) : List[Discount] = {
    val csvFile = Source.fromFile(path)

    csvFile
      .getLines()
      .foldLeft(List.empty[Discount]){
        (acc,line) =>
          val  col  = line.split(",").map(_.trim)
          acc :+ Discount(col(0), col(1).toDouble, itemsCounter.get(col(0)).getOrElse(0))
      }
  }
  def readCSVToConditionalDiscount(path: String): List[ConditionalDiscount] = {
    val csvFile =
      Source
        .fromFile(path)

    csvFile.getLines
      .foldLeft(List.empty[ConditionalDiscount]){
        (acc,line) =>
          val  col  = line.split(",").map(_.trim)
          acc :+ ConditionalDiscount(col(0), col(1).toDouble, Condition(col(2).split("-").map(_.trim).toList))
      }
  }


  def runApp(inputArgs : Array[String]) : Unit = {
    val pricesMap = readCSVFromPrices("src/main/resources/prices.csv")
    val shoppingBasket = GoodsBasket.apply(inputArgs.toList, pricesMap)
    val conditionalDiscounts = readCSVToConditionalDiscount(
      "src/main/resources/conditional_discounts.csv"
    )
    val discounts = readCSVForDiscount("src/main/resources/discounts.csv", shoppingBasket.countQuantityOfEach)
    val basketCalculated = calculateOnSaleGoods(
      shoppingBasket,
      pricesMap,
      conditionalDiscounts,
      discounts
    )
    outputNoOffers(basketCalculated)
    outputTotalBasketCost(basketCalculated, total = true)
  }

}

object ShoppingBasket {

  def main(args : Array[String]) : Unit = {

    if (args.length == 0) {
      println("Please specify at least one item")
      sys.exit(1)
    }
    new ShoppingBasket().runApp(args)

  }

}
