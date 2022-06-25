package utilities.discountFunctions

import utilities.caseClasses.{ConditionalDiscount, Discount, Good, GoodsBasket}
import utilities.discountFunctions.ApplyDiscounts.getBasketWithDiscountsApplied
import utilities.discountFunctions.ConditionalDiscounts.verifyApplicableDiscounts
import utilities.logging.Logging.{outputDiscount, outputTotalBasketCost}

import scala.math.BigDecimal.RoundingMode

object CalculateDiscounts {


  def getTotal(basket: GoodsBasket, discounted: Boolean): BigDecimal = {
    BigDecimal(
      basket.goods
        .map(good => good.getCost(discounted))
        .sum
    ).setScale(2, RoundingMode.HALF_EVEN)
  }

  def getMaxNumberOfDiscounts(discounts: List[Discount],
                              countOfItems: Map[String, Int]): List[Discount] =
    discounts
      .map(
        discount =>
          if (discount.numberOfTimesToApply > countOfItems(discount.item)) {
            discount.copy(numberOfTimesToApply = countOfItems(discount.item))
          } else {
            discount
          }
      )

  def generateCorrectNumberOfDiscounts(
                                        discounts: List[Discount],
                                        mapOfPrices: Map[String, Double],
                                        countOfItems: Map[String, Int]
                                      ): List[Discount] = {

    val allDiscounts = for {
      dis <- getMaxNumberOfDiscounts(discounts, countOfItems)
      _ <- 0 until dis.numberOfTimesToApply
    } yield {
      Discount(dis.item, dis.discount, dis.numberOfTimesToApply)
    }
    allDiscounts.foreach(dis => outputDiscount(dis, mapOfPrices(dis.item)))

    allDiscounts
  }

  def calculateOnSaleGoods(initialBasket: GoodsBasket,
                               pricesMap: Map[String, Double],
                               conditionalDiscounts: List[ConditionalDiscount],
                               discounts: List[Discount]): GoodsBasket = {

    outputTotalBasketCost(initialBasket, total = false)

    val allDiscounts = (discounts ::: verifyApplicableDiscounts(
      initialBasket,
      conditionalDiscounts
    )) filter (d => initialBasket.goods.map(g => g.itemName).contains(d.item))

    getBasketWithDiscountsApplied(
      initialBasket,
      generateCorrectNumberOfDiscounts(
        allDiscounts,
        pricesMap,
        initialBasket.goods.map(g => g.itemName).groupBy(identity).mapValues(_.size)
      )
    )
  }
}
