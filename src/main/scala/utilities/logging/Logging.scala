package utilities.logging

import utilities.caseClasses.{Discount, GoodsBasket}
import utilities.discountFunctions.CalculateDiscounts.getTotal

import scala.math.BigDecimal.RoundingMode

object Logging {

  def outputNoOffers(basket: GoodsBasket): Unit =
    if (getTotal(basket, discounted = false) == getTotal(
      basket,
      discounted = true
    )) println("(No offers available)")

  def outputDiscount(discount: Discount, oldPrice: Double): Unit = {
    val savings = oldPrice - (oldPrice * (1 - discount.discount))

    println(
      discount.item + " " + (discount.discount * 100) + "% off: " + (BigDecimal(
        savings
      ) * 100)
        .setScale(0, RoundingMode.HALF_EVEN) + "p"
    )
  }

  def outputTotalBasketCost(basket: GoodsBasket, total: Boolean): Unit =
    if (total) println(s"Total price: £${getTotal(basket, discounted = true)}")
    else println(s"Subtotal: £${getTotal(basket, discounted = false)}")
}
