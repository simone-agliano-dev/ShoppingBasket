package utilities.discountFunctions

import utilities.caseClasses.{ConditionalDiscount, Discount, GoodsBasket}

object ConditionalDiscounts {

  private def filterByDiscount(
                              numberOfRequiredGoods: Map[String,Int],
                              numbersOfGoodsInBasket : Map[String,Int]
                              ) : Boolean = {
    numberOfRequiredGoods
      .filter(
        requiredGood =>
          requiredGood._2 <= numbersOfGoodsInBasket
            .filter(
              numberOfGood => numberOfRequiredGoods.contains(numberOfGood._1)
            )
            .getOrElse(requiredGood._1, 0)
      )
      .equals(numberOfRequiredGoods)
  }


  def verifyApplicableDiscounts(
                               basket: GoodsBasket,
                               discounts: List[ConditionalDiscount]
                               ) : List[Discount] = {
    discounts
      .filter { conditionalDiscount =>
      {
        val numberOfGoodRequired: Map[String, Int] =
          conditionalDiscount.condition.countValues
        filterByDiscount(numberOfGoodRequired, basket.countQuantityOfEach)
      }
      }
      .map(d => {
        val numberRequired = d.condition.goodsRequired.size
        val numberInBasket =
          basket.countQuantityOfEach(d.condition.goodsRequired.head)
        Discount(d.item, d.discount, numberInBasket / numberRequired)
      })
  }

}
