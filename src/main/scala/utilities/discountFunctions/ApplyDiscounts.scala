package utilities.discountFunctions

import utilities.caseClasses.{Discount, Good, GoodsBasket}

object ApplyDiscounts {


  def applyDiscounts(goodsInABasket : GoodsBasket, listOfDiscounts : List[Discount]) : GoodsBasket = {
    GoodsBasket(
      listOfDiscounts
        .filter(d => goodsInABasket.goods.map(g => g.itemName).contains(d.item))
        .map(dis => {
          val priceGood = goodsInABasket.goods
            .find(b => { b.itemName == dis.item && b.price == b.priceWithDiscount })
            .get
            .price
          Good(dis.item, priceGood, priceGood * (1 - dis.discount))
        })
    )
  }
  def findNumberOfEachItemNotDiscounted(
                                         basket: GoodsBasket,
                                         discountedGoods: GoodsBasket
                                       ): Map[String, Int] = {

    def howMany(item: String): Int =
      discountedGoods.countQuantityOfEach.getOrElse(item, 0)

    basket
      .countQuantityOfEach flatMap { h =>
      h match {
        case x if x._2 - howMany(x._1) > 0 => Map(x._1 -> (x._2 -howMany(x._1)))
        case _ => None
      }
    }
  }

  def getBasketWithDiscountsApplied(basket: GoodsBasket,
                                    discounts: List[Discount]): GoodsBasket = {

    val discountedGoods = applyDiscounts(basket, discounts)

    val itemsNotDiscounted: Map[String, Int] =
      findNumberOfEachItemNotDiscounted(basket, discountedGoods)



    val goodsWithoutDiscountsApplied = (for {
      itemNotDiscounted <- itemsNotDiscounted
      priceGood = basket.goods
        .find(item => item.itemName == itemNotDiscounted._1)
        .get
        .price
      _ <- 0 until itemNotDiscounted._2
    } yield Good(itemNotDiscounted._1, priceGood, priceGood)).toList

    discountedGoods.addGood(GoodsBasket(goodsWithoutDiscountsApplied))
  }

}
