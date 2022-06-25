package utilities.caseClasses

case class Good(itemName: String, price: Double, priceWithDiscount: Double) {
  def getCost(applyDiscount: Boolean): Double ={
    if (applyDiscount) this.priceWithDiscount
    else this.price
}
}

case class GoodsBasket(goods : List[Good]) {

  def addGood(basket: GoodsBasket): GoodsBasket = GoodsBasket(this.goods ::: basket.goods )
  def countQuantityOfEach: Map[String, Int] =
    goods
      .groupBy(_.itemName)
      .map(listOfSameGood => listOfSameGood._1 -> listOfSameGood._2.size)
}

object GoodsBasket {
  def apply(items: List[String], pricesMap: Map[String, Double]): GoodsBasket =
    GoodsBasket(
      items
        .map(
          item => Good(
            item, pricesMap(item), pricesMap(item))
        )
    )
}
