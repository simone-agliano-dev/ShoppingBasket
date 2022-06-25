package utilities.caseClasses

abstract class DiscountGeneric {
    val item: String
    val discount: Double
  }

  case class ConditionalDiscount(item: String,
                                 discount: Double,
                                 condition: Condition)
    extends DiscountGeneric

  case class Discount(item: String, discount: Double, numberOfTimesToApply: Int)
    extends DiscountGeneric

  case class Condition(goodsRequired: List[String]) {
    def countValues: Map[String, Int] = {
      this.goodsRequired
        .map(
          element => element -> 1
        )
        .groupBy(
          _._1
        )
        .mapValues(
          _.size
        )
    }
  }
