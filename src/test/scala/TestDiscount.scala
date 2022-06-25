import org.scalatest.flatspec.AnyFlatSpec
import utilities.caseClasses.{Condition, ConditionalDiscount, Discount, Good, GoodsBasket}
import utilities.discountFunctions.ApplyDiscounts.getBasketWithDiscountsApplied
import utilities.discountFunctions.ConditionalDiscounts.verifyApplicableDiscounts

class TestDiscount extends AnyFlatSpec {

  it should "be applied when conditional" in {

    val goodsInBasket = GoodsBasket(List(
      Good("Apples", 1.0, 1.0),
      Good("Apples", 1.0, 1.0),
      Good("Bread", 0.8, 0.8)
    ))

    val expectedGoods = GoodsBasket(List(
      Good("Bread", 0.8, 0.4),
      Good("Apples", 1.0, 1.0),
      Good("Apples", 1.0, 1.0)
    ))

    val conditionalDiscounts: List[ConditionalDiscount] =
      List(
        ConditionalDiscount("Bread", 0.5, Condition(List("Apples", "Apples")))
      )

    val allDiscounts =
      verifyApplicableDiscounts(goodsInBasket, conditionalDiscounts)
    val actual =
      getBasketWithDiscountsApplied(goodsInBasket, allDiscounts)

    assert(actual.equals(expectedGoods))
  }

  it should "be applied when multiple conditional discounts" in {

    val goodsInBasket = GoodsBasket(List(
      Good("Apples", 1.0, 1.0),
      Good("Apples", 1.0, 1.0),
      Good("Bread", 0.8, 0.8),
      Good("pear", 2.0, 2.0),
      Good("banana", 1.8, 1.8)
    ))

    val expectedGoods = GoodsBasket(List(
      Good("Bread", 0.8, 0.4),
      Good("pear", 2.0, 1.6),
      Good("banana", 1.8, 1.8),
      Good("Apples", 1.0, 1.0),
      Good("Apples", 1.0, 1.0)
    ))

    val conditionalDiscounts: List[ConditionalDiscount] =
      List(
        ConditionalDiscount("Bread", 0.5, Condition(List("Apples", "Apples"))),
        ConditionalDiscount("pear", 0.2, Condition(List("banana")))
      )

    val allDiscounts =
      verifyApplicableDiscounts(goodsInBasket, conditionalDiscounts)
    val actual =
      getBasketWithDiscountsApplied(goodsInBasket, allDiscounts)

    assert(actual.equals(expectedGoods))
  }

  it should "be applied when multiple different goods are required for discount to apply" in {

    val goodsInBasket = GoodsBasket(List(
      Good("Apples", 1.0, 1.0),
      Good("Soup", 1.0, 1.0),
      Good("Soup", 1.0, 1.0),
      Good("Bread", 0.8, 0.8)
    ))

    val expectedGoods = GoodsBasket(List(
      Good("Bread", 0.8, 0.4),
      Good("Apples", 1.0, 1.0),
      Good("Soup", 1.0, 1.0),
      Good("Soup", 1.0, 1.0)
    ))

    val conditionalDiscounts: List[ConditionalDiscount] =
      List(ConditionalDiscount("Bread", 0.5, Condition(List("Apples", "Soup"))))

    val allDiscounts: List[Discount] =
      verifyApplicableDiscounts(goodsInBasket, conditionalDiscounts)

    val actual =
      getBasketWithDiscountsApplied(goodsInBasket, allDiscounts)

    assert(actual.equals(expectedGoods))
  }

  it should "only apply conditional discount to item once" in {

    val goodsInBasket = GoodsBasket(List(
      Good("Apples", 1.0, 1.0),
      Good("Apples", 1.0, 1.0),
      Good("Bread", 0.8, 0.8),
      Good("Bread", 0.8, 0.8)
    ))

    val expectedGoods = GoodsBasket(List(
      Good("Bread", 0.8, 0.4),
      Good("Bread", 0.8, 0.8),
      Good("Apples", 1.0, 1.0),
      Good("Apples", 1.0, 1.0)
    ))

    val conditionalDiscounts: List[ConditionalDiscount] =
      List(
        ConditionalDiscount("Bread", 0.5, Condition(List("Apples", "Apples")))
      )

    val allDiscounts: List[Discount] =
      verifyApplicableDiscounts(goodsInBasket, conditionalDiscounts)

    val applyNew =
      getBasketWithDiscountsApplied(goodsInBasket, allDiscounts)

    assert(expectedGoods.equals(applyNew))
  }

  it should "calculate normal price if no discounts applicable" in {

    val goodsInBasket = GoodsBasket(List(Good("Soup", 1.0, 1.0), Good("Apples", 1.0, 1.0)))

    val expectedGoods = GoodsBasket(List(Good("Apples", 1.0, 1.0),Good("Soup", 1.0, 1.0)))

    val conditionalDiscounts: List[ConditionalDiscount] =
      List(
        ConditionalDiscount("Bread", 0.5, Condition(List("Apples", "Apples")))
      )

    val allDiscounts: List[Discount] =
      verifyApplicableDiscounts(goodsInBasket, conditionalDiscounts)

    val applyNew =
      getBasketWithDiscountsApplied(goodsInBasket, allDiscounts)

    assert(applyNew.equals(expectedGoods))

  }

  it should "generate the required number of discounts" in {

    val goodsInBasket = GoodsBasket(List(
      Good("Bread", 0.8, 0.8),
      Good("Bread", 0.8, 0.8),
      Good("Soup", 1.0, 1.0),
      Good("Soup", 1.0, 1.0),
      Good("Soup", 1.0, 1.0),
      Good("Soup", 1.0, 1.0),
      Good("Soup", 1.0, 1.0),
      Good("Soup", 1.0, 1.0),
      Good("Soup", 1.0, 1.0)
    ))

    val conditionalDiscounts: List[ConditionalDiscount] =
      List(ConditionalDiscount("Bread", 0.5, Condition(List("Soup", "Soup"))))

    val allDiscounts: List[Discount] =
      verifyApplicableDiscounts(goodsInBasket, conditionalDiscounts)

    assert(allDiscounts.head.numberOfTimesToApply == 3)
  }
}
