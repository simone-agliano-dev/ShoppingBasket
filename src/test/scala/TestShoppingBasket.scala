import org.scalatest.flatspec.AnyFlatSpec
import utilities.caseClasses.{Condition, ConditionalDiscount, Discount, GoodsBasket}
import utilities.discountFunctions.CalculateDiscounts.{calculateOnSaleGoods, getTotal}

class TestShoppingBasket extends AnyFlatSpec{
  "Total price" should "be calculated correctly" in {
    val pricesMap =
      Map("Soup" -> 0.65, "Bread" -> 0.8, "Milk" -> 1.3, "Apples" -> 1.0)

    val twoSoupDiscountBread: ConditionalDiscount =
      ConditionalDiscount("Bread", 0.5, Condition(List("Soup", "Soup")))

    val basket = List("Apples", "Milk", "Bread")

    val discountOnApples: Discount = Discount("Apples", 0.1, 1)

    val goodsCalculated = calculateOnSaleGoods(
      GoodsBasket.apply(basket,pricesMap),
      pricesMap,
      List(twoSoupDiscountBread),
      List(discountOnApples)
    )

    val totalWithDiscount = getTotal(goodsCalculated, discounted = true)
    val totalWithoutDiscount = getTotal(goodsCalculated, discounted = false)

    assert(totalWithoutDiscount == 3.10)
    assert(totalWithDiscount == 3.00)
  }

  it should "be calculated if conditional applied multiple times" in {
    val pricesMap =
      Map("Soup" -> 1.0, "Bread" -> 0.8)

    val twoSoupDiscountBread: ConditionalDiscount =
      ConditionalDiscount("Bread", 0.5, Condition(List("Soup", "Soup")))

    val basket = List("Soup", "Soup", "Bread", "Soup", "Soup", "Bread")

    val goodsCalculated = calculateOnSaleGoods(
      GoodsBasket.apply(basket, pricesMap),
      pricesMap,
      List(twoSoupDiscountBread),
      List.empty
    )

    val totalWithDiscount = getTotal(goodsCalculated, discounted = true)
    val totalWithoutDiscount = getTotal(goodsCalculated, discounted = false)

    assert(totalWithoutDiscount == 5.60)
    assert(totalWithDiscount == 4.80)
  }

  it should "be calculated when multiple items with unconditional discount" in {
    val pricesMap =
      Map("Apples" -> 1.0, "Bread" -> 0.8)

    val basket = List("Apples", "Apples", "Apples", "Bread")

    val discounts: List[Discount] = List(Discount("Apples", 0.1, 3))

    val goodsCalculated =
      calculateOnSaleGoods(
        GoodsBasket.apply(basket, pricesMap),
        pricesMap,
        List.empty,
        discounts
      )

    val totalWithDiscount = getTotal(goodsCalculated, discounted = true)
    val totalWithoutDiscount = getTotal(goodsCalculated, discounted = false)

    println(totalWithDiscount)
    println(totalWithoutDiscount)

    assert(totalWithoutDiscount == 3.80)
    assert(totalWithDiscount == 3.50)
  }

  it should "be calculated when multiple items with unconditional discount and conditional offer" in {
    val pricesMap =
      Map("Apples" -> 1.0, "Bread" -> 0.8, "Soup" -> 0.2, "Milk" -> 0.5)

    val basket =
      List("Apples", "Apples", "Apples", "Bread", "Bread", "Soup", "Soup")

    val discounts: List[Discount] = List(Discount("Apples", 0.1, 3))

    val twoSoupDiscountBread: ConditionalDiscount =
      ConditionalDiscount("Bread", 0.5, Condition(List("Soup", "Soup")))

    val goodsCalculated =
      calculateOnSaleGoods(
        GoodsBasket.apply(basket, pricesMap),
        pricesMap,
        List(twoSoupDiscountBread),
        discounts
      )

    val totalWithDiscount = getTotal(goodsCalculated, discounted = true)
    val totalWithoutDiscount = getTotal(goodsCalculated, discounted = false)

    println(totalWithDiscount)
    println(totalWithoutDiscount)

    assert(totalWithoutDiscount == 5.00)
    assert(totalWithDiscount == 4.30)
  }
}
