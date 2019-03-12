package example

import org.scalatest._

class PizzaWaitTimeSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    OrderProcessor.greeting shouldEqual "hello"
  }
}
