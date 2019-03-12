package example

import java.io.{File, FileInputStream, InputStream}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class CustomerInfo(arrivalTime: Int, cookTime: Int)

object CustomerInfo {

  val orderingByCookTime: Ordering[CustomerInfo] =
    Ordering.by(_.cookTime)
}

object OrderProcessor extends App {

  /**
    * @param in An InputStream, which contains the following
                input:
                A line containing a single number: The number of
                guests N,
                Followed by N lines containing two numbers Ti
                and Li separated by space.
                There may be a trailing newline.
                Ti ist the ordering time for Ni, Li is the time
                it takes to bake Ni's pizza.
                0 <= N <= 100000
                0 <= Ti <= 1000000000
                1 <= Li <= 1000000000
2 / 3* @return
A Right containing the integer part of the
                average waiting time if the input is valid.
                A Left containing a syntax error description
                otherwise.
    */
  def process(in: InputStream): Either[String, Long] = {
    val bufferedReader = scala.io.Source.fromInputStream(in).bufferedReader()
    val streamStrings =
      Stream.continually(bufferedReader.readLine()).takeWhile(_ != null)
    val arrayInputs = streamStrings.toArray
    //take the number of customers we need to process, drop the first element to process the rest
    try {
      val numberCustomers = arrayInputs(0).toInt
      val customersRawArray = arrayInputs.drop(1)
      val customerInfoArray = customersRawArray
        .map(x => x.split(" "))
        .map(x => CustomerInfo(x(0).toInt, x(1).toInt))
        .sortBy(_.arrivalTime)

      val customerInfoBuffer =
        collection.mutable.ArrayBuffer(customerInfoArray: _*)

//      println(s"num customers $numberCustomers")
//      for (elem <- customerInfoArray) {
//        println(elem)
//      }
//      val customerByCookTime =
//        customerInfoArray.sorted(CustomerInfo.orderingByCookTime)
//      for (elem <- customerByCookTime) {
//        println(elem)
//      }
      var currentCustomerCount = 0
      var currentTime = 0
      var totalWaitTime: Long = 0L
      var customerQueue: scala.collection.mutable.PriorityQueue[CustomerInfo] =
        new mutable.PriorityQueue[CustomerInfo]()(
          CustomerInfo.orderingByCookTime.reverse)

//      println("array buffer state")
//      customerInfoBuffer.foreach(println(_))

//      while (numCustomersSoFar < numberCustomers) {
      while(currentCustomerCount < numberCustomers){
        customerQueue.enqueue(
          customerInfoBuffer.takeWhile(_.arrivalTime <= currentTime): _*)
        customerInfoBuffer --= customerQueue

        println("current state of the queue")
        customerQueue.foreach{println(_)}
        println("State of the buffer after removal")
        customerInfoBuffer.foreach{println(_)}

        val currentCustomer = customerQueue.dequeue()

        println(s"current customer is $currentCustomer")
        totalWaitTime += (currentTime - currentCustomer.arrivalTime) + currentCustomer.cookTime
        currentTime += currentCustomer.cookTime
        currentCustomerCount += 1
      }
//      println(s"wait time = $totalWaitTime")
//      println(s"current time = $currentTime")
      //println(strings.mkString("\n"))
      bufferedReader.close()
      Right((totalWaitTime / numberCustomers))
    } catch {
      case numberFormatException: NumberFormatException =>
        Left(
          s"Error with $numberFormatException and message ${numberFormatException.getMessage}")
      case arrayIndexOutOfBoundsException: ArrayIndexOutOfBoundsException =>
        Left(
          s"Error with $arrayIndexOutOfBoundsException and message ${arrayIndexOutOfBoundsException.getMessage}")
      case ex: Exception =>
        Left(
          s"Error processing with exception ${ex} and message ${ex.getMessage}")
    }
  }

  //start MAIN
  val fileInputStream = new FileInputStream(new File("input3.md"))
  println(process(fileInputStream) match {
    case Left(str) => s"process functions error out with $str"
    case Right(averageWaitTime) =>
      s"Process functions is successful with average wait time = $averageWaitTime"
  })
}

