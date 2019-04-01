package example

import java.io.{File, FileInputStream, IOException, InputStream}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
  case class CustomerInfo(arrivalTime: Int, cookTime: Int)

  object CustomerInfo {

    val orderingByCookTime: Ordering[CustomerInfo] =
      Ordering.by(_.cookTime)
  }

  def process(in: InputStream): Either[String, Long] = {
    val bufferedReader = scala.io.Source.fromInputStream(in).bufferedReader()
    val streamStrings =
      Stream.continually(bufferedReader.readLine()).takeWhile(_ != null)
    //NB: I can probably optimize this further by taking the input from this stream one at a time,
    //but from the constraint of N seems like this is fine for now

    //take the number of customers we need to process, drop the first element to process the rest
    try {
      val arrayInputs = streamStrings.toArray
      val numberCustomers = arrayInputs(0).toInt
      val customersRawArray = arrayInputs.drop(1)
      val customerInfoArray = customersRawArray
        .map(x => x.split(" "))
        .map(x => CustomerInfo(x(0).toInt, x(1).toInt))
        .sortBy(_.arrivalTime)

      val customerInfoBuffer =
        collection.mutable.ArrayBuffer(customerInfoArray: _*)

      var currentCustomerCount = 0
      var currentTime = 0
      var totalWaitTime: Long = 0L
      var customerQueue: scala.collection.mutable.PriorityQueue[CustomerInfo] =
        new mutable.PriorityQueue[CustomerInfo]()(
          CustomerInfo.orderingByCookTime.reverse)

      while (currentCustomerCount < numberCustomers) {
        //take all the customers that is currently in the store
        //and then remove those customers from the array buffer
        customerQueue.enqueue(
          customerInfoBuffer.takeWhile(_.arrivalTime <= currentTime): _*)
        customerInfoBuffer --= customerQueue

        val currentCustomer = customerQueue.dequeue()
        totalWaitTime += (currentTime - currentCustomer.arrivalTime) + currentCustomer.cookTime
        currentTime += currentCustomer.cookTime
        currentCustomerCount += 1
      }
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

  //start MAIN function
  try {
    val fileInputStream = new FileInputStream(new File("input.md"))
    println(process(fileInputStream) match {
      case Left(str) => s"process functions error out with $str"
      case Right(averageWaitTime) =>
        s"Process functions is successful with average wait time = $averageWaitTime"
    })
  } catch {
    case ioException: IOException => s"Error with $ioException and message ${ioException.getMessage}"
    case ex : Exception =>
      s"Error processing with exception ${ex} and message ${ex.getMessage}"
  }
}
