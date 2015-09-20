package ejemplos.cap1

class Printer(val greeting: String) {
  def printMessage(): Unit = println(greeting + "!")
  def printNumber(x: Int): Unit = {
    println("Number: " + x)
  }
}

trait Logging {
  def log(n: String): Unit
  def warn(s: String) = log("WARN: " + s)
  def error(s: String) = log("ERROR: " + s)
}

class PrintLogging extends Logging {
  def log(s: String) = println(s)
}
