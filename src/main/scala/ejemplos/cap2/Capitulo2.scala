package ejemplos.cap2

import concurrencia._

/*
  "All non-trivial abstractions, to some degree, are leaky" Jeff Atwood
 */
class Capitulo2 {
  val t: Thread = Thread.currentThread()
  val name = t.getName
  println(s"I am the thread $name")
}

// al extender de App, el metodo main se extrae del cuerpo del objeto
object ThreadsCreation extends App {

  class MyThread extends Thread {
    override def run(): Unit = {
      println("New thread running.")
    }
  }

  val t = new MyThread
  t.start()
  t.join() // el thread main espera a que t termine
  println("New thread joined.")

}


object ThreadSleep extends App {
  val t = thread {
    Thread.sleep(1000)
    log("New thread running.")
    Thread.sleep(1000)
    log("Still running.")
    Thread.sleep(1000)
    log("Completed.")
  }
  t.join
  log("New thread joined.")
}

object ThreadNondeterminism extends App {
  val t = thread {
    log("New thread running.")
  }
  log("...")
  log("...")
  t.join()
  log("New thread joined.")
}

object ThreadsCommunicate extends App {
  var result: String = null
  val t = thread {
    result = "\n Title\n" + "=" * 5
  }
  t.join()
  log(result)
}

object ThreadsUnprotectedUid extends App {
  var uidCount = 0L

  def getUniqueId() = {
    // metodo no atomico
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def printUniqueIds(n: Int): Unit = {
    val uids = for (i <- 0 until n) yield getUniqueId()
    log(s"Generated uids: $uids")
  }

  // debido a que no es atomico hay condiciones de carrera
  // los uids no son unicos.
  val t = thread {
    printUniqueIds(5)
  }
  printUniqueIds(5)
  t.join()
}

object ThreadsProtectedUid extends App {
  var uidCount = 0L

  def getUniqueId() = this.synchronized {
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def printUniqueIds(n: Int): Unit = {
    val uids = for (i <- 0 until n) yield getUniqueId()
    log(s"Generated uids: $uids")
  }

  // debido a que no es atomico hay condiciones de carrera
  // los uids no son unicos.
  val t = thread {
    printUniqueIds(5)
  }
  printUniqueIds(5)
  t.join()
}

object ThreadSharedStateAccessReordering extends App {
  for (i <- 0 until 100000) {
    var a = false
    var b = false
    var x = -1
    var y = -1
    val t1 = thread {
      a = true
      y = if (b) 0 else 1
    }
    val t2 = thread {
      b = true
      x = if (a) 0 else 1
    }

    t1.join()
    t2.join()
    assert(!(x == 1 && y == 1), s"x = $x, y = $y")
  }
}

// =============================================================

// Monitors and Synchronization

object SynchronizedNesting extends App {

  import scala.collection._

  private val transfers = mutable.ArrayBuffer[String]()

  def logTransfer(name: String, n: Int) = {
    transfers.synchronized {
      transfers += s"transfer to account '$name' = $n"
    }
  }

  class Account(val name: String, var money: Int)

  def add(account: Account, n: Int): Unit = account.synchronized {
    account.money += n
    if (n > 10) logTransfer(account.name, n)
  }

  val jane = new Account("Jane", 100)
  val john = new Account("John", 200)
  val t1 = thread {add(jane, 5)}
  val t2 = thread {add(john, 50)}
  val t3 = thread {add(jane, 70)}
  t1.join();t2.join();t3.join()
  log(s"---transfers ---\n $transfers ")
}

object SynchronizedDeadlock extends App {
  import SynchronizedNesting.Account

  def send(a: Account, b: Account, n: Int) = a.synchronized {
    b.synchronized {
      a.money -= n
      b.money += n
    }
  }

  val a = new Account("Jack", 1000)
  val b = new Account("Jill", 2000)
  val t1 = thread { for (i <- 0 until 100) send(a,b,1)}
  val t2 = thread { for (i <- 0 until 100) send(b,a,1)}
  t1.join(); t2.join()
  log(s"a = ${a.money}, b = ${b.money}")
}
