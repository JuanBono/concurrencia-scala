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

object SynchronizedProtectedUid extends App {
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

  import SynchronizedProtectedUid.getUniqueId
  import scala.collection._

  private val transfers = mutable.ArrayBuffer[String]()

  def logTransfer(name: String, n: Int) = {
    transfers.synchronized {
      transfers += s"transfer to account '$name' = $n"
    }
  }

  class Account(val name: String, var money: Int) {
    val uid = getUniqueId()
  }

  def add(account: Account, n: Int): Unit = account.synchronized {
    account.money += n
    if (n > 10) logTransfer(account.name, n)
  }

  val jane = new Account("Jane", 100)
  val john = new Account("John", 200)
  val t1 = thread {
    add(jane, 5)
  }
  val t2 = thread {
    add(john, 50)
  }
  val t3 = thread {
    add(jane, 70)
  }
  t1.join()
  t2.join()
  t3.join()
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
  val t1 = thread {
    for (i <- 0 until 100) send(a, b, 1)
  }
  val t2 = thread {
    for (i <- 0 until 100) send(b, a, 1)
  }
  t1.join();
  t2.join()
  log(s"a = ${a.money}, b = ${b.money}")
}

// el anterior arreglado
object SynchronizedAccounts extends App {

  import SynchronizedNesting.Account


  def send(a1: Account, a2: Account, n: Int) = {
    def adjust(): Unit = {
      a1.money -= n
      a2.money += n
    }
    if (a1.uid < a2.uid)
      a1.synchronized {
        a2.synchronized {
          adjust()
        }
      }
    else a2.synchronized {
      a1.synchronized {
        adjust()
      }
    }
  }

  val a = new Account("Jack", 1000)
  val b = new Account("Jill", 2000)
  val t1 = thread {
    for (i <- 0 until 100) send(a, b, 1)
  }
  val t2 = thread {
    for (i <- 0 until 100) send(b, a, 1)
  }
  t1.join()
  t2.join()
  log(s"a = ${a.money}, b = ${b.money}")
}

// ======================================================================

// Guarded Blocks

// thread pool: conjunto de threads, sirve para reutilizarlos.

import scala.collection._

object SynchronizedBadPool extends App {
  private val tasks = mutable.Queue[() => Unit]()

  val worker = new Thread {
    def poll(): Option[() => Unit] = tasks.synchronized {
      if (tasks.nonEmpty) Some(tasks.dequeue()) else None
    }

    override def run() = while (true) poll()
    match {
      case Some(task) => task()
      case None => Unit
    }
  }
  worker.setName("Worker")
  worker.setDaemon(true)
  worker.start()

  def asynchronous(body: => Unit) = tasks.synchronized {
    tasks.enqueue(() => body)
  }

  asynchronous {
    log("Hello")
  }
  asynchronous {
    log("World!")
  }
  Thread.sleep(5000)
}

// solo se puede llamar a wait() y notify() en x
// si el thread actual tiene el monitor del objeto x
object SynchronizedGuardedBlocks extends App {
  val lock = new AnyRef
  var message: Option[String] = None
  val greeter = thread {
    lock.synchronized {
      while (message == None) lock.wait()
      log(message.get)
    }
  }
  lock.synchronized {
    message = Some("Hello!")
    lock.notify()
  }
  greeter.join()
}

object SynchronizedPool extends App {
  private val tasks = mutable.Queue[() => Unit]()

  /* VERSION SIN GRACEFUL SHUTDOWN
  object Worker extends Thread {
    setDaemon(true)
    def poll() = tasks.synchronized {
      while (tasks.isEmpty) tasks.wait()
      tasks.dequeue()
    }
    override def run() = while (true) {
      val task = poll()
      task()
    }
  }
  */
  // version con graceful shutdown
  object Worker extends Thread {
    var terminated = false

    def poll(): Option[() => Unit] = tasks.synchronized {
      while (tasks.isEmpty && !terminated)
        tasks.wait()
      if (!terminated) Some(tasks.dequeue()) else None
    }

    import scala.annotation.tailrec

    @tailrec override def run() = poll() match {
      case Some(task) => task(); run()
      case None => Unit
    }

    def shutdown() = tasks.synchronized {
      terminated = true
      tasks.notify()
    }
  }

  // usar esta version para asegurarse que los threads terminan sin race cond.
  // solo es preferible usar interrupt() cuando el thread hace IO bloqueante
  // en un objeto InterruptibleChannel
  Worker.start()

  def asynchronous(body: => Unit) = tasks.synchronized {
    tasks.enqueue(() => body)
    tasks.notify()
  }

  asynchronous {
    log("Hello ")
  }
  asynchronous {
    log("World!")
  }
  Thread.sleep(5000)
}

//==========================================================

// Volatile Variables
// las variables volatiles son una forma mas liviana de sincronizar threads, pueden ser leidas y
// modificadas de forma atomica, se usan mayormente como flags de estado (para anunciar que una operacion
// se ha terminado o ha sido cancelada, por ejemplo.
// Cuando se escribe en una variable volatil, todos los demas threads lo veen (no es necesario notify)
class Page(val txt: String, var position: Int)
object Volatile extends App {
  val pages = for (i <- 1 to 5) yield
    new Page("Na" * (100 - 20 * i) + " Batman!", -1)
  @volatile var found = false
  for (p <- pages) yield thread {
    var i = 0
    while (i < p.txt.length && !found)
      if (p.txt(i) == '!') {
        p.position = i
        found = true
      } else i += 1
  }
  while (!found) {}
  log(s"results: ${pages.map(_.position)}")
}

// ==================================================================

// Immutable objects and final fields

// si un objeto solo tiene atributos finales, (no mutables, es decir con val) y ademas
// su referencia no es visible hasta su constructor termina, se lo considera inmutable.
// Se lo puede compartir a traves de los threads sin ningunt tipo de sincronizacion
// List y Vector no pueden ser compartidos sin sincronizacion