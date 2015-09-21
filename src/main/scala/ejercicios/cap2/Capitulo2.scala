package ejercicios.cap2

import concurrencia._

object Capitulo2 {
  // 1
  def parallel[A, B](a: => A, b: => B): (A, B) = {
    var res1 = null.asInstanceOf[A]
    var res2 = null.asInstanceOf[B]
    val t1 = thread {
      res1 = a
    }
    val t2 = thread {
      res2 = b
    }
    t1.join()
    t2.join

    (res1, res2)
  }

  // 2
  def periodically(duration: Long)(b: => Unit): Unit = {
    val worker = thread {
      while (true) {
        b
        Thread.sleep(duration)
      }
    }
    worker.setDaemon(true)
    worker.start()
  }

  // 3
  class SyncVar[T] {
    private var x = null.asInstanceOf[T]

    def get(): T = this.synchronized {
      if (x != null) {
        val aux = x
        x = null.asInstanceOf[T]
        aux
      } else throw Exception
    }

    def put(t: T): Unit = this.synchronized {
      if (x == null) x = t
      else throw Exception
    }

    // ejercicio 4
    def isEmpty: Boolean = synchronized {x == null}
    def nonEmpty: Boolean = synchronized {x != null}

    // ejercicio 5
    def getWait(): T = ???
    def putWait(): Unit = ???
  }

  // ejercicio 4 parte 2
  def consumerProducer(): Unit = {
    val syncVar = new SyncVar[Int]
    var x = 0

    val producer = thread {
      while (x < 15) {
        if (syncVar.isEmpty){
          syncVar.put(x)
          x += 1
        }
      }
    }

    val consumer = thread {
      while (x != 15) {
        if (syncVar.nonEmpty) {
          log(s"$x")
          x -= -1
        }
      }
    }

    producer.join()
    consumer.join()
  }



}

