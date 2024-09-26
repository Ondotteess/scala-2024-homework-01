import scala.util.boundary, boundary.break

class Memory {
  var acc: Int = 0
  var A: Int = 0
  var B: Int = 0
  var blink: Boolean = false
}

abstract class Command {
  def execute(memory: Memory): Command
}

class Plus extends Command {
  override def execute(memory: Memory): Command = {
    memory.acc = memory.A + memory.B
    memory.blink = false
    this
  }
}

class Minus extends Command {
  override def execute(memory: Memory): Command = {
    memory.acc = memory.A - memory.B
    memory.blink = false
    this
  }
}

class Mult extends Command {
  override def execute(memory: Memory): Command = {
    memory.acc = memory.A * memory.B
    memory.blink = false
    this
  }
}

class Division extends Command {
  override def execute(memory: Memory): Command = {
    if (memory.B == 0) {
      memory.A = 0
      memory.B = 0
      memory.acc = 0
    } else {
      memory.acc = memory.A / memory.B
    }
    memory.blink = false
    this
  }
}

class Blink extends Command {
  override def execute(memory: Memory): Command = {
    memory.blink = !memory.blink
    this
  }
}

class Swap extends Command {
  override def execute(memory: Memory): Command = {
    val temp = memory.A
    memory.A = memory.B
    memory.B = temp
    this
  }
}

class Acc extends Command {
  override def execute(memory: Memory): Command = {
    if (memory.blink) {
      memory.B = memory.acc
    } else {
      memory.A = memory.acc
    }
    memory.blink = !memory.blink
    this
  }
}


class LoadInt(val value: Int) extends Command {
  override def execute(memory: Memory): Command = {
    if (memory.blink) {
      memory.B = value
    } else {
      memory.A = value
    }
    memory.blink = !memory.blink
    this
  }
}

@main def calculator(commands: String*): Unit = {
  def parseInt(s: String): Int = s.toInt

  val mem = new Memory()
  
  boundary {
    for (c <- commands) {
      c match
        case "+" => new Plus().execute(mem)
        case "-" => new Minus().execute(mem)
        case "*" => new Mult().execute(mem)
        case "/" => new Division().execute(mem)
        case "swap" => new Swap().execute(mem)
        case "blink" => new Blink().execute(mem)
        case "acc" => new Acc().execute(mem)
        case "break" => break(None)
        case s: String if s.matches("-?\\d+") =>
          new LoadInt(s.toInt).execute(mem)
        case _ => println("Unknown command")
    }
  }

  println(mem.acc)
}
