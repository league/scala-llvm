
class sepderived extends septrait {
  val y: Int = 99
}

class sepsub extends sepsuper {
  val z: Int = 61
}

class sepboth extends sepsuper with septrait {
  val y: Int = 15
  def go {
    System.out.println(x)
    System.out.println(y)
  }
}

object sepderived {
  def main(args: Array[String]) {
    val q = new sepderived
    System.out.println(q.y)             // 99
    val r = new sepsub
    System.out.println(r.x)             // 42
    System.out.println(r.z)             // 61
    val b = new sepboth
    System.out.println(b.x)             // 42
    System.out.println(b.y)             // 15
    b.go
  }
}
