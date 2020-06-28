package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HelloSpec extends AnyFunSuite {
  import example.usecase.ConsoleChatImpl._

  test("quit") {
    assert(quit.matches(":quit"))
    assert(quit.matches(":q"))
    assert(!quit.matches("q"))
  }

  test("fetch") {
    "A fetch" match {
      case fetch(name) =>
        assert(name == "A")
      case _ => fail()
    }
  }

  test("post") {
    "A post Hello, World" match {
      case post(name, msg) =>
        assert(name == "A")
        assert(msg == "Hello, World")
      case _ => fail()
    }
  }

}
