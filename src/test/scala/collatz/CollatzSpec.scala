import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should._

class CollatzSpec extends AnyFunSuite with Matchers:

    test("collatz 6") {
        collatz(6) should be (3)
    }

    test("collatz 7") {
        collatz(7) should be (22)
    }

    test("collatz 1") {
        collatz(1) should be (4)
    }

    test("collatz 2") {
        collatz(2) should be (1)
    }

    test("collatz 42") {
        collatz(42) should be (21)
    }

    test("collatz 101") {
        collatz(101) should be (304)
    }

end CollatzSpec

class CollatzCountSpec extends AnyFunSuite with Matchers:

    test("collatzCount 1") {
        collatzCount(1) should be (0)
    }

end CollatzCountSpec
