package golf

import utest.{TestSuite, Tests, assert, test}

object GolfTests extends TestSuite:

  val tests: Tests = Tests.apply:
    test ("Tests that "):
      val noSuchFile = "no-such-file.txt"
      val fallback = "We'll just start"

      assert (true)
