package PipelinedRV32I_Tester

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import PipelinedRV32I._

class Assignment5RegressionSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def observeInOrder(dut: PipelinedRV32I, expected: Seq[BigInt], forbidden: Set[BigInt] = Set.empty): Unit = {
    var next = 0
    for (_ <- 0 until 80 if next < expected.length) {
      val value = dut.io.result.peek().litValue
      assert(!forbidden.contains(value), s"wrong-path result committed: $$value")
      if (value == expected(next)) next += 1
      dut.io.exception.expect(false.B)
      dut.clock.step()
    }
    assert(next == expected.length, "observed only " + next + " of " + expected.length + " expected results")
  }

  "Assignment 5 forwarding" should "handle dependent and chained ALU operations" in {
    test(new PipelinedRV32I("src/test/programs/BinaryFile_forwarding_chain")) { dut =>
      observeInOrder(dut, Seq(1, 3, 6, 9))
    }
  }

  "Assignment 5 exception handling" should "surface an invalid instruction" in {
    test(new PipelinedRV32I("src/test/programs/BinaryFile_invalid")) { dut =>
      var sawException = false
      for (_ <- 0 until 12) {
        sawException ||= dut.io.exception.peek().litToBoolean
        dut.clock.step()
      }
      assert(sawException, "invalid opcode never reached the exception output")
    }
  }

  "Assignment 6 branch recovery" should "handle adjacent, consecutive, and mixed branch outcomes" in {
    test(new PipelinedRV32I("src/test/programs/BinaryFile_branch_patterns")) { dut =>
      observeInOrder(dut, Seq(202, 203, 204, 205), Set(901, 902, 903))
    }
  }
}
