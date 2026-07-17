package PipelinedRV32I_Tester

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import PipelinedRV32I._

class Assignment6PipelineSpec extends AnyFlatSpec with ChiselScalatestTester {
  "Assignment 6 pipeline" should "recover branches while preserving loops, backward branches, JAL, and JALR" in {
    test(new PipelinedRV32I("src/test/programs/BinaryFile_a5_all")) { dut =>
      val expected = Seq[BigInt](101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
        111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124,
        125, 126, 129, 130, 131, 132)
      val wrongPath = (901 to 918).map(BigInt(_)).toSet
      var next = 0
      var cycles = 0
      while (next < expected.length && cycles < 300) {
        dut.io.exception.expect(false.B)
        val result = dut.io.result.peek().litValue
        assert(!wrongPath.contains(result), s"wrong-path instruction committed: $result")
        if (result == expected(next)) next += 1
        dut.clock.step()
        cycles += 1
      }
      assert(next == expected.length, s"only observed $next/${expected.length} expected results")
    }
  }
}

