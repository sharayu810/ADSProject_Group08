// ADS I Class Project
// Pipelined RISC-V Core - Assignment 05 Test Bench
//
// Branch and Jump Instruction Test

package PipelinedRV32I_Tester

import chisel3._
import chiseltest._
import PipelinedRV32I._
import org.scalatest.flatspec.AnyFlatSpec

class Assignment5BranchTest extends AnyFlatSpec with ChiselScalatestTester {

  private val wrongPathMarkers = (901 to 918).map(BigInt(_)).toSet 

  private def waitForResult(
      dut: PipelinedRV32I,
      expected: BigInt,
      label: String,
      maxCycles: Int = 40  //If the expected result does not appear within 40 cycles, fail.
  ): Unit = {              //it does not return a value. It only checks the CPU and may fail the test.
    var seen = false
    var cycles = 0

    while (!seen && cycles < maxCycles) {
      dut.io.exception.expect(false.B)       //

      val current = dut.io.result.peek().litValue  //Reads the current CPU output.  //
      assert(
        !wrongPathMarkers.contains(current),
        s"$label saw wrong-path marker $current before expected result $expected"
      )

      if (current == expected) {
        seen = true
      } else {
        dut.clock.step(1)
        cycles += 1
      }
    }

    withClue(s"$label did not produce result $expected within $maxCycles cycles") {
      dut.io.result.expect(expected.U)
      dut.io.exception.expect(false.B)
    }
  }

  "RV32I_Assignment5_All" should "execute branch and jump instructions" in {
    test(new PipelinedRV32I("src/test/programs/BinaryFile_a5_all")).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      dut.clock.step(5)

      val expectedResults = Seq(
        101 -> "BEQ taken",
        102 -> "BEQ not taken, first sequential marker",
        103 -> "BEQ not taken, second sequential marker",
        104 -> "BEQ not taken, third sequential marker",
        105 -> "BNE taken",
        106 -> "BNE not taken, first sequential marker",
        107 -> "BNE not taken, second sequential marker",
        108 -> "BNE not taken, third sequential marker",
        109 -> "BLT signed taken",
        110 -> "BLT signed not taken, first sequential marker",
        111 -> "BLT signed not taken, second sequential marker",
        112 -> "BLT signed not taken, third sequential marker",
        113 -> "BGE signed taken",
        114 -> "BGE signed not taken, first sequential marker",
        115 -> "BGE signed not taken, second sequential marker",
        116 -> "BGE signed not taken, third sequential marker",
        117 -> "BLTU unsigned taken",
        118 -> "BLTU unsigned not taken, first sequential marker",
        119 -> "BLTU unsigned not taken, second sequential marker",
        120 -> "BLTU unsigned not taken, third sequential marker",
        121 -> "BGEU unsigned taken",
        122 -> "BGEU unsigned not taken, first sequential marker",
        123 -> "BGEU unsigned not taken, second sequential marker",
        124 -> "BGEU unsigned not taken, third sequential marker",
        125 -> "forwarded operands into branch compare",
        126 -> "backward branch loop with negative B immediate",
        129 -> "JAL forward target",
        130 -> "JAL backward target",
        131 -> "JALR target",
        132 -> "final end marker"
      )

      expectedResults.foreach { case (value, label) =>
        waitForResult(dut, value, label)
      }
    }
  }
}