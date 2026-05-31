// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package adder

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


/** 
  * Full adder tester
  * Use the truth table from the exercise sheet to test all possible input combinations and the corresponding results exhaustively
  */
class FullAdderTester extends AnyFlatSpec with ChiselScalatestTester {

  "FullAdder" should "work" in {
    test(new FullAdder).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

          /*dut.io.a.poke(...)
           *dut.io.b.poke(...)
           *dut.io.ci.poke(...)
           *dut.io.s.expect(...)
           *dut.io.co.expect(...)
           *...
           *TODO: Insert your test cases
           */
          // Test case 1: 0 + 0 + 0
          dut.io.a.poke(false.B)
          dut.io.b.poke(false.B)
          dut.io.ci.poke(false.B)
          dut.io.s.expect(false.B)
          dut.io.co.expect(false.B)
          dut.clock.step(1)

          // Test case 2: 0 + 0 + 1
          dut.io.a.poke(false.B)
          dut.io.b.poke(false.B)
          dut.io.ci.poke(true.B)
          dut.io.s.expect(true.B)
          dut.io.co.expect(false.B)
          dut.clock.step(1)

          // Test case 3: 0 + 1 + 0
          dut.io.a.poke(false.B)
          dut.io.b.poke(true.B)
          dut.io.ci.poke(false.B)
          dut.io.s.expect(true.B)
          dut.io.co.expect(false.B)
          dut.clock.step(1)

          // Test case 4: 0 + 1 + 1
          dut.io.a.poke(false.B)
          dut.io.b.poke(true.B)
          dut.io.ci.poke(true.B)
          dut.io.s.expect(false.B)
          dut.io.co.expect(true.B)
          dut.clock.step(1)

          // Test case 5: 1 + 0 + 0
          dut.io.a.poke(true.B)
          dut.io.b.poke(false.B)
          dut.io.ci.poke(false.B)
          dut.io.s.expect(true.B)
          dut.io.co.expect(false.B)
          dut.clock.step(1)

          // Test case 6: 1 + 0 + 1
          dut.io.a.poke(true.B)
          dut.io.b.poke(false.B)
          dut.io.ci.poke(true.B)
          dut.io.s.expect(false.B)
          dut.io.co.expect(true.B)
          dut.clock.step(1)

          // Test case 7: 1 + 1 + 0
          dut.io.a.poke(true.B)
          dut.io.b.poke(true.B)
          dut.io.ci.poke(false.B)
          dut.io.s.expect(false.B)
          dut.io.co.expect(true.B)
          dut.clock.step(1)

          // Test case 8: 1 + 1 + 1
          dut.io.a.poke(true.B)
          dut.io.b.poke(true.B)
          dut.io.ci.poke(true.B)
          dut.io.s.expect(true.B)
          dut.io.co.expect(true.B)
          dut.clock.step(1)


        }
    } 
}

