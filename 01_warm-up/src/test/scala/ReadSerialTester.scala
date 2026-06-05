        
package readserial

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


/**
  * Serial Receiver Tester
  */
class ReadSerialTester extends AnyFlatSpec with ChiselScalatestTester {

  // -----------------------------------------------------------------------
  // Test 1: Basic transmission - send 0xB2 (10110010), check data and valid
  // -----------------------------------------------------------------------
  "ReadSerial" should "work" in {
    test(new ReadSerial).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      // idle
      dut.io.rxd.poke(true.B)
      dut.clock.step(1)

      // start bit
      dut.io.rxd.poke(false.B)
      dut.clock.step(1)

      // send 8 bits: 10110010 (MSB first)
      dut.io.rxd.poke(true.B)   // bit 7
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 6
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 5
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 4
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 3
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 2
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 1
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 0
      dut.clock.step(1)

      // valid should be high and data should match
      dut.io.valid.expect(true.B)
      dut.io.data.expect("b10110010".U)

      // back to idle - valid must drop after one cycle
      dut.io.rxd.poke(true.B)
      dut.clock.step(1)
      dut.io.valid.expect(false.B)

  // -----------------------------------------------------------------------
  // Test 2: Second transmission after idle - send 0x32 (00110010)
  // -----------------------------------------------------------------------

      // idle
      dut.io.rxd.poke(true.B)
      dut.clock.step(1)

      // start bit
      dut.io.rxd.poke(false.B)
      dut.clock.step(1)

      // send 8 bits: 00110010 (MSB first)
      dut.io.rxd.poke(false.B)  // bit 7
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 6
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 5
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 4
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 3
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 2
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 1
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 0
      dut.clock.step(1)

      // valid should be high and data should match
      dut.io.valid.expect(true.B)
      dut.io.data.expect("b00110010".U)

      // back to idle
      dut.io.rxd.poke(true.B)
      dut.clock.step(1)
      dut.io.valid.expect(false.B)
  

  // -----------------------------------------------------------------------
  // Test 3: Back-to-back transmissions with NO idle gap
  //         Frame 1: 0xB2 (10110010)  ->  Frame 2: 0x32 (00110010)
  // -----------------------------------------------------------------------
      // idle
      dut.io.rxd.poke(true.B)
      dut.clock.step(1)

      // ── Frame 1: 10110010 ──

      // start bit
      dut.io.rxd.poke(false.B)
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 7
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 6
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 5
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 4
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 3
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 2
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 1
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 0
      dut.clock.step(1)

      // check frame 1 result
      dut.io.valid.expect(true.B)
      dut.io.data.expect("b10110010".U)

      // ── Frame 2 starts immediately (no idle gap) ──

      // start bit of frame 2
      dut.io.rxd.poke(false.B)
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 7
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 6
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 5
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 4
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 3
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 2
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 1
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 0
      dut.clock.step(1)

      // check frame 2 result
      dut.io.valid.expect(true.B)
      dut.io.data.expect("b00110010".U)

      // back to idle
      dut.io.rxd.poke(true.B)
      dut.clock.step(1)
      dut.io.valid.expect(false.B)
  

  // -----------------------------------------------------------------------
  // Test 4: valid is asserted for exactly ONE clock cycle
  // -----------------------------------------------------------------------

      // idle
      dut.io.rxd.poke(true.B)
      dut.clock.step(1)

      // start bit
      dut.io.rxd.poke(false.B)
      dut.clock.step(1)

      // send 8 bits: 11110000 (MSB first)
      dut.io.rxd.poke(true.B)   // bit 7
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 6
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 5
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 4
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 3
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 2
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 1
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 0
      dut.clock.step(1)

      // valid must be high exactly now
      dut.io.valid.expect(true.B)
      dut.io.data.expect("b11110000".U)

      // must go low the very next cycle
      dut.io.rxd.poke(true.B)
      dut.clock.step(1)
      dut.io.valid.expect(false.B)

      // stays low
      dut.clock.step(1)
      dut.io.valid.expect(false.B)
  

  // -----------------------------------------------------------------------
  // Test 5: Reset during an ongoing transmission aborts it
  //         valid must be 0 after reset; new clean frame starts after
  // -----------------------------------------------------------------------

      // idle
      dut.io.rxd.poke(true.B)
      dut.clock.step(1)

      // start bit
      dut.io.rxd.poke(false.B)
      dut.clock.step(1)

      // send only 4 bits before reset: 1011 (partial frame)
      dut.io.rxd.poke(true.B)   // bit 7
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 6
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 5
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 4
      dut.clock.step(1)

      // apply reset mid-transmission
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)

      // valid must be 0 right after reset
      dut.io.valid.expect(false.B)

      // bus idle for a few cycles
      dut.io.rxd.poke(true.B)
      dut.clock.step(3)
      dut.io.valid.expect(false.B)

      // send a clean new frame: 10100101
      // start bit
      dut.io.rxd.poke(false.B)
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 7
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 6
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 5
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 4
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 3
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 2
      dut.clock.step(1)

      dut.io.rxd.poke(false.B)  // bit 1
      dut.clock.step(1)

      dut.io.rxd.poke(true.B)   // bit 0
      dut.clock.step(1)

      dut.io.valid.expect(true.B)
      dut.io.data.expect("b10100101".U)
    
    }
  }

}


