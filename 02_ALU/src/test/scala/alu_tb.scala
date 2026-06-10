// ADS I Class Project
// Pipelined RISC-V Core with Hazard Detection and Resolution
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 10/31/2025 by Tobias Jauch (tobias.jauch@rptu.de)

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import Assignment02._

// Test ADD operation
class ALUAddTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Add_Tester" should "test ADD operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      dut.io.operandA.poke(10.U)
      dut.io.operandB.poke(10.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect(20.U)
      dut.clock.step(1)

      //ToDo: add more test cases for ADD operation
      //Corner case: 0 + 0 = 0
      dut.io.operandA.poke(0.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)

      //Corner case : 255 + 0 = 255
      dut.io.operandA.poke(255.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect(255.U)
      dut.clock.step(1)

      //Corner Case : 255 + 1 = 256(No Overflow)
      dut.io.operandA.poke(255.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect(256.U)
      dut.clock.step(1)

      //Corner case : 255 + 255 = 510(No overflow)
      dut.io.operandA.poke(255.U)
      dut.io.operandB.poke(255.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect(510.U)
      dut.clock.step(1)

      // Corner case: overflow wraparound — 4294967295 + 1 = 0
      // 4294967295 = 0xFFFFFFFF = max 32-bit value
      dut.io.operandA.poke(4294967295L.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)
    }
  }
}

// ---------------------------------------------------
// ToDo: Add test classes for all other ALU operations
//---------------------------------------------------

//Test SUB Operation
class ALUSUBTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_SUB_Tester" should "test SUB operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      //Basic case
      dut.io.operandA.poke(10.U)
      dut.io.operandB.poke(5.U)
      dut.io.operation.poke(ALUOp.SUB)
      dut.io.aluResult.expect(5.U)
      dut.clock.step(1)

      //Equal Operand : Result 0 
      dut.io.operandA.poke(10.U)
      dut.io.operandB.poke(10.U)
      dut.io.operation.poke(ALUOp.SUB)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)

 
      // Corner case: underflow wraparound  0 - 1 = 4294967295
      // This is a 32-bit ALU — underflow wraps to 0xFFFFFFFF = 4294967295
      // NOT 255 (that would be 8-bit)
      dut.io.operandA.poke(0.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SUB)
      dut.io.aluResult.expect(4294967295L.U)
      dut.clock.step(1)

      //Corner case : Underflow 
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(0xFFFFFFFFL.U)
      dut.io.operation.poke(ALUOp.SUB)
      dut.io.aluResult.expect(2.U)

    }
  }  
 }


//Test AND Operation
class ALUAndTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_And_Tester" should "test AND operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      //Basic AND 
      dut.io.operandA.poke(0xFF00FF00L.U)
      dut.io.operandB.poke(0x0F0F0F0FL.U)
      dut.io.operation.poke(ALUOp.AND)
      dut.io.aluResult.expect(0x0F000F00L.U)
      dut.clock.step(1)

      //AND with all ones : Identity Property
      dut.io.operandA.poke(0xABCDEF12L.U)
      dut.io.operandB.poke(0xFFFFFFFFL.U)
      dut.io.operation.poke(ALUOp.AND)
      dut.io.aluResult.expect(0xABCDEF12L.U)
      dut.clock.step(1)

      //Corner case :AND with Zero
      dut.io.operandA.poke(0xFFFFFFFFL.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.AND)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)
      
      //Corner Case : Opposite Bit Pattern
      dut.io.operandA.poke(0xAAAAAAAAL.U)
      dut.io.operandB.poke(0x55555555L.U)
      dut.io.operation.poke(ALUOp.AND)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)

      //Property check :AND with itself
      dut.io.operandA.poke(0xDEADBEEFL.U)
      dut.io.operandB.poke(0xDEADBEEFL.U)
      dut.io.operation.poke(ALUOp.AND)
      dut.io.aluResult.expect(0xDEADBEEFL.U)
      dut.clock.step(1)

    }
  }
}

//Test OR Operation
class ALUOrTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Or_Tester" should "test OR operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      //Basic OR
      dut.io.operandA.poke(0xFF00FF00L.U)
      dut.io.operandB.poke(0x00FF00FFL.U)
      dut.io.operation.poke(ALUOp.OR)
      dut.io.aluResult.expect(0xFFFFFFFFL.U)
      dut.clock.step(1)

      //Corner Case : Identity element
      dut.io.operandA.poke(0xABCDEF12L.U)
      dut.io.operandB.poke(0.U)              // ← ZERO is the corner
      dut.io.operation.poke(ALUOp.OR)
      dut.io.aluResult.expect(0xABCDEF12L.U)
      dut.clock.step(1)

      //Corner Case : all ones 
      dut.io.operandA.poke(0.U)
      dut.io.operandB.poke(0xFFFFFFFFL.U)   // ← ALL ONES is the corner
      dut.io.operation.poke(ALUOp.OR)
      dut.io.aluResult.expect(0xFFFFFFFFL.U)
      dut.clock.step(1)

      //Corenr case : same value both side
      dut.io.operandA.poke(0xDEADBEEFL.U)
      dut.io.operandB.poke(0xDEADBEEFL.U)   // ← SAME VALUE is the corner
      dut.io.operation.poke(ALUOp.OR)
      dut.io.aluResult.expect(0xDEADBEEFL.U)
      dut.clock.step(1)

      //Corner case : both operands zero
      dut.io.operandA.poke(0.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.OR)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)

      //Corner Case : single bit set -only one bit should apper in result
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(2.U)
      dut.io.operation.poke(ALUOp.OR)
      dut.io.aluResult.expect(3.U)
      dut.clock.step(1)
    }
  }
}

//Test XOR Oepration
class ALUXorTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Xor_Tester" should "test XOR operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      //Basic XOR
      dut.io.operandA.poke(0xFF00FF00L.U)
      dut.io.operandB.poke(0x0F0F0F0FL.U)
      dut.io.operation.poke(ALUOp.XOR)
      dut.io.aluResult.expect(0xF00FF00FL.U)
      dut.clock.step(1)

      //Corner case: XOR with self : always zero
      dut.io.operandA.poke(0xABCDEF12L.U)
      dut.io.operandB.poke(0xABCDEF12L.U)
      dut.io.operation.poke(ALUOp.XOR)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)
 
      //Corner Case : identity - result equals operandA
      dut.io.operandA.poke(0xABCDEF12L.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.XOR)
      dut.io.aluResult.expect(0xABCDEF12L.U)
      dut.clock.step(1)

      //Corner Case : XOR with all ones: bitwise NOT 
      dut.io.operandA.poke(0xABCDEF12L.U)
      dut.io.operandB.poke(0xFFFFFFFFL.U)
      dut.io.operation.poke(ALUOp.XOR)
      dut.io.aluResult.expect(0x543210EDL.U)
      dut.clock.step(1)
 
    }
  }
}

//Test SLL Operation (Only low 5 bits of operandB used as shift amount (RV32 semantics))
class ALUSllTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Sll_Tester" should "test SLL operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      //Basic Shift left by 1
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect(2.U)
      dut.clock.step(1) 

      //Shift left by 4
      dut.io.operandA.poke(0x00000001L.U)
      dut.io.operandB.poke(4.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect(0x00000010L.U)
      dut.clock.step(1)

      // shift by 0: identity — result unchanged
      dut.io.operandA.poke(0xABCDEF12L.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect(0xABCDEF12L.U)
      dut.clock.step(1)
 
      // corner case: shift by 31 — only MSB survives
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(31.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect(0x80000000L.U)
      dut.clock.step(1)
 
      // corner case: only low 5 bits of operandB matter
      // operandB = 0x20 = 32 → low 5 bits = 0 → shift by 0, result unchanged
      dut.io.operandA.poke(0xABCDEF12L.U)
      dut.io.operandB.poke(0x20.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect(0xABCDEF12L.U)
      dut.clock.step(1)

    }
  }
}

//Test SRL Operation - zero-fill from MSB (Only low 5 bits of operandB used as shift amount (RV32 semantics))
class ALUSrlTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Srl_Tester" should "test SRL operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
 
      // basic shift right logical: MSB filled with 0
      dut.io.operandA.poke(0x80000000L.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SRL)
      dut.io.aluResult.expect(0x40000000L.U)
      dut.clock.step(1)
 
      // shift by 0: identity — result unchanged
      dut.io.operandA.poke(0xABCDEF12L.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.SRL)
      dut.io.aluResult.expect(0xABCDEF12L.U)
      dut.clock.step(1)
 
      // shift by 31: only bit 31 remains as bit 0
      dut.io.operandA.poke(0x80000000L.U)
      dut.io.operandB.poke(31.U)
      dut.io.operation.poke(ALUOp.SRL)
      dut.io.aluResult.expect(1.U)
      dut.clock.step(1)
 
      // corner case: only low 5 bits of operandB matter
      // operandB = 0x20 = 32 → low 5 bits = 0 → shift by 0, result unchanged
      dut.io.operandA.poke(0xABCDEF12L.U)
      dut.io.operandB.poke(0x20.U)
      dut.io.operation.poke(ALUOp.SRL)
      dut.io.aluResult.expect(0xABCDEF12L.U)
      dut.clock.step(1)
    }
  }
}

// Test Operation SRA — shift right arithmetic (sign-extend from MSB)
// operandA is interpreted as signed; only low 5 bits of operandB used 
class ALUSraTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Sra_Tester" should "test SRA operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
 
      // positive number (MSB=0): SRA behaves like SRL
      dut.io.operandA.poke(0x40000000L.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SRA)
      dut.io.aluResult.expect(0x20000000L.U)
      dut.clock.step(1)
 
      // corner case: negative number (MSB=1): sign bit propagated
      dut.io.operandA.poke(0x80000000L.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SRA)
      dut.io.aluResult.expect(0xC0000000L.U)
      dut.clock.step(1)
 
      // shift by 0: identity — result unchanged
      dut.io.operandA.poke(0xABCDEF12L.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.SRA)
      dut.io.aluResult.expect(0xABCDEF12L.U)
      dut.clock.step(1)
 
      // corner case: shift most-negative value (0x80000000) by 31 → all ones
      dut.io.operandA.poke(0x80000000L.U)
      dut.io.operandB.poke(31.U)
      dut.io.operation.poke(ALUOp.SRA)
      dut.io.aluResult.expect(0xFFFFFFFFL.U)
      dut.clock.step(1)
 
      // shift all-ones (-1 signed) by any amount → stays all-ones
      dut.io.operandA.poke(0xFFFFFFFFL.U)
      dut.io.operandB.poke(16.U)
      dut.io.operation.poke(ALUOp.SRA)
      dut.io.aluResult.expect(0xFFFFFFFFL.U)
      dut.clock.step(1)
    }
  }
}

//Test Operation SLT — set less than (signed comparison)
// Result = 1 if operandA < operandB (signed), else 0 (zero-extended to 32 bits)
class ALUSltTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Slt_Tester" should "test SLT operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
 
      // A < B signed → 1
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(2.U)
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(1.U)
      dut.clock.step(1)
 
      // A > B signed → 0
      dut.io.operandA.poke(2.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)
 
      // A == B → 0
      dut.io.operandA.poke(5.U)
      dut.io.operandB.poke(5.U)
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)
 
      // corner case: 0x80000000 = -2147483648 (most negative signed) < 0 → 1
      dut.io.operandA.poke(0x80000000L.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(1.U)
      dut.clock.step(1)
 
      // corner case: 0 < 0x80000000 (-2147483648 signed) → 0
      dut.io.operandA.poke(0.U)
      dut.io.operandB.poke(0x80000000L.U)
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)
 
      // corner case: -1 (0xFFFFFFFF) < 0 signed → 1
      dut.io.operandA.poke(0xFFFFFFFFL.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(1.U)
      dut.clock.step(1)
    }
  }
}

// Test Operation SLTU — set less than unsigned
// Result = 1 if operandA < operandB (unsigned), else 0
class ALUSltuTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Sltu_Tester" should "test SLTU operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
 
      // A < B unsigned → 1
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(2.U)
      dut.io.operation.poke(ALUOp.SLTU)
      dut.io.aluResult.expect(1.U)
      dut.clock.step(1)
 
      // A > B unsigned → 0
      dut.io.operandA.poke(2.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SLTU)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)
 
      // A == B → 0
      dut.io.operandA.poke(5.U)
      dut.io.operandB.poke(5.U)
      dut.io.operation.poke(ALUOp.SLTU)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)
 
      // corner case: 0 < 0x80000000 unsigned (large positive) → 1
      // NOTE: differs from SLT where 0 > 0x80000000 (signed)
      dut.io.operandA.poke(0.U)
      dut.io.operandB.poke(0x80000000L.U)
      dut.io.operation.poke(ALUOp.SLTU)
      dut.io.aluResult.expect(1.U)
      dut.clock.step(1)
 
      // corner case: 0xFFFFFFFF is the largest unsigned — not less than anything smaller
      dut.io.operandA.poke(0xFFFFFFFFL.U)
      dut.io.operandB.poke(0x80000000L.U)
      dut.io.operation.poke(ALUOp.SLTU)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)
 
      // corner case: 0 < 1 unsigned → 1
      dut.io.operandA.poke(0.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SLTU)
      dut.io.aluResult.expect(1.U)
      dut.clock.step(1)
    }
  }
}

//Test PASSB — pass operandB unchanged to output
// operandA is completely ignored
class ALUPassbTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Passb_Tester" should "test PASSB operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
 
      // basic passthrough
      dut.io.operandA.poke(0xDEADBEEFL.U)
      dut.io.operandB.poke(0x12345678L.U)
      dut.io.operation.poke(ALUOp.PASSB)
      dut.io.aluResult.expect(0x12345678L.U)
      dut.clock.step(1)
 
      // operandA is ignored: different operandA, same operandB → same result
      dut.io.operandA.poke(0xFFFFFFFFL.U)
      dut.io.operandB.poke(0x12345678L.U)
      dut.io.operation.poke(ALUOp.PASSB)
      dut.io.aluResult.expect(0x12345678L.U)
      dut.clock.step(1)
 
      // operandB = 0 → result = 0
      dut.io.operandA.poke(0xFFFFFFFFL.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.PASSB)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)
 
      // operandB = all ones → result = all ones
      dut.io.operandA.poke(0.U)
      dut.io.operandB.poke(0xFFFFFFFFL.U)
      dut.io.operation.poke(ALUOp.PASSB)
      dut.io.aluResult.expect(0xFFFFFFFFL.U)
      dut.clock.step(1)
    }
  }
}
 

