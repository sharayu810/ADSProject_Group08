// ToDo: Add your ALU implementation from Assignment02 here

package Assignment02

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

//ToDo: define AluOp Enum
object ALUOp extends ChiselEnum {
  val ADD, SUB, AND, OR, XOR, SLL, SRL, SRA, SLT, SLTU, PASSB = Value
}

class ALU extends Module {
  
  val io = IO(new Bundle {
    //ToDo: define IOs
    val operandA  = Input(UInt(32.W))
    val operandB  = Input(UInt(32.W))
    val operation = Input(ALUOp())
    val aluResult = Output(UInt(32.W))
  })

  //ToDo: implement ALU functionality according to the task specification
  // Default output: 0 for any undefined/unspecified operation
  io.aluResult := 0.U

   
  switch(io.operation) {
 
    // ADD: two's-complement addition, modulo-2^32 wraparound
    is(ALUOp.ADD) {
      io.aluResult := io.operandA + io.operandB
    }
 
    // SUB: two's-complement subtraction, modulo-2^32 wraparound
    is(ALUOp.SUB) {
      io.aluResult := io.operandA - io.operandB
    }
 
    // AND: bitwise logical AND
    is(ALUOp.AND) {
      io.aluResult := io.operandA & io.operandB
    }
 
    // OR: bitwise logical OR
    is(ALUOp.OR) {
      io.aluResult := io.operandA | io.operandB
    }
 
    // XOR: bitwise logical XOR
    is(ALUOp.XOR) {
      io.aluResult := io.operandA ^ io.operandB
    }
 
    // SLL: shift left logical — only low 5 bits of operandB used (RV32 semantics)
    is(ALUOp.SLL) {
      io.aluResult := io.operandA << io.operandB(4, 0)
    }
 
    // SRL: shift right logical (zero-fill) — only low 5 bits of operandB used
    is(ALUOp.SRL) {
      io.aluResult := io.operandA >> io.operandB(4, 0)
    }
 
    // SRA: shift right arithmetic (sign-extend) — operandA treated as signed
    is(ALUOp.SRA) {
      io.aluResult := (io.operandA.asSInt >> io.operandB(4, 0)).asUInt
    }
 
    // SLT: set less than (signed) — result is 1 if A < B signed, else 0
    is(ALUOp.SLT) {
      io.aluResult := Mux(io.operandA.asSInt < io.operandB.asSInt, 1.U, 0.U)
    }
 
    // SLTU: set less than unsigned — result is 1 if A < B unsigned, else 0
    is(ALUOp.SLTU) {
      io.aluResult := Mux(io.operandA < io.operandB, 1.U, 0.U)
    }
 
    // PASSB: pass operandB unchanged to output (used e.g. for LUI)
    is(ALUOp.PASSB) {
      io.aluResult := io.operandB
    }
  }
}