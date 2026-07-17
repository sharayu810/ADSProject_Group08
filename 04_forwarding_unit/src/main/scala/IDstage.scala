// ADS I Class Project
// Pipelined RISC-V Core - ID Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
Instruction Decode (ID) Stage: decoding and operand fetch

Extracted Fields from 32-bit Instruction (see RISC-V specification for reference):
    opcode: instruction format identifier
    funct3: selects variant within instruction format
    funct7: further specifies operation type (R-type only)
    rd: destination register address
    rs1: first source register address
    rs2: second source register address
    imm: 12-bit immediate value (I-type, sign-extended)

Register File Interfaces:
    regFileReq_A, regFileResp_A: read port for rs1 operand
    regFileReq_B, regFileResp_B: read port for rs2 operand

Internal Signals:
    Combinational decoders for instructions

Functionality:
    Decode opcode to determine instruction and identify operation (ADD, SUB, XOR, ...)
    Output: uop (operation code), rd, operandA (from rs1), operandB (rs2 or immediate)

Outputs:
    uop: micro-operation code (identifies instruction type)
    rd: destination register index
    operandA: first operand
    operandB: second operand 
    XcptInvalid: exception flag for invalid instructions
*/

package core_tile

import chisel3._
import chisel3.util._
import uopc._

// -----------------------------------------
// Decode Stage
// -----------------------------------------

//ToDo: Add your implementation according to the specification above here 
class ID extends Module {
    val io = IO(new Bundle{
    val instr         = Input(UInt(32.W))
    val regFileReq_A  = Output(new regFileReadReq)
    val regFileResp_A = Input(new regFileReadResp)
    val regFileReq_B  = Output(new regFileReadReq)
    val regFileResp_B = Input(new regFileReadResp)
    val uop           = Output(uopc())
    val rd            = Output(UInt(5.W))
    val rs1           = Output(UInt(5.W))   // NEW: needed by forwarding unit
    val rs2           = Output(UInt(5.W))   // NEW: 0 for I-type
    val operandA      = Output(UInt(32.W))
    val operandB      = Output(UInt(32.W))
    val xcptInvalid   = Output(Bool()) 
   })
  val instr  = io.instr
  val opcode = instr(6, 0)
  val rd     = instr(11, 7)
  val funct3 = instr(14, 12)
  val rs1    = instr(19, 15)
  val rs2    = instr(24, 20)
  val funct7 = instr(31, 25)                       // funct7(5) == instr(30)
  val imm    = Cat(Fill(20, instr(31)), instr(31, 20))  // sign-extended I-immediate

  io.regFileReq_A.addr := rs1
  io.regFileReq_B.addr := rs2

  val isR  = opcode === "b0110011".U
  val isI  = opcode === "b0010011".U
  val uop  = WireDefault(uopc.isNOP)
  val xcpt = WireDefault(false.B)

  when(isR) {
    switch(funct3) {
      is("b000".U) { uop := Mux(funct7(5), uopc.isSUB, uopc.isADD) }
      is("b001".U) { uop := uopc.isSLL }
      is("b010".U) { uop := uopc.isSLT }
      is("b011".U) { uop := uopc.isSLTU }
      is("b100".U) { uop := uopc.isXOR }
      is("b101".U) { uop := Mux(funct7(5), uopc.isSRA, uopc.isSRL) }
      is("b110".U) { uop := uopc.isOR }
      is("b111".U) { uop := uopc.isAND }
    }
  } .elsewhen(isI) {
    switch(funct3) {
      is("b000".U) { uop := uopc.isADDI }
      is("b010".U) { uop := uopc.isSLTI }
      is("b011".U) { uop := uopc.isSLTIU }
      is("b100".U) { uop := uopc.isXORI }
      is("b110".U) { uop := uopc.isORI }
      is("b111".U) { uop := uopc.isANDI }
      is("b001".U) { uop := uopc.isSLLI }
      is("b101".U) { uop := Mux(funct7(5), uopc.isSRAI, uopc.isSRLI) }
    }
  } .otherwise {
    xcpt := true.B                                  // unknown opcode
  }

  io.uop         := uop
  io.rd          := Mux(xcpt, 0.U, rd)
  io.rs1         := rs1
  io.rs2         := Mux(isR, rs2, 0.U)   // I-type has no rs2 -> 0 prevents false forwarding into the immediate
  io.operandA    := io.regFileResp_A.data
  io.operandB    := Mux(isR, io.regFileResp_B.data, imm)
  io.xcptInvalid := xcpt
}
