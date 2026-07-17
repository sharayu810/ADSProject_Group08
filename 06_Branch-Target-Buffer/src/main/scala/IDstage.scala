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
    Handle flushes due to mispredicted branches

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
  val io = IO(new Bundle {
    val instr = Input(UInt(32.W)) //i/p instr from if stage
    val pc    = Input(UInt(32.W)) //i/p pc from if stage

    val regFileReq_A  = Output(new regFileReadReq) //Register File Read Request for rs1
    val regFileResp_A = Input(new regFileReadResp) //Register File Read Data for rs1

    val regFileReq_B  = Output(new regFileReadReq)
    val regFileResp_B = Input(new regFileReadResp)

    val uop          = Output(uopc()) //decoded micro operarion
    val rd           = Output(UInt(5.W)) // destination reg
    val rs1          = Output(UInt(5.W))  //my source reg1 addr
    val rs2          = Output(UInt(5.W))
    val operandA     = Output(UInt(32.W))  //alu operands
    val operandB     = Output(UInt(32.W))
    val outPC        = Output(UInt(32.W))  //forward pc
    val immI         = Output(UInt(32.W)) //i type imme value
    val immB         = Output(UInt(32.W)) //b type imme value
    val immJ         = Output(UInt(32.W)) //j type imme value
    val XcptInvalid  = Output(Bool())  //any exception
  })

  // ------------------------------------------------------------
  // Extract instruction fields
  // ------------------------------------------------------------
  val opcode = io.instr(6, 0)  //instr opcode
  val rd     = io.instr(11, 7)
  val funct3 = io.instr(14, 12) //func field for instr decoding
  val rs1    = io.instr(19, 15)
  val rs2    = io.instr(24, 20)
  val funct7 = io.instr(31, 25) //extended func field for instr decoding

  // ------------------------------------------------------------
  // Immediate generation
  // ------------------------------------------------------------

  // I-type immediate: used by ADDI, ANDI, ORI, XORI, SLTI, SLTIU, JALR
  val immI = io.instr(31, 20).asSInt.pad(32).asUInt

  // B-type immediate: used by BEQ, BNE, BLT, BGE, BLTU, BGEU
  val immB = Cat(
    Fill(19, io.instr(31)),  //Sign Extension (extends the sign bit to 32 bits)
    io.instr(31),  //Immediate Bit 12 (Sign Bit)
    io.instr(7),  //Immediate Bit 11
    io.instr(30, 25),  //Immediate Bits 10:5
    io.instr(11, 8),  //Immediate Bits 4:1
    0.U(1.W)
  )

  // J-type immediate: used by JAL
  // Generate J-Type (Jump) Immediate from the instruction
  val immJ = Cat(
    Fill(11, io.instr(31)),
    io.instr(31),
    io.instr(19, 12),
    io.instr(20),
    io.instr(30, 21),
    0.U(1.W)
  )

  // ------------------------------------------------------------
  // Register file read addresses
  // ------------------------------------------------------------
  io.regFileReq_A.addr := rs1  //Send rs1 address to Register File Read Port A
  io.regFileReq_B.addr := rs2

  // ------------------------------------------------------------
  // Default outputs
  // Important:
  // Start as invalid instruction.
  // For every valid instruction, we set XcptInvalid := false.B.
  // This avoids using default { } inside switch.
  // ------------------------------------------------------------

  //Initialize default outputs before instruction decoding
  io.uop         := uopc.NOP
  io.rd          := rd  //Output Destination Register
  io.rs1         := rs1
  io.rs2         := rs2
  io.operandA    := io.regFileResp_A.data //Read Operand A from Register File
  io.operandB    := io.regFileResp_B.data
  io.XcptInvalid := true.B  //Default Instruction as Invalid

  io.outPC := io.pc  //Forward Program Counter to EX Stage
  io.immI  := immI  //Output I-Type Immediate
  io.immB  := immB
  io.immJ  := immJ

  // ------------------------------------------------------------
  // Decode and generate ctrl signale
  // ------------------------------------------------------------
  switch(opcode) {

    // ----------------------------------------------------------
    // R-type instructions
    // opcode = 0110011
    // ----------------------------------------------------------
    is("b0110011".U) {
      switch(funct3) {

        is("b000".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.ADD
            io.XcptInvalid := false.B
          }.elsewhen(funct7 === "b0100000".U) {
            io.uop := uopc.SUB
            io.XcptInvalid := false.B
          }
        }

        is("b111".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.AND
            io.XcptInvalid := false.B
          }
        }

        is("b110".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.OR
            io.XcptInvalid := false.B
          }
        }

        is("b100".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.XOR
            io.XcptInvalid := false.B
          }
        }

        is("b001".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SLL
            io.XcptInvalid := false.B
          }
        }

        is("b101".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SRL
            io.XcptInvalid := false.B
          }.elsewhen(funct7 === "b0100000".U) {
            io.uop := uopc.SRA
            io.XcptInvalid := false.B
          }
        }

        is("b010".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SLT
            io.XcptInvalid := false.B
          }
        }

        is("b011".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SLTU
            io.XcptInvalid := false.B
          }
        }
      }
    }

    // ----------------------------------------------------------
    // I-type ALU instructions
    // opcode = 0010011
    // ----------------------------------------------------------
    is("b0010011".U) {
      io.operandB := immI
      io.rs2 := 0.U

      switch(funct3) {

        is("b000".U) {
          io.uop := uopc.ADDI
          io.XcptInvalid := false.B
        }

        is("b111".U) {
          io.uop := uopc.ANDI
          io.XcptInvalid := false.B
        }

        is("b110".U) {
          io.uop := uopc.ORI
          io.XcptInvalid := false.B
        }

        is("b100".U) {
          io.uop := uopc.XORI
          io.XcptInvalid := false.B
        }

        is("b001".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SLLI
            io.XcptInvalid := false.B
          }
        }

        is("b101".U) {
          when(funct7 === "b0000000".U) {
            io.uop := uopc.SRLI
            io.XcptInvalid := false.B
          }.elsewhen(funct7 === "b0100000".U) {
            io.uop := uopc.SRAI
            io.XcptInvalid := false.B
          }
        }

        is("b010".U) {
          io.uop := uopc.SLTI
          io.XcptInvalid := false.B
        }

        is("b011".U) {
          io.uop := uopc.SLTIU
          io.XcptInvalid := false.B
        }
      }
    }

    // ----------------------------------------------------------
    // B-type branch instructions
    // opcode = 1100011
    // ----------------------------------------------------------
    is("b1100011".U) {
      io.rd := 0.U

      switch(funct3) {
        is("b000".U) {
          io.uop := uopc.BEQ
          io.XcptInvalid := false.B
        }

        is("b001".U) {
          io.uop := uopc.BNE
          io.XcptInvalid := false.B
        }

        is("b100".U) {
          io.uop := uopc.BLT
          io.XcptInvalid := false.B
        }

        is("b101".U) {
          io.uop := uopc.BGE
          io.XcptInvalid := false.B
        }

        is("b110".U) {
          io.uop := uopc.BLTU
          io.XcptInvalid := false.B
        }

        is("b111".U) {
          io.uop := uopc.BGEU
          io.XcptInvalid := false.B
        }
      }
    }

    // ----------------------------------------------------------
    // JAL
    // opcode = 1101111
    // ----------------------------------------------------------
    is("b1101111".U) {
      io.uop := uopc.JAL
      io.rs1 := 0.U
      io.rs2 := 0.U
      io.operandA := immJ
      io.operandB := 0.U
      io.XcptInvalid := false.B
    }

    // ----------------------------------------------------------
    // JALR
    // opcode = 1100111
    // funct3 must be 000
    // ----------------------------------------------------------
    is("b1100111".U) {
      when(funct3 === "b000".U) {
        io.uop := uopc.JALR
        io.rs2 := 0.U
        io.operandB := immI
        io.XcptInvalid := false.B
      }
    }

    // ----------------------------------------------------------
    // NOP / flushed instruction
    // instruction = 0x00000000
    // ----------------------------------------------------------
    is("b0000000".U) {
      io.uop := uopc.NOP
      io.rd := 0.U
      io.rs1 := 0.U
      io.rs2 := 0.U
      io.operandA := 0.U
      io.operandB := 0.U
      io.XcptInvalid := false.B
    }
  }
}