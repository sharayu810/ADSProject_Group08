// ADS I Class Project
// Pipelined RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/15/2023 by Tobias Jauch (@tojauch)

/*
The goal of this task is to implement a 5-stage pipeline that features a subset of RV32I (all R-type and I-type instructions). 

    Instruction Memory:
        The CPU has an instruction memory (IMem) with 4096 words, each of 32 bits.
        The content of IMem is loaded from a binary file specified during the instantiation of the MultiCycleRV32Icore module.

    CPU Registers:
        The CPU has a program counter (PC) and a register file (regFile) with 32 registers, each holding a 32-bit value.
        Register x0 is hard-wired to zero.

    Microarchitectural Registers / Wires:
        Various signals are defined as either registers or wires depending on whether they need to be used in the same cycle or in a later cycle.

    Processor Stages:
        The FSM of the processor has five stages: fetch, decode, execute, memory, and writeback.
        All stages are active at the same time and process different instructions simultaneously.

        Fetch Stage:
            The instruction is fetched from the instruction memory based on the current value of the program counter (PC).

        Decode Stage:
            Instruction fields such as opcode, rd, funct3, and rs1 are extracted.
            For R-type instructions, additional fields like funct7 and rs2 are extracted.
            Control signals (isADD, isSUB, etc.) are set based on the opcode and funct3 values.
            Operands (operandA and operandB) are determined based on the instruction type.

        Execute Stage:
            Arithmetic and logic operations are performed based on the control signals and operands.
            The result is stored in the aluResult register.

        Memory Stage:
            No memory operations are implemented in this basic CPU.

        Writeback Stage:
            The result of the operation (writeBackData) is written back to the destination register (rd) in the register file.

    Check Result:
        The final result (writeBackData) is output to the io.check_res signal.
        The exception signal is also passed to the wrapper module. It indicates whether an invalid instruction has been encountered.
        In the fetch stage, a default value of 0 is assigned to io.check_res.
*/

package core_tile

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import Assignment02.{ALU, ALUOp}
import uopc._


class PipelinedRV32Icore (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    //ToDo: Add I/O ports
    val check_res = Output(UInt(32.W))
    val exception = Output(Bool())

  })

//ToDo: Add your implementation according to the specification above here 
  val fetch  = Module(new IF(BinaryFile))
  val ifBar  = Module(new IFBarrier)
  val decode = Module(new ID)
  val idBar  = Module(new IDBarrier)
  val exec   = Module(new EX)
  val exBar  = Module(new EXBarrier)
  val mem    = Module(new MEM)
  val memBar = Module(new MEMBarrier)
  val wb     = Module(new WB)
  val wbBar  = Module(new WBBarrier)
  val rf     = Module(new regFile)
  val fwd    = Module(new ForwardingUnit)   // NEW

  // IF -> IF/ID -> ID
  ifBar.io.inInstr := fetch.io.instr
  decode.io.instr  := ifBar.io.outInstr

  // ID <-> regFile read ports
  rf.io.req_1 := decode.io.regFileReq_A
  rf.io.req_2 := decode.io.regFileReq_B
  decode.io.regFileResp_A := rf.io.resp_1
  decode.io.regFileResp_B := rf.io.resp_2

  // ID -> ID/EX -> EX
  idBar.io.inUOP         := decode.io.uop
  idBar.io.inRD          := decode.io.rd
  idBar.io.inRS1         := decode.io.rs1     // NEW
  idBar.io.inRS2         := decode.io.rs2     // NEW
  idBar.io.inOperandA    := decode.io.operandA
  idBar.io.inOperandB    := decode.io.operandB
  idBar.io.inXcptInvalid := decode.io.xcptInvalid

  exec.io.uop         := idBar.io.outUOP
  exec.io.rd          := idBar.io.outRD
  exec.io.operandA    := idBar.io.outOperandA
  exec.io.operandB    := idBar.io.outOperandB
  exec.io.xcptInvalid := idBar.io.outXcptInvalid

  // Forwarding unit
  fwd.io.rs1_EX   := idBar.io.outRS1
  fwd.io.rs2_EX   := idBar.io.outRS2
  fwd.io.rd_MEM   := exBar.io.outRD
  fwd.io.rd_WB    := memBar.io.outRD
  fwd.io.wrEn_MEM := true.B           // every R/I-type writes back; x0 filtered inside the unit
  fwd.io.wrEn_WB  := true.B
  exec.io.forwardA := fwd.io.forwardA
  exec.io.forwardB := fwd.io.forwardB
  exec.io.fwdEXMEM := exBar.io.outAluResult
  exec.io.fwdMEMWB := memBar.io.outAluResult

  // EX -> EX/MEM -> (MEM empty) -> MEM/WB
  exBar.io.inAluResult   := exec.io.aluResult
  exBar.io.inRD          := exec.io.rd_out
  exBar.io.inXcptInvalid := exec.io.xcptInvalid_out

  memBar.io.inAluResult := exBar.io.outAluResult
  memBar.io.inRD        := exBar.io.outRD
  memBar.io.inException := exBar.io.outXcptInvalid

  // MEM/WB -> WB -> regFile write
  wb.io.aluResult := memBar.io.outAluResult
  wb.io.rd        := memBar.io.outRD
  rf.io.req_3     := wb.io.regFileReq

  // WB -> WB/Barrier -> outputs
  wbBar.io.inCheckRes    := wb.io.check_res
  wbBar.io.inXcptInvalid := memBar.io.outException

  io.check_res := wbBar.io.outCheckRes
  io.exception := wbBar.io.outXcptInvalid
}
