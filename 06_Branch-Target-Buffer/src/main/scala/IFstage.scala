// ADS I Class Project
// Pipelined RISC-V Core - IF Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
The Instruction Fetch (IF) stage is the first stage of the pipeline and handles instruction retrieval from memory.

Memory:
    IMem: instruction memory with 4096 32-bit unsigned integer entires, loaded from a binary file at compile time

Internal Registers:
    PC: 32-bit unsigned integer register, initialized to 0 holding the current program counter address

Internal Signals:
    none

Functionality:
    Fetch the instruction at the current PC (word-aligned addressing)
    Increment the PC (word-aligned) each clock cycle to fetch the next sequential instruction
    Handle flushes due to mispredicted branches

Parameters:
    BinaryFile: String - path to the binary file to load into instruction memory

Inputs:
    none

Outputs:
    instr: send the fetched instruction to IF Barrier
*/

package core_tile

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile

// -----------------------------------------
// Fetch Stage
// -----------------------------------------

class IF(BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val redirect       = Input(Bool())  //says that The prediction was wrong, and the processor flushes wrong path and jumps to correct address ie  Indicates whether the PC should be redirected.
    val redirectPC     = Input(UInt(32.W)) //correct PC after Misprediction

    val predictTaken   = Input(Bool())  //if true , if stage jumps immediately 
    val predictedTarget = Input(UInt(32.W))  //address predicted by btb

    val instr          = Output(UInt(32.W))  //outputs the fetched instruction
    val pc             = Output(UInt(32.W))  //outputs the current pc

    val outPredictTaken = Output(Bool()) //passes the predicted info to next stage 
    val outPredictedTarget = Output(UInt(32.W))  //also forwarded to later stages
  })

  val PC = RegInit(0.U(32.W))  // Read the instruction at the current Program Counter (PC).
  val IMem = Mem(4096, UInt(32.W))  //creates instruction mem
  loadMemoryFromFile(IMem, BinaryFile) //loads binaryfile to imem , now program can fetch instructions

  io.instr := IMem(PC(13, 2))
  io.pc := PC  //sned current pc to next stage
  io.outPredictTaken := io.predictTaken //pass to next stage
  io.outPredictedTarget := io.predictedTarget //pass

//// Update PC: redirect to the correct PC on misprediction; otherwise jump to the BTB-predicted target if the branch is predicted taken.
  when(io.redirect) {
    PC := io.redirectPC
  }.elsewhen(io.predictTaken) {
    PC := io.predictedTarget
  }.otherwise {
    PC := PC + 4.U //if: no redirect or no prediction , execute normally
  }
}

