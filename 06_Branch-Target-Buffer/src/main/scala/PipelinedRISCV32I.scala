// ADS I Class Project
// Pipelined RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/10/2023 by Tobias Jauch (@tojauch)

/*
This file contains the top-level module for the Pipelined RISC-V 32I core and acts as the interface between the core and external testbenches.
*/

package PipelinedRV32I

import chisel3._
import chisel3.util._

import core_tile._

class PipelinedRV32I (BinaryFile: String, enableBTB: Boolean = true) extends Module {

val io = IO(new Bundle {
  val result    = Output(UInt(32.W)) 
  val exception = Output(Bool())
  // Performance counters are observational only and are used by the Assignment 6 benchmarks.
  val cycles = Output(UInt(32.W))
  val branches = Output(UInt(32.W))
  val correctPredictions = Output(UInt(32.W))
  val incorrectPredictions = Output(UInt(32.W))
 })
  
  val core = Module(new PipelinedRV32Icore(BinaryFile, enableBTB))

  io.result    := core.io.check_res
  io.exception := core.io.exception
  io.cycles := core.io.cycles
  io.branches := core.io.branches
  io.correctPredictions := core.io.correctPredictions
  io.incorrectPredictions := core.io.incorrectPredictions

}
