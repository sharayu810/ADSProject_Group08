// ADS I Class Project
// Pipelined RISC-V Core - Forwarding Unit
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/09/2026 by Tobias Jauch (@tojauch)

/*
Forwarding Unit: resolves data hazards by forwarding results from later pipeline stages to the ID stage

Functionality (cf. slide 6-24ff of the lecture slides):
    Detects data hazards by comparing source registers in the EX stage with destination registers in MEM and WB stages (EX and MEM barriers).
    Generates control signals for the multiplexers in the EX stage to select the correct data source for the ALU inputs
    Handles cases where multiple hazards occur simultaneously (e.g., forwarding from both MEM and WB stages)

Inputs:
    rs1_EX: source register 1 in EX stage
    rs2_EX: source register 2 in EX stage
    rd_MEM: destination register in MEM stage
    rd_WB: destination register in WB stage
    wrEn_MEM: write enable signal for MEM stage
    wrEn_WB: write enable signal for WB stage

Outputs:
    forwardA: control signal for selecting source of operand A in EX stage
    forwardB: control signal for selecting source of operand B in EX stage

*/

package core_tile

import chisel3._
import chisel3.util._
import uopc._

// -----------------------------------------
// Forwarding Unit
// -----------------------------------------

class ForwardingUnit extends Module {
  val io = IO(new Bundle {
    // Add I/O ports according to the specification above here
    val rs1_EX   = Input(UInt(5.W))     // rs1 of instruction now in EX
    val rs2_EX   = Input(UInt(5.W))     // rs2 of instruction now in EX (0 for I-type)
    val rd_MEM   = Input(UInt(5.W))    // rd held in EX/MEM barrier (exBar)
    val rd_WB    = Input(UInt(5.W))    // rd held in MEM/WB barrier (memBar)
    val wrEn_MEM = Input(Bool())       // EX/MEM instruction writes back?
    val wrEn_WB  = Input(Bool())       // MEM/WB instruction writes back?
    val forwardA = Output(UInt(2.W))   // operand A select
    val forwardB = Output(UInt(2.W))   // operand B select
  })

  //ToDo: Add your implementation according to the specification above here 
   // Encoding (Patterson-Hennessy): 00 = regfile, 10 = from EX/MEM, 01 = from MEM/WB.
  // *** If your slide 6-24 uses a different encoding, change these constants. ***
  io.forwardA := "b00".U
  io.forwardB := "b00".U

  // MEM/WB checked first; EX/MEM checked second so it overrides (it is the newer result).
  when(io.wrEn_WB  && io.rd_WB  =/= 0.U && io.rd_WB  === io.rs1_EX) { io.forwardA := "b01".U }
  when(io.wrEn_MEM && io.rd_MEM =/= 0.U && io.rd_MEM === io.rs1_EX) { io.forwardA := "b10".U }

  when(io.wrEn_WB  && io.rd_WB  =/= 0.U && io.rd_WB  === io.rs2_EX) { io.forwardB := "b01".U }
  when(io.wrEn_MEM && io.rd_MEM =/= 0.U && io.rd_MEM === io.rs2_EX) { io.forwardB := "b10".U }
  

}