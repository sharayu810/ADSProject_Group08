// ADS I Class Project
// Pipelined RISC-V Core - WB Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)


/*
Writeback (WB) Stage: result storage and register file updates

Register File Interface:
    regFileReq: write request bundle
        regFileReq.addr: destination register index
        regFileReq.data: result value to write
        regFileReq.wr_en: write enable signal

Inputs:
    aluResult: computation result from pipeline
    rd: destination register address

Internal Signals:
    Result forwarding paths
    Write enable control

Functionality:
    Forward aluResult to register file write port
    Set write address to rd
    Assert wr_en = true for all R-type and I-type instructions
    Output result on check_res for verification and debugging

Outputs:
    check_res: result value for verification
*/

package core_tile

import chisel3._

// -----------------------------------------
// Writeback Stage
// -----------------------------------------

//ToDo: Add your implementation according to the specification above here 
class WB extends Module {
  val io = IO(new Bundle {
    val aluResult     = Input(UInt(32.W))  //Receives the result computed by the ALU.
    val rd            = Input(UInt(5.W))  //Destination register number
    val inXcptInvalid = Input(Bool())

    val regFileReq = Output(new regFileWriteReq)

    val check_res = Output(UInt(32.W))
    val exception = Output(Bool())
  })

  io.regFileReq.addr  := io.rd                        //Where to write

  io.regFileReq.data  := io.aluResult                 //What to write

  io.regFileReq.wr_en := !io.inXcptInvalid            //Writes if there is no exception

  io.check_res := io.aluResult                        //check_res: result value for verification
  io.exception := io.inXcptInvalid
}