// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package readserial

import chisel3._
import chisel3.util._


/** controller class */
class Controller extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */
    val rxd    = Input(Bool())
    val done   = Input(Bool())   // from counter
    val cnt_s  = Output(Bool())  // counter reset
    val cnt_en = Output(Bool())  // counter / shift enable
    val valid  = Output(Bool())  // data ready pulse
    })

  // internal variables
  /* 
   * TODO: Define internal variables (registers and/or wires), if needed
   */
  val idle :: receiving :: Nil = Enum(2)
  val state = RegInit(idle)  

  // state machine
  /* 
   * TODO: Describe functionality if the controller as a state machine
   */
  io.cnt_s  := false.B
  io.cnt_en := false.B
  io.valid  := false.B

  switch(state){
    is(idle) {
      io.cnt_s := true.B     
      when(!io.rxd) {             
        state := receiving
      }
    }
    is(receiving) {       
      when(io.done) {
        io.valid := true.B
        io.cnt_s := true.B      
        when(!io.rxd) {
          state := receiving    
        }.otherwise {
          state  := idle
        }
      }.otherwise{
        io.cnt_en := true.B  
      }
    }
  }

}


/** counter class */
class Counter extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */
    val cnt_s  = Input(Bool())   
    val cnt_en = Input(Bool())   
    val done   = Output(Bool())  
    })

  // internal variables
  /* 
   * TODO: Define internal variables (registers and/or wires), if needed
   */
   val count = RegInit(0.U(4.W))
   val doneReg = RegInit(false.B)  
  // state machine
  /* 
   * TODO: Describe functionality if the counter as a state machine
   */
  when(io.cnt_s) {
    count := 0.U
    doneReg := false.B
  }.elsewhen(io.cnt_en) {
    count := count + 1.U
    doneReg := (count === 7.U)
  }.otherwise {
    doneReg := false.B
  }
 
  io.done := doneReg
}

/** shift register class */
class ShiftRegister extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */
    val rxd    = Input(Bool())         
    val en     = Input(Bool())        
    val data   = Output(UInt(8.W))    

    })

  // internal variables
  /* 
   * TODO: Define internal variables (registers and/or wires), if needed
   */
    val reg = RegInit(0.U(8.W))

  // functionality
  /* 
   * TODO: Describe functionality if the shift register
   */
  when(io.en) {
    reg := Cat(reg(6, 0), io.rxd)    // MSB first → shift left, new bit at LSB
  }
 
  io.data := reg
}

/** 
  * The last warm-up task deals with a more complex component. Your goal is to design a serial receiver.
  * It scans an input line (“serial bus”) named rxd for serial transmissions of data bytes. A transmission 
  * begins with a start bit ‘0’ followed by 8 data bits. The most significant bit (MSB) is transmitted first. 
  * There is no parity bit and no stop bit. After the last data bit has been transferred a new transmission 
  * (beginning with a start bit, ‘0’) may immediately follow. If there is no new transmission the bus line 
  * goes high (‘1’, this is considered the “idle” bus signal). In this case the receiver waits until the next 
  * transmission begins. The outputs of the design are an 8-bit parallel data signal and a valid signal. 
  * The valid signal goes high (‘1’) for one clock cycle after the last serial bit has been transmitted, 
  * indicating that a new data byte is ready.
  */
class ReadSerial extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a the component as stated in the documentation
     */
    val rxd   = Input(Bool())      
    val valid = Output(Bool())     
    val data  = Output(UInt(8.W))  
    })


  // instanciation of modules
  /* 
   * TODO: Instanciate the modules that you need
   */
  val ctrl  = Module(new Controller)
  val cnt   = Module(new Counter)
  val shift = Module(new ShiftRegister)

  // connections between modules
  /* 
   * TODO: connect the signals between the modules
   */
  // Controller ↔ Counter
  cnt.io.cnt_s  := ctrl.io.cnt_s
  cnt.io.cnt_en := ctrl.io.cnt_en

  // Controller ↔ Shift Register (same enable signal)
  shift.io.rxd := io.rxd
  shift.io.en  := ctrl.io.cnt_en

  // Controller ← Counter done
  ctrl.io.done := cnt.io.done

    // Controller receives rxd
  ctrl.io.rxd := io.rxd

  // global I/O 
  /* 
   * TODO: Describe output behaviour based on the input values and the internal signals
   */
  io.valid := ctrl.io.valid
  io.data  := shift.io.data
}