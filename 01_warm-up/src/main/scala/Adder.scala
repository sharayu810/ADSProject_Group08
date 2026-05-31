// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package adder

import chisel3._
import chisel3.util._


/** 
  * Half Adder Class 
  * 
  * Your task is to implement a basic half adder as presented in the lecture.
  * Each signal should only be one bit wide (inputs and outputs).
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class HalfAdder extends Module{
  
  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a half adder as presented in the lecture
     */
    
    //Inputs
    val a = Input(Bool())
    val b = Input(Bool())

    //Outputs
    val s= Output(Bool())
    val co = Output(Bool())
  })

  /* 
   * TODO: Describe output behaviour based on the input values
   */

  //Combinational Logic
  io.s := io.a ^ io.b
  io.co := io.a & io.b

}

/** 
  * Full Adder Class 
  * 
  * Your task is to implement a basic full adder. The component's behaviour should 
  * match the characteristics presented in the lecture. In addition, you are only allowed 
  * to use two half adders (use the class that you already implemented) and basic logic 
  * operators (AND, OR, ...).
  * Each signal should only be one bit wide (inputs and outputs).
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class FullAdder extends Module{

  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a half adder as presented in the lecture
     */
    //Inputs
    val a = Input(Bool())
    val b = Input(Bool())
    val ci = Input(Bool())

    //Outputs
    val s = Output(Bool())
    val co = Output(Bool())

    })


  /* 
   * TODO: Instanciate the two half adders you want to use based on your HalfAdder class
   */
  val ha1 = Module(new HalfAdder)
  val ha2 = Module(new HalfAdder)

  //First HalfAdder adds a and b
  ha1.io.a := io.a
  ha1.io.b := io.b

  //Second HalfAdder adds intermediate sum and carry input
  ha2.io.a := ha1.io.s
  ha2.io.b := io.ci


  /* 
   * TODO: Describe output behaviour based on the input values and the internal signals
   */
  io.s := ha2.io.s
  io.co := ha1.io.co | ha2.io.co

}

/** 
  * 4-bit Adder class 
  * 
  * Your task is to implement a 4-bit ripple-carry-adder. The component's behaviour should 
  * match the characteristics presented in the lecture.  Remember: An n-bit adder can be 
  * build using one half adder and n-1 full adders.
  * The inputs and the result should all be 4-bit wide, the carry-out only needs one bit.
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class FourBitAdder extends Module{

  val io = IO(new Bundle {
    /* 
     * TODO: Define IO ports of a 4-bit ripple-carry-adder as presented in the lecture
     */
    //Inputs
    val a = Input(UInt(4.W))
    val b = Input(UInt(4.W))

    //Outputs
    val s = Output(UInt(4.W))
    val co = Output(UInt(1.W))

    })

  /* 
   * TODO: Instanciate the full adders and one half adderbased on the previously defined classes
   */
  val ha = Module(new HalfAdder)
  val fa1 = Module(new FullAdder)
  val fa2 = Module(new FullAdder)
  val fa3 = Module(new FullAdder)   

  //First HalfAdder adds the least significant bits of a and b
  ha.io.a := io.a(0).asUInt
  ha.io.b := io.b(0).asUInt

  //First FullAdder adds the second least significant bits of a and b and the carry-out of the first half adder
  fa1.io.a  := io.a(1).asUInt
  fa1.io.b  := io.b(1).asUInt
  fa1.io.ci := ha.io.co 

  //Second FullAdder adds the third least significant bits of a and b and the carry-out of the first full adder

  fa2.io.a  := io.a(2).asUInt
  fa2.io.b  := io.b(2).asUInt
  fa2.io.ci := fa1.io.co    

  //Third FullAdder adds the most significant bits of a and b and the carry-out of the second full adder
  fa3.io.a  := io.a(3).asUInt
  fa3.io.b  := io.b(3).asUInt
  fa3.io.ci := fa2.io.co    

  /* 
   * TODO: Describe output behaviour based on the input values and the internal 
   */
  io.s := Cat(fa3.io.s, fa2.io.s, fa1.io.s, ha.io.s)
  io.co := fa3.io.co  

}
