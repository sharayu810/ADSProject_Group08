// ADS I Class Project - Assignment 6 Performance Evaluation
//
// Runs each branch-pattern benchmark with dynamic BTB prediction and with the
// Assignment 5 static not-taken policy. The core counters are observational.
package PipelinedRV32I_Tester

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import PipelinedRV32I._

class PerformanceEvaluationSpec extends AnyFlatSpec with ChiselScalatestTester {
  private case class Measurement(
      cycles: BigInt,
      branches: BigInt,
      correct: BigInt,
      incorrect: BigInt
  ) {
    def accuracy: Double =
      if (branches == 0) 100.0 else correct.toDouble * 100.0 / branches.toDouble
  }

  private def runBenchmark(binary: String, finalResult: BigInt, enableBTB: Boolean): Measurement = {
    var result: Option[Measurement] = None
    test(new PipelinedRV32I(binary, enableBTB)) { dut =>
      var done = false
      var elapsed = 0
      while (!done && elapsed < 500) {
        dut.io.exception.expect(false.B)
        if (dut.io.result.peek().litValue == finalResult) {
          done = true
        } else {
          dut.clock.step()
          elapsed += 1
        }
      }
      assert(done, "benchmark " + binary + " did not produce final result " + finalResult)

      val measurement = Measurement(
        dut.io.cycles.peek().litValue,
        dut.io.branches.peek().litValue,
        dut.io.correctPredictions.peek().litValue,
        dut.io.incorrectPredictions.peek().litValue
      )
      assert(
        measurement.correct + measurement.incorrect == measurement.branches,
        "invalid branch accounting for " + binary + ": " + measurement
      )
      result = Some(measurement)
    }
    result.getOrElse(throw new AssertionError("benchmark did not return a measurement"))
  }

  private def printSummary(name: String, enabled: Measurement, disabled: Measurement): Unit = {
    val reduction = disabled.cycles - enabled.cycles
    val speedup =
      if (enabled.cycles == 0) 1.0 else disabled.cycles.toDouble / enabled.cycles.toDouble
    println("\nAssignment 6 performance benchmark: " + name)
    println("  BTB enabled : cycles=%d, branches=%d, correct=%d, incorrect=%d, accuracy=%.2f%%".format(
      enabled.cycles, enabled.branches, enabled.correct, enabled.incorrect, enabled.accuracy))
    println("  Static NT   : cycles=%d, branches=%d, correct=%d, incorrect=%d, accuracy=%.2f%%".format(
      disabled.cycles, disabled.branches, disabled.correct, disabled.incorrect, disabled.accuracy))
    println("  Cycle reduction: %d cycles, speedup: %.3fx".format(reduction, speedup))
  }

  "Assignment 6 BTB performance evaluation" should "compare dynamic prediction with static not-taken for every branch pattern" in {
    val benchmarks = Seq(
      ("simple loop", "src/test/programs/Benchmark_simple_loop", BigInt(0x301)),
      ("nested loops", "src/test/programs/Benchmark_nested_loops", BigInt(0x302)),
      ("mostly taken", "src/test/programs/Benchmark_mostly_taken", BigInt(0x303)),
      ("mostly not taken", "src/test/programs/Benchmark_mostly_not_taken", BigInt(0x304)),
      ("alternating", "src/test/programs/Benchmark_alternating", BigInt(0x305)),
      ("mixed forward/backward branches", "src/test/programs/Benchmark_mixed_branches", BigInt(0x306))
    )

    var totalReduction = BigInt(0)
    benchmarks.foreach { case (name, binary, finalResult) =>
      val enabled = runBenchmark(binary, finalResult, enableBTB = true)
      val disabled = runBenchmark(binary, finalResult, enableBTB = false)
      assert(enabled.branches == disabled.branches, "branch count changed for " + name)
      assert(enabled.cycles <= disabled.cycles, "BTB regressed cycle count for " + name)
      printSummary(name, enabled, disabled)
      totalReduction += disabled.cycles - enabled.cycles
    }
    assert(totalReduction > 0, "BTB did not reduce cycles across the benchmark suite")
  }
}
