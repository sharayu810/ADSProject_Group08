package PipelinedRV32I_Tester

import chisel3._
import chiseltest._
import core_tile.BTB
import org.scalatest.flatspec.AnyFlatSpec

class BTBSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def idle(dut: BTB): Unit = {
    dut.io.update.poke(false.B)
    dut.io.mispredicted.poke(false.B)
    dut.io.updatePC.poke(0.U)
    dut.io.updateTarget.poke(0.U)
  }

  private def update(dut: BTB, pc: BigInt, target: BigInt, mispredicted: Boolean): Unit = {
    dut.io.update.poke(true.B)
    dut.io.updatePC.poke(pc.U)
    dut.io.updateTarget.poke(target.U)
    dut.io.mispredicted.poke(mispredicted.B)
    dut.clock.step()
    idle(dut)
  }

  private def lookup(dut: BTB, pc: BigInt, valid: Boolean, target: BigInt = 0,
                     taken: Boolean = false): Unit = {
    dut.io.PC.poke(pc.U)
    dut.io.valid.expect(valid.B)
    if (valid) {
      dut.io.target.expect(target.U)
      dut.io.predictTaken.expect(taken.B)
    }
  }

  "BTB" should "miss initially, hit after update, and exercise all four saturating-counter states" in {
    test(new BTB) { dut =>
      idle(dut)
      lookup(dut, 0x100, valid = false)
      // Reset state is weak-not-taken: a taken first encounter is mispredicted.
      update(dut, 0x100, 0x080, mispredicted = true)
      // weak-taken
      lookup(dut, 0x100, valid = true, 0x080, taken = true)
      update(dut, 0x100, 0x080, mispredicted = false)
      // strong-taken
      lookup(dut, 0x100, valid = true, 0x080, taken = true)
      update(dut, 0x100, 0x080, mispredicted = true)
      // strong-taken -> weak-taken: prediction remains taken.
      lookup(dut, 0x100, valid = true, 0x080, taken = true)
      update(dut, 0x100, 0x080, mispredicted = true)
      // weak-taken -> weak-not-taken: prediction flips.
      lookup(dut, 0x100, valid = true, 0x080, taken = false)
      update(dut, 0x100, 0x080, mispredicted = false)
      // weak-not-taken -> strong-not-taken; another correct outcome saturates.
      update(dut, 0x100, 0x080, mispredicted = false)
      lookup(dut, 0x100, valid = true, 0x080, taken = false)
    }
  }

  it should "use both ways and evict the least recently used entry in a set" in {
    test(new BTB) { dut =>
      idle(dut)
      update(dut, 0x000, 0x040, mispredicted = true)
      update(dut, 0x020, 0x060, mispredicted = true)
      lookup(dut, 0x000, valid = true, 0x040, taken = true)
      dut.clock.step() // Commit the lookup's LRU state before inserting a third entry.
      update(dut, 0x040, 0x080, mispredicted = true)
      lookup(dut, 0x000, valid = true, 0x040, taken = true)
      lookup(dut, 0x020, valid = false)
      lookup(dut, 0x040, valid = true, 0x080, taken = true)
    }
  }

  it should "match tags exactly and retain two distinct entries in one set" in {
    test(new BTB) { dut =>
      idle(dut)
      // 0x000 and 0x020 have the same index but different PC[31:5] tags.
      update(dut, 0x000, 0x100, mispredicted = true)
      update(dut, 0x020, 0x120, mispredicted = false)
      lookup(dut, 0x000, valid = true, 0x100, taken = true)
      lookup(dut, 0x020, valid = true, 0x120, taken = false)
      lookup(dut, 0x040, valid = false) // same set, third unmatched tag
    }
  }

  it should "update the target and predictor state of an existing valid entry" in {
    test(new BTB) { dut =>
      idle(dut)
      update(dut, 0x180, 0x100, mispredicted = true)
      lookup(dut, 0x180, valid = true, 0x100, taken = true)

      // A matching update must retain the entry and replace its stored target.
      update(dut, 0x180, 0x140, mispredicted = false)
      lookup(dut, 0x180, valid = true, 0x140, taken = true)
    }
  }
}
