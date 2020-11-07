package juli.blocks.devices.tdpmem

import Memory.onChip.TrueDualPortBRAM

import chisel3._
import chisel3.util._

import freechips.rocketchip.regmapper._
import sifive.blocks.util.{SlaveRegIF, GenericTimerIO, GenericTimer, GenericTimerCfgDescs, DefaultGenericTimerCfgDescs}

class TDPMem(regWidth: Int = 32, Indizes: Int = 5) extends Module {
  protected val prefix = "tdpmem"
  
  val io = IO(new Bundle {
    val address = Input(UInt(log2Ceil(Indizes).W))
    val wrdata = Input(UInt(8.W))
    val wren = Input(Bool())
    val rddata = Output(UInt(8.W))
  })
  
  val mem = Module(new TrueDualPortBRAM(Indizes, UInt(regWidth.W)))
  // Register
  mem.io.MemIO.wr := io.wren
  mem.io.MemIO.wrAddr := io.address
  mem.io.MemIO.wrData := io. wrdata
  mem.io.MemIO.rdAddr := io.address
  io.rddata := mem.io.MemIO.rdData


  mem.io.clockWr := clock
  mem.io.clockRd := clock

}