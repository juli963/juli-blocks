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
  mem.io.MemA.wr := io.wren
  mem.io.MemA.wrData := io. wrdata
  mem.io.MemB.wr := false.B
  mem.io.MemB.wrData := 0.U
  mem.io.MemB.Addr := io.address
  mem.io.MemA.Addr := io.address
  io.rddata := mem.io.MemB.rdData


  mem.io.clockA := clock
  mem.io.clockB := clock

}