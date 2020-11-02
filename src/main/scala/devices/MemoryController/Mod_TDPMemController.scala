package juli.blocks.devices.tdpmem

import Memory.onChip.TrueDualPortBRAM

import chisel3._
import chisel3.util._

import freechips.rocketchip.regmapper._
import sifive.blocks.util.{SlaveRegIF, GenericTimerIO, GenericTimer, GenericTimerCfgDescs, DefaultGenericTimerCfgDescs}

class TDPMem(regWidth: Int = 32, Indizes: Int = 5) extends Module {
  protected val prefix = "tdpmem"
  
  val io = IO(new Bundle {
    val wrena = new SlaveRegIF(1)
    val wraddr = new SlaveRegIF(log2Ceil(Indizes))
    val wrdata = new SlaveRegIF(regWidth)
    val rdaddr = new SlaveRegIF(log2Ceil(Indizes))
    val rddata = new SlaveRegIF(regWidth)
  })
  
  val mem = Module(new TrueDualPortBRAM(Indizes, UInt(regWidth.W)))
  // Register
  val wrena = RegEnable(io.wrena.write.bits, io.wrena.write.valid)
  io.wrena.read := wrena
  mem.io.MemIO.wr := wrena
  val wraddr = RegEnable(io.wraddr.write.bits, io.wraddr.write.valid)
  io.wraddr.read := wraddr
  mem.io.MemIO.wrAddr := wraddr
  val wrdata = RegEnable(io.wrdata.write.bits, io.wrdata.write.valid)
  io.wrdata.read := wrdata
  mem.io.MemIO.wrData := wrdata
  val rdaddr = RegEnable(io.rdaddr.write.bits, io.rdaddr.write.valid)
  io.rdaddr.read := rdaddr
  mem.io.MemIO.rdAddr := rdaddr

  io.rddata.read := mem.io.MemIO.rdData

  mem.io.clockWr := clock
  mem.io.clockRd := clock

  // Register Descriptions
  protected def wr_desc:        RegFieldDesc = RegFieldDesc(s"${prefix}_write", "Write Enable")
  protected def wraddr_desc:    RegFieldDesc = RegFieldDesc(s"${prefix}_wraddr", "Write Address")
  protected def wrdata_desc:    RegFieldDesc = RegFieldDesc(s"${prefix}_wrdata", "Write Data")
  protected def rdaddr_desc:    RegFieldDesc = RegFieldDesc(s"${prefix}_rdaddr", "Read Address")
  protected def rddata_desc:    RegFieldDesc = RegFieldDesc(s"${prefix}_rddata", "Read Data", access=RegFieldAccessType.R, volatile=true)
}

object TDPMem {

  // Seq( (i1, reg1), (i2,reg2) )  -> This sequence holds elements, with i1 offset of reg1 etc. ++ concats inner elements
  def RegMap(arr: TDPMem, regBytes: Int): Seq[(Int, Seq[RegField])] = {
    val wrena = Seq( regBytes*0 -> Seq(arr.io.wrena.toRegField(Some(arr.wr_desc))) )
    val wraddr = Seq( regBytes*1 -> Seq(arr.io.wraddr.toRegField(Some(arr.wraddr_desc))) )
    val wrdata = Seq( regBytes*2 -> Seq(arr.io.wrdata.toRegField(Some(arr.wrdata_desc))) )
    val rdaddr = Seq( regBytes*3 -> Seq(arr.io.rdaddr.toRegField(Some(arr.rdaddr_desc))) )
    val rddata = Seq( regBytes*4 -> Seq(arr.io.rddata.toRegField(Some(arr.rddata_desc))) )
    wrena ++ wraddr ++ wrdata ++ rdaddr ++ rddata
  }
}