package juli.blocks.devices.tdpmem

import Chisel._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.subsystem._
import freechips.rocketchip.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import chisel3.experimental.withClock

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

import sifive.blocks.util.{SlaveRegIF, GenericTimerIO, GenericTimer, GenericTimerCfgDescs, DefaultGenericTimerCfgDescs}

case class TDPMemParams(address: BigInt = 0x2000, regBytes: Int = 4, useAXI4: Boolean = false, sizeBytes: Int = 32, fAddress : BigInt = 0x3000, fSize : Int = 32) 
{
  def Offset:Int = 0
}

case object TDPMemListKey extends Field[Option[Seq[TDPMemParams]]](None)

class TDPMemBundle(c: TDPMemParams) extends Bundle
{
;
override def cloneType = (new TDPMemBundle(c)).asInstanceOf[this.type]
}

class TDPMemModule(c: TDPMemParams, outer: TLTDPMem) extends LazyModuleImp(outer)
{
  val params = c

  val mem = Module( new TDPMem(regWidth = 8/*c.regBytes*8*/, Indizes = (c.sizeBytes.toFloat/c.regBytes.toFloat).ceil.toInt ))
  //outer.interrupts(0)
  //outer.port

    /*val address = Input(UInt(log2Ceil(Indizes).W))
    val wrdata = Input(UInt(8.W))
    val wren = Input(Bool())
    val rddata = Output(UInt(8.W))*/

  val (f, n) = outer.fnode.in(0)
  val a_channel = RegEnable(f.a.bits, f.a.fire())
  val d_valid = RegInit(false.B)
  val a_valid_ff = RegNext(f.a.valid)
  //val a_ready = RegNext(f.a.valid)  // 1 Clock Delay
  f.b.valid := Bool(false)
  f.c.ready := Bool(true)
  f.e.ready := Bool(true)
  f.a.ready := f.a.valid
  f.d.valid := d_valid
  mem.io.wren := f.a.bits.opcode === 0.U && f.a.valid
  mem.io.wrdata := f.a.bits.data
  mem.io.address := f.a.bits.address

  when(f.a.bits.opcode === 0.U && f.a.valid){   // PutFull
    f.d.bits := outer.fnode.edges.in.head.AccessAck(a_channel)  //AccessAck
    d_valid := true.B
  }.elsewhen(a_channel.opcode === 4.U && a_valid_ff){  // Get
    f.d.bits := outer.fnode.edges.in.head.AccessAck(a_channel, mem.io.rddata) //AccessAckData
    d_valid := true.B
  }.elsewhen(f.d.ready){
    d_valid := false.B
  }


  //var ct = None
  //private val (f, n) = fnode.in(0)
  //ct = n.bundle.dataBits
  println("TDP Mem Bus Parameters: ")
  println("Data Bits = " + n.bundle.dataBits);
  println("Address Bits = " + n.bundle.addressBits);
  println("Source Bits = " + n.bundle.sourceBits);
  println("Sink Bits = " + n.bundle.sinkBits);
  println("Size Bits = " + n.bundle.sizeBits);

  //val s = IO(new SlaveRegIF(1))
  //val sreg = RegEnable(s.write.bits,s.write.valid)
  //s.read := sreg
  //protected def wr_desc:        RegFieldDesc = RegFieldDesc(s"write", "Write Enable")

  //val regmap_f =  Seq( c.regBytes*0 -> Seq(s.toRegField(Some(wr_desc))) )  //TDPMem.RegMap(mem, c.regBytes) 

}

abstract class TLTDPMemBase( c: TDPMemParams, beatBytes:Int)(implicit p: Parameters) extends IORegisterRouter(
      RegisterRouterParams(
        name = "TDPMem",
        compat = Seq("juli,TDPMem"),
        base = c.address,
        size = c.sizeBytes,
        beatBytes = beatBytes),
      new TDPMemBundle(c))
    with HasInterruptSources {

  require(isPow2(c.fSize))
  require(isPow2(c.sizeBytes))

  val fnode = TLManagerNode(Seq(TLManagerPortParameters(
    managers = Seq(TLManagerParameters(
      address     = Seq(AddressSet(c.fAddress, c.fSize-1)),
      resources   = device.reg("mem"),
      regionType  = RegionType.UNCACHED,
      executable  = false,  // No Executable Code
      supportsGet = TransferSizes(1, 1),  // Transfer Size in Bytes(min, max) 
      supportsPutFull = TransferSizes(1, 1),  // Transfer Size in Bytes(min, max) 
      fifoId      = Some(0))),
    beatBytes = 1)))

  override def nInterrupts = 0
}

class TLTDPMem(c: TDPMemParams, w: Int)(implicit p: Parameters)
    extends TLTDPMemBase(c,w)(p)
    with HasTLControlRegMap {
  lazy val module = new TDPMemModule(c, this) {

    //regmap(regmap_f :_*)
  }
}

class AXI4TDPMem(params: TDPMemParams, beatBytes: Int)(implicit p: Parameters)

// java -jar rocket-chip/sbt-launch.jar ++2.12.4 "runMain freechips.rocketchip.system.Generator /home/julian/RISCV/freedom-e/builds/e300artydevkit sifive.freedom.everywhere.e300artydevkit E300ArtyDevKitFPGAChip sifive.freedom.everywhere.e300artydevkit E300ArtyDevKitConfig"

trait CanHavePeripheryTDPMemList
{ this: BaseSubsystem =>
  
  val tdpmem = p(TDPMemListKey) match {
    case Some(params) => {
      Some(
        params.map{ ps =>
          TLTDPMem.attach(ps, p, pbus, ibus)
      })
    }
    case None => None
  }
}

trait CanHavePeripheryTDPMemListModuleImp extends LazyModuleImp {
  val outer: CanHavePeripheryTDPMemList
  
  val tdpmem_io = outer.tdpmem match {
    case Some(tdpmem) => { 
      Some(tdpmem.map{ case (tmod: Either[TLTDPMem, (AXI4TDPMem, TLToAXI4)], crossing: TLAsyncCrossingSink) =>
        tmod match{
          /*case Right((mod, toaxi4)) =>{
            val c = mod.module.params
            /*val wdog = IO(new WDIO( c.Resets ))

            wdog.outputs := mod.module.io.wdog.outputs*/
            //mod.module.io.wdog.clock := clock
            crossing.module.clock := clock
            toaxi4.module.clock := clock
            clock
          }*/
          case Left(mod) =>{
            val c = mod.module.params
            /*val wdog = IO(new WDIO( c.Resets ))

            wdog.outputs := mod.module.io.wdog.outputs*/
            //mod.module.io.wdog.clock := clock
            crossing.module.clock := clock
            clock
          }
        }
      })
    }
    case None => None
  }
}

object TLTDPMem {
  val nextId = { var i = -1; () => { i += 1; i} }

  def attach(params: TDPMemParams, parameter: Parameters, pBus: PeripheryBus, iBus: InterruptBusWrapper) : (Either[TLTDPMem, (AXI4TDPMem, TLToAXI4)],TLAsyncCrossingSink) = {
    implicit val p = parameter
   /* if (params.useAXI4){
      val name = s"axi4TDPMem_${nextId()}"
      val crossing = LazyModule(new TLAsyncCrossingSink(AsyncQueueParams.singleton()))
      val tlaxi4 = LazyModule(new TLToAXI4())
      val tdpmem = LazyModule(new AXI4TDPMem(params, pBus.beatBytes)(p))
      tdpmem.suggestName(name)
      pBus.toSlave(Some(name)) {
        tdpmem.node :=
        AXI4Buffer () :=
        tlaxi4.node :=
        crossing.node :=
        TLAsyncCrossingSource() :=
        // toVariableWidthSlave doesn't use holdFirstDeny, which TLToAXI4() needsx
        TLFragmenter(pBus.beatBytes, pBus.blockBytes, holdFirstDeny = true) 
      }
      //iBus.fromSync := IntSyncCrossingSink() := IntSyncCrossingSource(alreadyRegistered = true)  := tdpmem.intnode // Ibus -> Connecting to Interruptsystem
      
      (Right(tdpmem, tlaxi4),crossing)
    }else{*/

      
      val name = s"tlTDPMem_${nextId()}"
      val crossing = LazyModule(new TLAsyncCrossingSink(AsyncQueueParams.singleton()))
      val tdpmem = LazyModule(new TLTDPMem(params, pBus.beatBytes)(p))
      tdpmem.suggestName(name)
      val node = tdpmem.controlNode := crossing.node 
      
      pBus.toVariableWidthSlave(Some(name)) { node := TLAsyncCrossingSource() }  // Pbus -> Connecting to PeripherialBus // sbus -> Systembus
      pBus.coupleTo(s"mem_named_$name") { tdpmem.fnode := TLFragmenter(1, pBus.blockBytes) := TLWidthWidget(pBus.beatBytes) := _} 
      if (tdpmem.nInterrupts > 0){
        iBus.fromSync := IntSyncCrossingSink() := IntSyncCrossingSource(alreadyRegistered = true)  := tdpmem.intnode // Ibus -> Connecting to Interruptsystem
      }
      //var tdpNode = None 
      val tdpNode = tdpmem.ioNode.makeSink()(parameter)//.makeIO()(parameter)  //[TDPMemBundle]
      (Left(tdpmem),crossing)
   // }
  }
}