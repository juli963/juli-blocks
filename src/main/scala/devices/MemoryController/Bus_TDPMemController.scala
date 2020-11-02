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

case class TDPMemParams(address: BigInt = 0x2000, regBytes: Int = 4, useAXI4: Boolean = false, sizeBytes: Int = 32) 
{
  def Offset:Int = 0
}

case object TDPMemKey extends Field[Option[TDPMemParams]](None)

case object TDPMemListKey extends Field[Option[Seq[TDPMemParams]]](None)


trait TDPMemBundle
{
  val params: TDPMemParams
  val c = params

}

trait TDPMemModule extends HasRegMap
{
  val params: TDPMemParams
  val io: TDPMemBundle
  val interrupts: Vec[Bool]
  val c = params
  val mem = Module( new TDPMem(regWidth = c.regBytes*8, Indizes = (c.sizeBytes.toFloat/c.regBytes.toFloat).ceil.toInt ))
  regmap(
      (TDPMem.RegMap(mem, c.regBytes) :_*)
    ) 
}

// Create a concrete TL2 version of the abstract Example slave
class TLTDPMem( params: TDPMemParams, beatBytes:Int)(implicit p: Parameters)
  extends TLRegisterRouter(
  params.address,
  "TDPMem", 
  Seq("juli,TDPMem"), 
  beatBytes = beatBytes,
  interrupts = 0,
  concurrency = 1)(
  new TLRegBundle(params, _)    with TDPMemBundle)(
  new TLRegModule(params, _, _) with TDPMemModule) 

class AXI4TDPMem(params: TDPMemParams, beatBytes: Int)(implicit p: Parameters)
  extends AXI4RegisterRouter(
    params.address,
    beatBytes=beatBytes,
    interrupts = 0,
    concurrency = 1)(
      new AXI4RegBundle(params, _) with TDPMemBundle)(
      new AXI4RegModule(params, _, _) with TDPMemModule)

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
          case Right((mod, toaxi4)) =>{
            val c = mod.module.params
            /*val wdog = IO(new WDIO( c.Resets ))

            wdog.outputs := mod.module.io.wdog.outputs*/
            //mod.module.io.wdog.clock := clock
            crossing.module.clock := clock
            toaxi4.module.clock := clock
            clock
          }
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
    if (params.useAXI4){
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
    }else{
      val name = s"tlTDPMem_${nextId()}"
      val crossing = LazyModule(new TLAsyncCrossingSink(AsyncQueueParams.singleton()))
      val tdpmem = LazyModule(new TLTDPMem(params, pBus.beatBytes)(p))
      tdpmem.suggestName(name)
      val node = tdpmem.node := crossing.node 
      pBus.toVariableWidthSlave(Some(name)) { node := TLAsyncCrossingSource() }  // Pbus -> Connecting to PeripherialBus // sbus -> Systembus
      //iBus.fromSync := IntSyncCrossingSink() := IntSyncCrossingSource(alreadyRegistered = true)  := tdpmem.intnode // Ibus -> Connecting to Interruptsystem
      
      (Left(tdpmem),crossing)
    }
  }
}