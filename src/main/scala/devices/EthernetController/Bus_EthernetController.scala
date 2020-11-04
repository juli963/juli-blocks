package juli.blocks.devices.ethctrl

import chisel3._

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

import Ethernet.Interface.Types._

case class ETHCtrlParams(address: BigInt = 0x2000, regBytes: Int = 4, useAXI4: Boolean = false, sizeBytes: Int = 32, NumBuffers: Int = 4) 
{
  def Offset:Int = 0
}

case object ETHCtrlKey extends Field[Option[ETHCtrlParams]](None)

case object ETHCtrlListKey extends Field[Option[Seq[ETHCtrlParams]]](None)

class ETHPort() extends Bundle{
  val RGMII = new Ethernet.Interface.Types.RGMII_Interface()
  val PHY_nrst = Output(Bool())
  val EthernetClock125 = Input(Clock())
  val EthernetClock250 = Input(Clock())
}

trait ETHCtrlBundle
{
  val params: ETHCtrlParams
  val c = params

  val port = new ETHPort()
}

trait ETHCtrlModule extends HasRegMap
{
  val params: ETHCtrlParams
  val io: ETHCtrlBundle
  val interrupts: Vec[Bool]
  val c = params
  val eth = Module( new ETHCtrl(regWidth = c.regBytes*8, Indizes = (c.sizeBytes.toFloat/c.regBytes.toFloat).ceil.toInt, NumBuffers = c.NumBuffers) )
  interrupts(0) := eth.io.interrupt
  io.port.RGMII <> eth.io.RGMII
  io.port.PHY_nrst := eth.io.PHY_nrst
  eth.io.EthernetClock125 := io.port.EthernetClock125
  eth.io.EthernetClock250 := io.port.EthernetClock250
  regmap(
      (ETHCtrl.RegMap(eth, c.regBytes) :_*)
    ) 
}

// Create a concrete TL2 version of the abstract Example slave
class TLETHCtrl( params: ETHCtrlParams, beatBytes:Int)(implicit p: Parameters)
  extends TLRegisterRouter(
  params.address,
  "ETHCtrl", 
  Seq("juli,ETHCtrl"), 
  beatBytes = beatBytes,
  interrupts = 1,
  concurrency = 1)(
  new TLRegBundle(params, _)    with ETHCtrlBundle)(
  new TLRegModule(params, _, _) with ETHCtrlModule) 

class AXI4ETHCtrl(params: ETHCtrlParams, beatBytes: Int)(implicit p: Parameters)
  extends AXI4RegisterRouter(
    params.address,
    beatBytes=beatBytes,
    interrupts = 0,
    concurrency = 1)(
      new AXI4RegBundle(params, _) with ETHCtrlBundle)(
      new AXI4RegModule(params, _, _) with ETHCtrlModule)

// java -jar rocket-chip/sbt-launch.jar ++2.12.4 "runMain freechips.rocketchip.system.Generator /home/julian/RISCV/freedom-e/builds/e300artydevkit sifive.freedom.everywhere.e300artydevkit E300ArtyDevKitFPGAChip sifive.freedom.everywhere.e300artydevkit E300ArtyDevKitConfig"

trait CanHavePeripheryETHCtrlList
{ this: BaseSubsystem =>
  
  val ethctrl = p(ETHCtrlListKey) match {
    case Some(params) => {
      Some(
        params.map{ ps =>
          TLETHCtrl.attach(ps, p, pbus, ibus)
      })
    }
    case None => None
  }
}

trait CanHavePeripheryETHCtrlListModuleImp extends LazyModuleImp {
  val outer: CanHavePeripheryETHCtrlList
  
  val ethctrl_io = outer.ethctrl match {
    case Some(ethctrl) => { 
      Some(ethctrl.map{ case (tmod: Either[TLETHCtrl, (AXI4ETHCtrl, TLToAXI4)], crossing: TLAsyncCrossingSink) =>
        tmod match{
          case Right((mod, toaxi4)) =>{
            val c = mod.module.params
            val io = IO(new ETHPort())
            io <> mod.module.io.port
            /*val wdog = IO(new WDIO( c.Resets ))

            wdog.outputs := mod.module.io.wdog.outputs*/
            //mod.module.io.wdog.clock := clock
            crossing.module.clock := clock
            toaxi4.module.clock := clock
            io
          }
          case Left(mod) =>{
            val c = mod.module.params
            val io = IO(new ETHPort())
            io <> mod.module.io.port
            /*val wdog = IO(new WDIO( c.Resets ))

            wdog.outputs := mod.module.io.wdog.outputs*/
            //mod.module.io.wdog.clock := clock
            crossing.module.clock := clock
            io
          }
        }
      })
    }
    case None => None
  }
}

object TLETHCtrl {
  val nextId = { var i = -1; () => { i += 1; i} }

  def attach(params: ETHCtrlParams, parameter: Parameters, pBus: PeripheryBus, iBus: InterruptBusWrapper) : (Either[TLETHCtrl, (AXI4ETHCtrl, TLToAXI4)],TLAsyncCrossingSink) = {
    implicit val p = parameter
    if (params.useAXI4){
      val name = s"axi4ETHCtrl_${nextId()}"
      val crossing = LazyModule(new TLAsyncCrossingSink(AsyncQueueParams.singleton()))
      val tlaxi4 = LazyModule(new TLToAXI4())
      val ethctrl = LazyModule(new AXI4ETHCtrl(params, pBus.beatBytes)(p))
      ethctrl.suggestName(name)
      pBus.toSlave(Some(name)) {
        ethctrl.node :=
        AXI4Buffer () :=
        tlaxi4.node :=
        crossing.node :=
        TLAsyncCrossingSource() :=
        // toVariableWidthSlave doesn't use holdFirstDeny, which TLToAXI4() needsx
        TLFragmenter(pBus.beatBytes, pBus.blockBytes, holdFirstDeny = true) 
      }
      iBus.fromSync := IntSyncCrossingSink() := IntSyncCrossingSource(alreadyRegistered = true)  := ethctrl.intnode // Ibus -> Connecting to Interruptsystem
      
      (Right(ethctrl, tlaxi4),crossing)
    }else{
      val name = s"tlETHCtrl_${nextId()}"
      val crossing = LazyModule(new TLAsyncCrossingSink(AsyncQueueParams.singleton()))
      val ethctrl = LazyModule(new TLETHCtrl(params, pBus.beatBytes)(p))
      ethctrl.suggestName(name)
      val node = ethctrl.node := crossing.node 
      pBus.toVariableWidthSlave(Some(name)) { node := TLAsyncCrossingSource() }  // Pbus -> Connecting to PeripherialBus // sbus -> Systembus
      iBus.fromSync := IntSyncCrossingSink() := IntSyncCrossingSource(alreadyRegistered = true)  := ethctrl.intnode // Ibus -> Connecting to Interruptsystem
      
      (Left(ethctrl),crossing)
    }
  }
}