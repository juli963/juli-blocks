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
import chisel3.util._

import freechips.rocketchip.regmapper._

import Ethernet.Interface.Types._

case class ETHCtrlParams(address: BigInt = 0x2000, regBytes: Int = 4, useAXI4: Boolean = false, sizeBytes: Int = 32, fAddress: BigInt = 0x8000, NumBuffers: Int = 4) 
{
  def Offset:Int = 0
}

case object ETHCtrlKey extends Field[Option[ETHCtrlParams]](None)

case object ETHCtrlListKey extends Field[Option[Seq[ETHCtrlParams]]](None)

class ETHPort(c: ETHCtrlParams) extends Bundle{
  val params = c
  val RGMII = new Ethernet.Interface.Types.RGMII_Interface()
  val PHY_nrst = Output(Bool())
  val EthernetClock125 = Input(Clock())
  val EthernetClock250 = Input(Clock())
  override def cloneType = (new ETHPort(c)).asInstanceOf[this.type]
}

class ETHCtrlModule(c: ETHCtrlParams, outer: TLETHCtrl) extends LazyModuleImp(outer)
{
  val (f, n) = outer.fnode.in(0)
  println("ETH Bus Parameters: ")
  println("Data Bits = " + n.bundle.dataBits);
  println("Address Bits = " + n.bundle.addressBits);
  println("Source Bits = " + n.bundle.sourceBits);
  println("Sink Bits = " + n.bundle.sinkBits);
  println("Size Bits = " + n.bundle.sizeBits);



  val eth = Module( new ETHCtrl(regWidth = c.regBytes*8, Indizes = (c.sizeBytes.toFloat/c.regBytes.toFloat).ceil.toInt, NumBuffers = c.NumBuffers) )
  outer.interrupts(0) := eth.io.interrupt
  outer.port.RGMII <> eth.io.RGMII
  outer.port.PHY_nrst := eth.io.PHY_nrst
  eth.io.EthernetClock125 := outer.port.EthernetClock125
  eth.io.EthernetClock250 := outer.port.EthernetClock250
 /* regmap(
      (ETHCtrl.RegMap(eth, c.regBytes) :_*)
    ) */
    val regmap_val = ETHCtrl.RegMap(eth, c.regBytes)
}

abstract class TLETHCtrlBase( c: ETHCtrlParams, beatBytes:Int)(implicit p: Parameters) extends IORegisterRouter(
      RegisterRouterParams(
        name = "ETHCtrl",
        compat = Seq("juli,ETHCtrl"),
        base = c.address,
        size = 0x40,
        beatBytes = beatBytes),
      new ETHPort(c))
    with HasInterruptSources {

  require(isPow2(c.sizeBytes))
  //require(isPow2(c.sizeBytes))

  val fnode = TLManagerNode(Seq(TLManagerPortParameters(
    managers = Seq(TLManagerParameters(
      address     = Seq(AddressSet(c.fAddress, c.sizeBytes-1)),
      resources   = device.reg("mem"),
      regionType  = RegionType.UNCACHED,
      executable  = false,  // No Executable Code
      supportsGet = TransferSizes(1, 1),  // Transfer Size in Bytes(min, max) 
      supportsPutFull = TransferSizes(1, 1),  // Transfer Size in Bytes(min, max) 
      fifoId      = Some(0))),
    beatBytes = 1)))

  override def nInterrupts = 1
}

class TLETHCtrl(c: ETHCtrlParams, w: Int)(implicit p: Parameters)
    extends TLETHCtrlBase(c,w)(p)
    with HasTLControlRegMap {
  lazy val module = new ETHCtrlModule(c, this) {

    regmap(regmap_val :_*)
  }
}


class AXI4ETHCtrl(params: ETHCtrlParams, beatBytes: Int)(implicit p: Parameters)


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
      Some(ethctrl.map{ case (tmod: Either[BundleBridgeSink[ETHPort], (AXI4ETHCtrl, TLToAXI4)], crossing: TLAsyncCrossingSink) =>
        tmod match{
          /*case Right((mod, toaxi4)) =>{
            val c = mod.module.params
            val io = IO(new ETHPort())
            io <> mod.module.io.port
            /*val wdog = IO(new WDIO( c.Resets ))

            wdog.outputs := mod.module.io.wdog.outputs*/
            //mod.module.io.wdog.clock := clock
            crossing.module.clock := clock
            toaxi4.module.clock := clock
            io
          }*/
          case Left(mod) =>{
            val mIO = mod.makeIO()
            val c = mIO.params
            //val io = IO(new ETHPort(c))
            
            //io <> mIO
            /*val wdog = IO(new WDIO( c.Resets ))

            wdog.outputs := mod.module.io.wdog.outputs*/
            //mod.module.io.wdog.clock := clock
            crossing.module.clock := clock
            mIO
          }
        }
      })
    }
    case None => None
  }
}

object TLETHCtrl {
  val nextId = { var i = -1; () => { i += 1; i} }

  def attach(params: ETHCtrlParams, parameter: Parameters, pBus: PeripheryBus, iBus: InterruptBusWrapper) : (Either[BundleBridgeSink[ETHPort], (AXI4ETHCtrl, TLToAXI4)],TLAsyncCrossingSink) = {
    implicit val p = parameter
    /*if (params.useAXI4){
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
    }else{*/
      val name = s"tlETHCtrl_${nextId()}"
      val crossing = LazyModule(new TLAsyncCrossingSink(AsyncQueueParams.singleton()))
      val ethctrl = LazyModule(new TLETHCtrl(params, pBus.beatBytes)(p))
      ethctrl.suggestName(name)
      val node = ethctrl.controlNode := crossing.node 

      pBus.toVariableWidthSlave(Some(name)) { node := TLAsyncCrossingSource() }  // Pbus -> Connecting to PeripherialBus // sbus -> Systembus
      pBus.coupleTo(s"mem_named_$name") { ethctrl.fnode := TLFragmenter(1, pBus.blockBytes) := TLWidthWidget(pBus.beatBytes) := _} 
      if (ethctrl.nInterrupts > 0){
        iBus.fromSync := IntSyncCrossingSink() := IntSyncCrossingSource(alreadyRegistered = true)  := ethctrl.intnode // Ibus -> Connecting to Interruptsystem
      }
      val ethNode = ethctrl.ioNode.makeSink()//.makeIO()(parameter)  //[ETHPort]
      (Left(ethNode),crossing)
    //}
  }
}