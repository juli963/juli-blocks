package juli.blocks.devices.LiteDRAM_Ctrl
import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.subsystem._
import freechips.rocketchip.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap}
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.regmapper._
import sifive.blocks.util.{SlaveRegIF}
import Interface.Bus.{Wishbone_Master_Bundle, Wishbone_ClockCrosser}
import juli.blocks.tilelink.{Tilelink_ClockCrosser}

case class LiteDRAM_CtrlParams(useAXI4: Boolean = false
, mem_slave_0_address : BigInt, mem_slave_0_sizeBytes : Int
)
{
}

case object LiteDRAM_CtrlKey extends Field[Option[LiteDRAM_CtrlParams]](None)

case object LiteDRAM_CtrlListKey extends Field[Option[Seq[LiteDRAM_CtrlParams]]](None)

class LiteDRAM_CtrlBundle(c:LiteDRAM_CtrlParams) extends Bundle{
	 val params = c
	 val wbus = new Wishbone_Master_Bundle(32,30) 
 	 val dram_clock = Input(Clock())
	 override def cloneType = (new LiteDRAM_CtrlBundle(c)).asInstanceOf[this.type]
}


class TL_LiteDRAM_CtrlModule(c:LiteDRAM_CtrlParams, outer: TL_LiteDRAM_Ctrl) extends LazyModuleImp(outer){
	// outer.port.xxIO := -> Connection to IO
	val (mems_f_0, mems_n_0) = outer.mems_node_0.in(0)
	val tlparams = TLBundleParameters( mems_n_0.bundle.addressBits, mems_n_0.bundle.dataBits, mems_n_0.bundle.sourceBits, mems_n_0.bundle.sinkBits, mems_n_0.bundle.sizeBits)
	
	val wReset = Wire(Bool())
	withClock( outer.port.dram_clock){
		val reg_reset = RegNext(RegNext(RegNext(reset)))
        wReset := reg_reset
	}

	withClockAndReset( outer.port.dram_clock, wReset){
		val mod_litedram_ctrl = Module(new LiteDRAM_Ctrl(c, tlparams))
		mod_litedram_ctrl.io.node <> mems_f_0
		outer.port.wbus <> mod_litedram_ctrl.io.wbus
		/*println("MemorySlave_0 Bus Parameters:")
		println("Data Bits = " + mems_n_0.bundle.dataBits)
		println("Address Bits = " + mems_n_0.bundle.addressBits)
		println("Source Bits = " + mems_n_0.bundle.sourceBits)
		println("Sink Bits = " + mems_n_0.bundle.sinkBits)
		println("Size Bits = " + mems_n_0.bundle.sizeBits)*/

	}

}

abstract class TL_LiteDRAM_CtrlBase( c: LiteDRAM_CtrlParams)(implicit p: Parameters) 
	extends LazyModule //with HasClockDomainCrossing 
{
	require(isPow2(c.mem_slave_0_sizeBytes+1), "Memory Slave_0 Size is not Power of 2")
	require((c.mem_slave_0_address % 8) == 0, "Memory Slave_0 Address is not Aligned")
	val mems_node_0 = TLManagerNode(Seq(TLManagerPortParameters(
		managers = Seq(TLManagerParameters(
			address	= Seq(AddressSet(c.mem_slave_0_address, c.mem_slave_0_sizeBytes)),
			resources	= new SimpleDevice("LiteDRAM_Ctrl", Seq("juli-blocks")).reg("mem"),
			regionType	= RegionType.UNCACHED,
			executable	= false,
			supportsGet = TransferSizes(1, 4),  // Transfer Size in Bytes(min, max) 
			//supportsPutPartial = TransferSizes(1, 4),  // Transfer Size in Bytes(min, max) 
			supportsPutFull = TransferSizes(1, 4),  // Transfer Size in Bytes(min, max) 
			fifoId	= Some(0))),
		beatBytes = 4)))

	val ioNode = BundleBridgeSource(() => new LiteDRAM_CtrlBundle(c).cloneType)
	val port = InModuleBody { ioNode.bundle }
}

class TL_LiteDRAM_Ctrl(c:LiteDRAM_CtrlParams)(implicit p: Parameters) extends TL_LiteDRAM_CtrlBase(c)(p) 
{
	lazy val module = new TL_LiteDRAM_CtrlModule(c, this) {
	}
}

class AXI4_LiteDRAM_Ctrl(params: LiteDRAM_CtrlParams)(implicit p: Parameters)

trait CanHavePeripheryLiteDRAM_CtrlList
{ this: BaseSubsystem =>

	val litedram_ctrl = p(LiteDRAM_CtrlListKey) match {
		case Some(params) => {
		Some(
			params.map{ ps =>
				TL_LiteDRAM_Ctrl.attach(ps, p, pbus, sbus, fbus, ibus)
			})
		}
		case None => None
	}
}

trait CanHavePeripheryLiteDRAM_CtrlListModuleImp extends LazyModuleImp {
	val outer: CanHavePeripheryLiteDRAM_CtrlList

	val LiteDRAM_Ctrl_io = outer.litedram_ctrl match {
		case Some(litedram_ctrl) => { 
		Some(litedram_ctrl.map{ case (tmod: Either[BundleBridgeSink[LiteDRAM_CtrlBundle], (AXI4_LiteDRAM_Ctrl, TLToAXI4)], mems_crossing_0: TLAsyncCrossingSink, litedram_ctrl: TL_LiteDRAM_Ctrl)=>
			tmod match{
				/*case Right((mod, toaxi4_mems_0)) =>{
					val modNode = mod.ioNode.makeSink()
					val mIO = modNode.makeIO()
					val c = mIO.params
					mod.io.clock := clock // Change this to Module Clock
					toaxi4_mems_0.module.clock := clock
					mems_crossing_0.module.clock := mIO.dram_clock
					mIO
				}*/
				case Left(mod) =>{ // TileLink Case
					val litedram_ctrl_mIO = mod.makeIO()
					val c = litedram_ctrl_mIO.params
					
					val wReset = Wire(Bool())
					withClock( litedram_ctrl_mIO.dram_clock){
						val reg_reset = RegNext(RegNext(RegNext(reset)))
						wReset := reg_reset
					}

					mems_crossing_0.module.reset := wReset
					mems_crossing_0.module.clock := litedram_ctrl_mIO.dram_clock
					//litedram_ctrl.module.clock := litedram_ctrl_mIO.dram_clock

					litedram_ctrl_mIO
				}
				}
			})
			}
			case None => None
		}
}

object TL_LiteDRAM_Ctrl {
	val nextId = { var i = -1; () => { i += 1; i} }

	def attach(params: LiteDRAM_CtrlParams, parameter: Parameters, pbus: PeripheryBus,sbus: SystemBus,  fbus: FrontBus , ibus: InterruptBusWrapper) :(Either[BundleBridgeSink[LiteDRAM_CtrlBundle], (AXI4_LiteDRAM_Ctrl, TLToAXI4)], TLAsyncCrossingSink, TL_LiteDRAM_Ctrl) = {
		implicit val p = parameter
		/*if (params.useAXI4){
			val name = s"axi4LiteDRAM_Ctrl_${nextId()}"
			val litedram_ctrl = LazyModule(new AXI4_LiteDRAM_Ctrl(params)(p))
			litedram_ctrl.suggestName(name)
			val toaxi4_mems_0 = LazyModule(new TLToAXI4())
			val mems_crossing_0 = LazyModule(new TLAsyncCrossingSink(AsyncQueueParams.singleton()))
			(Right(litedram_ctrl, toaxi4_mems_0), mems_crossing_0)
		}else{*/
			val name = s"tlLiteDRAM_Ctrl_${nextId()}"
			val litedram_ctrl = LazyModule(new TL_LiteDRAM_Ctrl(params)(p))
			litedram_ctrl.suggestName(name)
			val mems_crossing_0 = LazyModule(new TLAsyncCrossingSink(AsyncQueueParams.singleton()))
			pbus.coupleTo(s"mem_slave_$name") { litedram_ctrl.mems_node_0  := mems_crossing_0.node := TLAsyncCrossingSource() := TLFragmenter(4, pbus.blockBytes) := TLBuffer(BufferParams.default) := TLWidthWidget(pbus.beatBytes) := _} 

			val LiteDRAM_Ctrl_Node = litedram_ctrl.ioNode.makeSink()
			(Left(LiteDRAM_Ctrl_Node), mems_crossing_0, litedram_ctrl)
		//}
	}
	
}