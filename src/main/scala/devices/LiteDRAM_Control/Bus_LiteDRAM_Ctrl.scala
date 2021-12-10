package juli.blocks.devices.LiteDRAM_Ctrl
import chisel3._
import chisel3.util._
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
import Interface.Bus.{Wishbone_Master_Bundle}


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

	val mems_0_d_valid = RegInit(false.B)
	val mems_0_a_ready = RegInit(true.B)  
	val mems_0_a_channel = RegEnable(mems_f_0.a.bits, mems_f_0.a.fire())  
	val mems_0_d_channel = RegInit(mems_f_0.d.bits)
	mems_f_0.b.valid := false.B
	mems_f_0.c.ready := true.B
	mems_f_0.e.ready := true.B
	mems_f_0.a.ready := mems_0_a_ready
	mems_f_0.d.valid := mems_0_d_valid
	mems_f_0.d.bits := mems_0_d_channel

	val wbus_cyc = RegInit(false.B)
	outer.port.wbus.cyc := wbus_cyc
	val wbus_sel = RegInit(false.B)
	outer.port.wbus.sel := wbus_sel
	val wbus_strobe = RegInit(false.B)
	outer.port.wbus.strobe := wbus_strobe
	val wbus_we = RegInit(false.B)
	outer.port.wbus.we := wbus_we
	val wbus_address = RegInit(0.U(30.W))
	outer.port.wbus.address := wbus_address
	val wbus_wr_data = RegInit(0.U(30.W))
	outer.port.wbus.wr_data := wbus_wr_data

	outer.port.wbus.cti := 0.U
	outer.port.wbus.bte := 0.U


	val transfer_size = RegInit(0.U(mems_n_0.bundle.sizeBits.W))
/* WBUS
	val address = Output(UInt(Addrwidth.W))
	val wr_data = Output(UInt(Datawidth.W))
	val rd_data = Input(UInt(Datawidth.W))
	
	val sel = Output(UInt((Datawidth/8).W))	// Valid Bytes 
	val cyc = Output(Bool())
	val strobe = Output(Bool())
	val ack = Input(Bool())
	val we = Output(Bool())
	
	val cti = Output(UInt(3.W))	// Cycle Type Ident
	val bte = Output(UInt(2.W))	// Burst Type Extension
		
	val err = Input(Bool())
*/

	outer.port.wbus.wr_data := mems_f_0.a.bits.data

	val ( s_idle :: s_read0 :: s_read1 :: s_read2 :: s_write :: s_finish  :: Nil) = Enum(6)
	val state = RegInit(s_idle)

	switch(state){
		is(s_idle){
			mems_0_d_valid := false.B
			
			when(mems_f_0.a.fire()){
				when(mems_f_0.a.bits.opcode === 0.U){
					mems_0_a_ready := false.B
					wbus_wr_data := mems_f_0.a.bits.data

					wbus_cyc := true.B
					wbus_strobe := true.B
					wbus_we := true.B

					state := s_write
				}.elsewhen(mems_f_0.a.bits.opcode === 1.U){
					mems_0_a_ready := false.B
					wbus_wr_data := mems_f_0.a.bits.data

					wbus_cyc := true.B
					wbus_strobe := true.B
					wbus_we := true.B

					state := s_write
				}.elsewhen(mems_f_0.a.bits.opcode === 4.U){
					mems_0_a_ready := false.B
					state := s_read0

					wbus_cyc := true.B
					wbus_strobe := true.B
					wbus_we := false.B
					transfer_size := mems_f_0.a.bits.size

					state := s_read0
				}

				wbus_sel := mems_f_0.a.bits.mask
				wbus_address := mems_f_0.a.bits.address - c.mem_slave_0_address.U
			}.otherwise{
				mems_0_a_ready := true.B
			}
		}
		is(s_read0){    // Wait States for Data to get Ready
			when(~transfer_size.orR()){	// Transfer Size = 0
				state := s_idle
				wbus_cyc := false.B
				wbus_strobe := false.B
				mems_0_a_ready := true.B
			}
			when(outer.port.wbus.ack && transfer_size.orR()){
				wbus_strobe := false.B
				mems_0_d_valid := true.B
				mems_0_d_channel := outer.mems_node_0.edges.in.head.AccessAck(mems_0_a_channel, outer.port.wbus.rd_data) //AccessAckData
			}
			when(mems_f_0.d.fire()){
				mems_0_d_valid := false.B
				when(transfer_size.orR()){
					transfer_size := transfer_size - 1.U
					wbus_strobe := true.B
					wbus_address := wbus_address + 1.U
				}
			}
		}
		
		is(s_write){
			when(outer.port.wbus.ack){
				wbus_cyc := false.B
				wbus_strobe := false.B

				mems_0_d_channel := outer.mems_node_0.edges.in.head.AccessAck(mems_0_a_channel)  //AccessAck
				mems_0_d_valid := true.B
				state := s_finish
			}
			
		}
		is(s_finish){
			when(mems_f_0.d.fire()){
				mems_0_d_valid := false.B
				mems_0_a_ready := true.B
				state := s_idle
			}
		}
	}

	// f.a.fire() valid and ready = 1
	// d_channel := outer.fnode.edges.in.head.AccessAck(a_channel, eth.io.RXrddata) //AccessAckData
	// d_channel := outer.fnode.edges.in.head.AccessAck(a_channel)  //AccessAck
	// when(f.a.bits.opcode === 0.U) -> Write Full
	// when(f.a.bits.opcode === 1.U) -> Write Partial
    //(f.a.bits.opcode === 4.U){ -> Read

	/* Wishbone -> 
		Write -> 
			1. Set Address, we, sel, cyc and stb, cti and bte and data
			2. Wait for Ack set
			3. Send next data and Change address, and set sel new
			4. Check Ack
			5. Insert Wait State with stb = 0 -> Ack will go low
			6. End Transmission with STB = 0 and Cyc = 0
		Read -> 
			1. Set Address, we=0, sel, cyc and stb, cti and bte
			2. Latch Data when Ack is set 
			3. Change address, and set sel new
			4. Check Ack
			5. Insert Wait State with stb = 0 -> Ack will go low
			6. End Transmission with STB = 0 and Cyc = 0

		Err will report abnormal cycle abort

		cti = 000 constant address
		cti = 010 incremental address
		
		bte = 00 linear burst
		bte = 01 4beat wrapped burst
	*/

	 //println("MemorySlave_0 Bus Parameters:")
	 //println("Data Bits = " + mems_n_0.bundle.dataBits)
	 //println("Address Bits = " + mems_n_0.bundle.addressBits)
	 //println("Source Bits = " + mems_n_0.bundle.sourceBits)
	 //println("Sink Bits = " + mems_n_0.bundle.sinkBits)
	 //println("Size Bits = " + mems_n_0.bundle.sizeBits)

}

abstract class TL_LiteDRAM_CtrlBase( c: LiteDRAM_CtrlParams)(implicit p: Parameters) 
	extends LazyModule with HasClockDomainCrossing 
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
			supportsPutPartial = TransferSizes(1, 4),  // Transfer Size in Bytes(min, max) 
			supportsPutFull = TransferSizes(1, 4),  // Transfer Size in Bytes(min, max) 
			fifoId	= Some(0))),
		beatBytes = 4)))
	//val mem_slave_0_Xing = this.crossIn(mems_node_0)

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
		Some(litedram_ctrl.map{ case (tmod: Either[BundleBridgeSink[LiteDRAM_CtrlBundle], (AXI4_LiteDRAM_Ctrl, TLToAXI4)], mems_crossing_0: TLAsyncCrossingSink)=>
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
					mems_crossing_0.module.clock := litedram_ctrl_mIO.dram_clock
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

	def attach(params: LiteDRAM_CtrlParams, parameter: Parameters, pbus: PeripheryBus,sbus: SystemBus,  fbus: FrontBus , ibus: InterruptBusWrapper) :(Either[BundleBridgeSink[LiteDRAM_CtrlBundle], (AXI4_LiteDRAM_Ctrl, TLToAXI4)], TLAsyncCrossingSink) = {
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
			//val node = litedram_ctrl.mem_slave_0_Xing(NoCrossing) := mems_crossing_0.node
			sbus.coupleTo(s"mem_slave_$name") { litedram_ctrl.mems_node_0 := mems_crossing_0.node := TLAsyncCrossingSource() := TLFragmenter(4, sbus.blockBytes) := TLBuffer(BufferParams(8), BufferParams.none) := TLWidthWidget(sbus.beatBytes) := _} 
	//AsynchronousCrossing()
			val LiteDRAM_Ctrl_Node = litedram_ctrl.ioNode.makeSink()
			(Left(LiteDRAM_Ctrl_Node), mems_crossing_0)
		//}
	}
	
}
