package juli.blocks.DRP_Mem
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
import Xilinx.{DRP_Bundle}


case class DRP_MemParams(useAXI4: Boolean = false, 
drp_width : Int, mem_slave_0_address : BigInt)
{
}

case object DRP_MemKey extends Field[Option[DRP_MemParams]](None)

case object DRP_MemListKey extends Field[Option[Seq[DRP_MemParams]]](None)

class DRP_MemBundle(c:DRP_MemParams) extends Bundle{
	val params = c
	//val drp = Flipped( new DRP_Bundle(c.drp_width) )
    
	val ADDR = Output(UInt(c.drp_width.W))
	val CLK = Output(Clock())
	val EN = Output(Bool())
	val DI = Output(UInt(16.W))
	val RDY = Input(Bool())
	val DO = Input(UInt(16.W))
	val WE = Output(Bool())
	
	override def cloneType = (new DRP_MemBundle(c)).asInstanceOf[this.type]
}

class TL_DRP_MemModule(c:DRP_MemParams, outer: TL_DRP_Mem) extends LazyModuleImp(outer){
	// outer.port.xxIO := -> Connection to IO
	val (mems_f_0, mems_n_0) = outer.mems_node_0.in(0)

	//val mems_0_d_valid = RegInit(false.B)
	//val mems_0_a_ready = RegInit(true.B)  
	//val mems_0_a_channel = RegEnable(mems_f_0.a.bits, mems_f_0.a.fire())  
	//val mems_0_d_channel = RegInit(mems_f_0.d.bits)
	//val mems_f_0.b.valid := Bool(false)
	//val mems_f_0.c.ready := Bool(true)
	//val mems_f_0.e.ready := Bool(true)
	//val mems_f_0.a.ready := a_ready
	//val mems_f_0.d.valid := d_valid

	//println("MemorySlave_0 Bus Parameters:")
	//println("Data Bits = " + mems_n_0.bundle.dataBits)
	//println("Address Bits = " + mems_n_0.bundle.addressBits)
	//println("Source Bits = " + mems_n_0.bundle.sourceBits)
	//println("Sink Bits = " + mems_n_0.bundle.sinkBits)
	//println("Size Bits = " + mems_n_0.bundle.sizeBits)

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


	val drp_we = RegInit(false.B)
	outer.port.WE := drp_we
	val drp_wr_data = RegInit(0.U(30.W))
	outer.port.DI := drp_wr_data
	val drp_address = RegInit(0.U(30.W))
	outer.port.ADDR := drp_address
	val drp_en = RegInit(false.B)
	outer.port.EN := drp_en

	outer.port.CLK := clock


	val transfer_size = RegInit(0.U(mems_n_0.bundle.sizeBits.W))
/* DRP
    val RDY = Output(Bool())
    val DO = Output(UInt(16.W))
*/

	drp_wr_data := mems_f_0.a.bits.data

	val ( s_idle :: s_read0 :: s_read1 :: s_read2 :: s_write :: s_finish  :: Nil) = Enum(6)
	val state = RegInit(s_idle)

	switch(state){
		is(s_idle){
			mems_0_d_valid := false.B
			
			when(mems_f_0.a.fire()){
				when(mems_f_0.a.bits.opcode === 0.U){
					mems_0_a_ready := false.B
					drp_wr_data := mems_f_0.a.bits.data

					drp_en := true.B
					drp_we := true.B

					state := s_write
				}.elsewhen(mems_f_0.a.bits.opcode === 1.U){
					mems_0_a_ready := false.B
					drp_wr_data := mems_f_0.a.bits.data

					drp_en := true.B
					drp_we := true.B

					state := s_write
				}.elsewhen(mems_f_0.a.bits.opcode === 4.U){
					mems_0_a_ready := false.B
					state := s_read0

					drp_en := true.B
					drp_we := false.B
					transfer_size := mems_f_0.a.bits.size

					state := s_read0
				}

				drp_address := (mems_f_0.a.bits.address - c.mem_slave_0_address.U)(c.drp_width-1,0)
			}.otherwise{
				mems_0_a_ready := true.B
			}
		}
		is(s_read0){    // Wait States for Data to get Ready
			drp_en := false.B

			when(~transfer_size.orR() && outer.port.RDY){	// Transfer Size = 0
				state := s_idle
				mems_0_a_ready := true.B
			}
			when(outer.port.RDY && transfer_size.orR()){
				mems_0_d_valid := true.B
				mems_0_d_channel := outer.mems_node_0.edges.in.head.AccessAck(mems_0_a_channel, outer.port.DO) //AccessAckData
			}
			when(mems_f_0.d.fire()){
				mems_0_d_valid := false.B
				when(transfer_size.orR()){
					transfer_size := transfer_size - 1.U
					drp_en := true.B
					drp_address := drp_address + 1.U
				}
			}
		}
		
		is(s_write){
			drp_en := false.B
			drp_we := false.B
			when(outer.port.RDY){
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

}

abstract class TL_DRP_MemBase( c: DRP_MemParams)(implicit p: Parameters) 
	extends LazyModule with HasClockDomainCrossing 
{
	//require(isPow2(c.mem_slave_0_sizeBytes), "Memory Slave_0 Size is not Power of 2")
	require((c.mem_slave_0_address % 8) == 0, "Memory Slave_0 Address is not Aligned")
	val mems_node_0 = TLManagerNode(Seq(TLManagerPortParameters(
		managers = Seq(TLManagerParameters(  //BigDecimal.valueOf(doubleValue).toBigInteger();
			address	= Seq(AddressSet(c.mem_slave_0_address, BigInt((math.ceil(scala.math.pow(2, c.drp_width))).longValue()-1) )),
			resources	= new SimpleDevice("DRP_Mem", Seq("juli-blocks")).reg("mem"),
			regionType	= RegionType.UNCACHED,
			supportsGet = TransferSizes(1, 4),  // Transfer Size in Bytes(min, max) 
			supportsPutPartial = TransferSizes(1, 4),  // Transfer Size in Bytes(min, max) 
			supportsPutFull = TransferSizes(1, 4),  // Transfer Size in Bytes(min, max) 
			executable	= false,
			fifoId	= Some(0))),
		beatBytes = 4)))
	val mem_slave_0_Xing = this.crossIn(mems_node_0)
	val ioNode = BundleBridgeSource(() => new DRP_MemBundle(c) ) 
	val port = InModuleBody { ioNode.bundle }
}

class TL_DRP_Mem(c:DRP_MemParams)(implicit p: Parameters) extends TL_DRP_MemBase(c)(p) 
{
	lazy val module = new TL_DRP_MemModule(c, this) {
	}
}

class AXI4_DRP_Mem(params: DRP_MemParams)(implicit p: Parameters)

trait CanHavePeripheryDRP_MemList
{ this: BaseSubsystem =>

	val drp_mem = p(DRP_MemListKey) match {
		case Some(params) => {
		Some(
			params.map{ ps =>
				TL_DRP_Mem.attach(ps, p, pbus, sbus, fbus, ibus)
			})
		}
		case None => None
	}
}

trait CanHavePeripheryDRP_MemListModuleImp extends LazyModuleImp {
	val outer: CanHavePeripheryDRP_MemList

	val drp_mem_io = outer.drp_mem match {
		case Some(drp_mem) => { 
		Some(drp_mem.map{ case (tmod: Either[BundleBridgeSink[DRP_MemBundle], (AXI4_DRP_Mem, TLToAXI4)])=>
			tmod match{
				/*case Right((mod, toaxi4_mems_0)) =>{
					val modNode = mod.ioNode.makeSink()
					val drp_mem_mIO = modNode.makeIO()
					val c = drp_mem_mIO.params
					mod.io.clock := clock // Change this to Module Clock
					toaxi4_mems_0.module.clock := clock
					drp_mem_mIO
				}*/
					case Left(mod) =>{ // TileLink Case
						val drp_mem_mIO = mod.makeIO()
						val c = drp_mem_mIO.params
						drp_mem_mIO
					}
				}
			})
			}
			case None => None
		}
}

object TL_DRP_Mem {
	val nextId = { var i = -1; () => { i += 1; i} }

def attach(params: DRP_MemParams, parameter: Parameters, pbus: PeripheryBus,sbus: SystemBus,  fbus: FrontBus , ibus: InterruptBusWrapper) :(Either[BundleBridgeSink[DRP_MemBundle], (AXI4_DRP_Mem, TLToAXI4)]) = {
implicit val p = parameter
	/*if (params.useAXI4){
		val name = s"axi4DRP_Mem_${nextId()}"
		val drp_mem = LazyModule(new AXI4_DRP_Mem(params)(p))
		drp_mem.suggestName(name)
		val toaxi4_mems_0 = LazyModule(new TLToAXI4())
		(Right(DRP_Mem, toaxi4_mems_0))
	}else{*/
		val name = s"tlDRP_Mem_${nextId()}"
		val drp_mem = LazyModule(new TL_DRP_Mem(params)(p))
		drp_mem.suggestName(name)
		pbus.coupleTo(s"mem_slave_$name") { drp_mem.mem_slave_0_Xing(NoCrossing) := TLFragmenter(4, pbus.blockBytes) := TLBuffer(BufferParams(8), BufferParams.none) := TLWidthWidget(pbus.beatBytes) := _} 

		val DRP_Mem_Node = drp_mem.ioNode.makeSink()
		(Left(DRP_Mem_Node))
	//}
}
}
