package juli.blocks.devices.ESP_FIFO
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
import Ethernet.Protocol._

case class ESP_FIFOParams(useAXI4: Boolean = false
, reg_slave_0_address : BigInt, reg_slave_0_sizeBytes : Int = 64//0x10
)
{
}

case object ESP_FIFOKey extends Field[Option[ESP_FIFOParams]](None)

case object ESP_FIFOListKey extends Field[Option[Seq[ESP_FIFOParams]]](None)

class ESP_FIFOBundle(c:ESP_FIFOParams) extends Bundle{
	val params = c
	val UDPBus = new Ethernet.Protocol.OSI4.UDPBus(8)   // Fix to 8
	val clkUDP = Input(Clock())

	override def cloneType = (new ESP_FIFOBundle(c)).asInstanceOf[this.type]
}

class TL_ESP_FIFOModule(c:ESP_FIFOParams, outer: TL_ESP_FIFO) extends LazyModuleImp(outer){
	val mod = Module(new ESP_FIFO())
	mod.io.UDPBus <> io.UDPBus
	mod.io.clkUDP := io.clkUDP

	val wreg_0 = Wire(new SlaveRegIF(32))
	val wreg_1 = Wire(new SlaveRegIF(32))
	val wreg_2 = Wire(new SlaveRegIF(32))
	val wreg_3 = Wire(new SlaveRegIF(32))
	val wreg_4 = Wire(new SlaveRegIF(32))
	
	val Get_new_data = RegEnable(wreg_0.write.bits(25), wreg_0.write.valid)
	val Get_new_data_ff = RegNext(Get_new_data)
	val deq = Get_new_data && ~Get_new_data_ff
	val Data_av = ~mod.io.deq_empty
	val Data_readed = RegInit(false.B)

	val Send = RegEnable(wreg_0.write.bits(17), wreg_0.write.valid)
	val Send_ff = RegNext(Get_new_data)
	val enq = Send && ~Send_ff
	val Full = mod.io.enq_full

	val trans_type = RegEnable(wreg_3.write.bits(7,0), wreg_3.write.valid)
	val trans_cmd = RegEnable(wreg_3.write.bits(23,16), wreg_3.write.valid)
	val trans_address = RegEnable(wreg_3.write.bits(31,24), wreg_3.write.valid)
	val trans_Ident = RegEnable(wreg_3.write.bits(15,8), wreg_3.write.valid)
	val trans_Data = RegEnable(wreg_4.write.bits, wreg_4.write.valid)

	wreg_0.read := Cat(Seq(0.U(5.W), Data_av, Get_new_data, Data_readed, 0.U(6.W), Send, Full, 0.U(8.W), 0.U(8.W)))

	// RX Registers
	wreg_1.read := Cat(Seq(mod.io.recv_address, mod.io.recv_cmd, mod.io.recv_Ident, mod.io.recv_type)) 
	wreg_2.read := mod.io.recv_Data

	// TX Registers
	wreg_3.read := Cat(Seq(trans_address, trans_cmd, trans_Ident, trans_type)) 
	wreg_4.read := trans_Data

	mod.io.deq_Data := deq
	mod.io.enq_Data := enq

	when(mod.io.deq_Data_new){
		Data_readed := false.B
	}

	def reg_0_desc: RegFieldDesc = RegFieldDesc(s"ESP_Ctrl", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)
	def reg_1_desc: RegFieldDesc = RegFieldDesc(s"ESP_RX_Header", s"RegDesc", access=RegFieldAccessType.R, volatile=true)
	def reg_2_desc: RegFieldDesc = RegFieldDesc(s"ESP_RX_Data", s"RegDesc", access=RegFieldAccessType.R, volatile=true)
	def reg_3_desc: RegFieldDesc = RegFieldDesc(s"ESP_TX_Header", s"RegDesc", access=RegFieldAccessType.RW, volatile=false)
	def reg_4_desc: RegFieldDesc = RegFieldDesc(s"ESP_TX_Data", s"RegDesc", access=RegFieldAccessType.RW, volatile=false)

	def reg_1_field: RegField = RegField(32, 
											RegReadFn(ready => {Data_readed := true.B; (Bool(true), wreg_1.read)} ), 
											RegWriteFn((v, d) => wreg_1.writeFn(v, d)), reg_1_desc)
	def reg_2_field: RegField = RegField(32, 
											RegReadFn(ready => {Data_readed := true.B; (Bool(true), wreg_2.read)} ), 
											RegWriteFn((v, d) => wreg_2.writeFn(v, d)), reg_2_desc)

	val regmap_0 = Seq(
			0 -> Seq(wreg_0.toRegField(Some( reg_0_desc ))),
			4 -> Seq(reg_1_field),
			8 -> Seq(reg_2_field),
			12 -> Seq(wreg_3.toRegField(Some( reg_3_desc ))),
			16 -> Seq(wreg_4.toRegField(Some( reg_4_desc )))
		 )
}

abstract class TL_ESP_FIFOBase( c: ESP_FIFOParams)(implicit p: Parameters) 
	extends IORegisterRouter(
	RegisterRouterParams(
		name = "ESP_FIFO",
		compat = Seq("juli-blocks,ESP_FIFO"),
		base = c.reg_slave_0_address,
		size = c.reg_slave_0_sizeBytes,
		beatBytes = 4),
	new ESP_FIFOBundle(c))
{

}

class TL_ESP_FIFO(c:ESP_FIFOParams)(implicit p: Parameters) extends TL_ESP_FIFOBase(c)(p) 
	with HasTLControlRegMap {

	lazy val module = new TL_ESP_FIFOModule(c, this) {
		regmap(regmap_0 :_*)
	}
}

class AXI4_ESP_FIFO(params: ESP_FIFOParams)(implicit p: Parameters)

trait CanHavePeripheryESP_FIFOList
{ this: BaseSubsystem =>

	val ESP_FIFO = p(ESP_FIFOListKey) match {
		case Some(params) => {
		Some(
			params.map{ ps =>
				TL_ESP_FIFO.attach(ps, p, pbus, sbus, fbus, ibus)
			})
		}
		case None => None
	}
}

trait CanHavePeripheryESP_FIFOListModuleImp extends LazyModuleImp {
	val outer: CanHavePeripheryESP_FIFOList
}

object TL_ESP_FIFO {
	val nextId = { var i = -1; () => { i += 1; i} }

	def attach(params: ESP_FIFOParams, parameter: Parameters, pbus: PeripheryBus,sbus: SystemBus, fbus: FrontBus , iBus: InterruptBusWrapper) :(Either[BundleBridgeSink[ESP_FIFOBundle], (AXI4_ESP_FIFO, TLToAXI4)]) = {
		implicit val p = parameter
		/*if (params.useAXI4){
			val name = s"axi4ESP_FIFO_${nextId()}"
			val ESP_FIFO = LazyModule(new AXI4_ESP_FIFO(params,new LazyModule(this))(p))
			//val ESP_FIFO = new LazyModuleImp(this)
			ESP_FIFO.suggestName(name)
			val toaxi4_reg_0 = LazyModule(new TLToAXI4())
			(Right(ESP_FIFO, toaxi4_reg_0))
		}else{*/
			val name = s"tlESP_FIFO_${nextId()}"
			val ESP_FIFO = LazyModule(new TL_ESP_FIFO(params)(p))
			ESP_FIFO.suggestName(name)
			val reg_node_0 = ESP_FIFO.controlNode
			pbus.toVariableWidthSlave(Some(name)) { reg_node_0  } 
			val ESP_FIFO_Node = ESP_FIFO.ioNode.makeSink()
			(Left(ESP_FIFO_Node))
		//}
	}
}

// java -jar rocket-chip/sbt-launch.jar ++2.12.4 "runMain freechips_copy.rocketchip.util.AsyncQueue"
object AsyncQueue extends App {
  chisel3.Driver.execute(Array("--target-dir", "generated/AsyncQueue"), () => new AsyncQueue(UInt(8.W), AsyncQueueParams(depth = 8)))
}