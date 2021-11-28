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


case class ESP_FIFOParams(useAXI4: Boolean = false
, reg_slave_0_address : BigInt, reg_slave_0_sizeBytes : Int = 64//0x10
)
{
}

case object ESP_FIFOKey extends Field[Option[ESP_FIFOParams]](None)

case object ESP_FIFOListKey extends Field[Option[Seq[ESP_FIFOParams]]](None)

class ESP_FIFOBundle(c:ESP_FIFOParams) extends Bundle{
	 val params = c
	 override def cloneType = (new ESP_FIFOBundle(c)).asInstanceOf[this.type]
}

class TL_ESP_FIFOModule(c:ESP_FIFOParams, outer: TL_ESP_FIFO) extends LazyModuleImp(outer){
	val wreg_0 = Wire(new SlaveRegIF(32))
	val wreg_1 = Wire(new SlaveRegIF(32))
	val wreg_2 = Wire(new SlaveRegIF(32))
	
	val Data_av = RegEnable(wreg_0.write.bits(26), wreg_0.write.valid)
	val Get_new_data = RegEnable(wreg_0.write.bits(25), wreg_0.write.valid)
	val Data_readed = RegInit(false.B)
	val Send_ack = RegEnable(wreg_0.write.bits(17), wreg_0.write.valid)
	val Send_nack = RegEnable(wreg_0.write.bits(16), wreg_0.write.valid)

	val Proto_type = RegInit(0.U(8.W))
	val Proto_cmd = RegInit(0.U(8.W))
	val Proto_address = RegInit(0.U(8.W))
	val Proto_data = RegInit(0.U(32.W))

	wreg_0.read := Cat(Seq(0.U(5.W), Data_av, Get_new_data, Data_readed, 0.U(6.W), Send_ack , Send_nack, 0.U(8.W), 0.U(8.W)))
	wreg_1.read := Cat(Seq(Proto_address, Proto_cmd, 0.U(8.W), Proto_type)) 
	wreg_2.read := Proto_data


	def reg_0_desc: RegFieldDesc = RegFieldDesc(s"FIFO_Ctrl", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)
	def reg_1_desc: RegFieldDesc = RegFieldDesc(s"ESP_Header", s"RegDesc", access=RegFieldAccessType.R, volatile=true)
	def reg_2_desc: RegFieldDesc = RegFieldDesc(s"ESP_Data", s"RegDesc", access=RegFieldAccessType.R, volatile=true)

	val regmap_0 = Seq(
			0 -> Seq(wreg_0.toRegField(Some( reg_0_desc ))),
			4 -> Seq(wreg_1.toRegField(Some( reg_1_desc ))),
			8 -> Seq(wreg_2.toRegField(Some( reg_2_desc )))
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