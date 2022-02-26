package juli.blocks.HSControl
import chisel3._

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

import Xilinx.GTP.{PLL_Ctrl}
import chisel3.util._
import device.hssniffer.{Conf_trigger_bundle}

case class HSControlParams(useAXI4: Boolean = false, 
num_channels : Int = 4, num_refclk : Int = 2, num_protocols : Int = 1
, reg_slave_0_address : BigInt, reg_slave_0_sizeBytes : Int = 512
)
{
}

case object HSControlKey extends Field[Option[HSControlParams]](None)

case object HSControlListKey extends Field[Option[Seq[HSControlParams]]](None)

class HSControlBundle(c:HSControlParams) extends Bundle{
	val params = c
	
	val gtp_regs = Flipped(new Bundle{ 
		val gtp_sel = Input(Vec(c.num_channels, UInt(log2Ceil(c.num_channels+1).W)))   
		//val pll_ctrl = Vec( c.num_refclk, (new PLL_Ctrl))  
		// GTP Status 
		val rx_selclk    = Vec(c.num_channels, Input(UInt(2.W)))   
		val tx_selclk    = Vec(c.num_channels, Input(UInt(2.W)))  

		val gtp_running = Vec(c.num_channels, Output(Bool())) 

		val protocol_sel = Vec(c.num_channels, Input(UInt(log2Ceil(c.num_protocols+1).W))) 
	})
	
	val c8b10b_regs = Flipped(Vec(c.num_channels, new Bundle{
		val trigger = new Conf_trigger_bundle(num_triggers = 2)	
		val data_rate = Input(UInt(3.W))   
		val rxpolarity = Input(Bool())     
	}))

	val gtp_clock = Input(Clock())

	override def cloneType = (new HSControlBundle(c)).asInstanceOf[this.type]
}

class TL_HSControlModule(c:HSControlParams, outer: TL_HSControl) extends LazyModuleImp(outer){
	 
	// GTP MUX
		val wmux_reg_0 = Wire(new SlaveRegIF(16))
		val wrx_clock_reg_0 = Wire(new SlaveRegIF(8))
		val wtx_clock_reg_0 = Wire(new SlaveRegIF(8))
		val wpllctrl_reg_0 = Wire(new SlaveRegIF(13))
		val wgtpstatus_reg_0 = Wire(new SlaveRegIF(4))
		val wprotocolsel_reg_0 = Wire(new SlaveRegIF(32))

		val gtp_mux_io = outer.port.gtp_regs.gtp_sel //Seq.tabulate(num_channels)( n =>  )
		val mux_reg_0 = RegEnable(wmux_reg_0.write.bits, wmux_reg_0.write.valid)
		wmux_reg_0.read := Cat(Seq( gtp_mux_io(3)(2,0) ,gtp_mux_io(2)(2,0) ,gtp_mux_io(1)(2,0) ,gtp_mux_io(0)(2,0) )) 
		gtp_mux_io(0) := mux_reg_0(0)
		gtp_mux_io(1) := mux_reg_0(1)
		gtp_mux_io(2) := mux_reg_0(2)
		gtp_mux_io(3) := mux_reg_0(3)

		val rx_clock_io = outer.port.gtp_regs.rx_selclk
		val rx_clock_reg_0 = RegEnable(wrx_clock_reg_0.write.bits, wrx_clock_reg_0.write.valid)
		wrx_clock_reg_0.read := rx_clock_reg_0	//Cat(Seq( rx_clock_io(3)(1,0) ,rx_clock_io(2)(1,0) ,rx_clock_io(1)(1,0) ,rx_clock_io(0)(1,0) )) 
		rx_clock_io(0) := 0.U//rx_clock_io(0)
		rx_clock_io(1) := 0.U//rx_clock_io(1)
		rx_clock_io(2) := 0.U//rx_clock_io(2)
		rx_clock_io(3) := 0.U//rx_clock_io(3)

		val tx_clock_io = outer.port.gtp_regs.tx_selclk
		val tx_clock_reg_0 = RegEnable(wtx_clock_reg_0.write.bits, wtx_clock_reg_0.write.valid)
		wtx_clock_reg_0.read := tx_clock_reg_0	//Cat(Seq( tx_clock_io(3)(1,0) ,tx_clock_io(2)(1,0) ,tx_clock_io(1)(1,0) ,tx_clock_io(0)(1,0) )) 
		tx_clock_io(0) := 0.U//tx_clock_reg_0(0)
		tx_clock_io(1) := 0.U//tx_clock_reg_0(1)
		tx_clock_io(2) := 0.U//tx_clock_reg_0(2)
		tx_clock_io(3) := 0.U//tx_clock_reg_0(3)

		//val pll_io = outer.port.gtp_regs.plls_ctrl
		val pllctrl_reg_0 = RegEnable(wpllctrl_reg_0.write.bits, wpllctrl_reg_0.write.valid)
		//wpllctrl_reg_0.read := Cat(Seq( pll_io(1).REFCLKSEL ,pll_io(1).LOCK ,pll_io(1).REFCLKLOST , 0.U(3.W) ,pll_io(0).REFCLKSEL ,pll_io(0).LOCK ,pll_io(0).REFCLKLOST )) 
		wpllctrl_reg_0.read := 0.U
		//pll_io(0).REFCLKSEL := pllctrl_reg_0(0)(4,2)
		//pll_io(1).REFCLKSEL := pllctrl_reg_0(1)(12,10)

		val gtpstatus_io = outer.port.gtp_regs.gtp_running
		val gtpstatus_reg_0 = RegEnable(wgtpstatus_reg_0.write.bits, wgtpstatus_reg_0.write.valid)
		wgtpstatus_reg_0.read := gtpstatus_reg_0//Cat(Seq( pll_io(1).REFCLKSEL ,pll_io(1).LOCK ,pll_io(1).REFCLKLOST  ,pll_io(0).REFCLKSEL ,pll_io(0).LOCK ,pll_io(0).REFCLKLOST )) 
		gtpstatus_io(0) := gtpstatus_reg_0(0)
		gtpstatus_io(1) := gtpstatus_reg_0(1)
		gtpstatus_io(2) := gtpstatus_reg_0(2)
		gtpstatus_io(3) := gtpstatus_reg_0(3)

		val protocolsel_io = outer.port.gtp_regs.protocol_sel
		val protocolsel_reg_0 = RegEnable(wprotocolsel_reg_0.write.bits, wprotocolsel_reg_0.write.valid)
		wprotocolsel_reg_0.read := protocolsel_reg_0//Cat(Seq( pll_io(1).REFCLKSEL ,pll_io(1).LOCK ,pll_io(1).REFCLKLOST  ,pll_io(0).REFCLKSEL ,pll_io(0).LOCK ,pll_io(0).REFCLKLOST )) 
		protocolsel_io(0) := protocolsel_reg_0(7,0)
		protocolsel_io(1) := protocolsel_reg_0(15,8)
		protocolsel_io(2) := protocolsel_reg_0(23,16)
		protocolsel_io(3) := protocolsel_reg_0(31,24)


		def mux_reg_0_desc: RegFieldDesc = RegFieldDesc(s"GTP_Mux", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)
		def rx_clock_reg_0_desc: RegFieldDesc = RegFieldDesc(s"GTP_RX_Clocksel", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)
		def tx_clock_reg_0_desc: RegFieldDesc = RegFieldDesc(s"GTP_TX_Clocksel", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)
		def pllctrl_reg_0_desc: RegFieldDesc = RegFieldDesc(s"GTP_PLLControl", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)
		def gtpstatus_reg_0_desc: RegFieldDesc = RegFieldDesc(s"GTP_Status", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)
		def protocolsel_reg_0_desc: RegFieldDesc = RegFieldDesc(s"GTP_Protocol", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)
	
	// 8b10b
		val wtrig_dec_control_reg_0 = Wire(Vec(c.num_channels, new SlaveRegIF(32)))
		//val wtrig_dec_status_reg_0 = Wire(new SlaveRegIF(16))
		val wtrig_dec_trigger_reg_0 = Wire(Vec(c.num_channels, new SlaveRegIF(32)))
		val wtrig_dec_trigger_reg_1 = Wire(Vec(c.num_channels, new SlaveRegIF(32)))
		
		val trig_dec_io = outer.port.c8b10b_regs
		val trig_dec_control_reg_0 = for (i <- 0 until c.num_channels ) yield ( RegEnable(wtrig_dec_control_reg_0(i).write.bits, wtrig_dec_control_reg_0(i).write.valid) )
		for(i <- 0 until c.num_channels){
			wtrig_dec_control_reg_0(i).read := trig_dec_control_reg_0(i) 
			trig_dec_io(i).trigger.protocol_en := trig_dec_control_reg_0(i)(0) 
			trig_dec_io(i).trigger.scrambler_en := trig_dec_control_reg_0(i)(1) 
			trig_dec_io(i).trigger.trigger_mode(0) := trig_dec_control_reg_0(i)(17,16) 
			trig_dec_io(i).trigger.trigger_mode(1) := trig_dec_control_reg_0(i)(19,18) 
			trig_dec_io(i).data_rate := trig_dec_control_reg_0(i)(5,3)
			trig_dec_io(i).rxpolarity := trig_dec_control_reg_0(i)(2)
		}
		
		val trig_dec_trigger_reg_0 = for (i <- 0 until c.num_channels ) yield ( RegEnable(wtrig_dec_trigger_reg_0(i).write.bits, wtrig_dec_trigger_reg_0(i).write.valid) )
		for(i <- 0 until c.num_channels){
			wtrig_dec_trigger_reg_0(i).read := trig_dec_trigger_reg_0(i)
			trig_dec_io(i).trigger.trigger_start(0).data	:= trig_dec_trigger_reg_0(i)(7,0)
			trig_dec_io(i).trigger.trigger_start(0).charisk	:= trig_dec_trigger_reg_0(i)(16)
			trig_dec_io(i).trigger.trigger_stop(0).data		:= trig_dec_trigger_reg_0(i)(15,8)
			trig_dec_io(i).trigger.trigger_stop(0).charisk	:= trig_dec_trigger_reg_0(i)(24)
		}

		val trig_dec_trigger_reg_1 = for (i <- 0 until c.num_channels ) yield ( RegEnable(wtrig_dec_trigger_reg_1(i).write.bits, wtrig_dec_trigger_reg_1(i).write.valid) )
		for(i <- 0 until c.num_channels){
			wtrig_dec_trigger_reg_1(i).read := trig_dec_trigger_reg_1(i)
			trig_dec_io(i).trigger.trigger_start(1).data	:= trig_dec_trigger_reg_1(i)(7,0)
			trig_dec_io(i).trigger.trigger_start(1).charisk	:= trig_dec_trigger_reg_1(i)(16)
			trig_dec_io(i).trigger.trigger_stop(1).data		:= trig_dec_trigger_reg_1(i)(15,8)
			trig_dec_io(i).trigger.trigger_stop(1).charisk	:= trig_dec_trigger_reg_1(i)(24)
		}

		def trig_decoder_control_reg_0_desc: RegFieldDesc = RegFieldDesc(s"8b10b_Control", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)
		//def trig_decoder_status_reg_0_desc: RegFieldDesc = RegFieldDesc(s"8b10b_Status", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)
		def trig_decoder_trigger_reg_0_desc: RegFieldDesc = RegFieldDesc(s"8b10b_Trigger_0", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)
		def trig_decoder_trigger_reg_1_desc: RegFieldDesc = RegFieldDesc(s"8b10b_Trigger_1", s"RegDesc", access=RegFieldAccessType.RW, volatile=true)


	val regmap_0 = Seq(
			/* GTP Multiplexer */
			0x00 -> Seq(wmux_reg_0.toRegField(Some( mux_reg_0_desc ))),
			0x04*4 -> Seq(wrx_clock_reg_0.toRegField(Some( rx_clock_reg_0_desc ))),
			0x06*4 -> Seq(wtx_clock_reg_0.toRegField(Some( tx_clock_reg_0_desc ))),
			0x08*4 -> Seq(wpllctrl_reg_0.toRegField(Some( pllctrl_reg_0_desc ))),
			0x0B*4 -> Seq(wgtpstatus_reg_0.toRegField(Some( gtpstatus_reg_0_desc ))),
			0x10*4 -> Seq(wprotocolsel_reg_0.toRegField(Some( protocolsel_reg_0_desc ))),

			/* 8b10b Trigger 4Channel */
			0x20*4 -> Seq(wtrig_dec_control_reg_0(0).toRegField(Some( trig_decoder_control_reg_0_desc ))),
			0x22*4 -> Seq(wtrig_dec_trigger_reg_0(0).toRegField(Some( trig_decoder_trigger_reg_0_desc ))),
			0x23*4 -> Seq(wtrig_dec_trigger_reg_1(0).toRegField(Some( trig_decoder_trigger_reg_1_desc ))),

			0x30*4 -> Seq(wtrig_dec_control_reg_0(1).toRegField(Some( trig_decoder_control_reg_0_desc ))),
			0x32*4 -> Seq(wtrig_dec_trigger_reg_0(1).toRegField(Some( trig_decoder_trigger_reg_0_desc ))),
			0x33*4 -> Seq(wtrig_dec_trigger_reg_1(1).toRegField(Some( trig_decoder_trigger_reg_1_desc ))),

			0x40*4 -> Seq(wtrig_dec_control_reg_0(2).toRegField(Some( trig_decoder_control_reg_0_desc ))),
			0x42*4 -> Seq(wtrig_dec_trigger_reg_0(2).toRegField(Some( trig_decoder_trigger_reg_0_desc ))),
			0x43*4 -> Seq(wtrig_dec_trigger_reg_1(2).toRegField(Some( trig_decoder_trigger_reg_1_desc ))),

			0x50*4 -> Seq(wtrig_dec_control_reg_0(3).toRegField(Some( trig_decoder_control_reg_0_desc ))),
			0x52*4 -> Seq(wtrig_dec_trigger_reg_0(3).toRegField(Some( trig_decoder_trigger_reg_0_desc ))),
			0x53*4 -> Seq(wtrig_dec_trigger_reg_1(3).toRegField(Some( trig_decoder_trigger_reg_1_desc ))),

		 )
}

abstract class TL_HSControlBase( c: HSControlParams)(implicit p: Parameters) 
	extends IORegisterRouter(
	RegisterRouterParams(
		name = "HSControl_reg_0",
		compat = Seq("juli-blocks,HSControl_reg_0"),
		base = c.reg_slave_0_address,
		size = c.reg_slave_0_sizeBytes,
		beatBytes = 4),
	new HSControlBundle(c))
{
}

class TL_HSControl(c:HSControlParams)(implicit p: Parameters) extends TL_HSControlBase(c)(p) 
	with HasTLControlRegMap {
	lazy val module = new TL_HSControlModule(c, this) {
		regmap(regmap_0 :_*)
		//regmap( )
	}
}

class AXI4_HSControl(params: HSControlParams)(implicit p: Parameters)

trait CanHavePeripheryHSControlList
{ this: BaseSubsystem =>

	val hscontrol = p(HSControlListKey) match {
		case Some(params) => {
		Some(
			params.map{ ps =>
				TL_HSControl.attach(ps, p, pbus, sbus, fbus, ibus)
			})
		}
		case None => None
	}
}

trait CanHavePeripheryHSControlListModuleImp extends LazyModuleImp {
	val outer: CanHavePeripheryHSControlList

	val HSControl_io = outer.hscontrol match {
		case Some(hscontrol) => { 
			Some(hscontrol.map{ case (tmod: Either[BundleBridgeSink[HSControlBundle], (AXI4_HSControl, TLToAXI4)], reg_crossing_0: TLAsyncCrossingSink)=>
				tmod match{
					// Uncomment AXI Else -> Seq[Any] Type
					/*case Right((mod, toaxi4_reg_0)) =>{
						/*val modNode = mod.ioNode.makeSink()
						val hscontrol_mIO = modNode.makeIO()
						val c = hscontrol_mIO.params
						mod.io.clock := clock // Change this to Module Clock
						toaxi4_reg_0.module.clock := clock
						reg_crossing_0.module.clock := hscontrol_mIO.gtp_clock
						hscontrol_mIO*/
					}*/
					case Left(mod) =>{ // TileLink Case
						val hscontrol_mIO = mod.makeIO()
						val c = hscontrol_mIO.params
						reg_crossing_0.module.clock := hscontrol_mIO.gtp_clock
						
						hscontrol_mIO
					}
				}
			})
			}
			case None => None
		}
	//val p: Nothing = HSControl_io
}

object TL_HSControl {
	val nextId = { var i = -1; () => { i += 1; i} }

def attach(params: HSControlParams, parameter: Parameters, pbus: PeripheryBus,sbus: SystemBus,  fbus: FrontBus , ibus: InterruptBusWrapper) :(Either[BundleBridgeSink[HSControlBundle], (AXI4_HSControl, TLToAXI4)], TLAsyncCrossingSink) = {
	implicit val p = parameter
		//if (params.useAXI4){
			/*val name = s"axi4HSControl_${nextId()}"
			val hscontrol = LazyModule(new AXI4_HSControl(params)(p))
			hscontrol.suggestName(name)
			val toaxi4_reg_0 = LazyModule(new TLToAXI4())
			val reg_crossing_0 = LazyModule(new TLAsyncCrossingSink(AsyncQueueParams.singleton()))
			(Right(HSControl, toaxi4_reg_0), reg_crossing_0)*/
		//}else{
			val name = s"tlHSControl_${nextId()}"
			val hscontrol = LazyModule(new TL_HSControl(params)(p))
			hscontrol.suggestName(name)
			val reg_crossing_0 = LazyModule(new TLAsyncCrossingSink(AsyncQueueParams.singleton()))
			val reg_node_0 = hscontrol.controlNode:= reg_crossing_0.node
			pbus.toVariableWidthSlave(Some(name)) { reg_node_0 := TLAsyncCrossingSource() } 
			val HSControl_Node = hscontrol.ioNode.makeSink()
			(Left(HSControl_Node), reg_crossing_0)
		//}
	}
}