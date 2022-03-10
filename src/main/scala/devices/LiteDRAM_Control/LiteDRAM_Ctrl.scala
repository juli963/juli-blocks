package juli.blocks.devices.LiteDRAM_Ctrl
import chisel3._
import chisel3.util._
import chisel3.experimental._
import Interface.Bus.{Wishbone_Master_Bundle}
import freechips.rocketchip.tilelink.{TLBundleParameters, TLBundle, TLMessages}

class LiteDRAM_Ctrl(c:LiteDRAM_CtrlParams, tlparam : TLBundleParameters) extends Module{
    val params = c

    val io = IO(  new Bundle{
        //val dram_clock = Input(Clock())
        val node = Flipped(TLBundle(tlparam))
        val wbus = new Wishbone_Master_Bundle(32,30) 
    })
        val mems_f_0 = io.node

        io.node.b.bits := 0.U.asTypeOf(io.node.b.bits)

    	val mems_0_d_valid = RegInit(false.B)
		mems_0_d_valid.suggestName("reg_mems_0_d_valid")
		val mems_0_a_ready = RegInit(true.B)  
		mems_0_a_ready.suggestName("reg_mems_0_a_ready")
		val mems_0_a_channel = RegEnable(mems_f_0.a.bits, mems_f_0.a.fire())  
		mems_0_a_channel.suggestName("reg_mems_0_a_channel")
		val mems_0_d_channel = RegInit(mems_f_0.d.bits)
		mems_0_d_channel.suggestName("reg_mems_0_d_channel")
		mems_f_0.b.valid := false.B
		mems_f_0.c.ready := true.B
		mems_f_0.e.ready := true.B
		mems_f_0.a.ready := mems_0_a_ready
		mems_f_0.d.valid := mems_0_d_valid
		mems_f_0.d.bits := mems_0_d_channel

		val reg_wbus_cyc = RegInit(false.B)
		reg_wbus_cyc.suggestName("reg_wbus_cyc")
		io.wbus.cyc := reg_wbus_cyc
		val reg_wbus_sel = RegInit(false.B)
		reg_wbus_sel.suggestName("reg_wbus_sel")
		io.wbus.sel := reg_wbus_sel
		val reg_wbus_strobe = RegInit(false.B)
		reg_wbus_strobe.suggestName("reg_wbus_strobe")
		io.wbus.strobe := reg_wbus_strobe
		val reg_wbus_we = RegInit(false.B)
		reg_wbus_we.suggestName("reg_wbus_we")
		io.wbus.we := reg_wbus_we
		val reg_wbus_address = RegInit(0.U(30.W))
		reg_wbus_address.suggestName("reg_wbus_address")
		io.wbus.address := reg_wbus_address
		val reg_wbus_wr_data = RegInit(0.U(32.W))
		reg_wbus_wr_data.suggestName("reg_wbus_wr_data")
		io.wbus.wr_data := reg_wbus_wr_data

		io.wbus.cti := 0.U
		io.wbus.bte := 0.U


		val transfer_size = RegInit(0.U(  (log2Ceil( 1<<(1<<tlparam.sizeBits))).W))

		val ( s_idle :: s_read0 :: s_read1 :: s_read2 :: s_write :: s_finish  :: Nil) = Enum(6)
		val state = RegInit(s_idle)

		switch(state){
			is(s_idle){
				mems_0_d_valid := false.B
				
				when(mems_f_0.a.fire()){
					when(mems_f_0.a.bits.opcode === 0.U){
						mems_0_a_ready := false.B
						reg_wbus_wr_data := mems_f_0.a.bits.data

						reg_wbus_cyc := true.B
						reg_wbus_strobe := true.B
						reg_wbus_we := true.B

						state := s_write
					}.elsewhen(mems_f_0.a.bits.opcode === 1.U){
						mems_0_a_ready := false.B
						reg_wbus_wr_data := mems_f_0.a.bits.data

						reg_wbus_cyc := true.B
						reg_wbus_strobe := true.B
						reg_wbus_we := true.B

						state := s_write
					}.elsewhen(mems_f_0.a.bits.opcode === 4.U){
						mems_0_a_ready := false.B

						reg_wbus_cyc := true.B
						reg_wbus_strobe := true.B
						reg_wbus_we := false.B
						transfer_size := (1.U<<mems_f_0.a.bits.size)

						state := s_read0
					}

					reg_wbus_sel := mems_f_0.a.bits.mask
					reg_wbus_address := mems_f_0.a.bits.address(tlparam.addressBits-1,2) // - c.mem_slave_0_address.U
				}.otherwise{
					mems_0_a_ready := true.B
				}
			}
			is(s_read0){    // Wait States for Data to get Ready
				reg_wbus_strobe := false.B
				/*when(transfer_size < 4.U ){	// Transfer Size = 0
					state := s_idle
					reg_wbus_cyc := false.B
					//reg_wbus_strobe := false.B
					mems_0_a_ready := true.B
				}*/
				when(io.wbus.ack && transfer_size.orR()){
					//reg_wbus_strobe := false.B
					mems_0_d_valid := true.B
				    mems_0_d_channel.opcode := TLMessages.AccessAckData
				    mems_0_d_channel.param := 0.U
				    mems_0_d_channel.size := mems_0_a_channel.size
				    mems_0_d_channel.source := mems_0_a_channel.source
				    mems_0_d_channel.sink := 0.U
				    mems_0_d_channel.denied := false.B
				    mems_0_d_channel.data := io.wbus.rd_data
				    mems_0_d_channel.corrupt := false.B

                }
				when(mems_f_0.d.fire()){
					mems_0_d_valid := false.B
					transfer_size := transfer_size - 4.U
					when(transfer_size > 4.U){
						reg_wbus_strobe := true.B
						reg_wbus_address := reg_wbus_address + 1.U
					}.elsewhen(transfer_size <= 4.U){
						state := s_idle
						reg_wbus_cyc := false.B
						mems_0_a_ready := true.B
					}
				}
			}
			
			is(s_write){
				reg_wbus_strobe := false.B
				when(io.wbus.ack){
					reg_wbus_cyc := false.B
					reg_wbus_we := false.B

					mems_0_d_valid := true.B
				    mems_0_d_channel.opcode := TLMessages.AccessAck
				    mems_0_d_channel.param := 0.U
				    mems_0_d_channel.size := mems_0_a_channel.size
				    mems_0_d_channel.source := mems_0_a_channel.source
				    mems_0_d_channel.sink := 0.U
				    mems_0_d_channel.denied := false.B
				    mems_0_d_channel.data := 0.U
				    mems_0_d_channel.corrupt := false.B

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
}