package juli.blocks.devices.I2C_Sniffer
import chisel3._
import chisel3.util._
import chisel3.experimental._
import FIFO.{Async_FIFO_new}
import Checksum.{IP_Checksum_8}
import device.hssniffer.{Slave_bundle_dram_fifo}

class I2C_Pin extends Bundle{
    val in = Input(Bool())
    val out = Output(Bool())
    val tri = Output(Bool())
}

class I2C_Sniffer_Bundle(Sim : Boolean = false) extends Bundle{
  //  val deq_Data = Input(Bool())
  //  val deq_valid = Output(Bool())

  //  val deq_empty = Output(Bool())

    val master_scl = new I2C_Pin()
    val master_sda = new I2C_Pin()

    val slave_scl = new I2C_Pin()
    val slave_sda = new I2C_Pin()

    val main_sda = if(Sim) {Some(Input(Bool()))}else{None}
  //  val clkDeq = Input(Clock())
}

class I2C_Sniffer(Sim : Boolean = false) extends Module{
    val io = IO(new I2C_Sniffer_Bundle(Sim))

    io.master_scl.out := false.B
    io.master_sda.out := false.B

    io.slave_scl.out := false.B
    io.slave_sda.out := false.B

    val master_scl_ff = RegNext(io.master_scl.in)
    val master_scl_en = RegInit(false.B)
    val master_sda_ff = RegNext(io.master_sda.in)
    val master_sda_en = RegInit(false.B)
    io.master_scl.tri := ~master_scl_en
    io.master_sda.tri := ~master_sda_en

    val slave_scl_en = RegInit(false.B)
    val slave_sda_en = RegInit(false.B)
    io.slave_scl.tri := ~slave_scl_en
    io.slave_sda.tri := ~slave_sda_en

    val s_i2c_idle :: s_addr :: s_rw :: s_ack_address :: s_ack :: s_data :: s_stop :: Nil = Enum(7)
    val state = RegInit(s_i2c_idle)

    val master_scl_rise = io.master_scl.in && ~master_scl_ff
    val master_scl_fall = ~io.master_scl.in && master_scl_ff
    val master_sda_rise = io.master_sda.in && ~master_sda_ff
    val master_sda_fall = ~io.master_sda.in && master_sda_ff
    val start_bit = io.master_scl.in && master_sda_fall
    val stop_bit = io.master_scl.in && master_sda_rise
    val counter = RegInit(0.U(4.W))
    val rd = RegInit(false.B)

    // http://www.amateurfunkbasteln.de/i2c/index.html

    when(io.master_scl.in && ~io.slave_scl.in){ // Slave holds clock low    (Clock Stretching)
        master_scl_en := ~io.slave_scl.in
    }.elsewhen(~io.master_scl.in){     // Master gives Slave Clock
        slave_scl_en := ~io.master_scl.in
    }.elsewhen(io.master_scl.in){      // Master releases Clock
        slave_scl_en := false.B
    }.elsewhen(io.slave_scl.in){       // Release Master
        master_scl_en := false.B
    }

    switch(state){
        is(s_i2c_idle){
            slave_sda_en := ~io.master_sda.in
        }
        is(s_addr){
            slave_sda_en := ~io.master_sda.in
            when(master_scl_rise){
                counter := counter + 1.U
            }
            when(counter >= 7.U && master_scl_fall){
                state := s_rw
                counter := 0.U
            }
        }
        is(s_data){
            when(rd){
                when(~io.master_sda.in && io.slave_sda.in && io.master_scl.in){
                    slave_sda_en := true.B
                    master_sda_en := false.B
                    state := s_stop
                }.otherwise{
                    slave_sda_en := false.B
                    master_sda_en := ~io.slave_sda.in
                }
            }.otherwise{
                when(~io.master_sda.in && io.slave_sda.in && io.master_scl.in){
                    slave_sda_en := true.B
                    master_sda_en := false.B
                    state := s_stop
                }.otherwise{
                    master_sda_en := false.B
                    slave_sda_en := ~io.master_sda.in
                }
            }
            when(master_scl_fall){
                counter := counter + 1.U
            }
            when(counter >= 8.U){
                state := s_ack
                counter := 0.U
            }
        }
        is(s_rw){
            when(master_scl_fall){
                state := s_ack_address
            }
            when(master_scl_rise){
                rd := io.master_sda.in
            }
            slave_sda_en := ~io.master_sda.in
        }
        is(s_ack_address){
            when(master_scl_fall){
                state := s_data
            }
            slave_sda_en := false.B
            master_sda_en := ~io.slave_sda.in
        }
        is(s_ack){
            when(master_scl_fall){
                state := s_data
            }
            when(rd){
                master_sda_en := false.B
                slave_sda_en := ~io.master_sda.in
            }.otherwise{
                slave_sda_en := false.B
                master_sda_en := ~io.slave_sda.in
            }
        }
        is(s_stop){
            master_sda_en := false.B
            slave_sda_en := ~io.master_sda.in
            when(io.master_sda.in){
                state := s_i2c_idle
            }
        }
    }

    when(start_bit && ~(io.master_scl.in ^ master_scl_ff)){
        state := s_addr
    }
    when(stop_bit && ~(io.master_scl.in ^ master_scl_ff)){
        state := s_stop
    }

}

class I2C_MGMT extends Bundle{
    val length = UInt(8.W)
    val checksum = UInt(16.W)
    val flags = new Bundle{
        val w_stop = Bool()
        val timeout = Bool()
        val stretching = Bool()
        val read = Bool()
        val abort = Bool()
        val data_full = Bool()
    }
}

class I2C_Data extends Bundle{
    val data = UInt(8.W)
}

class I2C_Decoder extends Module{
    val io = IO(new Bundle{
        val i2c_scl = Input(Bool())
        val i2c_sda = Input(Bool())

        val data_enq = DecoupledIO(new I2C_Data)
        val mgmt_enq = DecoupledIO(new I2C_MGMT)
    })

    val enq_data = RegInit(false.B)
    io.data_enq.valid := enq_data
    val data_data = RegInit(0.U.asTypeOf(new I2C_Data))
    io.data_enq.bits := data_data

    
    val enq_mgmt = RegInit(false.B)
    io.mgmt_enq.valid := enq_mgmt
    val data_mgmt = RegInit(0.U.asTypeOf(new I2C_MGMT))
    io.mgmt_enq.bits := data_mgmt

    val s_i2c_idle :: s_wait :: s_addr :: s_rw :: s_ack_address :: s_ack :: s_data :: s_stop :: Nil = Enum(8)
    val state = RegInit(s_i2c_idle)

    val scl_rise = io.i2c_scl && ~RegNext(io.i2c_scl)
    val scl_fall = ~io.i2c_scl && RegNext(io.i2c_scl)
    val sda_rise = io.i2c_sda && ~RegNext(io.i2c_sda)
    val sda_fall = ~io.i2c_sda && RegNext(io.i2c_sda)
    val start_bit = io.i2c_scl && sda_fall && ~scl_rise && ~scl_fall
    val stop_bit = io.i2c_scl && sda_rise && ~scl_rise && ~scl_fall

    val mChecksum = Module(new IP_Checksum_8(18))
    val rst_checksum = RegInit(false.B)
    val update_checksum = RegInit(false.B)
    val data_checksum = RegInit(0.U(8.W))
    mChecksum.io.data_in := data_data.data
    mChecksum.io.preload := 0.U
    mChecksum.io.update := update_checksum
    mChecksum.io.rst := rst_checksum
    

    val length = RegInit(0.U(10.W))
    val counter = RegInit(0.U(4.W))
    val busy = RegInit(false.B)

    switch(state){
        is(s_i2c_idle){
            data_data := 0.U.asTypeOf(new I2C_Data)
            data_mgmt := 0.U.asTypeOf(new I2C_MGMT)
            
            enq_mgmt := false.B
            enq_data := false.B

            rst_checksum := true.B
            busy := false.B
        }
        is(s_wait){
            when(busy){
                when(~mChecksum.io.busy){
                    data_mgmt.checksum := mChecksum.io.data_out
                    when(io.mgmt_enq.ready){
                        enq_mgmt := true.B
                        busy := false.B
                    }
                }
            }.otherwise{
                enq_mgmt := false.B
                data_mgmt := 0.U.asTypeOf(new I2C_MGMT)
            }
            when(scl_fall){
                busy := true.B
                state := s_addr
                data_data := 0.U.asTypeOf(data_data)
                rst_checksum := false.B
            }
        }
        is(s_addr){
            
            when(io.i2c_scl){
                data_data.data := data_data.data | (io.i2c_sda<<(6.U-counter))
            }
            when(scl_fall){
                counter := counter + 1.U
                
                when(counter >= 6.U){
                    update_checksum := true.B
                    data_mgmt.length := data_mgmt.length + 1.U
                    counter := 0.U
                    state := s_rw
                }
            }
        }
        is(s_data){
            
            when(~io.i2c_scl && counter === 0.U){  // Data is in FIFO
                data_data := 0.U.asTypeOf(data_data)
                enq_data := false.B
            }
            when(io.i2c_scl){
                data_data.data := data_data.data | (io.i2c_sda<<(7.U-counter))
            }
            when(scl_fall){
                counter := counter + 1.U
                when(counter >= 7.U){
                    update_checksum := true.B
                    data_mgmt.length := data_mgmt.length + 1.U
                    counter := 0.U
                    state := s_ack
                }
            }
        }
        is(s_rw){
            update_checksum := false.B
            when(io.i2c_scl){
                data_mgmt.flags.read := io.i2c_sda
            }
            when(scl_fall){
                state := s_ack
            }
        }
        is(s_ack){
            update_checksum := false.B
            when(io.i2c_scl){
                when(~data_mgmt.flags.abort){
                    data_mgmt.flags.abort := io.i2c_sda
                }
            }
            when(scl_fall){
                when(~io.data_enq.ready){
                    data_mgmt.flags.data_full := true.B
                }.otherwise { 
                    enq_data := true.B
                }
                state := s_data
            }
        }
        is(s_stop){
            when(~mChecksum.io.busy){
                data_mgmt.checksum := mChecksum.io.data_out
                when(io.mgmt_enq.ready){
                    enq_mgmt := true.B
                    state := s_i2c_idle
                }
            }
            enq_data := false.B
        }
    }

    when(start_bit){
        state := s_wait
    }
    when(stop_bit){
        data_mgmt.flags.w_stop := true.B
        state := s_stop
    }

}

class I2C_Fifo(DRAM_Width : Int) extends Module{
    val io = IO(new Bundle{
        val mem_clock = Input(Clock())
        
        val mgmt_enq = Flipped(DecoupledIO(new I2C_MGMT))
        val data_enq = Flipped(DecoupledIO(new I2C_Data))

        val dram_fifo = new Slave_bundle_dram_fifo(DRAM_Width)
       // val arbit_fifo = Output(Bool())
       // val dram_fifo = DecoupledIO(UInt(DRAM_Width.W))
    })

    val data_fifo = Module(new Async_FIFO_new(new I2C_Data, 10)) // 256 Entries
    data_fifo.io.clkA := clock
    data_fifo.io.clkB := io.mem_clock
    data_fifo.io.queue.enq <> io.data_enq
    val mgmt_fifo = Module(new Async_FIFO_new(new I2C_MGMT, 6)) // 64 Entries
    mgmt_fifo.io.clkA := clock
    mgmt_fifo.io.clkB := io.mem_clock
    mgmt_fifo.io.queue.enq <> io.mgmt_enq


    withClock(io.mem_clock){
        val arbit_fifo = RegInit(false.B)
        io.dram_fifo.arbit_fifo := arbit_fifo
        val enq_dram = RegInit(false.B)
        io.dram_fifo.slave_fifo.valid := enq_dram
        val dram_data = RegInit(0.U(128.W))
        io.dram_fifo.slave_fifo.bits := dram_data

        val deq_data = RegInit(false.B)
        data_fifo.io.queue.deq.ready := deq_data
        val deq_mgmt = RegInit(false.B)
        mgmt_fifo.io.queue.deq.ready := deq_mgmt

        val temp = RegInit(0.U(8.W))
        temp.suggestName("temp")

        val cDataType = "h14".U

        if(DRAM_Width == 128){    
            val s_idle :: s_header :: s_timestamp :: s_wait :: s_data :: s_stop :: Nil = Enum(6)
            val state = RegInit(s_idle)
            state.suggestName("State_DRAM_FIFO")

            switch(state){
                is(s_idle){
                    arbit_fifo := false.B
                    when(~mgmt_fifo.io.empty){
                        arbit_fifo := true.B
                        when(io.dram_fifo.slave_fifo.ready){
                            state := s_header
                            deq_mgmt := true.B
                        }
                    }
                }
                is(s_header){
                    deq_mgmt := false.B
                    when(mgmt_fifo.io.queue.deq.valid){
                        state := s_timestamp
                        dram_data := cDataType | (mgmt_fifo.io.queue.deq.bits.length << 8) | (mgmt_fifo.io.queue.deq.bits.flags.asUInt() << 32) | (mgmt_fifo.io.queue.deq.bits.checksum << 64)
                        enq_dram := true.B
                    }
                }
                is(s_timestamp){
                    when(io.dram_fifo.slave_fifo.ready){
                        state := s_wait
                        dram_data := 0.U    // 2x 64bit
                    }
                }
                is(s_wait){ 
                    when(io.dram_fifo.slave_fifo.ready){
                        enq_dram := false.B
                        state := s_data
                        temp := 0.U
                    }
                }
                is(s_data){
                    when(io.dram_fifo.slave_fifo.ready){
                        when(temp === mgmt_fifo.io.queue.deq.bits.length ){
                            state := s_stop
                            enq_dram := true.B
                            deq_data := false.B
                        }.otherwise{
                            when( ((temp % ((DRAM_Width/8).U)) === 0.U) && temp > 0.U){
                                enq_dram := true.B
                            }.otherwise { 
                                enq_dram := false.B
                            }
                            deq_data := true.B
                        }
                        when((temp+2.U) >= mgmt_fifo.io.queue.deq.bits.length){
                            deq_data := false.B
                        }
                    }.otherwise { 
                        deq_data := false.B
                    }
                    when(data_fifo.io.queue.deq.valid){
                        when((temp % ((DRAM_Width/8).U)) === 0.U){
                            dram_data := data_fifo.io.queue.deq.bits.data
                        }.otherwise { 
                            dram_data := dram_data.data | (data_fifo.io.queue.deq.bits.asUInt << ((temp%((DRAM_Width/8).U))*8.U))
                        }
                        temp := temp + 1.U
                    }
                }
                is(s_stop){
                    when(io.dram_fifo.slave_fifo.ready){
                        enq_dram := false.B
                        state := s_idle
                    }
                }
            }
        }
    }
}

//java -jar rocket-chip/sbt-launch.jar ++2.12.4 "runMain juli.blocks.devices.I2C_Sniffer.I2C_Sniffer"
object I2C_Sniffer extends App {
  chisel3.Driver.execute(Array("--target-dir", "generated/I2C_Sniffer"), () => new I2C_Sniffer(true))
}