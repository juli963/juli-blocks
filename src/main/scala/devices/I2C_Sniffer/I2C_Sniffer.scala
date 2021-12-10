package juli.blocks.devices.I2C_Sniffer
import chisel3._
import chisel3.util._
import chisel3.experimental._
import FIFO.{Async_FIFO_new}
import Ethernet.Protocol.OSI4._
import Ethernet.Protocol._
import Checksum.{IP_Checksum_8}

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
    }.elsewhen(~io.master_scl.in){      // Master gives Slave Clock
        slave_scl_en := ~io.master_scl.in
    }.elsewhen(io.master_scl.in){      // Master releases Clock
        slave_scl_en := false.B
    }.elsewhen(io.slave_scl.in){        // Release Master
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
                slave_sda_en := false.B
                master_sda_en := ~io.slave_sda.in
            }.otherwise{
                master_sda_en := false.B
                slave_sda_en := ~io.master_sda.in
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
        }
    }

    when(start_bit && ~(io.master_scl.in ^ master_scl_ff)){
        state := s_addr
    }
    when(stop_bit && ~(io.master_scl.in ^ master_scl_ff)){
        state := s_i2c_idle
    }

}

//java -jar rocket-chip/sbt-launch.jar ++2.12.4 "runMain juli.blocks.devices.I2C_Sniffer.I2C_Sniffer"
object I2C_Sniffer extends App {
  chisel3.Driver.execute(Array("--target-dir", "generated/I2C_Sniffer"), () => new I2C_Sniffer(true))
}