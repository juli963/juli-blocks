package juli.blocks.devices.ESP_FIFO
import chisel3._
import chisel3.util._
import chisel3.experimental._
import FIFO.{Async_FIFO_new}
import Ethernet.Protocol.OSI4._
import Ethernet.Protocol._
import Checksum.{IP_Checksum_8}

class ESP_Prot_Bundle extends Bundle{
    val Data = UInt(32.W)
    val address = UInt(8.W)
    val cmd = UInt(8.W)
    val Ident = UInt(8.W)
    val ttype = UInt(8.W)
}

class ESP_Bundle extends Bundle{
    val deq_Data = Input(Bool())
    val deq_Data_new = Output(Bool())
    val deq_empty = Output(Bool())
	val recv_type = Output(UInt(8.W))
	val recv_cmd = Output(UInt(8.W))
	val recv_address = Output(UInt(8.W))
    val recv_Data = Output(UInt(32.W))
    val recv_Ident = Output(UInt(8.W))

    val enq_Data = Input(Bool())
    val enq_full = Output(Bool())
	val trans_type = Input(UInt(8.W))
	val trans_cmd = Input(UInt(8.W))
	val trans_address = Input(UInt(8.W))
    val trans_Data = Input(UInt(32.W))
    val trans_Ident = Input(UInt(8.W))

    val UDPBus = new Ethernet.Protocol.OSI4.UDPBus(8)   // Fix to 8

    val clkUDP = Input(Clock())
}

class ESP_FIFO() extends Module{
    val io = IO(new ESP_Bundle)

    val rx_fifo = Module(new Async_FIFO_new(new ESP_Prot_Bundle, 4)) // 16 Entries
    val tx_fifo = Module(new Async_FIFO_new(new ESP_Prot_Bundle, 4)) // 16 Entries
    rx_fifo.io.clkA := clock
    rx_fifo.io.clkB := io.clkUDP
    tx_fifo.io.clkA := clock
    tx_fifo.io.clkB := io.clkUDP


    tx_fifo.io.queue.enq.valid := io.enq_Data
    io.enq_full := tx_fifo.io.full
    tx_fifo.io.queue.enq.bits.ttype := io.trans_type
    tx_fifo.io.queue.enq.bits.cmd := io.trans_cmd
    tx_fifo.io.queue.enq.bits.address := io.trans_address
    tx_fifo.io.queue.enq.bits.Data := io.trans_Data
    tx_fifo.io.queue.enq.bits.Ident := io.trans_Ident


    rx_fifo.io.queue.deq.ready := io.deq_Data
    io.deq_Data_new := ~rx_fifo.io.queue.deq.valid && RegNext(rx_fifo.io.queue.deq.valid)
    io.deq_empty := rx_fifo.io.empty
    io.recv_type := rx_fifo.io.queue.deq.bits.ttype
    io.recv_cmd := rx_fifo.io.queue.deq.bits.cmd
    io.recv_address := rx_fifo.io.queue.deq.bits.address
    io.recv_Data := rx_fifo.io.queue.deq.bits.Data
    io.recv_Ident := rx_fifo.io.queue.deq.bits.Ident


    withClock(io.clkUDP){
        // Transmitter 
        val s_tx_idle :: s_tx_check :: s_tx_checksum :: s_tx_send :: s_wait :: Nil = Enum(5)

        val pMAC = "h44_48_49_50_50_51".U(48.W)
        val pIP = "hC0_A8_2_F0".U(32.W)
        val MAC = RegInit(pMAC)   // Start MAC and IP
        val IP = RegInit(pIP)
        val Port = RegInit(15000.U)

        val tx_deq_en = RegInit(false.B)
        val tx_counter = RegInit(0.U(4.W))
        tx_fifo.io.queue.deq.ready := tx_deq_en

        io.UDPBus.tx.bits.info.IP := IP
        io.UDPBus.tx.bits.info.MAC := MAC
        io.UDPBus.tx.bits.info.Port := Port
        io.UDPBus.tx.bits.info.Length := 8.U
        
        io.UDPBus.tx.bits.error := false.B
        
        val tx_strb = RegInit(0.U(1.W))
        val tx_data = RegInit(0.U(8.W))
        val tx_valid = RegInit(false.B)
        io.UDPBus.tx.bits.strb := tx_strb
        io.UDPBus.tx.bits.data := tx_data
        io.UDPBus.tx.valid := tx_valid

        val mChecksum = Module(new IP_Checksum_8(18))
        val rst_checksum = RegInit(false.B)
        val update_checksum = RegInit(false.B)
        val data_checksum = RegInit(0.U(8.W))
        mChecksum.io.data_in := data_checksum
        mChecksum.io.preload := 0.U
        mChecksum.io.update := update_checksum
        mChecksum.io.rst := rst_checksum
        io.UDPBus.tx.bits.info.Checksum := mChecksum.io.data_out


        val state_tx = RegInit(s_tx_idle)
        switch(state_tx){
            is(s_tx_idle){
                when(~tx_fifo.io.empty){
                    mChecksum.io.rst := false.B
                    tx_deq_en := true.B
                    tx_counter := 0.U
                    state_tx := s_tx_check
                }
            }
            is(s_tx_check){
                tx_deq_en := false.B  
                when(tx_fifo.io.queue.deq.valid || tx_counter > 0.U){
                    tx_counter := tx_counter + 1.U
                    update_checksum := true.B
                    data_checksum := tx_fifo.io.queue.deq.bits.asTypeOf(Vec(8,UInt(8.W)))(tx_counter)
                    when(tx_counter >= 7.U){
                        state_tx := s_tx_checksum
                    }
                }.otherwise{
                    update_checksum := false.B
                }
            }
            is(s_tx_checksum){
                when(~mChecksum.io.busy){
                    tx_counter := 0.U
                    state_tx := s_tx_send
                }
            }
            is(s_tx_send){
                tx_strb := 1.U
                tx_data := Mux(~io.UDPBus.tx.ready, tx_fifo.io.queue.deq.bits.asTypeOf(Vec(8,UInt(8.W)))(tx_counter), tx_fifo.io.queue.deq.bits.asTypeOf(Vec(8,UInt(8.W)))(tx_counter+1.U))
                
                when(io.UDPBus.tx.ready){
                    tx_counter := tx_counter + 1.U
                    when(tx_counter >= 6.U){
                        state_tx := s_wait
                    }
                }.otherwise{
                    tx_valid := true.B
                }
            }
            is(s_wait){
                tx_strb := 0.U
                when(~tx_strb.orR){
                    tx_valid := false.B
                    mChecksum.io.rst := true.B
                    state_tx := s_tx_idle
                }

            }
        }
    


        // Receiver
        io.UDPBus.rx.ready := true.B
        val rx_counter = RegInit(0.U(4.W))
        val rx_data = RegInit(0.U.asTypeOf(Vec(8,UInt(8.W))))
        when(rx_counter <= 8.U && io.UDPBus.rx.bits.strb.orR && io.UDPBus.rx.valid){
            rx_counter := rx_counter + 1.U
        }
        when(~io.UDPBus.rx.bits.strb.orR && RegNext(io.UDPBus.rx.bits.strb.orR)){
            rx_counter := 0.U
        }

        when(rx_counter < 8.U && io.UDPBus.rx.bits.strb.orR){
            rx_data(rx_counter) := io.UDPBus.rx.bits.data
        }
        when(io.UDPBus.rx.valid && (io.UDPBus.rx.bits.info.Port === io.UDPBus.tx.bits.info.Port)){
            MAC := io.UDPBus.rx.bits.info.MAC
            IP := io.UDPBus.rx.bits.info.IP
            Port := io.UDPBus.rx.bits.info.Port
        }
        

        rx_fifo.io.queue.enq.bits := rx_data.asTypeOf(new ESP_Prot_Bundle())
        rx_fifo.io.queue.enq.valid := rx_counter === 8.U || (~io.UDPBus.rx.bits.strb.orR && RegNext(io.UDPBus.rx.bits.strb.orR))
    }
}

class ESP_FIFO_Test extends Module{
    val io = IO(new Bundle{
        val deq_Data = Input(Bool())
        val deq_Data_new = Output(Bool())
        val deq_empty = Output(Bool())
        val recv_type = Output(UInt(8.W))
        val recv_cmd = Output(UInt(8.W))
        val recv_address = Output(UInt(8.W))
        val recv_Data = Output(UInt(32.W))
        val recv_Ident = Output(UInt(8.W))

        val enq_Data = Input(Bool())
        val enq_full = Output(Bool())
        val trans_type = Input(UInt(8.W))
        val trans_cmd = Input(UInt(8.W))
        val trans_address = Input(UInt(8.W))
        val trans_Data = Input(UInt(32.W))
        val trans_Ident = Input(UInt(8.W))

        val Stats = Flipped(new Ethernet.Interface.Types.PHYStat())
        val EthernetBus = Flipped(new Ethernet.Protocol.Types.DataBus(8))
    })

    val mod = Module(new ESP_FIFO())
    mod.io.deq_Data := io.deq_Data
    io.deq_Data_new := mod.io.deq_Data_new
    io.deq_empty := mod.io.deq_empty
    io.recv_type := mod.io.recv_type
    io.recv_cmd := mod.io.recv_cmd
    io.recv_address := mod.io.recv_address
    io.recv_Data := mod.io.recv_Data
    io.recv_Ident := mod.io.recv_Ident


    mod.io.enq_Data := io.enq_Data
    io.enq_full := mod.io.enq_full
    mod.io.trans_type := io.trans_type
    mod.io.trans_cmd := io.trans_cmd
    mod.io.trans_address := io.trans_address
    mod.io.trans_Data := io.trans_Data
    mod.io.trans_Ident := io.trans_Ident

    mod.io.clkUDP := clock

    val m_arp = Module(new Ethernet.Protocol.ARP(8))
    val m_arb = Module(new Ethernet.Protocol.ETH_Arbiter_FirstPriority(2,8))

    val m_IP4 = Module(new IP(2,8))
    val m_ICMP4 = Module(new ICMP4(8))
    val m_UDP = Module(new UDP(8, 15000))
    
    val Params = Wire(new Ethernet.Protocol.Types.EthernetParams()) 
    //Params := io.Params
    Params.MAC := "h111213141516".U
    Params.IP := "hC0A802D4".U

    io.EthernetBus <> m_arb.io.FIFOBus
    m_arb.io.FIFOStats := io.Stats

    m_arp.io.EthernetParameters := Params
    m_arb.io.EthernetBus(0) <>  m_arp.io.EthernetBus

    m_arb.io.EthernetBus(1) <> m_IP4.io.EthernetBus
    m_IP4.io.EthernetParameters := Params
    m_IP4.io.IPBus(0) <> m_ICMP4.io.IPBus
    

    m_ICMP4.io.EthernetParameters := Params

    m_IP4.io.IPBus(1) <> m_UDP.io.IPBus
    m_UDP.io.EthernetParameters := Params
    m_UDP.io.UDPBus <> mod.io.UDPBus

}

//java -jar rocket-chip/sbt-launch.jar ++2.12.4 "runMain juli.blocks.devices.ESP_FIFO.ESP_FIFO_Test"
object ESP_FIFO_Test extends App {
  chisel3.Driver.execute(Array("--target-dir", "generated/ESP_FIFO_Test"), () => new ESP_FIFO_Test())
}