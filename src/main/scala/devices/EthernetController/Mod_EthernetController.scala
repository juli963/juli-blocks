package juli.blocks.devices.ethctrl

import Memory.onChip.TrueDualPortBRAM

import chisel3._
import chisel3.util._
import chisel3.experimental._

import freechips.rocketchip.regmapper._
import sifive.blocks.util.{SlaveRegIF}

// java -jar rocket-chip/sbt-launch.jar ++2.12.4 "runMain juli.blocks.devices.ethctrl.mETHCtrl"
object mETHCtrl extends App {
  chisel3.Driver.execute(Array("--target-dir", "generated/ETHCtrl"), () => new ETHCtrl(4, 2048, 5))
}

class ETHCtrl(regBytes: Int = 4, Indizes: Int = 5, NumBuffers: Int = 3) extends Module {
  protected val prefix = "ethctrl"
  val io = IO(new Bundle {
    val RGMII = new Ethernet.Interface.Types.RGMII_Interface()
    val PHY_nrst = Output(Bool())
    val EthernetClock125 = Input(Clock())
    val EthernetClock250 = Input(Clock())
    val interrupt = Output(Bool())

    val TXwrena   = Input(Bool())
    val TXaddr  = Input(UInt(log2Ceil(Indizes).W))
    val TXwrdata  = Input(UInt(8.W))
    val TXrddata  = Output(UInt(8.W))

    val RXaddr  = Input(UInt(log2Ceil(Indizes).W))
    val RXrddata  = Output(UInt(8.W))
    val RXwrdata  = Input(UInt(8.W))
    val RXwrena = Input(Bool())

    val regs = new Bundle{
      val PHYEst = new SlaveRegIF(1)
      val PHYDuplex = new SlaveRegIF(1)
      val PHYForce = new SlaveRegIF(2)
      val PHYLink = new SlaveRegIF(4)

      val start_TX = new SlaveRegIF(1)
      val busy_TX = new SlaveRegIF(1)
      val buffer_TX = new SlaveRegIF(8)

      val busy_RX = new SlaveRegIF(1)
      val ipRX = new SlaveRegIF(1)
      val bufferLockRX = new SlaveRegIF(1)
      val bufferStatRX = new SlaveRegIF(8)
      val bufferSelectRX = new SlaveRegIF(8)
      
      
    }
  })
  
  val mem_RX = Module(new TrueDualPortBRAM(Indizes*NumBuffers, UInt(8.W)))
  val mem_TX = Module(new TrueDualPortBRAM(Indizes*NumBuffers, UInt(8.W)))
  val m_fifo = Module(new Ethernet.Protocol.EthernetFIFO8())
  val m_rgmii = Module(new Ethernet.Interface.RGMII.RGMII(canForceSpeed = true))
  m_rgmii.clock := io.EthernetClock125

  // Register
  val PHYForce = RegEnable(io.regs.PHYForce.write.bits, io.regs.PHYForce.write.valid)
  io.regs.PHYForce.read := PHYForce
  m_rgmii.io.forceSpeed.get := PHYForce



  val start_TX = RegEnable(io.regs.start_TX.write.bits.toBool, io.regs.start_TX.write.valid)
    val start_TX_ff = RegNext(start_TX)
    io.regs.start_TX.read := start_TX
    val wsync_busy_TX = Wire(Bool())
    val wsync_busy_TX_ff = RegNext(wsync_busy_TX)
  val busy_TX = RegEnable( (start_TX && ~start_TX_ff) || wsync_busy_TX, (~wsync_busy_TX && wsync_busy_TX_ff) ||  (start_TX && ~start_TX_ff) )
    io.regs.busy_TX.read := busy_TX
  val buffer_TX = RegEnable(io.regs.buffer_TX.write.bits, io.regs.buffer_TX.write.valid)
    io.regs.buffer_TX.read := buffer_TX

    val wError_RX = Wire(Bool())
    val error_RX =RegNext(wError_RX) // Probably Synchronize twice
    val wsync_busy_RX = Wire(Bool())
  val wsync_busy_RX_ff = RegNext(wsync_busy_RX) // Probably Synchronize twice
    io.regs.busy_RX.read := RegNext(wsync_busy_RX_ff) // Probably Synchronize twice
    val wip = Wire(Bool())
    val wip_ff = RegNext(wip)
  val ipRX = RegEnable(io.regs.ipRX.write.bits.toBool || (wip && ~wip_ff), io.regs.ipRX.write.valid || (wip && ~wip_ff))
    io.regs.ipRX.read := ipRX
    io.interrupt := ipRX
  val bufferLockRX = RegEnable(io.regs.bufferLockRX.write.bits.toBool, io.regs.bufferLockRX.write.valid)
    io.regs.bufferLockRX.read := bufferLockRX
  val bufferStatRX = RegInit(0.U(NumBuffers.W)) 
    io.regs.bufferStatRX.read := bufferStatRX
    val wlastBuffer_RX = Wire(UInt(NumBuffers.W))
    val synclastBuffer_RX = RegNext(wlastBuffer_RX) // Probably Synchronize twice
    when(~wsync_busy_RX_ff && io.regs.busy_RX.read.toBool && ~error_RX && ~io.regs.bufferStatRX.write.valid){  // Falling Edge and no error -> Update Buffer Stats
      bufferStatRX := bufferStatRX | synclastBuffer_RX
    }.elsewhen(io.regs.bufferStatRX.write.valid){
      bufferStatRX := io.regs.bufferStatRX.write.bits
    }
  val bufferSelectRX = RegEnable(io.regs.bufferSelectRX.write.bits, io.regs.bufferSelectRX.write.valid)
    io.regs.bufferSelectRX.read := bufferSelectRX

  mem_TX.io.MemA.Addr := io.TXaddr
  mem_TX.io.MemA.wrData := io.TXwrdata
  mem_TX.io.MemA.wr := io.TXwrena
  io.TXrddata:= mem_TX.io.MemA.rdData

  mem_RX.io.MemA.Addr := io.RXaddr
  mem_RX.io.MemA.wrData := io.RXwrdata
  mem_RX.io.MemA.wr := io.RXwrena
  io.RXrddata:= mem_RX.io.MemA.rdData

  io.regs.PHYEst.read := m_rgmii.io.PHYStat.Link
  io.regs.PHYLink.read := m_rgmii.io.PHYStat.Speed
  io.regs.PHYDuplex.read := m_rgmii.io.PHYStat.Duplex

  mem_RX.io.clockA := clock
  mem_TX.io.clockA := clock

  mem_RX.io.clockB := io.EthernetClock125 
  mem_TX.io.clockB := io.EthernetClock125 
  m_fifo.io.EthernetBus.busclock := io.EthernetClock125 
  io.PHY_nrst := ~reset.toBool
  
  // PHY Logic Here
  withClock(io.EthernetClock125){
    io.RGMII <> m_rgmii.io.PHY 
    
    m_rgmii.io.clk_250M := io.EthernetClock250
    m_fifo.io.PHYStat := m_rgmii.io.PHYStat 
    m_rgmii.io.Bus <> m_fifo.io.PHYBus

    def TX_FSM_busy: Bool = {
        val start_Trig = RegNext(RegNext(start_TX && ~start_TX_ff))
        val ( s_idle :: s_delay0 :: s_delay1 :: s_getLength :: s_send_Data :: s_finish :: Nil) = Enum(6)
        val state = RegInit(s_idle)
        state.suggestName("State_TX")
        val byteCounter = RegInit(0.U( log2Ceil(Indizes).W ))
        byteCounter.suggestName("byteCounter_TX")
        val shiftCounter = RegInit(0.U( log2Ceil(6).W ))
        shiftCounter.suggestName("shiftCounter_TX")
        val selectedBuffer = RegNext(RegNext(buffer_TX))
        val data = mem_TX.io.MemB.rdData
        val address = RegInit(0.U(log2Ceil(Indizes*NumBuffers).W))
        mem_TX.io.MemB.Addr := address
        mem_TX.io.MemB.wrData := 0.U
        mem_TX.io.MemB.wr := false.B
        
        val frun = RegInit(false.B)
        val fstrb = RegInit(0.U(1.W))
        val fdata = RegInit(0.U(8.W))
        m_fifo.io.EthernetBus.tx.run := frun
        m_fifo.io.EthernetBus.tx.data := fdata
        m_fifo.io.EthernetBus.tx.strb := fstrb

        switch(state){
          is(s_idle){
            frun := false.B
            when(start_Trig){
              address := (selectedBuffer * Indizes.U)+0.U
              state := s_delay0
              shiftCounter := 0.U
            }
          }
          is(s_delay0){
            state := s_delay1
            address := address + 1.U
          }
          is(s_delay1){
            state := s_getLength
            address := address + 1.U
          }
          is(s_getLength){
            
            when(shiftCounter > 3.U){
              when( m_fifo.io.EthernetBus.tx.ready ){
                state := s_send_Data
                byteCounter := byteCounter + 2.U  // Add 2 Bytes, for counting until 0
                frun := true.B
                shiftCounter := 0.U
              }
            }.otherwise{
              shiftCounter := shiftCounter + 1.U
              byteCounter := byteCounter | (data << (shiftCounter*8.U))
              when(shiftCounter === 0.U){
                address := address + 1.U
              }
            }
            fdata := 0.U
            fstrb := 0.U
          }
          is(s_send_Data){
            when( m_fifo.io.EthernetBus.tx.ready ){ // FIFO not Full
              byteCounter := byteCounter - 1.U
              address := address + 1.U
              when(byteCounter === 0.U){
                state := s_finish
              }
              fdata := data
              fstrb := 1.U
            }
          }
          is(s_finish){
            when( m_fifo.io.EthernetBus.tx.ready ){
              state := s_idle
            }
            fdata := 0.U
            fstrb := 0.U
          }
        }
      state =/= s_idle
    }
    wsync_busy_TX := TX_FSM_busy

    def RX_FSM_busy: Bool = {
        val ( s_idle :: s_receive_first :: s_writeDestMAC :: s_writeSrcMAC :: s_writeLength :: s_finish :: Nil) = Enum(6)
        val state = RegInit(s_idle)
        state.suggestName("State_RX")
        val byteCounter = RegInit(0.U( log2Ceil(Indizes).W ))
        val shiftCounter = RegInit(0.U( log2Ceil(6).W ))
        val bufferStates = RegInit(0.U(NumBuffers.W))//RegNext(RegNext(bufferStatRX))
        val bufferStates_ff = RegInit(0.U(NumBuffers.W))
        bufferStates_ff := bufferStatRX
        bufferStates := bufferStates_ff
        val selectedBuffer = RegInit(0.U(log2Ceil(NumBuffers).W))
        selectedBuffer.suggestName("selectedBuffer_RX")
        val wselectedBuffer = Wire(UInt(log2Ceil(NumBuffers).W))
        wselectedBuffer.suggestName("wselectedBuffer_RX")
        val error = RegInit(false.B)
        val lock = RegNext(RegNext(bufferLockRX))
        lock.suggestName("lock_RX")
        val lockedBuffer =  RegInit(0.U(NumBuffers.W))//RegNext(RegNext(UIntToOH(bufferSelectRX)))
        val lockedBuffer_ff = RegInit(0.U(NumBuffers.W))
        lockedBuffer_ff := UIntToOH(bufferSelectRX)>>1
        lockedBuffer := lockedBuffer_ff
        lockedBuffer.suggestName("lockedBuffer_RX")
        val data = RegInit(0.U(8.W))
        val address = RegInit(0.U(log2Ceil(Indizes*NumBuffers).W))
        val wr = RegInit(false.B)
        val ip = RegInit(false.B)
        val eth_info = RegInit(0.U.asTypeOf(m_fifo.io.EthernetBus.rx.info))
        mem_RX.io.MemB.Addr := address
        mem_RX.io.MemB.wrData := data
        mem_RX.io.MemB.wr := wr
        wip := ip
        wError_RX := error
        wlastBuffer_RX := UIntToOH(selectedBuffer)

        when(lock){
          when( bufferStates < (scala.math.pow(2,NumBuffers)-1).ceil.toInt.U ){
            wselectedBuffer := PriorityEncoder( ~(bufferStates | lockedBuffer) )
          }.otherwise{
            wselectedBuffer := PriorityEncoder( ~lockedBuffer )
          }
        }.otherwise{
          when( bufferStates < (scala.math.pow(2,NumBuffers)-1).ceil.toInt.U ){
            wselectedBuffer := PriorityEncoder( ~bufferStates )
          }.otherwise{
            wselectedBuffer := 0.U
          }
        }

        switch(state){
          is(s_idle){
            wr := false.B
            when(m_fifo.io.EthernetBus.rx.strb > 0.U){
              eth_info := m_fifo.io.EthernetBus.rx.info
              ip := false.B
              error := m_fifo.io.EthernetBus.rx.error
              data := m_fifo.io.EthernetBus.rx.data 
              byteCounter := 19.U//1.U
              shiftCounter := 0.U
              wr := true.B
              address := (wselectedBuffer * Indizes.U) + 18.U
              selectedBuffer := wselectedBuffer 
              state := s_receive_first
            }
          }
          is(s_receive_first){
            when((m_fifo.io.EthernetBus.rx.strb > 0.U) && ~m_fifo.io.EthernetBus.rx.empty){
              error := m_fifo.io.EthernetBus.rx.error
              byteCounter := byteCounter + 1.U
              address := address + 1.U
              data :=  m_fifo.io.EthernetBus.rx.data
              wr := true.B
            }.otherwise{
              wr := false.B
            }
            when(m_fifo.io.EthernetBus.rx.last){
              state := s_writeLength
              shiftCounter := 0.U
            }
          }
          is(s_writeLength){
            wr := true.B
            data := (byteCounter >> (shiftCounter*8.U)) & "hFF".U
            
            when (shiftCounter === 0.U){
              error := m_fifo.io.EthernetBus.rx.error
              address := (selectedBuffer * Indizes.U)
              shiftCounter := shiftCounter + 1.U
            }.elsewhen(shiftCounter >= 3.U){
              shiftCounter := 0.U
              address := address + 1.U
              state := s_writeDestMAC
            }.otherwise{
              address := address + 1.U
              shiftCounter := shiftCounter + 1.U
            }
          }
          is(s_writeDestMAC){
            when(shiftCounter >= 5.U){
              shiftCounter := 0.U
              state := s_writeSrcMAC
            }.otherwise{
              shiftCounter := shiftCounter + 1.U
            }
            wr := true.B
            data :=  (eth_info.destMAC >> ((5.U-shiftCounter)*8.U)) & "hFF".U
            address := address + 1.U

          }
          is(s_writeSrcMAC){
            when(shiftCounter >= 5.U){
              shiftCounter := 0.U
              state := s_finish
            }.otherwise{
              shiftCounter := shiftCounter + 1.U
            }
            wr := true.B
            data :=  (eth_info.srcMAC >> ((5.U-shiftCounter)*8.U)) & "hFF".U
            address := address + 1.U
          }
          is(s_finish){
            when(shiftCounter >= 1.U){
              shiftCounter := 0.U
              state := s_idle
            }.otherwise{
              shiftCounter := shiftCounter + 1.U
            }
            wr := true.B
            data :=  (eth_info.etype >> ((1.U-shiftCounter)*8.U)) & "hFF".U
            address := address + 1.U

            when(~error){
              ip := true.B
            }
            
          }
        }
      state =/= s_idle
    }
    wsync_busy_RX := RX_FSM_busy


  }


  // Register Descriptions
  protected val PHY_desc : Seq[RegFieldDesc] = Seq(
    RegFieldDesc(s"${prefix}_PHYEst", "Link established", access=RegFieldAccessType.R, volatile=true),
    RegFieldDesc(s"${prefix}_PHYDupl", "Half or Full Duplex", access=RegFieldAccessType.R, volatile=true),
    RegFieldDesc(s"${prefix}_PHYForce", "Force Link Speed"),
    RegFieldDesc(s"${prefix}_PHYSpeed", "Linkspeed", access=RegFieldAccessType.R, volatile=true)
  )
  protected val TX_desc : Seq[RegFieldDesc] = Seq(
    RegFieldDesc(s"${prefix}_TXWriteEn", "Enable Writeaccess to Buffer"),
    RegFieldDesc(s"${prefix}_TXStart", "Start Transmission of selected Buffer"),
    RegFieldDesc(s"${prefix}_TXBusy", "Transmission in Progress", access=RegFieldAccessType.R, volatile=true),
    RegFieldDesc(s"${prefix}_TXBuffer", "Buffer for Sending")
  )
  protected val RX_desc : Seq[RegFieldDesc] = Seq(
    RegFieldDesc(s"${prefix}_RXBusy", "Receiver Busy", access=RegFieldAccessType.R, volatile=true),
    RegFieldDesc(s"${prefix}_RXIp", "Interrupt Bit", volatile=true),
    RegFieldDesc(s"${prefix}_RXLock", "Enable Lock"),
    RegFieldDesc(s"${prefix}_RXBuffer", "Buffer for Lock"),
    RegFieldDesc(s"${prefix}_RXStat", "Buffer Status", volatile=true)
  )
  protected def txaddr_desc:    RegFieldDesc = RegFieldDesc(s"${prefix}_wraddr", "Write Address")
  protected def txdata_desc:    RegFieldDesc = RegFieldDesc(s"${prefix}_wrdata", "Write Data")
  protected def rxaddr_desc:    RegFieldDesc = RegFieldDesc(s"${prefix}_rdaddr", "Read Address")
  protected def rxdata_desc:    RegFieldDesc = RegFieldDesc(s"${prefix}_rddata", "Read Data", access=RegFieldAccessType.R, volatile=true)
}

object ETHCtrl {

  // Seq( (i1, reg1), (i2,reg2) )  -> This sequence holds elements, with i1 offset of reg1 etc. ++ concats inner elements
  def RegMap(mod: ETHCtrl, regBytes: Int): Seq[(Int, Seq[RegField])] = {
    val PHYCtrl = Seq( regBytes*0 -> RegFieldGroup("PHYCtrl", Some("PHYCtrl"), 
                                Seq(
                                  mod.io.regs.PHYEst.toRegField(Some(mod.PHY_desc(0))),
                                  mod.io.regs.PHYDuplex.toRegField(Some(mod.PHY_desc(1))),
                                  mod.io.regs.PHYForce.toRegField(Some(mod.PHY_desc(2))),
                                  mod.io.regs.PHYLink.toRegField(Some(mod.PHY_desc(3)))
                                )
                              ))
    val TXCtrl = Seq( regBytes*1 -> RegFieldGroup("TXCtrl", Some("TXCtrl"), 
                                Seq(
                                  RegField(1),
                                  mod.io.regs.start_TX.toRegField(Some(mod.TX_desc(1))),
                                  mod.io.regs.busy_TX.toRegField(Some(mod.TX_desc(2))),
                                  RegField(5),
                                  mod.io.regs.buffer_TX.toRegField(Some(mod.TX_desc(3)))
                                )
                              ))
    val RXCtrl = Seq( regBytes*2 -> RegFieldGroup("RXCtrl", Some("RXCtrl"), 
                                Seq(
                                  mod.io.regs.busy_RX.toRegField(Some(mod.RX_desc(0))),
                                  mod.io.regs.ipRX.toRegField(Some(mod.RX_desc(1))),
                                  mod.io.regs.bufferLockRX.toRegField(Some(mod.RX_desc(2))),
                                  RegField(5),
                                  mod.io.regs.bufferSelectRX.toRegField(Some(mod.RX_desc(3))),
                                  mod.io.regs.bufferStatRX.toRegField(Some(mod.RX_desc(4)))
                                )
                              ))

    PHYCtrl ++ TXCtrl ++ RXCtrl 
  }
}