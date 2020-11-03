package juli.blocks.devices.ethctrl

import Memory.onChip.TrueDualPortBRAM

import chisel3._
import chisel3.util._

import freechips.rocketchip.regmapper._
import sifive.blocks.util.{SlaveRegIF, GenericTimerIO, GenericTimer, GenericTimerCfgDescs, DefaultGenericTimerCfgDescs}

class ETHCtrl(regWidth: Int = 32, Indizes: Int = 5, NumBuffers: Int = 3) extends Module {
  protected val prefix = "ethctrl"
  protected def regBytes = regWidth/8
  val io = IO(new Bundle {
    val RGMII = new Ethernet.Interface.Types.RGMII_Interface()
    val EthernetClock125 = Input(Clock())
    val EthernetClock250 = Input(Clock())
    val interrupt = Output(Bool())

    val regs = new Bundle{
      val PHYEst = new SlaveRegIF(1)
      val PHYDuplex = new SlaveRegIF(1)
      val PHYForce = new SlaveRegIF(2)
      val PHYLink = new SlaveRegIF(4)

      val wrena = new SlaveRegIF(1)
      val start_TX = new SlaveRegIF(1)
      val busy_TX = new SlaveRegIF(1)
      val buffer_TX = new SlaveRegIF(8)

      val busy_RX = new SlaveRegIF(1)
      val ipRX = new SlaveRegIF(1)
      val bufferLockRX = new SlaveRegIF(1)
      val bufferStatRX = new SlaveRegIF(8)
      val bufferSelectRX = new SlaveRegIF(8)
      
      val wraddr = new SlaveRegIF(log2Ceil(Indizes))
      val wrdata = new SlaveRegIF(regWidth)
      val rdaddr = new SlaveRegIF(log2Ceil(Indizes))
      val rddata = new SlaveRegIF(regWidth)
    }
  })
  
  val mem_RX = Module(new TrueDualPortBRAM(Indizes*NumBuffers, UInt(regWidth.W)))
  val mem_TX = Module(new TrueDualPortBRAM(Indizes*NumBuffers, UInt(regWidth.W)))
  val m_fifo = Module(new Ethernet.Protocol.EthernetFIFO8())
  val m_rgmii = Module(new Ethernet.Interface.RGMII.RGMII())
  m_rgmii.clock := io.EthernetClock125

  // Register
  val PHYForce = RegEnable(io.regs.PHYForce.write.bits, io.regs.PHYForce.write.valid)
  io.regs.PHYForce.read := PHYForce


  val wrena = RegEnable(io.regs.wrena.write.bits, io.regs.wrena.write.valid)
    io.regs.wrena.read := wrena
    mem_TX.io.MemIO.wr := wrena
  val start_TX = RegEnable(io.regs.start_TX.write.bits, io.regs.start_TX.write.valid)
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
  val ipRX = RegEnable(io.regs.ipRX.write.bits || wip, io.regs.ipRX.write.valid || wip)
    io.regs.ipRX.read := ipRX
    interrupt := ipRX
  val bufferLockRX = RegEnable(io.regs.bufferLockRX.write.bits, io.regs.bufferLockRX.write.valid)
    io.regs.bufferLockRX.read := wbufferLockRX
  val bufferStatRX = RegInit(0.U(NumBuffers.W)) 
    io.regs.bufferStatRX.read := bufferStatRX
    when(io.regs.bufferStatRX.write.valid){
      bufferStatRX := io.regs.bufferStatRX.write.bits
    }
    val wlastBuffer_RX := Wire(UInt(NumBuffers.W))
    val synclastBuffer_RX := RegNext(wlastBuffer_RX) // Probably Synchronize twice
    when(~wsync_busy_RX_ff && io.regs.busy_RX.read && ~error_RX){  // Falling Edge and no error
      bufferStatRX := io.regs.bufferStatRX.write.bits | synclastBuffer_RX
    } 
  val bufferSelectRX = RegEnable(io.regs.bufferSelectRX.write.bits, io.regs.bufferSelectRX.write.valid)
    io.regs.bufferSelectRX.read := bufferSelectRX

  val wraddr = RegEnable(io.regs.wraddr.write.bits, io.regs.wraddr.write.valid)
    io.regs.wraddr.read := wraddr
    mem_TX.io.MemIO.wrAddr := wraddr
  val wrdata = RegEnable(io.regs.wrdata.write.bits, io.regs.wrdata.write.valid)
    io.regs.wrdata.read := wrdata
    mem_TX.io.MemIO.wrData := wrdata
  val rdaddr = RegEnable(io.regs.rdaddr.write.bits, io.regs.rdaddr.write.valid)
    io.regs.rdaddr.read := rdaddr
    mem_RX.io.MemIO.rdAddr := rdaddr

  io.regs.rddata.read := mem.io.MemIO.rdData

  io.PHYEst.read := m_rgmii.io.PHYStat.Link
  io.PHYLink.read := m_rgmii.io.PHYStat.Speed
  io.PHYDuplex.read := m_rgmii.io.PHYStat.Duplex

  mem_RX.io.clockRd := clock
  mem_TX.io.clockWr := clock

  mem_RX.io.clockWr := io.EthernetClock125 
  mem_TX.io.clockRd := io.EthernetClock125 
  m_fifo.io.EthernetBus.busclock := io.EthernetClock125 

  // PHY Logic Here
  withClock(io.EthernetClock125){
    io.RGMII <> m_rgmii.io.PHY 
    io.PHY_nrst := ~reset
    m_rgmii.io.clk_250M := io.EthernetClock250
    m_fifo.io.PHYStat := m_rgmii.io.PHYStat 
    m_rgmii.io.Bus <> m_fifo.io.PHYBus

    protected def TX_FSM_busy: Bool = {
        val start_Trig = RegNext(RegNext(start_TX && ~start_TX_ff))
        val ( s_idle :: s_setAddress :: s_getLength :: s_send_Data :: s_finish :: Nil) = Enum(UInt(), 5)
        val state = RegInit(s_idle)
        val byteCounter = RegInit(UInt( log2Ceil(regBytes*Indizes).W ))
        switch(state){
          is(s_idle){
            
          }
        }
      state =/= s_idle
    }
    wsync_busy_TX := TX_FSM_busy

    protected def RX_FSM_busy: Bool = {
        val ( s_idle :: s_receive_first :: s_receive :: s_writeMACs :: s_writeLength :: s_finish :: Nil) = Enum(UInt(), 6)
        val state = RegInit(s_idle)
        val byteCounter = RegInit(UInt( log2Ceil(regBytes*Indizes).W ))
        val shiftCounter = RegInit(UInt( log2Ceil(4).W ))
        val bufferStates = RegNext(RegNext(bufferStatRX))
        val selectedBuffer = RegInit(0.U(log2Ceil(NumBuffers.W)))
        val error = RegInit(false.B)
        val lock = RegNext(RegNext(bufferLockRX))
        val lockedBuffer = RegNext(RegNext(UIntToOH(bufferSelectRX)))
        val data = RegInit(0.U(32.W))
        val address = RegInit(0.U(log2Ceil(Indizes).W))
        val wr = RegInit(false.B)
        mem_RX.io.MemIO.wrAddr := address
        mem_RX.io.MemIO.wrData := data
        mem_RX.io.MemIO.wr := wr

        wError_RX := error
        wlastBuffer_RX := UIntToOH(selectedBuffer)


        switch(state){
          is(s_idle){
            wr := false.B
            when(m_fifo.io.EthernetBus.rx.strb)
              error := m_fifo.io.EthernetBus.rx.error
              data := m_fifo.io.EthernetBus.rx.data << 16.U | m_fifo.io.EthernetBus.rx.info.etype
              byteCounter := 1.U
              shiftCounter := 3.U
              when(lock){
                when( bufferStates < (scala.math.pow(2,NumBuffers)-1).U ){
                  selectedBuffer := PriorityEncoder( ~(bufferStates | lockedBuffer) )
                }.otherwise{
                  selectedBuffer := PriorityEncoder( ~(lockedBuffer) )
                }
              }.otherwise{
                when( bufferStates < (scala.math.pow(2,NumBuffers)-1).U ){
                  selectedBuffer := PriorityEncoder( ~(bufferStates) )
                }.otherwise{
                  selectedBuffer := 0.U
                }
              }
            }
          }
          is(s_receive_first){
            when(m_fifo.io.EthernetBus.rx.strb && ~m_fifo.io.EthernetBus.rx.empty){
              error := m_fifo.io.EthernetBus.rx.error
              byteCounter := byteCounter + 1.U
              shiftCounter := 0.U
              state := s_receive
              address := (selectedBuffer * Indizes.U)+4.U
              data := data | m_fifo.io.EthernetBus.rx.data << (shiftCounter * 8.U)
              wr := true.B
            }
          }
          is(s_receive){
            when(m_fifo.io.EthernetBus.rx.strb && ~m_fifo.io.EthernetBus.rx.empty){ // strb and last were always together true!!
              error := m_fifo.io.EthernetBus.rx.error
              when(shiftCounter < 3.U){
                shiftCounter := shiftCounter + 1.U
              }.otherwise{
                shiftCounter := 0.U
              }
              
              wr := shiftCounter === 3.U  // Data is full -> Let's write Data
              byteCounter := byteCounter + 1.U
              
              when(shiftCounter === 0.U){
                address := address+1.U
                data := m_fifo.io.EthernetBus.rx.data << (shiftCounter * 8.U)
              }.otherwise{
                data := data | m_fifo.io.EthernetBus.rx.data << (shiftCounter * 8.U)
              }
              
            }.otherwise{
              wr := false.B
            }
            when(m_fifo.io.EthernetBus.rx.last){
              state := s_writeMACs
            }
          }
          is(s_writeMACs){
            error := m_fifo.io.EthernetBus.rx.error
            when(shiftCounter < 3.U){
              wr := true.B
            }
            shiftCounter := 0.U
            state := s_writeLength
          }
          is(s_writeLength){
            wr := true.B
            when(shiftCounter === 0.U){
              address := (selectedBuffer * Indizes.U)+1.U
              data := m_fifo.io.EthernetBus.rx.info.destMAC(31,0)
            }.elsewhen(shiftCounter === 1.U){
              address := address + 1.U
              data := m_fifo.io.EthernetBus.rx.info.srcMAC(15,0) << 16 | m_fifo.io.EthernetBus.rx.info.destMAC(47,32)
            }.otherwise{
              address := address + 1.U
              data := m_fifo.io.EthernetBus.rx.info.srcMAC(47,16)
            }
            shiftCounter := shiftCounter + 1.U
            state := s_finish
          }
          is(s_finish){
            address := (selectedBuffer * Indizes.U)+1.U
            data := byteCounter
            state := s_idle
          }
        }
      state =/= s_idle
    }
    wsync_busy_RX := RX_FSM_busy


  }


  // Register Descriptions
  protected val PHY_desc : Seq[RegFieldDesc] = Seq(
    RegFieldDesc(s"${prefix}_PHYEst", "Link established", access=RegFieldAccessType.R, volatile=true)
    RegFieldDesc(s"${prefix}_PHYDupl", "Half or Full Duplex", access=RegFieldAccessType.R, volatile=true)
    RegFieldDesc(s"${prefix}_PHYForce", "Force Link Speed")
    RegFieldDesc(s"${prefix}_PHYSpeed", "Linkspeed", access=RegFieldAccessType.R, volatile=true)
  )
  protected val TX_desc : Seq[RegFieldDesc] = Seq(
    RegFieldDesc(s"${prefix}_TXWriteEn", "Enable Writeaccess to Buffer")
    RegFieldDesc(s"${prefix}_TXStart", "Start Transmission of selected Buffer")
    RegFieldDesc(s"${prefix}_TXBusy", "Transmission in Progress", access=RegFieldAccessType.R, volatile=true)
    RegFieldDesc(s"${prefix}_TXBuffer", "Buffer for Sending")
  )
  protected val RX_desc : Seq[RegFieldDesc] = Seq(
    RegFieldDesc(s"${prefix}_RXBusy", "Receiver Busy", access=RegFieldAccessType.R, volatile=true)
    RegFieldDesc(s"${prefix}_RXIp", "Interrupt Bit", volatile=true)
    RegFieldDesc(s"${prefix}_RXLock", "Enable Lock")
    RegFieldDesc(s"${prefix}_RXBuffer", "Buffer for Lock")
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
                                  mod.io.regs.wrena.toRegField(Some(mod.TX_desc(0))),
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

    val wraddr = Seq( regBytes*3 -> Seq(mod.io.regs.wraddr.toRegField(Some(mod.txaddr_desc))) )
    val wrdata = Seq( regBytes*4 -> Seq(mod.io.regs.wrdata.toRegField(Some(mod.txdata_desc))) )
    val rdaddr = Seq( regBytes*5 -> Seq(mod.io.regs.rdaddr.toRegField(Some(mod.rxaddr_desc))) )
    val rddata = Seq( regBytes*6 -> Seq(mod.io.regs.rddata.toRegField(Some(mod.rxdata_desc))) )

    PHYCtrl ++ TXCtrl ++ RXCtrl ++ wraddr ++ wrdata ++ rdaddr ++ rddata
  }
}