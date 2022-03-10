package juli.blocks.tilelink
import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.tilelink.{TLBundleParameters, TLBundle, TLBundleA, TLBundleD}
import FIFO.{Async_FIFO_new}

class Tilelink_ClockCrosser(tlparam : TLBundleParameters) extends RawModule{
    val io = IO(  new Bundle{
        //val dram_clock = Input(Clock())
        val reset = Input(Bool())
        val node_in = Flipped(TLBundle(tlparam))
        val clock_in = Input(Clock())
        val node_out = TLBundle(tlparam)
        val clock_out = Input(Clock())
    })
    withClockAndReset( io.clock_in, io.reset){
        val to_node_out = Module(new Async_FIFO_new(new Bundle{
            val a_bundle = new TLBundleA(tlparam)
            val a_valid = Bool()
            val d_ready = Bool()
        }, 4)) // 4 Entries 
        to_node_out.suggestName("to_node_out")
        to_node_out.io.clkA := io.clock_in
        to_node_out.io.clkB := io.clock_out
        to_node_out.io.queue.deq.ready := true.B

        val to_node_in = Module(new Async_FIFO_new(new Bundle{
            val d_bundle = new TLBundleD(tlparam)
            val a_ready = Bool()
            val d_valid = Bool()
        }, 4)) // 4 Entries 
        to_node_in.suggestName("to_node_in")
        to_node_in.io.clkB := io.clock_in
        to_node_in.io.clkA := io.clock_out
        to_node_in.io.queue.deq.ready := true.B
        //mgmt_fifo.io.queue.deq <> io.mgmt_deq(i)
        //mgmt_fifo.io.queue.enq.valid := enq_mgmt(i)
        //mgmt_fifo.io.queue.enq.bits := mgmt(i)


        val wReset_in = Wire(Bool())
        withClockAndReset( io.clock_in, io.reset){
            val reg_reset = RegNext(RegNext(RegNext(io.reset)))
            wReset_in := reg_reset
        }
        withClockAndReset( io.clock_in, wReset_in ){
            io.node_in.b.valid := false.B
            io.node_in.c.ready := true.B
            io.node_in.e.ready := true.B
            io.node_in.b.bits := 0.U.asTypeOf(io.node_in.b.bits)

            // Output Logic
            val a_ready_in = RegInit(true.B)  
            //val a_valid_in = RegInit(false.B)  
            //val a_channel_in = RegEnable(io.node_in.a.bits, io.node_in.a.fire())  
            io.node_in.a.ready := a_ready_in
            to_node_out.io.queue.enq.bits.a_bundle := io.node_in.a.bits
            to_node_out.io.queue.enq.bits.a_valid := io.node_in.a.valid
            to_node_out.io.queue.enq.bits.d_ready := io.node_in.d.ready
            to_node_out.io.queue.enq.valid := io.node_in.a.fire() || RegNext(io.node_in.d.fire()) || Helper.RTrig(io.node_in.d.ready)

            val d_channel_in = RegEnable(to_node_in.io.queue.deq.bits.d_bundle, to_node_in.io.queue.deq.bits.d_valid && to_node_in.io.queue.deq.valid)  
            val d_valid_in = RegInit(false.B)
            io.node_in.d.valid := d_valid_in
            io.node_in.d.bits := d_channel_in

            when(io.node_in.a.fire()){
                a_ready_in := false.B
            }.elsewhen(to_node_in.io.queue.deq.bits.a_ready && to_node_in.io.queue.deq.valid){
                a_ready_in := true.B
            }
            when(io.node_in.d.fire()){
                d_valid_in := false.B
            }.elsewhen(to_node_in.io.queue.deq.bits.d_valid && to_node_in.io.queue.deq.valid){
                d_valid_in := true.B
            }
        }

        val wReset_out = Wire(Bool())
        withClockAndReset( io.clock_out, io.reset){
            val reg_reset = RegNext(RegNext(RegNext(io.reset)))
            wReset_out := reg_reset
        }
        withClockAndReset( io.clock_out, wReset_out ){
            io.node_out.b.ready := true.B
            io.node_out.c.valid := false.B
            io.node_out.e.valid := false.B
            io.node_out.c.bits := 0.U.asTypeOf(io.node_out.c.bits)
            io.node_out.e.bits := 0.U.asTypeOf(io.node_out.e.bits)

            // Output Logic
            val a_valid_out = RegInit(false.B)  
            val a_channel_out = RegEnable(to_node_out.io.queue.deq.bits.a_bundle, to_node_out.io.queue.deq.bits.a_valid && to_node_out.io.queue.deq.valid)  
            io.node_out.a.valid := a_valid_out
            io.node_out.a.bits := a_channel_out

            //val d_valid_out = RegInit(false.B)
            val d_ready_out = RegInit(false.B)
            //val d_channel_out = RegEnable(io.node_out.d.bits, io.node_out.d.fire())  
            io.node_out.d.ready := d_ready_out

            to_node_in.io.queue.enq.bits.d_bundle := io.node_out.d.bits
            to_node_in.io.queue.enq.bits.d_valid := io.node_out.d.valid
            to_node_in.io.queue.enq.bits.a_ready := io.node_out.a.ready
            to_node_in.io.queue.enq.valid := io.node_out.d.fire() || Helper.RTrig(io.node_out.a.ready) || ( io.node_out.a.ready && RegNext( io.node_out.a.fire() ))

            when(io.node_out.d.fire()){
                d_ready_out := false.B
            }.elsewhen(to_node_out.io.queue.deq.bits.d_ready && to_node_out.io.queue.deq.valid){
                d_ready_out := true.B
            }
            when(io.node_out.a.fire()){
                a_valid_out := false.B
            }.elsewhen(to_node_out.io.queue.deq.bits.a_valid && to_node_out.io.queue.deq.valid){
                a_valid_out := true.B
            }

        }
    }
}


// java -jar rocket-chip/sbt-launch.jar ++2.12.4 "runMain juli.blocks.tilelink.Tilelink_ClockCrosser" 
object Tilelink_ClockCrosser extends App {
    val tlparam =  TLBundleParameters( 8, 32, 8, 8, 10)
    //val tlparam = TLBundleParameters( addressBits, dataBits, sourceBits, sinkBits, sizeBits)

    chisel3.Driver.execute(Array("--target-dir", "generated/Tilelink_ClockCrosser"), () => new Tilelink_ClockCrosser(tlparam))
}

/*
class Tilelink_ClockCrosser(tlparam : TLBundleParameters) extends RawModule{
    val io = IO(  new Bundle{
        //val dram_clock = Input(Clock())
        val reset = Input(Bool())
        val node_in = Flipped(TLBundle(tlparam))
        val clock_in = Input(Clock())
        val node_out = TLBundle(tlparam)
        val clock_out = Input(Clock())
    })
        val valid_a_in = Wire(Bool())
        val ready_a_in = Wire(Bool())
        val bits_a_in = Wire( io.node_in.a.bits.cloneType )
        val valid_d_in = Wire(Bool())
        val ready_d_in = Wire(Bool())
        val bits_d_in = Wire( io.node_in.d.bits.cloneType )


        val wReset_in = Wire(Bool())
        withClockAndReset( io.clock_in, io.reset){
            val reg_reset = RegNext(RegNext(RegNext(io.reset)))
            wReset_in := reg_reset
        }
        withClockAndReset( io.clock_in, wReset_in ){
            io.node_in.b.valid := false.B
            io.node_in.c.ready := true.B
            io.node_in.e.ready := true.B
            io.node_in.b.bits := 0.U.asTypeOf(io.node_in.b.bits)

            // Output Logic
            val a_ready_in = RegInit(true.B)  
            val a_valid_in = RegInit(false.B)  
            val a_channel_in = RegEnable(io.node_in.a.bits, io.node_in.a.fire())  
            bits_a_in := a_channel_in
            valid_a_in := a_valid_in
            io.node_in.a.ready := a_ready_in

            val d_channel_in = RegNext(RegNext(RegNext(bits_d_in)))
            val d_valid_in = RegInit(false.B)
            val d_ready_in = RegInit(false.B)
            io.node_in.d.valid := d_valid_in
            ready_d_in := d_ready_in
            io.node_in.d.bits := d_channel_in

            when(io.node_in.a.fire()){
                a_valid_in := ~a_valid_in
            }
            when(io.node_in.d.fire()){
                d_ready_in := ~d_ready_in
            }

            // Clock Crossing
            val wa_ready_in_ff = RegNext(RegNext(ready_a_in))
            val wa_ready_in_2ff = RegNext(wa_ready_in_ff)  
            when(io.node_in.a.fire()){
                a_ready_in := false.B
            }.elsewhen(wa_ready_in_ff =/= wa_ready_in_2ff ){
                a_ready_in := true.B
            }

            val wd_valid_in_ff = RegNext(RegNext(valid_d_in))
            val wd_valid_in_2ff = RegNext(wd_valid_in_ff)  
            when(io.node_in.d.fire()){
                d_valid_in := false.B
            }.elsewhen(wd_valid_in_ff =/= wd_valid_in_2ff ){
                d_valid_in := true.B
            }
        }

        val wReset_out = Wire(Bool())
        withClockAndReset( io.clock_out, io.reset){
            val reg_reset = RegNext(RegNext(RegNext(io.reset)))
            wReset_out := reg_reset
        }
        withClockAndReset( io.clock_out, wReset_out ){
            io.node_out.b.ready := true.B
            io.node_out.c.valid := false.B
            io.node_out.e.valid := false.B
            io.node_out.c.bits := 0.U.asTypeOf(io.node_out.c.bits)
            io.node_out.e.bits := 0.U.asTypeOf(io.node_out.e.bits)

            // Output Logic
            val a_valid_out = RegInit(false.B)  
            val a_ready_out = RegInit(false.B)  
            val a_channel_out = RegNext(RegNext(RegNext(bits_a_in)))
            ready_a_in := a_ready_out
            io.node_out.a.valid := a_valid_out
            io.node_out.a.bits := a_channel_out

            val d_valid_out = RegInit(false.B)
            val d_ready_out = RegInit(true.B)
            val d_channel_out = RegEnable(io.node_out.d.bits, io.node_out.d.fire())  
            valid_d_in := d_valid_out
            io.node_out.d.ready := d_ready_out
            bits_d_in := d_channel_out

            when(io.node_out.a.fire()){
                a_ready_out := ~a_ready_out
            }
            when(io.node_out.d.fire()){
                d_valid_out := ~d_valid_out
            }

            // Clock Crossing

            val wa_valid_out_ff = RegNext(RegNext(valid_a_in))
            val wa_valid_out_2ff = RegNext(wa_valid_out_ff)  
            when(io.node_out.a.fire()){
                a_valid_out := false.B
            }.elsewhen(wa_valid_out_ff =/= wa_valid_out_2ff ){
                a_valid_out := true.B
            }

            val wd_ready_out_ff = RegNext(RegNext(ready_d_in))
            val wd_ready_out_2ff = RegNext(wd_ready_out_ff)  
            when(io.node_out.d.fire()){
                d_ready_out := false.B
            }.elsewhen(wd_ready_out_ff =/= wd_ready_out_2ff ){
                d_ready_out := true.B
            }
        }
}
*/