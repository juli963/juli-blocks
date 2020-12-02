package juli.blocks.devices.swapper

import freechips.rocketchip.regmapper._
import sifive.blocks.util.{SlaveRegIF}
import Chisel._
import chisel3.experimental.{RawModule, withClockAndReset}

class Mod_ByteSwapper(inBytes : Int ,regBytes : Int) extends RawModule {
    val multiples_in = (inBytes.toFloat/regBytes.toFloat).ceil.toInt // Calculate number of Registers
    /* IO Block */
    val swap_in = for(i<-0 until multiples_in) yield{
            if (i < multiples_in-1){
                IO(new SlaveRegIF(regBytes*8))
            }else{
                IO(new SlaveRegIF(inBytes*8-i*regBytes*8))
            }
    }
    val swap_out = for(i<-0 until inBytes-1) yield{
        val multi = ((i+2).toFloat/regBytes.toFloat).ceil.toInt 
        val w = Seq.tabulate( multi ) (x=> 
                                        if (x < multi-1){
                                            //println(s"RegLen ${x}: ${(regBytes*8)}")
                                            IO(new SlaveRegIF( (regBytes*8) ))
                                        }else{
                                            //println(s"RegLen ${x}: ${((i+2)*8) - (x*(regBytes*8))}")
                                            IO(new SlaveRegIF( ((i+2)*8) - (x*(regBytes*8)) ))
                                        }
                                    )
        w
    }
    val clock = IO(Clock(INPUT))
    val reset = IO(Bool(INPUT))

    /* Logic Implementation */
    withClockAndReset(clock, reset){
        val swapreg = Seq.tabulate(multiples_in) {i => RegEnable(swap_in(i).write.bits, swap_in(i).write.valid)}
        
        for(i<-0 until multiples_in) {
            swap_in(i).read := swapreg(i)
        }
        val swapbytes = for(i<- 0 until inBytes) yield{ 
            val reg = Cat(swapreg.reverse)( (i+1)*8-1, i*8)
            reg
            }
            
        val swap_out_in = for(i<-0 until inBytes-1) yield{
                                            val multiples = (((i+2)).toFloat/(regBytes).toFloat).ceil.toInt // Calculate number of Registers
                                            val w = Seq.tabulate( multiples ) (x=> 
                                                                        if (x < multiples-1){
                                                                            Wire(new SlaveRegIF( (regBytes*8) ))
                                                                        }else{
                                                                            Wire(new SlaveRegIF( ((i+2)*8) - (x*(regBytes*8)) ))
                                                                        }
                                                                    )
                                            val out = Cat(swapbytes.slice(0, i+2))
                                            for(x <- 0 until multiples){
                                                if (x < multiples-1){
                                                    w(x).read := out( ((x+1)*(regBytes*8))-1 ,x*(regBytes*8) )
                                                }else{
                                                    w(x).read := out( ((i+2)*8)-1 , x*(regBytes*8) )
                                                }
                                            }
                                            w
                                    }  

        for(i <- 0 until swap_out.length){
            for (x <- 0 until swap_out(i).length){
                swap_out(i)(x) <> swap_out_in(i)(x)
            }
        }
    }
  def swap_in_desc(subnum : Int):                   RegFieldDesc = RegFieldDesc(s"in_swap_${subnum}", s"Swap_${subnum} Input ")
  def swap_out_desc(num : Int, subnum : Int):       RegFieldDesc = RegFieldDesc(s"out_swap${(num+2)*8}_${subnum}", s"Swap${(num+2)*8} Output_${subnum}", access=RegFieldAccessType.R, volatile=true)
}

object ByteSwapper {
    def RegMap (mod: Mod_ByteSwapper, offset: Int, regBytes: Int): Seq[(Int, Seq[RegField])] = {
        var id = 0
        val regmap_in =  for (i <- 0 until mod.swap_in.length) yield{
                val seq = Seq( regBytes*(id + offset) -> Seq(mod.swap_in(i).toRegField(Some(mod.swap_in_desc(i)))) )
                id = id + 1
                seq
        }

        val regmap_out = Seq.tabulate(mod.swap_out.length) {i=> 
                                for (x <- 0 until mod.swap_out(i).length ) yield{
                                  val out = Seq(regBytes*(id + offset) -> Seq(mod.swap_out(i)(x).toRegField(Some( mod.swap_out_desc(i, x) )) ))
                                  id = id + 1
                                  out
                                }
                              }.flatten.flatten

        regmap_in.flatten ++ regmap_out           
    }
}

object mByteSwapper extends App{
    chisel3.Driver.execute(Array("--target-dir", "generated/ByteSwap"), () => new Mod_ByteSwapper(8,4))
}