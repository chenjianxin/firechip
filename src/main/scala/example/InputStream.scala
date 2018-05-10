package example

import chisel3._
import chisel3.core.IntParam
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.Parameters

class InputStream(
    address: BigInt,
    val beatBytes: Int = 8,
    val maxInflight: Int = 4)
    (implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("input-stream", Seq("example,input-stream"))
  val regnode = TLRegisterNode(
    address = Seq(AddressSet(address, 0x3f)),
    device = device,
    beatBytes = beatBytes)
  val intnode = IntSourceNode(IntSourcePortSimple(resources = device.int))
  val dmanode = TLClientNode(Seq(TLClientPortParameters(
    Seq(TLClientParameters(
      name = "input-stream",
      sourceId = IdRange(0, maxInflight))))))

  lazy val module = new InputStreamModuleImp(this)
}

class InputStreamModuleImp(outer: InputStream) extends LazyModuleImp(outer) {
  val (interrupt, _) = outer.intnode.out(0)
  val (tl, edge) = outer.dmanode.out(0)
  val addrBits = edge.bundle.addressBits
  val w = edge.bundle.dataBits
  val beatBytes = (w / 8)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(w.W)))
  })


  val addr = Reg(UInt(addrBits.W))
  val len = Reg(UInt(addrBits.W))
  val running = RegInit(false.B)
  val complete = RegInit(false.B)

  val s_idle :: s_issue :: s_wait :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val nXacts = outer.maxInflight
  val xactBusy = RegInit(0.U(nXacts.W))
  val xactOnehot = PriorityEncoderOH(~xactBusy)
  val canIssue = (state === s_issue) && !xactBusy.andR

  io.in.ready := canIssue && tl.a.ready
  tl.a.valid  := canIssue && io.in.valid
  tl.a.bits   := edge.Put(
    fromSource = OHToUInt(xactOnehot),
    toAddress = addr,
    lgSize = log2Ceil(beatBytes).U,
    data = io.in.bits)._2
  tl.d.ready := running && xactBusy.orR
  interrupt(0) := complete

  xactBusy := (xactBusy |
    Mux(tl.a.fire(), xactOnehot, 0.U(nXacts.W))) &
    ~Mux(tl.d.fire(), UIntToOH(tl.d.bits.source), 0.U)

  when (state === s_idle && running) {
    assert(addr(log2Ceil(beatBytes)-1,0) === 0.U,
      s"InputStream base address not aligned to ${beatBytes} bytes")
    assert(len(log2Ceil(beatBytes)-1,0) === 0.U,
      s"InputStream length not aligned to ${beatBytes} bytes")
    state := s_issue
  }

  when (io.in.fire()) {
    addr := addr + beatBytes.U
    len := len - beatBytes.U
    when (len === beatBytes.U) { state := s_wait }
  }

  when (state === s_wait && !xactBusy.orR) {
    running := false.B
    complete := true.B
    state := s_idle
  }

  outer.regnode.regmap(
    0x00 -> Seq(RegField(addrBits, addr)),
    0x08 -> Seq(RegField(addrBits, len)),
    0x10 -> Seq(RegField(1, running)),
    0x18 -> Seq(RegField(1, complete)))
}

trait HasPeripheryInputStream { this: BaseSubsystem =>
  private val portName = "input-stream"
  val streamWidth = pbus.beatBytes * 8
  val inputstream = LazyModule(new InputStream(0x10017000, pbus.beatBytes))
  pbus.toVariableWidthSlave(Some(portName)) { inputstream.regnode }
  sbus.fromPort(Some(portName))() := inputstream.dmanode
  ibus.fromSync := inputstream.intnode
}

trait HasPeripheryInputStreamModuleImp extends LazyModuleImp {
  val outer: HasPeripheryInputStream

  val stream_in = IO(Flipped(Decoupled(UInt(outer.streamWidth.W))))
  outer.inputstream.module.io.in <> stream_in

  def connectFixedInput(data: Seq[BigInt]) {
    val fixed = Module(new FixedInputStream(data, outer.streamWidth))
    stream_in <> fixed.io.out
  }

  def connectSimInput(clock: Clock, reset: Bool) {
    val sim = Module(new SimInputStream(outer.streamWidth))
    sim.io.clock := clock
    sim.io.reset := reset
    stream_in <> sim.io.out
  }
}

class FixedInputStream(data: Seq[BigInt], w: Int) extends Module {
  val io = IO(new Bundle {
    val out = Decoupled(UInt(w.W))
  })

  val dataROM = VecInit(data.map(_.U(w.W)))
  val idx = RegInit(0.U(log2Ceil(data.size).W))

  val s_start :: s_feed :: s_done :: Nil = Enum(3)
  val state = RegInit(s_start)

  when (state === s_start) { state := s_feed }
  when (io.out.fire()) {
    idx := idx + 1.U
    when (idx === (data.size-1).U) { state := s_done }
  }

  io.out.valid := state === s_feed
  io.out.bits  := dataROM(idx)
}

class SimInputStream(w: Int) extends BlackBox(Map("DATA_BITS" -> IntParam(w))) {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val out = Decoupled(UInt(w.W))
  })
}
