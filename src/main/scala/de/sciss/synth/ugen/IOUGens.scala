/*
 * IOUGens.scala
 * (ScalaCollider-UGens)
 *
 * This is a synthetically generated file.
 * Created: Fri Jan 07 14:02:37 GMT 2011
 * ScalaCollider-UGen version: 0.10
 */

package de.sciss.synth
package ugen
import collection.immutable.{IndexedSeq => IIdxSeq}
import UGenHelper._
object OffsetOut {
   def ar(bus: AnyGE, in: Multi[GE[audio, UGenIn[audio]]]) = apply(bus, in)
}
case class OffsetOut(bus: AnyGE, in: Multi[AnyGE]) extends UGenSource[OffsetOutUGen] with AudioRated with WritesBus {
   protected def expandUGens = {
      val _bus: IIdxSeq[AnyUGenIn] = bus.expand
      val _in: IIdxSeq[AnyGE] = in.mexpand
      val _sz_bus = _bus.size
      val _sz_in = _in.size
      val _exp_ = maxInt(_sz_bus, _sz_in)
      IIdxSeq.tabulate(_exp_)(i => OffsetOutUGen(_bus(i.%(_sz_bus)), _in(i.%(_sz_in)).expand))
   }
}
case class OffsetOutUGen(bus: AnyUGenIn, in: IIdxSeq[AnyUGenIn]) extends ZeroOutUGen(IIdxSeq[AnyUGenIn](bus).++(in)) with AudioRated with WritesBus
object LocalIn {
   def kr: LocalIn[control] = kr()
   def kr(numChannels: Int = 1) = apply[control](control, numChannels)
   def ar: LocalIn[audio] = ar()
   def ar(numChannels: Int = 1) = apply[audio](audio, numChannels)
}
case class LocalIn[R <: Rate](rate: R, numChannels: Int) extends MultiOutUGen(IIdxSeq.fill(numChannels)(rate), IIdxSeq.empty)
object XOut {
   def ar(bus: AnyGE, in: Multi[GE[audio, UGenIn[audio]]], xfade: AnyGE) = apply[audio](audio, bus, in, xfade)
   def kr(bus: AnyGE, in: Multi[AnyGE], xfade: AnyGE) = apply[control](control, bus, in, xfade)
}
case class XOut[R <: Rate](rate: R, bus: AnyGE, in: Multi[AnyGE], xfade: AnyGE) extends UGenSource[XOutUGen[R]] with WritesBus {
   protected def expandUGens = {
      val _bus: IIdxSeq[AnyUGenIn] = bus.expand
      val _xfade: IIdxSeq[AnyUGenIn] = xfade.expand
      val _in: IIdxSeq[AnyGE] = in.mexpand
      val _sz_bus = _bus.size
      val _sz_xfade = _xfade.size
      val _sz_in = _in.size
      val _exp_ = maxInt(_sz_bus, _sz_xfade, _sz_in)
      IIdxSeq.tabulate(_exp_)(i => XOutUGen(rate, _bus(i.%(_sz_bus)), _in(i.%(_sz_in)).expand, _xfade(i.%(_sz_xfade))))
   }
}
case class XOutUGen[R <: Rate](rate: R, bus: AnyUGenIn, in: IIdxSeq[AnyUGenIn], xfade: AnyUGenIn) extends ZeroOutUGen(IIdxSeq[AnyUGenIn](bus, xfade).++(in)) with WritesBus
object ReplaceOut {
   def ar(bus: AnyGE, in: Multi[GE[audio, UGenIn[audio]]]) = apply(bus, in)
}
case class ReplaceOut(bus: AnyGE, in: Multi[AnyGE]) extends UGenSource[ReplaceOutUGen] with AudioRated with WritesBus {
   protected def expandUGens = {
      val _bus: IIdxSeq[AnyUGenIn] = bus.expand
      val _in: IIdxSeq[AnyGE] = in.mexpand
      val _sz_bus = _bus.size
      val _sz_in = _in.size
      val _exp_ = maxInt(_sz_bus, _sz_in)
      IIdxSeq.tabulate(_exp_)(i => ReplaceOutUGen(_bus(i.%(_sz_bus)), _in(i.%(_sz_in)).expand))
   }
}
case class ReplaceOutUGen(bus: AnyUGenIn, in: IIdxSeq[AnyUGenIn]) extends ZeroOutUGen(IIdxSeq[AnyUGenIn](bus).++(in)) with AudioRated with WritesBus
object Out {
   def ar(bus: AnyGE, in: Multi[GE[audio, UGenIn[audio]]]) = apply[audio](audio, bus, in)
   def kr(bus: AnyGE, in: Multi[AnyGE]) = apply[control](control, bus, in)
   def ir(bus: AnyGE, in: Multi[AnyGE]) = apply[scalar](scalar, bus, in)
}
case class Out[R <: Rate](rate: R, bus: AnyGE, in: Multi[AnyGE]) extends UGenSource[OutUGen[R]] with WritesBus {
   protected def expandUGens = {
      val _bus: IIdxSeq[AnyUGenIn] = bus.expand
      val _in: IIdxSeq[AnyGE] = in.mexpand
      val _sz_bus = _bus.size
      val _sz_in = _in.size
      val _exp_ = maxInt(_sz_bus, _sz_in)
      IIdxSeq.tabulate(_exp_)(i => OutUGen(rate, _bus(i.%(_sz_bus)), _in(i.%(_sz_in)).expand))
   }
}
case class OutUGen[R <: Rate](rate: R, bus: AnyUGenIn, in: IIdxSeq[AnyUGenIn]) extends ZeroOutUGen(IIdxSeq[AnyUGenIn](bus).++(in)) with WritesBus
object LocalOut {
   def kr(in: Multi[AnyGE]) = apply[control](control, in)
   def ar(in: Multi[GE[audio, UGenIn[audio]]]) = apply[audio](audio, in)
}
case class LocalOut[R <: Rate](rate: R, in: Multi[AnyGE]) extends UGenSource[LocalOutUGen[R]] {
   protected def expandUGens = {
      val _in: IIdxSeq[AnyGE] = in.mexpand
      IIdxSeq.tabulate(_in.size)(i => LocalOutUGen(rate, _in(i).expand))
   }
}
case class LocalOutUGen[R <: Rate](rate: R, in: IIdxSeq[AnyUGenIn]) extends ZeroOutUGen(in)
object In {
   def ir(bus: AnyGE, numChannels: Int = 1) = apply[scalar](scalar, bus, numChannels)
   def kr(bus: AnyGE, numChannels: Int = 1) = apply[control](control, bus, numChannels)
   def ar(bus: AnyGE, numChannels: Int = 1) = apply[audio](audio, bus, numChannels)
}
case class In[R <: Rate](rate: R, bus: AnyGE, numChannels: Int) extends UGenSource[InUGen[R]] {
   protected def expandUGens = {
      val _bus: IIdxSeq[AnyUGenIn] = bus.expand
      IIdxSeq.tabulate(_bus.size)(i => InUGen(rate, _bus(i), numChannels))
   }
}
case class InUGen[R <: Rate](rate: R, bus: AnyUGenIn, numChannels: Int) extends MultiOutUGen(IIdxSeq.fill(numChannels)(rate), IIdxSeq(bus))
object LagIn {
   def kr(bus: AnyGE, numChannels: Int = 1, lag: AnyGE = 0.1f) = apply[control](control, bus, numChannels, lag)
}
case class LagIn[R <: Rate](rate: R, bus: AnyGE, numChannels: Int, lag: AnyGE) extends UGenSource[LagInUGen[R]] {
   protected def expandUGens = {
      val _bus: IIdxSeq[AnyUGenIn] = bus.expand
      val _lag: IIdxSeq[AnyUGenIn] = lag.expand
      val _sz_bus = _bus.size
      val _sz_lag = _lag.size
      val _exp_ = maxInt(_sz_bus, _sz_lag)
      IIdxSeq.tabulate(_exp_)(i => LagInUGen(rate, _bus(i.%(_sz_bus)), numChannels, _lag(i.%(_sz_lag))))
   }
}
case class LagInUGen[R <: Rate](rate: R, bus: AnyUGenIn, numChannels: Int, lag: AnyUGenIn) extends MultiOutUGen(IIdxSeq.fill(numChannels)(rate), IIdxSeq(bus, lag))
object InFeedback {
   def ar(bus: AnyGE, numChannels: Int = 1) = apply(bus, numChannels)
}
case class InFeedback(bus: AnyGE, numChannels: Int) extends UGenSource[InFeedbackUGen] with AudioRated {
   protected def expandUGens = {
      val _bus: IIdxSeq[AnyUGenIn] = bus.expand
      IIdxSeq.tabulate(_bus.size)(i => InFeedbackUGen(_bus(i), numChannels))
   }
}
case class InFeedbackUGen(bus: AnyUGenIn, numChannels: Int) extends MultiOutUGen(IIdxSeq.fill(numChannels)(audio), IIdxSeq(bus)) with AudioRated
object InTrig {
   def kr(bus: AnyGE, numChannels: Int = 1) = apply(bus, numChannels)
}
case class InTrig(bus: AnyGE, numChannels: Int) extends UGenSource[InTrigUGen] with ControlRated {
   protected def expandUGens = {
      val _bus: IIdxSeq[AnyUGenIn] = bus.expand
      IIdxSeq.tabulate(_bus.size)(i => InTrigUGen(_bus(i), numChannels))
   }
}
case class InTrigUGen(bus: AnyUGenIn, numChannels: Int) extends MultiOutUGen(IIdxSeq.fill(numChannels)(control), IIdxSeq(bus)) with ControlRated