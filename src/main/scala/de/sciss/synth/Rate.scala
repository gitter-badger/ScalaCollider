/*
 *  Rate.scala
 *  (ScalaCollider)
 *
 *  Copyright (c) 2008-2011 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.synth

/**
 *    @version 0.12, 28-Dec-10
 */
object Rate {
   def highest( rates: Rate* ) : Rate = rates.foldLeft[ Rate ]( scalar )( (a, b) => if( a.id > b.id ) a else b )
   def highest( ge: AnyGE ) : Rate = highest( ge.expand.map( _.rate ): _* )
   def lowest( rates: Rate* ) : Rate = rates.foldLeft[ Rate ]( scalar )( (a, b) => if( a.id < b.id ) a else b )
   def lowest( ge: AnyGE ) : Rate = lowest( ge.expand.map( _.rate ): _* )

//   def apply[ R <: Rate ] : R = R match {
//      case scalar => scalar
//      case scalar => scalar
//   }
}

/**
 *    The calculation rate of a UGen or a UGen output.
 */
sealed abstract class Rate {
   val id: Int
   val methodName: String
}

sealed trait scalar  extends Rate { final val id = 0; final val methodName = "ir" }
sealed trait control extends Rate { final val id = 1; final val methodName = "kr" }
sealed trait audio   extends Rate { final val id = 2; final val methodName = "ar" }
sealed trait demand  extends Rate { final val id = 3; final val methodName = "dr" }
case object scalar  extends scalar {
//   implicit val rate = scalar
}
case object control extends control {
//   implicit val rate = control
}
case object audio extends audio {
//   implicit val rate = audio
}
case object demand extends demand {
//   implicit val rate = demand
}

//case object scalar extends Rate {
//   val id = 0
//   val methodName = "ir"
//}
//case object control extends Rate {
//   val id = 1
//   val methodName = "kr"
//}
//case object audio   extends Rate { final val id = 2; final val methodName = "ar" }
//case object demand  extends Rate { final val id = 3; final val methodName = "dr" }

//trait RatedGE[ +R <: Rate, +U <: UGenIn[ R ]] extends GE[ U ] {
//   def rate : Rate // R
//   def madd( mul: RatedGE[ Rate, AnyUGenIn ], add: RatedGE[ Rate, AnyUGenIn ]) : RatedGE[ R, UGenIn[ R ]] = {
//      (rate match {
//         case `audio`   => MulAdd.ar( this, mul, add )
//         case `control` => MulAdd.kr( this, mul, add )
//         case `scalar`  => this * mul + add
//         case r         => error( "Illegal rate " + r )
//      }).asInstanceOf[ RatedGE[ R, UGenIn[ R ]]]
//   }
//}

trait ScalarRated  { def rate: scalar = scalar }
trait ControlRated { def rate: control = control }
trait AudioRated   { def rate: audio = audio }
trait DemandRated  { def rate: demand = demand }

object HigherRate {
//   implicit val demandGtAudio:    HigherRate[ demand,  audio   ] = new Impl[ demand, audio ] // (   demand,  audio   )
//   implicit val demandGtControl:  HigherRate[ demand,  control ] = new Impl[ demand, control ] // ( demand,  control )
//   implicit val demandGtScalar:   HigherRate[ demand,  scalar  ] = new Impl[ demand, scalar ] // (  demand,  scalar  )
   implicit def allGtScalar[ R <: Rate ] : HigherRate[ R, scalar ]   = new Impl[ R, scalar ]
   implicit def demandGtAll[ R <: Rate ] : HigherRate[ demand, R ]   = new Impl[ demand, R ]
   implicit val audioGtControl: HigherRate[ audio, control ]         = new Impl[ audio, control ] // (  audio,   control )
//   implicit val audioGtScalar:    HigherRate[ audio,   scalar  ] = new Impl[ audio, scalar ] // (   audio,   scalar  )
//   implicit val controlGtScalar:  HigherRate[ control, scalar  ] = new Impl[ control, scalar ] // ( control, scalar  )

   private class Impl[ R <: Rate, S <: Rate ] /* ( val rate1: R, val rate2: S ) */ extends HigherRate[ R, S ]
}
sealed trait HigherRate[ R <: Rate, S <: Rate ] {
//   def rate1: R
//   def rate2: S
}

sealed trait MaybeRateOrderLowImplicits {
   implicit def unknown[ R <: Rate, S <: Rate ] = RateOrderUnknown[ R, S ]()
}
object MaybeRateOrder extends MaybeRateOrderLowImplicits {
   implicit def same[ R <: Rate ] /* ( implicit rate: R ) */ : RateOrder[ R, R, R ] = new Impl[ R, R, R ] // ( /* rate, rate, */ rate )
   implicit def greater[ R <: Rate, S <: Rate ]( implicit rel: HigherRate[ R, S ]) : RateOrder[ R, S, R ] = new Impl[ R, S, R ] // ( /* rel.rate1, rel.rate2, */ rel.rate1 )
   implicit def less[ R <: Rate, S <: Rate ]( implicit rel: HigherRate[ S, R ]) : RateOrder[ R, S, S ] = new Impl[ R, S, S ] // ( /* rel.rate2, rel.rate1, */ rel.rate1 )

   private class Impl[ R <: Rate, S <: Rate, T <: Rate ]/*( /* val in1: R, val in2: S, */ val out: T )*/ extends RateOrder[ R, S, T ]
}
sealed trait MaybeRateOrder[ R <: Rate, S <: Rate, T <: Rate ] {
//   def getOrElse( r: => R, s: => S ) : T
}
case class RateOrderUnknown[ R <: Rate, S <: Rate ]() extends MaybeRateOrder[ R, S, Rate ] {
//   def getOrElse( r: => R, s: => S ) = Rate.highest( r, s )
}

sealed trait RateOrder[ R <: Rate, S <: Rate, T <: Rate ] extends MaybeRateOrder[ R, S, T ] {
//   def in1: R
//   def in2: S
//   def out: T

//   def getOrElse( r: => R, s: => S ) : T = out
}
