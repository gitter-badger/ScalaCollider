package de.sciss.synth

private[synth] object DoubleFun {
  final val LOG2 = math.log(2)

   // -------- unary ops --------

   @inline def rd_neg( d: Double ) : Double	      = -d
   @inline def rd_abs( d: Double ) : Double	      = math.abs( d )
   @inline def rd_ceil( d: Double ) : Double       = math.ceil( d )
   @inline def rd_floor( d: Double ) : Double	   = math.floor( d )
   @inline def rd_frac( d: Double ) : Double	      = (d - math.floor( d )) // according to jmc
   @inline def rd_signum( d: Double ) : Double     = math.signum( d )
   @inline def rd_squared( d: Double ) : Double    = d * d
   @inline def rd_cubed( d: Double ) : Double      = d * d * d
   @inline def rd_sqrt( d: Double ) : Double       = math.sqrt( d )
   @inline def rd_exp( d: Double ) : Double        = math.exp( d )
   @inline def rd_reciprocal( d: Double ) : Double = 1.0 / d
   @inline def rd_midicps( d: Double ) : Double    = (440 * math.pow( 2, (d - 69) / 12 ))
   @inline def rd_cpsmidi( d: Double ) : Double    = (math.log( d / 440 ) / LOG2 * 12 + 69)
   @inline def rd_midiratio( d: Double ) : Double  = (math.pow( 2, d / 12 ))
   @inline def rd_ratiomidi( d: Double ) : Double  = (12 * math.log( d ) / LOG2)
   @inline def rd_dbamp( d: Double ) : Double      = (math.pow( 10, d * 0.05 ))
   @inline def rd_ampdb( d: Double ) : Double      = (math.log10( d ) * 20)
   @inline def rd_octcps( d: Double ) : Double     = (440 * math.pow( 2, d - 4.75 ))
   @inline def rd_cpsoct( d: Double ) : Double     = (math.log( d / 440 ) / LOG2 + 4.75)
   @inline def rd_log( d: Double ) : Double        = math.log( d )
   @inline def rd_log2( d: Double ) : Double       = (math.log( d ) / LOG2)
   @inline def rd_log10( d: Double ) : Double      = math.log10( d )
   @inline def rd_sin( d: Double ) : Double        = math.sin( d )
   @inline def rd_cos( d: Double ) : Double        = math.cos( d )
   @inline def rd_tan( d: Double ) : Double        = math.tan( d )
   @inline def rd_asin( d: Double ) : Double       = math.asin( d )
   @inline def rd_acos( d: Double ) : Double       = math.acos( d )
   @inline def rd_atan( d: Double ) : Double       = math.atan( d )
   @inline def rd_sinh( d: Double ) : Double       = math.sinh( d )
   @inline def rd_cosh( d: Double ) : Double       = math.cosh( d )
   @inline def rd_tanh( d: Double ) : Double       = math.tanh( d )
//   @inline def rd_distort( d: Double ) : Double    = d / (1 + math.abs( d ))
//   @inline def rd_softclip( d: Double ) : Double   = { val absx = math.abs( d ); if( absx <= 0.5 ) d else (absx - 0.25) / d}
//   @inline def rd_ramp( d: Double ) : Double       = if( d <= 0 ) 0 else if( d >= 1 ) 1 else d
//   @inline def rd_scurve( d: Double ) : Double     = if( d <= 0 ) 0 else if( d > 1 ) 1 else d * d * (3 - 2 * d)

   // ---- binary ops ----

   @inline def rd_+( a: Double, b: Double ) : Double     = a + b
   @inline def rd_-( a: Double, b: Double ) : Double     = a - b
   @inline def rd_*( a: Double, b: Double ) : Double     = a * b
   @inline def rd_div( a: Double, b: Double ) : Int      = (a / b).toInt
   @inline def rd_/( a: Double, b: Double ) : Double     = a / b
   @inline def rd_%( a: Double, b: Double ) : Double     = a % b
//   @inline def rd_===
//   @inline def rd_!==
   @inline def rd_<( a: Double, b: Double ) : Boolean    = a < b
   @inline def rd_>( a: Double, b: Double ) : Boolean    = a > b
   @inline def rd_<=( a: Double, b: Double ) : Boolean   = a <= b
   @inline def rd_>=( a: Double, b: Double ) : Boolean   = a >= b
   @inline def rd_min( a: Double, b: Double ) : Double   = math.min( a, b )
   @inline def rd_max( a: Double, b: Double ) : Double   = math.max( a, b )

   @inline def rd_round( a: Double, b: Double ) =
      if( b == 0 ) a else math.floor( a / b + 0.5f ) * b

   @inline def rd_roundup( a: Double, b: Double ) =
      if( b == 0 ) a else math.ceil( a / b ) * b

   @inline def rd_trunc( a: Double, b: Double ) =
      if( b == 0 ) a else math.floor( a / b ) * b

   @inline def rd_atan2( a: Double, b: Double ) : Double = math.atan2( a, b )
   @inline def rd_hypot( a: Double, b: Double ) : Double = math.hypot( a, b )

   @inline def rd_hypotx( a: Double, b: Double ) = {
      val minab = math.min( math.abs( a ), math.abs( b ))
      a + b - (math.sqrt(2) - 1) * minab
   }

   @inline def rd_pow( a: Double, b: Double ) : Double = math.pow( a, b )

// <<
// >>
// UnsgnRghtShft
// fill

   @inline def rd_ring1( a: Double, b: Double ) =
      a * b + a

   @inline def rd_ring2( a: Double, b: Double ) =
      a * b + a + b

   @inline def rd_ring3( a: Double, b: Double ) =
      a * a * b

   @inline def rd_ring4( a: Double, b: Double ) = {
      val ab = a * b; a * ab - b * ab
   }

   @inline def rd_difsqr( a: Double, b: Double ) =
      a * a - b * b

   @inline def rd_sumsqr( a: Double, b: Double ) =
      a * a + b * b

   @inline def rd_sqrsum( a: Double, b: Double ) = {
      val z = a + b; z * z
   }

   @inline def rd_sqrdif( a: Double, b: Double ) = {
      val z = a - b; z * z
   }

   @inline def rd_absdif( a: Double, b: Double ) = math.abs( a - b )

   @inline def rd_thresh( a: Double, b: Double ) =
      if( a < b ) 0 else a

   @inline def rd_amclip( a: Double, b: Double ) =
      a * 0.5 * (b + math.abs( a ))

   @inline def rd_scaleneg( a: Double, b: Double ) =
      (math.abs( a ) - a) * (0.5 * b + 0.5) + a

   @inline def rd_clip2( a: Double, b: Double ) =
      math.max( math.min( a, b ), -b )

   @inline def rd_excess( a: Double, b: Double ) =
      a - math.max( math.min( a, b ), -b )

   @inline def rd_fold2( a: Double, b: Double ) = rd_fold( a, -b, b )

   @inline def rd_wrap2( a: Double, b: Double ) = rd_wrap( a, -b, b )

   // ---- n-ary ops ----

   @inline def rd_fold( in: Double, lo: Double, hi: Double ) : Double = {
      val x = in - lo
      // avoid the divide if possible
      if( in >= hi ) {
         val f = hi + hi - in
         if (f >= lo) return f
      } else if( in < lo ) {
         val f = lo + lo - in
         if( f < hi ) return f
      } else return in

      if( hi == lo ) return lo
      // ok do the divide
      val range   = hi - lo
      val range2  = range + range
      val c       = x - range2 * math.floor( x / range2 )
      lo + (if( c >= range ) range2 - c else c)
   }

   @inline def rd_wrap( in: Double, lo: Double, hi: Double ) : Double = {
      // avoid the divide if possible
      if( in >= hi ) {
         val range   = hi - lo
         val in2     = in - range
         if( in2 < hi ) in2 else if( hi == lo ) lo else {
            in2 - range * math.floor( (in2 - lo) / range )
         }
      } else if( in < lo ) {
         val range   = hi - lo
         val in2     = in + range
         if( in2 >= lo ) in2 else if( hi == lo ) lo else {
            in2 - range * math.floor( (in2 - lo) / range )
         }
      } else in
   }

   @inline def rd_linlin( in: Double, srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double = {
      (in - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo
   }

   @inline def rd_linexp( in: Double, srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double = {
      math.pow( dstHi / dstLo, (in - srcLo) / (srcHi - srcLo)) * dstLo
   }
}