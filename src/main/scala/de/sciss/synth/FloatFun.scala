package de.sciss.synth

private[synth] object FloatFun {
   import DoubleFun.LOG2

   // -------- unary ops --------

   @inline def rf_not( f: Float ) : Float          = if( f > 0f ) 0f else 1f
   @inline def rf_neg( f: Float ) : Float          = -f
   @inline def rf_abs( f: Float ) : Float          = math.abs( f )
   @inline def rf_ceil( f: Float ) : Float         = math.ceil( f ).toFloat
   @inline def rf_floor( f: Float ) : Float        = math.floor( f ).toFloat
   @inline def rf_frac( f: Float ) : Float         = (f - math.floor( f )).toFloat // according to jmc
   @inline def rf_signum( f: Float ) : Float       = math.signum( f )
   @inline def rf_squared( f: Float ) : Float      = f * f
   @inline def rf_cubed( f: Float ) : Float        = f * f * f
   @inline def rf_sqrt( f: Float ) : Float         = math.sqrt( f ).toFloat
   @inline def rf_exp( f: Float ) : Float          = math.exp( f ).toFloat
   @inline def rf_reciprocal( f: Float ) : Float   = 1.0f / f
   @inline def rf_midicps( f: Float ) : Float      = (440 * math.pow( 2, (f - 69) / 12 )).toFloat
   @inline def rf_cpsmidi( f: Float ) : Float      = (math.log( f / 440 ) / LOG2 * 12 + 69).toFloat
   @inline def rf_midiratio( f: Float ) : Float    = (math.pow( 2, f / 12 )).toFloat
   @inline def rf_ratiomidi( f: Float ) : Float    = (12 * math.log( f ) / LOG2).toFloat
   @inline def rf_dbamp( f: Float ) : Float        = (math.pow( 10, f * 0.05 )).toFloat
   @inline def rf_ampdb( f: Float ) : Float        = (math.log10( f )* 20).toFloat
   @inline def rf_octcps( f: Float ) : Float       = (440 * math.pow( 2, f - 4.75 )).toFloat
   @inline def rf_cpsoct( f: Float ) : Float       = (math.log( f / 440 ) / LOG2 + 4.75).toFloat
   @inline def rf_log( f: Float ) : Float          = math.log( f ).toFloat
   @inline def rf_log2( f: Float ) : Float         = (math.log( f ) / LOG2).toFloat
   @inline def rf_log10( f: Float ) : Float        = math.log10( f ).toFloat
   @inline def rf_sin( f: Float ) : Float          = math.sin( f ).toFloat
   @inline def rf_cos( f: Float ) : Float          = math.cos( f ).toFloat
   @inline def rf_tan( f: Float ) : Float          = math.tan( f ).toFloat
   @inline def rf_asin( f: Float ) : Float         = math.asin( f ).toFloat
   @inline def rf_acos( f: Float ) : Float         = math.acos( f ).toFloat
   @inline def rf_atan( f: Float ) : Float         = math.atan( f ).toFloat
   @inline def rf_sinh( f: Float ) : Float         = math.sinh( f ).toFloat
   @inline def rf_cosh( f: Float ) : Float         = math.cosh( f ).toFloat
   @inline def rf_tanh( f: Float ) : Float         = math.tanh( f ).toFloat

   @inline def rf_distort( f: Float ) : Float      = f / (1 + math.abs( f ))
   @inline def rf_softclip( f: Float ) : Float = {
      val absx = math.abs( f )
      if( absx <= 0.5f ) f else (absx - 0.25f) / f
   }
   @inline def rf_ramp( f: Float ) : Float         = if( f <= 0 ) 0 else if( f >= 1 ) 1 else f
   @inline def rf_scurve( f: Float ) : Float       = if( f <= 0 ) 0 else if( f > 1 ) 1 else f * f * (3 - 2 * f)

   // -------- binary ops --------

   @inline def rf_+( a: Float, b: Float ) : Float     = a + b
   @inline def rf_-( a: Float, b: Float ) : Float     = a - b
   @inline def rf_*( a: Float, b: Float ) : Float     = a * b
   @inline def rf_div( a: Float, b: Float ) : Int     = (a / b).toInt
   @inline def rf_/( a: Float, b: Float ) : Float     = a / b
   @inline def rf_%( a: Float, b: Float ) : Float     = a % b
   @inline def rf_===( a: Float, b: Float ) : Int     = if( a == b ) 1 else 0
   @inline def rf_!==( a: Float, b: Float ) : Int     = if( a != b ) 1 else 0
   @inline def rf_<( a: Float, b: Float ) : Boolean   = a < b
   @inline def rf_>( a: Float, b: Float ) : Boolean   = a > b
   @inline def rf_<=( a: Float, b: Float ) : Boolean  = a <= b
   @inline def rf_>=( a: Float, b: Float ) : Boolean  = a >= b
   @inline def rf_min( a: Float, b: Float ) : Float   = math.min( a, b )
   @inline def rf_max( a: Float, b: Float ) : Float   = math.max( a, b )
   @inline def rf_&( a: Float, b: Float ) : Float	   = a.toInt & b.toInt
   @inline def rf_|( a: Float, b: Float ) : Float	   = a.toInt | b.toInt
   @inline def rf_^( a: Float, b: Float ) : Float	   = a.toInt ^ b.toInt

   // lcm
   // gcd

   @inline def rf_round( a: Float, b: Float ) =
      if( b == 0 ) a else (math.floor( a / b + 0.5f ) * b).toFloat

   @inline def rf_roundup( a: Float, b: Float ) =
      if( b == 0 ) a else (math.ceil( a / b ) * b).toFloat

   @inline def rf_trunc( a: Float, b: Float ) =
      if( b == 0 ) a else (math.floor( a / b ) * b).toFloat

   @inline def rf_atan2( a: Float, b: Float ) : Float = math.atan2( a, b ).toFloat
   @inline def rf_hypot( a: Float, b: Float ) : Float = math.hypot( a, b ).toFloat

   @inline def rf_hypotx( a: Float, b: Float ) = {
      val minab = math.min( math.abs( a ), math.abs( b ))
      (a + b - (math.sqrt(2) - 1) * minab).toFloat
   }

   @inline def rf_pow( a: Float, b: Float ) : Float = math.pow( a, b ).toFloat

// <<
// >>
// UnsgnRghtShft
// fill

   @inline def rf_ring1( a: Float, b: Float ) =
      a * b + a

   @inline def rf_ring2( a: Float, b: Float ) =
      a * b + a + b

   @inline def rf_ring3( a: Float, b: Float ) =
      a * a * b

   @inline def rf_ring4( a: Float, b: Float ) = {
      val ab = a * b; a * ab - b * ab
   }

   @inline def rf_difsqr( a: Float, b: Float ) =
      a * a - b * b

   @inline def rf_sumsqr( a: Float, b: Float ) =
      a * a + b * b

   @inline def rf_sqrsum( a: Float, b: Float ) = {
      val z = a + b; z * z
   }

   @inline def rf_sqrdif( a: Float, b: Float ) = {
      val z = a - b; z * z
   }

   @inline def rf_absdif( a: Float, b: Float ) = math.abs( a - b )

   @inline def rf_thresh( a: Float, b: Float ) =
      if( a < b ) 0 else a

   @inline def rf_amclip( a: Float, b: Float ) =
      a * 0.5f * (b + math.abs( a ))

   @inline def rf_scaleneg( a: Float, b: Float ) =
      (math.abs( a ) - a) * (0.5f * b + 0.5f) + a

   @inline def rf_clip2( a: Float, b: Float ) =
      math.max( math.min( a, b ), -b )

   @inline def rf_excess( a: Float, b: Float ) =
      a - math.max( math.min( a, b ), -b )

   @inline def rf_fold2( a: Float, b: Float ) = rf_fold( a, -b, b )

   @inline def rf_wrap2( a: Float, b: Float ) = rf_wrap( a, -b, b )

   // -------- n-ary ops --------

   @inline def rf_fold( in: Float, lo: Float, hi: Float ) : Float = {
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
      val c       = x - range2 * math.floor( x / range2 ).toFloat
      lo + (if( c >= range ) range2 - c else c)
   }

   @inline def rf_wrap( in: Float, lo: Float, hi: Float ) : Float = {
      // avoid the divide if possible
      if( in >= hi ) {
         val range   = hi - lo
         val in2     = in - range
         if( in2 < hi ) in2 else if( hi == lo ) lo else {
            in2 - range * math.floor( (in2 - lo) / range ).toFloat
         }
      } else if( in < lo ) {
         val range   = hi - lo
         val in2     = in + range
         if( in2 >= lo ) in2 else if( hi == lo ) lo else {
            in2 - range * math.floor( (in2 - lo) / range ).toFloat
         }
      } else in
   }

   @inline def rf_linlin( in: Float, srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float ) : Float = {
      (in - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo
   }

   @inline def rf_linexp( in: Float, srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float ) : Float = {
      math.pow( dstHi / dstLo, (in - srcLo) / (srcHi - srcLo)).toFloat * dstLo
   }
}