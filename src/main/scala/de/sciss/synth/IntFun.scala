package de.sciss.synth

private[synth] object IntFun {
  @inline def ri_abs( a: Int ) : Int        = math.abs( a )
  @inline def ri_signum( a: Int ) : Int     = math.signum( a )
  @inline def ri_squared( a: Int ) : Long   = { val n = a.toLong; n * n }
  @inline def ri_cubed( a: Int ) : Long     = { val n = a.toLong; n * n * n }

  // ---- binary ops ----
  @inline def ri_min( a: Int, b: Int ) : Int   = math.min( a, b )
  @inline def ri_max( a: Int, b: Int ) : Int   = math.max( a, b )
}