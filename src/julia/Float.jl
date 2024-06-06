function setprecision!(x::BigFloat, p::Int)
  ccall((:mpfr_prec_round, :libmpfr), Nothing, (Ref{BigFloat}, Clong, Int32), x, p, Base.MPFR.ROUNDING_MODE[])
end

