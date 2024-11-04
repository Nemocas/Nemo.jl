function setprecision!(x::BigFloat, p::Int)
  @ccall :libmpfr.mpfr_prec_round(x::Ref{BigFloat}, p::Clong, Base.MPFR.ROUNDING_MODE[]::Int32)::Nothing
end

