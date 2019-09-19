export Fac, unit

################################################################################
#
#   Special functions for Fac{fmpz}
#
################################################################################

Base.in(b::Integer, a::Fac{fmpz}) = Base.in(fmpz(b), a)

Base.getindex(a::Fac{fmpz}, b::Integer) = getindex(a, fmpz(b))
