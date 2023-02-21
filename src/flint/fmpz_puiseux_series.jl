###############################################################################
#
#   fmpz_puiseux_series.jl : Puiseux series over Flint ZZRingElem integers
#
###############################################################################

###############################################################################
#
#   Ad hoc binary operations
#
###############################################################################

function *(x::FlintPuiseuxSeriesElem{fmpz_laurent_series}, y::ZZRingElem)
   z = parent(x)(x.data*y, x.scale)
   z = rescale!(z)
   return z
end

*(x::ZZRingElem, y::FlintPuiseuxSeriesElem{fmpz_laurent_series}) = y*x

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::FlintPuiseuxSeriesElem{fmpz_laurent_series}, y::ZZRingElem; check::Bool=true)
   return parent(x)(divexact(x.data, y; check=check), x.scale)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::FlintPuiseuxSeriesElem{fmpz_laurent_series}, y::ZZRingElem) = x.data == y

==(x::ZZRingElem, y::FlintPuiseuxSeriesElem{fmpz_laurent_series}) = y == x

###############################################################################
#
#   Special functions
#
###############################################################################

@doc Markdown.doc"""
    eta_qexp(x::FlintPuiseuxSeriesElem{fmpz_laurent_series})

Return the $q$-series for eta evaluated at $x$, which must currently be a rational
power of the generator of the Puiseux series ring.
"""
function eta_qexp(x::FlintPuiseuxSeriesElem{fmpz_laurent_series})
   v = valuation(x)
   d = eta_qexp(x.data)
   z = parent(x)(d, x.scale)
   return z*x^(1//24)
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::FlintPuiseuxSeriesRing{fmpz_laurent_series})(b::ZZRingElem)
   z = FlintPuiseuxSeriesRingElem{fmpz_laurent_series}(laurent_ring(R)(b), 1)
   z.parent = R
   return z
end

###############################################################################
#
#   PuiseuxSeriesRing constructor
#
###############################################################################

@doc Markdown.doc"""
    PuiseuxSeriesRing(R::ZZRing, prec::Int, s::AbstractString; cached=true)

Return a tuple $(S, x)$ consisting of the parent object `S` of a Puiseux series
ring over the given base ring and a generator `x` for the Puiseux series ring.
The maximum precision of the series in the ring is set to `prec`. This is taken as a
maximum relative precision of the underlying Laurent series that are used to implement
the Puiseux series in the ring. The supplied string `s` specifies the way the
generator of the Puiseux series ring will be printed. By default, the parent
object `S` will be cached so that supplying the same base ring, string and
precision in future will return the same parent object and generator. If
caching of the parent object is not required, `cached` can be set to `false`.
"""
function PuiseuxSeriesRing(R::ZZRing, prec::Int, s::Symbol; cached=true)
   S, x = LaurentSeriesRing(R, prec, s; cached=cached)

   parent_obj = FlintPuiseuxSeriesRing{fmpz_laurent_series}(S, cached)

   return parent_obj, gen(parent_obj)
end

function PuiseuxSeriesRing(R::ZZRing, prec::Int, s::AbstractString; cached=true)
   return PuiseuxSeriesRing(R, prec, Symbol(s); cached=cached)
end

