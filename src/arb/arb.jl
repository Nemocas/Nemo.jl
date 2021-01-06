###############################################################################
#
#   arb.jl : Arb real numbers
#
#   Copyright (C) 2015 Tommy Hofmann
#   Copyright (C) 2015 Fredrik Johansson
#
###############################################################################

import Base: ceil

export ball, radius, midpoint, contains, contains_zero,
       contains_negative, contains_positive, contains_nonnegative,
       contains_nonpositive, convert, iszero,
       isnonzero, isexact, isint, ispositive, isfinite,
       isnonnegative, isnegative, isnonpositive, add!, mul!,
       sub!, div!, prec, overlaps, unique_integer,
       accuracy_bits, trim, ldexp, setunion, setintersection,
       const_pi, const_e, const_log2, const_log10, const_euler,
       const_catalan, const_khinchin, const_glaisher,
       floor, ceil, hypot, rsqrt, sqrt1pm1, sqrtpos, root,
       log, log1p, expm1, sin, cos, sinpi, cospi, tan, cot,
       tanpi, cotpi, sinh, cosh, tanh, coth, atan, asin, acos,
       atanh, asinh, acosh, gamma, lgamma, rgamma, digamma, zeta,
       sincos, sincospi, sinhcosh, atan2,
       agm, fac, binom, fib, bernoulli, risingfac, risingfac2, polylog,
       chebyshev_t, chebyshev_t2, chebyshev_u, chebyshev_u2, bell, numpart,
       lindep, canonical_unit, simplest_rational_inside

###############################################################################
#
#   Basic manipulation
#
###############################################################################

elem_type(::Type{ArbField}) = arb

parent_type(::Type{arb}) = ArbField

@doc Markdown.doc"""
    base_ring(R::ArbField)

Returns `Union{}` since an Arb field does not depend on any other ring.
"""
base_ring(R::ArbField) = Union{}

@doc Markdown.doc"""
    base_ring(x::arb)

Returns `Union{}` since an Arb field does not depend on any other ring.
"""
base_ring(x::arb) = Union{}

@doc Markdown.doc"""
    parent(x::arb)

Return the parent of the given Arb field element.
"""
parent(x::arb) = x.parent

isdomain_type(::Type{arb}) = true

isexact_type(::Type{arb}) = false

@doc Markdown.doc"""
    zero(R::ArbField)

Return exact zero in the given Arb field.
"""
zero(R::ArbField) = R(0)

@doc Markdown.doc"""
    one(R::ArbField)

Return exact one in the given Arb field.
"""
one(R::ArbField) = R(1)

# TODO: Add hash (and document under arb basic functionality)

@doc Markdown.doc"""
    accuracy_bits(x::arb)

Return the relative accuracy of $x$ measured in bits, capped between
`typemax(Int)` and `-typemax(Int)`.
"""
function accuracy_bits(x::arb)
  return ccall((:arb_rel_accuracy_bits, libarb), Int, (Ref{arb},), x)
end

function deepcopy_internal(a::arb, dict::IdDict)
  b = parent(a)()
  ccall((:arb_set, libarb), Nothing, (Ref{arb}, Ref{arb}), b, a)
  return b
end


function canonical_unit(x::arb)
   return x
end

function check_parent(a::arb, b::arb)
   parent(a) != parent(b) &&
             error("Incompatible arb elements")
end

characteristic(::ArbField) = 0

################################################################################
#
#  Conversions
#
################################################################################

@doc Markdown.doc"""
    Float64(x::arb)

Return the midpoint of $x$ rounded down to a machine double.
"""
function Float64(x::arb)
   GC.@preserve x begin
      t = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct}, (Ref{arb}, ), x)
      # 4 == round to nearest
      d = ccall((:arf_get_d, libarb), Float64, (Ptr{arf_struct}, Int), t, 4)
   end
   return d
end

@doc Markdown.doc"""
    convert(::Type{Float64}, x::arb)

Return the midpoint of $x$ rounded down to a machine double.
"""
function convert(::Type{Float64}, x::arb)
    return Float64(x)
end

################################################################################
#
#  String I/O
#
################################################################################

function native_string(x::arb)
   d = ceil(parent(x).prec * 0.30102999566398119521)
   cstr = ccall((:arb_get_str, libarb), Ptr{UInt8},
                (Ref{arb}, Int, UInt),
                x, Int(d), UInt(0))
   res = unsafe_string(cstr)
   ccall((:flint_free, libflint), Nothing,
         (Ptr{UInt8},),
         cstr)
   return res
end

function AbstractAlgebra.expressify(x::arb; context = nothing)
   if isexact(x) && isnegative(x)
      # TODO isexact does not imply it is printed without radius
      return Expr(:call, :-, native_string(-x))
   else
      return native_string(x)
   end
end

function show(io::IO, x::ArbField)
  print(io, "Real Field with ")
  print(io, precision(x))
  print(io, " bits of precision and error bounds")
end

function show(io::IO, x::arb)
   print(io, native_string(x))
end

################################################################################
#
#  Containment
#
################################################################################

@doc Markdown.doc"""
    overlaps(x::arb, y::arb)

Returns `true` if any part of the ball $x$ overlaps any part of the ball $y$,
otherwise return `false`.
"""
function overlaps(x::arb, y::arb)
  r = ccall((:arb_overlaps, libarb), Cint, (Ref{arb}, Ref{arb}), x, y)
  return Bool(r)
end

#function contains(x::arb, y::arf)
#  r = ccall((:arb_contains_arf, libarb), Cint, (Ref{arb}, Ref{arf}), x, y)
#  return Bool(r)
#end

@doc Markdown.doc"""
    contains(x::arb, y::fmpq)

Returns `true` if the ball $x$ contains the given rational value, otherwise
return `false`.
"""
function contains(x::arb, y::fmpq)
  r = ccall((:arb_contains_fmpq, libarb), Cint, (Ref{arb}, Ref{fmpq}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::arb, y::fmpz)

Returns `true` if the ball $x$ contains the given integer value, otherwise
return `false`.
"""
function contains(x::arb, y::fmpz)
  r = ccall((:arb_contains_fmpz, libarb), Cint, (Ref{arb}, Ref{fmpz}), x, y)
  return Bool(r)
end

function contains(x::arb, y::Int)
  r = ccall((:arb_contains_si, libarb), Cint, (Ref{arb}, Int), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::arb, y::Integer)

Returns `true` if the ball $x$ contains the given integer value, otherwise
return `false`.
"""
contains(x::arb, y::Integer) = contains(x, fmpz(y))

@doc Markdown.doc"""
    contains(x::arb, y::Rational{T}) where {T <: Integer}

Returns `true` if the ball $x$ contains the given rational value, otherwise
return `false`.
"""
contains(x::arb, y::Rational{T}) where {T <: Integer} = contains(x, fmpq(y))

@doc Markdown.doc"""
    contains(x::arb, y::BigFloat)

Returns `true` if the ball $x$ contains the given floating point value,
otherwise return `false`.
"""
function contains(x::arb, y::BigFloat)
  r = ccall((:arb_contains_mpfr, libarb), Cint,
              (Ref{arb}, Ref{BigFloat}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains(x::arb, y::arb)

Returns `true` if the ball $x$ contains the ball $y$, otherwise return
`false`.
"""
function contains(x::arb, y::arb)
  r = ccall((:arb_contains, libarb), Cint, (Ref{arb}, Ref{arb}), x, y)
  return Bool(r)
end

@doc Markdown.doc"""
    contains_zero(x::arb)

Returns `true` if the ball $x$ contains zero, otherwise return `false`.
"""
function contains_zero(x::arb)
   r = ccall((:arb_contains_zero, libarb), Cint, (Ref{arb}, ), x)
   return Bool(r)
end

@doc Markdown.doc"""
    contains_negative(x::arb)

Returns `true` if the ball $x$ contains any negative value, otherwise return
`false`.
"""
function contains_negative(x::arb)
   r = ccall((:arb_contains_negative, libarb), Cint, (Ref{arb}, ), x)
   return Bool(r)
end

@doc Markdown.doc"""
    contains_positive(x::arb)

Returns `true` if the ball $x$ contains any positive value, otherwise return
`false`.
"""
function contains_positive(x::arb)
   r = ccall((:arb_contains_positive, libarb), Cint, (Ref{arb}, ), x)
   return Bool(r)
end

@doc Markdown.doc"""
    contains_nonnegative(x::arb)

Returns `true` if the ball $x$ contains any nonnegative value, otherwise
return `false`.
"""
function contains_nonnegative(x::arb)
   r = ccall((:arb_contains_nonnegative, libarb), Cint, (Ref{arb}, ), x)
   return Bool(r)
end

@doc Markdown.doc"""
    contains_nonpositive(x::arb)

Returns `true` if the ball $x$ contains any nonpositive value, otherwise
return `false`.
"""
function contains_nonpositive(x::arb)
   r = ccall((:arb_contains_nonpositive, libarb), Cint, (Ref{arb}, ), x)
   return Bool(r)
end

################################################################################
#
#  Comparison
#
################################################################################

@doc Markdown.doc"""
    isequal(x::arb, y::arb)

Return `true` if the balls $x$ and $y$ are precisely equal, i.e. have the
same midpoints and radii.
"""
function isequal(x::arb, y::arb)
  r = ccall((:arb_equal, libarb), Cint, (Ref{arb}, Ref{arb}), x, y)
  return Bool(r)
end

function ==(x::arb, y::arb)
    return Bool(ccall((:arb_eq, libarb), Cint, (Ref{arb}, Ref{arb}), x, y))
end

function !=(x::arb, y::arb)
    return Bool(ccall((:arb_ne, libarb), Cint, (Ref{arb}, Ref{arb}), x, y))
end

function >(x::arb, y::arb)
    return Bool(ccall((:arb_gt, libarb), Cint, (Ref{arb}, Ref{arb}), x, y))
end

function >=(x::arb, y::arb)
    return Bool(ccall((:arb_ge, libarb), Cint, (Ref{arb}, Ref{arb}), x, y))
end

function <(x::arb, y::arb)
    return Bool(ccall((:arb_lt, libarb), Cint, (Ref{arb}, Ref{arb}), x, y))
end

function <=(x::arb, y::arb)
    return Bool(ccall((:arb_le, libarb), Cint, (Ref{arb}, Ref{arb}), x, y))
end

==(x::arb, y::Int) = x == arb(y)
!=(x::arb, y::Int) = x != arb(y)
<=(x::arb, y::Int) = x <= arb(y)
>=(x::arb, y::Int) = x >= arb(y)
<(x::arb, y::Int) = x < arb(y)
>(x::arb, y::Int) = x > arb(y)

==(x::Int, y::arb) = arb(x) == y
!=(x::Int, y::arb) = arb(x) != y
<=(x::Int, y::arb) = arb(x) <= y
>=(x::Int, y::arb) = arb(x) >= y
<(x::Int, y::arb) = arb(x) < y
>(x::Int, y::arb) = arb(x) > y

==(x::arb, y::fmpz) = x == arb(y)
!=(x::arb, y::fmpz) = x != arb(y)
<=(x::arb, y::fmpz) = x <= arb(y)
>=(x::arb, y::fmpz) = x >= arb(y)
<(x::arb, y::fmpz) = x < arb(y)
>(x::arb, y::fmpz) = x > arb(y)

==(x::fmpz, y::arb) = arb(x) == y
!=(x::fmpz, y::arb) = arb(x) != y
<=(x::fmpz, y::arb) = arb(x) <= y
>=(x::fmpz, y::arb) = arb(x) >= y
<(x::fmpz, y::arb) = arb(x) < y
>(x::fmpz, y::arb) = arb(x) > y

==(x::arb, y::Integer) = x == fmpz(y)
!=(x::arb, y::Integer) = x != fmpz(y)
<=(x::arb, y::Integer) = x <= fmpz(y)
>=(x::arb, y::Integer) = x >= fmpz(y)
<(x::arb, y::Integer) = x < fmpz(y)
>(x::arb, y::Integer) = x > fmpz(y)


==(x::Integer, y::arb) = fmpz(x) == y
!=(x::Integer, y::arb) = fmpz(x) != y
<=(x::Integer, y::arb) = fmpz(x) <= y
>=(x::Integer, y::arb) = fmpz(x) >= y
<(x::Integer, y::arb) = fmpz(x) < y
>(x::Integer, y::arb) = fmpz(x) > y

==(x::arb, y::Float64) = x == arb(y)
!=(x::arb, y::Float64) = x != arb(y)
<=(x::arb, y::Float64) = x <= arb(y)
>=(x::arb, y::Float64) = x >= arb(y)
<(x::arb, y::Float64) = x < arb(y)
>(x::arb, y::Float64) = x > arb(y)

==(x::Float64, y::arb) = arb(x) == y
!=(x::Float64, y::arb) = arb(x) != y
<=(x::Float64, y::arb) = arb(x) <= y
>=(x::Float64, y::arb) = arb(x) >= y
<(x::Float64, y::arb) = arb(x) < y
>(x::Float64, y::arb) = arb(x) > y

==(x::arb, y::BigFloat) = x == arb(y)
!=(x::arb, y::BigFloat) = x != arb(y)
<=(x::arb, y::BigFloat) = x <= arb(y)
>=(x::arb, y::BigFloat) = x >= arb(y)
<(x::arb, y::BigFloat) = x < arb(y)
>(x::arb, y::BigFloat) = x > arb(y)

==(x::BigFloat, y::arb) = arb(x) == y
!=(x::BigFloat, y::arb) = arb(x) != y
<=(x::BigFloat, y::arb) = arb(x) <= y
>=(x::BigFloat, y::arb) = arb(x) >= y
<(x::BigFloat, y::arb) = arb(x) < y
>(x::BigFloat, y::arb) = arb(x) > y

==(x::arb, y::fmpq) = x == arb(y, precision(parent(x)))
!=(x::arb, y::fmpq) = x != arb(y, precision(parent(x)))
<=(x::arb, y::fmpq) = x <= arb(y, precision(parent(x)))
>=(x::arb, y::fmpq) = x >= arb(y, precision(parent(x)))
<(x::arb, y::fmpq) = x < arb(y, precision(parent(x)))
>(x::arb, y::fmpq) = x > arb(y, precision(parent(x)))

==(x::fmpq, y::arb) = arb(x, precision(parent(y))) == y
!=(x::fmpq, y::arb) = arb(x, precision(parent(y))) != y
<=(x::fmpq, y::arb) = arb(x, precision(parent(y))) <= y
>=(x::fmpq, y::arb) = arb(x, precision(parent(y))) >= y
<(x::fmpq, y::arb) = arb(x, precision(parent(y))) < y
>(x::fmpq, y::arb) = arb(x, precision(parent(y))) > y

==(x::arb, y::Rational{T}) where {T <: Integer} = x == fmpq(y)
!=(x::arb, y::Rational{T}) where {T <: Integer} = x != fmpq(y)
<=(x::arb, y::Rational{T}) where {T <: Integer} = x <= fmpq(y)
>=(x::arb, y::Rational{T}) where {T <: Integer} = x >= fmpq(y)
<(x::arb, y::Rational{T}) where {T <: Integer} = x < fmpq(y)
>(x::arb, y::Rational{T}) where {T <: Integer} = x > fmpq(y)

==(x::Rational{T}, y::arb) where {T <: Integer} = fmpq(x) == y
!=(x::Rational{T}, y::arb) where {T <: Integer} = fmpq(x) != y
<=(x::Rational{T}, y::arb) where {T <: Integer} = fmpq(x) <= y
>=(x::Rational{T}, y::arb) where {T <: Integer} = fmpq(x) >= y
<(x::Rational{T}, y::arb) where {T <: Integer} = fmpq(x) < y
>(x::Rational{T}, y::arb) where {T <: Integer} = fmpq(x) > y

################################################################################
#
#  Predicates
#
################################################################################

function isunit(x::arb)
   !iszero(x)
end

@doc Markdown.doc"""
    iszero(x::arb)

Return `true` if $x$ is certainly zero, otherwise return `false`.
"""
function iszero(x::arb)
   return Bool(ccall((:arb_is_zero, libarb), Cint, (Ref{arb},), x))
end

@doc Markdown.doc"""
    isnonzero(x::arb)

Return `true` if $x$ is certainly not equal to zero, otherwise return
`false`.
"""
function isnonzero(x::arb)
   return Bool(ccall((:arb_is_nonzero, libarb), Cint, (Ref{arb},), x))
end

@doc Markdown.doc"""
    isone(x::arb)

Return `true` if $x$ is certainly not equal to oneo, otherwise return
`false`.
"""
function isone(x::arb)
   return Bool(ccall((:arb_is_one, libarb), Cint, (Ref{arb},), x))
end

@doc Markdown.doc"""
    isfinite(x::arb)

Return `true` if $x$ is finite, i.e. having finite midpoint and radius,
otherwise return `false`.
"""
function isfinite(x::arb)
   return Bool(ccall((:arb_is_finite, libarb), Cint, (Ref{arb},), x))
end

@doc Markdown.doc"""
    isexact(x::arb)

Return `true` if $x$ is exact, i.e. has zero radius, otherwise return
`false`.
"""
function isexact(x::arb)
   return Bool(ccall((:arb_is_exact, libarb), Cint, (Ref{arb},), x))
end

@doc Markdown.doc"""
    isint(x::arb)

Return `true` if $x$ is an exact integer, otherwise return `false`.
"""
function isint(x::arb)
   return Bool(ccall((:arb_is_int, libarb), Cint, (Ref{arb},), x))
end

@doc Markdown.doc"""
    ispositive(x::arb)

Return `true` if $x$ is certainly positive, otherwise return `false`.
"""
function ispositive(x::arb)
   return Bool(ccall((:arb_is_positive, libarb), Cint, (Ref{arb},), x))
end

@doc Markdown.doc"""
    isnonnegative(x::arb)

Return `true` if $x$ is certainly nonnegative, otherwise return `false`.
"""
function isnonnegative(x::arb)
   return Bool(ccall((:arb_is_nonnegative, libarb), Cint, (Ref{arb},), x))
end

@doc Markdown.doc"""
    isnegative(x::arb)

Return `true` if $x$ is certainly negative, otherwise return `false`.
"""
function isnegative(x::arb)
   return Bool(ccall((:arb_is_negative, libarb), Cint, (Ref{arb},), x))
end

@doc Markdown.doc"""
    isnonpositive(x::arb)

Return `true` if $x$ is certainly nonpositive, otherwise return `false`.
"""
function isnonpositive(x::arb)
   return Bool(ccall((:arb_is_nonpositive, libarb), Cint, (Ref{arb},), x))
end

################################################################################
#
#  Parts of numbers
#
################################################################################

@doc Markdown.doc"""
    ball(mid::arb, rad::arb)

Constructs an `arb` enclosing the range $[m-|r|, m+|r|]$, given the pair
$(m, r)$.
"""
function ball(mid::arb, rad::arb)
  z = arb(mid, rad)
  z.parent = parent(mid)
  return z
end

@doc Markdown.doc"""
    radius(x::arb)

Return the radius of the ball $x$ as an Arb ball.
"""
function radius(x::arb)
  z = parent(x)()
  ccall((:arb_get_rad_arb, libarb), Nothing, (Ref{arb}, Ref{arb}), z, x)
  return z
end

@doc Markdown.doc"""
    midpoint(x::arb)

Return the midpoint of the ball $x$ as an Arb ball.
"""
function midpoint(x::arb)
  z = parent(x)()
  ccall((:arb_get_mid_arb, libarb), Nothing, (Ref{arb}, Ref{arb}), z, x)
  return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::arb)
  z = parent(x)()
  ccall((:arb_neg, libarb), Nothing, (Ref{arb}, Ref{arb}), z, x)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

for (s,f) in ((:+,"arb_add"), (:*,"arb_mul"), (://, "arb_div"), (:-,"arb_sub"))
  @eval begin
    function ($s)(x::arb, y::arb)
      z = parent(x)()
      ccall(($f, libarb), Nothing, (Ref{arb}, Ref{arb}, Ref{arb}, Int),
                           z, x, y, parent(x).prec)
      return z
    end
  end
end

for (f,s) in ((:+, "add"), (:*, "mul"))
  @eval begin
    #function ($f)(x::arb, y::arf)
    #  z = parent(x)()
    #  ccall(($("arb_"*s*"_arf"), libarb), Nothing,
    #              (Ref{arb}, Ref{arb}, Ref{arf}, Int),
    #              z, x, y, parent(x).prec)
    #  return z
    #end

    #($f)(x::arf, y::arb) = ($f)(y, x)

    function ($f)(x::arb, y::UInt)
      z = parent(x)()
      ccall(($("arb_"*s*"_ui"), libarb), Nothing,
                  (Ref{arb}, Ref{arb}, UInt, Int),
                  z, x, y, parent(x).prec)
      return z
    end

    ($f)(x::UInt, y::arb) = ($f)(y, x)

    function ($f)(x::arb, y::Int)
      z = parent(x)()
      ccall(($("arb_"*s*"_si"), libarb), Nothing,
      (Ref{arb}, Ref{arb}, Int, Int), z, x, y, parent(x).prec)
      return z
    end

    ($f)(x::Int, y::arb) = ($f)(y,x)

    function ($f)(x::arb, y::fmpz)
      z = parent(x)()
      ccall(($("arb_"*s*"_fmpz"), libarb), Nothing,
                  (Ref{arb}, Ref{arb}, Ref{fmpz}, Int),
                  z, x, y, parent(x).prec)
      return z
    end

    ($f)(x::fmpz, y::arb) = ($f)(y,x)
  end
end

#function -(x::arb, y::arf)
#  z = parent(x)()
#  ccall((:arb_sub_arf, libarb), Nothing,
#              (Ref{arb}, Ref{arb}, Ref{arf}, Int), z, x, y, parent(x).prec)
#  return z
#end

#-(x::arf, y::arb) = -(y - x)

function -(x::arb, y::UInt)
  z = parent(x)()
  ccall((:arb_sub_ui, libarb), Nothing,
              (Ref{arb}, Ref{arb}, UInt, Int), z, x, y, parent(x).prec)
  return z
end

-(x::UInt, y::arb) = -(y - x)

function -(x::arb, y::Int)
  z = parent(x)()
  ccall((:arb_sub_si, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Int, Int), z, x, y, parent(x).prec)
  return z
end

-(x::Int, y::arb) = -(y - x)

function -(x::arb, y::fmpz)
  z = parent(x)()
  ccall((:arb_sub_fmpz, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{fmpz}, Int),
              z, x, y, parent(x).prec)
  return z
end

-(x::fmpz, y::arb) = -(y-x)

+(x::arb, y::Integer) = x + fmpz(y)

-(x::arb, y::Integer) = x - fmpz(y)

*(x::arb, y::Integer) = x*fmpz(y)

//(x::arb, y::Integer) = x//fmpz(y)

+(x::Integer, y::arb) = fmpz(x) + y

-(x::Integer, y::arb) = fmpz(x) - y

*(x::Integer, y::arb) = fmpz(x)*y

//(x::Integer, y::arb) = fmpz(x)//y

#function //(x::arb, y::arf)
#  z = parent(x)()
#  ccall((:arb_div_arf, libarb), Nothing,
#              (Ref{arb}, Ref{arb}, Ref{arf}, Int), z, x, y, parent(x).prec)
#  return z
#end

function //(x::arb, y::UInt)
  z = parent(x)()
  ccall((:arb_div_ui, libarb), Nothing,
              (Ref{arb}, Ref{arb}, UInt, Int), z, x, y, parent(x).prec)
  return z
end

function //(x::arb, y::Int)
  z = parent(x)()
  ccall((:arb_div_si, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Int, Int), z, x, y, parent(x).prec)
  return z
end

function //(x::arb, y::fmpz)
  z = parent(x)()
  ccall((:arb_div_fmpz, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{fmpz}, Int),
              z, x, y, parent(x).prec)
  return z
end

function //(x::UInt, y::arb)
  z = parent(y)()
  ccall((:arb_ui_div, libarb), Nothing,
              (Ref{arb}, UInt, Ref{arb}, Int), z, x, y, parent(y).prec)
  return z
end

function //(x::Int, y::arb)
  z = parent(y)()
  t = arb(x)
  ccall((:arb_div, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), z, t, y, parent(y).prec)
  return z
end

function //(x::fmpz, y::arb)
  z = parent(y)()
  t = arb(x)
  ccall((:arb_div, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), z, t, y, parent(y).prec)
  return z
end

function ^(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_pow, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), z, x, y, parent(x).prec)
  return z
end

function ^(x::arb, y::fmpz)
  z = parent(x)()
  ccall((:arb_pow_fmpz, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{fmpz}, Int),
              z, x, y, parent(x).prec)
  return z
end

^(x::arb, y::Integer) = x^fmpz(y)

function ^(x::arb, y::UInt)
  z = parent(x)()
  ccall((:arb_pow_ui, libarb), Nothing,
              (Ref{arb}, Ref{arb}, UInt, Int), z, x, y, parent(x).prec)
  return z
end

function ^(x::arb, y::fmpq)
  z = parent(x)()
  ccall((:arb_pow_fmpq, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{fmpq}, Int),
              z, x, y, parent(x).prec)
  return z
end

+(x::fmpq, y::arb) = parent(y)(x) + y
+(x::arb, y::fmpq) = x + parent(x)(y)
-(x::fmpq, y::arb) = parent(y)(x) - y
//(x::arb, y::fmpq) = x//parent(x)(y)
//(x::fmpq, y::arb) = parent(y)(x)//y
-(x::arb, y::fmpq) = x - parent(x)(y)
*(x::fmpq, y::arb) = parent(y)(x) * y
*(x::arb, y::fmpq) = x * parent(x)(y)
^(x::fmpq, y::arb) = parent(y)(x) ^ y

+(x::Float64, y::arb) = parent(y)(x) + y
+(x::arb, y::Float64) = x + parent(x)(y)
-(x::Float64, y::arb) = parent(y)(x) - y
//(x::arb, y::Float64) = x//parent(x)(y)
//(x::Float64, y::arb) = parent(y)(x)//y
-(x::arb, y::Float64) = x - parent(x)(y)
*(x::Float64, y::arb) = parent(y)(x) * y
*(x::arb, y::Float64) = x * parent(x)(y)
^(x::Float64, y::arb) = parent(y)(x) ^ y
^(x::arb, y::Float64) = x ^ parent(x)(y)

+(x::BigFloat, y::arb) = parent(y)(x) + y
+(x::arb, y::BigFloat) = x + parent(x)(y)
-(x::BigFloat, y::arb) = parent(y)(x) - y
//(x::arb, y::BigFloat) = x//parent(x)(y)
//(x::BigFloat, y::arb) = parent(y)(x)//y
-(x::arb, y::BigFloat) = x - parent(x)(y)
*(x::BigFloat, y::arb) = parent(y)(x) * y
*(x::arb, y::BigFloat) = x * parent(x)(y)
^(x::BigFloat, y::arb) = parent(y)(x) ^ y
^(x::arb, y::BigFloat) = x ^ parent(x)(y)

+(x::Rational{T}, y::arb) where {T <: Integer} = fmpq(x) + y
+(x::arb, y::Rational{T}) where {T <: Integer} = x + fmpq(y)
-(x::Rational{T}, y::arb) where {T <: Integer} = fmpq(x) - y
-(x::arb, y::Rational{T}) where {T <: Integer} = x - fmpq(y)
//(x::Rational{T}, y::arb) where {T <: Integer} = fmpq(x)//y
//(x::arb, y::Rational{T}) where {T <: Integer} = x//fmpq(y)
*(x::Rational{T}, y::arb) where {T <: Integer} = fmpq(x) * y
*(x::arb, y::Rational{T}) where {T <: Integer} = x * fmpq(y)
^(x::Rational{T}, y::arb) where {T <: Integer} = fmpq(x) ^ y
^(x::arb, y::Rational{T}) where {T <: Integer} = x ^ fmpq(y)

/(x::arb, y::arb) = x // y
/(x::fmpz, y::arb) = x // y
/(x::arb, y::fmpz) = x // y
/(x::Int, y::arb) = x // y
/(x::arb, y::Int) = x // y
/(x::UInt, y::arb) = x // y
/(x::arb, y::UInt) = x // y
/(x::fmpq, y::arb) = x // y
/(x::arb, y::fmpq) = x // y
/(x::Rational{T}, y::arb) where {T <: Integer} = x // y
/(x::arb, y::Rational{T}) where {T <: Integer} = x // y

divexact(x::arb, y::arb) = x // y
divexact(x::fmpz, y::arb) = x // y
divexact(x::arb, y::fmpz) = x // y
divexact(x::Int, y::arb) = x // y
divexact(x::arb, y::Int) = x // y
divexact(x::UInt, y::arb) = x // y
divexact(x::arb, y::UInt) = x // y
divexact(x::fmpq, y::arb) = x // y
divexact(x::arb, y::fmpq) = x // y
divexact(x::Rational{T}, y::arb) where {T <: Integer} = x // y
divexact(x::arb, y::Rational{T}) where {T <: Integer} = x // y

################################################################################
#
#  Absolute value
#
################################################################################

@doc Markdown.doc"""
    abs(x::arb)

Return the absolute value of $x$.
"""
function abs(x::arb)
  z = parent(x)()
  ccall((:arb_abs, libarb), Nothing, (Ref{arb}, Ref{arb}), z, x)
  return z
end

################################################################################
#
#  Inverse
#
################################################################################

@doc Markdown.doc"""
    inv(x::arb)

Return the multiplicative inverse of $x$, i.e. $1/x$.
"""
function inv(x::arb)
  z = parent(x)()
  ccall((:arb_inv, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
  return parent(x)(z)
end

################################################################################
#
#  Shifting
#
################################################################################

@doc Markdown.doc"""
    ldexp(x::arb, y::Int)

Return $2^yx$. Note that $y$ can be positive, zero or negative.
"""
function ldexp(x::arb, y::Int)
  z = parent(x)()
  ccall((:arb_mul_2exp_si, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Int), z, x, y)
  return z
end

@doc Markdown.doc"""
    ldexp(x::arb, y::fmpz)

Return $2^yx$. Note that $y$ can be positive, zero or negative.
"""
function ldexp(x::arb, y::fmpz)
  z = parent(x)()
  ccall((:arb_mul_2exp_fmpz, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{fmpz}), z, x, y)
  return z
end

################################################################################
#
#  Miscellaneous
#
################################################################################

@doc Markdown.doc"""
    trim(x::arb)

Return an `arb` interval containing $x$ but which may be more economical,
by rounding off insignificant bits from the midpoint.
"""
function trim(x::arb)
  z = parent(x)()
  ccall((:arb_trim, libarb), Nothing, (Ref{arb}, Ref{arb}), z, x)
  return z
end

@doc Markdown.doc"""
    unique_integer(x::arb)

Return a pair where the first value is a boolean and the second is an `fmpz`
integer. The boolean indicates whether the interval $x$ contains a unique
integer. If this is the case, the second return value is set to this unique
integer.
"""
function unique_integer(x::arb)
  z = fmpz()
  unique = ccall((:arb_get_unique_fmpz, libarb), Int,
    (Ref{fmpz}, Ref{arb}), z, x)
  return (unique != 0, z)
end

function (::FlintIntegerRing)(a::arb)
   if !Nemo.isint(a)
      error("Argument must be an integer.")
   end
   ui = unique_integer(a)
   if ui[1] == false
      error("Argument must be an integer.")
   else
      return ui[2]
   end
end

@doc Markdown.doc"""
    setunion(x::arb, y::arb)

Return an `arb` containing the union of the intervals represented by $x$ and
$y$.
"""
function setunion(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_union, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), z, x, y, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    setintersection(x::arb, y::arb)

Return an `arb` containing the intersection of the intervals represented by
$x$ and $y$.
"""
function setintersection(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_intersection, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), z, x, y, parent(x).prec)
  return z
end

################################################################################
#
#  Constants
#
################################################################################

@doc Markdown.doc"""
    const_pi(r::ArbField)

Return $\pi = 3.14159\ldots$ as an element of $r$.
"""
function const_pi(r::ArbField)
  z = r()
  ccall((:arb_const_pi, libarb), Nothing, (Ref{arb}, Int), z, precision(r))
  return z
end

@doc Markdown.doc"""
    const_e(r::ArbField)

Return $e = 2.71828\ldots$ as an element of $r$.
"""
function const_e(r::ArbField)
  z = r()
  ccall((:arb_const_e, libarb), Nothing, (Ref{arb}, Int), z, precision(r))
  return z
end

@doc Markdown.doc"""
    const_log2(r::ArbField)

Return $\log(2) = 0.69314\ldots$ as an element of $r$.
"""
function const_log2(r::ArbField)
  z = r()
  ccall((:arb_const_log2, libarb), Nothing, (Ref{arb}, Int), z, precision(r))
  return z
end

@doc Markdown.doc"""
    const_log10(r::ArbField)

Return $\log(10) = 2.302585\ldots$ as an element of $r$.
"""
function const_log10(r::ArbField)
  z = r()
  ccall((:arb_const_log10, libarb), Nothing, (Ref{arb}, Int), z, precision(r))
  return z
end

@doc Markdown.doc"""
    const_euler(r::ArbField)

Return Euler's constant $\gamma = 0.577215\ldots$ as an element of $r$.
"""
function const_euler(r::ArbField)
  z = r()
  ccall((:arb_const_euler, libarb), Nothing, (Ref{arb}, Int), z, precision(r))
  return z
end

@doc Markdown.doc"""
    const_catalan(r::ArbField)

Return Catalan's constant $C = 0.915965\ldots$ as an element of $r$.
"""
function const_catalan(r::ArbField)
  z = r()
  ccall((:arb_const_catalan, libarb), Nothing, (Ref{arb}, Int), z, precision(r))
  return z
end

@doc Markdown.doc"""
    const_khinchin(r::ArbField)

Return Khinchin's constant $K = 2.685452\ldots$ as an element of $r$.
"""
function const_khinchin(r::ArbField)
  z = r()
  ccall((:arb_const_khinchin, libarb), Nothing, (Ref{arb}, Int), z, precision(r))
  return z
end

@doc Markdown.doc"""
    const_glaisher(r::ArbField)

Return Glaisher's constant $A = 1.282427\ldots$ as an element of $r$.
"""
function const_glaisher(r::ArbField)
  z = r()
  ccall((:arb_const_glaisher, libarb), Nothing, (Ref{arb}, Int), z, precision(r))
  return z
end

################################################################################
#
#  Real valued functions
#
################################################################################

# real - real functions

@doc Markdown.doc"""
    floor(x::arb)

Compute the floor of $x$, i.e. the greatest integer not exceeding $x$, as an
Arb.
"""
function floor(x::arb)
   z = parent(x)()
   ccall((:arb_floor, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    ceil(x::arb)

Return the ceiling of $x$, i.e. the least integer not less than $x$, as an
Arb.
"""
function ceil(x::arb)
   z = parent(x)()
   ccall((:arb_ceil, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    sqrt(x::arb)

Return the square root of $x$.
"""
function Base.sqrt(x::arb)
   z = parent(x)()
   ccall((:arb_sqrt, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    rsqrt(x::arb)

Return the reciprocal of the square root of $x$, i.e. $1/\sqrt{x}$.
"""
function rsqrt(x::arb)
   z = parent(x)()
   ccall((:arb_rsqrt, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    sqrt1pm1(x::arb)

Return $\sqrt{1+x}-1$, evaluated accurately for small $x$.
"""
function sqrt1pm1(x::arb)
   z = parent(x)()
   ccall((:arb_sqrt1pm1, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    sqrtpos(x::arb)

Return the sqrt root of $x$, assuming that $x$ represents a nonnegative
number. Thus any negative number in the input interval is discarded.
"""
function sqrtpos(x::arb)
   z = parent(x)()
   ccall((:arb_sqrtpos, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end


@doc Markdown.doc"""
    log(x::arb)

Return the principal branch of the logarithm of $x$.
"""
function log(x::arb)
   z = parent(x)()
   ccall((:arb_log, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    log1p(x::arb)

Return $\log(1+x)$, evaluated accurately for small $x$.
"""
function log1p(x::arb)
   z = parent(x)()
   ccall((:arb_log1p, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    exp(x::arb)

Return the exponential of $x$.
"""
function Base.exp(x::arb)
   z = parent(x)()
   ccall((:arb_exp, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    expm1(x::arb)

Return $\exp(x)-1$, evaluated accurately for small $x$.
"""
function expm1(x::arb)
   z = parent(x)()
   ccall((:arb_expm1, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    sin(x::arb)

Return the sine of $x$.
"""
function sin(x::arb)
   z = parent(x)()
   ccall((:arb_sin, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    cos(x::arb)

Return the cosine of $x$.
"""
function cos(x::arb)
   z = parent(x)()
   ccall((:arb_cos, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    sinpi(x::arb)

Return the sine of $\pi x$.
"""
function sinpi(x::arb)
   z = parent(x)()
   ccall((:arb_sin_pi, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    cospi(x::arb)

Return the cosine of $\pi x$.
"""
function cospi(x::arb)
   z = parent(x)()
   ccall((:arb_cos_pi, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    tan(x::arb)

Return the tangent of $x$.
"""
function tan(x::arb)
   z = parent(x)()
   ccall((:arb_tan, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    cot(x::arb)

Return the cotangent of $x$.
"""
function cot(x::arb)
   z = parent(x)()
   ccall((:arb_cot, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    tanpi(x::arb)

Return the tangent of $\pi x$.
"""
function tanpi(x::arb)
   z = parent(x)()
   ccall((:arb_tan_pi, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    cotpi(x::arb)

Return the cotangent of $\pi x$.
"""
function cotpi(x::arb)
   z = parent(x)()
   ccall((:arb_cot_pi, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    sinh(x::arb)

Return the hyperbolic sine of $x$.
"""
function sinh(x::arb)
   z = parent(x)()
   ccall((:arb_sinh, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    cosh(x::arb)

Return the hyperbolic cosine of $x$.
"""
function cosh(x::arb)
   z = parent(x)()
   ccall((:arb_cosh, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    tanh(x::arb)

Return the hyperbolic tangent of $x$.
"""
function tanh(x::arb)
   z = parent(x)()
   ccall((:arb_tanh, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    coth(x::arb)

Return the hyperbolic cotangent of $x$.
"""
function coth(x::arb)
   z = parent(x)()
   ccall((:arb_coth, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    atan(x::arb)

Return the arctangent of $x$.
"""
function atan(x::arb)
   z = parent(x)()
   ccall((:arb_atan, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    asin(x::arb)

Return the arcsine of $x$.
"""
function asin(x::arb)
   z = parent(x)()
   ccall((:arb_asin, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    acos(x::arb)

Return the arccosine of $x$.
"""
function acos(x::arb)
   z = parent(x)()
   ccall((:arb_acos, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    atanh(x::arb)

Return the hyperbolic arctangent of $x$.
"""
function atanh(x::arb)
   z = parent(x)()
   ccall((:arb_atanh, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    asinh(x::arb)

Return the hyperbolic arcsine of $x$.
"""
function asinh(x::arb)
   z = parent(x)()
   ccall((:arb_asinh, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    acosh(x::arb)

Return the hyperbolic arccosine of $x$.
"""
function acosh(x::arb)
   z = parent(x)()
   ccall((:arb_acosh, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    gamma(x::arb)

Return the Gamma function evaluated at $x$.
"""
function gamma(x::arb)
   z = parent(x)()
   ccall((:arb_gamma, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    lgamma(x::arb)

Return the logarithm of the Gamma function evaluated at $x$.
"""
function lgamma(x::arb)
   z = parent(x)()
   ccall((:arb_lgamma, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    rgamma(x::arb)

Return the reciprocal of the Gamma function evaluated at $x$.
"""
function rgamma(x::arb)
   z = parent(x)()
   ccall((:arb_rgamma, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    digamma(x::arb)

Return the  logarithmic derivative of the gamma function evaluated at $x$,
i.e. $\psi(x)$.
"""
function digamma(x::arb)
   z = parent(x)()
   ccall((:arb_digamma, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    zeta(x::arb)

Return the Riemann zeta function evaluated at $x$.
"""
function zeta(x::arb)
   z = parent(x)()
   ccall((:arb_zeta, libarb), Nothing, (Ref{arb}, Ref{arb}, Int), z, x, parent(x).prec)
   return z
end

@doc Markdown.doc"""
    sincos(x::arb)

Return a tuple $s, c$ consisting of the sine $s$ and cosine $c$ of $x$.
"""
function sincos(x::arb)
  s = parent(x)()
  c = parent(x)()
  ccall((:arb_sin_cos, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), s, c, x, parent(x).prec)
  return (s, c)
end

@doc Markdown.doc"""
    sincospi(x::arb)

Return a tuple $s, c$ consisting of the sine $s$ and cosine $c$ of $\pi x$.
"""
function sincospi(x::arb)
  s = parent(x)()
  c = parent(x)()
  ccall((:arb_sin_cos_pi, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), s, c, x, parent(x).prec)
  return (s, c)
end

@doc Markdown.doc"""
    sinpi(x::fmpq, r::ArbField)

Return the sine of $\pi x$ in the given Arb field.
"""
function sinpi(x::fmpq, r::ArbField)
  z = r()
  ccall((:arb_sin_pi_fmpq, libarb), Nothing,
        (Ref{arb}, Ref{fmpq}, Int), z, x, precision(r))
  return z
end

@doc Markdown.doc"""
    cospi(x::fmpq, r::ArbField)

 Return the cosine of $\pi x$ in the given Arb field.
"""
function cospi(x::fmpq, r::ArbField)
  z = r()
  ccall((:arb_cos_pi_fmpq, libarb), Nothing,
        (Ref{arb}, Ref{fmpq}, Int), z, x, precision(r))
  return z
end

@doc Markdown.doc"""
    sincospi(x::fmpq, r::ArbField)

Return a tuple $s, c$ consisting of the sine and cosine of $\pi x$ in the
given Arb field.
"""
function sincospi(x::fmpq, r::ArbField)
  s = r()
  c = r()
  ccall((:arb_sin_cos_pi_fmpq, libarb), Nothing,
        (Ref{arb}, Ref{arb}, Ref{fmpq}, Int), s, c, x, precision(r))
  return (s, c)
end

@doc Markdown.doc"""
    sinhcosh(x::arb)

Return a tuple $s, c$ consisting of the hyperbolic sine and cosine of $x$.
"""
function sinhcosh(x::arb)
  s = parent(x)()
  c = parent(x)()
  ccall((:arb_sinh_cosh, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), s, c, x, parent(x).prec)
  return (s, c)
end

@doc Markdown.doc"""
    atan2(x::arb, y::arb)

Return atan2$(b,a) = \arg(a+bi)$.
"""
function atan2(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_atan2, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), z, x, y, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    agm(x::arb, y::arb)

Return the arithmetic-geometric mean of $x$ and $y$
"""
function agm(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_agm, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), z, x, y, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    zeta(s::arb, a::arb)

Return the Hurwitz zeta function $\zeta(s,a)$.
"""
function zeta(s::arb, a::arb)
  z = parent(s)()
  ccall((:arb_hurwitz_zeta, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), z, s, a, parent(s).prec)
  return z
end

@doc Markdown.doc"""
    hypot(x::arb, y::arb)

Return $\sqrt{x^2 + y^2}$.
"""
function hypot(x::arb, y::arb)
  z = parent(x)()
  ccall((:arb_hypot, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), z, x, y, parent(x).prec)
  return z
end

function root(x::arb, n::UInt)
  z = parent(x)()
  ccall((:arb_root, libarb), Nothing,
              (Ref{arb}, Ref{arb}, UInt, Int), z, x, n, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    root(x::arb, n::Int)

Return the $n$-th root of $x$. We require $x \geq 0$.
"""
function root(x::arb, n::Int)
  x < 0 && throw(DomainError(x, "Argument must be positive"))
  return root(x, UInt(n))
end

@doc Markdown.doc"""
    fac(x::arb)

Return the factorial of $x$.
"""
fac(x::arb) = gamma(x+1)

function fac(n::UInt, r::ArbField)
  z = r()
  ccall((:arb_fac_ui, libarb), Nothing, (Ref{arb}, UInt, Int), z, n, r.prec)
  return z
end

@doc Markdown.doc"""
    fac(n::Int, r::ArbField)

Return the factorial of $n$ in the given Arb field.
"""
fac(n::Int, r::ArbField) = n < 0 ? fac(r(n)) : fac(UInt(n), r)

@doc Markdown.doc"""
    binom(x::arb, n::UInt)

Return the binomial coefficient ${x \choose n}$.
"""
function binom(x::arb, n::UInt)
  z = parent(x)()
  ccall((:arb_bin_ui, libarb), Nothing,
              (Ref{arb}, Ref{arb}, UInt, Int), z, x, n, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    binom(n::UInt, k::UInt, r::ArbField)

Return the binomial coefficient ${n \choose k}$ in the given Arb field.
"""
function binom(n::UInt, k::UInt, r::ArbField)
  z = r()
  ccall((:arb_bin_uiui, libarb), Nothing,
              (Ref{arb}, UInt, UInt, Int), z, n, k, r.prec)
  return z
end

@doc Markdown.doc"""
    fib(n::fmpz, r::ArbField)

Return the $n$-th Fibonacci number in the given Arb field.
"""
function fib(n::fmpz, r::ArbField)
  z = r()
  ccall((:arb_fib_fmpz, libarb), Nothing,
              (Ref{arb}, Ref{fmpz}, Int), z, n, r.prec)
  return z
end

function fib(n::UInt, r::ArbField)
  z = r()
  ccall((:arb_fib_ui, libarb), Nothing,
              (Ref{arb}, UInt, Int), z, n, r.prec)
  return z
end

@doc Markdown.doc"""
    fib(n::Int, r::ArbField)

Return the $n$-th Fibonacci number in the given Arb field.
"""
fib(n::Int, r::ArbField) = n >= 0 ? fib(UInt(n), r) : fib(fmpz(n), r)

@doc Markdown.doc"""
    gamma(x::fmpz, r::ArbField)

Return the Gamma function evaluated at $x$ in the given Arb field.
"""
function gamma(x::fmpz, r::ArbField)
  z = r()
  ccall((:arb_gamma_fmpz, libarb), Nothing,
              (Ref{arb}, Ref{fmpz}, Int), z, x, r.prec)
  return z
end

@doc Markdown.doc"""
    gamma(x::fmpq, r::ArbField)

Return the Gamma function evaluated at $x$ in the given Arb field.
"""
function gamma(x::fmpq, r::ArbField)
  z = r()
  ccall((:arb_gamma_fmpq, libarb), Nothing,
              (Ref{arb}, Ref{fmpq}, Int), z, x, r.prec)
  return z
end


function zeta(n::UInt, r::ArbField)
  z = r()
  ccall((:arb_zeta_ui, libarb), Nothing,
              (Ref{arb}, UInt, Int), z, n, r.prec)
  return z
end

@doc Markdown.doc"""
    zeta(n::Int, r::ArbField)

Return the Riemann zeta function $\zeta(n)$ as an element of the given Arb
field.
"""
zeta(n::Int, r::ArbField) = n >= 0 ? zeta(UInt(n), r) : zeta(r(n))

function bernoulli(n::UInt, r::ArbField)
  z = r()
  ccall((:arb_bernoulli_ui, libarb), Nothing,
              (Ref{arb}, UInt, Int), z, n, r.prec)
  return z
end

@doc Markdown.doc"""
    bernoulli(n::Int, r::ArbField)

Return the $n$-th Bernoulli number as an element of the given Arb field.
"""
bernoulli(n::Int, r::ArbField) = n >= 0 ? bernoulli(UInt(n), r) : throw(DomainError(n, "Index must be non-negative"))

function risingfac(x::arb, n::UInt)
  z = parent(x)()
  ccall((:arb_rising_ui, libarb), Nothing,
              (Ref{arb}, Ref{arb}, UInt, Int), z, x, n, parent(x).prec)
  return z
end

@doc Markdown.doc"""
    risingfac(x::arb, n::Int)

Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an Arb.
"""
risingfac(x::arb, n::Int) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : risingfac(x, UInt(n))

function risingfac(x::fmpq, n::UInt, r::ArbField)
  z = r()
  ccall((:arb_rising_fmpq_ui, libarb), Nothing,
              (Ref{arb}, Ref{fmpq}, UInt, Int), z, x, n, r.prec)
  return z
end

@doc Markdown.doc"""
    risingfac(x::fmpq, n::Int, r::ArbField)

Return the rising factorial $x(x + 1)\ldots (x + n - 1)$ as an element of the
given Arb field.
"""
risingfac(x::fmpq, n::Int, r::ArbField) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : risingfac(x, UInt(n), r)

function risingfac2(x::arb, n::UInt)
  z = parent(x)()
  w = parent(x)()
  ccall((:arb_rising2_ui, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, UInt, Int), z, w, x, n, parent(x).prec)
  return (z, w)
end

@doc Markdown.doc"""
    risingfac2(x::arb, n::Int)

Return a tuple containing the rising factorial $x(x + 1)\ldots (x + n - 1)$
and its derivative.
"""
risingfac2(x::arb, n::Int) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : risingfac2(x, UInt(n))

function polylog(s::arb, a::arb)
  z = parent(s)()
  ccall((:arb_polylog, libarb), Nothing,
              (Ref{arb}, Ref{arb}, Ref{arb}, Int), z, s, a, parent(s).prec)
  return z
end

function polylog(s::Int, a::arb)
  z = parent(a)()
  ccall((:arb_polylog_si, libarb), Nothing,
              (Ref{arb}, Int, Ref{arb}, Int), z, s, a, parent(a).prec)
  return z
end

@doc Markdown.doc"""
    polylog(s::Union{arb,Int}, a::arb)

Return the polylogarithm Li$_s(a)$.
""" polylog(s::Union{arb,Int}, a::arb)

function chebyshev_t(n::UInt, x::arb)
  z = parent(x)()
  ccall((:arb_chebyshev_t_ui, libarb), Nothing,
              (Ref{arb}, UInt, Ref{arb}, Int), z, n, x, parent(x).prec)
  return z
end

function chebyshev_u(n::UInt, x::arb)
  z = parent(x)()
  ccall((:arb_chebyshev_u_ui, libarb), Nothing,
              (Ref{arb}, UInt, Ref{arb}, Int), z, n, x, parent(x).prec)
  return z
end

function chebyshev_t2(n::UInt, x::arb)
  z = parent(x)()
  w = parent(x)()
  ccall((:arb_chebyshev_t2_ui, libarb), Nothing,
              (Ref{arb}, Ref{arb}, UInt, Ref{arb}, Int), z, w, n, x, parent(x).prec)
  return z, w
end

function chebyshev_u2(n::UInt, x::arb)
  z = parent(x)()
  w = parent(x)()
  ccall((:arb_chebyshev_u2_ui, libarb), Nothing,
              (Ref{arb}, Ref{arb}, UInt, Ref{arb}, Int), z, w, n, x, parent(x).prec)
  return z, w
end

@doc Markdown.doc"""
    chebyshev_t(n::Int, x::arb)

Return the value of the Chebyshev polynomial $T_n(x)$.
"""
chebyshev_t(n::Int, x::arb) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_t(UInt(n), x)

@doc Markdown.doc"""
    chebyshev_u(n::Int, x::arb)

Return the value of the Chebyshev polynomial $U_n(x)$.
"""
chebyshev_u(n::Int, x::arb) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_u(UInt(n), x)

@doc Markdown.doc"""
    chebyshev_t2(n::Int, x::arb)

Return the tuple $(T_{n}(x), T_{n-1}(x))$.
"""
chebyshev_t2(n::Int, x::arb) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_t2(UInt(n), x)

@doc Markdown.doc"""
    chebyshev_u2(n::Int, x::arb)

Return the tuple $(U_{n}(x), U_{n-1}(x))$
"""
chebyshev_u2(n::Int, x::arb) = n < 0 ? throw(DomainError(n, "Index must be non-negative")) : chebyshev_u2(UInt(n), x)

@doc Markdown.doc"""
    bell(n::fmpz, r::ArbField)

Return the Bell number $B_n$ as an element of $r$.
"""
function bell(n::fmpz, r::ArbField)
  z = r()
  ccall((:arb_bell_fmpz, libarb), Nothing,
              (Ref{arb}, Ref{fmpz}, Int), z, n, r.prec)
  return z
end

@doc Markdown.doc"""
    bell(n::Int, r::ArbField)

Return the Bell number $B_n$ as an element of $r$.
"""
bell(n::Int, r::ArbField) = bell(fmpz(n), r)

@doc Markdown.doc"""
    numpart(n::fmpz, r::ArbField)

Return the number of partitions $p(n)$ as an element of $r$.
"""
function numpart(n::fmpz, r::ArbField)
  z = r()
  ccall((:arb_partitions_fmpz, libarb), Nothing,
              (Ref{arb}, Ref{fmpz}, Int), z, n, r.prec)
  return z
end

@doc Markdown.doc"""
    numpart(n::Int, r::ArbField)

Return the number of partitions $p(n)$ as an element of $r$.
"""
numpart(n::Int, r::ArbField) = numpart(fmpz(n), r)

################################################################################
#
#  Linear dependence
#
################################################################################

@doc Markdown.doc"""
    lindep(A::Array{arb, 1}, bits::Int)

Find a small linear combination of the entries of the array $A$ that is small
(using LLL). The entries are first scaled by the given number of bits before
truncating to integers for use in LLL. This function can be used to find linear
dependence between a list of real numbers. The algorithm is heuristic only and
returns an array of Nemo integers representing the linear combination.
"""
function lindep(A::Array{arb, 1}, bits::Int)
  bits < 0 && throw(DomainError(bits, "Number of bits must be non-negative"))
  n = length(A)
  V = [floor(ldexp(s, bits) + 0.5) for s in A]
  M = zero_matrix(ZZ, n, n + 1)
  for i = 1:n
    M[i, i] = ZZ(1)
    flag, M[i, n + 1] = unique_integer(V[i])
    !flag && error("Insufficient precision in lindep")
  end
  L = lll(M)
  return [L[1, i] for i = 1:n]
end

################################################################################
#
#  Simplest rational inside
#
################################################################################

@doc Markdown.doc"""
      simplest_rational_inside(x::arb)

Return the simplest fraction inside the ball `x`. A canonical fraction
`a_1/b_1` is defined to be simpler than `a_2/b_2` iff `b_1 < b_2` or `b_1 =
b_2` and `a_1 < a_2`.
"""
function simplest_rational_inside(x::arb)
   a = fmpz()
   b = fmpz()
   e = fmpz()

   ccall((:arb_get_interval_fmpz_2exp, libarb), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{arb}), a, b, e, x)
   !fits(Int, e) && error("Result does not fit into an fmpq")
   _e = Int(e)
   if e >= 0
      return a << _e
   end
   _e = -_e
   d = fmpz(1) << _e
   return _fmpq_simplest_between(a, d, b, d)
end

################################################################################
#
#  Unsafe operations
#
################################################################################

function zero!(z::arb)
   ccall((:arb_zero, libarb), Nothing, (Ref{arb},), z)
   return z
end

for (s,f) in (("add!","arb_add"), ("mul!","arb_mul"), ("div!", "arb_div"),
              ("sub!","arb_sub"))
  @eval begin
    function ($(Symbol(s)))(z::arb, x::arb, y::arb)
      ccall(($f, libarb), Nothing, (Ref{arb}, Ref{arb}, Ref{arb}, Int),
                           z, x, y, parent(x).prec)
      return z
    end
  end
end

function addeq!(z::arb, x::arb)
    ccall((:arb_add, libarb), Nothing, (Ref{arb}, Ref{arb}, Ref{arb}, Int),
                           z, z, x, parent(x).prec)
    return z
end

################################################################################
#
#  Unsafe setting
#
################################################################################

for (typeofx, passtoc) in ((arb, Ref{arb}), (Ptr{arb}, Ptr{arb}))
  for (f,t) in (("arb_set_si", Int), ("arb_set_ui", UInt),
                ("arb_set_d", Float64))
    @eval begin
      function _arb_set(x::($typeofx), y::($t))
        ccall(($f, libarb), Nothing, (($passtoc), ($t)), x, y)
      end

      function _arb_set(x::($typeofx), y::($t), p::Int)
        _arb_set(x, y)
        ccall((:arb_set_round, libarb), Nothing,
                    (($passtoc), ($passtoc), Int), x, x, p)
      end
    end
  end

  @eval begin
    function _arb_set(x::($typeofx), y::fmpz)
      ccall((:arb_set_fmpz, libarb), Nothing, (($passtoc), Ref{fmpz}), x, y)
    end

    function _arb_set(x::($typeofx), y::fmpz, p::Int)
      ccall((:arb_set_round_fmpz, libarb), Nothing,
                  (($passtoc), Ref{fmpz}, Int), x, y, p)
    end

    function _arb_set(x::($typeofx), y::fmpq, p::Int)
      ccall((:arb_set_fmpq, libarb), Nothing,
                  (($passtoc), Ref{fmpq}, Int), x, y, p)
    end

    function _arb_set(x::($typeofx), y::arb)
      ccall((:arb_set, libarb), Nothing, (($passtoc), Ref{arb}), x, y)
    end

    function _arb_set(x::($typeofx), y::arb, p::Int)
      ccall((:arb_set_round, libarb), Nothing,
                  (($passtoc), Ref{arb}, Int), x, y, p)
    end

    function _arb_set(x::($typeofx), y::AbstractString, p::Int)
      s = string(y)
      err = ccall((:arb_set_str, libarb), Int32,
                  (($passtoc), Ptr{UInt8}, Int), x, s, p)
      err == 0 || error("Invalid real string: $(repr(s))")
    end

    function _arb_set(x::($typeofx), y::BigFloat)
      m = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct},
                  (($passtoc), ), x)
      r = ccall((:arb_rad_ptr, libarb), Ptr{mag_struct},
                  (($passtoc), ), x)
      ccall((:arf_set_mpfr, libarb), Nothing,
                  (Ptr{arf_struct}, Ref{BigFloat}), m, y)
      ccall((:mag_zero, libarb), Nothing, (Ptr{mag_struct}, ), r)
    end

    function _arb_set(x::($typeofx), y::BigFloat, p::Int)
      m = ccall((:arb_mid_ptr, libarb), Ptr{arf_struct}, (($passtoc), ), x)
      r = ccall((:arb_rad_ptr, libarb), Ptr{mag_struct}, (($passtoc), ), x)
      ccall((:arf_set_mpfr, libarb), Nothing,
                  (Ptr{arf_struct}, Ref{BigFloat}), m, y)
      ccall((:mag_zero, libarb), Nothing, (Ptr{mag_struct}, ), r)
      ccall((:arb_set_round, libarb), Nothing,
                  (($passtoc), ($passtoc), Int), x, x, p)
    end
  end
end

################################################################################
#
#  Parent object overloading
#
################################################################################

function (r::ArbField)()
  z = arb()
  z.parent = r
  return z
end

function (r::ArbField)(x::Int)
  z = arb(fmpz(x), r.prec)
  z.parent = r
  return z
end

function (r::ArbField)(x::UInt)
  z = arb(fmpz(x), r.prec)
  z.parent = r
  return z
end

function (r::ArbField)(x::fmpz)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

(r::ArbField)(x::Integer) = r(fmpz(x))

function (r::ArbField)(x::fmpq)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

(r::ArbField)(x::Rational{T}) where {T <: Integer} = r(fmpq(x))

#function call(r::ArbField, x::arf)
#  z = arb(arb(x), r.prec)
#  z.parent = r
#  return z
#end

function (r::ArbField)(x::Float64)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

function (r::ArbField)(x::arb)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

function (r::ArbField)(x::AbstractString)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

function (r::ArbField)(x::Irrational)
  if x == pi
    return const_pi(r)
  elseif x == e
    return const_e(r.prec)
  else
    error("constant not supported")
  end
end

function (r::ArbField)(x::BigFloat)
  z = arb(x, r.prec)
  z.parent = r
  return z
end

################################################################################
#
#  Arb real field constructor
#
################################################################################

# see inner constructor for ArbField
