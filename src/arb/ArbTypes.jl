###############################################################################
#
#   ArbTypes.jl : Parent and object types for Arb
#
#   Copyright (C) 2015 Tommy Hofmann
#   Copyright (C) 2015 Fredrik Johansson
#
###############################################################################

arb_check_precision(p::Int) = (p >= 2 && p < (typemax(Int) >> 4)) || throw(ArgumentError("invalid precision"))

# Rounding modes

const ARB_RND_DOWN = Cint(0)   # towards zero
const ARB_RND_UP = Cint(1)     # away from zero
const ARB_RND_FLOOR = Cint(2)  # towards -infinity
const ARB_RND_CEIL = Cint(3)   # towards +infinity
const ARB_RND_NEAR = Cint(4)   # to nearest

################################################################################
#
#  Structs for shallow operations
#
################################################################################

mutable struct arf_struct
  exp::Int    # ZZRingElem
  size::UInt  # mp_size_t
  d1::UInt    # mantissa_struct
  d2::UInt

  function arf_struct(exp, size, d1, d2)
    new(exp, size, d1, d2)
  end

  function arf_struct()
    z = new()
    ccall((:arf_init, libarb), Nothing, (Ref{arf_struct}, ), z)
    finalizer(_arf_clear_fn, z)
    return z
  end
end

function _arf_clear_fn(x::arf_struct)
  ccall((:arf_clear, libarb), Nothing, (Ref{arf_struct}, ), x)
end

mutable struct mag_struct
  exp::Int  # ZZRingElem
  man::UInt # mp_limb_t
end

mutable struct arb_struct
  mid_exp::Int # ZZRingElem
  mid_size::UInt # mp_size_t
  mid_d1::UInt # mantissa_struct
  mid_d2::UInt
  rad_exp::Int # ZZRingElem
  rad_man::UInt
end

mutable struct acb_struct
  real_mid_exp::Int # ZZRingElem
  real_mid_size::UInt # mp_size_t
  real_mid_d1::Int # mantissa_struct
  real_mid_d2::Int
  real_rad_exp::Int # ZZRingElem
  real_rad_man::UInt
  imag_mid_exp::Int # ZZRingElem
  imag_mid_size::UInt # mp_size_t
  imag_mid_d1::Int # mantissa_struct
  imag_mid_d2::Int
  imag_rad_exp::Int # ZZRingElem
  imag_rad_man::UInt
end

################################################################################
#
#  Types and memory management for ArbField
#
################################################################################

struct RealField <: Field
end

mutable struct RealFieldElem <: FieldElem
  mid_exp::Int    # ZZRingElem
  mid_size::UInt  # mp_size_t
  mid_d1::UInt    # mantissa_struct
  mid_d2::UInt
  rad_exp::Int    # ZZRingElem
  rad_man::UInt

  function RealFieldElem()
    z = new()
    ccall((:arb_init, libarb), Nothing, (Ref{RealFieldElem}, ), z)
    finalizer(_arb_clear_fn, z)
    return z
  end

  function RealFieldElem(x::Union{Real, ZZRingElem, QQFieldElem, AbstractString, RealFieldElem}, p::Int)
    z = new()
    ccall((:arb_init, libarb), Nothing, (Ref{RealFieldElem}, ), z)
    _arb_set(z, x, p)
    finalizer(_arb_clear_fn, z)
    return z
  end

  function RealFieldElem(x::Union{Real, ZZRingElem})
    z = new()
    ccall((:arb_init, libarb), Nothing, (Ref{RealFieldElem}, ), z)
    _arb_set(z, x)
    finalizer(_arb_clear_fn, z)
    return z
  end

  function RealFieldElem(mid::RealFieldElem, rad::RealFieldElem)
    z = new()
    ccall((:arb_init, libarb), Nothing, (Ref{RealFieldElem}, ), z)
    ccall((:arb_set, libarb), Nothing, (Ref{RealFieldElem}, Ref{RealFieldElem}), z, mid)
    ccall((:arb_add_error, libarb), Nothing, (Ref{RealFieldElem}, Ref{RealFieldElem}), z, rad)
    finalizer(_arb_clear_fn, z)
    return z
  end

end

function _arb_clear_fn(x::RealFieldElem)
  ccall((:arb_clear, libarb), Nothing, (Ref{RealFieldElem}, ), x)
end

# fixed precision

@attributes mutable struct ArbField <: Field
  prec::Int

  function ArbField(p::Int = 256; cached::Bool = true)
    arb_check_precision(p)
    return get_cached!(ArbFieldID, p, cached) do
      return new(p)
    end
  end
end

const ArbFieldID = CacheDictType{Int, ArbField}()

precision(x::ArbField) = x.prec

mutable struct ArbFieldElem <: FieldElem
  mid_exp::Int # ZZRingElem
  mid_size::UInt # mp_size_t
  mid_d1::UInt # mantissa_struct
  mid_d2::UInt
  rad_exp::Int # ZZRingElem
  rad_man::UInt
  parent::ArbField

  function ArbFieldElem()
    z = new()
    ccall((:arb_init, libarb), Nothing, (Ref{ArbFieldElem}, ), z)
    finalizer(_arb_clear_fn, z)
    return z
  end

  function ArbFieldElem(x::Union{Real, ZZRingElem, QQFieldElem, AbstractString, ArbFieldElem}, p::Int)
    z = new()
    ccall((:arb_init, libarb), Nothing, (Ref{ArbFieldElem}, ), z)
    _arb_set(z, x, p)
    finalizer(_arb_clear_fn, z)
    return z
  end

  function ArbFieldElem(x::Union{Real, ZZRingElem, ArbFieldElem})
    z = new()
    ccall((:arb_init, libarb), Nothing, (Ref{ArbFieldElem}, ), z)
    _arb_set(z, x)
    finalizer(_arb_clear_fn, z)
    return z
  end

  function ArbFieldElem(mid::ArbFieldElem, rad::ArbFieldElem)
    z = new()
    ccall((:arb_init, libarb), Nothing, (Ref{ArbFieldElem}, ), z)
    ccall((:arb_set, libarb), Nothing, (Ref{ArbFieldElem}, Ref{ArbFieldElem}), z, mid)
    ccall((:arb_add_error, libarb), Nothing, (Ref{ArbFieldElem}, Ref{ArbFieldElem}), z, rad)
    finalizer(_arb_clear_fn, z)
    return z
  end

  #function ArbFieldElem(x::arf)
  #  z = new()
  #  ccall((:arb_init, libarb), Nothing, (Ref{ArbFieldElem}, ), z)
  #  ccall((:arb_set_arf, libarb), Nothing, (Ref{ArbFieldElem}, Ptr{arf}), z, x)
  #  finalizer(_arb_clear_fn, z)
  #  return z
  #end
end

function _arb_clear_fn(x::ArbFieldElem)
  ccall((:arb_clear, libarb), Nothing, (Ref{ArbFieldElem}, ), x)
end


################################################################################
#
#  Types and memory management for AcbField
#
################################################################################

struct ComplexField <: Field
end

mutable struct ComplexFieldElem <: FieldElem
  real_mid_exp::Int     # ZZRingElem
  real_mid_size::UInt   # mp_size_t
  real_mid_d1::UInt     # mantissa_struct
  real_mid_d2::UInt
  real_rad_exp::Int     # ZZRingElem
  real_rad_man::UInt
  imag_mid_exp::Int     # ZZRingElem
  imag_mid_size::UInt   # mp_size_t
  imag_mid_d1::UInt     # mantissa_struct
  imag_mid_d2::UInt
  imag_rad_exp::Int     # ZZRingElem
  imag_rad_man::UInt

  function ComplexFieldElem()
    z = new()
    ccall((:acb_init, libarb), Nothing, (Ref{ComplexFieldElem}, ), z)
    finalizer(_acb_clear_fn, z)
    return z
  end

  function ComplexFieldElem(x::Union{Number, ZZRingElem, RealFieldElem, ComplexFieldElem})
    z = new()
    ccall((:acb_init, libarb), Nothing, (Ref{ComplexFieldElem}, ), z)
    _acb_set(z, x)
    finalizer(_acb_clear_fn, z)
    return z
  end

  function ComplexFieldElem(x::Union{Number, ZZRingElem, QQFieldElem, RealFieldElem, ComplexFieldElem, AbstractString}, p::Int)
    z = new()
    ccall((:acb_init, libarb), Nothing, (Ref{ComplexFieldElem}, ), z)
    _acb_set(z, x, p)
    finalizer(_acb_clear_fn, z)
    return z
  end

  function ComplexFieldElem(x::T, y::T, p::Int) where {T <: Union{Real, ZZRingElem, QQFieldElem, AbstractString, RealFieldElem}}
    z = new()
    ccall((:acb_init, libarb), Nothing, (Ref{ComplexFieldElem}, ), z)
    _acb_set(z, x, y, p)
    finalizer(_acb_clear_fn, z)
    return z
  end
end

function _acb_clear_fn(x::ComplexFieldElem)
  ccall((:acb_clear, libarb), Nothing, (Ref{ComplexFieldElem}, ), x)
end

################################################################################
#
#  Precision management
#
################################################################################

struct Balls
end

# ArbFieldElem as in arblib
const ARB_DEFAULT_PRECISION = Ref{Int}(64)

@doc raw"""
    set_precision!(::Type{Balls}, n::Int)

Set the precision for all ball arithmetic to be `n`.

# Examples

```julia
julia> const_pi(RealField())
[3.141592653589793239 +/- 5.96e-19]

julia> set_precision!(Balls, 200); const_pi(RealField())
[3.14159265358979323846264338327950288419716939937510582097494 +/- 5.73e-60]
```
"""
function set_precision!(::Type{Balls}, n::Int)
  arb_check_precision(n)
  ARB_DEFAULT_PRECISION[] = n
end

@doc raw"""
    precision(::Type{Balls})

Return the precision for ball arithmetic.

# Examples

```julia
julia> set_precision!(Balls, 200); precision(Balls)
200
```
"""
function Base.precision(::Type{Balls})
  return ARB_DEFAULT_PRECISION[]
end

@doc raw"""
    set_precision!(f, ::Type{Balls}, n::Int)

Change ball arithmetic precision to `n` for the duration of `f`..

# Examples

```jldoctest
julia> set_precision!(Balls, 4) do
         const_pi(RealField())
       end
[3e+0 +/- 0.376]

julia> set_precision!(Balls, 200) do
         const_pi(RealField())
       end
[3.1415926535897932385 +/- 3.74e-20]
```
"""
function set_precision!(f, ::Type{Balls}, prec::Int)
  arb_check_precision(prec)
  old = precision(Balls)
  set_precision!(Balls, prec)
  x = f()
  set_precision!(Balls, old)
  return x
end

for T in [RealField, ComplexField]
  @eval begin
    precision(::$T) = precision(Balls)
    precision(::Type{$T}) = precision(Balls)

    set_precision!(::$T, n) = set_precision!(Balls, n)
    set_precision!(::Type{$T}, n) = set_precision!(Balls, n)

    set_precision!(f, ::$T, n) = set_precision!(f, Balls, n)
    set_precision!(f, ::Type{$T}, n) = set_precision!(f, Balls, n)
  end
end

# fixed precision

@attributes mutable struct AcbField <: Field
  prec::Int

  function AcbField(p::Int = 256; cached::Bool = true)
    arb_check_precision(p)
    return get_cached!(AcbFieldID, p, cached) do
      return new(p)
    end
  end
end

const AcbFieldID = CacheDictType{Int, AcbField}()

precision(x::AcbField) = x.prec

mutable struct AcbFieldElem <: FieldElem
  real_mid_exp::Int     # ZZRingElem
  real_mid_size::UInt # mp_size_t
  real_mid_d1::UInt    # mantissa_struct
  real_mid_d2::UInt
  real_rad_exp::Int     # ZZRingElem
  real_rad_man::UInt
  imag_mid_exp::Int     # ZZRingElem
  imag_mid_size::UInt # mp_size_t
  imag_mid_d1::UInt    # mantissa_struct
  imag_mid_d2::UInt
  imag_rad_exp::Int     # ZZRingElem
  imag_rad_man::UInt
  parent::AcbField

  function AcbFieldElem()
    z = new()
    ccall((:acb_init, libarb), Nothing, (Ref{AcbFieldElem}, ), z)
    finalizer(_acb_clear_fn, z)
    return z
  end

  function AcbFieldElem(x::Union{Number, ZZRingElem, ArbFieldElem, AcbFieldElem})
    z = new()
    ccall((:acb_init, libarb), Nothing, (Ref{AcbFieldElem}, ), z)
    _acb_set(z, x)
    finalizer(_acb_clear_fn, z)
    return z
  end

  function AcbFieldElem(x::Union{Number, ZZRingElem, QQFieldElem, ArbFieldElem, AcbFieldElem, AbstractString}, p::Int)
    z = new()
    ccall((:acb_init, libarb), Nothing, (Ref{AcbFieldElem}, ), z)
    _acb_set(z, x, p)
    finalizer(_acb_clear_fn, z)
    return z
  end

  #function AcbFieldElem{T <: Union{Int, UInt, Float64, ZZRingElem, BigFloat, ArbFieldElem}}(x::T, y::T)
  #  z = new()
  #  ccall((:acb_init, libarb), Nothing, (Ref{AcbFieldElem}, ), z)
  #  _acb_set(z, x, y)
  #  finalizer(_acb_clear_fn, z)
  #  return z
  #end

  function AcbFieldElem(x::T, y::T, p::Int) where {T <: Union{Real, ZZRingElem, QQFieldElem, AbstractString, ArbFieldElem}}
    z = new()
    ccall((:acb_init, libarb), Nothing, (Ref{AcbFieldElem}, ), z)
    _acb_set(z, x, y, p)
    finalizer(_acb_clear_fn, z)
    return z
  end
end

function _acb_clear_fn(x::AcbFieldElem)
  ccall((:acb_clear, libarb), Nothing, (Ref{AcbFieldElem}, ), x)
end

################################################################################
#
#  Integration things
#
################################################################################

mutable struct acb_calc_integrate_opts
  deg_limit::Int   # <= 0: default of 0.5*min(prec, rel_goal) + 10
  eval_limit::Int  # <= 0: default of 1000*prec*prec^2
  depth_limit::Int # <= 0: default of 2*prec
  use_heap::Int32  # 0 append to the top of a stack; 1 binary heap
  verbose::Int32   # 1 less verbose; 2 more verbose

  function acb_calc_integrate_opts(deg_limit::Int, eval_limit::Int,
    depth_limit::Int, use_heap::Int32, verbose::Int32)
    return new(deg_limit, eval_limit, depth_limit, use_heap, verbose)
  end

  function acb_calc_integrate_opts()
    opts = new()
    ccall((:acb_calc_integrate_opt_init, libarb),
      Nothing, (Ref{acb_calc_integrate_opts}, ), opts)
    return opts
  end
end

################################################################################
#
#  Types and memory management for ArbPolyRing
#
################################################################################

@attributes mutable struct RealPolyRing <: PolyRing{RealFieldElem}
  S::Symbol

  function RealPolyRing(R::RealField, S::Symbol, cached::Bool = true)
    return get_cached!(RealPolyRingID, (S, ), cached) do
      return new(S)
    end
  end
end

const RealPolyRingID = CacheDictType{Tuple{Symbol}, RealPolyRing}()

mutable struct RealPoly <: PolyRingElem{RealFieldElem}
  coeffs::Ptr{Nothing}
  length::Int
  alloc::Int
  parent::RealPolyRing

  function RealPoly()
    z = new()
    ccall((:arb_poly_init, libarb), Nothing, (Ref{RealPoly}, ), z)
    finalizer(_RealPoly_clear_fn, z)
    return z
  end

  function RealPoly(x::RealFieldElem, p::Int)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{RealPoly}, ), z)
    ccall((:arb_poly_set_coeff_arb, libarb), Nothing,
                (Ref{RealPoly}, Int, Ref{RealFieldElem}), z, 0, x)
    finalizer(_RealPoly_clear_fn, z)
    return z
  end

  function RealPoly(x::Vector{RealFieldElem}, p::Int)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{RealPoly}, ), z)
    for i = 1:length(x)
        ccall((:arb_poly_set_coeff_arb, libarb), Nothing,
                (Ref{RealPoly}, Int, Ref{RealFieldElem}), z, i - 1, x[i])
    end
    finalizer(_RealPoly_clear_fn, z)
    return z
  end

  function RealPoly(x::RealPoly)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{RealPoly}, ), z)
    ccall((:arb_poly_set, libarb), Nothing, (Ref{RealPoly}, Ref{RealPoly}), z, x)
    finalizer(_RealPoly_clear_fn, z)
    return z
  end

  function RealPoly(x::RealPoly, p::Int)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{RealPoly}, ), z)
    ccall((:arb_poly_set_round, libarb), Nothing,
                (Ref{RealPoly}, Ref{RealPoly}, Int), z, x, p)
    finalizer(_RealPoly_clear_fn, z)
    return z
  end

  function RealPoly(x::ZZPolyRingElem, p::Int)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{RealPoly}, ), z)
    ccall((:arb_poly_set_fmpz_poly, libarb), Nothing,
                (Ref{RealPoly}, Ref{ZZPolyRingElem}, Int), z, x, p)
    finalizer(_RealPoly_clear_fn, z)
    return z
  end

  function RealPoly(x::QQPolyRingElem, p::Int)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{RealPoly}, ), z)
    ccall((:arb_poly_set_fmpq_poly, libarb), Nothing,
                (Ref{RealPoly}, Ref{QQPolyRingElem}, Int), z, x, p)
    finalizer(_RealPoly_clear_fn, z)
    return z
  end
end

function _RealPoly_clear_fn(x::RealPoly)
  ccall((:arb_poly_clear, libarb), Nothing, (Ref{RealPoly}, ), x)
end

parent(x::RealPoly) = x.parent

var(x::RealPolyRing) = x.S

base_ring(a::RealPolyRing) = RealField()

# fixed precision

@attributes mutable struct ArbPolyRing <: PolyRing{ArbFieldElem}
  base_ring::ArbField
  S::Symbol

  function ArbPolyRing(R::ArbField, S::Symbol, cached::Bool = true)
    return get_cached!(ArbPolyRingID, (R, S), cached) do
      return new(R, S)
    end
  end
end

const ArbPolyRingID = CacheDictType{Tuple{ArbField, Symbol}, ArbPolyRing}()

mutable struct ArbPolyRingElem <: PolyRingElem{ArbFieldElem}
  coeffs::Ptr{Nothing}
  length::Int
  alloc::Int
  parent::ArbPolyRing

  function ArbPolyRingElem()
    z = new()
    ccall((:arb_poly_init, libarb), Nothing, (Ref{ArbPolyRingElem}, ), z)
    finalizer(_arb_poly_clear_fn, z)
    return z
  end

  function ArbPolyRingElem(x::ArbFieldElem, p::Int)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{ArbPolyRingElem}, ), z)
    ccall((:arb_poly_set_coeff_arb, libarb), Nothing,
                (Ref{ArbPolyRingElem}, Int, Ref{ArbFieldElem}), z, 0, x)
    finalizer(_arb_poly_clear_fn, z)
    return z
  end

  function ArbPolyRingElem(x::Vector{ArbFieldElem}, p::Int)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{ArbPolyRingElem}, ), z)
    for i = 1:length(x)
        ccall((:arb_poly_set_coeff_arb, libarb), Nothing,
                (Ref{ArbPolyRingElem}, Int, Ref{ArbFieldElem}), z, i - 1, x[i])
    end
    finalizer(_arb_poly_clear_fn, z)
    return z
  end

  function ArbPolyRingElem(x::ArbPolyRingElem)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{ArbPolyRingElem}, ), z)
    ccall((:arb_poly_set, libarb), Nothing, (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}), z, x)
    finalizer(_arb_poly_clear_fn, z)
    return z
  end

  function ArbPolyRingElem(x::ArbPolyRingElem, p::Int)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{ArbPolyRingElem}, ), z)
    ccall((:arb_poly_set_round, libarb), Nothing,
                (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int), z, x, p)
    finalizer(_arb_poly_clear_fn, z)
    return z
  end

  function ArbPolyRingElem(x::ZZPolyRingElem, p::Int)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{ArbPolyRingElem}, ), z)
    ccall((:arb_poly_set_fmpz_poly, libarb), Nothing,
                (Ref{ArbPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, x, p)
    finalizer(_arb_poly_clear_fn, z)
    return z
  end

  function ArbPolyRingElem(x::QQPolyRingElem, p::Int)
    z = new() 
    ccall((:arb_poly_init, libarb), Nothing, (Ref{ArbPolyRingElem}, ), z)
    ccall((:arb_poly_set_fmpq_poly, libarb), Nothing,
                (Ref{ArbPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, p)
    finalizer(_arb_poly_clear_fn, z)
    return z
  end
end

function _arb_poly_clear_fn(x::ArbPolyRingElem)
  ccall((:arb_poly_clear, libarb), Nothing, (Ref{ArbPolyRingElem}, ), x)
end

parent(x::ArbPolyRingElem) = x.parent

var(x::ArbPolyRing) = x.S

precision(x::ArbPolyRing) = precision(x.base_ring)

base_ring(a::ArbPolyRing) = a.base_ring

################################################################################
#
#  Types and memory management for AcbPolyRing
#
################################################################################

@attributes mutable struct ComplexPolyRing <: PolyRing{ComplexFieldElem}
  S::Symbol

  function ComplexPolyRing(R::ComplexField, S::Symbol, cached::Bool = true)
    return get_cached!(ComplexPolyRingID, (S, ), cached) do
      return new(S)
    end
  end
end

const ComplexPolyRingID = CacheDictType{Tuple{Symbol}, ComplexPolyRing}()

mutable struct ComplexPoly <: PolyRingElem{ComplexFieldElem}
  coeffs::Ptr{Nothing}
  length::Int
  alloc::Int
  parent::ComplexPolyRing

  function ComplexPoly()
    z = new()
    ccall((:acb_poly_init, libarb), Nothing, (Ref{ComplexPoly}, ), z)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function ComplexPoly(x::ComplexFieldElem, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{ComplexPoly}, ), z)
    ccall((:acb_poly_set_coeff_acb, libarb), Nothing,
                (Ref{ComplexPoly}, Int, Ref{ComplexFieldElem}), z, 0, x)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function ComplexPoly(x::Vector{ComplexFieldElem}, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{ComplexPoly}, ), z)
    for i = 1:length(x)
        ccall((:acb_poly_set_coeff_acb, libarb), Nothing,
                (Ref{ComplexPoly}, Int, Ref{ComplexFieldElem}), z, i - 1, x[i])
    end
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function ComplexPoly(x::ComplexPoly)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{ComplexPoly}, ), z)
    ccall((:acb_poly_set, libarb), Nothing, (Ref{ComplexPoly}, Ref{ComplexPoly}), z, x)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function ComplexPoly(x::RealPoly, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{ComplexPoly}, ), z)
    ccall((:acb_poly_set_arb_poly, libarb), Nothing,
                (Ref{ComplexPoly}, Ref{ArbPolyRingElem}, Int), z, x, p)
    ccall((:acb_poly_set_round, libarb), Nothing,
                (Ref{ComplexPoly}, Ref{ComplexPoly}, Int), z, z, p)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function ComplexPoly(x::ComplexPoly, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{ComplexPoly}, ), z)
    ccall((:acb_poly_set_round, libarb), Nothing,
                (Ref{ComplexPoly}, Ref{ComplexPoly}, Int), z, x, p)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function ComplexPoly(x::ZZPolyRingElem, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{ComplexPoly}, ), z)
    ccall((:acb_poly_set_fmpz_poly, libarb), Nothing,
                (Ref{ComplexPoly}, Ref{ZZPolyRingElem}, Int), z, x, p)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function ComplexPoly(x::QQPolyRingElem, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{ComplexPoly}, ), z)
    ccall((:acb_poly_set_fmpq_poly, libarb), Nothing,
                (Ref{ComplexPoly}, Ref{QQPolyRingElem}, Int), z, x, p)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end
end

function _acb_poly_clear_fn(x::ComplexPoly)
  ccall((:acb_poly_clear, libarb), Nothing, (Ref{ComplexPoly}, ), x)
end

parent(x::ComplexPoly) = x.parent

var(x::ComplexPolyRing) = x.S

base_ring(a::ComplexPolyRing) = ComplexField()

# fixed precision

@attributes mutable struct AcbPolyRing <: PolyRing{AcbFieldElem}
  base_ring::AcbField
  S::Symbol

  function AcbPolyRing(R::AcbField, S::Symbol, cached::Bool = true)
    return get_cached!(AcbPolyRingID, (R, S), cached) do
      return new(R, S)
    end
  end
end

const AcbPolyRingID = CacheDictType{Tuple{AcbField, Symbol}, AcbPolyRing}()

mutable struct AcbPolyRingElem <: PolyRingElem{AcbFieldElem}
  coeffs::Ptr{Nothing}
  length::Int
  alloc::Int
  parent::AcbPolyRing

  function AcbPolyRingElem()
    z = new()
    ccall((:acb_poly_init, libarb), Nothing, (Ref{AcbPolyRingElem}, ), z)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function AcbPolyRingElem(x::AcbFieldElem, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{AcbPolyRingElem}, ), z)
    ccall((:acb_poly_set_coeff_acb, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Int, Ref{AcbFieldElem}), z, 0, x)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function AcbPolyRingElem(x::Vector{AcbFieldElem}, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{AcbPolyRingElem}, ), z)
    for i = 1:length(x)
        ccall((:acb_poly_set_coeff_acb, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Int, Ref{AcbFieldElem}), z, i - 1, x[i])
    end
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function AcbPolyRingElem(x::AcbPolyRingElem)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{AcbPolyRingElem}, ), z)
    ccall((:acb_poly_set, libarb), Nothing, (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}), z, x)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function AcbPolyRingElem(x::ArbPolyRingElem, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{AcbPolyRingElem}, ), z)
    ccall((:acb_poly_set_arb_poly, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ref{ArbPolyRingElem}, Int), z, x, p)
    ccall((:acb_poly_set_round, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int), z, z, p)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function AcbPolyRingElem(x::AcbPolyRingElem, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{AcbPolyRingElem}, ), z)
    ccall((:acb_poly_set_round, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int), z, x, p)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function AcbPolyRingElem(x::ZZPolyRingElem, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{AcbPolyRingElem}, ), z)
    ccall((:acb_poly_set_fmpz_poly, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, x, p)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function AcbPolyRingElem(x::QQPolyRingElem, p::Int)
    z = new() 
    ccall((:acb_poly_init, libarb), Nothing, (Ref{AcbPolyRingElem}, ), z)
    ccall((:acb_poly_set_fmpq_poly, libarb), Nothing,
                (Ref{AcbPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, p)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end
end

function _acb_poly_clear_fn(x::AcbPolyRingElem)
  ccall((:acb_poly_clear, libarb), Nothing, (Ref{AcbPolyRingElem}, ), x)
end

parent(x::AcbPolyRingElem) = x.parent

var(x::AcbPolyRing) = x.S

precision(x::AcbPolyRing) = precision(x.base_ring)

base_ring(a::AcbPolyRing) = a.base_ring



################################################################################
#
#  Types and memory management for ArbMatSpace
#
################################################################################

struct RealMatSpace <: MatSpace{RealFieldElem}
  nrows::Int
  ncols::Int

  function RealMatSpace(R::RealField, r::Int, c::Int)
    (r < 0 || c < 0) && throw(_err_dim_negative)
    return new(r, c)
  end
end

const RealMatSpaceID = CacheDictType{Tuple{Int, Int}, RealMatSpace}()

mutable struct RealMat <: MatElem{RealFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  #base_ring::ArbField

  function RealMat(r::Int, c::Int)
    z = new()
    ccall((:arb_mat_init, libarb), Nothing, (Ref{RealMat}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    return z
  end

  function RealMat(a::ZZMatrix)
    z = new()
    ccall((:arb_mat_init, libarb), Nothing,
                (Ref{RealMat}, Int, Int), z, a.r, a.c)
    ccall((:arb_mat_set_fmpz_mat, libarb), Nothing,
                (Ref{RealMat}, Ref{ZZMatrix}), z, a)
    finalizer(_arb_mat_clear_fn, z)
    return z
  end
  
  function RealMat(a::ZZMatrix, prec::Int)
    z = new()
    ccall((:arb_mat_init, libarb), Nothing,
                (Ref{RealMat}, Int, Int), z, a.r, a.c)
    ccall((:arb_mat_set_round_fmpz_mat, libarb), Nothing,
                (Ref{RealMat}, Ref{ZZMatrix}, Int), z, a, prec)
    finalizer(_arb_mat_clear_fn, z)
    return z
  end

  function RealMat(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, ZZRingElem, Float64, BigFloat, RealFieldElem}}
    z = new()
    ccall((:arb_mat_init, libarb), Nothing, 
                (Ref{RealMat}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, libarb), Ptr{RealFieldElem},
                    (Ref{RealMat}, Int, Int), z, i - 1, j - 1)
        Nemo._arb_set(el, arr[i, j])
      end
    end
    return z
  end

  function RealMat(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, ZZRingElem, Float64, BigFloat, RealFieldElem}}
    z = new()
    ccall((:arb_mat_init, libarb), Nothing, 
                (Ref{RealMat}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, libarb), Ptr{RealFieldElem},
                    (Ref{RealMat}, Int, Int), z, i - 1, j - 1)
        Nemo._arb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function RealMat(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, AbstractString}}
    z = new()
    ccall((:arb_mat_init, libarb), Nothing, 
                (Ref{RealMat}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, libarb), Ptr{RealFieldElem},
                    (Ref{RealMat}, Int, Int), z, i - 1, j - 1)
        _arb_set(el, arr[i, j], prec)
      end
    end
    return z
  end
     
  function RealMat(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, AbstractString}}
    z = new()
    ccall((:arb_mat_init, libarb), Nothing, 
                (Ref{RealMat}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, libarb), Ptr{RealFieldElem},
                    (Ref{RealMat}, Int, Int), z, i - 1, j - 1)
        _arb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function RealMat(a::QQMatrix, prec::Int)
    z = new()
    ccall((:arb_mat_init, libarb), Nothing,
                (Ref{RealMat}, Int, Int), z, a.r, a.c)
    ccall((:arb_mat_set_fmpq_mat, libarb), Nothing,
                (Ref{RealMat}, Ref{QQMatrix}, Int), z, a, prec)
    finalizer(_arb_mat_clear_fn, z)
    return z
  end
end

function _arb_mat_clear_fn(x::RealMat)
  ccall((:arb_mat_clear, libarb), Nothing, (Ref{RealMat}, ), x)
end

# fixed precision

struct ArbMatSpace <: MatSpace{ArbFieldElem}
  nrows::Int
  ncols::Int
  base_ring::ArbField

  function ArbMatSpace(R::ArbField, r::Int, c::Int)
    (r < 0 || c < 0) && throw(_err_dim_negative)
    return new(r, c, R)
  end
end

const ArbMatSpaceID = CacheDictType{Tuple{ArbField, Int, Int}, ArbMatSpace}()

mutable struct ArbMatrix <: MatElem{ArbFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  base_ring::ArbField

  function ArbMatrix(r::Int, c::Int)
    z = new()
    ccall((:arb_mat_init, libarb), Nothing, (Ref{ArbMatrix}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    return z
  end

  function ArbMatrix(a::ZZMatrix)
    z = new()
    ccall((:arb_mat_init, libarb), Nothing,
                (Ref{ArbMatrix}, Int, Int), z, a.r, a.c)
    ccall((:arb_mat_set_fmpz_mat, libarb), Nothing,
                (Ref{ArbMatrix}, Ref{ZZMatrix}), z, a)
    finalizer(_arb_mat_clear_fn, z)
    return z
  end
  
  function ArbMatrix(a::ZZMatrix, prec::Int)
    z = new()
    ccall((:arb_mat_init, libarb), Nothing,
                (Ref{ArbMatrix}, Int, Int), z, a.r, a.c)
    ccall((:arb_mat_set_round_fmpz_mat, libarb), Nothing,
                (Ref{ArbMatrix}, Ref{ZZMatrix}, Int), z, a, prec)
    finalizer(_arb_mat_clear_fn, z)
    return z
  end

  function ArbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, ZZRingElem, Float64, BigFloat, ArbFieldElem}}
    z = new()
    ccall((:arb_mat_init, libarb), Nothing, 
                (Ref{ArbMatrix}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, libarb), Ptr{ArbFieldElem},
                    (Ref{ArbMatrix}, Int, Int), z, i - 1, j - 1)
        Nemo._arb_set(el, arr[i, j])
      end
    end
    return z
  end

  function ArbMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, ZZRingElem, Float64, BigFloat, ArbFieldElem}}
    z = new()
    ccall((:arb_mat_init, libarb), Nothing, 
                (Ref{ArbMatrix}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, libarb), Ptr{ArbFieldElem},
                    (Ref{ArbMatrix}, Int, Int), z, i - 1, j - 1)
        Nemo._arb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function ArbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AbstractString}}
    z = new()
    ccall((:arb_mat_init, libarb), Nothing, 
                (Ref{ArbMatrix}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, libarb), Ptr{ArbFieldElem},
                    (Ref{ArbMatrix}, Int, Int), z, i - 1, j - 1)
        _arb_set(el, arr[i, j], prec)
      end
    end
    return z
  end
     
  function ArbMatrix(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AbstractString}}
    z = new()
    ccall((:arb_mat_init, libarb), Nothing, 
                (Ref{ArbMatrix}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, libarb), Ptr{ArbFieldElem},
                    (Ref{ArbMatrix}, Int, Int), z, i - 1, j - 1)
        _arb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function ArbMatrix(a::QQMatrix, prec::Int)
    z = new()
    ccall((:arb_mat_init, libarb), Nothing,
                (Ref{ArbMatrix}, Int, Int), z, a.r, a.c)
    ccall((:arb_mat_set_fmpq_mat, libarb), Nothing,
                (Ref{ArbMatrix}, Ref{QQMatrix}, Int), z, a, prec)
    finalizer(_arb_mat_clear_fn, z)
    return z
  end
end

function _arb_mat_clear_fn(x::ArbMatrix)
  ccall((:arb_mat_clear, libarb), Nothing, (Ref{ArbMatrix}, ), x)
end

################################################################################
#
#  Types and memory management for AcbMatSpace
#
################################################################################

struct ComplexMatSpace <: MatSpace{ComplexFieldElem}
  nrows::Int
  ncols::Int
  #base_ring::AcbField

  function ComplexMatSpace(R::ComplexField, r::Int, c::Int)
    (r < 0 || c < 0) && throw(_err_dim_negative)
    return new(r, c)
  end
end

const ComplexMatSpaceID = CacheDictType{Tuple{Int, Int}, ComplexMatSpace}()

mutable struct ComplexMat <: MatElem{ComplexFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  #base_ring::AcbField

  function ComplexMat(r::Int, c::Int)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end

  function ComplexMat(a::ZZMatrix)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing,
                (Ref{ComplexMat}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_fmpz_mat, libarb), Nothing,
                (Ref{ComplexMat}, Ref{ZZMatrix}), z, a)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end
  
  function ComplexMat(a::ZZMatrix, prec::Int)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing,
                (Ref{ComplexMat}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_round_fmpz_mat, libarb), Nothing,
                (Ref{ComplexMat}, Ref{ZZMatrix}, Int), z, a, prec)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end

  function ComplexMat(a::RealMat)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing,
                (Ref{ComplexMat}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_arb_mat, libarb), Nothing,
                (Ref{ComplexMat}, Ref{ArbMatrix}), z, a)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end

  function ComplexMat(a::ArbMatrix, prec::Int)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing,
                (Ref{ComplexMat}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_round_arb_mat, libarb), Nothing,
                (Ref{ComplexMat}, Ref{ArbMatrix}, Int), z, a, prec)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end
   
  function ComplexMat(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function ComplexMat(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{BigFloat, ComplexFieldElem, RealFieldElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function ComplexMat(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function ComplexMat(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{BigFloat, ComplexFieldElem, RealFieldElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function ComplexMat(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function ComplexMat(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{BigFloat, RealFieldElem, AbstractString, ComplexFieldElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{ComplexFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function ComplexMat(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{ComplexFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function ComplexMat(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{BigFloat, RealFieldElem, AbstractString, ComplexFieldElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{ComplexFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function ComplexMat(r::Int, c::Int, arr::AbstractMatrix{Tuple{T, T}}, prec::Int) where {T <: Union{Int, UInt, Float64, ZZRingElem}}

    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{ComplexFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function ComplexMat(r::Int, c::Int, arr::AbstractMatrix{Tuple{T, T}}, prec::Int) where {T <: Union{QQFieldElem, BigFloat, RealFieldElem, AbstractString}}

    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{ComplexFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function ComplexMat(r::Int, c::Int, arr::AbstractVector{Tuple{T, T}}, prec::Int) where {T <: Union{Int, UInt, Float64, ZZRingElem}}

    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{ComplexFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function ComplexMat(r::Int, c::Int, arr::AbstractVector{Tuple{T, T}}, prec::Int) where {T <: Union{QQFieldElem, BigFloat, RealFieldElem, AbstractString}}

    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{ComplexMat}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{ComplexFieldElem},
                    (Ref{ComplexMat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function ComplexMat(a::QQMatrix, prec::Int)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing,
                (Ref{ComplexMat}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_fmpq_mat, libarb), Nothing,
                (Ref{ComplexMat}, Ref{QQMatrix}, Int), z, a, prec)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end
end

function _acb_mat_clear_fn(x::ComplexMat)
  ccall((:acb_mat_clear, libarb), Nothing, (Ref{ComplexMat}, ), x)
end

# fixed precision

struct AcbMatSpace <: MatSpace{AcbFieldElem}
  nrows::Int
  ncols::Int
  base_ring::AcbField

  function AcbMatSpace(R::AcbField, r::Int, c::Int)
    (r < 0 || c < 0) && throw(_err_dim_negative)
    return new(r, c, R)
  end
end

const AcbMatSpaceID = CacheDictType{Tuple{AcbField, Int, Int}, AcbMatSpace}()

mutable struct AcbMatrix <: MatElem{AcbFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  base_ring::AcbField

  function AcbMatrix(r::Int, c::Int)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end

  function AcbMatrix(a::ZZMatrix)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing,
                (Ref{AcbMatrix}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_fmpz_mat, libarb), Nothing,
                (Ref{AcbMatrix}, Ref{ZZMatrix}), z, a)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end
  
  function AcbMatrix(a::ZZMatrix, prec::Int)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing,
                (Ref{AcbMatrix}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_round_fmpz_mat, libarb), Nothing,
                (Ref{AcbMatrix}, Ref{ZZMatrix}, Int), z, a, prec)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end

  function AcbMatrix(a::ArbMatrix)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing,
                (Ref{AcbMatrix}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_arb_mat, libarb), Nothing,
                (Ref{AcbMatrix}, Ref{ArbMatrix}), z, a)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end

  function AcbMatrix(a::ArbMatrix, prec::Int)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing,
                (Ref{AcbMatrix}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_round_arb_mat, libarb), Nothing,
                (Ref{AcbMatrix}, Ref{ArbMatrix}, Int), z, a, prec)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end
   
  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{BigFloat, AcbFieldElem, ArbFieldElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{BigFloat, AcbFieldElem, ArbFieldElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{BigFloat, ArbFieldElem, AbstractString, AcbFieldElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{BigFloat, ArbFieldElem, AbstractString, AcbFieldElem}}
    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{Tuple{T, T}}, prec::Int) where {T <: Union{Int, UInt, Float64, ZZRingElem}}

    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{Tuple{T, T}}, prec::Int) where {T <: Union{QQFieldElem, BigFloat, ArbFieldElem, AbstractString}}

    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{Tuple{T, T}}, prec::Int) where {T <: Union{Int, UInt, Float64, ZZRingElem}}

    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{Tuple{T, T}}, prec::Int) where {T <: Union{QQFieldElem, BigFloat, ArbFieldElem, AbstractString}}

    z = new()
    ccall((:acb_mat_init, libarb), Nothing, 
                (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, libarb), Ptr{AcbFieldElem},
                    (Ref{AcbMatrix}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function AcbMatrix(a::QQMatrix, prec::Int)
    z = new()
    ccall((:acb_mat_init, libarb), Nothing,
                (Ref{AcbMatrix}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_fmpq_mat, libarb), Nothing,
                (Ref{AcbMatrix}, Ref{QQMatrix}, Int), z, a, prec)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end
end

function _acb_mat_clear_fn(x::AcbMatrix)
  ccall((:acb_mat_clear, libarb), Nothing, (Ref{AcbMatrix}, ), x)
end

