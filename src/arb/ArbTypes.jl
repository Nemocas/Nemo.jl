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
    ccall((:arf_init, libflint), Nothing, (Ref{arf_struct}, ), z)
    finalizer(_arf_clear_fn, z)
    return z
  end
end

function _arf_clear_fn(x::arf_struct)
  ccall((:arf_clear, libflint), Nothing, (Ref{arf_struct}, ), x)
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
    ccall((:arb_init, libflint), Nothing, (Ref{RealFieldElem}, ), z)
    finalizer(_arb_clear_fn, z)
    return z
  end

  function RealFieldElem(x::Union{Real, ZZRingElem, QQFieldElem, AbstractString, RealFieldElem}, p::Int)
    z = RealFieldElem()
    _arb_set(z, x, p)
    return z
  end

  function RealFieldElem(x::Union{Real, ZZRingElem})
    z = RealFieldElem()
    _arb_set(z, x)
    return z
  end

  function RealFieldElem(mid::RealFieldElem, rad::RealFieldElem)
    z = RealFieldElem()
    ccall((:arb_set, libflint), Nothing, (Ref{RealFieldElem}, Ref{RealFieldElem}), z, mid)
    ccall((:arb_add_error, libflint), Nothing, (Ref{RealFieldElem}, Ref{RealFieldElem}), z, rad)
    return z
  end

end

function _arb_clear_fn(x::RealFieldElem)
  ccall((:arb_clear, libflint), Nothing, (Ref{RealFieldElem}, ), x)
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
    ccall((:arb_init, libflint), Nothing, (Ref{ArbFieldElem}, ), z)
    finalizer(_arb_clear_fn, z)
    return z
  end

  function ArbFieldElem(x::Union{Real, ZZRingElem, QQFieldElem, AbstractString, ArbFieldElem}, p::Int)
    z = ArbFieldElem()
    _arb_set(z, x, p)
    return z
  end

  function ArbFieldElem(x::Union{Real, ZZRingElem, ArbFieldElem})
    z = ArbFieldElem()
    _arb_set(z, x)
    return z
  end

  function ArbFieldElem(mid::ArbFieldElem, rad::ArbFieldElem)
    z = ArbFieldElem()
    ccall((:arb_set, libflint), Nothing, (Ref{ArbFieldElem}, Ref{ArbFieldElem}), z, mid)
    ccall((:arb_add_error, libflint), Nothing, (Ref{ArbFieldElem}, Ref{ArbFieldElem}), z, rad)
    return z
  end

  #function ArbFieldElem(x::arf)
  #  z = ArbFieldElem()
  #  ccall((:arb_set_arf, libflint), Nothing, (Ref{ArbFieldElem}, Ptr{arf}), z, x)
  #  return z
  #end
end

function _arb_clear_fn(x::ArbFieldElem)
  ccall((:arb_clear, libflint), Nothing, (Ref{ArbFieldElem}, ), x)
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
    ccall((:acb_init, libflint), Nothing, (Ref{ComplexFieldElem}, ), z)
    finalizer(_acb_clear_fn, z)
    return z
  end

  function ComplexFieldElem(x::Union{Number, ZZRingElem, RealFieldElem, ComplexFieldElem})
    z = ComplexFieldElem()
    _acb_set(z, x)
    return z
  end

  function ComplexFieldElem(x::Union{Number, ZZRingElem, QQFieldElem, RealFieldElem, ComplexFieldElem, AbstractString}, p::Int)
    z = ComplexFieldElem()
    _acb_set(z, x, p)
    return z
  end

  function ComplexFieldElem(x::T, y::T, p::Int) where {T <: Union{Real, ZZRingElem, QQFieldElem, AbstractString, RealFieldElem}}
    z = ComplexFieldElem()
    _acb_set(z, x, y, p)
    return z
  end
end

function _acb_clear_fn(x::ComplexFieldElem)
  ccall((:acb_clear, libflint), Nothing, (Ref{ComplexFieldElem}, ), x)
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
    ccall((:acb_init, libflint), Nothing, (Ref{AcbFieldElem}, ), z)
    finalizer(_acb_clear_fn, z)
    return z
  end

  function AcbFieldElem(x::Union{Number, ZZRingElem, ArbFieldElem, AcbFieldElem})
    z = AcbFieldElem()
    _acb_set(z, x)
    return z
  end

  function AcbFieldElem(x::Union{Number, ZZRingElem, QQFieldElem, ArbFieldElem, AcbFieldElem, AbstractString}, p::Int)
    z = AcbFieldElem()
    _acb_set(z, x, p)
    return z
  end

  #function AcbFieldElem{T <: Union{Int, UInt, Float64, ZZRingElem, BigFloat, ArbFieldElem}}(x::T, y::T)
  #  z = AcbFieldElem()
  #  _acb_set(z, x, y)
  #  return z
  #end

  function AcbFieldElem(x::T, y::T, p::Int) where {T <: Union{Real, ZZRingElem, QQFieldElem, AbstractString, ArbFieldElem}}
    z = AcbFieldElem()
    _acb_set(z, x, y, p)
    return z
  end
end

function _acb_clear_fn(x::AcbFieldElem)
  ccall((:acb_clear, libflint), Nothing, (Ref{AcbFieldElem}, ), x)
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
    ccall((:acb_calc_integrate_opt_init, libflint),
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

mutable struct RealPolyRingElem <: PolyRingElem{RealFieldElem}
  coeffs::Ptr{Nothing}
  length::Int
  alloc::Int
  parent::RealPolyRing

  function RealPolyRingElem()
    z = new()
    ccall((:arb_poly_init, libflint), Nothing, (Ref{RealPolyRingElem}, ), z)
    finalizer(_RealPoly_clear_fn, z)
    return z
  end

  function RealPolyRingElem(x::RealFieldElem, p::Int)
    z = RealPolyRingElem()
    ccall((:arb_poly_set_coeff_arb, libflint), Nothing,
          (Ref{RealPolyRingElem}, Int, Ref{RealFieldElem}), z, 0, x)
    return z
  end

  function RealPolyRingElem(x::Vector{RealFieldElem}, p::Int)
    z = RealPolyRingElem()
    for i = 1:length(x)
      ccall((:arb_poly_set_coeff_arb, libflint), Nothing,
            (Ref{RealPolyRingElem}, Int, Ref{RealFieldElem}), z, i - 1, x[i])
    end
    return z
  end

  function RealPolyRingElem(x::RealPolyRingElem)
    z = RealPolyRingElem()
    ccall((:arb_poly_set, libflint), Nothing, (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}), z, x)
    return z
  end

  function RealPolyRingElem(x::RealPolyRingElem, p::Int)
    z = RealPolyRingElem()
    ccall((:arb_poly_set_round, libflint), Nothing,
          (Ref{RealPolyRingElem}, Ref{RealPolyRingElem}, Int), z, x, p)
    return z
  end

  function RealPolyRingElem(x::ZZPolyRingElem, p::Int)
    z = RealPolyRingElem()
    ccall((:arb_poly_set_fmpz_poly, libflint), Nothing,
          (Ref{RealPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, x, p)
    return z
  end

  function RealPolyRingElem(x::QQPolyRingElem, p::Int)
    z = RealPolyRingElem()
    ccall((:arb_poly_set_fmpq_poly, libflint), Nothing,
          (Ref{RealPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, p)
    return z
  end
end

function _RealPoly_clear_fn(x::RealPolyRingElem)
  ccall((:arb_poly_clear, libflint), Nothing, (Ref{RealPolyRingElem}, ), x)
end

parent(x::RealPolyRingElem) = x.parent

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
    ccall((:arb_poly_init, libflint), Nothing, (Ref{ArbPolyRingElem}, ), z)
    finalizer(_arb_poly_clear_fn, z)
    return z
  end

  function ArbPolyRingElem(x::ArbFieldElem, p::Int)
    z = ArbPolyRingElem()
    ccall((:arb_poly_set_coeff_arb, libflint), Nothing,
          (Ref{ArbPolyRingElem}, Int, Ref{ArbFieldElem}), z, 0, x)
    return z
  end

  function ArbPolyRingElem(x::Vector{ArbFieldElem}, p::Int)
    z = ArbPolyRingElem()
    for i = 1:length(x)
      ccall((:arb_poly_set_coeff_arb, libflint), Nothing,
            (Ref{ArbPolyRingElem}, Int, Ref{ArbFieldElem}), z, i - 1, x[i])
    end
    return z
  end

  function ArbPolyRingElem(x::ArbPolyRingElem)
    z = ArbPolyRingElem()
    ccall((:arb_poly_set, libflint), Nothing, (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}), z, x)
    return z
  end

  function ArbPolyRingElem(x::ArbPolyRingElem, p::Int)
    z = ArbPolyRingElem()
    ccall((:arb_poly_set_round, libflint), Nothing,
          (Ref{ArbPolyRingElem}, Ref{ArbPolyRingElem}, Int), z, x, p)
    return z
  end

  function ArbPolyRingElem(x::ZZPolyRingElem, p::Int)
    z = ArbPolyRingElem()
    ccall((:arb_poly_set_fmpz_poly, libflint), Nothing,
          (Ref{ArbPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, x, p)
    return z
  end

  function ArbPolyRingElem(x::QQPolyRingElem, p::Int)
    z = ArbPolyRingElem()
    ccall((:arb_poly_set_fmpq_poly, libflint), Nothing,
          (Ref{ArbPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, p)
    return z
  end
end

function _arb_poly_clear_fn(x::ArbPolyRingElem)
  ccall((:arb_poly_clear, libflint), Nothing, (Ref{ArbPolyRingElem}, ), x)
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

mutable struct ComplexPolyRingElem <: PolyRingElem{ComplexFieldElem}
  coeffs::Ptr{Nothing}
  length::Int
  alloc::Int
  parent::ComplexPolyRing

  function ComplexPolyRingElem()
    z = new()
    ccall((:acb_poly_init, libflint), Nothing, (Ref{ComplexPolyRingElem}, ), z)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function ComplexPolyRingElem(x::ComplexFieldElem, p::Int)
    z = ComplexPolyRingElem()
    ccall((:acb_poly_set_coeff_acb, libflint), Nothing,
          (Ref{ComplexPolyRingElem}, Int, Ref{ComplexFieldElem}), z, 0, x)
    return z
  end

  function ComplexPolyRingElem(x::Vector{ComplexFieldElem}, p::Int)
    z = ComplexPolyRingElem()
    for i = 1:length(x)
      ccall((:acb_poly_set_coeff_acb, libflint), Nothing,
            (Ref{ComplexPolyRingElem}, Int, Ref{ComplexFieldElem}), z, i - 1, x[i])
    end
    return z
  end

  function ComplexPolyRingElem(x::ComplexPolyRingElem)
    z = ComplexPolyRingElem()
    ccall((:acb_poly_set, libflint), Nothing, (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}), z, x)
    return z
  end

  function ComplexPolyRingElem(x::RealPolyRingElem, p::Int)
    z = ComplexPolyRingElem()
    ccall((:acb_poly_set_arb_poly, libflint), Nothing,
          (Ref{ComplexPolyRingElem}, Ref{ArbPolyRingElem}, Int), z, x, p)
    ccall((:acb_poly_set_round, libflint), Nothing,
          (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int), z, z, p)
    return z
  end

  function ComplexPolyRingElem(x::ComplexPolyRingElem, p::Int)
    z = ComplexPolyRingElem()
    ccall((:acb_poly_set_round, libflint), Nothing,
          (Ref{ComplexPolyRingElem}, Ref{ComplexPolyRingElem}, Int), z, x, p)
    return z
  end

  function ComplexPolyRingElem(x::ZZPolyRingElem, p::Int)
    z = ComplexPolyRingElem()
    ccall((:acb_poly_set_fmpz_poly, libflint), Nothing,
          (Ref{ComplexPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, x, p)
    return z
  end

  function ComplexPolyRingElem(x::QQPolyRingElem, p::Int)
    z = ComplexPolyRingElem()
    ccall((:acb_poly_set_fmpq_poly, libflint), Nothing,
          (Ref{ComplexPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, p)
    return z
  end
end

function _acb_poly_clear_fn(x::ComplexPolyRingElem)
  ccall((:acb_poly_clear, libflint), Nothing, (Ref{ComplexPolyRingElem}, ), x)
end

parent(x::ComplexPolyRingElem) = x.parent

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
    ccall((:acb_poly_init, libflint), Nothing, (Ref{AcbPolyRingElem}, ), z)
    finalizer(_acb_poly_clear_fn, z)
    return z
  end

  function AcbPolyRingElem(x::AcbFieldElem, p::Int)
    z = AcbPolyRingElem()
    ccall((:acb_poly_set_coeff_acb, libflint), Nothing,
          (Ref{AcbPolyRingElem}, Int, Ref{AcbFieldElem}), z, 0, x)
    return z
  end

  function AcbPolyRingElem(x::Vector{AcbFieldElem}, p::Int)
    z = AcbPolyRingElem()
    for i = 1:length(x)
      ccall((:acb_poly_set_coeff_acb, libflint), Nothing,
            (Ref{AcbPolyRingElem}, Int, Ref{AcbFieldElem}), z, i - 1, x[i])
    end
    return z
  end

  function AcbPolyRingElem(x::AcbPolyRingElem)
    z = AcbPolyRingElem()
    ccall((:acb_poly_set, libflint), Nothing, (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}), z, x)
    return z
  end

  function AcbPolyRingElem(x::ArbPolyRingElem, p::Int)
    z = AcbPolyRingElem()
    ccall((:acb_poly_set_arb_poly, libflint), Nothing,
          (Ref{AcbPolyRingElem}, Ref{ArbPolyRingElem}, Int), z, x, p)
    ccall((:acb_poly_set_round, libflint), Nothing,
          (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int), z, z, p)
    return z
  end

  function AcbPolyRingElem(x::AcbPolyRingElem, p::Int)
    z = AcbPolyRingElem()
    ccall((:acb_poly_set_round, libflint), Nothing,
          (Ref{AcbPolyRingElem}, Ref{AcbPolyRingElem}, Int), z, x, p)
    return z
  end

  function AcbPolyRingElem(x::ZZPolyRingElem, p::Int)
    z = AcbPolyRingElem()
    ccall((:acb_poly_set_fmpz_poly, libflint), Nothing,
          (Ref{AcbPolyRingElem}, Ref{ZZPolyRingElem}, Int), z, x, p)
    return z
  end

  function AcbPolyRingElem(x::QQPolyRingElem, p::Int)
    z = AcbPolyRingElem()
    ccall((:acb_poly_set_fmpq_poly, libflint), Nothing,
          (Ref{AcbPolyRingElem}, Ref{QQPolyRingElem}, Int), z, x, p)
    return z
  end
end

function _acb_poly_clear_fn(x::AcbPolyRingElem)
  ccall((:acb_poly_clear, libflint), Nothing, (Ref{AcbPolyRingElem}, ), x)
end

parent(x::AcbPolyRingElem) = x.parent

var(x::AcbPolyRing) = x.S

precision(x::AcbPolyRing) = precision(x.base_ring)

base_ring(a::AcbPolyRing) = a.base_ring



################################################################################
#
#  Types and memory management for ArbMatrixSpace
#
################################################################################

const RealMatrixSpace = AbstractAlgebra.Generic.MatSpace{RealFieldElem}

mutable struct RealMatrix <: MatElem{RealFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  #base_ring::ArbField

  function RealMatrix(r::Int, c::Int)
    z = new()
    ccall((:arb_mat_init, libflint), Nothing, (Ref{RealMatrix}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    return z
  end

  function RealMatrix(a::ZZMatrix)
    z = RealMatrix(a.r, a.c)
    ccall((:arb_mat_set_fmpz_mat, libflint), Nothing,
          (Ref{RealMatrix}, Ref{ZZMatrix}), z, a)
    return z
  end

  function RealMatrix(a::ZZMatrix, prec::Int)
    z = RealMatrix(a.r, a.c)
    ccall((:arb_mat_set_round_fmpz_mat, libflint), Nothing,
          (Ref{RealMatrix}, Ref{ZZMatrix}, Int), z, a, prec)
    return z
  end

  function RealMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, ZZRingElem, Float64, BigFloat, RealFieldElem}}
    z = RealMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _arb_set(el, arr[i, j])
      end
    end
    return z
  end

  function RealMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, ZZRingElem, Float64, BigFloat, RealFieldElem}}
    z = RealMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _arb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function RealMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, AbstractString}}
    z = RealMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _arb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function RealMatrix(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, RealFieldElem, AbstractString}}
    z = RealMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _arb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function RealMatrix(a::QQMatrix, prec::Int)
    z = RealMatrix(a.r, a.c)
    ccall((:arb_mat_set_fmpq_mat, libflint), Nothing,
          (Ref{RealMatrix}, Ref{QQMatrix}, Int), z, a, prec)
    return z
  end
end

function _arb_mat_clear_fn(x::RealMatrix)
  ccall((:arb_mat_clear, libflint), Nothing, (Ref{RealMatrix}, ), x)
end

# fixed precision

const ArbMatrixSpace = AbstractAlgebra.Generic.MatSpace{ArbFieldElem}

mutable struct ArbMatrix <: MatElem{ArbFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  base_ring::ArbField

  function ArbMatrix(r::Int, c::Int)
    z = new()
    ccall((:arb_mat_init, libflint), Nothing, (Ref{ArbMatrix}, Int, Int), z, r, c)
    finalizer(_arb_mat_clear_fn, z)
    return z
  end

  function ArbMatrix(a::ZZMatrix)
    z = ArbMatrix(a.r, a.c)
    ccall((:arb_mat_set_fmpz_mat, libflint), Nothing,
          (Ref{ArbMatrix}, Ref{ZZMatrix}), z, a)
    return z
  end

  function ArbMatrix(a::ZZMatrix, prec::Int)
    z = ArbMatrix(a.r, a.c)
    ccall((:arb_mat_set_round_fmpz_mat, libflint), Nothing,
          (Ref{ArbMatrix}, Ref{ZZMatrix}, Int), z, a, prec)
    return z
  end

  function ArbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, ZZRingElem, Float64, BigFloat, ArbFieldElem}}
    z = ArbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _arb_set(el, arr[i, j])
      end
    end
    return z
  end

  function ArbMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, ZZRingElem, Float64, BigFloat, ArbFieldElem}}
    z = ArbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _arb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function ArbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AbstractString}}
    z = ArbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _arb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function ArbMatrix(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64, BigFloat, ArbFieldElem, AbstractString}}
    z = ArbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _arb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function ArbMatrix(a::QQMatrix, prec::Int)
    z = ArbMatrix(a.r, a.c)
    ccall((:arb_mat_set_fmpq_mat, libflint), Nothing,
          (Ref{ArbMatrix}, Ref{QQMatrix}, Int), z, a, prec)
    return z
  end
end

function _arb_mat_clear_fn(x::ArbMatrix)
  ccall((:arb_mat_clear, libflint), Nothing, (Ref{ArbMatrix}, ), x)
end

################################################################################
#
#  Types and memory management for AcbMatrixSpace
#
################################################################################

const ComplexMatrixSpace = AbstractAlgebra.Generic.MatSpace{ComplexFieldElem}

mutable struct ComplexMatrix <: MatElem{ComplexFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  #base_ring::AcbField

  function ComplexMatrix(r::Int, c::Int)
    z = new()
    ccall((:acb_mat_init, libflint), Nothing, (Ref{ComplexMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end

  function ComplexMatrix(a::ZZMatrix)
    z = ComplexMatrix(a.r, a.c)
    ccall((:acb_mat_set_fmpz_mat, libflint), Nothing,
          (Ref{ComplexMatrix}, Ref{ZZMatrix}), z, a)
    return z
  end

  function ComplexMatrix(a::ZZMatrix, prec::Int)
    z = ComplexMatrix(a.r, a.c)
    ccall((:acb_mat_set_round_fmpz_mat, libflint), Nothing,
          (Ref{ComplexMatrix}, Ref{ZZMatrix}, Int), z, a, prec)
    return z
  end

  function ComplexMatrix(a::RealMatrix)
    z = ComplexMatrix(a.r, a.c)
    ccall((:acb_mat_set_arb_mat, libflint), Nothing,
          (Ref{ComplexMatrix}, Ref{ArbMatrix}), z, a)
    return z
  end

  function ComplexMatrix(a::ArbMatrix, prec::Int)
    z = ComplexMatrix(a.r, a.c)
    ccall((:acb_mat_set_round_arb_mat, libflint), Nothing,
          (Ref{ComplexMatrix}, Ref{ArbMatrix}, Int), z, a, prec)
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{BigFloat, ComplexFieldElem, RealFieldElem}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{BigFloat, ComplexFieldElem, RealFieldElem}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{BigFloat, RealFieldElem, AbstractString, ComplexFieldElem}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{BigFloat, RealFieldElem, AbstractString, ComplexFieldElem}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractMatrix{Tuple{T, T}}, prec::Int) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractMatrix{Tuple{T, T}}, prec::Int) where {T <: Union{QQFieldElem, BigFloat, RealFieldElem, AbstractString}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractVector{Tuple{T, T}}, prec::Int) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function ComplexMatrix(r::Int, c::Int, arr::AbstractVector{Tuple{T, T}}, prec::Int) where {T <: Union{QQFieldElem, BigFloat, RealFieldElem, AbstractString}}
    z = ComplexMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function ComplexMatrix(a::QQMatrix, prec::Int)
    z = ComplexMatrix(a.r, a.c)
    ccall((:acb_mat_set_fmpq_mat, libflint), Nothing,
          (Ref{ComplexMatrix}, Ref{QQMatrix}, Int), z, a, prec)
    return z
  end
end

function _acb_mat_clear_fn(x::ComplexMatrix)
  ccall((:acb_mat_clear, libflint), Nothing, (Ref{ComplexMatrix}, ), x)
end

# fixed precision

const AcbMatrixSpace = AbstractAlgebra.Generic.MatSpace{AcbFieldElem}

mutable struct AcbMatrix <: MatElem{AcbFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  base_ring::AcbField

  function AcbMatrix(r::Int, c::Int)
    z = new()
    ccall((:acb_mat_init, libflint), Nothing, (Ref{AcbMatrix}, Int, Int), z, r, c)
    finalizer(_acb_mat_clear_fn, z)
    return z
  end

  function AcbMatrix(a::ZZMatrix)
    z = AcbMatrix(a.r, a.c)
    ccall((:acb_mat_set_fmpz_mat, libflint), Nothing,
          (Ref{AcbMatrix}, Ref{ZZMatrix}), z, a)
    return z
  end

  function AcbMatrix(a::ZZMatrix, prec::Int)
    z = AcbMatrix(a.r, a.c)
    ccall((:acb_mat_set_round_fmpz_mat, libflint), Nothing,
          (Ref{AcbMatrix}, Ref{ZZMatrix}, Int), z, a, prec)
    return z
  end

  function AcbMatrix(a::ArbMatrix)
    z = AcbMatrix(a.r, a.c)
    ccall((:acb_mat_set_arb_mat, libflint), Nothing,
          (Ref{AcbMatrix}, Ref{ArbMatrix}), z, a)
    return z
  end

  function AcbMatrix(a::ArbMatrix, prec::Int)
    z = AcbMatrix(a.r, a.c)
    ccall((:acb_mat_set_round_arb_mat, libflint), Nothing,
          (Ref{AcbMatrix}, Ref{ArbMatrix}, Int), z, a, prec)
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Union{BigFloat, AcbFieldElem, ArbFieldElem}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Union{BigFloat, AcbFieldElem, ArbFieldElem}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, prec::Int) where {T <: Union{BigFloat, ArbFieldElem, AbstractString, AcbFieldElem}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{Int, UInt, ZZRingElem, QQFieldElem, Float64}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{T}, prec::Int) where {T <: Union{BigFloat, ArbFieldElem, AbstractString, AcbFieldElem}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{Tuple{T, T}}, prec::Int) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractMatrix{Tuple{T, T}}, prec::Int) where {T <: Union{QQFieldElem, BigFloat, ArbFieldElem, AbstractString}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{Tuple{T, T}}, prec::Int) where {T <: Union{Int, UInt, Float64, ZZRingElem}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function AcbMatrix(r::Int, c::Int, arr::AbstractVector{Tuple{T, T}}, prec::Int) where {T <: Union{QQFieldElem, BigFloat, ArbFieldElem, AbstractString}}
    z = AcbMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function AcbMatrix(a::QQMatrix, prec::Int)
    z = AcbMatrix(a.r, a.c)
    ccall((:acb_mat_set_fmpq_mat, libflint), Nothing,
          (Ref{AcbMatrix}, Ref{QQMatrix}, Int), z, a, prec)
    return z
  end
end

function _acb_mat_clear_fn(x::AcbMatrix)
  ccall((:acb_mat_clear, libflint), Nothing, (Ref{AcbMatrix}, ), x)
end


################################################################################
#
#   Type unions
#
################################################################################

const RealFieldElemOrPtr = Union{RealFieldElem, Ref{RealFieldElem}, Ptr{RealFieldElem}}
const ArbFieldElemOrPtr = Union{ArbFieldElem, Ref{ArbFieldElem}, Ptr{ArbFieldElem}}
const ComplexFieldElemOrPtr = Union{ComplexFieldElem, Ref{ComplexFieldElem}, Ptr{ComplexFieldElem}}
const AcbFieldElemOrPtr = Union{AcbFieldElem, Ref{AcbFieldElem}, Ptr{AcbFieldElem}}

const RealPolyRingElemOrPtr = Union{RealPolyRingElem, Ref{RealPolyRingElem}, Ptr{RealPolyRingElem}}
const ArbPolyRingElemOrPtr = Union{ArbPolyRingElem, Ref{ArbPolyRingElem}, Ptr{ArbPolyRingElem}}
const ComplexPolyRingElemOrPtr = Union{ComplexPolyRingElem, Ref{ComplexPolyRingElem}, Ptr{ComplexPolyRingElem}}
const AcbPolyRingElemOrPtr = Union{AcbPolyRingElem, Ref{AcbPolyRingElem}, Ptr{AcbPolyRingElem}}

const RealMatrixOrPtr = Union{RealMatrix, Ref{RealMatrix}, Ptr{RealMatrix}}
const ArbMatrixOrPtr = Union{ArbMatrix, Ref{ArbMatrix}, Ptr{ArbMatrix}}
const ComplexMatrixOrPtr = Union{ComplexMatrix, Ref{ComplexMatrix}, Ptr{ComplexMatrix}}
const AcbMatrixOrPtr = Union{AcbMatrix, Ref{AcbMatrix}, Ptr{AcbMatrix}}
