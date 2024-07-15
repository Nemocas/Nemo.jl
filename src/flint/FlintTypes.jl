###############################################################################
#
#   FlintTypes.jl : Parent and object types for Flint
#
###############################################################################

const _err_dim_negative = ErrorException("Dimensions must be non-negative")

###############################################################################
#
#   ZZRing / ZZRingElem
#
###############################################################################

const zz_ring_doc = raw"""
    ZZRing <: Ring
    ZZRingElem <: RingElem

The ring of integers $\mathbb Z$ and its elements.
For convenience, we predefine the global variable `const ZZ = ZZRing()`,
so we can create elements via [`ZZ(x)`](@ref `(::Ring)(x)`).

# Examples

```jldoctest
julia> ZZ(2)
2

julia> ZZ(2)^100
1267650600228229401496703205376
```
"""

@doc zz_ring_doc
struct ZZRing <: Ring
end

@doc zz_ring_doc
const ZZ = ZZRing()

integer_ring() = ZZRing()

@doc zz_ring_doc
mutable struct ZZRingElem <: RingElem
  d::Int

  function ZZRingElem()
    z = new()
    ccall((:fmpz_init, libflint), Nothing, (Ref{ZZRingElem},), z)
    finalizer(_fmpz_clear_fn, z)
    return z
  end

  function ZZRingElem(x::Int)
    z = new()
    ccall((:fmpz_init_set_si, libflint), Nothing, (Ref{ZZRingElem}, Int), z, x)
    finalizer(_fmpz_clear_fn, z)
    return z
  end

  function ZZRingElem(x::UInt)
    z = new()
    ccall((:fmpz_init_set_ui, libflint), Nothing, (Ref{ZZRingElem}, UInt), z, x)
    finalizer(_fmpz_clear_fn, z)
    return z
  end

  function ZZRingElem(x::BigInt)
    z = ZZRingElem()
    ccall((:fmpz_set_mpz, libflint), Nothing, (Ref{ZZRingElem}, Ref{BigInt}), z, x)
    return z
  end

  function ZZRingElem(x::Ptr{Culong}, len::Clong)
    z = ZZRingElem()
    ccall((:fmpz_set_ui_array, libflint), Nothing, (Ref{ZZRingElem}, Ptr{Culong}, Clong), z, x, len)
    return z
  end

  function ZZRingElem(x::Float64)
    !isinteger(x) && throw(InexactError(:convert, ZZRingElem, x))
    z = ZZRingElem()
    ccall((:fmpz_set_d, libflint), Nothing, (Ref{ZZRingElem}, Cdouble), z, x)
    return z
  end

  ZZRingElem(x::ZZRingElem) = x
end

function _fmpz_clear_fn(a::ZZRingElem)
  ccall((:fmpz_clear, libflint), Nothing, (Ref{ZZRingElem},), a)
end

mutable struct fmpz_factor
  sign::Cint
  p::Ptr{Nothing} # Array of fmpz_struct's
  exp::Ptr{UInt}
  alloc::Int
  num::Int

  function fmpz_factor()
    z = new()
    ccall((:fmpz_factor_init, libflint), Nothing, (Ref{fmpz_factor}, ), z)
    finalizer(_fmpz_factor_clear_fn, z)
    return z
  end
end

function _fmpz_factor_clear_fn(a::fmpz_factor)
  ccall((:fmpz_factor_clear, libflint), Nothing,
        (Ref{fmpz_factor}, ), a)
end

struct ZZRingElemUnitRange <: AbstractUnitRange{ZZRingElem}
  start::ZZRingElem
  stop::ZZRingElem
  ZZRingElemUnitRange(start, stop) = new(start, fmpz_unitrange_last(start, stop))
end

###############################################################################
#
#   n_factor
#
###############################################################################

mutable struct n_factor
  num::Cint
  exp::NTuple{15, Cint}
  p::NTuple{15, UInt}

  function n_factor()
    z = new()
    ccall((:n_factor_init, libflint), Nothing, (Ref{n_factor}, ), z)
    # no finalizer needed
    return z
  end
end

###############################################################################
#
#   QQField / QQFieldElem
#
###############################################################################

const qq_field_doc = raw"""
    QQField <: FracField{ZZRingElem}
    QQFieldElem <: FracFieldElem{ZZRingElem}

The field of rationals $\mathbb Q$ and its elements.
For convenience, we predefine the global variable `const QQ = QQField()`.

# Examples

```jldoctest
julia> QQ(2//3) == ZZ(2)//ZZ(3)
true

julia> QQ(1//6) - QQ(1//7)
1//42
```
"""

@doc qq_field_doc
struct QQField <: FracField{ZZRingElem}
end

@doc qq_field_doc
const QQ = QQField()

rational_field() = QQ

@doc qq_field_doc
mutable struct QQFieldElem <: FracElem{ZZRingElem}
  num::Int
  den::Int

  function QQFieldElem()
    z = new()
    ccall((:fmpq_init, libflint), Nothing, (Ref{QQFieldElem},), z)
    finalizer(_fmpq_clear_fn, z)
    return z
  end

  function QQFieldElem(a::ZZRingElem, b::ZZRingElem)
    iszero(b) && throw(DivideError())
    z = QQFieldElem()
    set!(z, a, b)
    return z
  end

  function QQFieldElem(a::ZZRingElem)
    z = QQFieldElem()
    set!(z, a)
    return z
  end

  function QQFieldElem(a::Int, b::Int)
    b == 0 && throw(DivideError())
    z = QQFieldElem()
    if b == typemin(Int) || (b < 0 && a == typemin(Int))
      bz = -ZZ(b)
      az = -ZZ(a)
      set!(z, az, bz)
    else
      if b < 0 # Flint requires positive denominator
        b = -b
        a = -a
      end
      set!(z, a, UInt(b))
    end
    return z
  end

  function QQFieldElem(a::Int)
    z = QQFieldElem()
    set!(z, a)
    return z
  end

  QQFieldElem(a::QQFieldElem) = a
end

_fmpq_clear_fn(a::QQFieldElem) = ccall((:fmpq_clear, libflint), Nothing, (Ref{QQFieldElem},), a)

###############################################################################
#
#   ZZPolyRing / ZZPolyRingElem
#
###############################################################################

@attributes mutable struct ZZPolyRing <: PolyRing{ZZRingElem}
  S::Symbol

  function ZZPolyRing(R::ZZRing, s::Symbol, cached::Bool = true)
    return get_cached!(FmpzPolyID, s, cached) do
      return new(s)
    end
  end
end

const FmpzPolyID = CacheDictType{Symbol, ZZPolyRing}()

mutable struct ZZPolyRingElem <: PolyRingElem{ZZRingElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  parent::ZZPolyRing

  function ZZPolyRingElem()
    z = new()
    ccall((:fmpz_poly_init, libflint), Nothing, (Ref{ZZPolyRingElem},), z)
    finalizer(_fmpz_poly_clear_fn, z)
    return z
  end

  function ZZPolyRingElem(a::Vector{ZZRingElem})
    z = new()
    ccall((:fmpz_poly_init2, libflint), Nothing,
          (Ref{ZZPolyRingElem}, Int), z, length(a))
    for i = 1:length(a)
      ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{ZZPolyRingElem}, Int, Ref{ZZRingElem}), z, i - 1, a[i])
    end
    finalizer(_fmpz_poly_clear_fn, z)
    return z
  end

  function ZZPolyRingElem(a::Int)
    z = ZZPolyRingElem()
    ccall((:fmpz_poly_set_si, libflint), Nothing, (Ref{ZZPolyRingElem}, Int), z, a)
    return z
  end

  function ZZPolyRingElem(a::ZZRingElem)
    z = ZZPolyRingElem()
    ccall((:fmpz_poly_set_fmpz, libflint), Nothing,
          (Ref{ZZPolyRingElem}, Ref{ZZRingElem}), z, a)
    return z
  end

  function ZZPolyRingElem(a::ZZPolyRingElem)
    z = ZZPolyRingElem()
    ccall((:fmpz_poly_set, libflint), Nothing,
          (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, a)
    return z
  end
end

function _fmpz_poly_clear_fn(a::ZZPolyRingElem)
  ccall((:fmpz_poly_clear, libflint), Nothing, (Ref{ZZPolyRingElem},), a)
end

mutable struct fmpz_poly_factor
  d::Int # ZZRingElem
  p::Ptr{ZZPolyRingElem} # array of flint fmpz_poly_struct's
  exp::Ptr{Int}
  num::Int
  alloc::Int

  function fmpz_poly_factor()
    z = new()
    ccall((:fmpz_poly_factor_init, libflint), Nothing,
          (Ref{fmpz_poly_factor}, ), z)
    finalizer(_fmpz_poly_factor_clear_fn, z)
    return z
  end
end

function _fmpz_poly_factor_clear_fn(f::fmpz_poly_factor)
  ccall((:fmpz_poly_factor_clear, libflint), Nothing,
        (Ref{fmpz_poly_factor}, ), f)
  nothing
end

###############################################################################
#
#   QQPolyRing / QQPolyRingElem
#
###############################################################################

@attributes mutable struct QQPolyRing <: PolyRing{QQFieldElem}
  S::Symbol

  function QQPolyRing(R::QQField, s::Symbol, cached::Bool = true)
    return get_cached!(FmpqPolyID, s, cached) do
      return new(s)
    end
  end
end

const FmpqPolyID = CacheDictType{Symbol, QQPolyRing}()

mutable struct QQPolyRingElem <: PolyRingElem{QQFieldElem}
  coeffs::Ptr{Int}
  alloc::Int
  length::Int
  den::Int
  # end flint struct

  parent::QQPolyRing

  function QQPolyRingElem()
    z = new()
    ccall((:fmpq_poly_init, libflint), Nothing, (Ref{QQPolyRingElem},), z)
    finalizer(_fmpq_poly_clear_fn, z)
    return z
  end

  function QQPolyRingElem(a::Vector{QQFieldElem})
    z = new()
    ccall((:fmpq_poly_init2, libflint), Nothing,
          (Ref{QQPolyRingElem}, Int), z, length(a))
    for i = 1:length(a)
      ccall((:fmpq_poly_set_coeff_fmpq, libflint), Nothing,
            (Ref{QQPolyRingElem}, Int, Ref{QQFieldElem}), z, i - 1, a[i])
    end
    finalizer(_fmpq_poly_clear_fn, z)
    return z
  end

  function QQPolyRingElem(a::Int)
    z = QQPolyRingElem()
    ccall((:fmpq_poly_set_si, libflint), Nothing, (Ref{QQPolyRingElem}, Int), z, a)
    return z
  end

  function QQPolyRingElem(a::ZZRingElem)
    z = QQPolyRingElem()
    ccall((:fmpq_poly_set_fmpz, libflint), Nothing,
          (Ref{QQPolyRingElem}, Ref{ZZRingElem}), z, a)
    return z
  end

  function QQPolyRingElem(a::QQFieldElem)
    z = QQPolyRingElem()
    ccall((:fmpq_poly_set_fmpq, libflint), Nothing,
          (Ref{QQPolyRingElem}, Ref{QQFieldElem}), z, a)
    return z
  end

  function QQPolyRingElem(a::ZZPolyRingElem)
    z = QQPolyRingElem()
    ccall((:fmpq_poly_set_fmpz_poly, libflint), Nothing,
          (Ref{QQPolyRingElem}, Ref{ZZPolyRingElem}), z, a)
    return z
  end

  function QQPolyRingElem(a::QQPolyRingElem)
    z = QQPolyRingElem()
    ccall((:fmpq_poly_set, libflint), Nothing,
          (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, a)
    return z
  end
end

function _fmpq_poly_clear_fn(a::QQPolyRingElem)
  ccall((:fmpq_poly_clear, libflint), Nothing, (Ref{QQPolyRingElem},), a)
end

###############################################################################
#
#   zzModRing / zzModRingElem
#
###############################################################################

@doc raw"""
    zzModRing <: Ring

The ring $\mathbb Z/n\mathbb Z$ for some $n$. See [`residue_ring`](@ref).
Implementation for the modulus being a machine integer [`Int`](@ref).
For the modulus being a [`ZZRingElem`](@ref) see [`ZZModRing`](@ref).
"""
@attributes mutable struct zzModRing <: Ring
  n::UInt
  ninv::UInt

  function zzModRing(n::UInt, cached::Bool=true)
    return get_cached!(NmodRingID, n, cached) do
      ninv = ccall((:n_preinvert_limb, libflint), UInt, (UInt,), n)
      return new(n, ninv)
    end
  end
end

const NmodRingID = CacheDictType{UInt, zzModRing}()

@doc raw"""
    zzModRingElem <: RingElem

An element of a ring $\mathbb Z/n\mathbb Z$. See [`zzModRing`](@ref).
"""
struct zzModRingElem <: ResElem{UInt}
  data::UInt
  parent::zzModRing
end

################################################################################
#
#   fpField / gfp
#
###############################################################################

@doc raw"""
    fpField <: FinField

A Galois field $\mathbb F_p$ for some prime $p$. See [`GF`](@ref).
Implementation for $p$ being a machine integer [`Int`](@ref).
"""
@attributes mutable struct fpField <: FinField
  n::UInt
  ninv::UInt

  function fpField(n::UInt, cached::Bool=true)
    return get_cached!(GaloisFieldID, n, cached) do
      ninv = ccall((:n_preinvert_limb, libflint), UInt, (UInt,), n)
      return new(n, ninv)
    end
  end
end

const GaloisFieldID = CacheDictType{UInt, fpField}()

@doc raw"""
    fpFieldElem <: FinFieldElem

An element of a Galois field $\mathbb F_p$. See [`fpField`](@ref).
"""
struct fpFieldElem <: FinFieldElem
  data::UInt
  parent::fpField
end

###############################################################################
#
#   ZZModRing / ZZModRingElem
#
###############################################################################

mutable struct fmpz_mod_ctx_struct
  n::Int # fmpz_t
  add_fxn::Ptr{Nothing}
  sub_fxn::Ptr{Nothing}
  mul_fxn::Ptr{Nothing}
  n2::UInt
  ninv::UInt
  norm::UInt
  n_limbs::Tuple{UInt, UInt, UInt}
  ninv_limbs::Tuple{UInt, UInt, UInt}
  ninv_huge::Ptr{Nothing} # fmpz_preinvn_struct

  function fmpz_mod_ctx_struct()
    z = new()
    finalizer(_fmpz_mod_ctx_clear_fn, z)
    return z
  end
end

function _fmpz_mod_ctx_clear_fn(a::fmpz_mod_ctx_struct)
  ccall((:fmpz_mod_ctx_clear, libflint), Nothing, (Ref{fmpz_mod_ctx_struct},), a)
end

@doc raw"""
    ZZModRing <: Ring

The ring $\mathbb Z/n\mathbb Z$ for some $n$. See [`residue_ring`](@ref).
Implementation for the modulus being a big integer [`ZZRingElem`](@ref).
For the modulus being an [`Int`](@ref) see [`zzModRing`](@ref).
"""
@attributes mutable struct ZZModRing <: Ring
  n::ZZRingElem
  ninv::fmpz_mod_ctx_struct

  function ZZModRing(n::ZZRingElem, cached::Bool=true)
    return get_cached!(FmpzModRingID, n, cached) do
      ninv = fmpz_mod_ctx_struct()
      ccall((:fmpz_mod_ctx_init, libflint), Nothing, (Ref{fmpz_mod_ctx_struct}, Ref{ZZRingElem}), ninv, n)
      return new(n, ninv)
    end
  end
end

const FmpzModRingID = CacheDictType{ZZRingElem, ZZModRing}()

@doc raw"""
    ZZModRingElem <: RingElem

An element of a ring $\mathbb Z/n\mathbb Z$. See [`ZZModRing`](@ref).
"""
struct ZZModRingElem <: ResElem{ZZRingElem}
  data::ZZRingElem
  parent::ZZModRing
end

###############################################################################
#
#   FpField / FpFieldElem
#
###############################################################################

@doc raw"""
    FpField <: FinField

A Galois field $\mathbb F_p$ for some prime $p$. See [`GF`](@ref).
Implementation for $p$ being a big integer [`ZZRingElem`](@ref).
"""
@attributes mutable struct FpField <: FinField
  n::ZZRingElem
  ninv::fmpz_mod_ctx_struct

  function FpField(n::ZZRingElem, cached::Bool=true)
    return get_cached!(GaloisFmpzFieldID, n, cached) do
      ninv = fmpz_mod_ctx_struct()
      ccall((:fmpz_mod_ctx_init, libflint), Nothing,
            (Ref{fmpz_mod_ctx_struct}, Ref{ZZRingElem}), ninv, n)
      return new(n, ninv)
    end
  end
end

const GaloisFmpzFieldID = CacheDictType{ZZRingElem, FpField}()

@doc raw"""
    FpFieldElem <: FinFieldElem

An element of a Galois field $\mathbb F_p$. See [`FpField`](@ref).
"""
struct FpFieldElem <: FinFieldElem
  data::ZZRingElem
  parent::FpField
end

###############################################################################
#
#   zzModPolyRing / zzModPolyRingElem
#
###############################################################################

@attributes mutable struct zzModPolyRing <: PolyRing{zzModRingElem}
  base_ring::zzModRing
  S::Symbol
  n::UInt

  function zzModPolyRing(R::zzModRing, s::Symbol, cached::Bool = true)
    return get_cached!(NmodPolyRingID, (R, s), cached) do
      m = UInt(modulus(R))
      return new(R, s, m)
    end
  end
end

const NmodPolyRingID = CacheDictType{Tuple{zzModRing, Symbol}, zzModPolyRing}()

mutable struct zzModPolyRingElem <: PolyRingElem{zzModRingElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  mod_n::UInt
  mod_ninv::UInt
  mod_norm::UInt
  parent::zzModPolyRing

  function zzModPolyRingElem(n::UInt)
    z = new()
    ccall((:nmod_poly_init, libflint), Nothing, (Ref{zzModPolyRingElem}, UInt), z, n)
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModPolyRingElem(n::UInt, a::UInt)
    z = new()
    ccall((:nmod_poly_init, libflint), Nothing, (Ref{zzModPolyRingElem}, UInt), z, n)
    ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
          (Ref{zzModPolyRingElem}, Int, UInt), z, 0, a)
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModPolyRingElem(n::UInt, a::Int)
    z = new()
    ccall((:nmod_poly_init, libflint), Nothing, (Ref{zzModPolyRingElem}, UInt), z, n)
    ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
          (Ref{zzModPolyRingElem}, Int, UInt), z, 0, mod(a, n))
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModPolyRingElem(n::UInt, arr::Vector{ZZRingElem})
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModPolyRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:length(arr)
      tt = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt), arr[i], n)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{zzModPolyRingElem}, Int, UInt), z, i - 1, tt)
    end
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModPolyRingElem(n::UInt, arr::Vector{UInt})
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModPolyRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:length(arr)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{zzModPolyRingElem}, Int, UInt), z, i - 1, arr[i])
    end
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModPolyRingElem(n::UInt, arr::Vector{zzModRingElem})
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModPolyRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:length(arr)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{zzModPolyRingElem}, Int, UInt), z, i-1, arr[i].data)
    end
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModPolyRingElem(n::UInt, f::ZZPolyRingElem)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModPolyRingElem}, UInt, Int), z, n, length(f))
    ccall((:fmpz_poly_get_nmod_poly, libflint), Nothing,
          (Ref{zzModPolyRingElem}, Ref{ZZPolyRingElem}), z, f)
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModPolyRingElem(n::UInt, f::zzModPolyRingElem)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModPolyRingElem}, UInt, Int), z, n, length(f))
    ccall((:nmod_poly_set, libflint), Nothing,
          (Ref{zzModPolyRingElem}, Ref{zzModPolyRingElem}), z, f)
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end
end

function _nmod_poly_clear_fn(x::zzModPolyRingElem)
  ccall((:nmod_poly_clear, libflint), Nothing, (Ref{zzModPolyRingElem}, ), x)
end

mutable struct nmod_poly_factor
  poly::Ptr{zzModPolyRingElem}  # array of flint nmod_poly_struct's
  exp::Ptr{Int}
  num::Int
  alloc::Int
  n::UInt

  function nmod_poly_factor(n::UInt)
    z = new()
    ccall((:nmod_poly_factor_init, libflint), Nothing,
          (Ref{nmod_poly_factor}, ), z)
    z.n = n
    finalizer(_nmod_poly_factor_clear_fn, z)
    return z
  end
end

function _nmod_poly_factor_clear_fn(a::nmod_poly_factor)
  ccall((:nmod_poly_factor_clear, libflint), Nothing,
        (Ref{nmod_poly_factor}, ), a)
end

################################################################################
#
#   fpPolyRing / fpPolyRingElem
#
###############################################################################

@attributes mutable struct fpPolyRing <: PolyRing{fpFieldElem}
  base_ring::fpField
  S::Symbol
  n::UInt

  function fpPolyRing(R::fpField, s::Symbol, cached::Bool = true)
    return get_cached!(GFPPolyRingID, (R, s), cached) do
      m = UInt(modulus(R))
      return new(R, s, m)
    end
  end
end

const GFPPolyRingID = CacheDictType{Tuple{fpField, Symbol}, fpPolyRing}()

mutable struct fpPolyRingElem <: PolyRingElem{fpFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  mod_n::UInt
  mod_ninv::UInt
  mod_norm::UInt
  parent::fpPolyRing

  function fpPolyRingElem(n::UInt)
    z = new()
    ccall((:nmod_poly_init, libflint), Nothing, (Ref{fpPolyRingElem}, UInt), z, n)
    finalizer(_gfp_poly_clear_fn, z)
    return z
  end

  function fpPolyRingElem(n::UInt, a::UInt)
    z = new()
    ccall((:nmod_poly_init, libflint), Nothing, (Ref{fpPolyRingElem}, UInt), z, n)
    ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
          (Ref{fpPolyRingElem}, Int, UInt), z, 0, a)
    finalizer(_gfp_poly_clear_fn, z)
    return z
  end

  function fpPolyRingElem(n::UInt, a::Int)
    z = new()
    ccall((:nmod_poly_init, libflint), Nothing, (Ref{fpPolyRingElem}, UInt), z, n)
    ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
          (Ref{fpPolyRingElem}, Int, UInt), z, 0, mod(a, n))
    finalizer(_gfp_poly_clear_fn, z)
    return z
  end

  function fpPolyRingElem(n::UInt, arr::Vector{ZZRingElem})
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpPolyRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:length(arr)
      tt = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt), arr[i], n)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{fpPolyRingElem}, Int, UInt), z, i - 1, tt)
    end
    finalizer(_gfp_poly_clear_fn, z)
    return z
  end

  function fpPolyRingElem(n::UInt, arr::Vector{UInt})
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpPolyRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:length(arr)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{fpPolyRingElem}, Int, UInt), z, i - 1, arr[i])
    end
    finalizer(_gfp_poly_clear_fn, z)
    return z
  end

  function fpPolyRingElem(n::UInt, arr::Vector{fpFieldElem})
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpPolyRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:length(arr)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{fpPolyRingElem}, Int, UInt), z, i-1, arr[i].data)
    end
    finalizer(_gfp_poly_clear_fn, z)
    return z
  end

  function fpPolyRingElem(n::UInt, f::ZZPolyRingElem)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpPolyRingElem}, UInt, Int), z, n, length(f))
    ccall((:fmpz_poly_get_nmod_poly, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ref{ZZPolyRingElem}), z, f)
    finalizer(_gfp_poly_clear_fn, z)
    return z
  end

  function fpPolyRingElem(n::UInt, f::fpPolyRingElem)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpPolyRingElem}, UInt, Int), z, n, length(f))
    ccall((:nmod_poly_set, libflint), Nothing,
          (Ref{fpPolyRingElem}, Ref{fpPolyRingElem}), z, f)
    finalizer(_gfp_poly_clear_fn, z)
    return z
  end
end

function _gfp_poly_clear_fn(x::fpPolyRingElem)
  ccall((:nmod_poly_clear, libflint), Nothing, (Ref{fpPolyRingElem}, ), x)
end

mutable struct gfp_poly_factor
  poly::Ptr{fpPolyRingElem}  # array of flint nmod_poly_struct's
  exp::Ptr{Int}
  num::Int
  alloc::Int
  n::UInt

  function gfp_poly_factor(n::UInt)
    z = new()
    ccall((:nmod_poly_factor_init, libflint), Nothing,
          (Ref{gfp_poly_factor}, ), z)
    z.n = n
    finalizer(_gfp_poly_factor_clear_fn, z)
    return z
  end
end

function _gfp_poly_factor_clear_fn(a::gfp_poly_factor)
  ccall((:nmod_poly_factor_clear, libflint), Nothing,
        (Ref{gfp_poly_factor}, ), a)
end

###############################################################################
#
#   ZZModPolyRing / ZZModPolyRingElem
#
###############################################################################

@attributes mutable struct ZZModPolyRing <: PolyRing{ZZModRingElem}
  base_ring::ZZModRing
  S::Symbol
  n::ZZRingElem

  function ZZModPolyRing(R::ZZModRing, s::Symbol, cached::Bool = true)
    return get_cached!(FmpzModPolyRingID, (R, s), cached) do
      return new(R, s, modulus(R))
    end
  end
end

const FmpzModPolyRingID = CacheDictType{Tuple{ZZModRing, Symbol}, ZZModPolyRing}()

mutable struct ZZModPolyRingElem <: PolyRingElem{ZZModRingElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  # end of flint struct

  parent::ZZModPolyRing

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct)
    z = new()
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, n)
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end


  function ZZModPolyRingElem(R::ZZModRing)
    return ZZModPolyRingElem(R.ninv)
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, a::ZZRingElem)
    z = new()
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, n)
    ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Int, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, 0, a, n)
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModPolyRingElem(R::ZZModRing, a::ZZRingElem)
    return ZZModPolyRingElem(R.ninv, a)
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, a::UInt)
    z = new()
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, n)
    ccall((:fmpz_mod_poly_set_coeff_ui, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Int, UInt, Ref{fmpz_mod_ctx_struct}),
          z, 0, a, n)
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModPolyRingElem(R::ZZModRing, a::UInt)
    return ZZModPolyRingElem(R.ninv, a)
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, arr::Vector{ZZRingElem})
    length(arr) == 0 && error("Array must have length > 0")
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, length(arr), n)
    for i in 1:length(arr)
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{ZZModPolyRingElem}, Int, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
            z, i - 1, arr[i], n)
    end
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModPolyRingElem(R::ZZModRing, arr::Vector{ZZRingElem})
    return ZZModPolyRingElem(R.ninv, arr)
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, arr::Vector{ZZModRingElem})
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, length(arr), n)
    for i in 1:length(arr)
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{ZZModPolyRingElem}, Int, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
            z, i - 1, arr[i].data, n)
    end
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModPolyRingElem(R::ZZModRing, arr::Vector{ZZModRingElem})
    return ZZModPolyRingElem(R.ninv, arr)
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, f::ZZPolyRingElem)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, length(f), n)
    ccall((:fmpz_mod_poly_set_fmpz_poly, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Ref{ZZPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, f, n)
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModPolyRingElem(R::ZZModRing, f::ZZPolyRingElem)
    return ZZModPolyRingElem(R.ninv, f)
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, f::ZZModPolyRingElem)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, length(f), n)
    ccall((:fmpz_mod_poly_set, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, f, n)
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModPolyRingElem(R::ZZModRing, f::ZZModPolyRingElem)
    return ZZModPolyRingElem(R.ninv, f)
  end
end

function _fmpz_mod_poly_clear_fn(x::ZZModPolyRingElem)
  ccall((:fmpz_mod_poly_clear, libflint), Nothing,
        (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
        x, base_ring(parent(x)).ninv)
end

mutable struct fmpz_mod_poly_factor
  poly::Ptr{ZZModPolyRingElem}
  exp::Ptr{Int}
  num::Int
  alloc::Int
  # end flint struct

  n::fmpz_mod_ctx_struct

  function fmpz_mod_poly_factor(n::fmpz_mod_ctx_struct)
    z = new()
    ccall((:fmpz_mod_poly_factor_init, libflint), Nothing,
          (Ref{fmpz_mod_poly_factor}, Ref{fmpz_mod_ctx_struct}),
          z, n)
    z.n = n
    finalizer(_fmpz_mod_poly_factor_clear_fn, z)
    return z
  end

  function fmpz_mod_poly_factor(R::ZZModRing)
    return fmpz_mod_poly_factor(R.ninv)
  end
end

function _fmpz_mod_poly_factor_clear_fn(a::fmpz_mod_poly_factor)
  ccall((:fmpz_mod_poly_factor_clear, libflint), Nothing,
        (Ref{fmpz_mod_poly_factor}, Ref{fmpz_mod_ctx_struct}),
        a, a.n)
end

###############################################################################
#
#   FpPolyRing / FpPolyRingElem
#
###############################################################################

@attributes mutable struct FpPolyRing <: PolyRing{FpFieldElem}
  base_ring::FpField
  S::Symbol
  n::ZZRingElem

  function FpPolyRing(R::FpField, s::Symbol, cached::Bool = true)
    m = modulus(R)
    return get_cached!(GFPFmpzPolyRingID, (R, s), cached) do
      return new(R, s, m)
    end
  end
end

const GFPFmpzPolyRingID = CacheDictType{Tuple{FpField, Symbol}, FpPolyRing}()

mutable struct FpPolyRingElem <: PolyRingElem{FpFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  # end flint struct

  parent::FpPolyRing

  function FpPolyRingElem(n::fmpz_mod_ctx_struct)
    z = new()
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, n)
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(R::FpField)
    return FpPolyRingElem(R.ninv)
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, a::ZZRingElem)
    z = new()
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, n)
    ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
          (Ref{FpPolyRingElem}, Int, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, 0, a, n)
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(R::FpField, a::ZZRingElem)
    return FpPolyRingElem(R.ninv, a)
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, a::UInt)
    z = new()
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, n)
    ccall((:fmpz_mod_poly_set_coeff_ui, libflint), Nothing,
          (Ref{ZZModPolyRingElem}, Int, UInt, Ref{fmpz_mod_ctx_struct}),
          z, 0, a, n)
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(R::FpField, a::UInt)
    return FpPolyRingElem(R.ninv, a)
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, arr::Vector{ZZRingElem})
    length(arr) == 0 && error("Array must have length > 0")
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{FpPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, length(arr), n)
    for i in 1:length(arr)
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{FpPolyRingElem}, Int, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
            z, i - 1, arr[i], n)
    end
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(R::FpField, arr::Vector{ZZRingElem})
    FpPolyRingElem(R.ninv, arr)
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, arr::Vector{FpFieldElem})
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{FpPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, length(arr), n)
    for i in 1:length(arr)
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{FpPolyRingElem}, Int, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
            z, i - 1, arr[i].data, n)
    end
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(R::FpField, arr::Vector{FpFieldElem})
    return FpPolyRingElem(R.ninv, arr)
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, f::ZZPolyRingElem)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{FpPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, length(f), n)
    ccall((:fmpz_mod_poly_set_fmpz_poly, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{ZZPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, f, n)
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(R::FpField, f::ZZPolyRingElem)
    return FpPolyRingElem(R.ninv, f)
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, f::FpPolyRingElem)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{FpPolyRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, length(f), n)
    ccall((:fmpz_mod_poly_set, libflint), Nothing,
          (Ref{FpPolyRingElem}, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, f, n)
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(R::FpField, f::FpPolyRingElem)
    return FpPolyRingElem(R.ninv, f)
  end
end

function _fmpz_mod_poly_clear_fn(x::FpPolyRingElem)
  ccall((:fmpz_mod_poly_clear, libflint), Nothing,
        (Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}),
        x, base_ring(parent(x)).ninv)
end

mutable struct gfp_fmpz_poly_factor
  poly::Ptr{FpPolyRingElem}
  exp::Ptr{Int}
  num::Int
  alloc::Int
  # end flint struct

  n::fmpz_mod_ctx_struct

  function gfp_fmpz_poly_factor(n::fmpz_mod_ctx_struct)
    z = new()
    ccall((:fmpz_mod_poly_factor_init, libflint), Nothing,
          (Ref{gfp_fmpz_poly_factor}, Ref{fmpz_mod_ctx_struct}),
          z, n)
    z.n = n
    finalizer(_gfp_fmpz_poly_factor_clear_fn, z)
    return z
  end

  function gfp_fmpz_poly_factor(R::FpField)
    return gfp_fmpz_poly_factor(R.ninv)
  end
end

function _gfp_fmpz_poly_factor_clear_fn(a::gfp_fmpz_poly_factor)
  ccall((:fmpz_mod_poly_factor_clear, libflint), Nothing,
        (Ref{gfp_fmpz_poly_factor}, Ref{fmpz_mod_ctx_struct}),
        a, a.n)
end

###############################################################################
#
#   ZZMPolyRing / ZZMPolyRingElem
#
###############################################################################

const flint_orderings = [:lex, :deglex, :degrevlex]

@attributes mutable struct ZZMPolyRing <: MPolyRing{ZZRingElem}
  nvars::Int
  nfields::Cint
  ord::Int
  deg::Cint
  rev::Cint
  lut::NTuple{Base.GMP.BITS_PER_LIMB, Int}
  lut1::NTuple{Base.GMP.BITS_PER_LIMB, UInt8}

  S::Vector{Symbol}

  function ZZMPolyRing(s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(FmpzMPolyID, (s, S), cached) do
      if S == :lex
        ord = 0
      elseif S == :deglex
        ord = 1
      elseif S == :degrevlex
        ord = 2
      else
        error("$S is not a valid ordering")
      end

      z = new()
      ccall((:fmpz_mpoly_ctx_init, libflint), Nothing,
            (Ref{ZZMPolyRing}, Int, Int),
            z, length(s), ord)
      z.S = s
      finalizer(_fmpz_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end
ZZMPolyRing(::ZZRing, s::Vector{Symbol}, S::Symbol, cached::Bool=true) = ZZMPolyRing(s, S, cached)

function _fmpz_mpoly_ctx_clear_fn(a::ZZMPolyRing)
  ccall((:fmpz_mpoly_ctx_clear, libflint), Nothing,
        (Ref{ZZMPolyRing},), a)
end

const FmpzMPolyID = CacheDictType{Tuple{Vector{Symbol}, Symbol}, ZZMPolyRing}()

mutable struct ZZMPolyRingElem <: MPolyRingElem{ZZRingElem}
  coeffs::Ptr{Nothing}
  exps::Ptr{Nothing}
  alloc::Int
  length::Int
  bits::Int
  # end flint struct

  parent::ZZMPolyRing

  function ZZMPolyRingElem(ctx::ZZMPolyRing)
    z = new()
    ccall((:fmpz_mpoly_init, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing},), z, ctx)
    z.parent = ctx
    finalizer(_fmpz_mpoly_clear_fn, z)
    return z
  end

  function ZZMPolyRingElem(ctx::ZZMPolyRing, a::Vector{ZZRingElem}, b::Vector{Vector{UInt}})
    z = new()
    ccall((:fmpz_mpoly_init2, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_fmpz_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:fmpz_mpoly_push_term_fmpz_ui, libflint), Nothing,
            (Ref{ZZMPolyRingElem}, Ref{ZZRingElem}, Ptr{UInt}, Ref{ZZMPolyRing}),
            z, a[i], b[i], ctx)
    end

    ccall((:fmpz_mpoly_sort_terms, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), z, ctx)
    ccall((:fmpz_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), z, ctx)
    return z
  end

  function ZZMPolyRingElem(ctx::ZZMPolyRing, a::Vector{ZZRingElem}, b::Vector{Vector{Int}})
    z = new()
    ccall((:fmpz_mpoly_init2, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_fmpz_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:fmpz_mpoly_push_term_fmpz_ui, libflint), Nothing,
            (Ref{ZZMPolyRingElem}, Ref{ZZRingElem}, Ptr{Int}, Ref{ZZMPolyRing}),
            z, a[i], b[i], ctx)
    end

    ccall((:fmpz_mpoly_sort_terms, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), z, ctx)
    ccall((:fmpz_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), z, ctx)
    return z
  end

  function ZZMPolyRingElem(ctx::ZZMPolyRing, a::Vector{ZZRingElem}, b::Vector{Vector{ZZRingElem}})
    z = new()
    ccall((:fmpz_mpoly_init2, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_fmpz_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:fmpz_mpoly_push_term_fmpz_fmpz, libflint), Nothing,
            (Ref{ZZMPolyRingElem}, Ref{ZZRingElem}, Ptr{Ref{ZZRingElem}}, Ref{ZZMPolyRing}),
            z, a[i], b[i], ctx)
    end

    ccall((:fmpz_mpoly_sort_terms, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), z, ctx)
    ccall((:fmpz_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), z, ctx)
    return z
  end

  function ZZMPolyRingElem(ctx::ZZMPolyRing, a::ZZRingElem)
    z = new()
    ccall((:fmpz_mpoly_init, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing},), z, ctx)
    ccall((:fmpz_mpoly_set_fmpz, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Ref{ZZRingElem}, Ref{ZZMPolyRing}), z, a, ctx)
    z.parent = ctx
    finalizer(_fmpz_mpoly_clear_fn, z)
    return z
  end

  function ZZMPolyRingElem(ctx::ZZMPolyRing, a::Int)
    z = new()
    ccall((:fmpz_mpoly_init, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing},), z, ctx)
    ccall((:fmpz_mpoly_set_si, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Int, Ref{ZZMPolyRing}), z, a, ctx)
    finalizer(_fmpz_mpoly_clear_fn, z)
    z.parent = ctx
    return z
  end

  function ZZMPolyRingElem(ctx::ZZMPolyRing, a::UInt)
    z = new()
    ccall((:fmpz_mpoly_init, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing},), z, ctx)
    ccall((:fmpz_mpoly_set_ui, libflint), Nothing,
          (Ref{ZZMPolyRingElem}, UInt, Ref{ZZMPolyRing}), z, a, ctx)
    finalizer(_fmpz_mpoly_clear_fn, z)
    z.parent = ctx
    return z
  end
end

function _fmpz_mpoly_clear_fn(a::ZZMPolyRingElem)
  ccall((:fmpz_mpoly_clear, libflint), Nothing,
        (Ref{ZZMPolyRingElem}, Ref{ZZMPolyRing}), a, a.parent)
end

mutable struct fmpz_mpoly_factor
  constant::Int
  constant_den::Int
  poly::Ptr{Nothing}
  exp::Ptr{Nothing}
  num::Int
  alloc::Int
  # end flint struct

  parent::ZZMPolyRing

  function fmpz_mpoly_factor(ctx::ZZMPolyRing)
    z = new()
    ccall((:fmpz_mpoly_factor_init, libflint), Nothing,
          (Ref{fmpz_mpoly_factor}, Ref{ZZMPolyRing}),
          z, ctx)
    z.parent = ctx
    finalizer(_fmpz_mpoly_factor_clear_fn, z)
    return z
  end
end

function _fmpz_mpoly_factor_clear_fn(f::fmpz_mpoly_factor)
  ccall((:fmpz_mpoly_factor_clear, libflint), Nothing,
        (Ref{fmpz_mpoly_factor}, Ref{ZZMPolyRing}),
        f, f.parent)
end

###############################################################################
#
#   QQMPolyRing / QQMPolyRingElem
#
###############################################################################

@attributes mutable struct QQMPolyRing <: MPolyRing{QQFieldElem}
  nvars::Int
  nfields::Cint
  ord::Int
  deg::Cint
  rev::Cint
  lut::NTuple{Base.GMP.BITS_PER_LIMB, Int}
  lut1::NTuple{Base.GMP.BITS_PER_LIMB, UInt8}

  S::Vector{Symbol}

  function QQMPolyRing(s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(FmpqMPolyID, (s, S), cached) do
      if S == :lex
        ord = 0
      elseif S == :deglex
        ord = 1
      elseif S == :degrevlex
        ord = 2
      else
        error("$S is not a valid ordering")
      end

      z = new()
      ccall((:fmpq_mpoly_ctx_init, libflint), Nothing,
            (Ref{QQMPolyRing}, Int, Int),
            z, length(s), ord)
      z.S = s
      finalizer(_fmpq_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end
QQMPolyRing(::QQField, s::Vector{Symbol}, S::Symbol, cached::Bool=true) = QQMPolyRing(s, S, cached)

function _fmpq_mpoly_ctx_clear_fn(a::QQMPolyRing)
  ccall((:fmpq_mpoly_ctx_clear, libflint), Nothing,
        (Ref{QQMPolyRing},), a)
end

const FmpqMPolyID = CacheDictType{Tuple{Vector{Symbol}, Symbol}, QQMPolyRing}()

mutable struct QQMPolyRingElem <: MPolyRingElem{QQFieldElem}
  content_num::Int
  content_den::Int
  coeffs::Ptr{Nothing}
  exps::Ptr{Nothing}
  alloc::Int
  length::Int
  bits::Int

  parent::QQMPolyRing

  function QQMPolyRingElem(ctx::QQMPolyRing)
    z = new()
    ccall((:fmpq_mpoly_init, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQMPolyRing},), z, ctx)
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)
    return z
  end

  function QQMPolyRingElem(ctx::QQMPolyRing, a::Vector{QQFieldElem}, b::Vector{Vector{UInt}})
    z = new()
    ccall((:fmpq_mpoly_init2, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:fmpq_mpoly_push_term_fmpq_ui, libflint), Nothing,
            (Ref{QQMPolyRingElem}, Ref{QQFieldElem}, Ptr{UInt}, Ref{QQMPolyRing}),
            z, a[i], b[i], ctx)
    end

    ccall((:fmpq_mpoly_sort_terms, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), z, ctx)
    ccall((:fmpq_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), z, ctx)
    return z
  end

  function QQMPolyRingElem(ctx::QQMPolyRing, a::Vector{QQFieldElem}, b::Vector{Vector{Int}})
    z = new()
    ccall((:fmpq_mpoly_init2, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:fmpq_mpoly_push_term_fmpq_ui, libflint), Nothing,
            (Ref{QQMPolyRingElem}, Ref{QQFieldElem}, Ptr{Int}, Ref{QQMPolyRing}),
            z, a[i], b[i], ctx)
    end

    ccall((:fmpq_mpoly_sort_terms, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), z, ctx)
    ccall((:fmpq_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), z, ctx)
    return z
  end

  function QQMPolyRingElem(ctx::QQMPolyRing, a::Vector{QQFieldElem}, b::Vector{Vector{ZZRingElem}})
    z = new()
    ccall((:fmpq_mpoly_init2, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:fmpq_mpoly_push_term_fmpq_fmpz, libflint), Nothing,
            (Ref{QQMPolyRingElem}, Ref{QQFieldElem}, Ptr{Ref{ZZRingElem}}, Ref{QQMPolyRing}),
            z, a[i], b[i], ctx)
    end

    ccall((:fmpq_mpoly_sort_terms, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), z, ctx)
    ccall((:fmpq_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), z, ctx)
    return z
  end

  function QQMPolyRingElem(ctx::QQMPolyRing, a::ZZRingElem)
    z = new()
    ccall((:fmpq_mpoly_init, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQMPolyRing},), z, ctx)
    ccall((:fmpq_mpoly_set_fmpz, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{ZZRingElem}, Ref{QQMPolyRing}), z, a, ctx)
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)
    return z
  end

  function QQMPolyRingElem(ctx::QQMPolyRing, a::QQFieldElem)
    z = new()
    ccall((:fmpq_mpoly_init, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQMPolyRing},), z, ctx)
    ccall((:fmpq_mpoly_set_fmpq, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQFieldElem}, Ref{QQMPolyRing}), z, a, ctx)
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)
    return z
  end

  function QQMPolyRingElem(ctx::QQMPolyRing, a::Int)
    z = new()
    ccall((:fmpq_mpoly_init, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQMPolyRing},), z, ctx)
    ccall((:fmpq_mpoly_set_si, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Int, Ref{QQMPolyRing}), z, a, ctx)
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)
    return z
  end

  function QQMPolyRingElem(ctx::QQMPolyRing, a::UInt)
    z = new()
    ccall((:fmpq_mpoly_init, libflint), Nothing,
          (Ref{QQMPolyRingElem}, Ref{QQMPolyRing},), z, ctx)
    ccall((:fmpq_mpoly_set_ui, libflint), Nothing,
          (Ref{QQMPolyRingElem}, UInt, Ref{QQMPolyRing}), z, a, ctx)
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)
    return z
  end
end

function _fmpq_mpoly_clear_fn(a::QQMPolyRingElem)
  ccall((:fmpq_mpoly_clear, libflint), Nothing,
        (Ref{QQMPolyRingElem}, Ref{QQMPolyRing}), a, a.parent)
end

mutable struct fmpq_mpoly_factor
  constant_num::Int
  constant_den::Int
  poly::Ptr{Nothing}
  exp::Ptr{Nothing}
  num::Int
  alloc::Int
  # end flint struct

  parent::QQMPolyRing

  function fmpq_mpoly_factor(ctx::QQMPolyRing)
    z = new()
    ccall((:fmpq_mpoly_factor_init, libflint), Nothing,
          (Ref{fmpq_mpoly_factor}, Ref{QQMPolyRing}),
          z, ctx)
    z.parent = ctx
    finalizer(_fmpq_mpoly_factor_clear_fn, z)
    return z
  end
end

function _fmpq_mpoly_factor_clear_fn(f::fmpq_mpoly_factor)
  ccall((:fmpq_mpoly_factor_clear, libflint), Nothing,
        (Ref{fmpq_mpoly_factor}, Ref{QQMPolyRing}),
        f, f.parent)
end

###############################################################################
#
#   zzModMPolyRing / zzModMPolyRingElem
#
###############################################################################

@attributes mutable struct zzModMPolyRing <: MPolyRing{zzModRingElem}
  nvars::Int
  nfields::Int
  ord::Cint
  deg::Cint
  rev::Cint
  lut::NTuple{Base.GMP.BITS_PER_LIMB, Int}
  lut1::NTuple{Base.GMP.BITS_PER_LIMB, UInt8}

  n::UInt
  ninv::UInt
  norm::Int
  # end of flint struct

  base_ring::zzModRing
  S::Vector{Symbol}

  function zzModMPolyRing(R::zzModRing, s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(NmodMPolyID, (R, s, S), cached) do
      if S == :lex
        ord = 0
      elseif S == :deglex
        ord = 1
      elseif S == :degrevlex
        ord = 2
      else
        error("$S is not a valid ordering")
      end

      z = new()
      ccall((:nmod_mpoly_ctx_init, libflint), Nothing,
            (Ref{zzModMPolyRing}, Int, Cint, UInt),
            z, length(s), ord, R.n)
      z.base_ring = R
      z.S = s
      finalizer(_nmod_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end

function _nmod_mpoly_ctx_clear_fn(a::zzModMPolyRing)
  ccall((:nmod_mpoly_ctx_clear, libflint), Nothing,
        (Ref{zzModMPolyRing},), a)
end

const NmodMPolyID = CacheDictType{Tuple{zzModRing, Vector{Symbol}, Symbol}, zzModMPolyRing}()

mutable struct zzModMPolyRingElem <: MPolyRingElem{zzModRingElem}
  coeffs::Ptr{Nothing}
  exps::Ptr{Nothing}
  length::Int
  bits::Int
  coeffs_alloc::Int
  exps_alloc::Int
  # end of flint struct

  parent::zzModMPolyRing

  function zzModMPolyRingElem(ctx::zzModMPolyRing)
    z = new()
    ccall((:nmod_mpoly_init, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Ref{zzModMPolyRing},), z, ctx)
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)
    return z
  end

  function zzModMPolyRingElem(ctx::zzModMPolyRing, a::Vector{zzModRingElem}, b::Vector{Vector{UInt}})
    z = new()
    ccall((:nmod_mpoly_init2, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Int, Ref{zzModMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:nmod_mpoly_push_term_ui_ui, libflint), Nothing,
            (Ref{zzModMPolyRingElem}, UInt, Ptr{UInt}, Ref{zzModMPolyRing}),
            z, a[i].data, b[i], ctx)
    end

    ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Ref{zzModMPolyRing}), z, ctx)
    ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Ref{zzModMPolyRing}), z, ctx)
    return z
  end

  function zzModMPolyRingElem(ctx::zzModMPolyRing, a::Vector{zzModRingElem}, b::Vector{Vector{Int}})
    z = new()
    ccall((:nmod_mpoly_init2, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Int, Ref{zzModMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:nmod_mpoly_push_term_ui_ui, libflint), Nothing,
            (Ref{zzModMPolyRingElem}, UInt, Ptr{Int}, Ref{zzModMPolyRing}),
            z, a[i].data, b[i], ctx)
    end

    ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Ref{zzModMPolyRing}), z, ctx)
    ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Ref{zzModMPolyRing}), z, ctx)
    return z
  end

  function zzModMPolyRingElem(ctx::zzModMPolyRing, a::Vector{zzModRingElem}, b::Vector{Vector{ZZRingElem}})
    z = new()
    ccall((:nmod_mpoly_init2, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Int, Ref{zzModMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:nmod_mpoly_push_term_ui_fmpz, libflint), Nothing,
            (Ref{zzModMPolyRingElem}, UInt, Ptr{Ref{ZZRingElem}}, Ref{zzModMPolyRing}),
            z, a[i].data, b[i], ctx)
    end

    ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Ref{zzModMPolyRing}), z, ctx)
    ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Ref{zzModMPolyRing}), z, ctx)
    return z
  end

  function zzModMPolyRingElem(ctx::zzModMPolyRing, a::UInt)
    z = new()
    ccall((:nmod_mpoly_init, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Ref{zzModMPolyRing},), z, ctx)
    ccall((:nmod_mpoly_set_ui, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, UInt, Ref{zzModMPolyRing}), z, a, ctx)
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)
    return z
  end

  function zzModMPolyRingElem(ctx::zzModMPolyRing, a::zzModRingElem)
    z = new()
    ccall((:nmod_mpoly_init, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, Ref{zzModMPolyRing},), z, ctx)
    ccall((:nmod_mpoly_set_ui, libflint), Nothing,
          (Ref{zzModMPolyRingElem}, UInt, Ref{zzModMPolyRing}), z, a.data, ctx)
    finalizer(_nmod_mpoly_clear_fn, z)
    z.parent = ctx
    return z
  end
end

function _nmod_mpoly_clear_fn(a::zzModMPolyRingElem)
  ccall((:nmod_mpoly_clear, libflint), Nothing,
        (Ref{zzModMPolyRingElem}, Ref{zzModMPolyRing}), a, a.parent)
end

mutable struct nmod_mpoly_factor
  constant::UInt
  poly::Ptr{Nothing}
  exp::Ptr{Nothing}
  num::Int
  alloc::Int
  # end flint struct

  parent::zzModMPolyRing

  function nmod_mpoly_factor(ctx::zzModMPolyRing)
    z = new()
    ccall((:nmod_mpoly_factor_init, libflint), Nothing,
          (Ref{nmod_mpoly_factor}, Ref{zzModMPolyRing}),
          z, ctx)
    z.parent = ctx
    finalizer(_nmod_mpoly_factor_clear_fn, z)
    return z
  end
end

function _nmod_mpoly_factor_clear_fn(f::nmod_mpoly_factor)
  ccall((:nmod_mpoly_factor_clear, libflint), Nothing,
        (Ref{nmod_mpoly_factor}, Ref{zzModMPolyRing}),
        f, f.parent)
end

################################################################################
#
#   fpMPolyRing / fpMPolyRingElem
#
###############################################################################

@attributes mutable struct fpMPolyRing <: MPolyRing{fpFieldElem}
  nvars::Int
  nfields::Int
  ord::Cint
  deg::Cint
  rev::Cint
  lut::NTuple{Base.GMP.BITS_PER_LIMB, Int}
  lut1::NTuple{Base.GMP.BITS_PER_LIMB, UInt8}

  n::UInt
  ninv::UInt
  norm::Int
  # end of flint struct

  base_ring::fpField
  S::Vector{Symbol}

  function fpMPolyRing(R::fpField, s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(GFPMPolyID, (R, s, S), cached) do
      if S == :lex
        ord = 0
      elseif S == :deglex
        ord = 1
      elseif S == :degrevlex
        ord = 2
      else
        error("$S is not a valid ordering")
      end

      z = new()
      ccall((:nmod_mpoly_ctx_init, libflint), Nothing,
            (Ref{fpMPolyRing}, Int, Cint, UInt),
            z, length(s), ord, UInt(modulus(R)))
      z.base_ring = R
      z.S = s
      finalizer(_gfp_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end

function _gfp_mpoly_ctx_clear_fn(a::fpMPolyRing)
  ccall((:nmod_mpoly_ctx_clear, libflint), Nothing,
        (Ref{fpMPolyRing},), a)
end

const GFPMPolyID = CacheDictType{Tuple{fpField, Vector{Symbol}, Symbol}, fpMPolyRing}()

mutable struct fpMPolyRingElem <: MPolyRingElem{fpFieldElem}
  coeffs::Ptr{Nothing}
  exps::Ptr{Nothing}
  length::Int
  bits::Int
  coeffs_alloc::Int
  exps_alloc::Int
  # end of flint struct

  parent::fpMPolyRing

  function fpMPolyRingElem(ctx::fpMPolyRing)
    z = new()
    ccall((:nmod_mpoly_init, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Ref{fpMPolyRing}),
          z, ctx)
    z.parent = ctx
    finalizer(_gfp_mpoly_clear_fn, z)
    return z
  end

  function fpMPolyRingElem(ctx::fpMPolyRing, a::Vector{fpFieldElem}, b::Vector{Vector{UInt}})
    z = new()
    ccall((:nmod_mpoly_init2, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Int, Ref{fpMPolyRing}),
          z, length(a), ctx)
    z.parent = ctx
    finalizer(_gfp_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:nmod_mpoly_push_term_ui_ui, libflint), Nothing,
            (Ref{fpMPolyRingElem}, UInt, Ptr{UInt}, Ref{fpMPolyRing}),
            z, a[i].data, b[i], ctx)
    end

    ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Ref{fpMPolyRing}),
          z, ctx)
    ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Ref{fpMPolyRing}),
          z, ctx)
    return z
  end

  function fpMPolyRingElem(ctx::fpMPolyRing, a::Vector{fpFieldElem}, b::Vector{Vector{Int}})
    z = new()
    ccall((:nmod_mpoly_init2, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Int, Ref{fpMPolyRing}),
          z, length(a), ctx)
    z.parent = ctx
    finalizer(_gfp_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:nmod_mpoly_push_term_ui_ui, libflint), Nothing,
            (Ref{fpMPolyRingElem}, UInt, Ptr{Int}, Ref{fpMPolyRing}),
            z, a[i].data, b[i], ctx)
    end

    ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Ref{fpMPolyRing}),
          z, ctx)
    ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Ref{fpMPolyRing}),
          z, ctx)
    return z
  end

  function fpMPolyRingElem(ctx::fpMPolyRing, a::Vector{fpFieldElem}, b::Vector{Vector{ZZRingElem}})
    z = new()
    ccall((:nmod_mpoly_init2, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Int, Ref{fpMPolyRing}),
          z, length(a), ctx)
    z.parent = ctx
    finalizer(_gfp_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:nmod_mpoly_push_term_ui_fmpz, libflint), Nothing,
            (Ref{fpMPolyRingElem}, UInt, Ptr{Ref{ZZRingElem}}, Ref{fpMPolyRing}),
            z, a[i].data, b[i], ctx)
    end

    ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Ref{fpMPolyRing}),
          z, ctx)
    ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Ref{fpMPolyRing}),
          z, ctx)
    return z
  end

  function fpMPolyRingElem(ctx::fpMPolyRing, a::UInt)
    z = new()
    ccall((:nmod_mpoly_init, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Ref{fpMPolyRing}),
          z, ctx)
    ccall((:nmod_mpoly_set_ui, libflint), Nothing,
          (Ref{fpMPolyRingElem}, UInt, Ref{fpMPolyRing}),
          z, a, ctx)
    z.parent = ctx
    finalizer(_gfp_mpoly_clear_fn, z)
    return z
  end

  function fpMPolyRingElem(ctx::fpMPolyRing, a::fpFieldElem)
    z = new()
    ccall((:nmod_mpoly_init, libflint), Nothing,
          (Ref{fpMPolyRingElem}, Ref{fpMPolyRing}),
          z, ctx)
    ccall((:nmod_mpoly_set_ui, libflint), Nothing,
          (Ref{fpMPolyRingElem}, UInt, Ref{fpMPolyRing}),
          z, a.data, ctx)
    finalizer(_gfp_mpoly_clear_fn, z)
    z.parent = ctx
    return z
  end
end

function _gfp_mpoly_clear_fn(a::fpMPolyRingElem)
  ccall((:nmod_mpoly_clear, libflint), Nothing,
        (Ref{fpMPolyRingElem}, Ref{fpMPolyRing}),
        a, a.parent)
end

mutable struct gfp_mpoly_factor
  constant::UInt
  poly::Ptr{Nothing}
  exp::Ptr{Nothing}
  num::Int
  alloc::Int
  # end flint struct

  parent::fpMPolyRing

  function gfp_mpoly_factor(ctx::fpMPolyRing)
    z = new()
    ccall((:nmod_mpoly_factor_init, libflint), Nothing,
          (Ref{gfp_mpoly_factor}, Ref{fpMPolyRing}),
          z, ctx)
    z.parent = ctx
    finalizer(_gfp_mpoly_factor_clear_fn, z)
    return z
  end
end

function _gfp_mpoly_factor_clear_fn(f::gfp_mpoly_factor)
  ccall((:nmod_mpoly_factor_clear, libflint), Nothing,
        (Ref{gfp_mpoly_factor}, Ref{fpMPolyRing}),
        f, f.parent)
end

################################################################################
#
#   FpMPolyRing / FpMPolyRingElem
#
###############################################################################

@attributes mutable struct FpMPolyRing <: MPolyRing{FpFieldElem}
  nvars::Int
  nfields::Int
  ord::Cint
  deg::Cint
  rev::Cint
  lut::NTuple{Base.GMP.BITS_PER_LIMB, Int}
  lut1::NTuple{Base.GMP.BITS_PER_LIMB, UInt8}

  n::Int # fmpz_t
  add_fxn::Ptr{Nothing}
  sub_fxn::Ptr{Nothing}
  mul_fxn::Ptr{Nothing}
  n2::UInt
  ninv::UInt
  norm::UInt
  n_limbs::Tuple{UInt, UInt, UInt}
  ninv_limbs::Tuple{UInt, UInt, UInt}
  ninv_huge::Ptr{Nothing} # fmpz_preinvn_struct
  # end of flint struct

  base_ring::FpField
  S::Vector{Symbol}

  function FpMPolyRing(R::FpField, s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(GFPFmpzMPolyID, (R, s, S), cached) do
      if S == :lex
        ord = 0
      elseif S == :deglex
        ord = 1
      elseif S == :degrevlex
        ord = 2
      else
        error("$S is not a valid ordering")
      end

      z = new()
      ccall((:fmpz_mod_mpoly_ctx_init, libflint), Nothing,
            (Ref{FpMPolyRing}, Int, Cint, Ref{ZZRingElem}),
            z, length(s), ord, modulus(R))
      z.base_ring = R
      z.S = s
      finalizer(_gfp_fmpz_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end

function _gfp_fmpz_mpoly_ctx_clear_fn(a::FpMPolyRing)
  ccall((:fmpz_mod_mpoly_ctx_clear, libflint), Nothing,
        (Ref{FpMPolyRing},), a)
end

const GFPFmpzMPolyID = CacheDictType{Tuple{FpField, Vector{Symbol}, Symbol}, FpMPolyRing}()

mutable struct FpMPolyRingElem <: MPolyRingElem{FpFieldElem}
  coeffs::Ptr{Nothing}
  exps::Ptr{Nothing}
  length::Int
  bits::Int
  coeffs_alloc::Int
  exps_alloc::Int
  # end of flint struct

  parent::FpMPolyRing

  function FpMPolyRingElem(ctx::FpMPolyRing)
    z = new()
    ccall((:fmpz_mod_mpoly_init, libflint), Nothing,
          (Ref{FpMPolyRingElem}, Ref{FpMPolyRing}),
          z, ctx)
    z.parent = ctx
    finalizer(_gfp_fmpz_mpoly_clear_fn, z)
    return z
  end

  function FpMPolyRingElem(ctx::FpMPolyRing, a::Vector{FpFieldElem}, b::Vector{Vector{T}}) where T <: Union{UInt, Int, ZZRingElem}
    z = new()
    ccall((:fmpz_mod_mpoly_init2, libflint), Nothing,
          (Ref{FpMPolyRingElem}, Int, Ref{FpMPolyRing}),
          z, length(a), ctx)
    z.parent = ctx
    finalizer(_gfp_fmpz_mpoly_clear_fn, z)

    for i in 1:length(a)
      if T == ZZRingElem
        ccall((:fmpz_mod_mpoly_push_term_fmpz_fmpz, libflint), Nothing,
              (Ref{FpMPolyRingElem}, Ref{ZZRingElem}, Ptr{Ref{ZZRingElem}}, Ref{FpMPolyRing}),
              z, a[i].data, b[i], ctx)
      else
        ccall((:fmpz_mod_mpoly_push_term_fmpz_ui, libflint), Nothing,
              (Ref{FpMPolyRingElem}, Ref{ZZRingElem}, Ptr{UInt}, Ref{FpMPolyRing}),
              z, a[i].data, b[i], ctx)
      end
    end

    ccall((:fmpz_mod_mpoly_sort_terms, libflint), Nothing,
          (Ref{FpMPolyRingElem}, Ref{FpMPolyRing}),
          z, ctx)
    ccall((:fmpz_mod_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{FpMPolyRingElem}, Ref{FpMPolyRing}),
          z, ctx)
    return z
  end

  function FpMPolyRingElem(ctx::FpMPolyRing, a::Union{ZZRingElem, FpFieldElem})
    z = new()
    ccall((:fmpz_mod_mpoly_init, libflint), Nothing,
          (Ref{FpMPolyRingElem}, Ref{FpMPolyRing}),
          z, ctx)
    ccall((:fmpz_mod_mpoly_set_fmpz, libflint), Nothing,
          (Ref{FpMPolyRingElem}, Ref{ZZRingElem}, Ref{FpMPolyRing}),
          z, a isa ZZRingElem ? a : data(a), ctx)
    z.parent = ctx
    finalizer(_gfp_fmpz_mpoly_clear_fn, z)
    return z
  end
end

function _gfp_fmpz_mpoly_clear_fn(a::FpMPolyRingElem)
  ccall((:fmpz_mod_mpoly_clear, libflint), Nothing,
        (Ref{FpMPolyRingElem}, Ref{FpMPolyRing}),
        a, a.parent)
end

mutable struct gfp_fmpz_mpoly_factor
  constant::Int
  poly::Ptr{Nothing}
  exp::Ptr{Nothing}
  num::Int
  alloc::Int
  # end flint struct

  parent::FpMPolyRing

  function gfp_fmpz_mpoly_factor(ctx::FpMPolyRing)
    z = new()
    ccall((:fmpz_mod_mpoly_factor_init, libflint), Nothing,
          (Ref{gfp_fmpz_mpoly_factor}, Ref{FpMPolyRing}),
          z, ctx)
    z.parent = ctx
    finalizer(_gfp_fmpz_mpoly_factor_clear_fn, z)
    return z
  end
end

function _gfp_fmpz_mpoly_factor_clear_fn(f::gfp_fmpz_mpoly_factor)
  ccall((:fmpz_mod_mpoly_factor_clear, libflint), Nothing,
        (Ref{gfp_fmpz_mpoly_factor}, Ref{FpMPolyRing}),
        f, f.parent)
end

###############################################################################
#
#   fqPolyRepField / fqPolyRepFieldElem
#
###############################################################################

@doc raw"""
    fqPolyRepField <: FinField

A finite field. Implemented as $\mathbb F_p[t]/f$ for the prime $p$ being an [`Int`](@ref).
See [`FqPolyRepField`](@ref) for $p$ being a [`ZZRingElem`](@ref). See [`fqPolyRepFieldElem`](@ref) for elements.
"""
@attributes mutable struct fqPolyRepField <: FinField
  n :: UInt
  ninv :: UInt
  norm :: UInt
  sparse_modulus :: Cint
  is_conway :: Cint
  a :: Ptr{Nothing}
  j :: Ptr{Nothing}
  len :: Int
  mod_coeffs :: Ptr{Nothing}
  mod_alloc :: Int
  mod_length :: Int
  mod_n :: Int
  mod_ninv :: Int
  mod_norm :: Int
  inv_coeffs :: Ptr{Nothing}
  inv_alloc :: Int
  inv_length :: Int
  inv_n :: Int
  inv_ninv :: Int
  inv_norm :: Int
  var :: Ptr{Nothing}
  # end of flint struct

  overfields :: Dict{Int, Vector{FinFieldMorphism}}
  subfields :: Dict{Int, Vector{FinFieldMorphism}}

  function fqPolyRepField(c::UInt, deg::Int, s::Symbol, cached::Bool = true; check::Bool = true)
    check && !is_prime(c) &&
    throw(DomainError(c, "the characteristic must be a prime"))
    return get_cached!(FqNmodFiniteFieldID, (c, deg, s), cached) do
      d = new()
      ccall((:fq_nmod_ctx_init_ui, libflint), Nothing,
            (Ref{fqPolyRepField}, UInt, Int, Ptr{UInt8}),
            d, c, deg, string(s))
      finalizer(_FqNmodFiniteField_clear_fn, d)
      return d
    end
  end

  function fqPolyRepField(c::T, deg::Int, s::Symbol, cached::Bool = true; check::Bool = true) where T <: Union{Int, ZZRingElem}
    return fqPolyRepField(UInt(c), deg, s, cached, check = check)
  end

  function fqPolyRepField(f::zzModPolyRingElem, s::Symbol, cached::Bool = true; check::Bool = true)
    check && !is_prime(modulus(f)) &&
    throw(DomainError(base_ring(f), "the base ring of the polynomial must be a field"))
    return get_cached!(FqNmodFiniteFieldIDNmodPol, (parent(f), f, s), cached) do
      z = new()
      ccall((:fq_nmod_ctx_init_modulus, libflint), Nothing,
            (Ref{fqPolyRepField}, Ref{zzModPolyRingElem}, Ptr{UInt8}),
            z, f, string(s))
      finalizer(_FqNmodFiniteField_clear_fn, z)
      return z
    end
  end

  function fqPolyRepField(f::fpPolyRingElem, s::Symbol, cached::Bool = true; check::Bool=true)
    # check ignored
    return get_cached!(FqNmodFiniteFieldIDGFPPol, (parent(f), f, s), cached) do
      z = new()
      ccall((:fq_nmod_ctx_init_modulus, libflint), Nothing,
            (Ref{fqPolyRepField}, Ref{fpPolyRingElem}, Ptr{UInt8}),
            z, f, string(s))
      finalizer(_FqNmodFiniteField_clear_fn, z)
      return z
    end
  end

end

const FqNmodFiniteFieldID = CacheDictType{Tuple{UInt, Int, Symbol}, fqPolyRepField}()

const FqNmodFiniteFieldIDNmodPol = CacheDictType{Tuple{zzModPolyRing, zzModPolyRingElem, Symbol},
                                                 fqPolyRepField}()

const FqNmodFiniteFieldIDGFPPol = CacheDictType{Tuple{fpPolyRing, fpPolyRingElem, Symbol},
                                                fqPolyRepField}()


function _FqNmodFiniteField_clear_fn(a :: fqPolyRepField)
  ccall((:fq_nmod_ctx_clear, libflint), Nothing, (Ref{fqPolyRepField},), a)
end

@doc raw"""
    fqPolyRepFieldElem <: FinFieldElem

An element $\sum_{i=0}^{d-1} a_i t^i$ of a finite field $\mathbb F_{p^d} \cong \mathbb F_p[t]/f$.
Represented internally as $(a_i)_{0\le i<d}$. See [`fqPolyRepField`](@ref).
"""
mutable struct fqPolyRepFieldElem <: FinFieldElem
  coeffs :: Ptr{Nothing}
  alloc :: Int
  length :: Int
  n :: Int
  ninv :: Int
  norm :: Int
  parent::fqPolyRepField

  function fqPolyRepFieldElem(ctx::fqPolyRepField)
    d = new()
    ccall((:fq_nmod_init2, libflint), Nothing,
          (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, ctx)
    d.parent = ctx
    finalizer(_fq_nmod_clear_fn, d)
    return d
  end

  function fqPolyRepFieldElem(ctx::fqPolyRepField, x::Int)
    d = new()
    ccall((:fq_nmod_init2, libflint), Nothing,
          (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, ctx)
    d.parent = ctx
    finalizer(_fq_nmod_clear_fn, d)
    set!(d, x)
    return d
  end

  function fqPolyRepFieldElem(ctx::fqPolyRepField, x::ZZRingElem)
    d = new()
    ccall((:fq_nmod_init2, libflint), Nothing,
          (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, ctx)
    d.parent = ctx
    finalizer(_fq_nmod_clear_fn, d)
    set!(d, x)
    return d
  end

  function fqPolyRepFieldElem(ctx::fqPolyRepField, x::fqPolyRepFieldElem)
    d = new()
    ccall((:fq_nmod_init2, libflint), Nothing,
          (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, ctx)
    d.parent = ctx
    finalizer(_fq_nmod_clear_fn, d)
    set!(d, x)
    return d
  end

  function fqPolyRepFieldElem(ctx::fqPolyRepField, x::fpPolyRingElem)
    d = new()
    ccall((:fq_nmod_init2, libflint), Nothing,
          (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, ctx)
    d.parent = ctx
    finalizer(_fq_nmod_clear_fn, d)
    set!(d, x)
    return d
  end
end

function _fq_nmod_clear_fn(a::fqPolyRepFieldElem)
  ccall((:fq_nmod_clear, libflint), Nothing,
        (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), a, a.parent)
end

###############################################################################
#
#   FqField / FqFieldElem
#
###############################################################################

@doc raw"""
    FqField <: FinField

A finite field. The constructor automatically determines a fast implementation.
"""
@attributes mutable struct FqField <: FinField
  # gr_ctx_struct
  data::NTuple{6 * sizeof(UInt), Int8}
  which_ring::UInt
  sizeof_elem::Int
  methods::Ptr{Nothing}
  size_limit::UInt
  # end of flint struct

  var::String

  overfields::Dict{Int, Vector{FinFieldMorphism}}
  subfields::Dict{Int, Vector{FinFieldMorphism}}

  isstandard::Bool
  # isstandard means, that defining_polynomial(F) === modulus(F)
  # In particular, we can use the fast fq_default_get/set_*_poly
  # functions to go from polynomials to field elements
  isabsolute::Bool
  base_field
  defining_poly
  forwardmap
  backwardmap
  preimage_basefield
  image_basefield

  function FqField(char::ZZRingElem, deg::Int, s::Symbol, cached::Bool = true)
    return get_cached!(FqDefaultFiniteFieldID, (char, deg, s), cached) do
      d = new()
      d.var = string(s)
      d.isabsolute = true
      d.isstandard = true
      finalizer(_FqDefaultFiniteField_clear_fn, d)
      ccall((:fq_default_ctx_init, libflint), Nothing,
            (Ref{FqField}, Ref{ZZRingElem}, Int, Ptr{UInt8}),
            d, char, deg, d.var)
      return d
    end
  end

  function FqField(f::ZZModPolyRingElem, s::Symbol, cached::Bool = true; check::Bool = true)
    check && !is_probable_prime(modulus(f)) &&
    throw(DomainError(base_ring(f), "the base ring of the polynomial must be a field"))
    return get_cached!(FqDefaultFiniteFieldIDFmpzPol, (f, s), cached) do
      z = new()
      z.isabsolute = true
      z.isstandard = true
      z.var = string(s)
      ccall((:fq_default_ctx_init_modulus, libflint), Nothing,
            (Ref{FqField}, Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}, Ptr{UInt8}),
            z, f, base_ring(parent(f)).ninv, string(s))
      finalizer(_FqDefaultFiniteField_clear_fn, z)
      return z
    end
  end

  function FqField(f::FpPolyRingElem, s::Symbol, cached::Bool = true;  check::Bool = true)
    # check ignored
    return get_cached!(FqDefaultFiniteFieldIDGFPPol, (f, s), cached) do
      z = new()
      z.isabsolute = true
      z.isstandard = true
      z.var = string(s)
      ccall((:fq_default_ctx_init_modulus, libflint), Nothing,
            (Ref{FqField}, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}, Ptr{UInt8}),
            z, f, base_ring(parent(f)).ninv, string(s))
      finalizer(_FqDefaultFiniteField_clear_fn, z)
      return z
    end
  end

  function FqField(f::zzModPolyRingElem, s::Symbol, cached::Bool = true; check::Bool = true)
    check && !is_prime(modulus(f)) &&
    throw(DomainError(base_ring(f), "the base ring of the polynomial must be a field"))
    return get_cached!(FqDefaultFiniteFieldIDNmodPol, (f, s), cached) do
      z = new()
      z.isabsolute = true
      z.isstandard = true
      z.var = string(s)
      ccall((:fq_default_ctx_init_modulus_nmod, libflint), Nothing,
            (Ref{FqField}, Ref{zzModPolyRingElem}, Ptr{UInt8}),
            z, f, string(s))
      finalizer(_FqDefaultFiniteField_clear_fn, z)
      return z
    end
  end

  function FqField(f::fpPolyRingElem, s::Symbol, cached::Bool = true; check::Bool = true)
    # check ignored
    return get_cached!(FqDefaultFiniteFieldIDGFPNmodPol, (f, s), cached) do
      z = new()
      z.isabsolute = true
      z.isstandard = true
      z.var = string(s)
      ccall((:fq_default_ctx_init_modulus_nmod, libflint), Nothing,
            (Ref{FqField}, Ref{fpPolyRingElem}, Ptr{UInt8}),
            z, f, string(s))
      finalizer(_FqDefaultFiniteField_clear_fn, z)
      return z
    end
  end
end

const FqDefaultFiniteFieldID = CacheDictType{Tuple{ZZRingElem, Int, Symbol}, FqField}()

const FqDefaultFiniteFieldIDFmpzPol = CacheDictType{Tuple{ZZModPolyRingElem, Symbol}, FqField}()

const FqDefaultFiniteFieldIDGFPPol = CacheDictType{Tuple{FpPolyRingElem, Symbol}, FqField}()

const FqDefaultFiniteFieldIDNmodPol = CacheDictType{Tuple{zzModPolyRingElem, Symbol}, FqField}()

const FqDefaultFiniteFieldIDGFPNmodPol = CacheDictType{Tuple{fpPolyRingElem, Symbol}, FqField}()

function _FqDefaultFiniteField_clear_fn(a :: FqField)
  ccall((:fq_default_ctx_clear, libflint), Nothing, (Ref{FqField},), a)
end

@doc raw"""
    FqFieldElem <: FinFieldElem

An element of a finite field. See [`FqField`](@ref).
"""
mutable struct FqFieldElem <: FinFieldElem
  opaque::NTuple{48, Int8} # fq_default_struct is 48 bytes on a 64 bit machine
  parent::FqField
  poly#::Union{Nothing, FqPolyRingElem}

  function FqFieldElem(ctx::FqField)
    d = new()
    ccall((:fq_default_init2, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{FqField}), d, ctx)
    if _fq_default_ctx_type(ctx) != _FQ_DEFAULT_NMOD
      finalizer(_fq_default_clear_fn, d)
    end
    d.poly = nothing
    d.parent = ctx
    return d
  end

  function FqFieldElem(ctx::FqField, x::Int)
    d = FqFieldElem(ctx)
    ccall((:fq_default_set_si, libflint), Nothing,
          (Ref{FqFieldElem}, Int, Ref{FqField}), d, x, ctx)
    return d
  end

  function FqFieldElem(ctx::FqField, x::ZZRingElem)
    d = FqFieldElem(ctx)
    ccall((:fq_default_set_fmpz, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{ZZRingElem}, Ref{FqField}), d, x, ctx)
    return d
  end

  function FqFieldElem(ctx::FqField, x::ZZPolyRingElem)
    d = FqFieldElem(ctx)
    ccall((:fq_default_set_fmpz_poly, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{ZZPolyRingElem}, Ref{FqField}), d, x, ctx)
    return d
  end

  function FqFieldElem(ctx::FqField, x::zzModPolyRingElem)
    d = FqFieldElem(ctx)
    ccall((:fq_default_set_nmod_poly, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{zzModPolyRingElem}, Ref{FqField}), d, x, ctx)
    return d
  end

  function FqFieldElem(ctx::FqField, x::fpPolyRingElem)
    d = FqFieldElem(ctx)
    ccall((:fq_default_set_nmod_poly, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{fpPolyRingElem}, Ref{FqField}), d, x, ctx)
    return d
  end

  function FqFieldElem(ctx::FqField, x::ZZModPolyRingElem)
    d = FqFieldElem(ctx)
    ccall((:fq_default_set_fmpz_mod_poly, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{ZZModPolyRingElem}, Ref{FqField}), d, x, ctx)
    return d
  end

  function FqFieldElem(ctx::FqField, x::FpPolyRingElem)
    d = FqFieldElem(ctx)
    ccall((:fq_default_set_fmpz_mod_poly, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{FpPolyRingElem}, Ref{FqField}), d, x, ctx)
    return d
  end

  function FqFieldElem(ctx::FqField, x::FqFieldElem)
    d = FqFieldElem(ctx)
    ccall((:fq_default_set, libflint), Nothing,
          (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), d, x, ctx)
    return d
  end
end

function _fq_default_clear_fn(a::FqFieldElem)
  ccall((:fq_default_clear, libflint), Nothing,
        (Ref{FqFieldElem}, Ref{FqField}), a, a.parent)
end

###############################################################################
#
#   FqPolyRepField / FqPolyRepFieldElem
#
###############################################################################

@doc raw"""
    FqPolyRepField <: FinField

A finite field. Implemented as $\mathbb F_p[t]/f$ for the prime $p$ being a [`ZZRingElem`](@ref).
See [`fqPolyRepField`](@ref) for $p$ being an [`Int`](@ref). See [`FqPolyRepFieldElem`](@ref) for elements.
"""
@attributes mutable struct FqPolyRepField <: FinField
  p::Int # fmpz_t
  add_fxn::Ptr{Nothing}
  sub_fxn::Ptr{Nothing}
  mul_fxn::Ptr{Nothing}
  n2::UInt
  ninv::UInt
  norm::UInt
  n_limbs::Tuple{UInt, UInt, UInt}
  ninv_limbs::Tuple{UInt, UInt, UInt}
  ninv_huge::Ptr{Nothing} # fmpz_preinvn_struct

  sparse_modulus :: Cint
  is_conway :: Cint
  a::Ptr{Nothing}
  j::Ptr{Nothing}
  len::Int
  mod_coeffs::Ptr{Nothing}
  mod_alloc::Int
  mod_length::Int
  inv_coeffs::Ptr{Nothing}
  inv_alloc::Int
  inv_length::Int
  var::Ptr{Nothing}
  # end of flint struct

  overfields :: Dict{Int, Vector{FinFieldMorphism}}
  subfields :: Dict{Int, Vector{FinFieldMorphism}}

  function FqPolyRepField(char::ZZRingElem, deg::Int, s::Symbol, cached::Bool = true; check::Bool = true)
    check && !is_probable_prime(char) &&
    throw(DomainError(char, "the characteristic must be a prime"))
    return get_cached!(FqFiniteFieldID, (char, deg, s), cached) do
      d = new()
      finalizer(_FqFiniteField_clear_fn, d)
      ccall((:fq_ctx_init, libflint), Nothing,
            (Ref{FqPolyRepField}, Ref{ZZRingElem}, Int, Ptr{UInt8}),
            d, char, deg, string(s))
      return d
    end
  end

  function FqPolyRepField(f::ZZModPolyRingElem, s::Symbol, cached::Bool = true; check::Bool = true)
    check && !is_probable_prime(modulus(f)) &&
    throw(DomainError(base_ring(f), "the base ring of the polynomial must be a field"))
    return get_cached!(FqFiniteFieldIDFmpzPol, (f, s), cached) do
      z = new()
      ccall((:fq_ctx_init_modulus, libflint), Nothing,
            (Ref{FqPolyRepField}, Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}, Ptr{UInt8}),
            z, f, base_ring(parent(f)).ninv, string(s))
      finalizer(_FqFiniteField_clear_fn, z)
      return z
    end
  end

  function FqPolyRepField(f::FpPolyRingElem, s::Symbol, cached::Bool = true; check::Bool = true)
    # check ignored
    return get_cached!(FqFiniteFieldIDGFPPol, (f, s), cached) do
      z = new()
      ccall((:fq_ctx_init_modulus, libflint), Nothing,
            (Ref{FqPolyRepField}, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}, Ptr{UInt8}),
            z, f, base_ring(parent(f)).ninv, string(s))
      finalizer(_FqFiniteField_clear_fn, z)
      return z
    end
  end
end


const FqFiniteFieldID = CacheDictType{Tuple{ZZRingElem, Int, Symbol}, FqPolyRepField}()

const FqFiniteFieldIDFmpzPol = CacheDictType{Tuple{ZZModPolyRingElem, Symbol}, FqPolyRepField}()

const FqFiniteFieldIDGFPPol = CacheDictType{Tuple{FpPolyRingElem, Symbol}, FqPolyRepField}()

function _FqFiniteField_clear_fn(a :: FqPolyRepField)
  ccall((:fq_ctx_clear, libflint), Nothing, (Ref{FqPolyRepField},), a)
end

@doc raw"""
    FqPolyRepFieldElem <: FinFieldElem

An element $\sum_{i=0}^{d-1} a_i t^i$ of a finite field $\mathbb F_{p^d} \cong \mathbb F_p[t]/f$.
Represented internally as $(a_i)_{0\le i<d}$. See [`FqPolyRepField`](@ref).
"""
mutable struct FqPolyRepFieldElem <: FinFieldElem
  coeffs :: Ptr{Nothing}
  alloc :: Int
  length :: Int
  parent::FqPolyRepField

  function FqPolyRepFieldElem(ctx::FqPolyRepField)
    d = new()
    ccall((:fq_init2, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), d, ctx)
    finalizer(_fq_clear_fn, d)
    d.parent = ctx
    return d
  end

  function FqPolyRepFieldElem(ctx::FqPolyRepField, x::Int)
    d = new()
    ccall((:fq_init2, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), d, ctx)
    finalizer(_fq_clear_fn, d)
    ccall((:fq_set_si, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Int, Ref{FqPolyRepField}), d, x, ctx)
    d.parent = ctx
    return d
  end

  function FqPolyRepFieldElem(ctx::FqPolyRepField, x::ZZRingElem)
    d = new()
    ccall((:fq_init2, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), d, ctx)
    finalizer(_fq_clear_fn, d)
    ccall((:fq_set_fmpz, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{FqPolyRepField}), d, x, ctx)
    d.parent = ctx
    return d
  end

  function FqPolyRepFieldElem(ctx::FqPolyRepField, x::FpPolyRingElem)
    d = new()
    ccall((:fq_init2, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), d, ctx)
    finalizer(_fq_clear_fn, d)
    ccall((:fq_set_fmpz_mod_poly, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ref{FpPolyRingElem}, Ref{FqPolyRepField}),
          d, x, ctx)
    d.parent = ctx
    return d
  end

  function FqPolyRepFieldElem(ctx::FqPolyRepField, x::FqPolyRepFieldElem)
    d = new()
    ccall((:fq_init2, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), d, ctx)
    finalizer(_fq_clear_fn, d)
    ccall((:fq_set, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), d, x, ctx)
    d.parent = ctx
    return d
  end

  function FqPolyRepFieldElem(ctx::FqPolyRepField, x::ZZPolyRingElem)
    d = new()
    ccall((:fq_init2, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), d, ctx)
    finalizer(_fq_clear_fn, d)
    ccall((:fq_set_fmpz_poly, libflint), Nothing,
          (Ref{FqPolyRepFieldElem}, Ref{ZZPolyRingElem}, Ref{FqPolyRepField}), d, x, ctx)
    d.parent = ctx
    return d
  end
end

function _fq_clear_fn(a::FqPolyRepFieldElem)
  ccall((:fq_clear, libflint), Nothing,
        (Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), a, a.parent)
end


###############################################################################
#
#   fqPolyRepMPolyRing / fqPolyRepMPolyRingElem
#
###############################################################################

@attributes mutable struct fqPolyRepMPolyRing <: MPolyRing{fqPolyRepFieldElem}
  nvars::Int
  nfields::Int
  ord::Cint
  deg::Cint
  rev::Cint
  lut::NTuple{Base.GMP.BITS_PER_LIMB, Int}
  lut1::NTuple{Base.GMP.BITS_PER_LIMB, UInt8}

  p :: Int
  n :: Int
  ninv :: Int
  norm :: Int
  sparse_modulus :: Cint
  is_conway :: Cint
  a :: Ptr{Nothing}
  j :: Ptr{Nothing}
  len :: Int
  mod_coeffs :: Ptr{Nothing}
  mod_alloc :: Int
  mod_length :: Int
  mod_n :: Int
  mod_ninv :: Int
  mod_norm :: Int
  inv_coeffs :: Ptr{Nothing}
  inv_alloc :: Int
  inv_length :: Int
  inv_n :: Int
  inv_ninv :: Int
  inv_norm :: Int
  var :: Ptr{Nothing}
  # end of flint struct

  base_ring::fqPolyRepField
  S::Vector{Symbol}

  function fqPolyRepMPolyRing(R::fqPolyRepField, s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(FqNmodMPolyID, (R, s, S), cached) do
      if S == :lex
        ord = 0
      elseif S == :deglex
        ord = 1
      elseif S == :degrevlex
        ord = 2
      else
        error("$S is not a valid ordering")
      end

      z = new()
      ccall((:fq_nmod_mpoly_ctx_init, libflint), Nothing,
            (Ref{fqPolyRepMPolyRing}, Int, Cint, Ref{fqPolyRepField}),
            z, length(s), ord, R)
      z.base_ring = R
      z.S = s
      finalizer(_fq_nmod_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end

function _fq_nmod_mpoly_ctx_clear_fn(a::fqPolyRepMPolyRing)
  ccall((:fq_nmod_mpoly_ctx_clear, libflint), Nothing,
        (Ref{fqPolyRepMPolyRing},), a)
end

const FqNmodMPolyID = CacheDictType{Tuple{fqPolyRepField, Vector{Symbol}, Symbol}, fqPolyRepMPolyRing}()

mutable struct fqPolyRepMPolyRingElem <: MPolyRingElem{fqPolyRepFieldElem}
  coeffs::Ptr{Nothing}
  exps::Ptr{Nothing}
  length::Int
  bits::Int
  coeffs_alloc::Int
  exps_alloc::Int
  # end of flint struct

  parent::fqPolyRepMPolyRing

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing)
    z = new()
    ccall((:fq_nmod_mpoly_init, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing},), z, ctx)
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_clear_fn, z)
    return z
  end

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing, a::Vector{fqPolyRepFieldElem}, b::Vector{Vector{UInt}})
    z = new()
    ccall((:fq_nmod_mpoly_init2, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:fq_nmod_mpoly_push_term_fq_nmod_ui, libflint), Nothing,
            (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepFieldElem}, Ptr{UInt}, Ref{fqPolyRepMPolyRing}),
            z, a[i], b[i], ctx)
    end

    ccall((:fq_nmod_mpoly_sort_terms, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}), z, ctx)
    ccall((:fq_nmod_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}), z, ctx)
    return z
  end

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing, a::Vector{fqPolyRepFieldElem}, b::Vector{Vector{Int}})
    z = new()
    ccall((:fq_nmod_mpoly_init2, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:fq_nmod_mpoly_push_term_fq_nmod_ui, libflint), Nothing,
            (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepFieldElem}, Ptr{Int}, Ref{fqPolyRepMPolyRing}),
            z, a[i], b[i], ctx)
    end

    ccall((:fq_nmod_mpoly_sort_terms, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}), z, ctx)
    ccall((:fq_nmod_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}), z, ctx)
    return z
  end

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing, a::Vector{fqPolyRepFieldElem}, b::Vector{Vector{ZZRingElem}})
    z = new()
    ccall((:fq_nmod_mpoly_init2, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Int, Ref{fqPolyRepMPolyRing},), z, length(a), ctx)
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      ccall((:fq_nmod_mpoly_push_term_fq_nmod_fmpz, libflint), Nothing,
            (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepFieldElem}, Ptr{Ref{ZZRingElem}}, Ref{fqPolyRepMPolyRing}),
            z, a[i], b[i], ctx)
    end

    ccall((:fq_nmod_mpoly_sort_terms, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}), z, ctx)
    ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}), z, ctx)
    return z
  end

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing, a::UInt)
    z = new()
    ccall((:fq_nmod_mpoly_init, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing},), z, ctx)
    ccall((:fq_nmod_mpoly_set_ui, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, UInt, Ref{fqPolyRepMPolyRing}), z, a, ctx)
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_clear_fn, z)
    return z
  end

  function fqPolyRepMPolyRingElem(ctx::zzModMPolyRing, a::zzModRingElem)
    return fqPolyRepMPolyRingElem(ctx, a.data)
  end

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing, a::fqPolyRepFieldElem)
    z = new()
    ccall((:fq_nmod_mpoly_init, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing},), z, ctx)
    ccall((:fq_nmod_mpoly_set_fq_nmod, libflint), Nothing,
          (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepMPolyRing}), z, a, ctx)
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_clear_fn, z)
    return z
  end
end

function _fq_nmod_mpoly_clear_fn(a::fqPolyRepMPolyRingElem)
  ccall((:fq_nmod_mpoly_clear, libflint), Nothing,
        (Ref{fqPolyRepMPolyRingElem}, Ref{fqPolyRepMPolyRing}), a, a.parent)
end

mutable struct fq_nmod_mpoly_factor
  constant_coeffs::Ptr{Nothing}
  constant_alloc::Int
  constant_length::Int
  constant_n::UInt
  constant_ninv::UInt
  constant_norm::Int

  poly::Ptr{Nothing}
  exp::Ptr{Nothing}
  num::Int
  alloc::Int
  # end flint struct

  parent::fqPolyRepMPolyRing

  function fq_nmod_mpoly_factor(ctx::fqPolyRepMPolyRing)
    z = new()
    ccall((:fq_nmod_mpoly_factor_init, libflint), Nothing,
          (Ref{fq_nmod_mpoly_factor}, Ref{fqPolyRepMPolyRing}),
          z, ctx)
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_factor_clear_fn, z)
    return z
  end
end

function _fq_nmod_mpoly_factor_clear_fn(f::fq_nmod_mpoly_factor)
  ccall((:fq_nmod_mpoly_factor_clear, libflint), Nothing,
        (Ref{fq_nmod_mpoly_factor}, Ref{fqPolyRepMPolyRing}),
        f, f.parent)
end

###############################################################################
#
#   FlintLocalField / Local field type hierarchy
#
###############################################################################

# Abstract types
abstract type NonArchLocalField     <: Field end
abstract type NonArchLocalFieldElem <: FieldElem end

abstract type FlintLocalField     <: NonArchLocalField end
abstract type FlintLocalFieldElem <: NonArchLocalFieldElem end


# Alias
const NALocalField     = NonArchLocalField
const NALocalFieldElem = NonArchLocalFieldElem


###############################################################################
#
#   PadicField / PadicFieldElem
#
###############################################################################

const flint_padic_printing_mode = [:terse, :series, :val_unit]

@doc raw"""
    PadicField <: FlintLocalField <: NonArchLocalField <: Field

A $p$-adic field for some prime $p$.
"""
@attributes mutable struct PadicField <: FlintLocalField
  p::Int
  pinv::Float64
  pow::Ptr{Nothing}
  minpre::Int
  maxpre::Int
  mode::Cint
  prec_max::Int

  function PadicField(p::ZZRingElem, prec::Int = 64; cached::Bool = true, check::Bool = true)
    check && !is_probable_prime(p) && throw(DomainError(p, "Integer must be prime"))

    Qp = get_cached!(PadicBase, p, cached) do
      d = new()
      ccall((:padic_ctx_init, libflint), Nothing,
            (Ref{PadicField}, Ref{ZZRingElem}, Int, Int, Cint),
            d, p, 0, 0, 0)
      finalizer(_padic_ctx_clear_fn, d)
      return d
    end
    Qp.prec_max = prec
    return Qp
  end
end

const PadicBase = CacheDictType{ZZRingElem, PadicField}()

function _padic_ctx_clear_fn(a::PadicField)
  ccall((:padic_ctx_clear, libflint), Nothing, (Ref{PadicField},), a)
end

@doc raw"""
    PadicFieldElem <: FlintLocalFieldElem <: NonArchLocalFieldElem <: FieldElem

An element of a $p$-adic field. See [`PadicField`](@ref).
"""
mutable struct PadicFieldElem <: FlintLocalFieldElem
  u :: Int
  v :: Int
  N :: Int
  parent::PadicField

  function PadicFieldElem(prec::Int)
    d = new()
    ccall((:padic_init2, libflint), Nothing, (Ref{PadicFieldElem}, Int), d, prec)
    finalizer(_padic_clear_fn, d)
    return d
  end
end

function _padic_clear_fn(a::PadicFieldElem)
  ccall((:padic_clear, libflint), Nothing, (Ref{PadicFieldElem},), a)
end

###############################################################################
#
#   QadicField / QadicFieldElem
#
###############################################################################

@doc raw"""
    QadicField <: FlintLocalField <: NonArchLocalField <: Field

A $p^n$-adic field for some prime power $p^n$.
"""
@attributes mutable struct QadicField <: FlintLocalField
  p::Int
  pinv::Float64
  pow::Ptr{Nothing}
  minpre::Int
  maxpre::Int
  mode::Cint
  a::Int         # ZZRingElem
  j::Ptr{Nothing}   # slong*
  len::Int
  var::Cstring   # char*
  prec_max::Int

  function QadicField(p::ZZRingElem, d::Int, prec::Int = 64, var::String = "a"; cached::Bool = true, check::Bool = true, base_field::PadicField = PadicField(p, prec, cached = cached))

    @assert p == prime(base_field)
    check && !is_probable_prime(p) && throw(DomainError(p, "Integer must be prime"))

    z = get_cached!(QadicBase, (base_field, d), cached) do
      zz = new()
      ccall((:qadic_ctx_init, libflint), Nothing,
            (Ref{QadicField}, Ref{ZZRingElem}, Int, Int, Int, Cstring, Cint),
            zz, p, d, 0, 0, var, 0)
      finalizer(_qadic_ctx_clear_fn, zz)
      return zz
    end
    z.prec_max = prec
    set_attribute!(z, :base_field, base_field)

    return z, gen(z)
  end
end

const QadicBase = CacheDictType{Tuple{PadicField, Int}, QadicField}()

function _qadic_ctx_clear_fn(a::QadicField)
  ccall((:qadic_ctx_clear, libflint), Nothing, (Ref{QadicField},), a)
end

@doc raw"""
    QadicFieldElem <: FlintLocalFieldElem <: NonArchLocalFieldElem <: FieldElem

An element of a $q$-adic field. See [`QadicField`](@ref).
"""
mutable struct QadicFieldElem <: FlintLocalFieldElem
  coeffs::Int
  alloc::Int
  length::Int
  val::Int
  N::Int
  parent::QadicField

  function QadicFieldElem(prec::Int)
    z = new()
    ccall((:qadic_init2, libflint), Nothing, (Ref{QadicFieldElem}, Int), z, prec)
    finalizer(_qadic_clear_fn, z)
    return z
  end
end

function _qadic_clear_fn(a::QadicFieldElem)
  ccall((:qadic_clear, libflint), Nothing, (Ref{QadicFieldElem},), a)
end

###############################################################################
#
#   ZZRelPowerSeriesRing / ZZRelPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct ZZRelPowerSeriesRing <: SeriesRing{ZZRingElem}
  prec_max::Int
  S::Symbol

  function ZZRelPowerSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
    return get_cached!(FmpzRelSeriesID, (prec, s), cached) do
      return new(prec, s)
    end
  end
end

const FmpzRelSeriesID = CacheDictType{Tuple{Int, Symbol}, ZZRelPowerSeriesRing}()

mutable struct ZZRelPowerSeriesRingElem <: RelPowerSeriesRingElem{ZZRingElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  # end flint struct

  prec::Int
  val::Int
  parent::ZZRelPowerSeriesRing

  function ZZRelPowerSeriesRingElem()
    z = new()
    ccall((:fmpz_poly_init, libflint), Nothing,
          (Ref{ZZRelPowerSeriesRingElem},), z)
    finalizer(_fmpz_rel_series_clear_fn, z)
    return z
  end

  function ZZRelPowerSeriesRingElem(a::Vector{ZZRingElem}, len::Int, prec::Int, val::Int)
    z = new()
    ccall((:fmpz_poly_init2, libflint), Nothing,
          (Ref{ZZRelPowerSeriesRingElem}, Int), z, len)
    for i = 1:len
      ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{ZZRelPowerSeriesRingElem}, Int, Ref{ZZRingElem}), z, i - 1, a[i])
    end
    z.prec = prec
    z.val = val
    finalizer(_fmpz_rel_series_clear_fn, z)
    return z
  end

  function ZZRelPowerSeriesRingElem(a::ZZRelPowerSeriesRingElem)
    z = new()
    ccall((:fmpz_poly_init, libflint), Nothing, (Ref{ZZRelPowerSeriesRingElem},), z)
    ccall((:fmpz_poly_set, libflint), Nothing,
          (Ref{ZZRelPowerSeriesRingElem}, Ref{ZZRelPowerSeriesRingElem}), z, a)
    finalizer(_fmpz_rel_series_clear_fn, z)
    return z
  end
end

function _fmpz_rel_series_clear_fn(a::ZZRelPowerSeriesRingElem)
  ccall((:fmpz_poly_clear, libflint), Nothing, (Ref{ZZRelPowerSeriesRingElem},), a)
end

###############################################################################
#
#   ZZAbsPowerSeriesRing / ZZAbsPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct ZZAbsPowerSeriesRing <: SeriesRing{ZZRingElem}
  prec_max::Int
  S::Symbol

  function ZZAbsPowerSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
    return get_cached!(FmpzAbsSeriesID, (prec, s), cached) do
      return new(prec, s)
    end
  end
end

const FmpzAbsSeriesID = CacheDictType{Tuple{Int, Symbol}, ZZAbsPowerSeriesRing}()

mutable struct ZZAbsPowerSeriesRingElem <: AbsPowerSeriesRingElem{ZZRingElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  prec :: Int
  parent::ZZAbsPowerSeriesRing

  function ZZAbsPowerSeriesRingElem()
    z = new()
    ccall((:fmpz_poly_init, libflint), Nothing,
          (Ref{ZZAbsPowerSeriesRingElem},), z)
    finalizer(_fmpz_abs_series_clear_fn, z)
    return z
  end

  function ZZAbsPowerSeriesRingElem(a::Vector{ZZRingElem}, len::Int, prec::Int)
    z = new()
    ccall((:fmpz_poly_init2, libflint), Nothing,
          (Ref{ZZAbsPowerSeriesRingElem}, Int), z, len)
    for i = 1:len
      ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{ZZAbsPowerSeriesRingElem}, Int, Ref{ZZRingElem}), z, i - 1, a[i])
    end
    z.prec = prec
    finalizer(_fmpz_abs_series_clear_fn, z)
    return z
  end

  function ZZAbsPowerSeriesRingElem(a::ZZAbsPowerSeriesRingElem)
    z = new()
    ccall((:fmpz_poly_init, libflint), Nothing, (Ref{ZZAbsPowerSeriesRingElem},), z)
    ccall((:fmpz_poly_set, libflint), Nothing,
          (Ref{ZZAbsPowerSeriesRingElem}, Ref{ZZAbsPowerSeriesRingElem}), z, a)
    finalizer(_fmpz_abs_series_clear_fn, z)
    return z
  end
end

function _fmpz_abs_series_clear_fn(a::ZZAbsPowerSeriesRingElem)
  ccall((:fmpz_poly_clear, libflint), Nothing, (Ref{ZZAbsPowerSeriesRingElem},), a)
end

###############################################################################
#
#   FlintPuiseuxSeriesRing / FlintPuiseuxSeriesRingElem
#
###############################################################################

@attributes mutable struct FlintPuiseuxSeriesRing{T <: RingElem} <: Ring where T
  laurent_ring::Ring

  function FlintPuiseuxSeriesRing{T}(R::Ring, cached::Bool = true) where T
    return get_cached!(FlintPuiseuxSeriesID, R, cached) do
      return new{T}(R)
    end::FlintPuiseuxSeriesRing{T}
  end
end

const FlintPuiseuxSeriesID = CacheDictType{Ring, Ring}()

mutable struct FlintPuiseuxSeriesRingElem{T <: RingElem} <: RingElem
  data::T
  scale::Int
  parent::FlintPuiseuxSeriesRing{T}

  function FlintPuiseuxSeriesRingElem{T}(d::T, scale::Int) where T <: RingElem
    new{T}(d, scale)
  end
end

###############################################################################
#
#   FlintPuiseuxSeriesField / FlintPuiseuxSeriesFieldElem
#
###############################################################################

@attributes mutable struct FlintPuiseuxSeriesField{T <: RingElem} <: Field
  laurent_ring::Ring

  function FlintPuiseuxSeriesField{T}(R::Field, cached::Bool = true) where T
    return get_cached!(FlintPuiseuxSeriesID, R, cached) do
      return new{T}(R)
    end::FlintPuiseuxSeriesField{T}
  end
end

mutable struct FlintPuiseuxSeriesFieldElem{T <: RingElem} <: FieldElem
  data::T
  scale::Int
  parent::FlintPuiseuxSeriesField{T}

  function FlintPuiseuxSeriesFieldElem{T}(d::T, scale::Int) where T <: RingElem
    new{T}(d, scale)
  end
end

###############################################################################
#
#   ZZLaurentSeriesRing / ZZLaurentSeriesRingElem
#
###############################################################################

@attributes mutable struct ZZLaurentSeriesRing <: Ring
  prec_max::Int
  S::Symbol

  function ZZLaurentSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
    return get_cached!(FmpzLaurentSeriesID, (prec, s), cached) do
      return new(prec, s)
    end
  end
end

const FmpzLaurentSeriesID = CacheDictType{Tuple{Int, Symbol}, ZZLaurentSeriesRing}()

mutable struct ZZLaurentSeriesRingElem <: RingElem
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  prec::Int
  val::Int
  scale::Int
  parent::ZZLaurentSeriesRing

  function ZZLaurentSeriesRingElem()
    z = new()
    ccall((:fmpz_poly_init, libflint), Nothing,
          (Ref{ZZLaurentSeriesRingElem},), z)
    finalizer(_fmpz_laurent_series_clear_fn, z)
    return z
  end

  function ZZLaurentSeriesRingElem(a::Vector{ZZRingElem}, len::Int, prec::Int, val::Int, scale::Int)
    z = new()
    ccall((:fmpz_poly_init2, libflint), Nothing,
          (Ref{ZZLaurentSeriesRingElem}, Int), z, len)
    for i = 1:len
      ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{ZZLaurentSeriesRingElem}, Int, Ref{ZZRingElem}), z, i - 1, a[i])
    end
    z.prec = prec
    z.val = val
    z.scale = scale
    finalizer(_fmpz_laurent_series_clear_fn, z)
    return z
  end

  function ZZLaurentSeriesRingElem(a::ZZLaurentSeriesRingElem)
    z = new()
    ccall((:fmpz_poly_init, libflint), Nothing, (Ref{ZZLaurentSeriesRingElem},), z)
    ccall((:fmpz_poly_set, libflint), Nothing,
          (Ref{ZZLaurentSeriesRingElem}, Ref{ZZLaurentSeriesRingElem}), z, a)
    finalizer(_fmpz_laurent_series_clear_fn, z)
    return z
  end
end

function _fmpz_laurent_series_clear_fn(a::ZZLaurentSeriesRingElem)
  ccall((:fmpz_poly_clear, libflint), Nothing, (Ref{ZZLaurentSeriesRingElem},), a)
end

###############################################################################
#
#   QQRelPowerSeriesRing / QQRelPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct QQRelPowerSeriesRing <: SeriesRing{QQFieldElem}
  prec_max::Int
  S::Symbol

  function QQRelPowerSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
    return get_cached!(FmpqRelSeriesID, (prec, s), cached) do
      return new(prec, s)
    end
  end
end

const FmpqRelSeriesID = CacheDictType{Tuple{Int, Symbol}, QQRelPowerSeriesRing}()

mutable struct QQRelPowerSeriesRingElem <: RelPowerSeriesRingElem{QQFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  den::Int
  # end flint struct

  prec::Int
  val::Int
  parent::QQRelPowerSeriesRing

  function QQRelPowerSeriesRingElem()
    z = new()
    ccall((:fmpq_poly_init, libflint), Nothing,
          (Ref{QQRelPowerSeriesRingElem},), z)
    finalizer(_fmpq_rel_series_clear_fn, z)
    return z
  end

  function QQRelPowerSeriesRingElem(a::Vector{QQFieldElem}, len::Int, prec::Int, val::Int)
    z = new()
    ccall((:fmpq_poly_init2, libflint), Nothing,
          (Ref{QQRelPowerSeriesRingElem}, Int), z, len)
    for i = 1:len
      ccall((:fmpq_poly_set_coeff_fmpq, libflint), Nothing,
            (Ref{QQRelPowerSeriesRingElem}, Int, Ref{QQFieldElem}), z, i - 1, a[i])
    end
    z.prec = prec
    z.val = val
    finalizer(_fmpq_rel_series_clear_fn, z)
    return z
  end

  function QQRelPowerSeriesRingElem(a::QQRelPowerSeriesRingElem)
    z = new()
    ccall((:fmpq_poly_init, libflint), Nothing, (Ref{QQRelPowerSeriesRingElem},), z)
    ccall((:fmpq_poly_set, libflint), Nothing,
          (Ref{QQRelPowerSeriesRingElem}, Ref{QQRelPowerSeriesRingElem}), z, a)
    finalizer(_fmpq_rel_series_clear_fn, z)
    return z
  end
end

function _fmpq_rel_series_clear_fn(a::QQRelPowerSeriesRingElem)
  ccall((:fmpq_poly_clear, libflint), Nothing, (Ref{QQRelPowerSeriesRingElem},), a)
end

###############################################################################
#
#   QQAbsPowerSeriesRing / QQAbsPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct QQAbsPowerSeriesRing <: SeriesRing{QQFieldElem}
  prec_max::Int
  S::Symbol

  function QQAbsPowerSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
    return get_cached!(FmpqAbsSeriesID, (prec, s), cached) do
      return new(prec, s)
    end
  end
end

const FmpqAbsSeriesID = CacheDictType{Tuple{Int, Symbol}, QQAbsPowerSeriesRing}()

mutable struct QQAbsPowerSeriesRingElem <: AbsPowerSeriesRingElem{QQFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  den::Int
  # end flint struct

  prec :: Int
  parent::QQAbsPowerSeriesRing

  function QQAbsPowerSeriesRingElem()
    z = new()
    ccall((:fmpq_poly_init, libflint), Nothing,
          (Ref{QQAbsPowerSeriesRingElem},), z)
    finalizer(_fmpq_abs_series_clear_fn, z)
    return z
  end

  function QQAbsPowerSeriesRingElem(a::Vector{QQFieldElem}, len::Int, prec::Int)
    z = new()
    ccall((:fmpq_poly_init2, libflint), Nothing,
          (Ref{QQAbsPowerSeriesRingElem}, Int), z, len)
    for i = 1:len
      ccall((:fmpq_poly_set_coeff_fmpq, libflint), Nothing,
            (Ref{QQAbsPowerSeriesRingElem}, Int, Ref{QQFieldElem}), z, i - 1, a[i])
    end
    z.prec = prec
    finalizer(_fmpq_abs_series_clear_fn, z)
    return z
  end

  function QQAbsPowerSeriesRingElem(a::QQAbsPowerSeriesRingElem)
    z = new()
    ccall((:fmpq_poly_init, libflint), Nothing, (Ref{QQAbsPowerSeriesRingElem},), z)
    ccall((:fmpq_poly_set, libflint), Nothing,
          (Ref{QQAbsPowerSeriesRingElem}, Ref{QQAbsPowerSeriesRingElem}), z, a)
    finalizer(_fmpq_abs_series_clear_fn, z)
    return z
  end
end

function _fmpq_abs_series_clear_fn(a::QQAbsPowerSeriesRingElem)
  ccall((:fmpq_poly_clear, libflint), Nothing, (Ref{QQAbsPowerSeriesRingElem},), a)
end

###############################################################################
#
#   fpRelPowerSeriesRing / fpRelPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct fpRelPowerSeriesRing <: SeriesRing{zzModRingElem}
  base_ring::fpField
  prec_max::Int
  S::Symbol

  function fpRelPowerSeriesRing(R::fpField, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(GFPRelSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const GFPRelSeriesID = CacheDictType{Tuple{fpField, Int, Symbol},
                                     fpRelPowerSeriesRing}()

mutable struct fpRelPowerSeriesRingElem <: RelPowerSeriesRingElem{fpFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  mod_n::UInt
  mod_ninv::UInt
  mod_norm::UInt
  prec::Int
  val::Int
  parent::fpRelPowerSeriesRing

  function fpRelPowerSeriesRingElem(p::UInt)
    z = new()
    ccall((:nmod_poly_init, libflint), Nothing,
          (Ref{fpRelPowerSeriesRingElem}, UInt), z, p)
    finalizer(_gfp_rel_series_clear_fn, z)
    return z
  end

  function fpRelPowerSeriesRingElem(p::UInt, a::Vector{ZZRingElem}, len::Int, prec::Int, val::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpRelPowerSeriesRingElem}, UInt, Int), z, p, len)
    for i = 1:len
      tt = ccall((:fmpz_fdiv_ui, libflint), UInt,
                 (Ref{ZZRingElem}, UInt), a[i], p)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{fpRelPowerSeriesRingElem}, Int, UInt), z, i - 1, tt)
    end
    z.prec = prec
    z.val = val
    finalizer(_gfp_rel_series_clear_fn, z)
    return z
  end

  function fpRelPowerSeriesRingElem(p::UInt, a::Vector{UInt}, len::Int, prec::Int, val::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpRelPowerSeriesRingElem}, UInt, Int), z, p, len)
    for i = 1:len
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{fpRelPowerSeriesRingElem}, Int, UInt), z, i - 1, a[i])
    end
    z.prec = prec
    z.val = val
    finalizer(_gfp_rel_series_clear_fn, z)
    return z
  end

  function fpRelPowerSeriesRingElem(p::UInt, a::Vector{fpFieldElem}, len::Int, prec::Int, val::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpRelPowerSeriesRingElem}, UInt, Int), z, p, len)
    for i = 1:len
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{fpRelPowerSeriesRingElem}, Int, UInt), z, i - 1, data(a[i]))
    end
    z.prec = prec
    z.val = val
    finalizer(_gfp_rel_series_clear_fn, z)
    return z
  end

  function fpRelPowerSeriesRingElem(a::fpRelPowerSeriesRingElem)
    z = new()
    p = modulus(base_ring(parent(a)))
    ccall((:nmod_poly_init, libflint), Nothing,
          (Ref{fpRelPowerSeriesRingElem}, UInt), z, p)
    ccall((:nmod_poly_set, libflint), Nothing,
          (Ref{fpRelPowerSeriesRingElem}, Ref{fpRelPowerSeriesRingElem}), z, a)
    finalizer(_gfp_rel_series_clear_fn, z)
    return z
  end
end

function _gfp_rel_series_clear_fn(a::fpRelPowerSeriesRingElem)
  ccall((:nmod_poly_clear, libflint), Nothing, (Ref{fpRelPowerSeriesRingElem},), a)
end

###############################################################################
#
#   zzModRelPowerSeriesRing / zzModRelPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct zzModRelPowerSeriesRing <: SeriesRing{zzModRingElem}
  base_ring::zzModRing
  prec_max::Int
  S::Symbol

  function zzModRelPowerSeriesRing(R::zzModRing, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(NmodRelSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const NmodRelSeriesID = CacheDictType{Tuple{zzModRing, Int, Symbol},
                                      zzModRelPowerSeriesRing}()

mutable struct zzModRelPowerSeriesRingElem <: RelPowerSeriesRingElem{zzModRingElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  mod_n::UInt
  mod_ninv::UInt
  mod_norm::UInt
  prec::Int
  val::Int
  parent::zzModRelPowerSeriesRing

  function zzModRelPowerSeriesRingElem(p::UInt)
    z = new()
    ccall((:nmod_poly_init, libflint), Nothing,
          (Ref{zzModRelPowerSeriesRingElem}, UInt), z, p)
    finalizer(_nmod_rel_series_clear_fn, z)
    return z
  end

  function zzModRelPowerSeriesRingElem(p::UInt, a::Vector{ZZRingElem}, len::Int, prec::Int, val::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModRelPowerSeriesRingElem}, UInt, Int), z, p, len)
    for i = 1:len
      tt = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt), a[i], p)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{zzModRelPowerSeriesRingElem}, Int, UInt), z, i - 1, tt)
    end
    z.prec = prec
    z.val = val
    finalizer(_nmod_rel_series_clear_fn, z)
    return z
  end

  function zzModRelPowerSeriesRingElem(p::UInt, a::Vector{UInt}, len::Int, prec::Int, val::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModRelPowerSeriesRingElem}, UInt, Int), z, p, len)
    for i = 1:len
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{zzModRelPowerSeriesRingElem}, Int, UInt), z, i - 1, a[i])
    end
    z.prec = prec
    z.val = val
    finalizer(_nmod_rel_series_clear_fn, z)
    return z
  end

  function zzModRelPowerSeriesRingElem(p::UInt, a::Vector{zzModRingElem}, len::Int, prec::Int, val::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModRelPowerSeriesRingElem}, UInt, Int), z, p, len)
    for i = 1:len
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{zzModRelPowerSeriesRingElem}, Int, UInt), z, i - 1, data(a[i]))
    end
    z.prec = prec
    z.val = val
    finalizer(_nmod_rel_series_clear_fn, z)
    return z
  end

  function zzModRelPowerSeriesRingElem(a::zzModRelPowerSeriesRingElem)
    z = new()
    p = modulus(base_ring(parent(a)))
    ccall((:nmod_poly_init, libflint), Nothing,
          (Ref{zzModRelPowerSeriesRingElem}, UInt), z, p)
    ccall((:nmod_poly_set, libflint), Nothing,
          (Ref{zzModRelPowerSeriesRingElem}, Ref{zzModRelPowerSeriesRingElem}), z, a)
    finalizer(_nmod_rel_series_clear_fn, z)
    return z
  end
end

function _nmod_rel_series_clear_fn(a::zzModRelPowerSeriesRingElem)
  ccall((:nmod_poly_clear, libflint), Nothing, (Ref{zzModRelPowerSeriesRingElem},), a)
end

###############################################################################
#
#   FpRelPowerSeriesRing / FpRelPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct FpRelPowerSeriesRing <: SeriesRing{FpFieldElem}
  base_ring::FpField
  prec_max::Int
  S::Symbol

  function FpRelPowerSeriesRing(R::Ring, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(GFPFmpzRelSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const GFPFmpzRelSeriesID = CacheDictType{Tuple{FpField, Int, Symbol},
                                         FpRelPowerSeriesRing}()

mutable struct FpRelPowerSeriesRingElem <: RelPowerSeriesRingElem{FpFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  # end flint struct

  prec::Int
  val::Int
  parent::FpRelPowerSeriesRing

  function FpRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct)
    z = new()
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{FpRelPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, p)
    finalizer(_gfp_fmpz_rel_series_clear_fn, z)
    return z
  end

  function FpRelPowerSeriesRingElem(R::FpField)
    return FpRelPowerSeriesRingElem(R.ninv)
  end

  function FpRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZRingElem},
      len::Int, prec::Int, val::Int)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{FpRelPowerSeriesRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, len, p)
    for i = 1:len
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{FpRelPowerSeriesRingElem}, Int, Ref{ZZRingElem},
             Ref{fmpz_mod_ctx_struct}),
            z, i - 1, a[i], p)
    end
    z.prec = prec
    z.val = val
    finalizer(_gfp_fmpz_rel_series_clear_fn, z)
    return z
  end

  function FpRelPowerSeriesRingElem(R::FpField, a::Vector{ZZRingElem},
      len::Int, prec::Int, val::Int)
    return FpRelPowerSeriesRingElem(R.ninv, a, len, prec, val)
  end

  function FpRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{FpFieldElem},
      len::Int, prec::Int, val::Int)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{FpRelPowerSeriesRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, len, p)
    for i = 1:len
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{FpRelPowerSeriesRingElem}, Int, Ref{ZZRingElem},
             Ref{fmpz_mod_ctx_struct}),
            z, i - 1, data(a[i]), p)
    end
    z.prec = prec
    z.val = val
    finalizer(_gfp_fmpz_rel_series_clear_fn, z)
    return z
  end

  function FpRelPowerSeriesRingElem(R::FpField, a::Vector{FpFieldElem},
      len::Int, prec::Int, val::Int)
    return FpRelPowerSeriesRingElem(R.ninv, a, len, prec, val)
  end

  function FpRelPowerSeriesRingElem(a::FpRelPowerSeriesRingElem)
    z = new()
    p = base_ring(parent(a)).ninv
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{FpRelPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, p)
    ccall((:fmpz_mod_poly_set, libflint), Nothing,
          (Ref{FpRelPowerSeriesRingElem}, Ref{FpRelPowerSeriesRingElem},
           Ref{fmpz_mod_ctx_struct}),
          z, a, p)
    finalizer(_gfp_fmpz_rel_series_clear_fn, z)
    return z
  end
end

function _gfp_fmpz_rel_series_clear_fn(a::FpRelPowerSeriesRingElem)
  ccall((:fmpz_mod_poly_clear, libflint), Nothing,
        (Ref{FpRelPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
        a, base_ring(parent(a)).ninv)
end

###############################################################################
#
#   ZZModRelPowerSeriesRing / ZZModRelPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct ZZModRelPowerSeriesRing <: SeriesRing{ZZModRingElem}
  base_ring::ZZModRing
  prec_max::Int
  S::Symbol

  function ZZModRelPowerSeriesRing(R::Ring, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(FmpzModRelSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const FmpzModRelSeriesID = CacheDictType{Tuple{ZZModRing, Int, Symbol},
                                         ZZModRelPowerSeriesRing}()

mutable struct ZZModRelPowerSeriesRingElem <: RelPowerSeriesRingElem{ZZModRingElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  # end flint struct

  prec::Int
  val::Int
  parent::ZZModRelPowerSeriesRing

  function ZZModRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct)
    z = new()
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{ZZModRelPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, p)
    finalizer(_fmpz_mod_rel_series_clear_fn, z)
    return z
  end

  function ZZModRelPowerSeriesRingElem(R::ZZModRing)
    return ZZModRelPowerSeriesRingElem(R.ninv)
  end

  function ZZModRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZRingElem},
      len::Int, prec::Int, val::Int)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{ZZModRelPowerSeriesRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, len, p)
    for i = 1:len
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{ZZModRelPowerSeriesRingElem}, Int, Ref{ZZRingElem},
             Ref{fmpz_mod_ctx_struct}),
            z, i - 1, a[i], p)
    end
    z.prec = prec
    z.val = val
    finalizer(_fmpz_mod_rel_series_clear_fn, z)
    return z
  end

  function ZZModRelPowerSeriesRingElem(R::ZZModRing, a::Vector{ZZRingElem},
      len::Int, prec::Int, val::Int)
    return ZZModRelPowerSeriesRingElem(R.ninv, a, len, prec, val)
  end

  function ZZModRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZModRingElem},
      len::Int, prec::Int, val::Int)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{ZZModRelPowerSeriesRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, len, p)
    for i = 1:len
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{ZZModRelPowerSeriesRingElem}, Int, Ref{ZZRingElem},
             Ref{fmpz_mod_ctx_struct}),
            z, i - 1, data(a[i]), p)
    end
    z.prec = prec
    z.val = val
    finalizer(_fmpz_mod_rel_series_clear_fn, z)
    return z
  end

  function ZZModRelPowerSeriesRingElem(R::ZZModRing, a::Vector{ZZModRingElem},
      len::Int, prec::Int, val::Int)
    return ZZModRelPowerSeriesRingElem(R.ninv, a, len, prec, val)
  end

  function ZZModRelPowerSeriesRingElem(a::ZZModRelPowerSeriesRingElem)
    z = new()
    p = base_ring(parent(a)).ninv
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{ZZModRelPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, p)
    ccall((:fmpz_mod_poly_set, libflint), Nothing,
          (Ref{ZZModRelPowerSeriesRingElem}, Ref{ZZModRelPowerSeriesRingElem},
           Ref{fmpz_mod_ctx_struct}),
          z, a, p)
    finalizer(_fmpz_mod_rel_series_clear_fn, z)
    return z
  end
end

function _fmpz_mod_rel_series_clear_fn(a::ZZModRelPowerSeriesRingElem)
  ccall((:fmpz_mod_poly_clear, libflint), Nothing,
        (Ref{ZZModRelPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
        a, base_ring(parent(a)).ninv)
end

###############################################################################
#
#   FpAbsPowerSeriesRing / FpAbsPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct FpAbsPowerSeriesRing <: SeriesRing{FpFieldElem}
  base_ring::FpField
  prec_max::Int
  S::Symbol

  function FpAbsPowerSeriesRing(R::Ring, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(GFPFmpzAbsSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const GFPFmpzAbsSeriesID = CacheDictType{Tuple{FpField, Int, Symbol},
                                         FpAbsPowerSeriesRing}()

mutable struct FpAbsPowerSeriesRingElem <: AbsPowerSeriesRingElem{FpFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  # end flint struct

  prec::Int
  parent::FpAbsPowerSeriesRing

  function FpAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct)
    z = new()
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{FpAbsPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, p)
    finalizer(_gfp_fmpz_abs_series_clear_fn, z)
    return z
  end

  function FpAbsPowerSeriesRingElem(R::FpField)
    return FpAbsPowerSeriesRingElem(R.ninv)
  end

  function FpAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZRingElem},
      len::Int, prec::Int)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{FpAbsPowerSeriesRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, len, p)
    for i = 1:len
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{FpAbsPowerSeriesRingElem}, Int, Ref{ZZRingElem},
             Ref{fmpz_mod_ctx_struct}),
            z, i - 1, a[i], p)
    end
    z.prec = prec
    finalizer(_gfp_fmpz_abs_series_clear_fn, z)
    return z
  end

  function FpAbsPowerSeriesRingElem(R::FpField, a::Vector{ZZRingElem}, len::Int, prec::Int)
    return FpAbsPowerSeriesRingElem(R.ninv, a, len, prec)
  end

  function FpAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{FpFieldElem},
      len::Int, prec::Int)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{FpAbsPowerSeriesRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, len, p)
    for i = 1:len
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{FpAbsPowerSeriesRingElem}, Int, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
            z, i - 1, data(a[i]), p)
    end
    z.prec = prec
    finalizer(_gfp_fmpz_abs_series_clear_fn, z)
    return z
  end

  function FpAbsPowerSeriesRingElem(R::FpField, a::Vector{FpFieldElem},
      len::Int, prec::Int)
    return FpAbsPowerSeriesRingElem(R.ninv, a, len, prec)
  end

  function FpAbsPowerSeriesRingElem(a::FpAbsPowerSeriesRingElem)
    z = new()
    p = base_ring(parent(a)).ninv
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{FpAbsPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, p)
    ccall((:fmpz_mod_poly_set, libflint), Nothing,
          (Ref{FpAbsPowerSeriesRingElem}, Ref{FpAbsPowerSeriesRingElem},
           Ref{fmpz_mod_ctx_struct}),
          z, a, p)
    finalizer(_gfp_fmpz_abs_series_clear_fn, z)
    return z
  end
end

function _gfp_fmpz_abs_series_clear_fn(a::FpAbsPowerSeriesRingElem)
  ccall((:fmpz_mod_poly_clear, libflint), Nothing,
        (Ref{FpAbsPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
        a, base_ring(parent(a)).ninv)
end

###############################################################################
#
#   zzModAbsPowerSeriesRing / zzModAbsPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct zzModAbsPowerSeriesRing <: SeriesRing{zzModRingElem}
  base_ring::zzModRing
  prec_max::Int
  n::UInt
  S::Symbol

  function zzModAbsPowerSeriesRing(R::Ring, prec::Int, s::Symbol,
      cached::Bool = true)
    m = modulus(R)
    return get_cached!(NmodAbsSeriesID, (R, prec, s), cached) do
      return new(R, prec, m, s)
    end
  end
end

const NmodAbsSeriesID = CacheDictType{Tuple{zzModRing, Int, Symbol},
                                      zzModAbsPowerSeriesRing}()

mutable struct zzModAbsPowerSeriesRingElem <: AbsPowerSeriesRingElem{zzModRingElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  mod_n::UInt
  mod_ninv::UInt
  mod_norm::UInt
  # end of flint struct

  prec::Int
  parent::zzModAbsPowerSeriesRing

  function zzModAbsPowerSeriesRingElem(n::UInt)
    z = new()
    ccall((:nmod_poly_init, libflint), Nothing,
          (Ref{zzModAbsPowerSeriesRingElem}, UInt), z, n)
    finalizer(_nmod_abs_series_clear_fn, z)
    return z
  end

  function zzModAbsPowerSeriesRingElem(n::UInt, arr::Vector{ZZRingElem}, len::Int, prec::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModAbsPowerSeriesRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:len
      tt = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt), arr[i], n)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{zzModAbsPowerSeriesRingElem}, Int, UInt), z, i - 1, tt)
    end
    z.prec = prec
    finalizer(_nmod_abs_series_clear_fn, z)
    return z
  end

  function zzModAbsPowerSeriesRingElem(n::UInt, arr::Vector{UInt}, len::Int, prec::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModAbsPowerSeriesRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:len
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{zzModAbsPowerSeriesRingElem}, Int, UInt), z, i - 1, arr[i])
    end
    z.prec = prec
    finalizer(_nmod_abs_series_clear_fn, z)
    return z
  end

  function zzModAbsPowerSeriesRingElem(n::UInt, arr::Vector{zzModRingElem}, len::Int, prec::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModAbsPowerSeriesRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:len
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{zzModAbsPowerSeriesRingElem}, Int, UInt), z, i-1, arr[i].data)
    end
    z.prec = prec
    finalizer(_nmod_abs_series_clear_fn, z)
    return z
  end

  function zzModAbsPowerSeriesRingElem(a::zzModAbsPowerSeriesRingElem)
    z = new()
    R = base_ring(a)
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{zzModAbsPowerSeriesRingElem}, UInt, Int), z, R.n, length(a))
    ccall((:nmod_poly_set, libflint), Nothing,
          (Ref{zzModAbsPowerSeriesRingElem}, Ref{zzModAbsPowerSeriesRingElem}), z, a)
    z.prec = a.prec
    finalizer(_nmod_abs_series_clear_fn, z)
    return z
  end
end

function _nmod_abs_series_clear_fn(x::zzModAbsPowerSeriesRingElem)
  ccall((:nmod_poly_clear, libflint), Nothing, (Ref{zzModAbsPowerSeriesRingElem}, ), x)
end

###############################################################################
#
#   fpAbsPowerSeriesRing / fpAbsPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct fpAbsPowerSeriesRing <: SeriesRing{fpFieldElem}
  base_ring::fpField
  prec_max::Int
  n::UInt
  S::Symbol

  function fpAbsPowerSeriesRing(R::Ring, prec::Int, s::Symbol,
      cached::Bool = true)
    m = modulus(R)
    return get_cached!(GFPAbsSeriesID, (R, prec, s), cached) do
      return new(R, prec, m, s)
    end
  end
end

const GFPAbsSeriesID = CacheDictType{Tuple{fpField, Int, Symbol},
                                     fpAbsPowerSeriesRing}()

mutable struct fpAbsPowerSeriesRingElem <: AbsPowerSeriesRingElem{fpFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  mod_n::UInt
  mod_ninv::UInt
  mod_norm::UInt
  # end of flint struct

  prec::Int
  parent::fpAbsPowerSeriesRing

  function fpAbsPowerSeriesRingElem(n::UInt)
    z = new()
    ccall((:nmod_poly_init, libflint), Nothing,
          (Ref{fpAbsPowerSeriesRingElem}, UInt), z, n)
    finalizer(_gfp_abs_series_clear_fn, z)
    return z
  end

  function fpAbsPowerSeriesRingElem(n::UInt, arr::Vector{ZZRingElem}, len::Int, prec::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpAbsPowerSeriesRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:len
      tt = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{ZZRingElem}, UInt), arr[i], n)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{fpAbsPowerSeriesRingElem}, Int, UInt), z, i - 1, tt)
    end
    z.prec = prec
    finalizer(_gfp_abs_series_clear_fn, z)
    return z
  end

  function fpAbsPowerSeriesRingElem(n::UInt, arr::Vector{UInt}, len::Int, prec::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpAbsPowerSeriesRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:len
      ccall((:nmod_poly_series_set_coeff_ui, libflint), Nothing,
            (Ref{fpAbsPowerSeriesRingElem}, Int, UInt), z, i - 1, arr[i])
    end
    z.prec = prec
    finalizer(_gfp_abs_series_clear_fn, z)
    return z
  end

  function fpAbsPowerSeriesRingElem(n::UInt, arr::Vector{fpFieldElem}, len::Int, prec::Int)
    z = new()
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpAbsPowerSeriesRingElem}, UInt, Int), z, n, length(arr))
    for i in 1:len
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{fpAbsPowerSeriesRingElem}, Int, UInt), z, i-1, arr[i].data)
    end
    z.prec = prec
    finalizer(_gfp_abs_series_clear_fn, z)
    return z
  end

  function fpAbsPowerSeriesRingElem(a::fpAbsPowerSeriesRingElem)
    z = new()
    R = base_ring(a)
    ccall((:nmod_poly_init2, libflint), Nothing,
          (Ref{fpAbsPowerSeriesRingElem}, UInt, Int), z, R.n, length(a))
    ccall((:nmod_poly_set, libflint), Nothing,
          (Ref{fpAbsPowerSeriesRingElem}, Ref{fpAbsPowerSeriesRingElem}), z, a)
    z.prec = a.prec
    finalizer(_gfp_abs_series_clear_fn, z)
    return z
  end
end

function _gfp_abs_series_clear_fn(x::fpAbsPowerSeriesRingElem)
  ccall((:nmod_poly_clear, libflint), Nothing, (Ref{fpAbsPowerSeriesRingElem}, ), x)
end

###############################################################################
#
#   ZZModAbsPowerSeriesRing / ZZModAbsPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct ZZModAbsPowerSeriesRing <: SeriesRing{ZZModRingElem}
  base_ring::ZZModRing
  prec_max::Int
  S::Symbol

  function ZZModAbsPowerSeriesRing(R::Ring, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(FmpzModAbsSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const FmpzModAbsSeriesID = CacheDictType{Tuple{ZZModRing, Int, Symbol},
                                         ZZModAbsPowerSeriesRing}()

mutable struct ZZModAbsPowerSeriesRingElem <: AbsPowerSeriesRingElem{ZZModRingElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  # end flint struct

  prec::Int
  parent::ZZModAbsPowerSeriesRing

  function ZZModAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct)
    z = new()
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{ZZModAbsPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, p)
    finalizer(_fmpz_mod_abs_series_clear_fn, z)
    return z
  end

  function ZZModAbsPowerSeriesRingElem(R::ZZModRing)
    return ZZModAbsPowerSeriesRingElem(R.ninv)
  end

  function ZZModAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZRingElem},
      len::Int, prec::Int)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{ZZModAbsPowerSeriesRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, len, p)
    for i = 1:len
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{ZZModAbsPowerSeriesRingElem}, Int, Ref{ZZRingElem},
             Ref{fmpz_mod_ctx_struct}),
            z, i - 1, a[i], p)
    end
    z.prec = prec
    finalizer(_fmpz_mod_abs_series_clear_fn, z)
    return z
  end

  function ZZModAbsPowerSeriesRingElem(R::ZZModRing, a::Vector{ZZRingElem}, len::Int, prec::Int)
    return ZZModAbsPowerSeriesRingElem(R.ninv, a, len, prec)
  end

  function ZZModAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZModRingElem},
      len::Int, prec::Int)
    z = new()
    ccall((:fmpz_mod_poly_init2, libflint), Nothing,
          (Ref{ZZModAbsPowerSeriesRingElem}, Int, Ref{fmpz_mod_ctx_struct}),
          z, len, p)
    for i = 1:len
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{ZZModAbsPowerSeriesRingElem}, Int, Ref{ZZRingElem}, Ref{fmpz_mod_ctx_struct}),
            z, i - 1, data(a[i]), p)
    end
    z.prec = prec
    finalizer(_fmpz_mod_abs_series_clear_fn, z)
    return z
  end

  function ZZModAbsPowerSeriesRingElem(R::ZZModRing, a::Vector{ZZModRingElem},
      len::Int, prec::Int)
    return ZZModAbsPowerSeriesRingElem(R.ninv, a, len, prec)
  end

  function ZZModAbsPowerSeriesRingElem(a::ZZModAbsPowerSeriesRingElem)
    z = new()
    p = base_ring(parent(a)).ninv
    ccall((:fmpz_mod_poly_init, libflint), Nothing,
          (Ref{ZZModAbsPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
          z, p)
    ccall((:fmpz_mod_poly_set, libflint), Nothing,
          (Ref{ZZModAbsPowerSeriesRingElem}, Ref{ZZModAbsPowerSeriesRingElem},
           Ref{fmpz_mod_ctx_struct}),
          z, a, p)
    z.prec = a.prec
    finalizer(_fmpz_mod_abs_series_clear_fn, z)
    return z
  end
end

function _fmpz_mod_abs_series_clear_fn(a::ZZModAbsPowerSeriesRingElem)
  ccall((:fmpz_mod_poly_clear, libflint), Nothing,
        (Ref{ZZModAbsPowerSeriesRingElem}, Ref{fmpz_mod_ctx_struct}),
        a, base_ring(parent(a)).ninv)
end


###############################################################################
#
#   FqRelPowerSeriesRing / FqRelPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct FqRelPowerSeriesRing <: SeriesRing{FqFieldElem}
  base_ring::FqField
  prec_max::Int
  S::Symbol

  function FqRelPowerSeriesRing(R::FqField, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(FqDefaultRelSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const FqDefaultRelSeriesID = CacheDictType{Tuple{FqField, Int, Symbol}, FqRelPowerSeriesRing}()

mutable struct FqRelPowerSeriesRingElem <: RelPowerSeriesRingElem{FqFieldElem}
  # fq_default_poly_struct is 48 bytes on 64 bit machine
  opaque::NTuple{48, Int8}
  # end of flint struct

  prec::Int
  val::Int
  parent::FqRelPowerSeriesRing

  function FqRelPowerSeriesRingElem(ctx::FqField)
    z = new()
    ccall((:fq_default_poly_init, libflint), Nothing,
          (Ref{FqRelPowerSeriesRingElem}, Ref{FqField}), z, ctx)
    finalizer(_fq_default_rel_series_clear_fn, z)
    return z
  end

  function FqRelPowerSeriesRingElem(ctx::FqField, a::Vector{FqFieldElem}, len::Int, prec::Int, val::Int)
    z = new()
    ccall((:fq_default_poly_init2, libflint), Nothing,
          (Ref{FqRelPowerSeriesRingElem}, Int, Ref{FqField}), z, len, ctx)
    for i = 1:len
      ccall((:fq_default_poly_set_coeff, libflint), Nothing,
            (Ref{FqRelPowerSeriesRingElem}, Int, Ref{FqFieldElem},
             Ref{FqField}), z, i - 1, a[i], ctx)
    end
    z.prec = prec
    z.val = val
    finalizer(_fq_default_rel_series_clear_fn, z)
    return z
  end

  function FqRelPowerSeriesRingElem(ctx::FqField, a::FqRelPowerSeriesRingElem)
    z = new()
    ccall((:fq_default_poly_init, libflint), Nothing,
          (Ref{FqRelPowerSeriesRingElem}, Ref{FqField}), z, ctx)
    ccall((:fq_default_poly_set, libflint), Nothing,
          (Ref{FqRelPowerSeriesRingElem}, Ref{FqRelPowerSeriesRingElem},
           Ref{FqField}), z, a, ctx)
    finalizer(_fq_default_rel_series_clear_fn, z)
    return z
  end
end

function _fq_default_rel_series_clear_fn(a::FqRelPowerSeriesRingElem)
  ctx = base_ring(a)
  ccall((:fq_default_poly_clear, libflint), Nothing,
        (Ref{FqRelPowerSeriesRingElem}, Ref{FqField}), a, ctx)
end

###############################################################################
#
#   FqPolyRepRelPowerSeriesRing / FqPolyRepRelPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct FqPolyRepRelPowerSeriesRing <: SeriesRing{FqPolyRepFieldElem}
  base_ring::FqPolyRepField
  prec_max::Int
  S::Symbol

  function FqPolyRepRelPowerSeriesRing(R::FqPolyRepField, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(FqRelSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const FqRelSeriesID = CacheDictType{Tuple{FqPolyRepField, Int, Symbol}, FqPolyRepRelPowerSeriesRing}()

mutable struct FqPolyRepRelPowerSeriesRingElem <: RelPowerSeriesRingElem{FqPolyRepFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  prec::Int
  val::Int
  parent::FqPolyRepRelPowerSeriesRing

  function FqPolyRepRelPowerSeriesRingElem(ctx::FqPolyRepField)
    z = new()
    ccall((:fq_poly_init, libflint), Nothing,
          (Ref{FqPolyRepRelPowerSeriesRingElem}, Ref{FqPolyRepField}), z, ctx)
    finalizer(_fq_rel_series_clear_fn, z)
    return z
  end

  function FqPolyRepRelPowerSeriesRingElem(ctx::FqPolyRepField, a::Vector{FqPolyRepFieldElem}, len::Int, prec::Int, val::Int)
    z = new()
    ccall((:fq_poly_init2, libflint), Nothing,
          (Ref{FqPolyRepRelPowerSeriesRingElem}, Int, Ref{FqPolyRepField}), z, len, ctx)
    for i = 1:len
      ccall((:fq_poly_set_coeff, libflint), Nothing,
            (Ref{FqPolyRepRelPowerSeriesRingElem}, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
            z, i - 1, a[i], ctx)
    end
    z.prec = prec
    z.val = val
    finalizer(_fq_rel_series_clear_fn, z)
    return z
  end

  function FqPolyRepRelPowerSeriesRingElem(ctx::FqPolyRepField, a::FqPolyRepRelPowerSeriesRingElem)
    z = new()
    ccall((:fq_poly_init, libflint), Nothing,
          (Ref{FqPolyRepRelPowerSeriesRingElem}, Ref{FqPolyRepField}), z, ctx)
    ccall((:fq_poly_set, libflint), Nothing,
          (Ref{FqPolyRepRelPowerSeriesRingElem}, Ref{FqPolyRepRelPowerSeriesRingElem}, Ref{FqPolyRepField}), z, a, ctx)
    finalizer(_fq_rel_series_clear_fn, z)
    return z
  end
end

function _fq_rel_series_clear_fn(a::FqPolyRepRelPowerSeriesRingElem)
  ctx = base_ring(a)
  ccall((:fq_poly_clear, libflint), Nothing,
        (Ref{FqPolyRepRelPowerSeriesRingElem}, Ref{FqPolyRepField}), a, ctx)
end

###############################################################################
#
#   FqAbsPowerSeriesRing / FqAbsPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct FqAbsPowerSeriesRing <: SeriesRing{FqFieldElem}
  base_ring::FqField
  prec_max::Int
  S::Symbol

  function FqAbsPowerSeriesRing(R::FqField, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(FqDefaultAbsSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const FqDefaultAbsSeriesID = CacheDictType{Tuple{FqField, Int, Symbol}, FqAbsPowerSeriesRing}()

mutable struct FqAbsPowerSeriesRingElem <: AbsPowerSeriesRingElem{FqFieldElem}
  # fq_default_poly_struct is 48 bytes on 64 bit machine
  opaque::NTuple{48, Int8}
  # end of flint struct

  prec::Int
  parent::FqAbsPowerSeriesRing

  function FqAbsPowerSeriesRingElem(ctx::FqField)
    z = new()
    ccall((:fq_default_poly_init, libflint), Nothing,
          (Ref{FqAbsPowerSeriesRingElem}, Ref{FqField}), z, ctx)
    finalizer(_fq_default_abs_series_clear_fn, z)
    return z
  end

  function FqAbsPowerSeriesRingElem(ctx::FqField, a::Vector{FqFieldElem}, len::Int, prec::Int)
    z = new()
    ccall((:fq_default_poly_init2, libflint), Nothing,
          (Ref{FqAbsPowerSeriesRingElem}, Int, Ref{FqField}), z, len, ctx)
    for i = 1:len
      ccall((:fq_default_poly_set_coeff, libflint), Nothing,
            (Ref{FqAbsPowerSeriesRingElem}, Int, Ref{FqFieldElem}, Ref{FqField}),
            z, i - 1, a[i], ctx)
    end
    z.prec = prec
    finalizer(_fq_default_abs_series_clear_fn, z)
    return z
  end

  function FqAbsPowerSeriesRingElem(ctx::FqField, a::FqAbsPowerSeriesRingElem)
    z = new()
    ccall((:fq_default_poly_init, libflint), Nothing,
          (Ref{FqAbsPowerSeriesRingElem}, Ref{FqField}), z, ctx)
    ccall((:fq_default_poly_set, libflint), Nothing,
          (Ref{FqAbsPowerSeriesRingElem}, Ref{FqAbsPowerSeriesRingElem}, Ref{FqField}), z, a, ctx)
    finalizer(_fq_default_abs_series_clear_fn, z)
    return z
  end
end

function _fq_default_abs_series_clear_fn(a::FqAbsPowerSeriesRingElem)
  ctx = base_ring(a)
  ccall((:fq_default_poly_clear, libflint), Nothing,
        (Ref{FqAbsPowerSeriesRingElem}, Ref{FqField}), a, ctx)
end

###############################################################################
#
#   FqPolyRepAbsPowerSeriesRing / FqPolyRepAbsPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct FqPolyRepAbsPowerSeriesRing <: SeriesRing{FqPolyRepFieldElem}
  base_ring::FqPolyRepField
  prec_max::Int
  S::Symbol

  function FqPolyRepAbsPowerSeriesRing(R::FqPolyRepField, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(FqAbsSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const FqAbsSeriesID = CacheDictType{Tuple{FqPolyRepField, Int, Symbol}, FqPolyRepAbsPowerSeriesRing}()

mutable struct FqPolyRepAbsPowerSeriesRingElem <: AbsPowerSeriesRingElem{FqPolyRepFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  prec::Int
  parent::FqPolyRepAbsPowerSeriesRing

  function FqPolyRepAbsPowerSeriesRingElem(ctx::FqPolyRepField)
    z = new()
    ccall((:fq_poly_init, libflint), Nothing,
          (Ref{FqPolyRepAbsPowerSeriesRingElem}, Ref{FqPolyRepField}), z, ctx)
    finalizer(_fq_abs_series_clear_fn, z)
    return z
  end

  function FqPolyRepAbsPowerSeriesRingElem(ctx::FqPolyRepField, a::Vector{FqPolyRepFieldElem}, len::Int, prec::Int)
    z = new()
    ccall((:fq_poly_init2, libflint), Nothing,
          (Ref{FqPolyRepAbsPowerSeriesRingElem}, Int, Ref{FqPolyRepField}), z, len, ctx)
    for i = 1:len
      ccall((:fq_poly_set_coeff, libflint), Nothing,
            (Ref{FqPolyRepAbsPowerSeriesRingElem}, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
            z, i - 1, a[i], ctx)
    end
    z.prec = prec
    finalizer(_fq_abs_series_clear_fn, z)
    return z
  end

  function FqPolyRepAbsPowerSeriesRingElem(ctx::FqPolyRepField, a::FqPolyRepAbsPowerSeriesRingElem)
    z = new()
    ccall((:fq_poly_init, libflint), Nothing,
          (Ref{FqPolyRepAbsPowerSeriesRingElem}, Ref{FqPolyRepField}), z, ctx)
    ccall((:fq_poly_set, libflint), Nothing,
          (Ref{FqPolyRepAbsPowerSeriesRingElem}, Ref{FqPolyRepAbsPowerSeriesRingElem}, Ref{FqPolyRepField}), z, a, ctx)
    finalizer(_fq_abs_series_clear_fn, z)
    return z
  end
end

function _fq_abs_series_clear_fn(a::FqPolyRepAbsPowerSeriesRingElem)
  ctx = base_ring(a)
  ccall((:fq_poly_clear, libflint), Nothing,
        (Ref{FqPolyRepAbsPowerSeriesRingElem}, Ref{FqPolyRepField}), a, ctx)
end

###############################################################################
#
#   fqPolyRepRelPowerSeriesRing / fqPolyRepRelPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct fqPolyRepRelPowerSeriesRing <: SeriesRing{fqPolyRepFieldElem}
  base_ring::fqPolyRepField
  prec_max::Int
  S::Symbol

  function fqPolyRepRelPowerSeriesRing(R::fqPolyRepField, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(FqNmodRelSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const FqNmodRelSeriesID = CacheDictType{Tuple{fqPolyRepField, Int, Symbol},
                                        fqPolyRepRelPowerSeriesRing}()

mutable struct fqPolyRepRelPowerSeriesRingElem <: RelPowerSeriesRingElem{fqPolyRepFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  prec::Int
  val::Int
  parent::fqPolyRepRelPowerSeriesRing

  function fqPolyRepRelPowerSeriesRingElem(ctx::fqPolyRepField)
    z = new()
    ccall((:fq_nmod_poly_init, libflint), Nothing,
          (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepField}), z, ctx)
    finalizer(_fq_nmod_rel_series_clear_fn, z)
    return z
  end

  function fqPolyRepRelPowerSeriesRingElem(ctx::fqPolyRepField, a::Vector{fqPolyRepFieldElem}, len::Int, prec::Int, val::Int)
    z = new()
    ccall((:fq_nmod_poly_init2, libflint), Nothing,
          (Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepField}), z, len, ctx)
    for i = 1:len
      ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
            (Ref{fqPolyRepRelPowerSeriesRingElem}, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
            z, i - 1, a[i], ctx)
    end
    z.prec = prec
    z.val = val
    finalizer(_fq_nmod_rel_series_clear_fn, z)
    return z
  end

  function fqPolyRepRelPowerSeriesRingElem(ctx::fqPolyRepField, a::fqPolyRepRelPowerSeriesRingElem)
    z = new()
    ccall((:fq_nmod_poly_init, libflint), Nothing,
          (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepField}), z, ctx)
    ccall((:fq_nmod_poly_set, libflint), Nothing,
          (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepField}), z, a, ctx)
    finalizer(_fq_nmod_rel_series_clear_fn, z)
    return z
  end
end

function _fq_nmod_rel_series_clear_fn(a::fqPolyRepRelPowerSeriesRingElem)
  ctx = base_ring(a)
  ccall((:fq_nmod_poly_clear, libflint), Nothing,
        (Ref{fqPolyRepRelPowerSeriesRingElem}, Ref{fqPolyRepField}), a, ctx)
end

###############################################################################
#
#   fqPolyRepAbsPowerSeriesRing / fqPolyRepAbsPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct fqPolyRepAbsPowerSeriesRing <: SeriesRing{fqPolyRepFieldElem}
  base_ring::fqPolyRepField
  prec_max::Int
  S::Symbol

  function fqPolyRepAbsPowerSeriesRing(R::fqPolyRepField, prec::Int, s::Symbol,
      cached::Bool = true)
    return get_cached!(FqNmodAbsSeriesID, (R, prec, s), cached) do
      return new(R, prec, s)
    end
  end
end

const FqNmodAbsSeriesID = CacheDictType{Tuple{fqPolyRepField, Int, Symbol},
                                        fqPolyRepAbsPowerSeriesRing}()

mutable struct fqPolyRepAbsPowerSeriesRingElem <: AbsPowerSeriesRingElem{fqPolyRepFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  prec::Int
  parent::fqPolyRepAbsPowerSeriesRing

  function fqPolyRepAbsPowerSeriesRingElem(ctx::fqPolyRepField)
    z = new()
    ccall((:fq_nmod_poly_init, libflint), Nothing,
          (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepField}), z, ctx)
    finalizer(_fq_nmod_abs_series_clear_fn, z)
    return z
  end

  function fqPolyRepAbsPowerSeriesRingElem(ctx::fqPolyRepField, a::Vector{fqPolyRepFieldElem}, len::Int, prec::Int)
    z = new()
    ccall((:fq_nmod_poly_init2, libflint), Nothing,
          (Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepField}), z, len, ctx)
    for i = 1:len
      ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
            (Ref{fqPolyRepAbsPowerSeriesRingElem}, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
            z, i - 1, a[i], ctx)
    end
    z.prec = prec
    finalizer(_fq_nmod_abs_series_clear_fn, z)
    return z
  end

  function fqPolyRepAbsPowerSeriesRingElem(ctx::fqPolyRepField, a::fqPolyRepAbsPowerSeriesRingElem)
    z = new()
    ccall((:fq_nmod_poly_init, libflint), Nothing,
          (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepField}), z, ctx)
    ccall((:fq_nmod_poly_set, libflint), Nothing,
          (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepField}), z, a, ctx)
    finalizer(_fq_nmod_abs_series_clear_fn, z)
    return z
  end
end

function _fq_nmod_abs_series_clear_fn(a::fqPolyRepAbsPowerSeriesRingElem)
  ctx = base_ring(a)
  ccall((:fq_nmod_poly_clear, libflint), Nothing,
        (Ref{fqPolyRepAbsPowerSeriesRingElem}, Ref{fqPolyRepField}), a, ctx)
end

###############################################################################
#
#   QQMatrixSpace / QQMatrix
#
###############################################################################

const QQMatrixSpace = AbstractAlgebra.Generic.MatSpace{QQFieldElem}

QQMatrixSpace(r::Int, c::Int) = QQMatrixSpace(QQ, r, c)

mutable struct QQMatrix <: MatElem{QQFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  view_parent

  # used by windows, not finalised!!
  function QQMatrix()
    return new()
  end

  function QQMatrix(r::Int, c::Int)
    z = new()
    ccall((:fmpq_mat_init, libflint), Nothing,
          (Ref{QQMatrix}, Int, Int), z, r, c)
    finalizer(_fmpq_mat_clear_fn, z)
    return z
  end

  function QQMatrix(r::Int, c::Int, arr::AbstractMatrix{QQFieldElem})
    z = QQMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        set!(el, arr[i, j])
      end
    end
    return z
  end

  function QQMatrix(r::Int, c::Int, arr::AbstractMatrix{ZZRingElem})
    z = QQMatrix(r, c)
    b = ZZRingElem(1)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        set!(el, arr[i, j], b)
      end
    end
    return z
  end


  function QQMatrix(r::Int, c::Int, arr::AbstractVector{QQFieldElem})
    z = QQMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        set!(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function QQMatrix(r::Int, c::Int, arr::AbstractVector{ZZRingElem})
    z = QQMatrix(r, c)
    b = ZZRingElem(1)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        set!(el, arr[(i-1)*c+j], b)
      end
    end
    return z
  end


  function QQMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Integer}
    z = QQMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        set!(el, QQFieldElem(arr[i, j]))
      end
    end
    return z
  end

  function QQMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Integer}
    z = QQMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        set!(el, QQFieldElem(arr[(i-1)*c+j]))
      end
    end
    return z
  end

  function QQMatrix(r::Int, c::Int, d::QQFieldElem)
    z = QQMatrix(r, c)
    GC.@preserve z for i = 1:min(r, c)
      el = mat_entry_ptr(z, i, i)
      set!(el, d)
    end
    return z
  end

  function QQMatrix(m::QQMatrix)
    z = new()
    ccall((:fmpq_mat_init_set, libflint), Nothing,
          (Ref{QQMatrix}, Ref{QQMatrix}), z, m)
    finalizer(_fmpq_mat_clear_fn, z)
    return z
  end
end

function _fmpq_mat_clear_fn(a::QQMatrix)
  ccall((:fmpq_mat_clear, libflint), Nothing, (Ref{QQMatrix},), a)
end

###############################################################################
#
#   ZZMatrixSpace / ZZMatrix
#
###############################################################################

const ZZMatrixSpace = AbstractAlgebra.Generic.MatSpace{ZZRingElem}

ZZMatrixSpace(r::Int, c::Int) = ZZMatrixSpace(ZZ, r, c)

mutable struct ZZMatrix <: MatElem{ZZRingElem}
  entries::Ptr{ZZRingElem}
  r::Int
  c::Int
  rows::Ptr{Ptr{ZZRingElem}}
  view_parent

  # Used by view, not finalised!!
  function ZZMatrix()
    return new()
  end

  function ZZMatrix(r::Int, c::Int)
    z = new()
    ccall((:fmpz_mat_init, libflint), Nothing,
          (Ref{ZZMatrix}, Int, Int), z, r, c)
    finalizer(_fmpz_mat_clear_fn, z)
    return z
  end

  function ZZMatrix(r::Int, c::Int, arr::AbstractMatrix{ZZRingElem})
    z = ZZMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        set!(el, arr[i, j])
      end
    end
    return z
  end

  function ZZMatrix(r::Int, c::Int, arr::AbstractVector{ZZRingElem})
    z = ZZMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        set!(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function ZZMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Integer}
    z = ZZMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        set!(el, ZZRingElem(arr[i, j]))
      end
    end
    return z
  end

  function ZZMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Integer}
    z = ZZMatrix(r, c)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        set!(el, ZZRingElem(arr[(i-1)*c+j]))
      end
    end
    return z
  end

  function ZZMatrix(r::Int, c::Int, d::ZZRingElem)
    z = ZZMatrix(r, c)
    GC.@preserve z for i = 1:min(r, c)
      el = mat_entry_ptr(z, i, i)
      set!(el, d)
    end
    return z
  end

  function ZZMatrix(m::ZZMatrix)
    z = new()
    ccall((:fmpz_mat_init_set, libflint), Nothing,
          (Ref{ZZMatrix}, Ref{ZZMatrix}), z, m)
    finalizer(_fmpz_mat_clear_fn, z)
    return z
  end
end

function _fmpz_mat_clear_fn(a::ZZMatrix)
  ccall((:fmpz_mat_clear, libflint), Nothing, (Ref{ZZMatrix},), a)
end

###############################################################################
#
#   zzModMatrixSpace / zzModMatrix
#
###############################################################################

const zzModMatrixSpace = AbstractAlgebra.Generic.MatSpace{zzModRingElem}

mutable struct zzModMatrix <: MatElem{zzModRingElem}
  entries::Ptr{Nothing}
  r::Int                  # Int
  c::Int                  # Int
  rows::Ptr{Nothing}
  n::UInt                # mp_limb_t / Culong
  ninv::UInt             # mp_limb_t / Culong
  norm::UInt             # mp_limb_t / Culong
  base_ring::zzModRing
  view_parent

  # Used by view, not finalised!!
  function zzModMatrix()
    z = new()
    return z
  end

  function zzModMatrix(r::Int, c::Int, n::UInt)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
          (Ref{zzModMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
    return z
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{UInt}, transpose::Bool = false)
    z = zzModMatrix(r, c, n)
    if transpose
      arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, mod(arr[i, j], n), i, j)
      end
    end
    return z
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractVector{UInt})
    z = zzModMatrix(r, c, n)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, mod(arr[(i - 1) * c + j], n), i, j)
      end
    end
    return z
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{ZZRingElem}, transpose::Bool = false)
    z = zzModMatrix(r, c, n)
    if transpose
      arr = Base.transpose(arr)
    end
    t = ZZRingElem()
    for i = 1:r
      for j = 1:c
        ccall((:fmpz_mod_ui, libflint), Nothing,
              (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt), t, arr[i, j], n)
        setindex_raw!(z, t, i, j)
      end
    end
    return z
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractVector{ZZRingElem})
    z = zzModMatrix(r, c, n)
    t = ZZRingElem()
    for i = 1:r
      for j = 1:c
        ccall((:fmpz_mod_ui, libflint), Nothing,
              (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt), t, arr[(i - 1) * c + j], n)
        setindex!(z, t, i, j)
      end
    end
    return z
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{T}, transpose::Bool = false) where {T <: Integer}
    arr_fmpz = map(ZZRingElem, arr)
    return zzModMatrix(r, c, n, arr_fmpz, transpose)
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractVector{T}) where {T <: Integer}
    arr_fmpz = map(ZZRingElem, arr)
    return zzModMatrix(r, c, n, arr_fmpz)
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{zzModRingElem}, transpose::Bool = false)
    z = zzModMatrix(r, c, n)
    if transpose
      arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, arr[i, j].data, i, j) # no reduction necessary
      end
    end
    return z
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractVector{zzModRingElem})
    z = zzModMatrix(r, c, n)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, arr[(i - 1) * c + j].data, i, j) # no reduction necessary
      end
    end
    return z
  end

  function zzModMatrix(n::UInt, b::ZZMatrix)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
          (Ref{zzModMatrix}, Int, Int, UInt), z, b.r, b.c, n)
    finalizer(_nmod_mat_clear_fn, z)
    ccall((:fmpz_mat_get_nmod_mat, libflint), Nothing,
          (Ref{zzModMatrix}, Ref{ZZMatrix}), z, b)
    return z
  end

  function zzModMatrix(n::Int, b::ZZMatrix)
    (n < 0) && error("Modulus must be positive")
    return zzModMatrix(UInt(n), b)
  end

  function zzModMatrix(n::ZZRingElem, b::ZZMatrix)
    (n < 0) && error("Modulus must be positive")
    (n > typemax(UInt)) &&
    error("Modulus must be smaller than ", ZZRingElem(typemax(UInt)))
    return zzModMatrix(UInt(n), b)
  end
end

function _nmod_mat_clear_fn(mat::zzModMatrix)
  ccall((:nmod_mat_clear, libflint), Nothing, (Ref{zzModMatrix}, ), mat)
end

###############################################################################
#
#   ZZModMatrixSpace / ZZModMatrix
#
###############################################################################

const ZZModMatrixSpace = AbstractAlgebra.Generic.MatSpace{ZZModRingElem}

mutable struct ZZModMatrix <: MatElem{ZZModRingElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  # end flint struct

  base_ring::ZZModRing
  view_parent

  # Used by view, not finalised!!
  function ZZModMatrix()
    z = new()
    return z
  end

  function ZZModMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct)
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
          (Ref{ZZModMatrix}, Int, Int, Ref{fmpz_mod_ctx_struct}), z, r, c, ctx)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    return z
  end

  function ZZModMatrix(r::Int, c::Int, R::ZZModRing)
    ZZModMatrix(r, c, R.ninv)
  end

  function ZZModMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{ZZRingElem}, transpose::Bool = false)
    z = ZZModMatrix(r, c, ctx)
    if transpose
      arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(arr[i, j], ctx), i, j)
      end
    end
    return z
  end

  function ZZModMatrix(r::Int, c::Int, R::ZZModRing, arr::AbstractMatrix{ZZRingElem}, transpose::Bool = false)
    ZZModMatrix(r, c, R.ninv, arr, transpose)
  end

  function ZZModMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{T}, transpose::Bool = false) where T <: Integer
    z = ZZModMatrix(r, c, ctx)
    if transpose
      arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(ZZRingElem(arr[i, j]), ctx), i, j)
      end
    end
    return z
  end

  function ZZModMatrix(r::Int, c::Int, R::ZZModRing, arr::AbstractMatrix{T}, transpose::Bool = false) where T <: Integer
    ZZModMatrix(r, c, R.ninv, arr, transpose)
  end

  function ZZModMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{ZZModRingElem}, transpose::Bool = false)
    # FIXME: Check compatibility between ctx and arr?
    z = ZZModMatrix(r, c, ctx)
    if transpose
      arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, arr[i, j].data, i, j)
      end
    end
    return z
  end

  function ZZModMatrix(r::Int, c::Int, R::ZZModRing, arr::AbstractMatrix{ZZModRingElem}, transpose::Bool = false)
    ZZModMatrix(r, c, R, arr, transpose)
  end

  function ZZModMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractVector{ZZRingElem})
    z = ZZModMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(arr[(i - 1)*c + j], ctx), i, j)
      end
    end
    return z
  end

  function ZZModMatrix(r::Int, c::Int, R::ZZModRing, arr::AbstractVector{ZZRingElem})
    ZZModMatrix(r, c, R.ninv, arr)
  end

  function ZZModMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractVector{T}) where T <: Integer
    z = ZZModMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(ZZRingElem(arr[(i - 1)*c + j]), ctx), i, j)
      end
    end
    return z
  end

  function ZZModMatrix(r::Int, c::Int, R::ZZModRing, arr::AbstractVector{T}) where T <: Integer
    ZZModMatrix(r, c, R.ninv, arr)
  end

  function ZZModMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractVector{ZZModRingElem})
    # FIXME: Check compatibility between ctx and arr?
    z = ZZModMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, arr[(i - 1)*c + j].data, i, j)
      end
    end
    return z
  end

  function ZZModMatrix(n::UInt, b::ZZMatrix)
    return ZZModMatrix(ZZ(n), b)
  end

  function ZZModMatrix(n::Int, b::ZZMatrix)
    return ZZModMatrix(ZZ(n), b)
  end

  function ZZModMatrix(n::ZZRingElem, b::ZZMatrix)
    (n < 2) && error("Modulus must be >= 2")
    z = new()
    R = ZZModRing(n)
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
          (Ref{ZZModMatrix}, Int, Int, Ref{fmpz_mod_ctx_struct}),
          z, b.r, b.c, R.ninv)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    for i = 1:b.r
      for j = 1:b.c
        setindex_raw!(z, _reduce(b[i,j], R.ninv), i, j)
      end
    end
    return z
  end
end


function _fmpz_mod_mat_clear_fn(mat::ZZModMatrix)
  ccall((:fmpz_mod_mat_clear, libflint), Nothing,
        (Ref{ZZModMatrix}, Ref{Nothing}), mat, C_NULL) # Hack
end

###############################################################################
#
#   FpMatrixSpace / FpMatrix
#
###############################################################################

const FpMatrixSpace = AbstractAlgebra.Generic.MatSpace{FpFieldElem}

mutable struct FpMatrix <: MatElem{FpFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  # end flint struct

  base_ring::FpField
  view_parent

  # Used by view, not finalised!!
  function FpMatrix()
    z = new()
    return z
  end

  function FpMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct)
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
          (Ref{FpMatrix}, Int, Int, Ref{fmpz_mod_ctx_struct}), z, r, c, ctx)
    finalizer(_gfp_fmpz_mat_clear_fn, z)
    return z
  end

  function FpMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{ZZRingElem}, transpose::Bool = false)
    z = FpMatrix(r, c, ctx)
    if transpose
      arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(arr[i, j], ctx), i, j)
      end
    end
    return z
  end

  function FpMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{T}, transpose::Bool = false) where T <: Integer
    z = FpMatrix(r, c, ctx)
    if transpose
      arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(ZZRingElem(arr[i, j]), ctx), i, j)
      end
    end
    return z
  end

  function FpMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{FpFieldElem}, transpose::Bool = false)
    # FIXME: Check compatibility between ctx and arr?
    z = FpMatrix(r, c, ctx)
    if transpose
      arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, arr[i, j].data, i, j)
      end
    end
    return z
  end

  function FpMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractVector{ZZRingElem})
    z = FpMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(arr[(i - 1)*c + j], ctx), i, j)
      end
    end
    return z
  end

  function FpMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractVector{T}) where T <: Integer
    z = FpMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(ZZRingElem(arr[(i - 1)*c + j]), ctx), i, j)
      end
    end
    return z
  end

  function FpMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractVector{FpFieldElem})
    # FIXME: Check compatibility between ctx and arr?
    z = FpMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, arr[(i - 1)*c + j].data, i, j)
      end
    end
    return z
  end
end

function _gfp_fmpz_mat_clear_fn(mat::FpMatrix)
  ccall((:fmpz_mod_mat_clear, libflint), Nothing,
        (Ref{FpMatrix}, Ref{Nothing}), mat, C_NULL) # Hack
end

################################################################################
#
#   fpMatrixSpace / fpMatrix
#
###############################################################################

const fpMatrixSpace = AbstractAlgebra.Generic.MatSpace{fpFieldElem}

mutable struct fpMatrix <: MatElem{fpFieldElem}
  entries::Ptr{Nothing}
  r::Int                  # Int
  c::Int                  # Int
  rows::Ptr{Nothing}
  n::UInt                # mp_limb_t / Culong
  ninv::UInt             # mp_limb_t / Culong
  norm::UInt             # mp_limb_t / Culong
  base_ring::fpField
  view_parent

  # Used by view, not finalised!!
  function fpMatrix()
    z = new()
    return z
  end

  function fpMatrix(r::Int, c::Int, n::UInt)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
          (Ref{fpMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
    return z
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{UInt}, transpose::Bool = false)
    z = fpMatrix(r, c, n)
    if transpose
      arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, mod(arr[i, j], n), i, j)
      end
    end
    return z
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractVector{UInt})
    z = fpMatrix(r, c, n)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, mod(arr[(i - 1) * c + j], n), i, j)
      end
    end
    return z
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{ZZRingElem}, transpose::Bool = false)
    z = fpMatrix(r, c, n)
    if transpose
      arr = Base.transpose(arr)
    end
    t = ZZRingElem()
    for i = 1:r
      for j = 1:c
        ccall((:fmpz_mod_ui, libflint), Nothing,
              (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt), t, arr[i, j], n)
        setindex_raw!(z, t, i, j)
      end
    end
    return z
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractVector{ZZRingElem})
    z = fpMatrix(r, c, n)
    t = ZZRingElem()
    for i = 1:r
      for j = 1:c
        ccall((:fmpz_mod_ui, libflint), Nothing,
              (Ref{ZZRingElem}, Ref{ZZRingElem}, UInt), t, arr[(i - 1) * c + j], n)
        setindex!(z, t, i, j)
      end
    end
    return z
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{T}, transpose::Bool = false) where {T <: Integer}
    arr_fmpz = map(ZZRingElem, arr)
    return fpMatrix(r, c, n, arr_fmpz, transpose)
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractVector{T}) where {T <: Integer}
    arr_fmpz = map(ZZRingElem, arr)
    return fpMatrix(r, c, n, arr_fmpz)
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{fpFieldElem}, transpose::Bool = false)
    z = fpMatrix(r, c, n)
    if transpose
      arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, arr[i, j].data, i, j) # no reduction necessary
      end
    end
    return z
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractVector{fpFieldElem})
    z = fpMatrix(r, c, n)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, arr[(i - 1) * c + j].data, i, j) # no reduction necessary
      end
    end
    return z
  end

  function fpMatrix(n::UInt, b::ZZMatrix)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
          (Ref{fpMatrix}, Int, Int, UInt), z, b.r, b.c, n)
    finalizer(_gfp_mat_clear_fn, z)
    ccall((:fmpz_mat_get_nmod_mat, libflint), Nothing,
          (Ref{fpMatrix}, Ref{ZZMatrix}), z, b)
    return z
  end

  function fpMatrix(n::Int, b::ZZMatrix)
    (n < 0) && error("Modulus must be positive")
    return fpMatrix(UInt(n), b)
  end

  function fpMatrix(n::ZZRingElem, b::ZZMatrix)
    (n < 0) && error("Modulus must be positive")
    (n > typemax(UInt)) &&
    error("Modulus must be smaller than ", ZZRingElem(typemax(UInt)))
    return fpMatrix(UInt(n), b)
  end
end

function _gfp_mat_clear_fn(mat::fpMatrix)
  ccall((:nmod_mat_clear, libflint), Nothing, (Ref{fpMatrix}, ), mat)
end

###############################################################################
#
#   FqPolyRing / FqPolyRingElem
#
###############################################################################

@attributes mutable struct FqPolyRing <: PolyRing{FqFieldElem}
  base_ring::FqField
  S::Symbol

  function FqPolyRing(R::FqField, s::Symbol, cached::Bool = true)
    return get_cached!(FqDefaultPolyID, (R, s), cached) do
      return new(R,s)
    end
  end
end

const FqDefaultPolyID = CacheDictType{Tuple{FqField, Symbol}, FqPolyRing}()

mutable struct FqPolyRingElem <: PolyRingElem{FqFieldElem}
  # fq_default_poly_struct is 48 bytes on 64 bit machine
  opaque::NTuple{48, Int8}
  # end of flint struct

  parent::FqPolyRing

  function FqPolyRingElem(ctx::FqField)
    z = new()
    ccall((:fq_default_poly_init, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{FqField}), z, ctx)
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::FqPolyRingElem, ctx::FqField)
    z = new()
    ccall((:fq_default_poly_init, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{FqField}), z, ctx)
    ccall((:fq_default_poly_set, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{FqPolyRingElem}, Ref{FqField}),
          z, a, ctx)
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::FqFieldElem, ctx::FqField)
    z = new()
    ccall((:fq_default_poly_init, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{FqField}), z, ctx)
    ccall((:fq_default_poly_set_fq_default, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{FqFieldElem}, Ref{FqField}),
          z, a, ctx)
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::Vector{FqFieldElem}, ctx::FqField)
    z = new()
    ccall((:fq_default_poly_init2, libflint), Nothing,
          (Ref{FqPolyRingElem}, Int, Ref{FqField}),
          z, length(a), ctx)
    for i = 1:length(a)
      ccall((:fq_default_poly_set_coeff, libflint), Nothing,
            (Ref{FqPolyRingElem}, Int, Ref{FqFieldElem}, Ref{FqField}),
            z, i - 1, a[i], ctx)
    end
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::Vector{ZZRingElem}, ctx::FqField)
    z = new()
    temp = ctx()
    ccall((:fq_default_poly_init2, libflint), Nothing,
          (Ref{FqPolyRingElem}, Int, Ref{FqField}),
          z, length(a), ctx)
    for i = 1:length(a)
      temp = ctx(a[i])
      ccall((:fq_default_poly_set_coeff, libflint), Nothing,
            (Ref{FqPolyRingElem}, Int, Ref{FqFieldElem}, Ref{FqField}),
            z, i - 1, temp, ctx)
    end
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::ZZPolyRingElem, ctx::FqField)
    z = new()
    ccall((:fq_default_poly_init2, libflint), Nothing,
          (Ref{FqPolyRingElem}, Int, Ref{FqField}),
          z, length(a), ctx)
    ccall((:fq_default_poly_set_fmpz_poly, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{ZZPolyRingElem}, Ref{FqField}),
          z, a, ctx)
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::zzModPolyRingElem, ctx::FqField)
    z = new()
    ccall((:fq_default_poly_init2, libflint), Nothing,
          (Ref{FqPolyRingElem}, Int, Ref{FqField}),
          z, length(a), ctx)
    ccall((:fq_default_poly_set_nmod_poly, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{zzModPolyRingElem}, Ref{FqField}),
          z, a, ctx)
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::fpPolyRingElem, ctx::FqField)
    z = new()
    ccall((:fq_default_poly_init2, libflint), Nothing,
          (Ref{FqPolyRingElem}, Int, Ref{FqField}),
          z, length(a), ctx)
    ccall((:fq_default_poly_set_nmod_poly, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{fpPolyRingElem}, Ref{FqField}),
          z, a, ctx)
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::ZZModPolyRingElem, ctx::FqField)
    z = new()
    ccall((:fq_default_poly_init2, libflint), Nothing,
          (Ref{FqPolyRingElem}, Int, Ref{FqField}),
          z, length(a), ctx)
    ccall((:fq_default_poly_set_fmpz_mod_poly, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{ZZModPolyRingElem}, Ref{FqField}),
          z, a, ctx)
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::FpPolyRingElem, ctx::FqField)
    z = new()
    ccall((:fq_default_poly_init2, libflint), Nothing,
          (Ref{FqPolyRingElem}, Int, Ref{FqField}),
          z, length(a), ctx)
    ccall((:fq_default_poly_set_fmpz_mod_poly, libflint), Nothing,
          (Ref{FqPolyRingElem}, Ref{FpPolyRingElem}, Ref{FqField}),
          z, a, ctx)
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end
end

function _fq_default_poly_clear_fn(a::FqPolyRingElem)
  ccall((:fq_default_poly_clear, libflint), Nothing,
        (Ref{FqPolyRingElem}, Ref{FqField}),
        a, base_ring(a))
end

mutable struct fq_default_poly_factor
  # fq_default_ctx_struct is 32 bytes on 64 bit machine
  opaque::NTuple{32, Int8}
  # end of flint struct
  base_field::FqField

  function fq_default_poly_factor(ctx::FqField)
    z = new()
    ccall((:fq_default_poly_factor_init, libflint), Nothing,
          (Ref{fq_default_poly_factor}, Ref{FqField}), z, ctx)
    z.base_field = ctx
    finalizer(_fq_default_poly_factor_clear_fn, z)
    return z
  end
end

function _fq_default_poly_factor_clear_fn(a::fq_default_poly_factor)
  K = a.base_field
  ccall((:fq_default_poly_factor_clear, libflint), Nothing,
        (Ref{fq_default_poly_factor}, Ref{FqField}),
        a, K)
end

###############################################################################
#
#   FqPolyRepPolyRing / FqPolyRepPolyRingElem
#
###############################################################################

@attributes mutable struct FqPolyRepPolyRing <: PolyRing{FqPolyRepFieldElem}
  base_ring::FqPolyRepField
  S::Symbol

  function FqPolyRepPolyRing(R::FqPolyRepField, s::Symbol, cached::Bool = true)
    return get_cached!(FqPolyID, (R, s), cached) do
      return new(R,s)
    end
  end
end

const FqPolyID = CacheDictType{Tuple{FqPolyRepField, Symbol}, FqPolyRepPolyRing}()

mutable struct FqPolyRepPolyRingElem <: PolyRingElem{FqPolyRepFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  parent::FqPolyRepPolyRing

  function FqPolyRepPolyRingElem()
    z = new()
    ccall((:fq_poly_init, libflint), Nothing, (Ref{FqPolyRepPolyRingElem},), z)
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepPolyRingElem(a::FqPolyRepPolyRingElem)
    z = new()
    ctx = base_ring(parent(a))
    ccall((:fq_poly_init, libflint), Nothing,
          (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}), z, ctx)
    ccall((:fq_poly_set, libflint), Nothing,
          (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}),
          z, a, ctx)
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepPolyRingElem(a::FqPolyRepFieldElem)
    z = new()
    ctx = parent(a)
    ccall((:fq_poly_init, libflint), Nothing,
          (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepField}), z, ctx)
    ccall((:fq_poly_set_fq, libflint), Nothing,
          (Ref{FqPolyRepPolyRingElem}, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
          z, a, ctx)
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepPolyRingElem(a::Vector{FqPolyRepFieldElem})
    z = new()
    ctx = parent(a[1])
    ccall((:fq_poly_init2, libflint), Nothing,
          (Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepField}),
          z, length(a), ctx)
    for i = 1:length(a)
      ccall((:fq_poly_set_coeff, libflint), Nothing,
            (Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
            z, i - 1, a[i], ctx)
    end
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepPolyRingElem(a::Vector{ZZRingElem}, ctx::FqPolyRepField)
    z = new()
    temp = ctx()
    ccall((:fq_poly_init2, libflint), Nothing,
          (Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepField}),
          z, length(a), ctx)
    for i = 1:length(a)
      temp = ctx(a[i])
      ccall((:fq_poly_set_coeff, libflint), Nothing,
            (Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
            z, i - 1, temp, ctx)
    end
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepPolyRingElem(a::ZZPolyRingElem, ctx::FqPolyRepField)
    z = new()
    ccall((:fq_poly_init2, libflint), Nothing,
          (Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepField}),
          z, length(a), ctx)
    for i = 1:length(a)
      temp = ctx(coeff(a, i-1))
      ccall((:fq_poly_set_coeff, libflint), Nothing,
            (Ref{FqPolyRepPolyRingElem}, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
            z, i - 1, temp, ctx)
    end
    finalizer(_fq_poly_clear_fn, z)
    return z
  end
end

function _fq_poly_clear_fn(a::FqPolyRepPolyRingElem)
  ccall((:fq_poly_clear, libflint), Nothing, (Ref{FqPolyRepPolyRingElem},), a)
end

mutable struct fq_poly_factor
  poly::Ptr{FqPolyRepPolyRingElem}
  exp::Ptr{Int}
  num::Int
  alloc::Int
  base_field::FqPolyRepField

  function fq_poly_factor(ctx::FqPolyRepField)
    z = new()
    ccall((:fq_poly_factor_init, libflint), Nothing,
          (Ref{fq_poly_factor}, Ref{FqPolyRepField}), z, ctx)
    z.base_field = ctx
    finalizer(_fq_poly_factor_clear_fn, z)
    return z
  end
end

function _fq_poly_factor_clear_fn(a::fq_poly_factor)
  ccall((:fq_poly_factor_clear, libflint), Nothing,
        (Ref{fq_poly_factor}, Ref{FqPolyRepField}),
        a, a.base_field)
end

###############################################################################
#
#   fqPolyRepPolyRing / fqPolyRepPolyRingElem
#
###############################################################################

@attributes mutable struct fqPolyRepPolyRing <: PolyRing{fqPolyRepFieldElem}
  base_ring::fqPolyRepField
  S::Symbol

  function fqPolyRepPolyRing(R::fqPolyRepField, s::Symbol, cached::Bool = true)
    return get_cached!(FqNmodPolyID, (R, s), cached) do
      return new(R,s)
    end
  end
end

const FqNmodPolyID = CacheDictType{Tuple{fqPolyRepField, Symbol}, fqPolyRepPolyRing}()

mutable struct fqPolyRepPolyRingElem <: PolyRingElem{fqPolyRepFieldElem}
  coeffs::Ptr{Nothing}
  alloc::Int
  length::Int
  parent::fqPolyRepPolyRing

  function fqPolyRepPolyRingElem()
    z = new()
    ccall((:fq_nmod_poly_init, libflint), Nothing, (Ref{fqPolyRepPolyRingElem},), z)
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepPolyRingElem(a::fqPolyRepPolyRingElem)
    z = new()
    ctx = base_ring(parent(a))
    ccall((:fq_nmod_poly_init, libflint), Nothing,
          (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}), z, ctx)
    ccall((:fq_nmod_poly_set, libflint), Nothing,
          (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}),
          z, a, ctx)
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepPolyRingElem(a::fqPolyRepFieldElem)
    z = new()
    ctx = parent(a)
    ccall((:fq_nmod_poly_init, libflint), Nothing,
          (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepField}), z, ctx)
    ccall((:fq_nmod_poly_set_fq_nmod, libflint), Nothing,
          (Ref{fqPolyRepPolyRingElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
          z, a, ctx)
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepPolyRingElem(a::Vector{fqPolyRepFieldElem})
    z = new()
    ctx = parent(a[1])
    ccall((:fq_nmod_poly_init2, libflint), Nothing,
          (Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepField}),
          z, length(a), ctx)
    for i = 1:length(a)
      ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
            (Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
            z, i - 1, a[i], ctx)
    end
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepPolyRingElem(a::Vector{ZZRingElem}, ctx::fqPolyRepField)
    z = new()
    temp = ctx()
    ccall((:fq_nmod_poly_init2, libflint), Nothing,
          (Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepField}),
          z, length(a), ctx)
    for i = 1:length(a)
      temp = ctx(a[i])
      ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
            (Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
            z, i - 1, temp, ctx)
    end
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepPolyRingElem(a::ZZPolyRingElem, ctx::fqPolyRepField)
    z = new()
    ccall((:fq_nmod_poly_init2, libflint), Nothing,
          (Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepField}),
          z, length(a), ctx)
    for i = 1:length(a)
      temp = ctx(coeff(a,i-1))
      ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
            (Ref{fqPolyRepPolyRingElem}, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
            z, i - 1, temp, ctx)
    end
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end
end

function _fq_nmod_poly_clear_fn(a::fqPolyRepPolyRingElem)
  ccall((:fq_nmod_poly_clear, libflint), Nothing, (Ref{fqPolyRepPolyRingElem},), a)
end

mutable struct fq_nmod_poly_factor
  poly::Ptr{fqPolyRepPolyRingElem}
  exp::Ptr{Int}
  num::Int
  alloc::Int
  base_field::fqPolyRepField

  function fq_nmod_poly_factor(ctx::fqPolyRepField)
    z = new()
    ccall((:fq_nmod_poly_factor_init, libflint), Nothing,
          (Ref{fq_nmod_poly_factor}, Ref{fqPolyRepField}), z, ctx)
    z.base_field = ctx
    finalizer(_fq_nmod_poly_factor_clear_fn, z)
    return z
  end
end

function _fq_nmod_poly_factor_clear_fn(a::fq_nmod_poly_factor)
  ccall((:fq_nmod_poly_factor_clear, libflint), Nothing,
        (Ref{fq_nmod_poly_factor}, Ref{fqPolyRepField}),
        a, a.base_field)
end

###############################################################################
#
#   FqMatrixSpace/FqMatrix
#
###############################################################################

const FqMatrixSpace = AbstractAlgebra.Generic.MatSpace{FqFieldElem}

mutable struct FqMatrix <: MatElem{FqFieldElem}
  # fq_default_mat_struct is 56 bytes on 64 bit machine
  opaque::NTuple{56, Int8}
  # end of flint struct

  base_ring::FqField
  view_parent

  # used by windows, not finalised!!
  function FqMatrix()
    return new()
  end

  function FqMatrix(r::Int, c::Int, ctx::FqField)
    z = new()
    ccall((:fq_default_mat_init, libflint), Nothing,
          (Ref{FqMatrix}, Int, Int, Ref{FqField}),
          z, r, c, ctx)
    z.base_ring = ctx
    finalizer(_fq_default_mat_clear_fn, z)
    return z
  end

  function FqMatrix(r::Int, c::Int, arr::AbstractMatrix{FqFieldElem}, ctx::FqField)
    z = FqMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        ccall((:fq_default_mat_entry_set, libflint), Nothing,
              (Ref{FqMatrix}, Int, Int, Ref{FqFieldElem},
               Ref{FqField}),
              z, i - 1, j - 1, arr[i, j], ctx)
      end
    end
    return z
  end

  function FqMatrix(r::Int, c::Int, arr::AbstractVector{FqFieldElem}, ctx::FqField)
    z = FqMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        ccall((:fq_default_mat_entry_set, libflint), Nothing,
              (Ref{FqMatrix}, Int, Int, Ref{FqFieldElem},
               Ref{FqField}),
              z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
      end
    end
    return z
  end

  function FqMatrix(r::Int, c::Int, arr::AbstractMatrix{ZZRingElem}, ctx::FqField)
    z = FqMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        ccall((:fq_default_mat_entry_set_fmpz, libflint), Nothing,
              (Ref{FqMatrix}, Int, Int, Ref{ZZRingElem},
               Ref{FqField}),
              z, i - 1, j - 1, arr[i, j], ctx)
      end
    end
    return z
  end

  function FqMatrix(r::Int, c::Int, arr::AbstractVector{ZZRingElem}, ctx::FqField)
    z = FqMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        ccall((:fq_default_mat_entry_set_fmpz, libflint), Nothing,
              (Ref{FqMatrix}, Int, Int, Ref{ZZRingElem},
               Ref{FqField}),
              z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
      end
    end
    return z
  end

  function FqMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, ctx::FqField) where {T <: Integer}
    z = FqMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        ccall((:fq_default_mat_entry_set, libflint), Nothing,
              (Ref{FqMatrix}, Int, Int, Ref{FqFieldElem},
               Ref{FqField}),
              z, i - 1, j - 1, ctx(arr[i, j]), ctx)
      end
    end
    return z
  end

  function FqMatrix(r::Int, c::Int, arr::AbstractVector{T}, ctx::FqField) where {T <: Integer}
    z = FqMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        ccall((:fq_default_mat_entry_set, libflint), Nothing,
              (Ref{FqMatrix}, Int, Int, Ref{FqFieldElem},
               Ref{FqField}),
              z, i - 1, j - 1, ctx(arr[(i - 1) * c + j]), ctx)
      end
    end
    return z
  end

  function FqMatrix(r::Int, c::Int, d::FqFieldElem)
    ctx = parent(d)
    z = FqMatrix(r, c, ctx)
    for i = 1:min(r, c)
      ccall((:fq_default_mat_entry_set, libflint), Nothing,
            (Ref{FqMatrix}, Int, Int, Ref{FqFieldElem},
             Ref{FqField}), z, i - 1, i - 1, d, ctx)
    end
    return z
  end

  function FqMatrix(m::ZZMatrix, ctx::FqField)
    r = nrows(m)
    c = ncols(m)
    z = FqMatrix(r, c, ctx)
    ccall((:fq_default_mat_set_fmpz_mat, libflint), Nothing,
          (Ref{FqMatrix}, Ref{ZZMatrix}, Ref{FqField}),
          z, m, ctx)
    return z
  end

  function FqMatrix(m::ZZModMatrix, ctx::FqField)
    r = nrows(m)
    c = ncols(m)
    z = FqMatrix(r, c, ctx)
    ccall((:fq_default_mat_set_fmpz_mod_mat, libflint), Nothing,
          (Ref{FqMatrix}, Ref{ZZModMatrix}, Ref{FqField}),
          z, m, ctx)
    return z
  end

  function FqMatrix(m::zzModMatrix, ctx::FqField)
    r = nrows(m)
    c = ncols(m)
    z = FqMatrix(r, c, ctx)
    ccall((:fq_default_mat_set_nmod_mat, libflint), Nothing,
          (Ref{FqMatrix}, Ref{zzModMatrix}, Ref{FqField}),
          z, m, ctx)
    return z
  end

  function FqMatrix(m::fpMatrix, ctx::FqField)
    r = nrows(m)
    c = ncols(m)
    z = FqMatrix(r, c, ctx)
    ccall((:fq_default_mat_set_nmod_mat, libflint), Nothing,
          (Ref{FqMatrix}, Ref{fpMatrix}, Ref{FqField}),
          z, m, ctx)
    return z
  end
end

function _fq_default_mat_clear_fn(a::FqMatrix)
  ccall((:fq_default_mat_clear, libflint), Nothing,
        (Ref{FqMatrix}, Ref{FqField}), a, base_ring(a))
end

###############################################################################
#
#   FqPolyRepMatrixSpace/FqPolyRepMatrix
#
###############################################################################

const FqPolyRepMatrixSpace = AbstractAlgebra.Generic.MatSpace{FqPolyRepFieldElem}

mutable struct FqPolyRepMatrix <: MatElem{FqPolyRepFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  base_ring::FqPolyRepField
  view_parent

  # used by windows, not finalised!!
  function FqPolyRepMatrix()
    return new()
  end

  function FqPolyRepMatrix(r::Int, c::Int, ctx::FqPolyRepField)
    z = new()
    ccall((:fq_mat_init, libflint), Nothing,
          (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepField}), z, r, c, ctx)
    z.base_ring = ctx
    finalizer(_fq_mat_clear_fn, z)
    return z
  end

  function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractMatrix{FqPolyRepFieldElem}, ctx::FqPolyRepField)
    z = FqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        ccall((:fq_mat_entry_set, libflint), Nothing,
              (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
              z, i - 1, j - 1, arr[i, j], ctx)
      end
    end
    return z
  end

  function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{FqPolyRepFieldElem}, ctx::FqPolyRepField)
    z = FqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        ccall((:fq_mat_entry_set, libflint), Nothing,
              (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
              z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
      end
    end
    return z
  end

  function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractMatrix{ZZRingElem}, ctx::FqPolyRepField)
    z = FqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        ccall((:fq_set_fmpz, libflint), Nothing,
              (Ptr{FqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{FqPolyRepField}), el, arr[i, j], ctx)
      end
    end
    return z
  end

  function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{ZZRingElem}, ctx::FqPolyRepField)
    z = FqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        ccall((:fq_set_fmpz, libflint), Nothing,
              (Ptr{FqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{FqPolyRepField}), el, arr[(i - 1) * c + j], ctx)
      end
    end
    return z
  end

  function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, ctx::FqPolyRepField) where {T <: Integer}
    z = FqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        ccall((:fq_mat_entry_set, libflint), Nothing,
              (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
              z, i - 1, j - 1, ctx(arr[i, j]), ctx)
      end
    end
    return z
  end

  function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{T}, ctx::FqPolyRepField) where {T <: Integer}
    z = FqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        ccall((:fq_mat_entry_set, libflint), Nothing,
              (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
              z, i - 1, j - 1, ctx(arr[(i - 1) * c + j]), ctx)
      end
    end
    return z
  end

  function FqPolyRepMatrix(r::Int, c::Int, d::FqPolyRepFieldElem)
    ctx = parent(d)
    z = FqPolyRepMatrix(r, c, ctx)
    for i = 1:min(r, c)
      ccall((:fq_mat_entry_set, libflint), Nothing,
            (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, i - 1, i- 1, d, ctx)
    end
    return z
  end

  function FqPolyRepMatrix(m::ZZMatrix, ctx::FqPolyRepField)
    r = nrows(m)
    c = ncols(m)
    z = FqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el1 = mat_entry_ptr(z, i, j)
        el2 = mat_entry_ptr(m, i, j)

        ccall((:fq_set_fmpz, libflint), Nothing,
              (Ptr{FqPolyRepFieldElem}, Ptr{ZZRingElem}, Ref{FqPolyRepField}), el1, el2, ctx)
      end
    end
    return z
  end
end

function _fq_mat_clear_fn(a::FqPolyRepMatrix)
  ccall((:fq_mat_clear, libflint), Nothing, (Ref{FqPolyRepMatrix}, Ref{FqPolyRepField}), a, base_ring(a))
end

###############################################################################
#
#   fqPolyRepMatrixSpace/fqPolyRepMatrix
#
###############################################################################

const fqPolyRepMatrixSpace = AbstractAlgebra.Generic.MatSpace{fqPolyRepFieldElem}

mutable struct fqPolyRepMatrix <: MatElem{fqPolyRepFieldElem}
  entries::Ptr{Nothing}
  r::Int
  c::Int
  rows::Ptr{Nothing}
  base_ring::fqPolyRepField
  view_parent

  # used by windows, not finalised!!
  function fqPolyRepMatrix()
    return new()
  end

  function fqPolyRepMatrix(r::Int, c::Int, ctx::fqPolyRepField)
    z = new()
    ccall((:fq_nmod_mat_init, libflint), Nothing,
          (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepField}), z, r, c, ctx)
    z.base_ring = ctx
    finalizer(_fq_nmod_mat_clear_fn, z)
    return z
  end

  function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractMatrix{fqPolyRepFieldElem}, ctx::fqPolyRepField)
    z = fqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
              (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
              z, i - 1, j - 1, arr[i, j], ctx)
      end
    end
    return z
  end

  function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{fqPolyRepFieldElem}, ctx::fqPolyRepField)
    z = fqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
              (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
              z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
      end
    end
    return z
  end

  function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractMatrix{ZZRingElem}, ctx::fqPolyRepField)
    z = fqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        ccall((:fq_nmod_set_fmpz, libflint), Nothing,
              (Ptr{fqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{fqPolyRepField}), el, arr[i, j], ctx)
      end
    end
    return z
  end

  function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{ZZRingElem}, ctx::fqPolyRepField)
    z = fqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el = mat_entry_ptr(z, i, j)
        ccall((:fq_nmod_set_fmpz, libflint), Nothing,
              (Ptr{fqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{fqPolyRepField}), el, arr[(i - 1) * c + j], ctx)
      end
    end
    return z
  end

  function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, ctx::fqPolyRepField) where {T <: Integer}
    z = fqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
              (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
              z, i - 1, j - 1, ctx(arr[i, j]), ctx)
      end
    end
    return z
  end

  function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{T}, ctx::fqPolyRepField) where {T <: Integer}
    z = fqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
              (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
              z, i - 1, j - 1, ctx(arr[(i - 1) * c + j]), ctx)
      end
    end
    return z
  end

  function fqPolyRepMatrix(r::Int, c::Int, d::fqPolyRepFieldElem)
    ctx = parent(d)
    z = fqPolyRepMatrix(r, c, ctx)
    for i = 1:min(r, c)
      ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
            (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), z, i - 1, i- 1, d, ctx)
    end
    return z
  end

  function fqPolyRepMatrix(m::ZZMatrix, ctx::fqPolyRepField)
    r = nrows(m)
    c = ncols(m)
    z = fqPolyRepMatrix(r, c, ctx)
    GC.@preserve z for i = 1:r
      for j = 1:c
        el1 = mat_entry_ptr(z, i, j)
        el2 = mat_entry_ptr(m, i, j)

        ccall((:fq_nmod_set_fmpz, libflint), Nothing,
              (Ptr{fqPolyRepFieldElem}, Ptr{ZZRingElem}, Ref{fqPolyRepField}), el1, el2, ctx)
      end
    end
    return z
  end
end

function _fq_nmod_mat_clear_fn(a::fqPolyRepMatrix)
  ccall((:fq_nmod_mat_clear, libflint), Nothing, (Ref{fqPolyRepMatrix}, Ref{fqPolyRepField}), a, base_ring(a))
end

################################################################################
#
#   Rand state
#
################################################################################

if NEW_FLINT
  mutable struct rand_ctx
    gmp_state::Ptr{Cvoid}
    randval::UInt
    randval2::UInt

    function rand_ctx()
      a = new()
      ccall((:flint_rand_init, libflint), Cvoid, (Ref{rand_ctx},), a)
      finalizer(_rand_ctx_clear_fn, a)
      return a
    end
  end

  function _rand_ctx_clear_fn(a::rand_ctx)
    ccall((:flint_rand_clear, libflint), Cvoid, (Ref{rand_ctx},), a)
    nothing
  end
else
  mutable struct rand_ctx
    data::NTuple{56, Int8}

    function rand_ctx()
      a = new()
      ccall((:flint_randinit, libflint), Cvoid, (Ref{rand_ctx},), a)
      finalizer(_rand_ctx_clear_fn, a)
      return a
    end
  end

  function _rand_ctx_clear_fn(a::rand_ctx)
    ccall((:flint_randclear, libflint), Cvoid, (Ref{rand_ctx},), a)
    nothing
  end
end

###############################################################################
#
#   FqMPolyRing / FqMPolyRingElem
#
###############################################################################

const _fq_default_mpoly_union = Union{AbstractAlgebra.Generic.MPoly{FqPolyRepFieldElem},
                                      fqPolyRepMPolyRingElem,
                                      fpMPolyRingElem,
                                      #FpMPolyRingElem
                                     }

@attributes mutable struct FqMPolyRing <: MPolyRing{FqFieldElem}
  data::Union{fpMPolyRing,
              #FpMPolyRing,
              fqPolyRepMPolyRing,
              AbstractAlgebra.Generic.MPolyRing{FqPolyRepFieldElem}}
  base_ring::FqField
  typ::Int    # keep these in sync with @fq_default_mpoly_do_op

  function FqMPolyRing(R::FqField, s::Vector{Symbol}, internal_ordering::Symbol = :lex, cached::Bool = true)
    return get_cached!(FqDefaultMPolyID, (R, s, internal_ordering), cached) do
      # in the following all constructors should use chached = false
      m = modulus(R)
      p = characteristic(R)
      if fits(UInt, p)
        Fq = Native.GF(UInt(p))
        if isone(degree(m))
          Fqx = polynomial_ring(Fq, s, cached = cached, internal_ordering = internal_ordering)[1]
          return new(Fqx, R, 3)
        end
        mm = polynomial_ring(Fq, "x")[1](lift(polynomial_ring(ZZ, "x")[1], m))
        Fq = Native.FiniteField(mm, R.var, cached = cached, check = false)[1]
        Fqx = polynomial_ring(Fq, s, cached = cached, internal_ordering = internal_ordering)[1]
        return new(Fqx, R, 2)
      end
      Fq = FqPolyRepField(m, Symbol(R.var), cached, check = false)
      Fqx = polynomial_ring(Fq, s, cached = cached, internal_ordering = internal_ordering)[1]
      return new(Fqx, R, 1)
    end::FqMPolyRing
  end
end

const FqDefaultMPolyID = CacheDictType{
                                       Tuple{FqField, Vector{Symbol}, Symbol},
                                       FqMPolyRing}()

mutable struct FqMPolyRingElem <: MPolyRingElem{FqFieldElem}
  parent::FqMPolyRing
  data::_fq_default_mpoly_union

  function FqMPolyRingElem(a::FqMPolyRing, b::_fq_default_mpoly_union)
    a.data == parent(b) || error("bad parents")
    return new(a, b)
  end
end

# julia fails to generate decent code unless it is all pasted in
macro fq_default_mpoly_do_op(f, R, a...)
  f = Expr(:escape, f)
  R = Expr(:escape, R)
  a = (Expr(:escape, ai) for ai in a)
  res = nothing
  for (tnum, T) in ((1, :(AbstractAlgebra.Generic.MPoly{FqPolyRepFieldElem})),
                    (2, :(fqPolyRepMPolyRingElem)),
                    (3, :(fpMPolyRingElem)),
                   )
    ret = (Expr(:(::), Expr(:(.), ai, QuoteNode(:data)), T) for ai in a)
    ret = Expr(:return, Expr(:call, :FqMPolyRingElem, R, Expr(:call, f, ret...)))
    if res == nothing
      res = ret
    else
      cond = Expr(:call, :(==), Expr(:(.), R, QuoteNode(:typ)), tnum)
      res = Expr(:if, cond, ret, res)
    end
  end
  return res
end

################################################################################
#
#   Type unions
#
################################################################################

const IntegerUnion = Union{Integer, ZZRingElem}

const ZmodNFmpzPolyRing = Union{ZZModPolyRing, FpPolyRing}

const Zmodn_poly = Union{zzModPolyRingElem, fpPolyRingElem}

const Zmodn_fmpz_poly = Union{ZZModPolyRingElem, FpPolyRingElem}

const Zmodn_mpoly = Union{zzModMPolyRingElem, fpMPolyRingElem}

const FlintPuiseuxSeriesElem{T} = Union{FlintPuiseuxSeriesRingElem{T},
                                        FlintPuiseuxSeriesFieldElem{T}} where T <: RingElem

const Zmodn_mat = Union{zzModMatrix, fpMatrix}

const Zmod_fmpz_mat = Union{ZZModMatrix, FpMatrix}

const FlintMPolyUnion = Union{ZZMPolyRingElem, QQMPolyRingElem, zzModMPolyRingElem, fpMPolyRingElem,
                              fqPolyRepMPolyRingElem, FpMPolyRingElem}


const ZZRingElemOrPtr = Union{ZZRingElem, Ref{ZZRingElem}, Ptr{ZZRingElem}}
const QQFieldElemOrPtr = Union{QQFieldElem, Ref{QQFieldElem}, Ptr{QQFieldElem}}
const zzModRingElemOrPtr = Union{zzModRingElem, Ref{zzModRingElem}, Ptr{zzModRingElem}}
const ZZModRingElemOrPtr = Union{ZZModRingElem, Ref{ZZModRingElem}, Ptr{ZZModRingElem}}
const fpFieldElemOrPtr = Union{fpFieldElem, Ref{fpFieldElem}, Ptr{fpFieldElem}}
const FpFieldElemOrPtr = Union{FpFieldElem, Ref{FpFieldElem}, Ptr{FpFieldElem}}
const fqPolyRepFieldElemOrPtr = Union{fqPolyRepFieldElem, Ref{fqPolyRepFieldElem}, Ptr{fqPolyRepFieldElem}}
const FqPolyRepFieldElemOrPtr = Union{FqPolyRepFieldElem, Ref{FqPolyRepFieldElem}, Ptr{FqPolyRepFieldElem}}
const FqFieldElemOrPtr = Union{FqFieldElem, Ref{FqFieldElem}, Ptr{FqFieldElem}}

const ZZPolyRingElemOrPtr = Union{ZZPolyRingElem, Ref{ZZPolyRingElem}, Ptr{ZZPolyRingElem}}
const QQPolyRingElemOrPtr = Union{QQPolyRingElem, Ref{QQPolyRingElem}, Ptr{QQPolyRingElem}}
const zzModPolyRingElemOrPtr = Union{zzModPolyRingElem, Ref{zzModPolyRingElem}, Ptr{zzModPolyRingElem}}
const ZZModPolyRingElemOrPtr = Union{ZZModPolyRingElem, Ref{ZZModPolyRingElem}, Ptr{ZZModPolyRingElem}}
const fpPolyRingElemOrPtr = Union{fpPolyRingElem, Ref{fpPolyRingElem}, Ptr{fpPolyRingElem}}
const FpPolyRingElemOrPtr = Union{FpPolyRingElem, Ref{FpPolyRingElem}, Ptr{FpPolyRingElem}}
const fqPolyRepPolyRingElemOrPtr = Union{fqPolyRepPolyRingElem, Ref{fqPolyRepPolyRingElem}, Ptr{fqPolyRepPolyRingElem}}
const FqPolyRepPolyRingElemOrPtr = Union{FqPolyRepPolyRingElem, Ref{FqPolyRepPolyRingElem}, Ptr{FqPolyRepPolyRingElem}}
const FqPolyRingElemOrPtr = Union{FqPolyRingElem, Ref{FqPolyRingElem}, Ptr{FqPolyRingElem}}

const ZZMPolyRingElemOrPtr = Union{ZZMPolyRingElem, Ref{ZZMPolyRingElem}, Ptr{ZZMPolyRingElem}}
const QQMPolyRingElemOrPtr = Union{QQMPolyRingElem, Ref{QQMPolyRingElem}, Ptr{QQMPolyRingElem}}
const zzModMPolyRingElemOrPtr = Union{zzModMPolyRingElem, Ref{zzModMPolyRingElem}, Ptr{zzModMPolyRingElem}}
# ZZModMPolyRingElem does not exits
const fpMPolyRingElemOrPtr = Union{fpMPolyRingElem, Ref{fpMPolyRingElem}, Ptr{fpMPolyRingElem}}
const FpMPolyRingElemOrPtr = Union{FpMPolyRingElem, Ref{FpMPolyRingElem}, Ptr{FpMPolyRingElem}}
const fqPolyRepMPolyRingElemOrPtr = Union{fqPolyRepMPolyRingElem, Ref{fqPolyRepMPolyRingElem}, Ptr{fqPolyRepMPolyRingElem}}
# FqPolyRepMPolyRingElem does not exits
const FqMPolyRingElemOrPtr = Union{FqMPolyRingElem, Ref{FqMPolyRingElem}, Ptr{FqMPolyRingElem}}

const ZZRelPowerSeriesRingElemOrPtr = Union{ZZRelPowerSeriesRingElem, Ref{ZZRelPowerSeriesRingElem}, Ptr{ZZRelPowerSeriesRingElem}}
const ZZAbsPowerSeriesRingElemOrPtr = Union{ZZAbsPowerSeriesRingElem, Ref{ZZAbsPowerSeriesRingElem}, Ptr{ZZAbsPowerSeriesRingElem}}
const QQRelPowerSeriesRingElemOrPtr = Union{QQRelPowerSeriesRingElem, Ref{QQRelPowerSeriesRingElem}, Ptr{QQRelPowerSeriesRingElem}}
const QQAbsPowerSeriesRingElemOrPtr = Union{QQAbsPowerSeriesRingElem, Ref{QQAbsPowerSeriesRingElem}, Ptr{QQAbsPowerSeriesRingElem}}
const zzModRelPowerSeriesRingElemOrPtr = Union{zzModRelPowerSeriesRingElem, Ref{zzModRelPowerSeriesRingElem}, Ptr{zzModRelPowerSeriesRingElem}}
const zzModAbsPowerSeriesRingElemOrPtr = Union{zzModAbsPowerSeriesRingElem, Ref{zzModAbsPowerSeriesRingElem}, Ptr{zzModAbsPowerSeriesRingElem}}
const ZZModRelPowerSeriesRingElemOrPtr = Union{ZZModRelPowerSeriesRingElem, Ref{ZZModRelPowerSeriesRingElem}, Ptr{ZZModRelPowerSeriesRingElem}}
const ZZModAbsPowerSeriesRingElemOrPtr = Union{ZZModAbsPowerSeriesRingElem, Ref{ZZModAbsPowerSeriesRingElem}, Ptr{ZZModAbsPowerSeriesRingElem}}
const fpRelPowerSeriesRingElemOrPtr = Union{fpRelPowerSeriesRingElem, Ref{fpRelPowerSeriesRingElem}, Ptr{fpRelPowerSeriesRingElem}}
const fpAbsPowerSeriesRingElemOrPtr = Union{fpAbsPowerSeriesRingElem, Ref{fpAbsPowerSeriesRingElem}, Ptr{fpAbsPowerSeriesRingElem}}
const FpRelPowerSeriesRingElemOrPtr = Union{FpRelPowerSeriesRingElem, Ref{FpRelPowerSeriesRingElem}, Ptr{FpRelPowerSeriesRingElem}}
const FpAbsPowerSeriesRingElemOrPtr = Union{FpAbsPowerSeriesRingElem, Ref{FpAbsPowerSeriesRingElem}, Ptr{FpAbsPowerSeriesRingElem}}
const fqPolyRepRelPowerSeriesRingElemOrPtr = Union{fqPolyRepRelPowerSeriesRingElem, Ref{fqPolyRepRelPowerSeriesRingElem}, Ptr{fqPolyRepRelPowerSeriesRingElem}}
const fqPolyRepAbsPowerSeriesRingElemOrPtr = Union{fqPolyRepAbsPowerSeriesRingElem, Ref{fqPolyRepAbsPowerSeriesRingElem}, Ptr{fqPolyRepAbsPowerSeriesRingElem}}
const FqPolyRepRelPowerSeriesRingElemOrPtr = Union{FqPolyRepRelPowerSeriesRingElem, Ref{FqPolyRepRelPowerSeriesRingElem}, Ptr{FqPolyRepRelPowerSeriesRingElem}}
const FqPolyRepAbsPowerSeriesRingElemOrPtr = Union{FqPolyRepAbsPowerSeriesRingElem, Ref{FqPolyRepAbsPowerSeriesRingElem}, Ptr{FqPolyRepAbsPowerSeriesRingElem}}
const FqRelPowerSeriesRingElemOrPtr = Union{FqRelPowerSeriesRingElem, Ref{FqRelPowerSeriesRingElem}, Ptr{FqRelPowerSeriesRingElem}}
const FqAbsPowerSeriesRingElemOrPtr = Union{FqAbsPowerSeriesRingElem, Ref{FqAbsPowerSeriesRingElem}, Ptr{FqAbsPowerSeriesRingElem}}

const ZZMatrixOrPtr = Union{ZZMatrix, Ref{ZZMatrix}, Ptr{ZZMatrix}}
const QQMatrixOrPtr = Union{QQMatrix, Ref{QQMatrix}, Ptr{QQMatrix}}
const zzModMatrixOrPtr = Union{zzModMatrix, Ref{zzModMatrix}, Ptr{zzModMatrix}}
const ZZModMatrixOrPtr = Union{ZZModMatrix, Ref{ZZModMatrix}, Ptr{ZZModMatrix}}
const fpMatrixOrPtr = Union{fpMatrix, Ref{fpMatrix}, Ptr{fpMatrix}}
const FpMatrixOrPtr = Union{FpMatrix, Ref{FpMatrix}, Ptr{FpMatrix}}
const fqPolyRepMatrixOrPtr = Union{fqPolyRepMatrix, Ref{fqPolyRepMatrix}, Ptr{fqPolyRepMatrix}}
const FqPolyRepMatrixOrPtr = Union{FqPolyRepMatrix, Ref{FqPolyRepMatrix}, Ptr{FqPolyRepMatrix}}
const FqMatrixOrPtr = Union{FqMatrix, Ref{FqMatrix}, Ptr{FqMatrix}}

const ZZLaurentSeriesRingElemOrPtr = Union{ZZLaurentSeriesRingElem, Ref{ZZLaurentSeriesRingElem}, Ptr{ZZLaurentSeriesRingElem}}

const FlintPuiseuxSeriesRingElemOrPtr{T <: RingElem} = Union{FlintPuiseuxSeriesRingElem{T}, Ref{FlintPuiseuxSeriesRingElem{T}}, Ptr{FlintPuiseuxSeriesRingElem{T}}}
const FlintPuiseuxSeriesFieldElemOrPtr{T <: RingElem} = Union{FlintPuiseuxSeriesFieldElem{T}, Ref{FlintPuiseuxSeriesFieldElem{T}}, Ptr{FlintPuiseuxSeriesFieldElem{T}}}

const PadicFieldElemOrPtr = Union{PadicFieldElem, Ref{PadicFieldElem}, Ptr{PadicFieldElem}}
const QadicFieldElemOrPtr = Union{QadicFieldElem, Ref{QadicFieldElem}, Ptr{QadicFieldElem}}

###############################################################################
#
#   Docstrings for the systematically added types in this file
#
###############################################################################

module DocstringInfo

const base_rings = Dict(
                        :ZZ => ("ZZRing", "\\mathbb Z"),
                        :QQ => ("QQField", "\\mathbb Q"),
                        :ZZMod => ("ZZModRing", "\\mathbb Z/n\\mathbb Z"),
                        :zzMod => ("zzModRing", "\\mathbb Z/n\\mathbb Z"),
                        :Fq => ("FqField", "\\mathbb F_q"),
                        :Fp => ("FpField", "\\mathbb F_p"),
                        :fp => ("fpField", "\\mathbb F_p"),
                        :FqPolyRep => ("FqPolyRepField", "\\mathbb F_q"),
                        :fqPolyRep => ("fqPolyRepField", "\\mathbb F_q"),
                       )

const constructions = Dict(
                           :MatrixSpace => ("MatSpace", "Module", "A matrix space", "matrix_space"),
                           :Matrix => ("MatElem", "ModuleElem", "A matrix", "matrix(::Ring)"),
                           :PolyRing => ("PolyRing", "Ring", "The polynomial ring", "polynomial_ring(R, :x)"),
                           :PolyRingElem => ("PolyRingElem", "RingElem", "A polynomial", "polynomial_ring(R, :x)"),
                           :MPolyRing => ("MPolyRing", "Ring", "A multivariate polynomial ring", "polynomial_ring(R, :x, :y)"),
                           :MPolyRingElem => ("MPolyRingElem", "RingElem", "A multivariate polynomial", "polynomial_ring(R, :x, :y)"),
                          )

@doc raw"""
    docstring(base::Symbol, suffix::Symbol)

Docstring for some constructions of rings.
"""
function docstring(base::Symbol, suffix::Symbol)
  name = String(base) * String(suffix)
  ring_name, latex = base_rings[base]
  abstract_type, super_type, description, reference = constructions[suffix]
  """
  $name <: $abstract_type{$(ring_name)Elem} <: $super_type

  $description over ``$latex``. See [`$reference`](@ref).
  """
end

for base in keys(base_rings), suffix in keys(constructions)
  d = docstring(base, suffix)
  name = Symbol(String(base)*String(suffix))
  Core.eval(parentmodule(DocstringInfo), :(Core.@doc $d $name))
end
end
