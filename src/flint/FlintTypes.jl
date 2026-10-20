###############################################################################
#
#   FlintTypes.jl : Parent and object types for FLINT
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
  cstruct::FlintC.fmpz

  function ZZRingElem()
    z = new()
    @ccall libflint.fmpz_init(z::Ref{ZZRingElem})::Nothing
    finalizer(_fmpz_clear_fn, z)
    return z
  end

  #this creates an ZZRingElem (fmpz) without a finalizer. Thus you need to
  #manually call `_fmpz_clean_fn` on it after use.
  #by bypassing the GC, this makes temorary variables more efficient.
  #if this makes no sense, don't use it....
  function ZZRingElem(::Val{:raw})
    z = new(0)
    return z
  end

  function ZZRingElem(x::Int)
    z = new()
    @ccall libflint.fmpz_init_set_si(z::Ref{ZZRingElem}, x::Int)::Nothing
    finalizer(_fmpz_clear_fn, z)
    return z
  end

  function ZZRingElem(x::UInt)
    z = new()
    @ccall libflint.fmpz_init_set_ui(z::Ref{ZZRingElem}, x::UInt)::Nothing
    finalizer(_fmpz_clear_fn, z)
    return z
  end

  function ZZRingElem(x::Integer)
    y = BigInt(x)::BigInt
    z = ZZRingElem()
    @ccall libflint.fmpz_set_mpz(z::Ref{ZZRingElem}, y::Ref{BigInt})::Nothing
    return z
  end

  function ZZRingElem(x::Ptr{Culong}, len::Clong)
    z = ZZRingElem()
    @ccall libflint.fmpz_set_ui_array(z::Ref{ZZRingElem}, x::Ptr{Culong}, len::Clong)::Nothing
    return z
  end

  function ZZRingElem(x::Float64)
    !isinteger(x) && throw(InexactError(:convert, ZZRingElem, x))
    z = ZZRingElem()
    @ccall libflint.fmpz_set_d(z::Ref{ZZRingElem}, x::Cdouble)::Nothing
    return z
  end

  ZZRingElem(x::ZZRingElem) = x
end
@flint_fields ZZRingElem
@flint_extra_fields ZZRingElem begin
  d = cstruct
  d! = v
end

mutable struct fmpz_factor
  cstruct::FlintC.fmpz_factor_struct

  function fmpz_factor()
    z = new()
    @ccall libflint.fmpz_factor_init(z::Ref{fmpz_factor})::Nothing
    finalizer(_fmpz_factor_clear_fn, z)
    return z
  end
end
@flint_fields fmpz_factor

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
  cstruct::FlintC.n_factor_t

  function n_factor()
    z = new()
    @ccall libflint.n_factor_init(z::Ref{n_factor})::Nothing
    # no finalizer needed
    return z
  end
end
@flint_fields n_factor

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
  cstruct::FlintC.fmpq

  function QQFieldElem()
    z = new()
    @ccall libflint.fmpq_init(z::Ref{QQFieldElem})::Nothing
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
      if b < 0 # FLINT requires positive denominator
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
@flint_fields QQFieldElem

################################################################################
#
#   Type unions
#
################################################################################

"""
    IntegerUnion = Union{Integer, ZZRingElem}

The `IntegerUnion` type union allows convenient and compact declaration
of methods that accept both Julia and Nemo integers.
"""
const IntegerUnion = Union{Integer, ZZRingElem}

"""
    RationalUnion = Union{Integer, ZZRingElem, Rational, QQFieldElem}

The `RationalUnion` type union allows convenient and compact declaration
of methods that accept both Julia and Nemo integers or rationals.
"""
const RationalUnion = Union{Integer, ZZRingElem, Rational, QQFieldElem}

"""
    flintify(x::RationalUnion)

Return either an `Int`, `ZZRingElem` or `QQFieldElem` equal to `x`.

This internal helper allow us to cleanly and compactly implement efficient
dispatch for arithmetics that involve native Nemo objects plus a Julia
integer.

Indeed, many internal arithmetics functions in FLINT have optimize variants
for the case when one operand is an `ZZRingElem` or an `Int` (sometimes also
`UInt` is supported). E.g. there are special methods for adding one of these
to a `ZZRingPolyElem`.

In order to handling adding an arbitrary Julia integer to a `ZZRingPolyElem`,
further dispatch is needed. The easiest is to provide a method

    +(a::ZZRingPolyElem, b::Integer) = a + ZZ(b)

However this is inefficient when `b` is e.g. an `UInt16`. So we could
do this (at least on a 64 bit machine):

    +(a::ZZRingPolyElem, b::Integer) = a + ZZ(b)
    +(a::ZZRingPolyElem, b::{Int64,Int32,Int16,Int8,UInt32,UInt16,UInt8}) = a + Int(b)

Doing this repeatedly is cumbersome and error prone. This can be avoided by
using `flintify`, which allows us to write

    +(a::ZZRingPolyElem, b::Integer) = a + flintify(b)

to get optimal dispatch.

This also works for Nemo types that also have special handlers for `UInt`,
as their method for `b::UInt` takes precedence over the fallback method.
"""
flintify(::RationalUnion)

flintify(x::ZZRingElem) = x
flintify(x::QQFieldElem) = x
flintify(x::Int) = x
flintify(x::Integer) = ZZRingElem(x)::ZZRingElem
flintify(x::Rational) = QQFieldElem(x)::QQFieldElem
@static if Int === Int64
  flintify(x::Union{Int64,Int32,Int16,Int8,UInt32,UInt16,UInt8}) = Int(x)
else
  flintify(x::Union{Int32,Int16,Int8,UInt16,UInt8}) = Int(x)
end


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
  cstruct::FlintC.fmpz_poly_struct

  parent::ZZPolyRing

  function ZZPolyRingElem()
    z = new()
    @ccall libflint.fmpz_poly_init(z::Ref{ZZPolyRingElem})::Nothing
    finalizer(_fmpz_poly_clear_fn, z)
    return z
  end

  function ZZPolyRingElem(a::Vector{<:IntegerUnion})
    z = new()
    @ccall libflint.fmpz_poly_init2(z::Ref{ZZPolyRingElem}, length(a)::Int)::Nothing
    for i = 1:length(a)
      setcoeff!(z, i - 1, a[i])
    end
    finalizer(_fmpz_poly_clear_fn, z)
    return z
  end

  ZZPolyRingElem(a::IntegerUnion) = set!(ZZPolyRingElem(), a)
  ZZPolyRingElem(a::ZZPolyRingElem) = set!(ZZPolyRingElem(), a)

  function ZZPolyRingElem(R::ZZPolyRing, args...)
    z = ZZPolyRingElem(args...)
    z.parent = R
    return z
  end
end
@flint_fields ZZPolyRingElem

mutable struct fmpz_poly_factor
  cstruct::FlintC.fmpz_poly_factor_struct

  function fmpz_poly_factor()
    z = new()
    @ccall libflint.fmpz_poly_factor_init(z::Ref{fmpz_poly_factor})::Nothing
    finalizer(_fmpz_poly_factor_clear_fn, z)
    return z
  end
end
@flint_fields fmpz_poly_factor p = Ptr{ZZPolyRingElem}
@flint_legacy_fields fmpz_poly_factor begin
  d = cstruct.c
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
  cstruct::FlintC.fmpq_poly_struct

  parent::QQPolyRing

  function QQPolyRingElem()
    z = new()
    @ccall libflint.fmpq_poly_init(z::Ref{QQPolyRingElem})::Nothing
    finalizer(_fmpq_poly_clear_fn, z)
    return z
  end

  function QQPolyRingElem(a::Vector{<:RationalUnion})
    z = new()
    @ccall libflint.fmpq_poly_init2(z::Ref{QQPolyRingElem}, length(a)::Int)::Nothing
    for i = 1:length(a)
      setcoeff!(z, i - 1, a[i])
    end
    finalizer(_fmpq_poly_clear_fn, z)
    return z
  end

  QQPolyRingElem(a::RationalUnion) = set!(QQPolyRingElem(), a)

  QQPolyRingElem(a::ZZPolyRingElem) = set!(QQPolyRingElem(), a)
  QQPolyRingElem(a::QQPolyRingElem) = set!(QQPolyRingElem(), a)

  function QQPolyRingElem(R::QQPolyRing, args...)
    z = QQPolyRingElem(args...)
    z.parent = R
    return z
  end
end
@flint_fields QQPolyRingElem

###############################################################################
#
#   zzModRing / zzModRingElem
#
###############################################################################

# FLINT's `nmod_t`, passed by value; the C part of `zzModRing` and `fpField`
const nmod_t = FlintC.nmod_t

@doc raw"""
    zzModRing <: Ring

The ring $\mathbb Z/n\mathbb Z$ for some $n$. See [`residue_ring`](@ref).
Implementation for the modulus being a machine integer [`Int`](@ref).
For the modulus being a [`ZZRingElem`](@ref) see [`ZZModRing`](@ref).
"""
@attributes mutable struct zzModRing <: ResidueRing{UInt}
  cstruct::FlintC.nmod_t

  function zzModRing(n::UInt, cached::Bool=true)
    return get_cached!(NmodRingID, n, cached) do
      z = new()
      @ccall libflint.nmod_init(z::Ref{zzModRing}, n::UInt)::Nothing
      return z
    end
  end
end
@flint_fields zzModRing

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
  cstruct::FlintC.nmod_t

  function fpField(n::UInt, cached::Bool=true)
    return get_cached!(GaloisFieldID, n, cached) do
      z = new()
      @ccall libflint.nmod_init(z::Ref{fpField}, n::UInt)::Nothing
      return z
    end
  end
end
@flint_fields fpField

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
  cstruct::FlintC.fmpz_mod_ctx_struct

  function fmpz_mod_ctx_struct()
    z = new()
    finalizer(_fmpz_mod_ctx_clear_fn, z)
    return z
  end
end
@flint_fields fmpz_mod_ctx_struct
@flint_legacy_fields fmpz_mod_ctx_struct begin
  n2 = cstruct.mod.n
  ninv = cstruct.mod.ninv
  norm = cstruct.mod.norm
end

@doc raw"""
    ZZModRing <: Ring

The ring $\mathbb Z/n\mathbb Z$ for some $n$. See [`residue_ring`](@ref).
Implementation for the modulus being a big integer [`ZZRingElem`](@ref).
For the modulus being an [`Int`](@ref) see [`zzModRing`](@ref).
"""
@attributes mutable struct ZZModRing <: ResidueRing{ZZRingElem}
  n::ZZRingElem
  ninv::fmpz_mod_ctx_struct

  function ZZModRing(n::ZZRingElem, cached::Bool=true)
    # Modulus of zero cannot be supported. E.g. FLINT library could not be expected to
    # do matrices over Z/0 using a Z/nZ type. The former is multiprecision, the latter not.
    n <= 0 && throw(DomainError(n, "Modulus must be positive"))

    return get_cached!(FmpzModRingID, n, cached) do
      ninv = fmpz_mod_ctx_struct()
      @ccall libflint.fmpz_mod_ctx_init(ninv::Ref{fmpz_mod_ctx_struct}, n::Ref{ZZRingElem})::Nothing
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
      @ccall libflint.fmpz_mod_ctx_init(ninv::Ref{fmpz_mod_ctx_struct}, n::Ref{ZZRingElem})::Nothing
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

  function zzModPolyRing(R::zzModRing, s::Symbol, cached::Bool = true)
    return get_cached!(NmodPolyRingID, (R, s), cached) do
      return new(R, s)
    end
  end
end

const NmodPolyRingID = CacheDictType{Tuple{zzModRing, Symbol}, zzModPolyRing}()

mutable struct zzModPolyRingElem <: PolyRingElem{zzModRingElem}
  cstruct::FlintC.nmod_poly_struct
  parent::zzModPolyRing

  function zzModPolyRingElem(n::UInt)
    z = new()
    @ccall libflint.nmod_poly_init(z::Ref{zzModPolyRingElem}, n::UInt)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModPolyRingElem(n::UInt, a::UInt)
    z = zzModPolyRingElem(n)
    setcoeff!(z, 0, a)
    return z
  end

  function zzModPolyRingElem(n::UInt, a::Integer)
    return zzModPolyRingElem(n, mod(a, n) % UInt)
  end

  function zzModPolyRingElem(n::UInt, arr::Vector{ZZRingElem})
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{zzModPolyRingElem}, n::UInt, length(arr)::Int)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    for i in 1:length(arr)
      setcoeff!(z, i - 1, arr[i])
    end
    return z
  end

  function zzModPolyRingElem(n::UInt, arr::Vector{UInt})
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{zzModPolyRingElem}, n::UInt, length(arr)::Int)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    for i in 1:length(arr)
      setcoeff!(z, i - 1, arr[i])
    end
    return z
  end

  function zzModPolyRingElem(n::UInt, arr::Vector{zzModRingElem})
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{zzModPolyRingElem}, n::UInt, length(arr)::Int)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    for i in 1:length(arr)
      setcoeff!(z, i-1, arr[i].data)
    end
    return z
  end

  function zzModPolyRingElem(n::UInt, f::ZZPolyRingElem)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{zzModPolyRingElem}, n::UInt, length(f)::Int)::Nothing
    @ccall libflint.fmpz_poly_get_nmod_poly(z::Ref{zzModPolyRingElem}, f::Ref{ZZPolyRingElem})::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModPolyRingElem(n::UInt, f::zzModPolyRingElem)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{zzModPolyRingElem}, n::UInt, length(f)::Int)::Nothing
    @ccall libflint.nmod_poly_set(z::Ref{zzModPolyRingElem}, f::Ref{zzModPolyRingElem})::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModPolyRingElem(R::zzModPolyRing, args...)
    z = zzModPolyRingElem(modulus(R), args...)
    z.parent = R
    return z
  end
end
@flint_fields zzModPolyRingElem
@flint_legacy_fields zzModPolyRingElem begin
  mod_n = cstruct.mod.n
  mod_ninv = cstruct.mod.ninv
  mod_norm = cstruct.mod.norm
end

mutable struct nmod_poly_factor
  cstruct::FlintC.nmod_poly_factor_struct
  n::UInt

  function nmod_poly_factor(n::UInt)
    z = new()
    @ccall libflint.nmod_poly_factor_init(z::Ref{nmod_poly_factor})::Nothing
    z.n = n
    finalizer(_nmod_poly_factor_clear_fn, z)
    return z
  end
end
@flint_fields nmod_poly_factor p = Ptr{zzModPolyRingElem}
@flint_legacy_fields nmod_poly_factor begin
  poly = Ptr{zzModPolyRingElem}(cstruct.p)
end

################################################################################
#
#   fpPolyRing / fpPolyRingElem
#
###############################################################################

@attributes mutable struct fpPolyRing <: PolyRing{fpFieldElem}
  base_ring::fpField
  S::Symbol

  function fpPolyRing(R::fpField, s::Symbol, cached::Bool = true)
    return get_cached!(GFPPolyRingID, (R, s), cached) do
      return new(R, s)
    end
  end
end

const GFPPolyRingID = CacheDictType{Tuple{fpField, Symbol}, fpPolyRing}()

mutable struct fpPolyRingElem <: PolyRingElem{fpFieldElem}
  cstruct::FlintC.nmod_poly_struct
  parent::fpPolyRing

  function fpPolyRingElem(n::UInt)
    z = new()
    @ccall libflint.nmod_poly_init(z::Ref{fpPolyRingElem}, n::UInt)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function fpPolyRingElem(n::UInt, a::UInt)
    z = fpPolyRingElem(n)
    setcoeff!(z, 0, a)
    return z
  end

  function fpPolyRingElem(n::UInt, a::Integer)
    return fpPolyRingElem(n, mod(a, n) % UInt)
  end

  function fpPolyRingElem(n::UInt, arr::Vector{ZZRingElem})
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{fpPolyRingElem}, n::UInt, length(arr)::Int)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    for i in 1:length(arr)
      setcoeff!(z, i - 1, arr[i])
    end
    return z
  end

  function fpPolyRingElem(n::UInt, arr::Vector{UInt})
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{fpPolyRingElem}, n::UInt, length(arr)::Int)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    for i in 1:length(arr)
      setcoeff!(z, i - 1, arr[i])
    end
    return z
  end

  function fpPolyRingElem(n::UInt, arr::Vector{fpFieldElem})
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{fpPolyRingElem}, n::UInt, length(arr)::Int)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    for i in 1:length(arr)
      setcoeff!(z, i-1, arr[i].data)
    end
    return z
  end

  function fpPolyRingElem(n::UInt, f::ZZPolyRingElem)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{fpPolyRingElem}, n::UInt, length(f)::Int)::Nothing
    @ccall libflint.fmpz_poly_get_nmod_poly(z::Ref{fpPolyRingElem}, f::Ref{ZZPolyRingElem})::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function fpPolyRingElem(n::UInt, f::fpPolyRingElem)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{fpPolyRingElem}, n::UInt, length(f)::Int)::Nothing
    @ccall libflint.nmod_poly_set(z::Ref{fpPolyRingElem}, f::Ref{fpPolyRingElem})::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function fpPolyRingElem(R::fpPolyRing, args...)
    z = fpPolyRingElem(modulus(R), args...)
    z.parent = R
    return z
  end
end
@flint_fields fpPolyRingElem
@flint_legacy_fields fpPolyRingElem begin
  mod_n = cstruct.mod.n
  mod_ninv = cstruct.mod.ninv
  mod_norm = cstruct.mod.norm
end

function _set_mod!(z::T, m::nmod_t) where {T <: Union{zzModPolyRingElem, fpPolyRingElem}}
  @ccall libflint.nmod_poly_set_mod(z::Ref{T}, m::nmod_t)::Nothing
  return z
end

_set_mod!(z::Union{zzModPolyRingElem, fpPolyRingElem}, R::Union{zzModRing, fpField}) = _set_mod!(z, nmod_t(R))

_set_mod!(z::Union{zzModPolyRingElem, fpPolyRingElem}, n::UInt) = _set_mod!(z, nmod_t(n))

# Moves `z` to the polynomial ring `P`, keeping the coefficients.
function _set_mod!(z::Union{zzModPolyRingElem, fpPolyRingElem}, P::Union{zzModPolyRing, fpPolyRing})
  _set_mod!(z, base_ring(P))
  z.parent = P
  return z
end

mutable struct gfp_poly_factor
  cstruct::FlintC.nmod_poly_factor_struct
  n::UInt

  function gfp_poly_factor(n::UInt)
    z = new()
    @ccall libflint.nmod_poly_factor_init(z::Ref{gfp_poly_factor})::Nothing
    z.n = n
    finalizer(_nmod_poly_factor_clear_fn, z)
    return z
  end
end
@flint_fields gfp_poly_factor p = Ptr{fpPolyRingElem}
@flint_legacy_fields gfp_poly_factor begin
  poly = Ptr{fpPolyRingElem}(cstruct.p)
end

###############################################################################
#
#   ZZModPolyRing / ZZModPolyRingElem
#
###############################################################################

@attributes mutable struct ZZModPolyRing <: PolyRing{ZZModRingElem}
  base_ring::ZZModRing
  S::Symbol

  function ZZModPolyRing(R::ZZModRing, s::Symbol, cached::Bool = true)
    return get_cached!(FmpzModPolyRingID, (R, s), cached) do
      return new(R, s)
    end
  end
end

const FmpzModPolyRingID = CacheDictType{Tuple{ZZModRing, Symbol}, ZZModPolyRing}()

mutable struct ZZModPolyRingElem <: PolyRingElem{ZZModRingElem}
  cstruct::FlintC.fmpz_mod_poly_struct

  parent::ZZModPolyRing

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct)
    z = new()
    @ccall libflint.fmpz_mod_poly_init(z::Ref{ZZModPolyRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, a::ZZRingElem)
    z = ZZModPolyRingElem(n)
    @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{ZZModPolyRingElem}, 0::Int, a::Ref{ZZRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    return z
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, a::UInt)
    z = ZZModPolyRingElem(n)
    @ccall libflint.fmpz_mod_poly_set_coeff_ui(z::Ref{ZZModPolyRingElem}, 0::Int, a::UInt, n::Ref{fmpz_mod_ctx_struct})::Nothing
    return z
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, arr::Vector{ZZRingElem})
    length(arr) == 0 && error("Array must have length > 0")
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{ZZModPolyRingElem}, length(arr)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    for i in 1:length(arr)
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{ZZModPolyRingElem}, (i - 1)::Int, arr[i]::Ref{ZZRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, arr::Vector{ZZModRingElem})
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{ZZModPolyRingElem}, length(arr)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    for i in 1:length(arr)
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{ZZModPolyRingElem}, (i - 1)::Int, arr[i].data::Ref{ZZRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, f::ZZPolyRingElem)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{ZZModPolyRingElem}, length(f)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    @ccall libflint.fmpz_mod_poly_set_fmpz_poly(z::Ref{ZZModPolyRingElem}, f::Ref{ZZPolyRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModPolyRingElem(n::fmpz_mod_ctx_struct, f::ZZModPolyRingElem)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{ZZModPolyRingElem}, length(f)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    @ccall libflint.fmpz_mod_poly_set(z::Ref{ZZModPolyRingElem}, f::Ref{ZZModPolyRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModPolyRingElem(R::ZZModRing, args...)
    z = ZZModPolyRingElem(R.ninv, args...)
  end

  function ZZModPolyRingElem(R::ZZModPolyRing, args...)
    z = ZZModPolyRingElem(base_ring(R), args...)
    z.parent = R
    return z
  end
end
@flint_fields ZZModPolyRingElem

mutable struct fmpz_mod_poly_factor
  cstruct::FlintC.fmpz_mod_poly_factor_struct

  n::fmpz_mod_ctx_struct

  function fmpz_mod_poly_factor(n::fmpz_mod_ctx_struct)
    z = new()
    @ccall libflint.fmpz_mod_poly_factor_init(z::Ref{fmpz_mod_poly_factor}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    z.n = n
    finalizer(_fmpz_mod_poly_factor_clear_fn, z)
    return z
  end

  function fmpz_mod_poly_factor(R::ZZModRing)
    return fmpz_mod_poly_factor(R.ninv)
  end
end
@flint_fields fmpz_mod_poly_factor poly = Ptr{ZZModPolyRingElem}

###############################################################################
#
#   FpPolyRing / FpPolyRingElem
#
###############################################################################

@attributes mutable struct FpPolyRing <: PolyRing{FpFieldElem}
  base_ring::FpField
  S::Symbol

  function FpPolyRing(R::FpField, s::Symbol, cached::Bool = true)
    return get_cached!(GFPFmpzPolyRingID, (R, s), cached) do
      return new(R, s)
    end
  end
end

const GFPFmpzPolyRingID = CacheDictType{Tuple{FpField, Symbol}, FpPolyRing}()

mutable struct FpPolyRingElem <: PolyRingElem{FpFieldElem}
  cstruct::FlintC.fmpz_mod_poly_struct

  parent::FpPolyRing

  function FpPolyRingElem(n::fmpz_mod_ctx_struct)
    z = new()
    @ccall libflint.fmpz_mod_poly_init(z::Ref{FpPolyRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, a::ZZRingElem)
    z = FpPolyRingElem(n)
    @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{FpPolyRingElem}, 0::Int, a::Ref{ZZRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    return z
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, a::UInt)
    z = FpPolyRingElem(n)
    @ccall libflint.fmpz_mod_poly_set_coeff_ui(z::Ref{FpPolyRingElem}, 0::Int, a::UInt, n::Ref{fmpz_mod_ctx_struct})::Nothing
    return z
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, arr::Vector{ZZRingElem})
    length(arr) == 0 && error("Array must have length > 0")
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{FpPolyRingElem}, length(arr)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    for i in 1:length(arr)
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{FpPolyRingElem}, (i - 1)::Int, arr[i]::Ref{ZZRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, arr::Vector{FpFieldElem})
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{FpPolyRingElem}, length(arr)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    for i in 1:length(arr)
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{FpPolyRingElem}, (i - 1)::Int, arr[i].data::Ref{ZZRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, f::ZZPolyRingElem)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{FpPolyRingElem}, length(f)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    @ccall libflint.fmpz_mod_poly_set_fmpz_poly(z::Ref{FpPolyRingElem}, f::Ref{ZZPolyRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(n::fmpz_mod_ctx_struct, f::FpPolyRingElem)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{FpPolyRingElem}, length(f)::Int, n::Ref{fmpz_mod_ctx_struct})::Nothing
    @ccall libflint.fmpz_mod_poly_set(z::Ref{FpPolyRingElem}, f::Ref{FpPolyRingElem}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpPolyRingElem(R::FpField, args...)
    z = FpPolyRingElem(R.ninv, args...)
  end

  function FpPolyRingElem(R::FpPolyRing, args...)
    z = FpPolyRingElem(base_ring(R), args...)
    z.parent = R
    return z
  end
end
@flint_fields FpPolyRingElem

mutable struct gfp_fmpz_poly_factor
  cstruct::FlintC.fmpz_mod_poly_factor_struct

  n::fmpz_mod_ctx_struct

  function gfp_fmpz_poly_factor(n::fmpz_mod_ctx_struct)
    z = new()
    @ccall libflint.fmpz_mod_poly_factor_init(z::Ref{gfp_fmpz_poly_factor}, n::Ref{fmpz_mod_ctx_struct})::Nothing
    z.n = n
    finalizer(_fmpz_mod_poly_factor_clear_fn, z)
    return z
  end

  function gfp_fmpz_poly_factor(R::FpField)
    return gfp_fmpz_poly_factor(R.ninv)
  end
end
@flint_fields gfp_fmpz_poly_factor poly = Ptr{FpPolyRingElem}

###############################################################################
#
#   ZZMPolyRing / ZZMPolyRingElem
#
###############################################################################

# values of FLINT's `ordering_t`
const _ORD_LEX       = FlintC.ORD_LEX
const _ORD_DEGLEX    = FlintC.ORD_DEGLEX
const _ORD_DEGREVLEX = FlintC.ORD_DEGREVLEX

# used by the tests
const flint_orderings = [:lex, :deglex, :degrevlex]

function _flint_ordering(S::Symbol)
  if S == :lex
    return _ORD_LEX
  elseif S == :deglex
    return _ORD_DEGLEX
  elseif S == :degrevlex
    return _ORD_DEGREVLEX
  else
    throw(ArgumentError("$S is not a valid ordering"))
  end
end

function _flint_ordering_symbol(b::FlintC.ordering_t)
  if b == _ORD_LEX
    return :lex
  elseif b == _ORD_DEGLEX
    return :deglex
  elseif b == _ORD_DEGREVLEX
    return :degrevlex
  else
    throw(ArgumentError("$b is not a valid FLINT ordering"))
  end
end

@attributes mutable struct ZZMPolyRing <: MPolyRing{ZZRingElem}
  cstruct::FlintC.fmpz_mpoly_ctx_struct

  S::Vector{Symbol}

  function ZZMPolyRing(s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(FmpzMPolyID, (s, S), cached) do
      ord = _flint_ordering(S)

      z = new()
      @ccall libflint.fmpz_mpoly_ctx_init(z::Ref{ZZMPolyRing}, length(s)::Int, ord::FlintC.ordering_t)::Nothing
      z.S = s
      finalizer(_fmpz_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end
@flint_fields ZZMPolyRing
@flint_legacy_fields ZZMPolyRing begin
  nvars = cstruct.minfo.nvars
  nfields = cstruct.minfo.nfields
  ord = Cint(cstruct.minfo.ord)
  deg = cstruct.minfo.deg
  rev = cstruct.minfo.rev
  lut = cstruct.minfo.lut_words_per_exp
  lut1 = cstruct.minfo.lut_fix_bits
end
ZZMPolyRing(::ZZRing, s::Vector{Symbol}, S::Symbol, cached::Bool=true) = ZZMPolyRing(s, S, cached)

const FmpzMPolyID = CacheDictType{Tuple{Vector{Symbol}, Symbol}, ZZMPolyRing}()

mutable struct ZZMPolyRingElem <: MPolyRingElem{ZZRingElem}
  cstruct::FlintC.fmpz_mpoly_struct

  parent::ZZMPolyRing

  function ZZMPolyRingElem(ctx::ZZMPolyRing)
    z = new()
    @ccall libflint.fmpz_mpoly_init(z::Ref{ZZMPolyRingElem}, ctx::Ref{ZZMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpz_mpoly_clear_fn, z)
    return z
  end

  function ZZMPolyRingElem(ctx::ZZMPolyRing, a::Vector{ZZRingElem}, b::Vector{Vector{UInt}})
    z = new()
    @ccall libflint.fmpz_mpoly_init2(z::Ref{ZZMPolyRingElem}, length(a)::Int, ctx::Ref{ZZMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpz_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.fmpz_mpoly_push_term_fmpz_ui(z::Ref{ZZMPolyRingElem}, a[i]::Ref{ZZRingElem}, b[i]::Ptr{UInt}, ctx::Ref{ZZMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function ZZMPolyRingElem(ctx::ZZMPolyRing, a::Vector{ZZRingElem}, b::Vector{Vector{Int}})
    @req all(x -> all(!is_negative, x), b) "Negative exponents are not allowed"
    z = new()
    @ccall libflint.fmpz_mpoly_init2(z::Ref{ZZMPolyRingElem}, length(a)::Int, ctx::Ref{ZZMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpz_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.fmpz_mpoly_push_term_fmpz_ui(z::Ref{ZZMPolyRingElem}, a[i]::Ref{ZZRingElem}, b[i]::Ptr{Int}, ctx::Ref{ZZMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function ZZMPolyRingElem(ctx::ZZMPolyRing, a::Vector{ZZRingElem}, b::Vector{Vector{ZZRingElem}})
    @req all(x -> all(!is_negative, x), b) "Negative exponents are not allowed"
    z = new()
    @ccall libflint.fmpz_mpoly_init2(z::Ref{ZZMPolyRingElem}, length(a)::Int, ctx::Ref{ZZMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpz_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.fmpz_mpoly_push_term_fmpz_fmpz(z::Ref{ZZMPolyRingElem}, a[i]::Ref{ZZRingElem}, b[i]::Ptr{Ref{ZZRingElem}}, ctx::Ref{ZZMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  ZZMPolyRingElem(ctx::ZZMPolyRing, a::ZZRingElem) = set!(ZZMPolyRingElem(ctx), a)
  ZZMPolyRingElem(ctx::ZZMPolyRing, a::Integer) = set!(ZZMPolyRingElem(ctx), a)
end
@flint_fields ZZMPolyRingElem

mutable struct fmpz_mpoly_factor
  cstruct::FlintC.fmpz_mpoly_factor_struct

  parent::ZZMPolyRing

  function fmpz_mpoly_factor(ctx::ZZMPolyRing)
    z = new()
    @ccall libflint.fmpz_mpoly_factor_init(z::Ref{fmpz_mpoly_factor}, ctx::Ref{ZZMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpz_mpoly_factor_clear_fn, z)
    return z
  end
end
@flint_fields fmpz_mpoly_factor

###############################################################################
#
#   QQMPolyRing / QQMPolyRingElem
#
###############################################################################

@attributes mutable struct QQMPolyRing <: MPolyRing{QQFieldElem}
  cstruct::FlintC.fmpq_mpoly_ctx_struct

  S::Vector{Symbol}

  function QQMPolyRing(s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(FmpqMPolyID, (s, S), cached) do
      ord = _flint_ordering(S)

      z = new()
      @ccall libflint.fmpq_mpoly_ctx_init(z::Ref{QQMPolyRing}, length(s)::Int, ord::FlintC.ordering_t)::Nothing
      z.S = s
      finalizer(_fmpq_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end
@flint_fields QQMPolyRing
@flint_legacy_fields QQMPolyRing begin
  nvars = cstruct.zctx.minfo.nvars
  nfields = cstruct.zctx.minfo.nfields
  ord = Cint(cstruct.zctx.minfo.ord)
  deg = cstruct.zctx.minfo.deg
  rev = cstruct.zctx.minfo.rev
  lut = cstruct.zctx.minfo.lut_words_per_exp
  lut1 = cstruct.zctx.minfo.lut_fix_bits
end
QQMPolyRing(::QQField, s::Vector{Symbol}, S::Symbol, cached::Bool=true) = QQMPolyRing(s, S, cached)

const FmpqMPolyID = CacheDictType{Tuple{Vector{Symbol}, Symbol}, QQMPolyRing}()

mutable struct QQMPolyRingElem <: MPolyRingElem{QQFieldElem}
  cstruct::FlintC.fmpq_mpoly_struct

  parent::QQMPolyRing

  function QQMPolyRingElem(ctx::QQMPolyRing)
    z = new()
    @ccall libflint.fmpq_mpoly_init(z::Ref{QQMPolyRingElem}, ctx::Ref{QQMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)
    return z
  end

  function QQMPolyRingElem(ctx::QQMPolyRing, a::Vector{QQFieldElem}, b::Vector{Vector{UInt}})
    z = new()
    @ccall libflint.fmpq_mpoly_init2(z::Ref{QQMPolyRingElem}, length(a)::Int, ctx::Ref{QQMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.fmpq_mpoly_push_term_fmpq_ui(z::Ref{QQMPolyRingElem}, a[i]::Ref{QQFieldElem}, b[i]::Ptr{UInt}, ctx::Ref{QQMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function QQMPolyRingElem(ctx::QQMPolyRing, a::Vector{QQFieldElem}, b::Vector{Vector{Int}})
    @req all(x -> all(!is_negative, x), b) "Negative exponents are not allowed"
    z = new()
    @ccall libflint.fmpq_mpoly_init2(z::Ref{QQMPolyRingElem}, length(a)::Int, ctx::Ref{QQMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.fmpq_mpoly_push_term_fmpq_ui(z::Ref{QQMPolyRingElem}, a[i]::Ref{QQFieldElem}, b[i]::Ptr{Int}, ctx::Ref{QQMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function QQMPolyRingElem(ctx::QQMPolyRing, a::Vector{QQFieldElem}, b::Vector{Vector{ZZRingElem}})
    @req all(x -> all(!is_negative, x), b) "Negative exponents are not allowed"
    z = new()
    @ccall libflint.fmpq_mpoly_init2(z::Ref{QQMPolyRingElem}, length(a)::Int, ctx::Ref{QQMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpq_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.fmpq_mpoly_push_term_fmpq_fmpz(z::Ref{QQMPolyRingElem}, a[i]::Ref{QQFieldElem}, b[i]::Ptr{Ref{ZZRingElem}}, ctx::Ref{QQMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  QQMPolyRingElem(ctx::QQMPolyRing, a::ZZRingElem) = set!(QQMPolyRingElem(ctx), a)
  QQMPolyRingElem(ctx::QQMPolyRing, a::QQFieldElem) = set!(QQMPolyRingElem(ctx), a)
  QQMPolyRingElem(ctx::QQMPolyRing, a::Integer) = set!(QQMPolyRingElem(ctx), a)
  QQMPolyRingElem(ctx::QQMPolyRing, a::Rational) = set!(QQMPolyRingElem(ctx), a)

end
@flint_fields QQMPolyRingElem
@flint_legacy_fields QQMPolyRingElem begin
  content_num = cstruct.content.num
  content_den = cstruct.content.den
  coeffs = cstruct.zpoly.coeffs
  exps = cstruct.zpoly.exps
  alloc = cstruct.zpoly.alloc
  length = cstruct.zpoly.length
  bits = cstruct.zpoly.bits
end

mutable struct fmpq_mpoly_factor
  cstruct::FlintC.fmpq_mpoly_factor_struct

  parent::QQMPolyRing

  function fmpq_mpoly_factor(ctx::QQMPolyRing)
    z = new()
    @ccall libflint.fmpq_mpoly_factor_init(z::Ref{fmpq_mpoly_factor}, ctx::Ref{QQMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpq_mpoly_factor_clear_fn, z)
    return z
  end
end
@flint_fields fmpq_mpoly_factor
@flint_legacy_fields fmpq_mpoly_factor begin
  constant_num = cstruct.constant.num
  constant_den = cstruct.constant.den
end

###############################################################################
#
#   zzModMPolyRing / zzModMPolyRingElem
#
###############################################################################

@attributes mutable struct zzModMPolyRing <: MPolyRing{zzModRingElem}
  cstruct::FlintC.nmod_mpoly_ctx_struct

  base_ring::zzModRing
  S::Vector{Symbol}

  function zzModMPolyRing(R::zzModRing, s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(NmodMPolyID, (R, s, S), cached) do
      ord = _flint_ordering(S)

      z = new()
      @ccall libflint.nmod_mpoly_ctx_init(z::Ref{zzModMPolyRing}, length(s)::Int, ord::FlintC.ordering_t, R.n::UInt)::Nothing
      z.base_ring = R
      z.S = s
      finalizer(_nmod_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end
@flint_fields zzModMPolyRing
@flint_legacy_fields zzModMPolyRing begin
  nvars = cstruct.minfo.nvars
  nfields = cstruct.minfo.nfields
  ord = Cint(cstruct.minfo.ord)
  deg = cstruct.minfo.deg
  rev = cstruct.minfo.rev
  lut = cstruct.minfo.lut_words_per_exp
  lut1 = cstruct.minfo.lut_fix_bits
  n = cstruct.mod.n
  ninv = cstruct.mod.ninv
  norm = cstruct.mod.norm
end

const NmodMPolyID = CacheDictType{Tuple{zzModRing, Vector{Symbol}, Symbol}, zzModMPolyRing}()

mutable struct zzModMPolyRingElem <: MPolyRingElem{zzModRingElem}
  cstruct::FlintC.nmod_mpoly_struct

  parent::zzModMPolyRing

  function zzModMPolyRingElem(ctx::zzModMPolyRing)
    z = new()
    @ccall libflint.nmod_mpoly_init(z::Ref{zzModMPolyRingElem}, ctx::Ref{zzModMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)
    return z
  end

  function zzModMPolyRingElem(ctx::zzModMPolyRing, a::Vector{zzModRingElem}, b::Vector{Vector{UInt}})
    z = new()
    @ccall libflint.nmod_mpoly_init2(z::Ref{zzModMPolyRingElem}, length(a)::Int, ctx::Ref{zzModMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.nmod_mpoly_push_term_ui_ui(z::Ref{zzModMPolyRingElem}, a[i].data::UInt, b[i]::Ptr{UInt}, ctx::Ref{zzModMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function zzModMPolyRingElem(ctx::zzModMPolyRing, a::Vector{zzModRingElem}, b::Vector{Vector{Int}})
    @req all(x -> all(!is_negative, x), b) "Negative exponents are not allowed"
    z = new()
    @ccall libflint.nmod_mpoly_init2(z::Ref{zzModMPolyRingElem}, length(a)::Int, ctx::Ref{zzModMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.nmod_mpoly_push_term_ui_ui(z::Ref{zzModMPolyRingElem}, a[i].data::UInt, b[i]::Ptr{Int}, ctx::Ref{zzModMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function zzModMPolyRingElem(ctx::zzModMPolyRing, a::Vector{zzModRingElem}, b::Vector{Vector{ZZRingElem}})
    @req all(x -> all(!is_negative, x), b) "Negative exponents are not allowed"
    z = new()
    @ccall libflint.nmod_mpoly_init2(z::Ref{zzModMPolyRingElem}, length(a)::Int, ctx::Ref{zzModMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.nmod_mpoly_push_term_ui_fmpz(z::Ref{zzModMPolyRingElem}, a[i].data::UInt, b[i]::Ptr{Ref{ZZRingElem}}, ctx::Ref{zzModMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function zzModMPolyRingElem(ctx::zzModMPolyRing, a::UInt)
    z = zzModMPolyRingElem(ctx)
    @ccall libflint.nmod_mpoly_set_ui(z::Ref{zzModMPolyRingElem}, a::UInt, ctx::Ref{zzModMPolyRing})::Nothing
    return z
  end

  function zzModMPolyRingElem(ctx::zzModMPolyRing, a::zzModRingElem)
    z = zzModMPolyRingElem(ctx)
    @ccall libflint.nmod_mpoly_set_ui(z::Ref{zzModMPolyRingElem}, a.data::UInt, ctx::Ref{zzModMPolyRing})::Nothing
    return z
  end
end
@flint_fields zzModMPolyRingElem

mutable struct nmod_mpoly_factor
  cstruct::FlintC.nmod_mpoly_factor_struct

  parent::zzModMPolyRing

  function nmod_mpoly_factor(ctx::zzModMPolyRing)
    z = new()
    @ccall libflint.nmod_mpoly_factor_init(z::Ref{nmod_mpoly_factor}, ctx::Ref{zzModMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_nmod_mpoly_factor_clear_fn, z)
    return z
  end
end
@flint_fields nmod_mpoly_factor

################################################################################
#
#   fpMPolyRing / fpMPolyRingElem
#
###############################################################################

@attributes mutable struct fpMPolyRing <: MPolyRing{fpFieldElem}
  cstruct::FlintC.nmod_mpoly_ctx_struct

  base_ring::fpField
  S::Vector{Symbol}

  function fpMPolyRing(R::fpField, s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(GFPMPolyID, (R, s, S), cached) do
      ord = _flint_ordering(S)

      z = new()
      @ccall libflint.nmod_mpoly_ctx_init(z::Ref{fpMPolyRing}, length(s)::Int, ord::FlintC.ordering_t, UInt(modulus(R))::UInt)::Nothing
      z.base_ring = R
      z.S = s
      finalizer(_nmod_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end
@flint_fields fpMPolyRing
@flint_legacy_fields fpMPolyRing begin
  nvars = cstruct.minfo.nvars
  nfields = cstruct.minfo.nfields
  ord = Cint(cstruct.minfo.ord)
  deg = cstruct.minfo.deg
  rev = cstruct.minfo.rev
  lut = cstruct.minfo.lut_words_per_exp
  lut1 = cstruct.minfo.lut_fix_bits
  n = cstruct.mod.n
  ninv = cstruct.mod.ninv
  norm = cstruct.mod.norm
end

const GFPMPolyID = CacheDictType{Tuple{fpField, Vector{Symbol}, Symbol}, fpMPolyRing}()

mutable struct fpMPolyRingElem <: MPolyRingElem{fpFieldElem}
  cstruct::FlintC.nmod_mpoly_struct

  parent::fpMPolyRing

  function fpMPolyRingElem(ctx::fpMPolyRing)
    z = new()
    @ccall libflint.nmod_mpoly_init(z::Ref{fpMPolyRingElem}, ctx::Ref{fpMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)
    return z
  end

  function fpMPolyRingElem(ctx::fpMPolyRing, a::Vector{fpFieldElem}, b::Vector{Vector{UInt}})
    z = new()
    @ccall libflint.nmod_mpoly_init2(z::Ref{fpMPolyRingElem}, length(a)::Int, ctx::Ref{fpMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.nmod_mpoly_push_term_ui_ui(z::Ref{fpMPolyRingElem}, a[i].data::UInt, b[i]::Ptr{UInt}, ctx::Ref{fpMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function fpMPolyRingElem(ctx::fpMPolyRing, a::Vector{fpFieldElem}, b::Vector{Vector{Int}})
    @req all(x -> all(!is_negative, x), b) "Negative exponents are not allowed"
    z = new()
    @ccall libflint.nmod_mpoly_init2(z::Ref{fpMPolyRingElem}, length(a)::Int, ctx::Ref{fpMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.nmod_mpoly_push_term_ui_ui(z::Ref{fpMPolyRingElem}, a[i].data::UInt, b[i]::Ptr{Int}, ctx::Ref{fpMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function fpMPolyRingElem(ctx::fpMPolyRing, a::Vector{fpFieldElem}, b::Vector{Vector{ZZRingElem}})
    @req all(x -> all(!is_negative, x), b) "Negative exponents are not allowed"
    z = new()
    @ccall libflint.nmod_mpoly_init2(z::Ref{fpMPolyRingElem}, length(a)::Int, ctx::Ref{fpMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.nmod_mpoly_push_term_ui_fmpz(z::Ref{fpMPolyRingElem}, a[i].data::UInt, b[i]::Ptr{Ref{ZZRingElem}}, ctx::Ref{fpMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function fpMPolyRingElem(ctx::fpMPolyRing, a::UInt)
    z = fpMPolyRingElem(ctx)
    @ccall libflint.nmod_mpoly_set_ui(z::Ref{fpMPolyRingElem}, a::UInt, ctx::Ref{fpMPolyRing})::Nothing
    return z
  end

  function fpMPolyRingElem(ctx::fpMPolyRing, a::fpFieldElem)
    z = fpMPolyRingElem(ctx)
    @ccall libflint.nmod_mpoly_set_ui(z::Ref{fpMPolyRingElem}, a.data::UInt, ctx::Ref{fpMPolyRing})::Nothing
    return z
  end
end
@flint_fields fpMPolyRingElem

mutable struct gfp_mpoly_factor
  cstruct::FlintC.nmod_mpoly_factor_struct

  parent::fpMPolyRing

  function gfp_mpoly_factor(ctx::fpMPolyRing)
    z = new()
    @ccall libflint.nmod_mpoly_factor_init(z::Ref{gfp_mpoly_factor}, ctx::Ref{fpMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_nmod_mpoly_factor_clear_fn, z)
    return z
  end
end
@flint_fields gfp_mpoly_factor

################################################################################
#
#   FpMPolyRing / FpMPolyRingElem
#
###############################################################################

@attributes mutable struct FpMPolyRing <: MPolyRing{FpFieldElem}
  cstruct::FlintC.fmpz_mod_mpoly_ctx_struct

  base_ring::FpField
  S::Vector{Symbol}

  function FpMPolyRing(R::FpField, s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(GFPFmpzMPolyID, (R, s, S), cached) do
      ord = _flint_ordering(S)

      z = new()
      @ccall libflint.fmpz_mod_mpoly_ctx_init(z::Ref{FpMPolyRing}, length(s)::Int, ord::FlintC.ordering_t, modulus(R)::Ref{ZZRingElem})::Nothing
      z.base_ring = R
      z.S = s
      finalizer(_fmpz_mod_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end
@flint_fields FpMPolyRing
@flint_legacy_fields FpMPolyRing begin
  nvars = cstruct.minfo.nvars
  nfields = cstruct.minfo.nfields
  ord = Cint(cstruct.minfo.ord)
  deg = cstruct.minfo.deg
  rev = cstruct.minfo.rev
  lut = cstruct.minfo.lut_words_per_exp
  lut1 = cstruct.minfo.lut_fix_bits
  n = cstruct.ffinfo.n
  add_fxn = cstruct.ffinfo.add_fxn
  sub_fxn = cstruct.ffinfo.sub_fxn
  mul_fxn = cstruct.ffinfo.mul_fxn
  n2 = cstruct.ffinfo.mod.n
  ninv = cstruct.ffinfo.mod.ninv
  norm = cstruct.ffinfo.mod.norm
  n_limbs = cstruct.ffinfo.n_limbs
  ninv_limbs = cstruct.ffinfo.ninv_limbs
  ninv_huge = cstruct.ffinfo.ninv_huge
end

const GFPFmpzMPolyID = CacheDictType{Tuple{FpField, Vector{Symbol}, Symbol}, FpMPolyRing}()

mutable struct FpMPolyRingElem <: MPolyRingElem{FpFieldElem}
  cstruct::FlintC.fmpz_mod_mpoly_struct

  parent::FpMPolyRing

  function FpMPolyRingElem(ctx::FpMPolyRing)
    z = new()
    @ccall libflint.fmpz_mod_mpoly_init(z::Ref{FpMPolyRingElem}, ctx::Ref{FpMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpz_mod_mpoly_clear_fn, z)
    return z
  end

  function FpMPolyRingElem(ctx::FpMPolyRing, a::Vector{FpFieldElem}, b::Vector{Vector{T}}) where T <: Union{UInt, Int, ZZRingElem}
    @req all(x -> all(!is_negative, x), b) "Negative exponents are not allowed"
    z = new()
    @ccall libflint.fmpz_mod_mpoly_init2(z::Ref{FpMPolyRingElem}, length(a)::Int, ctx::Ref{FpMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpz_mod_mpoly_clear_fn, z)

    for i in 1:length(a)
      if T == ZZRingElem
        @ccall libflint.fmpz_mod_mpoly_push_term_fmpz_fmpz(z::Ref{FpMPolyRingElem}, a[i].data::Ref{ZZRingElem}, b[i]::Ptr{Ref{ZZRingElem}}, ctx::Ref{FpMPolyRing})::Nothing
      else
        @ccall libflint.fmpz_mod_mpoly_push_term_fmpz_ui(z::Ref{FpMPolyRingElem}, a[i].data::Ref{ZZRingElem}, b[i]::Ptr{UInt}, ctx::Ref{FpMPolyRing})::Nothing
      end
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function FpMPolyRingElem(ctx::FpMPolyRing, a::Union{ZZRingElem, FpFieldElem})
    z = FpMPolyRingElem(ctx)
    @ccall libflint.fmpz_mod_mpoly_set_fmpz(z::Ref{FpMPolyRingElem}, (a isa ZZRingElem ? a : data(a))::Ref{ZZRingElem}, ctx::Ref{FpMPolyRing})::Nothing
    return z
  end
end
@flint_fields FpMPolyRingElem

mutable struct gfp_fmpz_mpoly_factor
  cstruct::FlintC.fmpz_mod_mpoly_factor_struct

  parent::FpMPolyRing

  function gfp_fmpz_mpoly_factor(ctx::FpMPolyRing)
    z = new()
    @ccall libflint.fmpz_mod_mpoly_factor_init(z::Ref{gfp_fmpz_mpoly_factor}, ctx::Ref{FpMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fmpz_mod_mpoly_factor_clear_fn, z)
    return z
  end
end
@flint_fields gfp_fmpz_mpoly_factor

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
  cstruct::FlintC.fq_nmod_ctx_struct

  overfields :: Dict{Int, Vector{FinFieldMorphism{fqPolyRepField, fqPolyRepField}}}
  subfields :: Dict{Int, Vector{FinFieldMorphism{fqPolyRepField, fqPolyRepField}}}

  function fqPolyRepField(c::UInt, deg::Int, s::Symbol, cached::Bool = true; check::Bool = true)
    check && !is_prime(c) &&
    throw(DomainError(c, "the characteristic must be a prime"))
    return get_cached!(FqNmodFiniteFieldID, (c, deg, s), cached) do
      d = new()
      @ccall libflint.fq_nmod_ctx_init_ui(d::Ref{fqPolyRepField}, c::UInt, deg::Int, string(s)::Ptr{UInt8})::Nothing
      finalizer(_fq_nmod_ctx_clear_fn, d)
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
      @ccall libflint.fq_nmod_ctx_init_modulus(z::Ref{fqPolyRepField}, f::Ref{zzModPolyRingElem}, string(s)::Ptr{UInt8})::Nothing
      finalizer(_fq_nmod_ctx_clear_fn, z)
      return z
    end
  end

  function fqPolyRepField(f::fpPolyRingElem, s::Symbol, cached::Bool = true; check::Bool=true)
    # check ignored
    return get_cached!(FqNmodFiniteFieldIDGFPPol, (parent(f), f, s), cached) do
      z = new()
      @ccall libflint.fq_nmod_ctx_init_modulus(z::Ref{fqPolyRepField}, f::Ref{fpPolyRingElem}, string(s)::Ptr{UInt8})::Nothing
      finalizer(_fq_nmod_ctx_clear_fn, z)
      return z
    end
  end

end
@flint_fields fqPolyRepField
@flint_legacy_fields fqPolyRepField begin
  n = cstruct.mod.n
  ninv = cstruct.mod.ninv
  norm = cstruct.mod.norm
  mod_coeffs = cstruct.modulus.coeffs
  mod_alloc = cstruct.modulus.alloc
  mod_length = cstruct.modulus.length
  mod_n = cstruct.modulus.mod.n
  mod_ninv = cstruct.modulus.mod.ninv
  mod_norm = cstruct.modulus.mod.norm
  inv_coeffs = cstruct.inv.coeffs
  inv_alloc = cstruct.inv.alloc
  inv_length = cstruct.inv.length
  inv_n = cstruct.inv.mod.n
  inv_ninv = cstruct.inv.mod.ninv
  inv_norm = cstruct.inv.mod.norm
end

const FqNmodFiniteFieldID = CacheDictType{Tuple{UInt, Int, Symbol}, fqPolyRepField}()

const FqNmodFiniteFieldIDNmodPol = CacheDictType{Tuple{zzModPolyRing, zzModPolyRingElem, Symbol},
                                                 fqPolyRepField}()

const FqNmodFiniteFieldIDGFPPol = CacheDictType{Tuple{fpPolyRing, fpPolyRingElem, Symbol},
                                                fqPolyRepField}()


@doc raw"""
    fqPolyRepFieldElem <: FinFieldElem

An element $\sum_{i=0}^{d-1} a_i t^i$ of a finite field $\mathbb F_{p^d} \cong \mathbb F_p[t]/f$.
Represented internally as $(a_i)_{0\le i<d}$. See [`fqPolyRepField`](@ref).
"""
mutable struct fqPolyRepFieldElem <: FinFieldElem
  cstruct::FlintC.fq_nmod_struct
  parent::fqPolyRepField

  function fqPolyRepFieldElem(ctx::fqPolyRepField)
    d = new()
    @ccall libflint.fq_nmod_init2(d::Ref{fqPolyRepFieldElem}, ctx::Ref{fqPolyRepField})::Nothing
    d.parent = ctx
    finalizer(_fq_nmod_clear_fn, d)
    return d
  end

  function fqPolyRepFieldElem(ctx::fqPolyRepField, x::Int)
    d = fqPolyRepFieldElem(ctx)
    set!(d, x)
    return d
  end

  function fqPolyRepFieldElem(ctx::fqPolyRepField, x::ZZRingElem)
    d = fqPolyRepFieldElem(ctx)
    set!(d, x)
    return d
  end

  function fqPolyRepFieldElem(ctx::fqPolyRepField, x::fqPolyRepFieldElem)
    d = fqPolyRepFieldElem(ctx)
    set!(d, x)
    return d
  end

  function fqPolyRepFieldElem(ctx::fqPolyRepField, x::fpPolyRingElem)
    d = fqPolyRepFieldElem(ctx)
    set!(d, x)
    return d
  end
end
@flint_fields fqPolyRepFieldElem
@flint_legacy_fields fqPolyRepFieldElem begin
  n = cstruct.mod.n
  ninv = cstruct.mod.ninv
  norm = cstruct.mod.norm
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
  cstruct::FlintC.gr_ctx_struct

  var::String

  overfields::Dict{Int, Vector{FinFieldMorphism{FqField, FqField}}}
  subfields::Dict{Int, Vector{FinFieldMorphism{FqField, FqField}}}

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
      finalizer(_fq_default_ctx_clear_fn, d)
      @ccall libflint.fq_default_ctx_init(d::Ref{FqField}, char::Ref{ZZRingElem}, deg::Int, d.var::Ptr{UInt8})::Nothing
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
      @ccall libflint.fq_default_ctx_init_modulus(z::Ref{FqField}, f::Ref{ZZModPolyRingElem}, base_ring(parent(f)).ninv::Ref{fmpz_mod_ctx_struct}, string(s)::Ptr{UInt8})::Nothing
      finalizer(_fq_default_ctx_clear_fn, z)
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
      @ccall libflint.fq_default_ctx_init_modulus(z::Ref{FqField}, f::Ref{FpPolyRingElem}, base_ring(parent(f)).ninv::Ref{fmpz_mod_ctx_struct}, string(s)::Ptr{UInt8})::Nothing
      finalizer(_fq_default_ctx_clear_fn, z)
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
      @ccall libflint.fq_default_ctx_init_modulus_nmod(z::Ref{FqField}, f::Ref{zzModPolyRingElem}, string(s)::Ptr{UInt8})::Nothing
      finalizer(_fq_default_ctx_clear_fn, z)
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
      @ccall libflint.fq_default_ctx_init_modulus_nmod(z::Ref{FqField}, f::Ref{fpPolyRingElem}, string(s)::Ptr{UInt8})::Nothing
      finalizer(_fq_default_ctx_clear_fn, z)
      return z
    end
  end
end
@flint_fields FqField

const FqDefaultFiniteFieldID = CacheDictType{Tuple{ZZRingElem, Int, Symbol}, FqField}()

const FqDefaultFiniteFieldIDFmpzPol = CacheDictType{Tuple{ZZModPolyRingElem, Symbol}, FqField}()

const FqDefaultFiniteFieldIDGFPPol = CacheDictType{Tuple{FpPolyRingElem, Symbol}, FqField}()

const FqDefaultFiniteFieldIDNmodPol = CacheDictType{Tuple{zzModPolyRingElem, Symbol}, FqField}()

const FqDefaultFiniteFieldIDGFPNmodPol = CacheDictType{Tuple{fpPolyRingElem, Symbol}, FqField}()

@doc raw"""
    FqFieldElem <: FinFieldElem

An element of a finite field. See [`FqField`](@ref).
"""
mutable struct FqFieldElem <: FinFieldElem
  cstruct::FlintC.fq_default_struct
  parent::FqField
  poly#::Union{Nothing, FqPolyRingElem}

  function FqFieldElem(ctx::FqField)
    d = new()
    @ccall libflint.fq_default_init2(d::Ref{FqFieldElem}, ctx::Ref{FqField})::Nothing
    if _fq_default_ctx_type(ctx) != _FQ_DEFAULT_NMOD
      finalizer(_fq_default_clear_fn, d)
    end
    d.poly = nothing
    d.parent = ctx
    return d
  end

  function FqFieldElem(
    ctx::FqField,
    x::Union{
      FqFieldElem,
      Integer, ZZRingElem,
      ZZPolyRingElem,
      zzModPolyRingElem, fpPolyRingElem,
      ZZModPolyRingElem, FpPolyRingElem,
    },
  )
    d = FqFieldElem(ctx)
    set!(d, x)
    return d
  end
end
@flint_fields FqFieldElem

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
  cstruct::FlintC.fq_ctx_struct

  overfields :: Dict{Int, Vector{FinFieldMorphism{FqPolyRepField, FqPolyRepField}}}
  subfields :: Dict{Int, Vector{FinFieldMorphism{FqPolyRepField, FqPolyRepField}}}

  function FqPolyRepField(char::ZZRingElem, deg::Int, s::Symbol, cached::Bool = true; check::Bool = true)
    check && !is_probable_prime(char) &&
    throw(DomainError(char, "the characteristic must be a prime"))
    return get_cached!(FqFiniteFieldID, (char, deg, s), cached) do
      d = new()
      finalizer(_fq_ctx_clear_fn, d)
      @ccall libflint.fq_ctx_init(d::Ref{FqPolyRepField}, char::Ref{ZZRingElem}, deg::Int, string(s)::Ptr{UInt8})::Nothing
      return d
    end
  end

  function FqPolyRepField(f::ZZModPolyRingElem, s::Symbol, cached::Bool = true; check::Bool = true)
    check && !is_probable_prime(modulus(f)) &&
    throw(DomainError(base_ring(f), "the base ring of the polynomial must be a field"))
    return get_cached!(FqFiniteFieldIDFmpzPol, (f, s), cached) do
      z = new()
      @ccall libflint.fq_ctx_init_modulus(z::Ref{FqPolyRepField}, f::Ref{ZZModPolyRingElem}, base_ring(parent(f)).ninv::Ref{fmpz_mod_ctx_struct}, string(s)::Ptr{UInt8})::Nothing
      finalizer(_fq_ctx_clear_fn, z)
      return z
    end
  end

  function FqPolyRepField(f::FpPolyRingElem, s::Symbol, cached::Bool = true; check::Bool = true)
    # check ignored
    return get_cached!(FqFiniteFieldIDGFPPol, (f, s), cached) do
      z = new()
      @ccall libflint.fq_ctx_init_modulus(z::Ref{FqPolyRepField}, f::Ref{FpPolyRingElem}, base_ring(parent(f)).ninv::Ref{fmpz_mod_ctx_struct}, string(s)::Ptr{UInt8})::Nothing
      finalizer(_fq_ctx_clear_fn, z)
      return z
    end
  end
end
@flint_fields FqPolyRepField
@flint_legacy_fields FqPolyRepField begin
  p = cstruct.ctxp.n
  add_fxn = cstruct.ctxp.add_fxn
  sub_fxn = cstruct.ctxp.sub_fxn
  mul_fxn = cstruct.ctxp.mul_fxn
  n2 = cstruct.ctxp.mod.n
  ninv = cstruct.ctxp.mod.ninv
  norm = cstruct.ctxp.mod.norm
  n_limbs = cstruct.ctxp.n_limbs
  ninv_limbs = cstruct.ctxp.ninv_limbs
  ninv_huge = cstruct.ctxp.ninv_huge
  mod_coeffs = cstruct.modulus.coeffs
  mod_alloc = cstruct.modulus.alloc
  mod_length = cstruct.modulus.length
  inv_coeffs = cstruct.inv.coeffs
  inv_alloc = cstruct.inv.alloc
  inv_length = cstruct.inv.length
end


const FqFiniteFieldID = CacheDictType{Tuple{ZZRingElem, Int, Symbol}, FqPolyRepField}()

const FqFiniteFieldIDFmpzPol = CacheDictType{Tuple{ZZModPolyRingElem, Symbol}, FqPolyRepField}()

const FqFiniteFieldIDGFPPol = CacheDictType{Tuple{FpPolyRingElem, Symbol}, FqPolyRepField}()

@doc raw"""
    FqPolyRepFieldElem <: FinFieldElem

An element $\sum_{i=0}^{d-1} a_i t^i$ of a finite field $\mathbb F_{p^d} \cong \mathbb F_p[t]/f$.
Represented internally as $(a_i)_{0\le i<d}$. See [`FqPolyRepField`](@ref).
"""
mutable struct FqPolyRepFieldElem <: FinFieldElem
  cstruct::FlintC.fq_struct
  parent::FqPolyRepField

  function FqPolyRepFieldElem(ctx::FqPolyRepField)
    d = new()
    @ccall libflint.fq_init2(d::Ref{FqPolyRepFieldElem}, ctx::Ref{FqPolyRepField})::Nothing
    finalizer(_fq_clear_fn, d)
    d.parent = ctx
    return d
  end

  function FqPolyRepFieldElem(
    ctx::FqPolyRepField,
    x::Union{
      FqPolyRepFieldElem,
      Integer,ZZRingElem,
      ZZPolyRingElem,
      ZZModPolyRingElem,FpPolyRingElem
    },
  )
    d = FqPolyRepFieldElem(ctx)
    set!(d, x)
    return d
  end
end
@flint_fields FqPolyRepFieldElem


###############################################################################
#
#   fqPolyRepMPolyRing / fqPolyRepMPolyRingElem
#
###############################################################################

@attributes mutable struct fqPolyRepMPolyRing <: MPolyRing{fqPolyRepFieldElem}
  cstruct::FlintC.fq_nmod_mpoly_ctx_struct

  base_ring::fqPolyRepField
  S::Vector{Symbol}

  function fqPolyRepMPolyRing(R::fqPolyRepField, s::Vector{Symbol}, S::Symbol, cached::Bool = true)
    return get_cached!(FqNmodMPolyID, (R, s, S), cached) do
      ord = _flint_ordering(S)

      z = new()
      @ccall libflint.fq_nmod_mpoly_ctx_init(z::Ref{fqPolyRepMPolyRing}, length(s)::Int, ord::FlintC.ordering_t, R::Ref{fqPolyRepField})::Nothing
      z.base_ring = R
      z.S = s
      finalizer(_fq_nmod_mpoly_ctx_clear_fn, z)
      return z
    end
  end
end
@flint_fields fqPolyRepMPolyRing
@flint_legacy_fields fqPolyRepMPolyRing begin
  nvars = cstruct.minfo.nvars
  nfields = cstruct.minfo.nfields
  ord = Cint(cstruct.minfo.ord)
  deg = cstruct.minfo.deg
  rev = cstruct.minfo.rev
  lut = cstruct.minfo.lut_words_per_exp
  lut1 = cstruct.minfo.lut_fix_bits
  n = cstruct.fqctx.mod.n
  ninv = cstruct.fqctx.mod.ninv
  norm = cstruct.fqctx.mod.norm
  sparse_modulus = cstruct.fqctx.sparse_modulus
  is_conway = cstruct.fqctx.is_conway
  a = cstruct.fqctx.a
  j = cstruct.fqctx.j
  len = cstruct.fqctx.len
  var = cstruct.fqctx.var
  mod_coeffs = cstruct.fqctx.modulus.coeffs
  mod_alloc = cstruct.fqctx.modulus.alloc
  mod_length = cstruct.fqctx.modulus.length
  mod_n = cstruct.fqctx.modulus.mod.n
  mod_ninv = cstruct.fqctx.modulus.mod.ninv
  mod_norm = cstruct.fqctx.modulus.mod.norm
  inv_coeffs = cstruct.fqctx.inv.coeffs
  inv_alloc = cstruct.fqctx.inv.alloc
  inv_length = cstruct.fqctx.inv.length
  inv_n = cstruct.fqctx.inv.mod.n
  inv_ninv = cstruct.fqctx.inv.mod.ninv
  inv_norm = cstruct.fqctx.inv.mod.norm
end

const FqNmodMPolyID = CacheDictType{Tuple{fqPolyRepField, Vector{Symbol}, Symbol}, fqPolyRepMPolyRing}()

mutable struct fqPolyRepMPolyRingElem <: MPolyRingElem{fqPolyRepFieldElem}
  cstruct::FlintC.fq_nmod_mpoly_struct

  parent::fqPolyRepMPolyRing

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing)
    z = new()
    @ccall libflint.fq_nmod_mpoly_init(z::Ref{fqPolyRepMPolyRingElem}, ctx::Ref{fqPolyRepMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_clear_fn, z)
    return z
  end

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing, a::Vector{fqPolyRepFieldElem}, b::Vector{Vector{UInt}})
    z = new()
    @ccall libflint.fq_nmod_mpoly_init2(z::Ref{fqPolyRepMPolyRingElem}, length(a)::Int, ctx::Ref{fqPolyRepMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.fq_nmod_mpoly_push_term_fq_nmod_ui(z::Ref{fqPolyRepMPolyRingElem}, a[i]::Ref{fqPolyRepFieldElem}, b[i]::Ptr{UInt}, ctx::Ref{fqPolyRepMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing, a::Vector{fqPolyRepFieldElem}, b::Vector{Vector{Int}})
    z = new()
    @ccall libflint.fq_nmod_mpoly_init2(z::Ref{fqPolyRepMPolyRingElem}, length(a)::Int, ctx::Ref{fqPolyRepMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.fq_nmod_mpoly_push_term_fq_nmod_ui(z::Ref{fqPolyRepMPolyRingElem}, a[i]::Ref{fqPolyRepFieldElem}, b[i]::Ptr{Int}, ctx::Ref{fqPolyRepMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing, a::Vector{fqPolyRepFieldElem}, b::Vector{Vector{ZZRingElem}})
    z = new()
    @ccall libflint.fq_nmod_mpoly_init2(z::Ref{fqPolyRepMPolyRingElem}, length(a)::Int, ctx::Ref{fqPolyRepMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_clear_fn, z)

    for i in 1:length(a)
      @ccall libflint.fq_nmod_mpoly_push_term_fq_nmod_fmpz(z::Ref{fqPolyRepMPolyRingElem}, a[i]::Ref{fqPolyRepFieldElem}, b[i]::Ptr{Ref{ZZRingElem}}, ctx::Ref{fqPolyRepMPolyRing})::Nothing
    end

    sort_terms!(z)
    combine_like_terms!(z)
    return z
  end

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing, a::UInt)
    z = fqPolyRepMPolyRingElem(ctx)
    @ccall libflint.fq_nmod_mpoly_set_ui(z::Ref{fqPolyRepMPolyRingElem}, a::UInt, ctx::Ref{fqPolyRepMPolyRing})::Nothing
    return z
  end

  function fqPolyRepMPolyRingElem(ctx::fqPolyRepMPolyRing, a::fqPolyRepFieldElem)
    z = fqPolyRepMPolyRingElem(ctx)
    @ccall libflint.fq_nmod_mpoly_set_fq_nmod(z::Ref{fqPolyRepMPolyRingElem}, a::Ref{fqPolyRepFieldElem}, ctx::Ref{fqPolyRepMPolyRing})::Nothing
    return z
  end
end
@flint_fields fqPolyRepMPolyRingElem

mutable struct fq_nmod_mpoly_factor
  cstruct::FlintC.fq_nmod_mpoly_factor_struct

  parent::fqPolyRepMPolyRing

  function fq_nmod_mpoly_factor(ctx::fqPolyRepMPolyRing)
    z = new()
    @ccall libflint.fq_nmod_mpoly_factor_init(z::Ref{fq_nmod_mpoly_factor}, ctx::Ref{fqPolyRepMPolyRing})::Nothing
    z.parent = ctx
    finalizer(_fq_nmod_mpoly_factor_clear_fn, z)
    return z
  end
end
@flint_fields fq_nmod_mpoly_factor
@flint_legacy_fields fq_nmod_mpoly_factor begin
  constant_coeffs = cstruct.constant.coeffs
  constant_alloc = cstruct.constant.alloc
  constant_length = cstruct.constant.length
  constant_n = cstruct.constant.mod.n
  constant_ninv = cstruct.constant.mod.ninv
  constant_norm = cstruct.constant.mod.norm
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

@doc raw"""
    PadicField <: FlintLocalField <: NonArchLocalField <: Field

A $p$-adic field for some prime $p$.
"""
@attributes mutable struct PadicField <: FlintLocalField
  p::Int
  pinv::Float64
  pow::Ptr{ZZRingElem} # array of fmpz
  minpre::Int
  maxpre::Int
  mode::Cint
  prec_max::Int

  function PadicField(p::ZZRingElem, prec::Int = 64; cached::Bool = true, check::Bool = true)
    check && !is_probable_prime(p) && throw(DomainError(p, "Integer must be prime"))

    Qp = get_cached!(PadicBase, p, cached) do
      d = new()
      @ccall libflint.padic_ctx_init(d::Ref{PadicField}, p::Ref{ZZRingElem}, 0::Int, 0::Int, 0::Cint)::Nothing
      finalizer(_padic_ctx_clear_fn, d)
      return d
    end
    Qp.prec_max = prec
    return Qp
  end
end

const PadicBase = CacheDictType{ZZRingElem, PadicField}()

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
    @ccall libflint.padic_init2(d::Ref{PadicFieldElem}, prec::Int)::Nothing
    finalizer(_padic_clear_fn, d)
    return d
  end
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
  pow::Ptr{ZZRingElem} # array of fmpz
  minpre::Int
  maxpre::Int
  mode::Cint
  a::Int         # ZZRingElem
  j::Ptr{Int} # array of slong
  len::Int
  var::Cstring   # char*
  prec_max::Int

  function QadicField(p::ZZRingElem, d::Int, prec::Int = 64, var::String = "a"; cached::Bool = true, check::Bool = true, base_field::PadicField = PadicField(p, prec, cached = cached))
    @req p == prime(base_field) "prime of base_field does not match p"
    check && @req is_probable_prime(p) "Integer must be prime"

    z = get_cached!(QadicBase, (base_field, d), cached) do
      zz = new()
      @ccall libflint.qadic_ctx_init(zz::Ref{QadicField}, p::Ref{ZZRingElem}, d::Int, 0::Int, 0::Int, var::Cstring, 0::Cint)::Nothing
      finalizer(_qadic_ctx_clear_fn, zz)
      return zz
    end
    z.prec_max = prec
    set_attribute!(z, :base_field, base_field)

    return z, gen(z)
  end

  function QadicField(f::T, prec::Int = 64, var::String = "a";
                      cached::Bool = true, check::Bool = true,
                      base_field::PadicField = PadicField(modulus(f), prec, cached = cached)
                     ) where T <: Union{zzModPolyRingElem,fpPolyRingElem,ZZModPolyRingElem,FpPolyRingElem}
    p = modulus(f)
    @req p == prime(base_field) "prime of base_field does not match modulus of f"
    check && @req is_irreducible(f) "Defining polynomial must be irreducible"
    check && @req is_monic(f) "Defining polynomial must be monic"

    z = get_cached!(_qadic_modulus_cache(T), (base_field, f), cached) do
      zz = new()
      _qadic_ctx_init_modulus!(zz, p, f, var)
      finalizer(_qadic_ctx_clear_fn, zz)
      return zz
    end
    z.prec_max = prec
    set_attribute!(z, :base_field, base_field)

    return z, gen(z)
  end

  # The QadicField(::FqPolyRingElem, ...) outer constructor lives in
  # flint/fq_default_poly.jl, since FqPolyRingElem is not yet defined here.
end

const QadicBase = CacheDictType{Tuple{PadicField, Int}, QadicField}()

const QadicBaseFmpzPol = CacheDictType{Tuple{PadicField, ZZModPolyRingElem}, QadicField}()
const QadicBaseGFPPol = CacheDictType{Tuple{PadicField, FpPolyRingElem}, QadicField}()
const QadicBaseNmodPol = CacheDictType{Tuple{PadicField, zzModPolyRingElem}, QadicField}()
const QadicBaseGFPNmodPol = CacheDictType{Tuple{PadicField, fpPolyRingElem}, QadicField}()

# Simple helper to pick correct cache on underlying poly type
_qadic_modulus_cache(::Type{ZZModPolyRingElem}) = QadicBaseFmpzPol
_qadic_modulus_cache(::Type{FpPolyRingElem}) = QadicBaseGFPPol
_qadic_modulus_cache(::Type{zzModPolyRingElem}) = QadicBaseNmodPol
_qadic_modulus_cache(::Type{fpPolyRingElem}) = QadicBaseGFPNmodPol

# Function-barrier helper to get concrete type T
function _qadic_ctx_init_modulus!(zz::QadicField, p::ZZRingElem, f::T, var::String) where {T}
  @ccall libflint.qadic_ctx_init_modulus(zz::Ref{QadicField}, p::Ref{ZZRingElem}, f::Ref{T}, 0::Int, 0::Int, var::Cstring, 0::Cint)::Nothing
end
function _qadic_ctx_init_modulus!(zz::QadicField, p::UInt, f::T, var::String) where {T}
  @ccall libflint.qadic_ctx_init_modulus_nmod(zz::Ref{QadicField}, p::UInt, f::Ref{T}, 0::Int, 0::Int, var::Cstring, 0::Cint)::Nothing
end


@doc raw"""
    QadicFieldElem <: FlintLocalFieldElem <: NonArchLocalFieldElem <: FieldElem

An element of a $q$-adic field. See [`QadicField`](@ref).
"""
mutable struct QadicFieldElem <: FlintLocalFieldElem
  coeffs::Ptr{ZZRingElem} # array of fmpz
  alloc::Int
  length::Int
  val::Int
  N::Int
  parent::QadicField

  function QadicFieldElem(prec::Int)
    z = new()
    @ccall libflint.qadic_init2(z::Ref{QadicFieldElem}, prec::Int)::Nothing
    finalizer(_qadic_clear_fn, z)
    return z
  end
end

@doc raw"""
    Qadic2SqrtPrecomp

Opaque precomputation used to speed up square roots in a $q$-adic field of
characteristic 2. Wraps a FLINT `struct qadic2_sqrt_precomp *`.
"""
mutable struct Qadic2SqrtPrecomp
  ptr::Ptr{FlintC.struct_qadic2_sqrt_precomp}
  parent::QadicField

  function Qadic2SqrtPrecomp(R::QadicField)
    prime(R) == 2 || throw(DomainError(prime(R), "characteristic must be 2"))
    ptr = @ccall libflint._qadic_char2_sqrt_precomp_init(R::Ref{QadicField})::Ptr{FlintC.struct_qadic2_sqrt_precomp}
    z = new(ptr, R)
    finalizer(_qadic_char2_sqrt_precomp_clear_fn, z)
    return z
  end
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
  cstruct::FlintC.fmpz_poly_struct

  prec::Int
  val::Int
  parent::ZZRelPowerSeriesRing

  function ZZRelPowerSeriesRingElem()
    z = new()
    @ccall libflint.fmpz_poly_init(z::Ref{ZZRelPowerSeriesRingElem})::Nothing
    finalizer(_fmpz_poly_clear_fn, z)
    return z
  end

  function ZZRelPowerSeriesRingElem(a::Vector{ZZRingElem}, len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.fmpz_poly_init2(z::Ref{ZZRelPowerSeriesRingElem}, len::Int)::Nothing
    for i = 1:len
      @ccall libflint.fmpz_poly_set_coeff_fmpz(z::Ref{ZZRelPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{ZZRingElem})::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_fmpz_poly_clear_fn, z)
    return z
  end

  function ZZRelPowerSeriesRingElem(a::ZZRelPowerSeriesRingElem)
    z = ZZRelPowerSeriesRingElem()
    @ccall libflint.fmpz_poly_set(z::Ref{ZZRelPowerSeriesRingElem}, a::Ref{ZZRelPowerSeriesRingElem})::Nothing
    return z
  end
end
@flint_fields ZZRelPowerSeriesRingElem

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
  cstruct::FlintC.fmpz_poly_struct
  prec :: Int
  parent::ZZAbsPowerSeriesRing

  function ZZAbsPowerSeriesRingElem()
    z = new()
    @ccall libflint.fmpz_poly_init(z::Ref{ZZAbsPowerSeriesRingElem})::Nothing
    finalizer(_fmpz_poly_clear_fn, z)
    return z
  end

  function ZZAbsPowerSeriesRingElem(a::Vector{ZZRingElem}, len::Int, prec::Int)
    z = new()
    @ccall libflint.fmpz_poly_init2(z::Ref{ZZAbsPowerSeriesRingElem}, len::Int)::Nothing
    for i = 1:len
      @ccall libflint.fmpz_poly_set_coeff_fmpz(z::Ref{ZZAbsPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{ZZRingElem})::Nothing
    end
    z.prec = prec
    finalizer(_fmpz_poly_clear_fn, z)
    return z
  end

  function ZZAbsPowerSeriesRingElem(a::ZZAbsPowerSeriesRingElem)
    z = ZZAbsPowerSeriesRingElem()
    @ccall libflint.fmpz_poly_set(z::Ref{ZZAbsPowerSeriesRingElem}, a::Ref{ZZAbsPowerSeriesRingElem})::Nothing
    return z
  end
end
@flint_fields ZZAbsPowerSeriesRingElem

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
  cstruct::FlintC.fmpz_poly_struct
  prec::Int
  val::Int
  scale::Int
  parent::ZZLaurentSeriesRing

  function ZZLaurentSeriesRingElem()
    z = new()
    @ccall libflint.fmpz_poly_init(z::Ref{ZZLaurentSeriesRingElem})::Nothing
    finalizer(_fmpz_poly_clear_fn, z)
    return z
  end

  function ZZLaurentSeriesRingElem(a::Vector{ZZRingElem}, len::Int, prec::Int, val::Int, scale::Int)
    z = new()
    @ccall libflint.fmpz_poly_init2(z::Ref{ZZLaurentSeriesRingElem}, len::Int)::Nothing
    for i = 1:len
      @ccall libflint.fmpz_poly_set_coeff_fmpz(z::Ref{ZZLaurentSeriesRingElem}, (i - 1)::Int, a[i]::Ref{ZZRingElem})::Nothing
    end
    z.prec = prec
    z.val = val
    z.scale = scale
    finalizer(_fmpz_poly_clear_fn, z)
    return z
  end

  function ZZLaurentSeriesRingElem(a::ZZLaurentSeriesRingElem)
    z = ZZLaurentSeriesRingElem()
    @ccall libflint.fmpz_poly_set(z::Ref{ZZLaurentSeriesRingElem}, a::Ref{ZZLaurentSeriesRingElem})::Nothing
    return z
  end
end
@flint_fields ZZLaurentSeriesRingElem

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
  cstruct::FlintC.fmpq_poly_struct

  prec::Int
  val::Int
  parent::QQRelPowerSeriesRing

  function QQRelPowerSeriesRingElem()
    z = new()
    @ccall libflint.fmpq_poly_init(z::Ref{QQRelPowerSeriesRingElem})::Nothing
    finalizer(_fmpq_poly_clear_fn, z)
    return z
  end

  function QQRelPowerSeriesRingElem(a::Vector{QQFieldElem}, len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.fmpq_poly_init2(z::Ref{QQRelPowerSeriesRingElem}, len::Int)::Nothing
    for i = 1:len
      @ccall libflint.fmpq_poly_set_coeff_fmpq(z::Ref{QQRelPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{QQFieldElem})::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_fmpq_poly_clear_fn, z)
    return z
  end

  function QQRelPowerSeriesRingElem(a::QQRelPowerSeriesRingElem)
    z = QQRelPowerSeriesRingElem()
    @ccall libflint.fmpq_poly_set(z::Ref{QQRelPowerSeriesRingElem}, a::Ref{QQRelPowerSeriesRingElem})::Nothing
    return z
  end
end
@flint_fields QQRelPowerSeriesRingElem

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
  cstruct::FlintC.fmpq_poly_struct

  prec :: Int
  parent::QQAbsPowerSeriesRing

  function QQAbsPowerSeriesRingElem()
    z = new()
    @ccall libflint.fmpq_poly_init(z::Ref{QQAbsPowerSeriesRingElem})::Nothing
    finalizer(_fmpq_poly_clear_fn, z)
    return z
  end

  function QQAbsPowerSeriesRingElem(a::Vector{QQFieldElem}, len::Int, prec::Int)
    z = new()
    @ccall libflint.fmpq_poly_init2(z::Ref{QQAbsPowerSeriesRingElem}, len::Int)::Nothing
    for i = 1:len
      @ccall libflint.fmpq_poly_set_coeff_fmpq(z::Ref{QQAbsPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{QQFieldElem})::Nothing
    end
    z.prec = prec
    finalizer(_fmpq_poly_clear_fn, z)
    return z
  end

  function QQAbsPowerSeriesRingElem(a::QQAbsPowerSeriesRingElem)
    z = QQAbsPowerSeriesRingElem()
    @ccall libflint.fmpq_poly_set(z::Ref{QQAbsPowerSeriesRingElem}, a::Ref{QQAbsPowerSeriesRingElem})::Nothing
    return z
  end
end
@flint_fields QQAbsPowerSeriesRingElem

###############################################################################
#
#   fpRelPowerSeriesRing / fpRelPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct fpRelPowerSeriesRing <: SeriesRing{fpFieldElem}
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
  cstruct::FlintC.nmod_poly_struct
  prec::Int
  val::Int
  parent::fpRelPowerSeriesRing

  function fpRelPowerSeriesRingElem(p::UInt)
    z = new()
    @ccall libflint.nmod_poly_init(z::Ref{fpRelPowerSeriesRingElem}, p::UInt)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function fpRelPowerSeriesRingElem(p::UInt, a::Vector{ZZRingElem}, len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{fpRelPowerSeriesRingElem}, p::UInt, len::Int)::Nothing
    for i = 1:len
      tt = @ccall libflint.fmpz_fdiv_ui(a[i]::Ref{ZZRingElem}, p::UInt)::UInt
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{fpRelPowerSeriesRingElem}, (i - 1)::Int, tt::UInt)::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function fpRelPowerSeriesRingElem(p::UInt, a::Vector{UInt}, len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{fpRelPowerSeriesRingElem}, p::UInt, len::Int)::Nothing
    for i = 1:len
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{fpRelPowerSeriesRingElem}, (i - 1)::Int, a[i]::UInt)::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function fpRelPowerSeriesRingElem(p::UInt, a::Vector{fpFieldElem}, len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{fpRelPowerSeriesRingElem}, p::UInt, len::Int)::Nothing
    for i = 1:len
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{fpRelPowerSeriesRingElem}, (i - 1)::Int, data(a[i])::UInt)::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function fpRelPowerSeriesRingElem(a::fpRelPowerSeriesRingElem)
    p = modulus(base_ring(parent(a)))
    z = fpRelPowerSeriesRingElem(p)
    @ccall libflint.nmod_poly_set(z::Ref{fpRelPowerSeriesRingElem}, a::Ref{fpRelPowerSeriesRingElem})::Nothing
    if isdefined(a, :parent)
      z.parent = a.parent
    end
    return z
  end
end
@flint_fields fpRelPowerSeriesRingElem
@flint_legacy_fields fpRelPowerSeriesRingElem begin
  mod_n = cstruct.mod.n
  mod_ninv = cstruct.mod.ninv
  mod_norm = cstruct.mod.norm
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
  cstruct::FlintC.nmod_poly_struct
  prec::Int
  val::Int
  parent::zzModRelPowerSeriesRing

  function zzModRelPowerSeriesRingElem(p::UInt)
    z = new()
    @ccall libflint.nmod_poly_init(z::Ref{zzModRelPowerSeriesRingElem}, p::UInt)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModRelPowerSeriesRingElem(p::UInt, a::Vector{ZZRingElem}, len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{zzModRelPowerSeriesRingElem}, p::UInt, len::Int)::Nothing
    for i = 1:len
      tt = @ccall libflint.fmpz_fdiv_ui(a[i]::Ref{ZZRingElem}, p::UInt)::UInt
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{zzModRelPowerSeriesRingElem}, (i - 1)::Int, tt::UInt)::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModRelPowerSeriesRingElem(p::UInt, a::Vector{UInt}, len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{zzModRelPowerSeriesRingElem}, p::UInt, len::Int)::Nothing
    for i = 1:len
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{zzModRelPowerSeriesRingElem}, (i - 1)::Int, a[i]::UInt)::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModRelPowerSeriesRingElem(p::UInt, a::Vector{zzModRingElem}, len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{zzModRelPowerSeriesRingElem}, p::UInt, len::Int)::Nothing
    for i = 1:len
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{zzModRelPowerSeriesRingElem}, (i - 1)::Int, data(a[i])::UInt)::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModRelPowerSeriesRingElem(a::zzModRelPowerSeriesRingElem)
    p = modulus(base_ring(parent(a)))
    z = zzModRelPowerSeriesRingElem(p)
    @ccall libflint.nmod_poly_set(z::Ref{zzModRelPowerSeriesRingElem}, a::Ref{zzModRelPowerSeriesRingElem})::Nothing
    if isdefined(a, :parent)
      z.parent = a.parent
    end
    return z
  end
end
@flint_fields zzModRelPowerSeriesRingElem
@flint_legacy_fields zzModRelPowerSeriesRingElem begin
  mod_n = cstruct.mod.n
  mod_ninv = cstruct.mod.ninv
  mod_norm = cstruct.mod.norm
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
  cstruct::FlintC.fmpz_mod_poly_struct

  prec::Int
  val::Int
  parent::FpRelPowerSeriesRing

  function FpRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct)
    z = new()
    @ccall libflint.fmpz_mod_poly_init(z::Ref{FpRelPowerSeriesRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpRelPowerSeriesRingElem(R::FpField)
    return FpRelPowerSeriesRingElem(R.ninv)
  end

  function FpRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZRingElem},
      len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{FpRelPowerSeriesRingElem}, len::Int, p::Ref{fmpz_mod_ctx_struct})::Nothing
    for i = 1:len
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{FpRelPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{ZZRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpRelPowerSeriesRingElem(R::FpField, a::Vector{ZZRingElem},
      len::Int, prec::Int, val::Int)
    return FpRelPowerSeriesRingElem(R.ninv, a, len, prec, val)
  end

  function FpRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{FpFieldElem},
      len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{FpRelPowerSeriesRingElem}, len::Int, p::Ref{fmpz_mod_ctx_struct})::Nothing
    for i = 1:len
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{FpRelPowerSeriesRingElem}, (i - 1)::Int, data(a[i])::Ref{ZZRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpRelPowerSeriesRingElem(R::FpField, a::Vector{FpFieldElem},
      len::Int, prec::Int, val::Int)
    return FpRelPowerSeriesRingElem(R.ninv, a, len, prec, val)
  end

  function FpRelPowerSeriesRingElem(a::FpRelPowerSeriesRingElem)
    z = new()
    p = base_ring(parent(a)).ninv
    @ccall libflint.fmpz_mod_poly_init(z::Ref{FpRelPowerSeriesRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    @ccall libflint.fmpz_mod_poly_set(z::Ref{FpRelPowerSeriesRingElem}, a::Ref{FpRelPowerSeriesRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    if isdefined(a, :parent)
      z.parent = a.parent
    end
    return z
  end
end
@flint_fields FpRelPowerSeriesRingElem

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
  cstruct::FlintC.fmpz_mod_poly_struct

  prec::Int
  val::Int
  parent::ZZModRelPowerSeriesRing

  function ZZModRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct)
    z = new()
    @ccall libflint.fmpz_mod_poly_init(z::Ref{ZZModRelPowerSeriesRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModRelPowerSeriesRingElem(R::ZZModRing)
    return ZZModRelPowerSeriesRingElem(R.ninv)
  end

  function ZZModRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZRingElem},
      len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{ZZModRelPowerSeriesRingElem}, len::Int, p::Ref{fmpz_mod_ctx_struct})::Nothing
    for i = 1:len
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{ZZModRelPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{ZZRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModRelPowerSeriesRingElem(R::ZZModRing, a::Vector{ZZRingElem},
      len::Int, prec::Int, val::Int)
    return ZZModRelPowerSeriesRingElem(R.ninv, a, len, prec, val)
  end

  function ZZModRelPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZModRingElem},
      len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{ZZModRelPowerSeriesRingElem}, len::Int, p::Ref{fmpz_mod_ctx_struct})::Nothing
    for i = 1:len
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{ZZModRelPowerSeriesRingElem}, (i - 1)::Int, data(a[i])::Ref{ZZRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModRelPowerSeriesRingElem(R::ZZModRing, a::Vector{ZZModRingElem},
      len::Int, prec::Int, val::Int)
    return ZZModRelPowerSeriesRingElem(R.ninv, a, len, prec, val)
  end

  function ZZModRelPowerSeriesRingElem(a::ZZModRelPowerSeriesRingElem)
    z = new()
    p = base_ring(parent(a)).ninv
    @ccall libflint.fmpz_mod_poly_init(z::Ref{ZZModRelPowerSeriesRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    @ccall libflint.fmpz_mod_poly_set(z::Ref{ZZModRelPowerSeriesRingElem}, a::Ref{ZZModRelPowerSeriesRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    if isdefined(a, :parent)
      z.parent = a.parent
    end
    return z
  end
end
@flint_fields ZZModRelPowerSeriesRingElem

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
  cstruct::FlintC.fmpz_mod_poly_struct

  prec::Int
  parent::FpAbsPowerSeriesRing

  function FpAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct)
    z = new()
    @ccall libflint.fmpz_mod_poly_init(z::Ref{FpAbsPowerSeriesRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpAbsPowerSeriesRingElem(R::FpField)
    return FpAbsPowerSeriesRingElem(R.ninv)
  end

  function FpAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZRingElem},
      len::Int, prec::Int)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{FpAbsPowerSeriesRingElem}, len::Int, p::Ref{fmpz_mod_ctx_struct})::Nothing
    for i = 1:len
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{FpAbsPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{ZZRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    z.prec = prec
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpAbsPowerSeriesRingElem(R::FpField, a::Vector{ZZRingElem}, len::Int, prec::Int)
    return FpAbsPowerSeriesRingElem(R.ninv, a, len, prec)
  end

  function FpAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{FpFieldElem},
      len::Int, prec::Int)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{FpAbsPowerSeriesRingElem}, len::Int, p::Ref{fmpz_mod_ctx_struct})::Nothing
    for i = 1:len
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{FpAbsPowerSeriesRingElem}, (i - 1)::Int, data(a[i])::Ref{ZZRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    z.prec = prec
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function FpAbsPowerSeriesRingElem(R::FpField, a::Vector{FpFieldElem},
      len::Int, prec::Int)
    return FpAbsPowerSeriesRingElem(R.ninv, a, len, prec)
  end

  function FpAbsPowerSeriesRingElem(a::FpAbsPowerSeriesRingElem)
    p = base_ring(parent(a)).ninv
    z = FpAbsPowerSeriesRingElem(p)
    @ccall libflint.fmpz_mod_poly_set(z::Ref{FpAbsPowerSeriesRingElem}, a::Ref{FpAbsPowerSeriesRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    if isdefined(a, :parent)
      z.parent = a.parent
    end
    return z
  end
end
@flint_fields FpAbsPowerSeriesRingElem

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
  cstruct::FlintC.nmod_poly_struct

  prec::Int
  parent::zzModAbsPowerSeriesRing

  function zzModAbsPowerSeriesRingElem(n::UInt)
    z = new()
    @ccall libflint.nmod_poly_init(z::Ref{zzModAbsPowerSeriesRingElem}, n::UInt)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModAbsPowerSeriesRingElem(n::UInt, arr::Vector{ZZRingElem}, len::Int, prec::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{zzModAbsPowerSeriesRingElem}, n::UInt, length(arr)::Int)::Nothing
    for i in 1:len
      tt = @ccall libflint.fmpz_fdiv_ui(arr[i]::Ref{ZZRingElem}, n::UInt)::UInt
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{zzModAbsPowerSeriesRingElem}, (i - 1)::Int, tt::UInt)::Nothing
    end
    z.prec = prec
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModAbsPowerSeriesRingElem(n::UInt, arr::Vector{UInt}, len::Int, prec::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{zzModAbsPowerSeriesRingElem}, n::UInt, length(arr)::Int)::Nothing
    for i in 1:len
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{zzModAbsPowerSeriesRingElem}, (i - 1)::Int, arr[i]::UInt)::Nothing
    end
    z.prec = prec
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModAbsPowerSeriesRingElem(n::UInt, arr::Vector{zzModRingElem}, len::Int, prec::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{zzModAbsPowerSeriesRingElem}, n::UInt, length(arr)::Int)::Nothing
    for i in 1:len
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{zzModAbsPowerSeriesRingElem}, (i-1)::Int, arr[i].data::UInt)::Nothing
    end
    z.prec = prec
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function zzModAbsPowerSeriesRingElem(a::zzModAbsPowerSeriesRingElem)
    z = new()
    R = base_ring(a)
    @ccall libflint.nmod_poly_init2(z::Ref{zzModAbsPowerSeriesRingElem}, R.n::UInt, length(a)::Int)::Nothing
    @ccall libflint.nmod_poly_set(z::Ref{zzModAbsPowerSeriesRingElem}, a::Ref{zzModAbsPowerSeriesRingElem})::Nothing
    z.prec = a.prec
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end
end
@flint_fields zzModAbsPowerSeriesRingElem
@flint_legacy_fields zzModAbsPowerSeriesRingElem begin
  mod_n = cstruct.mod.n
  mod_ninv = cstruct.mod.ninv
  mod_norm = cstruct.mod.norm
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
  cstruct::FlintC.nmod_poly_struct

  prec::Int
  parent::fpAbsPowerSeriesRing

  function fpAbsPowerSeriesRingElem(n::UInt)
    z = new()
    @ccall libflint.nmod_poly_init(z::Ref{fpAbsPowerSeriesRingElem}, n::UInt)::Nothing
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function fpAbsPowerSeriesRingElem(n::UInt, arr::Vector{ZZRingElem}, len::Int, prec::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{fpAbsPowerSeriesRingElem}, n::UInt, length(arr)::Int)::Nothing
    for i in 1:len
      tt = @ccall libflint.fmpz_fdiv_ui(arr[i]::Ref{ZZRingElem}, n::UInt)::UInt
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{fpAbsPowerSeriesRingElem}, (i - 1)::Int, tt::UInt)::Nothing
    end
    z.prec = prec
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function fpAbsPowerSeriesRingElem(n::UInt, arr::Vector{UInt}, len::Int, prec::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{fpAbsPowerSeriesRingElem}, n::UInt, length(arr)::Int)::Nothing
    for i in 1:len
      @ccall libflint.nmod_poly_series_set_coeff_ui(z::Ref{fpAbsPowerSeriesRingElem}, (i - 1)::Int, arr[i]::UInt)::Nothing
    end
    z.prec = prec
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function fpAbsPowerSeriesRingElem(n::UInt, arr::Vector{fpFieldElem}, len::Int, prec::Int)
    z = new()
    @ccall libflint.nmod_poly_init2(z::Ref{fpAbsPowerSeriesRingElem}, n::UInt, length(arr)::Int)::Nothing
    for i in 1:len
      @ccall libflint.nmod_poly_set_coeff_ui(z::Ref{fpAbsPowerSeriesRingElem}, (i-1)::Int, arr[i].data::UInt)::Nothing
    end
    z.prec = prec
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end

  function fpAbsPowerSeriesRingElem(a::fpAbsPowerSeriesRingElem)
    z = new()
    R = base_ring(a)
    @ccall libflint.nmod_poly_init2(z::Ref{fpAbsPowerSeriesRingElem}, R.n::UInt, length(a)::Int)::Nothing
    @ccall libflint.nmod_poly_set(z::Ref{fpAbsPowerSeriesRingElem}, a::Ref{fpAbsPowerSeriesRingElem})::Nothing
    z.prec = a.prec
    finalizer(_nmod_poly_clear_fn, z)
    return z
  end
end
@flint_fields fpAbsPowerSeriesRingElem
@flint_legacy_fields fpAbsPowerSeriesRingElem begin
  mod_n = cstruct.mod.n
  mod_ninv = cstruct.mod.ninv
  mod_norm = cstruct.mod.norm
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
  cstruct::FlintC.fmpz_mod_poly_struct

  prec::Int
  parent::ZZModAbsPowerSeriesRing

  function ZZModAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct)
    z = new()
    @ccall libflint.fmpz_mod_poly_init(z::Ref{ZZModAbsPowerSeriesRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModAbsPowerSeriesRingElem(R::ZZModRing)
    return ZZModAbsPowerSeriesRingElem(R.ninv)
  end

  function ZZModAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZRingElem},
      len::Int, prec::Int)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{ZZModAbsPowerSeriesRingElem}, len::Int, p::Ref{fmpz_mod_ctx_struct})::Nothing
    for i = 1:len
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{ZZModAbsPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{ZZRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    z.prec = prec
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModAbsPowerSeriesRingElem(R::ZZModRing, a::Vector{ZZRingElem}, len::Int, prec::Int)
    return ZZModAbsPowerSeriesRingElem(R.ninv, a, len, prec)
  end

  function ZZModAbsPowerSeriesRingElem(p::fmpz_mod_ctx_struct, a::Vector{ZZModRingElem},
      len::Int, prec::Int)
    z = new()
    @ccall libflint.fmpz_mod_poly_init2(z::Ref{ZZModAbsPowerSeriesRingElem}, len::Int, p::Ref{fmpz_mod_ctx_struct})::Nothing
    for i = 1:len
      @ccall libflint.fmpz_mod_poly_set_coeff_fmpz(z::Ref{ZZModAbsPowerSeriesRingElem}, (i - 1)::Int, data(a[i])::Ref{ZZRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    end
    z.prec = prec
    finalizer(_fmpz_mod_poly_clear_fn, z)
    return z
  end

  function ZZModAbsPowerSeriesRingElem(R::ZZModRing, a::Vector{ZZModRingElem},
      len::Int, prec::Int)
    return ZZModAbsPowerSeriesRingElem(R.ninv, a, len, prec)
  end

  function ZZModAbsPowerSeriesRingElem(a::ZZModAbsPowerSeriesRingElem)
    p = base_ring(parent(a)).ninv
    z = ZZModAbsPowerSeriesRingElem(p)
    @ccall libflint.fmpz_mod_poly_set(z::Ref{ZZModAbsPowerSeriesRingElem}, a::Ref{ZZModAbsPowerSeriesRingElem}, p::Ref{fmpz_mod_ctx_struct})::Nothing
    z.prec = a.prec
    if isdefined(a, :parent)
      z.parent = a.parent
    end
    return z
  end
end
@flint_fields ZZModAbsPowerSeriesRingElem

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
  cstruct::FlintC.fq_default_poly_struct

  prec::Int
  val::Int
  parent::FqRelPowerSeriesRing

  function FqRelPowerSeriesRingElem(ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_init(z::Ref{FqRelPowerSeriesRingElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqRelPowerSeriesRingElem(ctx::FqField, a::Vector{FqFieldElem}, len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.fq_default_poly_init2(z::Ref{FqRelPowerSeriesRingElem}, len::Int, ctx::Ref{FqField})::Nothing
    for i = 1:len
      @ccall libflint.fq_default_poly_set_coeff(z::Ref{FqRelPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{FqFieldElem}, ctx::Ref{FqField})::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqRelPowerSeriesRingElem(ctx::FqField, a::FqRelPowerSeriesRingElem)
    z = new()
    @ccall libflint.fq_default_poly_init(z::Ref{FqRelPowerSeriesRingElem}, ctx::Ref{FqField})::Nothing
    @ccall libflint.fq_default_poly_set(z::Ref{FqRelPowerSeriesRingElem}, a::Ref{FqRelPowerSeriesRingElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end
end
@flint_fields FqRelPowerSeriesRingElem

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
  cstruct::FlintC.fq_poly_struct
  prec::Int
  val::Int
  parent::FqPolyRepRelPowerSeriesRing

  function FqPolyRepRelPowerSeriesRingElem(ctx::FqPolyRepField)
    z = new()
    @ccall libflint.fq_poly_init(z::Ref{FqPolyRepRelPowerSeriesRingElem}, ctx::Ref{FqPolyRepField})::Nothing
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepRelPowerSeriesRingElem(ctx::FqPolyRepField, a::Vector{FqPolyRepFieldElem}, len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.fq_poly_init2(z::Ref{FqPolyRepRelPowerSeriesRingElem}, len::Int, ctx::Ref{FqPolyRepField})::Nothing
    for i = 1:len
      @ccall libflint.fq_poly_set_coeff(z::Ref{FqPolyRepRelPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{FqPolyRepFieldElem}, ctx::Ref{FqPolyRepField})::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepRelPowerSeriesRingElem(ctx::FqPolyRepField, a::FqPolyRepRelPowerSeriesRingElem)
    z = new()
    @ccall libflint.fq_poly_init(z::Ref{FqPolyRepRelPowerSeriesRingElem}, ctx::Ref{FqPolyRepField})::Nothing
    @ccall libflint.fq_poly_set(z::Ref{FqPolyRepRelPowerSeriesRingElem}, a::Ref{FqPolyRepRelPowerSeriesRingElem}, ctx::Ref{FqPolyRepField})::Nothing
    finalizer(_fq_poly_clear_fn, z)
    return z
  end
end
@flint_fields FqPolyRepRelPowerSeriesRingElem

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
  cstruct::FlintC.fq_default_poly_struct

  prec::Int
  parent::FqAbsPowerSeriesRing

  function FqAbsPowerSeriesRingElem(ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_init(z::Ref{FqAbsPowerSeriesRingElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqAbsPowerSeriesRingElem(ctx::FqField, a::Vector{FqFieldElem}, len::Int, prec::Int)
    z = new()
    @ccall libflint.fq_default_poly_init2(z::Ref{FqAbsPowerSeriesRingElem}, len::Int, ctx::Ref{FqField})::Nothing
    for i = 1:len
      @ccall libflint.fq_default_poly_set_coeff(z::Ref{FqAbsPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{FqFieldElem}, ctx::Ref{FqField})::Nothing
    end
    z.prec = prec
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqAbsPowerSeriesRingElem(ctx::FqField, a::FqAbsPowerSeriesRingElem)
    z = new()
    @ccall libflint.fq_default_poly_init(z::Ref{FqAbsPowerSeriesRingElem}, ctx::Ref{FqField})::Nothing
    @ccall libflint.fq_default_poly_set(z::Ref{FqAbsPowerSeriesRingElem}, a::Ref{FqAbsPowerSeriesRingElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end
end
@flint_fields FqAbsPowerSeriesRingElem

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
  cstruct::FlintC.fq_poly_struct
  prec::Int
  parent::FqPolyRepAbsPowerSeriesRing

  function FqPolyRepAbsPowerSeriesRingElem(ctx::FqPolyRepField)
    z = new()
    @ccall libflint.fq_poly_init(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, ctx::Ref{FqPolyRepField})::Nothing
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepAbsPowerSeriesRingElem(ctx::FqPolyRepField, a::Vector{FqPolyRepFieldElem}, len::Int, prec::Int)
    z = new()
    @ccall libflint.fq_poly_init2(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, len::Int, ctx::Ref{FqPolyRepField})::Nothing
    for i = 1:len
      @ccall libflint.fq_poly_set_coeff(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{FqPolyRepFieldElem}, ctx::Ref{FqPolyRepField})::Nothing
    end
    z.prec = prec
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepAbsPowerSeriesRingElem(ctx::FqPolyRepField, a::FqPolyRepAbsPowerSeriesRingElem)
    z = new()
    @ccall libflint.fq_poly_init(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, ctx::Ref{FqPolyRepField})::Nothing
    @ccall libflint.fq_poly_set(z::Ref{FqPolyRepAbsPowerSeriesRingElem}, a::Ref{FqPolyRepAbsPowerSeriesRingElem}, ctx::Ref{FqPolyRepField})::Nothing
    finalizer(_fq_poly_clear_fn, z)
    return z
  end
end
@flint_fields FqPolyRepAbsPowerSeriesRingElem

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
  cstruct::FlintC.fq_nmod_poly_struct
  prec::Int
  val::Int
  parent::fqPolyRepRelPowerSeriesRing

  function fqPolyRepRelPowerSeriesRingElem(ctx::fqPolyRepField)
    z = new()
    @ccall libflint.fq_nmod_poly_init(z::Ref{fqPolyRepRelPowerSeriesRingElem}, ctx::Ref{fqPolyRepField})::Nothing
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepRelPowerSeriesRingElem(ctx::fqPolyRepField, a::Vector{fqPolyRepFieldElem}, len::Int, prec::Int, val::Int)
    z = new()
    @ccall libflint.fq_nmod_poly_init2(z::Ref{fqPolyRepRelPowerSeriesRingElem}, len::Int, ctx::Ref{fqPolyRepField})::Nothing
    for i = 1:len
      @ccall libflint.fq_nmod_poly_set_coeff(z::Ref{fqPolyRepRelPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{fqPolyRepFieldElem}, ctx::Ref{fqPolyRepField})::Nothing
    end
    z.prec = prec
    z.val = val
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepRelPowerSeriesRingElem(ctx::fqPolyRepField, a::fqPolyRepRelPowerSeriesRingElem)
    z = new()
    @ccall libflint.fq_nmod_poly_init(z::Ref{fqPolyRepRelPowerSeriesRingElem}, ctx::Ref{fqPolyRepField})::Nothing
    @ccall libflint.fq_nmod_poly_set(z::Ref{fqPolyRepRelPowerSeriesRingElem}, a::Ref{fqPolyRepRelPowerSeriesRingElem}, ctx::Ref{fqPolyRepField})::Nothing
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end
end
@flint_fields fqPolyRepRelPowerSeriesRingElem

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
  cstruct::FlintC.fq_nmod_poly_struct
  prec::Int
  parent::fqPolyRepAbsPowerSeriesRing

  function fqPolyRepAbsPowerSeriesRingElem(ctx::fqPolyRepField)
    z = new()
    @ccall libflint.fq_nmod_poly_init(z::Ref{fqPolyRepAbsPowerSeriesRingElem}, ctx::Ref{fqPolyRepField})::Nothing
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepAbsPowerSeriesRingElem(ctx::fqPolyRepField, a::Vector{fqPolyRepFieldElem}, len::Int, prec::Int)
    z = new()
    @ccall libflint.fq_nmod_poly_init2(z::Ref{fqPolyRepAbsPowerSeriesRingElem}, len::Int, ctx::Ref{fqPolyRepField})::Nothing
    for i = 1:len
      @ccall libflint.fq_nmod_poly_set_coeff(z::Ref{fqPolyRepAbsPowerSeriesRingElem}, (i - 1)::Int, a[i]::Ref{fqPolyRepFieldElem}, ctx::Ref{fqPolyRepField})::Nothing
    end
    z.prec = prec
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepAbsPowerSeriesRingElem(ctx::fqPolyRepField, a::fqPolyRepAbsPowerSeriesRingElem)
    z = new()
    @ccall libflint.fq_nmod_poly_init(z::Ref{fqPolyRepAbsPowerSeriesRingElem}, ctx::Ref{fqPolyRepField})::Nothing
    @ccall libflint.fq_nmod_poly_set(z::Ref{fqPolyRepAbsPowerSeriesRingElem}, a::Ref{fqPolyRepAbsPowerSeriesRingElem}, ctx::Ref{fqPolyRepField})::Nothing
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end
end
@flint_fields fqPolyRepAbsPowerSeriesRingElem

###############################################################################
#
#   QQMatrixSpace / QQMatrix
#
###############################################################################

const QQMatrixSpace = AbstractAlgebra.Generic.MatSpace{QQFieldElem}

QQMatrixSpace(r::Int, c::Int) = QQMatrixSpace(QQ, r, c)

mutable struct QQMatrix <: FlintMatElem{QQFieldElem}
  cstruct::FlintC.fmpq_mat_struct
  view_parent

  # MatElem interface
  QQMatrix(::QQField, ::UndefInitializer, r::Int, c::Int) = QQMatrix(r, c)

  # Used by view, not finalised!!
  function QQMatrix()
    return new()
  end

  function QQMatrix(r::Int, c::Int)
    z = new()
    @ccall libflint.fmpq_mat_init(z::Ref{QQMatrix}, r::Int, c::Int)::Nothing
    finalizer(_fmpq_mat_clear_fn, z)
    return z
  end

  function QQMatrix(m::QQMatrix)
    z = new()
    @ccall libflint.fmpq_mat_init_set(z::Ref{QQMatrix}, m::Ref{QQMatrix})::Nothing
    finalizer(_fmpq_mat_clear_fn, z)
    return z
  end
end
@flint_fields QQMatrix entries = Ptr{QQFieldElem}

###############################################################################
#
#   ZZMatrixSpace / ZZMatrix
#
###############################################################################

const ZZMatrixSpace = AbstractAlgebra.Generic.MatSpace{ZZRingElem}

ZZMatrixSpace(r::Int, c::Int) = ZZMatrixSpace(ZZ, r, c)

mutable struct ZZMatrix <: FlintMatElem{ZZRingElem}
  cstruct::FlintC.fmpz_mat_struct
  view_parent

  # MatElem interface
  ZZMatrix(::ZZRing, ::UndefInitializer, r::Int, c::Int) = ZZMatrix(r, c)

  # Used by view, not finalised!!
  function ZZMatrix()
    return new()
  end

  function ZZMatrix(r::Int, c::Int)
    z = new()
    @ccall libflint.fmpz_mat_init(z::Ref{ZZMatrix}, r::Int, c::Int)::Nothing
    finalizer(_fmpz_mat_clear_fn, z)
    return z
  end

  function ZZMatrix(m::ZZMatrix)
    z = new()
    @ccall libflint.fmpz_mat_init_set(z::Ref{ZZMatrix}, m::Ref{ZZMatrix})::Nothing
    finalizer(_fmpz_mat_clear_fn, z)
    return z
  end
end

@flint_fields ZZMatrix entries = Ptr{ZZRingElem}

###############################################################################
#
#   ZZPolyRingMatrix
#
###############################################################################

mutable struct ZZPolyRingMatrix <: FlintMatElem{ZZPolyRingElem}
  cstruct::FlintC.fmpz_poly_mat_struct
  base_ring::ZZPolyRing
  view_parent

  # MatElem interface
  function ZZPolyRingMatrix(R::ZZPolyRing, ::UndefInitializer, r::Int, c::Int)
    z = ZZPolyRingMatrix(r, c, R)
    return z
  end

  # Used by view, not finalised!!
  function ZZPolyRingMatrix()
    return new()
  end

  function ZZPolyRingMatrix(r::Int, c::Int, R::ZZPolyRing)
    z = new()
    @ccall libflint.fmpz_poly_mat_init(z::Ref{ZZPolyRingMatrix}, r::Int, c::Int)::Nothing
    finalizer(_fmpz_poly_mat_clear_fn, z)
    z.base_ring = R
    return z
  end

  function ZZPolyRingMatrix(m::ZZPolyRingMatrix)
    z = new()
    @ccall libflint.fmpz_poly_mat_init_set(z::Ref{ZZPolyRingMatrix}, m::Ref{ZZPolyRingMatrix})::Nothing
    finalizer(_fmpz_poly_mat_clear_fn, z)
    z.base_ring = m.base_ring
    return z
  end
end
@flint_fields ZZPolyRingMatrix entries = Ptr{ZZPolyRingElem}

###############################################################################
#
#   zzModMatrixSpace / zzModMatrix
#
###############################################################################

const zzModMatrixSpace = AbstractAlgebra.Generic.MatSpace{zzModRingElem}

mutable struct zzModMatrix <: FlintMatElem{zzModRingElem}
  cstruct::FlintC.nmod_mat_struct
  base_ring::zzModRing
  view_parent

  # MatElem interface
  function zzModMatrix(R::zzModRing, ::UndefInitializer, r::Int, c::Int)
    z = zzModMatrix(r, c, R)
    z.base_ring = R
    return z
  end

  # Used by view, not finalised!!
  function zzModMatrix()
    return new()
  end

  function zzModMatrix(r::Int, c::Int, n::Union{UInt, zzModRing})
    z = new()
    # The entry buffer is Julia-owned; `view_parent` keeps it alive, so no
    # finalizer is needed and FLINT must never be asked to clear this matrix.
    u = zeros(UInt, r*c)
    z.entries = pointer(u)
    z.view_parent = u
    z.r = r
    z.c = c
    z.stride = c
    _set_mod!(z, n)
    return z
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{UInt})
    z = zzModMatrix(r, c, n)
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

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{ZZRingElem})
    z = zzModMatrix(r, c, n)
    t = ZZRingElem()
    for i = 1:r
      for j = 1:c
        @ccall libflint.fmpz_mod_ui(t::Ref{ZZRingElem}, arr[i, j]::Ref{ZZRingElem}, n::UInt)::Nothing
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
        @ccall libflint.fmpz_mod_ui(t::Ref{ZZRingElem}, (arr[(i - 1) * c + j])::Ref{ZZRingElem}, n::UInt)::Nothing
        setindex!(z, t, i, j)
      end
    end
    return z
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{T}) where {T <: Integer}
    arr_fmpz = map(ZZRingElem, arr)
    return zzModMatrix(r, c, n, arr_fmpz)
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractVector{T}) where {T <: Integer}
    arr_fmpz = map(ZZRingElem, arr)
    return zzModMatrix(r, c, n, arr_fmpz)
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{zzModRingElem})
    z = zzModMatrix(r, c, n)
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
    z = zzModMatrix(nrows(b), ncols(b), n)
    @ccall libflint.fmpz_mat_get_nmod_mat(z::Ref{zzModMatrix}, b::Ref{ZZMatrix})::Nothing
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
@flint_fields zzModMatrix entries = Ptr{UInt}
@flint_legacy_fields zzModMatrix begin
  n = cstruct.mod.n
  ninv = cstruct.mod.ninv
  norm = cstruct.mod.norm
end

###############################################################################
#
#   ZZModMatrixSpace / ZZModMatrix
#
###############################################################################

const ZZModMatrixSpace = AbstractAlgebra.Generic.MatSpace{ZZModRingElem}

mutable struct ZZModMatrix <: FlintMatElem{ZZModRingElem}
  cstruct::FlintC.fmpz_mod_mat_struct

  base_ring::ZZModRing
  view_parent

  # MatElem interface
  function ZZModMatrix(R::ZZModRing, ::UndefInitializer, r::Int, c::Int)
    z = ZZModMatrix(r, c, R.ninv)
    z.base_ring = R
    return z
  end

  # Used by view, not finalised!!
  function ZZModMatrix()
    return new()
  end

  function ZZModMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct)
    z = new()
    @ccall libflint.fmpz_mod_mat_init(z::Ref{ZZModMatrix}, r::Int, c::Int, ctx::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_mat_clear_fn, z)
    return z
  end

  function ZZModMatrix(r::Int, c::Int, R::ZZModRing)
    ZZModMatrix(r, c, R.ninv)
  end

  function ZZModMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{ZZRingElem})
    z = ZZModMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(arr[i, j], ctx), i, j)
      end
    end
    return z
  end

  function ZZModMatrix(r::Int, c::Int, R::ZZModRing, arr::AbstractMatrix{ZZRingElem})
    ZZModMatrix(r, c, R.ninv, arr)
  end

  function ZZModMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{T}) where T <: Integer
    z = ZZModMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(ZZRingElem(arr[i, j]), ctx), i, j)
      end
    end
    return z
  end

  function ZZModMatrix(r::Int, c::Int, R::ZZModRing, arr::AbstractMatrix{T}) where T <: Integer
    ZZModMatrix(r, c, R.ninv, arr)
  end

  function ZZModMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{ZZModRingElem})
    # FIXME: Check compatibility between ctx and arr?
    z = ZZModMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, arr[i, j].data, i, j)
      end
    end
    return z
  end

  function ZZModMatrix(r::Int, c::Int, R::ZZModRing, arr::AbstractMatrix{ZZModRingElem})
    ZZModMatrix(r, c, R, arr)
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
    @ccall libflint.fmpz_mod_mat_init(z::Ref{ZZModMatrix}, nrows(b)::Int, ncols(b)::Int, R.ninv::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_mat_clear_fn, z)
    for i = 1:nrows(b)
      for j = 1:ncols(b)
        setindex_raw!(z, _reduce(b[i,j], R.ninv), i, j)
      end
    end
    return z
  end
end
@flint_fields ZZModMatrix entries = Ptr{ZZRingElem}

###############################################################################
#
#   FpMatrixSpace / FpMatrix
#
###############################################################################

const FpMatrixSpace = AbstractAlgebra.Generic.MatSpace{FpFieldElem}

mutable struct FpMatrix <: FlintMatElem{FpFieldElem}
  cstruct::FlintC.fmpz_mod_mat_struct

  base_ring::FpField
  view_parent

  # MatElem interface
  function FpMatrix(R::FpField, ::UndefInitializer, r::Int, c::Int)
    z = FpMatrix(r, c, R.ninv)
    z.base_ring = R
    return z
  end

  # Used by view, not finalised!!
  function FpMatrix()
    return new()
  end

  function FpMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct)
    z = new()
    @ccall libflint.fmpz_mod_mat_init(z::Ref{FpMatrix}, r::Int, c::Int, ctx::Ref{fmpz_mod_ctx_struct})::Nothing
    finalizer(_fmpz_mod_mat_clear_fn, z)
    return z
  end

  function FpMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{ZZRingElem})
    z = FpMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(arr[i, j], ctx), i, j)
      end
    end
    return z
  end

  function FpMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{T}) where T <: Integer
    z = FpMatrix(r, c, ctx)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, _reduce(ZZRingElem(arr[i, j]), ctx), i, j)
      end
    end
    return z
  end

  function FpMatrix(r::Int, c::Int, ctx::fmpz_mod_ctx_struct, arr::AbstractMatrix{FpFieldElem})
    # FIXME: Check compatibility between ctx and arr?
    z = FpMatrix(r, c, ctx)
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
@flint_fields FpMatrix entries = Ptr{ZZRingElem}

################################################################################
#
#   fpMatrixSpace / fpMatrix
#
###############################################################################

const fpMatrixSpace = AbstractAlgebra.Generic.MatSpace{fpFieldElem}

mutable struct fpMatrix <: FlintMatElem{fpFieldElem}
  cstruct::FlintC.nmod_mat_struct
  base_ring::fpField
  view_parent

  # MatElem interface
  function fpMatrix(R::fpField, ::UndefInitializer, r::Int, c::Int)
    z = fpMatrix(r, c, R)
    z.base_ring = R
    return z
  end

  # Used by view, not finalised!!
  function fpMatrix()
    return new()
  end

  function fpMatrix(r::Int, c::Int, n::Union{UInt, fpField})
    z = new()
    # The entry buffer is Julia-owned; `view_parent` keeps it alive, so no
    # finalizer is needed and FLINT must never be asked to clear this matrix.
    u = zeros(UInt, r*c)
    z.entries = pointer(u)
    z.view_parent = u
    z.r = r
    z.c = c
    z.stride = c
    _set_mod!(z, n)
    return z
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{UInt})
    z = fpMatrix(r, c, n)
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

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{ZZRingElem})
    z = fpMatrix(r, c, n)
    t = ZZRingElem()
    for i = 1:r
      for j = 1:c
        @ccall libflint.fmpz_mod_ui(t::Ref{ZZRingElem}, arr[i, j]::Ref{ZZRingElem}, n::UInt)::Nothing
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
        @ccall libflint.fmpz_mod_ui(t::Ref{ZZRingElem}, (arr[(i - 1) * c + j])::Ref{ZZRingElem}, n::UInt)::Nothing
        setindex!(z, t, i, j)
      end
    end
    return z
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{T}) where {T <: Integer}
    arr_fmpz = map(ZZRingElem, arr)
    return fpMatrix(r, c, n, arr_fmpz)
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractVector{T}) where {T <: Integer}
    arr_fmpz = map(ZZRingElem, arr)
    return fpMatrix(r, c, n, arr_fmpz)
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{fpFieldElem})
    z = fpMatrix(r, c, n)
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
    z = fpMatrix(nrows(b), ncols(b), n)
    @ccall libflint.fmpz_mat_get_nmod_mat(z::Ref{fpMatrix}, b::Ref{ZZMatrix})::Nothing
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
@flint_fields fpMatrix entries = Ptr{UInt}
@flint_legacy_fields fpMatrix begin
  n = cstruct.mod.n
  ninv = cstruct.mod.ninv
  norm = cstruct.mod.norm
end

# Copying the ring's `nmod_t` avoids recomputing `ninv` in `nmod_mat_set_mod`.
function _set_mod!(z::Union{zzModMatrix, fpMatrix}, R::Union{zzModRing, fpField})
  z.n = R.n
  z.ninv = R.ninv
  z.norm = R.norm
  return z
end

function _set_mod!(z::T, n::UInt) where {T <: Union{zzModMatrix, fpMatrix}}
  @ccall libflint.nmod_mat_set_mod(z::Ref{T}, n::UInt)::Nothing
  return z
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
  cstruct::FlintC.fq_default_poly_struct

  parent::FqPolyRing

  function FqPolyRingElem(ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_init(z::Ref{FqPolyRingElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::FqPolyRingElem, ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_init(z::Ref{FqPolyRingElem}, ctx::Ref{FqField})::Nothing
    @ccall libflint.fq_default_poly_set(z::Ref{FqPolyRingElem}, a::Ref{FqPolyRingElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::FqFieldElem, ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_init(z::Ref{FqPolyRingElem}, ctx::Ref{FqField})::Nothing
    @ccall libflint.fq_default_poly_set_fq_default(z::Ref{FqPolyRingElem}, a::Ref{FqFieldElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::Vector{FqFieldElem}, ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_init2(z::Ref{FqPolyRingElem}, length(a)::Int, ctx::Ref{FqField})::Nothing
    for i = 1:length(a)
      @ccall libflint.fq_default_poly_set_coeff(z::Ref{FqPolyRingElem}, (i - 1)::Int, a[i]::Ref{FqFieldElem}, ctx::Ref{FqField})::Nothing
    end
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::Vector{ZZRingElem}, ctx::FqField)
    z = new()
    temp = ctx()
    @ccall libflint.fq_default_poly_init2(z::Ref{FqPolyRingElem}, length(a)::Int, ctx::Ref{FqField})::Nothing
    for i = 1:length(a)
      temp = ctx(a[i])
      @ccall libflint.fq_default_poly_set_coeff(z::Ref{FqPolyRingElem}, (i - 1)::Int, temp::Ref{FqFieldElem}, ctx::Ref{FqField})::Nothing
    end
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::ZZPolyRingElem, ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_init2(z::Ref{FqPolyRingElem}, length(a)::Int, ctx::Ref{FqField})::Nothing
    @ccall libflint.fq_default_poly_set_fmpz_poly(z::Ref{FqPolyRingElem}, a::Ref{ZZPolyRingElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::zzModPolyRingElem, ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_init2(z::Ref{FqPolyRingElem}, length(a)::Int, ctx::Ref{FqField})::Nothing
    @ccall libflint.fq_default_poly_set_nmod_poly(z::Ref{FqPolyRingElem}, a::Ref{zzModPolyRingElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::fpPolyRingElem, ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_init2(z::Ref{FqPolyRingElem}, length(a)::Int, ctx::Ref{FqField})::Nothing
    @ccall libflint.fq_default_poly_set_nmod_poly(z::Ref{FqPolyRingElem}, a::Ref{fpPolyRingElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::ZZModPolyRingElem, ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_init2(z::Ref{FqPolyRingElem}, length(a)::Int, ctx::Ref{FqField})::Nothing
    @ccall libflint.fq_default_poly_set_fmpz_mod_poly(z::Ref{FqPolyRingElem}, a::Ref{ZZModPolyRingElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end

  function FqPolyRingElem(a::FpPolyRingElem, ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_init2(z::Ref{FqPolyRingElem}, length(a)::Int, ctx::Ref{FqField})::Nothing
    @ccall libflint.fq_default_poly_set_fmpz_mod_poly(z::Ref{FqPolyRingElem}, a::Ref{FpPolyRingElem}, ctx::Ref{FqField})::Nothing
    finalizer(_fq_default_poly_clear_fn, z)
    return z
  end
end
@flint_fields FqPolyRingElem

mutable struct fq_default_poly_factor
  # fq_default_ctx_struct is 32 bytes on 64 bit machine
  cstruct::FlintC.fq_default_poly_factor_struct
  base_field::FqField

  function fq_default_poly_factor(ctx::FqField)
    z = new()
    @ccall libflint.fq_default_poly_factor_init(z::Ref{fq_default_poly_factor}, ctx::Ref{FqField})::Nothing
    z.base_field = ctx
    finalizer(_fq_default_poly_factor_clear_fn, z)
    return z
  end
end
@flint_fields fq_default_poly_factor

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
  cstruct::FlintC.fq_poly_struct
  parent::FqPolyRepPolyRing

  function FqPolyRepPolyRingElem()
    z = new()
    @ccall libflint.fq_poly_init(z::Ref{FqPolyRepPolyRingElem})::Nothing
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepPolyRingElem(a::FqPolyRepPolyRingElem)
    z = new()
    ctx = base_ring(parent(a))
    @ccall libflint.fq_poly_init(z::Ref{FqPolyRepPolyRingElem}, ctx::Ref{FqPolyRepField})::Nothing
    @ccall libflint.fq_poly_set(z::Ref{FqPolyRepPolyRingElem}, a::Ref{FqPolyRepPolyRingElem}, ctx::Ref{FqPolyRepField})::Nothing
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepPolyRingElem(a::FqPolyRepFieldElem)
    z = new()
    ctx = parent(a)
    @ccall libflint.fq_poly_init(z::Ref{FqPolyRepPolyRingElem}, ctx::Ref{FqPolyRepField})::Nothing
    @ccall libflint.fq_poly_set_fq(z::Ref{FqPolyRepPolyRingElem}, a::Ref{FqPolyRepFieldElem}, ctx::Ref{FqPolyRepField})::Nothing
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepPolyRingElem(a::Vector{FqPolyRepFieldElem})
    z = new()
    @req allequal(map(parent, a)) "parents do not match"
    ctx = parent(a[1])
    @ccall libflint.fq_poly_init2(z::Ref{FqPolyRepPolyRingElem}, length(a)::Int, ctx::Ref{FqPolyRepField})::Nothing
    for i = 1:length(a)
      @ccall libflint.fq_poly_set_coeff(z::Ref{FqPolyRepPolyRingElem}, (i - 1)::Int, a[i]::Ref{FqPolyRepFieldElem}, ctx::Ref{FqPolyRepField})::Nothing
    end
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepPolyRingElem(a::Vector{ZZRingElem}, ctx::FqPolyRepField)
    z = new()
    temp = ctx()
    @ccall libflint.fq_poly_init2(z::Ref{FqPolyRepPolyRingElem}, length(a)::Int, ctx::Ref{FqPolyRepField})::Nothing
    for i = 1:length(a)
      temp = ctx(a[i])
      @ccall libflint.fq_poly_set_coeff(z::Ref{FqPolyRepPolyRingElem}, (i - 1)::Int, temp::Ref{FqPolyRepFieldElem}, ctx::Ref{FqPolyRepField})::Nothing
    end
    finalizer(_fq_poly_clear_fn, z)
    return z
  end

  function FqPolyRepPolyRingElem(a::ZZPolyRingElem, ctx::FqPolyRepField)
    z = new()
    @ccall libflint.fq_poly_init2(z::Ref{FqPolyRepPolyRingElem}, length(a)::Int, ctx::Ref{FqPolyRepField})::Nothing
    for i = 1:length(a)
      temp = ctx(coeff(a, i-1))
      @ccall libflint.fq_poly_set_coeff(z::Ref{FqPolyRepPolyRingElem}, (i - 1)::Int, temp::Ref{FqPolyRepFieldElem}, ctx::Ref{FqPolyRepField})::Nothing
    end
    finalizer(_fq_poly_clear_fn, z)
    return z
  end
end
@flint_fields FqPolyRepPolyRingElem

mutable struct fq_poly_factor
  cstruct::FlintC.fq_poly_factor_struct
  base_field::FqPolyRepField

  function fq_poly_factor(ctx::FqPolyRepField)
    z = new()
    @ccall libflint.fq_poly_factor_init(z::Ref{fq_poly_factor}, ctx::Ref{FqPolyRepField})::Nothing
    z.base_field = ctx
    finalizer(_fq_poly_factor_clear_fn, z)
    return z
  end
end
@flint_fields fq_poly_factor poly = Ptr{FqPolyRepPolyRingElem}

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
  cstruct::FlintC.fq_nmod_poly_struct
  parent::fqPolyRepPolyRing

  function fqPolyRepPolyRingElem()
    z = new()
    @ccall libflint.fq_nmod_poly_init(z::Ref{fqPolyRepPolyRingElem})::Nothing
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepPolyRingElem(a::fqPolyRepPolyRingElem)
    z = new()
    ctx = base_ring(parent(a))
    @ccall libflint.fq_nmod_poly_init(z::Ref{fqPolyRepPolyRingElem}, ctx::Ref{fqPolyRepField})::Nothing
    @ccall libflint.fq_nmod_poly_set(z::Ref{fqPolyRepPolyRingElem}, a::Ref{fqPolyRepPolyRingElem}, ctx::Ref{fqPolyRepField})::Nothing
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepPolyRingElem(a::fqPolyRepFieldElem)
    z = new()
    ctx = parent(a)
    @ccall libflint.fq_nmod_poly_init(z::Ref{fqPolyRepPolyRingElem}, ctx::Ref{fqPolyRepField})::Nothing
    @ccall libflint.fq_nmod_poly_set_fq_nmod(z::Ref{fqPolyRepPolyRingElem}, a::Ref{fqPolyRepFieldElem}, ctx::Ref{fqPolyRepField})::Nothing
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepPolyRingElem(a::Vector{fqPolyRepFieldElem})
    z = new()
    @req allequal(map(parent, a)) "parents do not match"
    ctx = parent(a[1])
    @ccall libflint.fq_nmod_poly_init2(z::Ref{fqPolyRepPolyRingElem}, length(a)::Int, ctx::Ref{fqPolyRepField})::Nothing
    for i = 1:length(a)
      @ccall libflint.fq_nmod_poly_set_coeff(z::Ref{fqPolyRepPolyRingElem}, (i - 1)::Int, a[i]::Ref{fqPolyRepFieldElem}, ctx::Ref{fqPolyRepField})::Nothing
    end
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepPolyRingElem(a::Vector{ZZRingElem}, ctx::fqPolyRepField)
    z = new()
    temp = ctx()
    @ccall libflint.fq_nmod_poly_init2(z::Ref{fqPolyRepPolyRingElem}, length(a)::Int, ctx::Ref{fqPolyRepField})::Nothing
    for i = 1:length(a)
      temp = ctx(a[i])
      @ccall libflint.fq_nmod_poly_set_coeff(z::Ref{fqPolyRepPolyRingElem}, (i - 1)::Int, temp::Ref{fqPolyRepFieldElem}, ctx::Ref{fqPolyRepField})::Nothing
    end
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end

  function fqPolyRepPolyRingElem(a::ZZPolyRingElem, ctx::fqPolyRepField)
    z = new()
    @ccall libflint.fq_nmod_poly_init2(z::Ref{fqPolyRepPolyRingElem}, length(a)::Int, ctx::Ref{fqPolyRepField})::Nothing
    for i = 1:length(a)
      temp = ctx(coeff(a,i-1))
      @ccall libflint.fq_nmod_poly_set_coeff(z::Ref{fqPolyRepPolyRingElem}, (i - 1)::Int, temp::Ref{fqPolyRepFieldElem}, ctx::Ref{fqPolyRepField})::Nothing
    end
    finalizer(_fq_nmod_poly_clear_fn, z)
    return z
  end
end
@flint_fields fqPolyRepPolyRingElem

mutable struct fq_nmod_poly_factor
  cstruct::FlintC.fq_nmod_poly_factor_struct
  base_field::fqPolyRepField

  function fq_nmod_poly_factor(ctx::fqPolyRepField)
    z = new()
    @ccall libflint.fq_nmod_poly_factor_init(z::Ref{fq_nmod_poly_factor}, ctx::Ref{fqPolyRepField})::Nothing
    z.base_field = ctx
    finalizer(_fq_nmod_poly_factor_clear_fn, z)
    return z
  end
end
@flint_fields fq_nmod_poly_factor poly = Ptr{fqPolyRepPolyRingElem}

###############################################################################
#
#   FqMatrixSpace/FqMatrix
#
###############################################################################

const FqMatrixSpace = AbstractAlgebra.Generic.MatSpace{FqFieldElem}

mutable struct FqMatrix <: FlintMatElem{FqFieldElem}
  # fq_default_mat_struct is a union of FLINT matrix structs. They all start
  # with these four fields; nmod_mat_struct, the largest, has three more words.
  cstruct::FlintC.fq_default_mat_struct

  base_ring::FqField
  view_parent

  # MatElem interface
  function FqMatrix(R::FqField, ::UndefInitializer, r::Int, c::Int)
    return FqMatrix(r, c, R)
  end

  # Used by view, not finalised!!
  function FqMatrix()
    return new()
  end

  function FqMatrix(r::Int, c::Int, ctx::FqField)
    z = new()
    @ccall libflint.fq_default_mat_init(z::Ref{FqMatrix}, r::Int, c::Int, ctx::Ref{FqField})::Nothing
    z.base_ring = ctx
    finalizer(_fq_default_mat_clear_fn, z)
    return z
  end

  function FqMatrix(arr::AbstractMatrix{<:Union{FqFieldElem,ZZRingElem,Integer}}, ctx::FqField)
    (r, c) = size(arr)
    z = FqMatrix(r, c, ctx)
    for i = 1:r, j = 1:c
      @inbounds z[i, j] = arr[i, j]
    end
    return z
  end

  function FqMatrix(r::Int, c::Int, arr::AbstractVector{<:Union{FqFieldElem,ZZRingElem,Integer}}, ctx::FqField)
    z = FqMatrix(r, c, ctx)
    for i = 1:r, j = 1:c
      el = arr[(i - 1) * c + j]
      @inbounds z[i, j] = el
    end
    return z
  end

  function FqMatrix(r::Int, c::Int, d::FqFieldElem)
    ctx = parent(d)
    z = FqMatrix(r, c, ctx)
    for i = 1:min(r, c)
      @inbounds z[i, i] = d
    end
    return z
  end

  function FqMatrix(m::ZZMatrix, ctx::FqField)
    r = nrows(m)
    c = ncols(m)
    z = FqMatrix(r, c, ctx)
    @ccall libflint.fq_default_mat_set_fmpz_mat(z::Ref{FqMatrix}, m::Ref{ZZMatrix}, ctx::Ref{FqField})::Nothing
    return z
  end

  function FqMatrix(m::ZZModMatrix, ctx::FqField)
    r = nrows(m)
    c = ncols(m)
    z = FqMatrix(r, c, ctx)
    @ccall libflint.fq_default_mat_set_fmpz_mod_mat(z::Ref{FqMatrix}, m::Ref{ZZModMatrix}, ctx::Ref{FqField})::Nothing
    return z
  end

  function FqMatrix(m::FpMatrix, ctx::FqField)
    r = nrows(m)
    c = ncols(m)
    z = FqMatrix(r, c, ctx)
    @ccall libflint.fq_default_mat_set_fmpz_mod_mat(z::Ref{FqMatrix}, m::Ref{FpMatrix}, ctx::Ref{FqField})::Nothing
    return z
  end

  function FqMatrix(m::zzModMatrix, ctx::FqField)
    r = nrows(m)
    c = ncols(m)
    z = FqMatrix(r, c, ctx)
    @ccall libflint.fq_default_mat_set_nmod_mat(z::Ref{FqMatrix}, m::Ref{zzModMatrix}, ctx::Ref{FqField})::Nothing
    return z
  end

  function FqMatrix(m::fpMatrix, ctx::FqField)
    r = nrows(m)
    c = ncols(m)
    z = FqMatrix(r, c, ctx)
    @ccall libflint.fq_default_mat_set_nmod_mat(z::Ref{FqMatrix}, m::Ref{fpMatrix}, ctx::Ref{FqField})::Nothing
    return z
  end
end
@flint_fields FqMatrix
@flint_extra_fields FqMatrix begin
  entries = Ptr{FqFieldElem}(_union_word(cstruct, 1))
  entries! = _set_union_word(cstruct, 1, v)
  r = _union_word(cstruct, 2) % Int
  r! = _set_union_word(cstruct, 2, v)
  c = _union_word(cstruct, 3) % Int
  c! = _set_union_word(cstruct, 3, v)
  stride = _union_word(cstruct, 4) % Int
  stride! = _set_union_word(cstruct, 4, v)
end

###############################################################################
#
#   FqPolyRepMatrixSpace/FqPolyRepMatrix
#
###############################################################################

const FqPolyRepMatrixSpace = AbstractAlgebra.Generic.MatSpace{FqPolyRepFieldElem}

mutable struct FqPolyRepMatrix <: FlintMatElem{FqPolyRepFieldElem}
  cstruct::FlintC.fq_mat_struct
  base_ring::FqPolyRepField
  view_parent

  # MatElem interface
  function FqPolyRepMatrix(R::FqPolyRepField, ::UndefInitializer, r::Int, c::Int)
    return FqPolyRepMatrix(r, c, R)
  end

  # Used by view, not finalised!!
  function FqPolyRepMatrix()
    return new()
  end

  function FqPolyRepMatrix(r::Int, c::Int, ctx::FqPolyRepField)
    z = new()
    @ccall libflint.fq_mat_init(z::Ref{FqPolyRepMatrix}, r::Int, c::Int, ctx::Ref{FqPolyRepField})::Nothing
    z.base_ring = ctx
    finalizer(_fq_mat_clear_fn, z)
    return z
  end

  function FqPolyRepMatrix(arr::AbstractMatrix{<:Union{FqPolyRepFieldElem,ZZRingElem,Integer}}, ctx::FqPolyRepField)
    (r, c) = size(arr)
    z = FqPolyRepMatrix(r, c, ctx)
    for i = 1:r, j = 1:c
      @inbounds z[i, j] = arr[i, j]
    end
    return z
  end

  function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{<:Union{FqPolyRepFieldElem,ZZRingElem,Integer}}, ctx::FqPolyRepField)
    z = FqPolyRepMatrix(r, c, ctx)
    for i = 1:r, j = 1:c
      el = arr[(i - 1) * c + j]
      @inbounds z[i, j] = el
    end
    return z
  end

  function FqPolyRepMatrix(r::Int, c::Int, d::FqPolyRepFieldElem)
    ctx = parent(d)
    z = FqPolyRepMatrix(r, c, ctx)
    for i = 1:min(r, c)
      @inbounds z[i, i] = d
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

        @ccall libflint.fq_set_fmpz(el1::Ptr{FqPolyRepFieldElem}, el2::Ptr{ZZRingElem}, ctx::Ref{FqPolyRepField})::Nothing
      end
    end
    return z
  end
end
@flint_fields FqPolyRepMatrix entries = Ptr{FqPolyRepFieldElem}

###############################################################################
#
#   fqPolyRepMatrixSpace/fqPolyRepMatrix
#
###############################################################################

const fqPolyRepMatrixSpace = AbstractAlgebra.Generic.MatSpace{fqPolyRepFieldElem}

mutable struct fqPolyRepMatrix <: FlintMatElem{fqPolyRepFieldElem}
  cstruct::FlintC.fq_nmod_mat_struct
  base_ring::fqPolyRepField
  view_parent

  # MatElem interface
  function fqPolyRepMatrix(R::fqPolyRepField, ::UndefInitializer, r::Int, c::Int)
    return fqPolyRepMatrix(r, c, R)
  end

  # Used by view, not finalised!!
  function fqPolyRepMatrix()
    return new()
  end

  function fqPolyRepMatrix(r::Int, c::Int, ctx::fqPolyRepField)
    z = new()
    @ccall libflint.fq_nmod_mat_init(z::Ref{fqPolyRepMatrix}, r::Int, c::Int, ctx::Ref{fqPolyRepField})::Nothing
    z.base_ring = ctx
    finalizer(_fq_nmod_mat_clear_fn, z)
    return z
  end

  function fqPolyRepMatrix(arr::AbstractMatrix{<:Union{fqPolyRepFieldElem,ZZRingElem,Integer}}, ctx::fqPolyRepField)
    (r, c) = size(arr)
    z = fqPolyRepMatrix(r, c, ctx)
    for i = 1:r, j = 1:c
      @inbounds z[i, j] = arr[i, j]
    end
    return z
  end

  function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{<:Union{fqPolyRepFieldElem,ZZRingElem,Integer}}, ctx::fqPolyRepField)
    z = fqPolyRepMatrix(r, c, ctx)
    for i = 1:r, j = 1:c
      el = arr[(i - 1) * c + j]
      @inbounds z[i, j] = el
    end
    return z
  end

  function fqPolyRepMatrix(r::Int, c::Int, d::fqPolyRepFieldElem)
    ctx = parent(d)
    z = fqPolyRepMatrix(r, c, ctx)
    for i = 1:min(r, c)
      @inbounds z[i, i] = d
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

        @ccall libflint.fq_nmod_set_fmpz(el1::Ptr{fqPolyRepFieldElem}, el2::Ptr{ZZRingElem}, ctx::Ref{fqPolyRepField})::Nothing
      end
    end
    return z
  end
end
@flint_fields fqPolyRepMatrix entries = Ptr{fqPolyRepFieldElem}

################################################################################
#
#   Rand state
#
################################################################################

mutable struct rand_ctx
  cstruct::FlintC.flint_rand_struct

  function rand_ctx()
    a = new()
    @ccall libflint.flint_rand_init(a::Ref{rand_ctx})::Cvoid
    finalizer(_rand_ctx_clear_fn, a)
    return a
  end
end
@flint_fields rand_ctx
@flint_legacy_fields rand_ctx begin
  gmp_state = cstruct.__gmp_state
  randval = cstruct.__randval
  randval2 = cstruct.__randval2
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
        mm = change_base_ring(Fq, lift(polynomial_ring(ZZ, "x")[1], m); cached = false)
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
#   Finalizer
#
################################################################################

function _fmpq_clear_fn(a::QQFieldElem)
  @ccall libflint.fmpq_clear(a::Ref{QQFieldElem})::Nothing
end

function _fmpq_mat_clear_fn(a::QQMatrix)
  @ccall libflint.fmpq_mat_clear(a::Ref{QQMatrix})::Nothing
end

function _fmpq_mpoly_clear_fn(a::QQMPolyRingElem)
  @ccall libflint.fmpq_mpoly_clear(a::Ref{QQMPolyRingElem}, parent(a)::Ref{QQMPolyRing})::Nothing
end

function _fmpq_mpoly_ctx_clear_fn(a::QQMPolyRing)
  @ccall libflint.fmpq_mpoly_ctx_clear(a::Ref{QQMPolyRing})::Nothing
end

function _fmpq_mpoly_factor_clear_fn(f::fmpq_mpoly_factor)
  @ccall libflint.fmpq_mpoly_factor_clear(f::Ref{fmpq_mpoly_factor}, f.parent::Ref{QQMPolyRing})::Nothing
end

function _fmpq_poly_clear_fn(a::T) where T <: Union{QQPolyRingElem, QQRelPowerSeriesRingElem, QQAbsPowerSeriesRingElem}
  @ccall libflint.fmpq_poly_clear(a::Ref{T})::Nothing
end

function _fmpz_clear_fn(a::ZZRingElem)
  @ccall libflint.fmpz_clear(a::Ref{ZZRingElem})::Nothing
end

function _fmpz_factor_clear_fn(f::fmpz_factor)
  @ccall libflint.fmpz_factor_clear(f::Ref{fmpz_factor})::Nothing
end

function _fmpz_mat_clear_fn(a::ZZMatrix)
  @ccall libflint.fmpz_mat_clear(a::Ref{ZZMatrix})::Nothing
end

function _fmpz_poly_mat_clear_fn(a::ZZPolyRingMatrix)
  @ccall libflint.fmpz_poly_mat_clear(a::Ref{ZZPolyRingMatrix})::Nothing
end

function _fmpz_mod_ctx_clear_fn(a::fmpz_mod_ctx_struct)
  @ccall libflint.fmpz_mod_ctx_clear(a::Ref{fmpz_mod_ctx_struct})::Nothing
end

function _fmpz_mod_mat_clear_fn(mat::T) where T <: Union{ZZModMatrix, FpMatrix}
  @ccall libflint.fmpz_mod_mat_clear(mat::Ref{T}, C_NULL::Ref{Nothing})::Nothing
end

function _fmpz_mod_mpoly_clear_fn(a::FpMPolyRingElem)
  @ccall libflint.fmpz_mod_mpoly_clear(a::Ref{FpMPolyRingElem}, parent(a)::Ref{FpMPolyRing})::Nothing
end

function _fmpz_mod_mpoly_ctx_clear_fn(a::FpMPolyRing)
  @ccall libflint.fmpz_mod_mpoly_ctx_clear(a::Ref{FpMPolyRing})::Nothing
end

function _fmpz_mod_mpoly_factor_clear_fn(f::gfp_fmpz_mpoly_factor)
  @ccall libflint.fmpz_mod_mpoly_factor_clear(f::Ref{gfp_fmpz_mpoly_factor}, f.parent::Ref{FpMPolyRing})::Nothing
end

function _fmpz_mod_poly_clear_fn(x::T) where T <: Union{ZZModPolyRingElem, FpPolyRingElem, ZZModRelPowerSeriesRingElem, FpRelPowerSeriesRingElem, ZZModAbsPowerSeriesRingElem, FpAbsPowerSeriesRingElem}
  @ccall libflint.fmpz_mod_poly_clear(x::Ref{T}, (base_ring(parent(x))).ninv::Ref{fmpz_mod_ctx_struct})::Nothing
end

function _fmpz_mod_poly_factor_clear_fn(f::T) where T <: Union{fmpz_mod_poly_factor, gfp_fmpz_poly_factor}
  @ccall libflint.fmpz_mod_poly_factor_clear(f::Ref{T}, f.n::Ref{fmpz_mod_ctx_struct})::Nothing
end

function _fmpz_mpoly_clear_fn(a::ZZMPolyRingElem)
  @ccall libflint.fmpz_mpoly_clear(a::Ref{ZZMPolyRingElem}, parent(a)::Ref{ZZMPolyRing})::Nothing
end

function _fmpz_mpoly_ctx_clear_fn(a::ZZMPolyRing)
  @ccall libflint.fmpz_mpoly_ctx_clear(a::Ref{ZZMPolyRing})::Nothing
end

function _fmpz_mpoly_factor_clear_fn(f::fmpz_mpoly_factor)
  @ccall libflint.fmpz_mpoly_factor_clear(f::Ref{fmpz_mpoly_factor}, f.parent::Ref{ZZMPolyRing})::Nothing
end

function _fmpz_poly_clear_fn(a::T) where T <: Union{ZZPolyRingElem, ZZRelPowerSeriesRingElem, ZZAbsPowerSeriesRingElem, ZZLaurentSeriesRingElem}
  @ccall libflint.fmpz_poly_clear(a::Ref{T})::Nothing
end

function _fmpz_poly_factor_clear_fn(f::fmpz_poly_factor)
  @ccall libflint.fmpz_poly_factor_clear(f::Ref{fmpz_poly_factor})::Nothing
end

function _fq_clear_fn(a::FqPolyRepFieldElem)
  @ccall libflint.fq_clear(a::Ref{FqPolyRepFieldElem}, parent(a)::Ref{FqPolyRepField})::Nothing
end

function _fq_ctx_clear_fn(a::FqPolyRepField)
  @ccall libflint.fq_ctx_clear(a::Ref{FqPolyRepField})::Nothing
end

function _fq_default_clear_fn(a::FqFieldElem)
  @ccall libflint.fq_default_clear(a::Ref{FqFieldElem}, parent(a)::Ref{FqField})::Nothing
end

function _fq_default_ctx_clear_fn(a::FqField)
  @ccall libflint.fq_default_ctx_clear(a::Ref{FqField})::Nothing
end

function _fq_default_mat_clear_fn(a::FqMatrix)
  @ccall libflint.fq_default_mat_clear(a::Ref{FqMatrix}, base_ring(a)::Ref{FqField})::Nothing
end

function _fq_default_poly_clear_fn(a::T) where T <: Union{FqPolyRingElem, FqRelPowerSeriesRingElem, FqAbsPowerSeriesRingElem}
  @ccall libflint.fq_default_poly_clear(a::Ref{T}, base_ring(a)::Ref{FqField})::Nothing
end

function _fq_default_poly_factor_clear_fn(f::fq_default_poly_factor)
  @ccall libflint.fq_default_poly_factor_clear(f::Ref{fq_default_poly_factor}, f.base_field::Ref{FqField})::Nothing
end

function _fq_mat_clear_fn(a::FqPolyRepMatrix)
  @ccall libflint.fq_mat_clear(a::Ref{FqPolyRepMatrix}, base_ring(a)::Ref{FqPolyRepField})::Nothing
end

function _fq_nmod_clear_fn(a::fqPolyRepFieldElem)
  @ccall libflint.fq_nmod_clear(a::Ref{fqPolyRepFieldElem}, parent(a)::Ref{fqPolyRepField})::Nothing
end

function _fq_nmod_ctx_clear_fn(a::fqPolyRepField)
  @ccall libflint.fq_nmod_ctx_clear(a::Ref{fqPolyRepField})::Nothing
end

function _fq_nmod_mat_clear_fn(a::fqPolyRepMatrix)
  @ccall libflint.fq_nmod_mat_clear(a::Ref{fqPolyRepMatrix}, base_ring(a)::Ref{fqPolyRepField})::Nothing
end

function _fq_nmod_mpoly_clear_fn(a::fqPolyRepMPolyRingElem)
  @ccall libflint.fq_nmod_mpoly_clear(a::Ref{fqPolyRepMPolyRingElem}, parent(a)::Ref{fqPolyRepMPolyRing})::Nothing
end

function _fq_nmod_mpoly_ctx_clear_fn(a::fqPolyRepMPolyRing)
  @ccall libflint.fq_nmod_mpoly_ctx_clear(a::Ref{fqPolyRepMPolyRing})::Nothing
end

function _fq_nmod_mpoly_factor_clear_fn(f::fq_nmod_mpoly_factor)
  @ccall libflint.fq_nmod_mpoly_factor_clear(f::Ref{fq_nmod_mpoly_factor}, f.parent::Ref{fqPolyRepMPolyRing})::Nothing
end

function _fq_nmod_poly_clear_fn(a::T) where T <: Union{fqPolyRepPolyRingElem, fqPolyRepRelPowerSeriesRingElem, fqPolyRepAbsPowerSeriesRingElem}
  @ccall libflint.fq_nmod_poly_clear(a::Ref{T}, base_ring(a)::Ref{fqPolyRepField})::Nothing
end

function _fq_nmod_poly_factor_clear_fn(f::fq_nmod_poly_factor)
  @ccall libflint.fq_nmod_poly_factor_clear(f::Ref{fq_nmod_poly_factor}, f.base_field::Ref{fqPolyRepField})::Nothing
end

function _fq_poly_clear_fn(a::T) where T <: Union{FqPolyRepPolyRingElem, FqPolyRepRelPowerSeriesRingElem, FqPolyRepAbsPowerSeriesRingElem}
  @ccall libflint.fq_poly_clear(a::Ref{T}, base_ring(a)::Ref{FqPolyRepField})::Nothing
end

function _fq_poly_factor_clear_fn(f::fq_poly_factor)
  @ccall libflint.fq_poly_factor_clear(f::Ref{fq_poly_factor}, f.base_field::Ref{FqPolyRepField})::Nothing
end

function _nmod_mpoly_clear_fn(a::zzModMPolyRingElem)
  @ccall libflint.nmod_mpoly_clear(a::Ref{zzModMPolyRingElem}, parent(a)::Ref{zzModMPolyRing})::Nothing
end

function _nmod_mpoly_clear_fn(a::fpMPolyRingElem)
  @ccall libflint.nmod_mpoly_clear(a::Ref{fpMPolyRingElem}, parent(a)::Ref{fpMPolyRing})::Nothing
end

function _nmod_mpoly_ctx_clear_fn(a::T) where T <: Union{zzModMPolyRing, fpMPolyRing}
  @ccall libflint.nmod_mpoly_ctx_clear(a::Ref{T})::Nothing
end

function _nmod_mpoly_factor_clear_fn(f::nmod_mpoly_factor)
  @ccall libflint.nmod_mpoly_factor_clear(f::Ref{nmod_mpoly_factor}, f.parent::Ref{zzModMPolyRing})::Nothing
end

function _nmod_mpoly_factor_clear_fn(f::gfp_mpoly_factor)
  @ccall libflint.nmod_mpoly_factor_clear(f::Ref{gfp_mpoly_factor}, f.parent::Ref{fpMPolyRing})::Nothing
end

function _nmod_poly_clear_fn(x::T) where T <: Union{zzModPolyRingElem, fpPolyRingElem, zzModRelPowerSeriesRingElem, fpRelPowerSeriesRingElem, zzModAbsPowerSeriesRingElem, fpAbsPowerSeriesRingElem}
  @ccall libflint.nmod_poly_clear(x::Ref{T})::Nothing
end

function _nmod_poly_factor_clear_fn(f::T) where T <: Union{nmod_poly_factor, gfp_poly_factor}
  @ccall libflint.nmod_poly_factor_clear(f::Ref{T})::Nothing
end

function _padic_clear_fn(a::PadicFieldElem)
  @ccall libflint.padic_clear(a::Ref{PadicFieldElem})::Nothing
end

function _padic_ctx_clear_fn(a::PadicField)
  @ccall libflint.padic_ctx_clear(a::Ref{PadicField})::Nothing
end

function _qadic_clear_fn(a::QadicFieldElem)
  @ccall libflint.qadic_clear(a::Ref{QadicFieldElem})::Nothing
end

function _qadic_ctx_clear_fn(a::QadicField)
  @ccall libflint.qadic_ctx_clear(a::Ref{QadicField})::Nothing
end

function _qadic_char2_sqrt_precomp_clear_fn(a::Qadic2SqrtPrecomp)
  @ccall libflint._qadic_char2_sqrt_precomp_clear(a.ptr::Ptr{FlintC.struct_qadic2_sqrt_precomp})::Nothing
end

function _rand_ctx_clear_fn(a::rand_ctx)
  @ccall libflint.flint_rand_clear(a::Ref{rand_ctx})::Nothing
end

################################################################################
#
#  More type unions
#
################################################################################

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


const ZZRingElemOrPtr = TypeOrPtr{ZZRingElem}
const QQFieldElemOrPtr = TypeOrPtr{QQFieldElem}
const zzModRingElemOrPtr = TypeOrPtr{zzModRingElem}
const ZZModRingElemOrPtr = TypeOrPtr{ZZModRingElem}
const fpFieldElemOrPtr = TypeOrPtr{fpFieldElem}
const FpFieldElemOrPtr = TypeOrPtr{FpFieldElem}
const fqPolyRepFieldElemOrPtr = TypeOrPtr{fqPolyRepFieldElem}
const FqPolyRepFieldElemOrPtr = TypeOrPtr{FqPolyRepFieldElem}
const FqFieldElemOrPtr = TypeOrPtr{FqFieldElem}

const ZZPolyRingElemOrPtr = TypeOrPtr{ZZPolyRingElem}
const QQPolyRingElemOrPtr = TypeOrPtr{QQPolyRingElem}
const zzModPolyRingElemOrPtr = TypeOrPtr{zzModPolyRingElem}
const ZZModPolyRingElemOrPtr = TypeOrPtr{ZZModPolyRingElem}
const fpPolyRingElemOrPtr = TypeOrPtr{fpPolyRingElem}
const FpPolyRingElemOrPtr = TypeOrPtr{FpPolyRingElem}
const fqPolyRepPolyRingElemOrPtr = TypeOrPtr{fqPolyRepPolyRingElem}
const FqPolyRepPolyRingElemOrPtr = TypeOrPtr{FqPolyRepPolyRingElem}
const FqPolyRingElemOrPtr = TypeOrPtr{FqPolyRingElem}

const ZZMPolyRingElemOrPtr = TypeOrPtr{ZZMPolyRingElem}
const QQMPolyRingElemOrPtr = TypeOrPtr{QQMPolyRingElem}
const zzModMPolyRingElemOrPtr = TypeOrPtr{zzModMPolyRingElem}
# ZZModMPolyRingElem does not exits
const fpMPolyRingElemOrPtr = TypeOrPtr{fpMPolyRingElem}
const FpMPolyRingElemOrPtr = TypeOrPtr{FpMPolyRingElem}
const fqPolyRepMPolyRingElemOrPtr = TypeOrPtr{fqPolyRepMPolyRingElem}
# FqPolyRepMPolyRingElem does not exits
const FqMPolyRingElemOrPtr = TypeOrPtr{FqMPolyRingElem}

const ZZRelPowerSeriesRingElemOrPtr = TypeOrPtr{ZZRelPowerSeriesRingElem}
const ZZAbsPowerSeriesRingElemOrPtr = TypeOrPtr{ZZAbsPowerSeriesRingElem}
const QQRelPowerSeriesRingElemOrPtr = TypeOrPtr{QQRelPowerSeriesRingElem}
const QQAbsPowerSeriesRingElemOrPtr = TypeOrPtr{QQAbsPowerSeriesRingElem}
const zzModRelPowerSeriesRingElemOrPtr = TypeOrPtr{zzModRelPowerSeriesRingElem}
const zzModAbsPowerSeriesRingElemOrPtr = TypeOrPtr{zzModAbsPowerSeriesRingElem}
const ZZModRelPowerSeriesRingElemOrPtr = TypeOrPtr{ZZModRelPowerSeriesRingElem}
const ZZModAbsPowerSeriesRingElemOrPtr = TypeOrPtr{ZZModAbsPowerSeriesRingElem}
const fpRelPowerSeriesRingElemOrPtr = TypeOrPtr{fpRelPowerSeriesRingElem}
const fpAbsPowerSeriesRingElemOrPtr = TypeOrPtr{fpAbsPowerSeriesRingElem}
const FpRelPowerSeriesRingElemOrPtr = TypeOrPtr{FpRelPowerSeriesRingElem}
const FpAbsPowerSeriesRingElemOrPtr = TypeOrPtr{FpAbsPowerSeriesRingElem}
const fqPolyRepRelPowerSeriesRingElemOrPtr = TypeOrPtr{fqPolyRepRelPowerSeriesRingElem}
const fqPolyRepAbsPowerSeriesRingElemOrPtr = TypeOrPtr{fqPolyRepAbsPowerSeriesRingElem}
const FqPolyRepRelPowerSeriesRingElemOrPtr = TypeOrPtr{FqPolyRepRelPowerSeriesRingElem}
const FqPolyRepAbsPowerSeriesRingElemOrPtr = TypeOrPtr{FqPolyRepAbsPowerSeriesRingElem}
const FqRelPowerSeriesRingElemOrPtr = TypeOrPtr{FqRelPowerSeriesRingElem}
const FqAbsPowerSeriesRingElemOrPtr = TypeOrPtr{FqAbsPowerSeriesRingElem}

const ZZMatrixOrPtr = TypeOrPtr{ZZMatrix}
const QQMatrixOrPtr = TypeOrPtr{QQMatrix}
const ZZPolyRingMatrixOrPtr = TypeOrPtr{ZZPolyRingMatrix}
const zzModMatrixOrPtr = TypeOrPtr{zzModMatrix}
const ZZModMatrixOrPtr = TypeOrPtr{ZZModMatrix}
const fpMatrixOrPtr = TypeOrPtr{fpMatrix}
const FpMatrixOrPtr = TypeOrPtr{FpMatrix}
const fqPolyRepMatrixOrPtr = TypeOrPtr{fqPolyRepMatrix}
const FqPolyRepMatrixOrPtr = TypeOrPtr{FqPolyRepMatrix}
const FqMatrixOrPtr = TypeOrPtr{FqMatrix}

const ZZLaurentSeriesRingElemOrPtr = TypeOrPtr{ZZLaurentSeriesRingElem}

const FlintPuiseuxSeriesRingElemOrPtr{T <: RingElem} = TypeOrPtr{FlintPuiseuxSeriesRingElem{T}}
const FlintPuiseuxSeriesFieldElemOrPtr{T <: RingElem} = TypeOrPtr{FlintPuiseuxSeriesFieldElem{T}}

const PadicFieldElemOrPtr = TypeOrPtr{PadicFieldElem}
const QadicFieldElemOrPtr = TypeOrPtr{QadicFieldElem}

const IntegerUnionOrPtr = Union{Integer, TypeOrPtr{ZZRingElem}}
const RationalUnionOrPtr = Union{Integer, TypeOrPtr{ZZRingElem}, Rational, TypeOrPtr{QQFieldElem}}

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
