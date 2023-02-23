###############################################################################
#
#   FlintTypes.jl : Parent and object types for Flint
#
###############################################################################

###############################################################################
#
#   ZZRing / ZZRingElem
#
###############################################################################

struct ZZRing <: Ring
end

const FlintZZ = ZZRing()

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
        z = new()
        ccall((:fmpz_init, libflint), Nothing, (Ref{ZZRingElem},), z)
        ccall((:fmpz_set_mpz, libflint), Nothing, (Ref{ZZRingElem}, Ref{BigInt}), z, x)
        finalizer(_fmpz_clear_fn, z)
        return z
    end

    function ZZRingElem(x::Ptr{Culong}, len::Clong)
        z = new()
        ccall((:fmpz_init, libflint), Nothing, (Ref{ZZRingElem},), z)
        ccall((:fmpz_set_ui_array, libflint), Nothing, (Ref{ZZRingElem}, Ptr{Culong}, Clong), z, x, len)
        finalizer(_fmpz_clear_fn, z)
        return z
    end

    function ZZRingElem(x::Float64)
        !isinteger(x) && throw(InexactError(:convert, ZZRingElem, x))
        z = new()
        ccall((:fmpz_init, libflint), Nothing, (Ref{ZZRingElem},), z)
        ccall((:fmpz_set_d, libflint), Nothing, (Ref{ZZRingElem}, Cdouble), z, x)
        finalizer(_fmpz_clear_fn, z)
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

struct QQField <: FracField{ZZRingElem}
end

const FlintQQ = QQField()

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
      z = new()
      ccall((:fmpq_init, libflint), Nothing, (Ref{QQFieldElem},), z)
      ccall((:fmpq_set_fmpz_frac, libflint), Nothing,
            (Ref{QQFieldElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, a, b)
      finalizer(_fmpq_clear_fn, z)
      return z
   end

   function QQFieldElem(a::ZZRingElem)
      z = new()
      ccall((:fmpq_init, libflint), Nothing, (Ref{QQFieldElem},), z)
      b = ZZRingElem(1)
      ccall((:fmpq_set_fmpz_frac, libflint), Nothing,
            (Ref{QQFieldElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, a, b)
      finalizer(_fmpq_clear_fn, z)
      return z
   end

   function QQFieldElem(a::Int, b::Int)
      b == 0 && throw(DivideError())
      z = new()
      if b == typemin(Int) || (b < 0 && a == typemin(Int))
         bz = -ZZ(b)
         az = -ZZ(a)
         ccall((:fmpq_init, libflint), Nothing, (Ref{QQFieldElem},), z)
         ccall((:fmpq_set_fmpz_frac, libflint), Nothing,
	       (Ref{QQFieldElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), z, az, bz)
      else
         if b < 0 # Flint requires positive denominator
            b = -b
            a = -a
         end
         ccall((:fmpq_init, libflint), Nothing, (Ref{QQFieldElem},), z)
         ccall((:fmpq_set_si, libflint), Nothing,
               (Ref{QQFieldElem}, Int, Int), z, a, b)
      end
      finalizer(_fmpq_clear_fn, z)
      return z
   end

   function QQFieldElem(a::Int)
      z = new()
      ccall((:fmpq_init, libflint), Nothing, (Ref{QQFieldElem},), z)
      ccall((:fmpq_set_si, libflint), Nothing,
            (Ref{QQFieldElem}, Int, Int), z, a, 1)
      finalizer(_fmpq_clear_fn, z)
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
   base_ring::ZZRing
   S::Symbol

   function ZZPolyRing(R::ZZRing, s::Symbol, cached::Bool = true)
      return get_cached!(FmpzPolyID, (R, s), cached) do
         return new(R, s)
      end
   end
end

const FmpzPolyID = Dict{Tuple{ZZRing, Symbol}, ZZPolyRing}()

mutable struct ZZPolyRingElem <: PolyElem{ZZRingElem}
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
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing, (Ref{ZZPolyRingElem},), z)
      ccall((:fmpz_poly_set_si, libflint), Nothing, (Ref{ZZPolyRingElem}, Int), z, a)
      finalizer(_fmpz_poly_clear_fn, z)
      return z
   end

   function ZZPolyRingElem(a::ZZRingElem)
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing, (Ref{ZZPolyRingElem},), z)
      ccall((:fmpz_poly_set_fmpz, libflint), Nothing,
            (Ref{ZZPolyRingElem}, Ref{ZZRingElem}), z, a)
      finalizer(_fmpz_poly_clear_fn, z)
      return z
   end

   function ZZPolyRingElem(a::ZZPolyRingElem)
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing, (Ref{ZZPolyRingElem},), z)
      ccall((:fmpz_poly_set, libflint), Nothing,
            (Ref{ZZPolyRingElem}, Ref{ZZPolyRingElem}), z, a)
      finalizer(_fmpz_poly_clear_fn, z)
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
   base_ring::QQField
   S::Symbol

   function QQPolyRing(R::QQField, s::Symbol, cached::Bool = true)
      return get_cached!(FmpqPolyID, (R, s), cached) do
         return new(R, s)
      end
   end
end

const FmpqPolyID = Dict{Tuple{QQField, Symbol}, QQPolyRing}()

mutable struct QQPolyRingElem <: PolyElem{QQFieldElem}
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
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{QQPolyRingElem},), z)
      ccall((:fmpq_poly_set_si, libflint), Nothing, (Ref{QQPolyRingElem}, Int), z, a)
      finalizer(_fmpq_poly_clear_fn, z)
      return z
   end

   function QQPolyRingElem(a::ZZRingElem)
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{QQPolyRingElem},), z)
      ccall((:fmpq_poly_set_fmpz, libflint), Nothing,
            (Ref{QQPolyRingElem}, Ref{ZZRingElem}), z, a)
      finalizer(_fmpq_poly_clear_fn, z)
      return z
   end

   function QQPolyRingElem(a::QQFieldElem)
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{QQPolyRingElem},), z)
      ccall((:fmpq_poly_set_fmpq, libflint), Nothing,
            (Ref{QQPolyRingElem}, Ref{QQFieldElem}), z, a)
      finalizer(_fmpq_poly_clear_fn, z)
      return z
   end

   function QQPolyRingElem(a::ZZPolyRingElem)
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{QQPolyRingElem},), z)
      ccall((:fmpq_poly_set_fmpz_poly, libflint), Nothing,
            (Ref{QQPolyRingElem}, Ref{ZZPolyRingElem}), z, a)
      finalizer(_fmpq_poly_clear_fn, z)
      return z
   end

   function QQPolyRingElem(a::QQPolyRingElem)
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{QQPolyRingElem},), z)
      ccall((:fmpq_poly_set, libflint), Nothing,
            (Ref{QQPolyRingElem}, Ref{QQPolyRingElem}), z, a)
      finalizer(_fmpq_poly_clear_fn, z)
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

const NmodRingID = Dict{UInt, zzModRing}()

struct zzModRingElem <: ResElem{UInt}
   data::UInt
   parent::zzModRing
end

################################################################################
#
#   fpField / gfp
#
###############################################################################

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

const GaloisFieldID = Dict{UInt, fpField}()

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

   function fmpz_mod_ctx_struct()
      z = new()
      finalizer(_fmpz_mod_ctx_clear_fn, z)
      return z
   end
end

function _fmpz_mod_ctx_clear_fn(a::fmpz_mod_ctx_struct)
   ccall((:fmpz_mod_ctx_clear, libflint), Nothing, (Ref{fmpz_mod_ctx_struct},), a)
end

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

const FmpzModRingID = Dict{ZZRingElem, ZZModRing}()

struct ZZModRingElem <: ResElem{ZZRingElem}
   data::ZZRingElem
   parent::ZZModRing
end

###############################################################################
#
#   FpField / FpFieldElem
#
###############################################################################

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

const GaloisFmpzFieldID = Dict{ZZRingElem, FpField}()

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

const NmodPolyRingID = Dict{Tuple{zzModRing, Symbol}, zzModPolyRing}()

mutable struct zzModPolyRingElem <: PolyElem{zzModRingElem}
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

const GFPPolyRingID = Dict{Tuple{fpField, Symbol}, fpPolyRing}()

mutable struct fpPolyRingElem <: PolyElem{fpFieldElem}
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

const FmpzModPolyRingID = Dict{Tuple{ZZModRing, Symbol}, ZZModPolyRing}()

mutable struct ZZModPolyRingElem <: PolyElem{ZZModRingElem}
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
         x, x.parent.base_ring.ninv)
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

const GFPFmpzPolyRingID = Dict{Tuple{FpField, Symbol}, FpPolyRing}()

mutable struct FpPolyRingElem <: PolyElem{FpFieldElem}
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
         x, x.parent.base_ring.ninv)
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

   base_ring::ZZRing
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

         isempty(s) && error("need at least one indeterminate")

         z = new()
         ccall((:fmpz_mpoly_ctx_init, libflint), Nothing,
               (Ref{ZZMPolyRing}, Int, Int),
               z, length(s), ord)
         z.base_ring = FlintZZ
         z.S = s
         finalizer(_fmpz_mpoly_ctx_clear_fn, z)
         return z
      end
   end
end

function _fmpz_mpoly_ctx_clear_fn(a::ZZMPolyRing)
   ccall((:fmpz_mpoly_ctx_clear, libflint), Nothing,
           (Ref{ZZMPolyRing},), a)
end

const FmpzMPolyID = Dict{Tuple{Vector{Symbol}, Symbol}, ZZMPolyRing}()

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

   base_ring::QQField
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

         isempty(s) && error("need at least one indeterminate")

         z = new()
         ccall((:fmpq_mpoly_ctx_init, libflint), Nothing,
               (Ref{QQMPolyRing}, Int, Int),
               z, length(s), ord)
         z.base_ring = FlintQQ
         z.S = s
         finalizer(_fmpq_mpoly_ctx_clear_fn, z)
         return z
      end
   end
end

function _fmpq_mpoly_ctx_clear_fn(a::QQMPolyRing)
  ccall((:fmpq_mpoly_ctx_clear, libflint), Nothing,
          (Ref{QQMPolyRing},), a)
end

const FmpqMPolyID = Dict{Tuple{Vector{Symbol}, Symbol}, QQMPolyRing}()

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

         isempty(s) && error("need at least one indeterminate")

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

const NmodMPolyID = Dict{Tuple{zzModRing, Vector{Symbol}, Symbol}, zzModMPolyRing}()

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

         isempty(s) && error("need at least one indeterminate")

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

const GFPMPolyID = Dict{Tuple{fpField, Vector{Symbol}, Symbol}, fpMPolyRing}()

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

const GFPFmpzMPolyID = Dict{Tuple{FpField, Vector{Symbol}, Symbol}, FpMPolyRing}()

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

@attributes mutable struct fqPolyRepField <: FinField
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

   overfields :: Dict{Int, Vector{FinFieldMorphism}}
   subfields :: Dict{Int, Vector{FinFieldMorphism}}

   function fqPolyRepField(c::ZZRingElem, deg::Int, s::Symbol, cached::Bool = true)
      return get_cached!(FqNmodFiniteFieldID, (c, deg, s), cached) do
         d = new()
         ccall((:fq_nmod_ctx_init, libflint), Nothing,
               (Ref{fqPolyRepField}, Ref{ZZRingElem}, Int, Ptr{UInt8}),
			    d, c, deg, string(s))
         finalizer(_FqNmodFiniteField_clear_fn, d)
         return d
      end
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

const FqNmodFiniteFieldID = Dict{Tuple{ZZRingElem, Int, Symbol}, fqPolyRepField}()

const FqNmodFiniteFieldIDNmodPol = Dict{Tuple{zzModPolyRing, zzModPolyRingElem, Symbol},
                                    fqPolyRepField}()

const FqNmodFiniteFieldIDGFPPol = Dict{Tuple{fpPolyRing, fpPolyRingElem, Symbol},
                                    fqPolyRepField}()


function _FqNmodFiniteField_clear_fn(a :: fqPolyRepField)
   ccall((:fq_nmod_ctx_clear, libflint), Nothing, (Ref{fqPolyRepField},), a)
end

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
      finalizer(_fq_nmod_clear_fn, d)
      return d
   end

   function fqPolyRepFieldElem(ctx::fqPolyRepField, x::Int)
      d = new()
      ccall((:fq_nmod_init2, libflint), Nothing,
            (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, ctx)
      finalizer(_fq_nmod_clear_fn, d)
      ccall((:fq_nmod_set_si, libflint), Nothing,
                (Ref{fqPolyRepFieldElem}, Int, Ref{fqPolyRepField}), d, x, ctx)
      return d
   end

   function fqPolyRepFieldElem(ctx::fqPolyRepField, x::ZZRingElem)
      d = new()
      ccall((:fq_nmod_init2, libflint), Nothing,
            (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, ctx)
      finalizer(_fq_nmod_clear_fn, d)
      ccall((:fq_nmod_set_fmpz, libflint), Nothing,
            (Ref{fqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{fqPolyRepField}), d, x, ctx)
      return d
   end

      function fqPolyRepFieldElem(ctx::fqPolyRepField, x::fqPolyRepFieldElem)
      d = new()
      ccall((:fq_nmod_init2, libflint), Nothing,
            (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, ctx)
      finalizer(_fq_nmod_clear_fn, d)
      ccall((:fq_nmod_set, libflint), Nothing,
            (Ref{fqPolyRepFieldElem}, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), d, x, ctx)
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

@attributes mutable struct FqField <: FinField
   # fq_default_ctx_struct is 200 bytes on 64 bit machine
   opaque::NTuple{200, Int8}
   # end of flint struct

   var::String
   
   overfields::Dict{Int, Vector{FinFieldMorphism}}
   subfields::Dict{Int, Vector{FinFieldMorphism}}

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
         z.var = string(s)
         ccall((:fq_default_ctx_init_modulus, libflint), Nothing,
               (Ref{FqField}, Ref{ZZModPolyRingElem}, Ref{fmpz_mod_ctx_struct}, Ptr{UInt8}),
                  z, f, f.parent.base_ring.ninv, string(s))
         finalizer(_FqDefaultFiniteField_clear_fn, z)
         return z
      end
   end

   function FqField(f::FpPolyRingElem, s::Symbol, cached::Bool = true; check::Bool = true)
      # check ignored
      return get_cached!(FqDefaultFiniteFieldIDGFPPol, (f, s), cached) do
         z = new()
         z.isabsolute = true
         z.var = string(s)
         ccall((:fq_default_ctx_init_modulus, libflint), Nothing,
               (Ref{FqField}, Ref{FpPolyRingElem}, Ref{fmpz_mod_ctx_struct}, Ptr{UInt8}),
                  z, f, f.parent.base_ring.ninv, string(s))
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
         z.var = string(s)
         ccall((:fq_default_ctx_init_modulus_nmod, libflint), Nothing,
               (Ref{FqField}, Ref{fpPolyRingElem}, Ptr{UInt8}),
                  z, f, string(s))
         finalizer(_FqDefaultFiniteField_clear_fn, z)
         return z
      end
   end
end

const FqDefaultFiniteFieldID = Dict{Tuple{ZZRingElem, Int, Symbol}, FqField}()

const FqDefaultFiniteFieldIDFmpzPol = Dict{Tuple{ZZModPolyRingElem, Symbol}, FqField}()

const FqDefaultFiniteFieldIDGFPPol = Dict{Tuple{FpPolyRingElem, Symbol}, FqField}()

const FqDefaultFiniteFieldIDNmodPol = Dict{Tuple{zzModPolyRingElem, Symbol}, FqField}()

const FqDefaultFiniteFieldIDGFPNmodPol = Dict{Tuple{fpPolyRingElem, Symbol}, FqField}()

function _FqDefaultFiniteField_clear_fn(a :: FqField)
   ccall((:fq_default_ctx_clear, libflint), Nothing, (Ref{FqField},), a)
end

mutable struct FqFieldElem <: FinFieldElem
   opaque::NTuple{48, Int8} # fq_default_struct is 48 bytes on a 64 bit machine
   parent::FqField
   poly#::Union{Nothing, FqPolyRingElem}

   function FqFieldElem(ctx::FqField)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{FqField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      d.poly = nothing
      d.parent = ctx
      return d
   end

   function FqFieldElem(ctx::FqField, x::Int)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{FqField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_si, libflint), Nothing,
                (Ref{FqFieldElem}, Int, Ref{FqField}), d, x, ctx)
      d.parent = ctx
      d.poly = nothing
      return d
   end

   function FqFieldElem(ctx::FqField, x::ZZRingElem)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{FqField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_fmpz, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{ZZRingElem}, Ref{FqField}), d, x, ctx)
      d.parent = ctx
      d.poly = nothing
      return d
   end

   function FqFieldElem(ctx::FqField, x::ZZPolyRingElem)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{FqField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_fmpz_poly, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{ZZPolyRingElem}, Ref{FqField}), d, x, ctx)
      d.parent = ctx
      d.poly = nothing
      return d
   end

   function FqFieldElem(ctx::FqField, x::zzModPolyRingElem)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{FqField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_nmod_poly, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{zzModPolyRingElem}, Ref{FqField}), d, x, ctx)
      d.parent = ctx
      d.poly = nothing
      return d
   end

   function FqFieldElem(ctx::FqField, x::fpPolyRingElem)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{FqField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_nmod_poly, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{fpPolyRingElem}, Ref{FqField}), d, x, ctx)
      d.parent = ctx
      d.poly = nothing
      return d
   end

   function FqFieldElem(ctx::FqField, x::ZZModPolyRingElem)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{FqField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_fmpz_mod_poly, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{ZZModPolyRingElem}, Ref{FqField}), d, x, ctx)
      d.parent = ctx
      d.poly = nothing
      return d
   end

   function FqFieldElem(ctx::FqField, x::FpPolyRingElem)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{FqField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_fmpz_mod_poly, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{FpPolyRingElem}, Ref{FqField}), d, x, ctx)
      d.parent = ctx
      d.poly = nothing
      return d
   end

   function FqFieldElem(ctx::FqField, x::FqFieldElem)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{FqField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set, libflint), Nothing,
            (Ref{FqFieldElem}, Ref{FqFieldElem}, Ref{FqField}), d, x, ctx)
      d.parent = ctx
      d.poly = nothing
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

   function FqPolyRepField(char::ZZRingElem, deg::Int, s::Symbol, cached::Bool = true)
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
                  z, f, f.parent.base_ring.ninv, string(s))
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
                  z, f, f.parent.base_ring.ninv, string(s))
         finalizer(_FqFiniteField_clear_fn, z)
         return z
      end
   end

end

const FqFiniteFieldID = Dict{Tuple{ZZRingElem, Int, Symbol}, FqPolyRepField}()

const FqFiniteFieldIDFmpzPol = Dict{Tuple{ZZModPolyRingElem, Symbol}, FqPolyRepField}()

const FqFiniteFieldIDGFPPol = Dict{Tuple{FpPolyRingElem, Symbol}, FqPolyRepField}()

function _FqFiniteField_clear_fn(a :: FqPolyRepField)
   ccall((:fq_ctx_clear, libflint), Nothing, (Ref{FqPolyRepField},), a)
end

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

         isempty(s) && error("need at least one indeterminate")

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

const FqNmodMPolyID = Dict{Tuple{fqPolyRepField, Vector{Symbol}, Symbol}, fqPolyRepMPolyRing}()

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
#   FlintPadicField / padic
#
###############################################################################

const flint_padic_printing_mode = [:terse, :series, :val_unit]

mutable struct FlintPadicField <: FlintLocalField
   p::Int
   pinv::Float64
   pow::Ptr{Nothing}
   minpre::Int
   maxpre::Int
   mode::Cint
   prec_max::Int

   function FlintPadicField(p::ZZRingElem, prec::Int; cached::Bool = true, check::Bool = true)
      check && !is_probable_prime(p) && throw(DomainError(p, "Characteristic must be prime"))

      return get_cached!(PadicBase, (p, prec), cached) do
         d = new()
         ccall((:padic_ctx_init, libflint), Nothing,
               (Ref{FlintPadicField}, Ref{ZZRingElem}, Int, Int, Cint),
               d, p, 0, 0, 0)
         finalizer(_padic_ctx_clear_fn, d)
         d.prec_max = prec
         return d
      end
   end
end

const PadicBase = Dict{Tuple{ZZRingElem, Int}, FlintPadicField}()

function _padic_ctx_clear_fn(a::FlintPadicField)
   ccall((:padic_ctx_clear, libflint), Nothing, (Ref{FlintPadicField},), a)
end

mutable struct padic <: FlintLocalFieldElem
   u :: Int
   v :: Int
   N :: Int
   parent::FlintPadicField

   function padic(prec::Int)
      d = new()
      ccall((:padic_init2, libflint), Nothing, (Ref{padic}, Int), d, prec)
      finalizer(_padic_clear_fn, d)
      return d
   end
end

function _padic_clear_fn(a::padic)
   ccall((:padic_clear, libflint), Nothing, (Ref{padic},), a)
end

###############################################################################
#
#   FlintQadicField / qadic
#
###############################################################################

mutable struct FlintQadicField <: FlintLocalField
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

   function FlintQadicField(p::ZZRingElem, d::Int, prec::Int, var::String = "a"; cached::Bool = true, check::Bool = true)

      check && !is_probable_prime(p) && throw(DomainError(p, "Characteristic must be prime"))

      z = get_cached!(QadicBase, (p, d, prec), cached) do
         zz = new()
         ccall((:qadic_ctx_init, libflint), Nothing,
              (Ref{FlintQadicField}, Ref{ZZRingElem}, Int, Int, Int, Cstring, Cint),
                                        zz, p, d, 0, 0, var, 0)
         finalizer(_qadic_ctx_clear_fn, zz)
         zz.prec_max = prec
         return zz
      end

      return z, gen(z)
   end
end

const QadicBase = Dict{Tuple{ZZRingElem, Int, Int}, FlintQadicField}()

function _qadic_ctx_clear_fn(a::FlintQadicField)
   ccall((:qadic_ctx_clear, libflint), Nothing, (Ref{FlintQadicField},), a)
end

mutable struct qadic <: FlintLocalFieldElem
   coeffs::Int
   alloc::Int
   length::Int
   val::Int
   N::Int
   parent::FlintQadicField

   function qadic(prec::Int)
      z = new()
      ccall((:qadic_init2, libflint), Nothing, (Ref{qadic}, Int), z, prec)
      finalizer(_qadic_clear_fn, z)
      return z
   end
end

function _qadic_clear_fn(a::qadic)
   ccall((:qadic_clear, libflint), Nothing, (Ref{qadic},), a)
end

###############################################################################
#
#   ZZRelPowerSeriesRing / ZZRelPowerSeriesRingElem
#
###############################################################################

@attributes mutable struct ZZRelPowerSeriesRing <: SeriesRing{ZZRingElem}
   base_ring::ZZRing
   prec_max::Int
   S::Symbol

   function ZZRelPowerSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
      return get_cached!(FmpzRelSeriesID, (prec, s), cached) do
         return new(FlintZZ, prec, s)
      end
   end
end

const FmpzRelSeriesID = Dict{Tuple{Int, Symbol}, ZZRelPowerSeriesRing}()

mutable struct ZZRelPowerSeriesRingElem <: RelSeriesElem{ZZRingElem}
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
   base_ring::ZZRing
   prec_max::Int
   S::Symbol

   function ZZAbsPowerSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
      return get_cached!(FmpzAbsSeriesID, (prec, s), cached) do
         return new(FlintZZ, prec, s)
      end
   end
end

const FmpzAbsSeriesID = Dict{Tuple{Int, Symbol}, ZZAbsPowerSeriesRing}()

mutable struct ZZAbsPowerSeriesRingElem <: AbsSeriesElem{ZZRingElem}
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

const FlintPuiseuxSeriesID = Dict{Ring, Ring}()

mutable struct FlintPuiseuxSeriesRingElem{T <: RingElem} <: RingElem
   data::T
   scale::Int
   parent::FlintPuiseuxSeriesRing{T}

   function FlintPuiseuxSeriesRingElem{T}(d::T, scale::Int) where T <:
RingElem
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

   function FlintPuiseuxSeriesFieldElem{T}(d::T, scale::Int) where T <:
RingElem
      new{T}(d, scale)
   end
end

###############################################################################
#
#   ZZLaurentSeriesRing / ZZLaurentSeriesRingElem
#
###############################################################################

@attributes mutable struct ZZLaurentSeriesRing <: Ring
   base_ring::ZZRing
   prec_max::Int
   S::Symbol

   function ZZLaurentSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
      return get_cached!(FmpzLaurentSeriesID, (prec, s), cached) do
         return new(FlintZZ, prec, s)
      end
   end
end

const FmpzLaurentSeriesID = Dict{Tuple{Int, Symbol}, ZZLaurentSeriesRing}()

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
   base_ring::QQField
   prec_max::Int
   S::Symbol

   function QQRelPowerSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
      return get_cached!(FmpqRelSeriesID, (prec, s), cached) do
         return new(FlintQQ, prec, s)
      end
   end
end

const FmpqRelSeriesID = Dict{Tuple{Int, Symbol}, QQRelPowerSeriesRing}()

mutable struct QQRelPowerSeriesRingElem <: RelSeriesElem{QQFieldElem}
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
   base_ring::QQField
   prec_max::Int
   S::Symbol

   function QQAbsPowerSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
      return get_cached!(FmpqAbsSeriesID, (prec, s), cached) do
         return new(FlintQQ, prec, s)
      end
   end
end

const FmpqAbsSeriesID = Dict{Tuple{Int, Symbol}, QQAbsPowerSeriesRing}()

mutable struct QQAbsPowerSeriesRingElem <: AbsSeriesElem{QQFieldElem}
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

const GFPRelSeriesID = Dict{Tuple{fpField, Int, Symbol},
                                fpRelPowerSeriesRing}()

mutable struct fpRelPowerSeriesRingElem <: RelSeriesElem{fpFieldElem}
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

const NmodRelSeriesID = Dict{Tuple{zzModRing, Int, Symbol},
                                zzModRelPowerSeriesRing}()

mutable struct zzModRelPowerSeriesRingElem <: RelSeriesElem{zzModRingElem}
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

const GFPFmpzRelSeriesID = Dict{Tuple{FpField, Int, Symbol},
                                FpRelPowerSeriesRing}()

mutable struct FpRelPowerSeriesRingElem <: RelSeriesElem{FpFieldElem}
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
      p = a.parent.base_ring.ninv
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
         a, a.parent.base_ring.ninv)
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

const FmpzModRelSeriesID = Dict{Tuple{ZZModRing, Int, Symbol},
                                ZZModRelPowerSeriesRing}()

mutable struct ZZModRelPowerSeriesRingElem <: RelSeriesElem{ZZModRingElem}
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
      p = a.parent.base_ring.ninv
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
         a, a.parent.base_ring.ninv)
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

const GFPFmpzAbsSeriesID = Dict{Tuple{FpField, Int, Symbol},
                                FpAbsPowerSeriesRing}()

mutable struct FpAbsPowerSeriesRingElem <: AbsSeriesElem{FpFieldElem}
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
      p = a.parent.base_ring.ninv
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
         a, a.parent.base_ring.ninv)
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
 
const NmodAbsSeriesID = Dict{Tuple{zzModRing, Int, Symbol},
                                 zzModAbsPowerSeriesRing}()
  
mutable struct zzModAbsPowerSeriesRingElem <: AbsSeriesElem{zzModRingElem}
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
 
const GFPAbsSeriesID = Dict{Tuple{fpField, Int, Symbol},
                                 fpAbsPowerSeriesRing}()
  
mutable struct fpAbsPowerSeriesRingElem <: AbsSeriesElem{fpFieldElem}
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

const FmpzModAbsSeriesID = Dict{Tuple{ZZModRing, Int, Symbol},
                                ZZModAbsPowerSeriesRing}()

mutable struct ZZModAbsPowerSeriesRingElem <: AbsSeriesElem{ZZModRingElem}
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
      p = a.parent.base_ring.ninv
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
         a, a.parent.base_ring.ninv)
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
 
const FqDefaultRelSeriesID = Dict{Tuple{FqField, Int, Symbol}, FqRelPowerSeriesRing}()
 
mutable struct FqRelPowerSeriesRingElem <: RelSeriesElem{FqFieldElem}
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

const FqRelSeriesID = Dict{Tuple{FqPolyRepField, Int, Symbol}, FqPolyRepRelPowerSeriesRing}()

mutable struct FqPolyRepRelPowerSeriesRingElem <: RelSeriesElem{FqPolyRepFieldElem}
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
 
const FqDefaultAbsSeriesID = Dict{Tuple{FqField, Int, Symbol}, FqAbsPowerSeriesRing}()
 
mutable struct FqAbsPowerSeriesRingElem <: AbsSeriesElem{FqFieldElem}
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

const FqAbsSeriesID = Dict{Tuple{FqPolyRepField, Int, Symbol}, FqPolyRepAbsPowerSeriesRing}()

mutable struct FqPolyRepAbsPowerSeriesRingElem <: AbsSeriesElem{FqPolyRepFieldElem}
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

const FqNmodRelSeriesID = Dict{Tuple{fqPolyRepField, Int, Symbol},
                               fqPolyRepRelPowerSeriesRing}()

mutable struct fqPolyRepRelPowerSeriesRingElem <: RelSeriesElem{fqPolyRepFieldElem}
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

const FqNmodAbsSeriesID = Dict{Tuple{fqPolyRepField, Int, Symbol},
                               fqPolyRepAbsPowerSeriesRing}()

mutable struct fqPolyRepAbsPowerSeriesRingElem <: AbsSeriesElem{fqPolyRepFieldElem}
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

# not really a mathematical ring
mutable struct QQMatrixSpace <: MatSpace{QQFieldElem}
   nrows::Int
   ncols::Int
   base_ring::QQField

   function QQMatrixSpace(r::Int, c::Int, cached::Bool = true)
      return get_cached!(FmpqMatID, (r, c), cached) do
         return new(r, c, FlintQQ)
      end
   end
end

const FmpqMatID = Dict{Tuple{Int, Int}, QQMatrixSpace}()

mutable struct QQMatrix <: MatElem{QQFieldElem}
   entries::Ptr{Nothing}
   r::Int
   c::Int
   rows::Ptr{Nothing}
   base_ring::QQField
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
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{QQMatrix}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{QQFieldElem},
                       (Ref{QQMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set, libflint), Nothing,
                  (Ptr{QQFieldElem}, Ref{QQFieldElem}), el, arr[i, j])
         end
      end
      return z
   end

   function QQMatrix(r::Int, c::Int, arr::AbstractMatrix{ZZRingElem})
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{QQMatrix}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      b = ZZRingElem(1)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{QQFieldElem},
                       (Ref{QQMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set_fmpz_frac, libflint), Nothing,
                  (Ptr{QQFieldElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), el, arr[i, j], b)
         end
      end
      return z
   end


   function QQMatrix(r::Int, c::Int, arr::AbstractVector{QQFieldElem})
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{QQMatrix}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{QQFieldElem},
                       (Ref{QQMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set, libflint), Nothing,
                  (Ptr{QQFieldElem}, Ref{QQFieldElem}), el, arr[(i-1)*c+j])
         end
      end
      return z
   end

   function QQMatrix(r::Int, c::Int, arr::AbstractVector{ZZRingElem})
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{QQMatrix}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      b = ZZRingElem(1)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{QQFieldElem},
                       (Ref{QQMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set_fmpz_frac, libflint), Nothing,
                  (Ptr{QQFieldElem}, Ref{ZZRingElem}, Ref{ZZRingElem}), el, arr[(i-1)*c+j], b)
         end
      end
      return z
   end


   function QQMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Integer}
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{QQMatrix}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{QQFieldElem},
                       (Ref{QQMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set, libflint), Nothing,
                  (Ptr{QQFieldElem}, Ref{QQFieldElem}), el, QQFieldElem(arr[i, j]))
         end
      end
      return z
   end

   function QQMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Integer}
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{QQMatrix}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{QQFieldElem},
                       (Ref{QQMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set, libflint), Nothing,
                  (Ptr{QQFieldElem}, Ref{QQFieldElem}), el, QQFieldElem(arr[(i-1)*c+j]))
         end
      end
      return z
   end

   function QQMatrix(r::Int, c::Int, d::QQFieldElem)
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{QQMatrix}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      GC.@preserve z for i = 1:min(r, c)
         el = ccall((:fmpq_mat_entry, libflint), Ptr{QQFieldElem},
                    (Ref{QQMatrix}, Int, Int), z, i - 1, i - 1)
         ccall((:fmpq_set, libflint), Nothing,
               (Ptr{QQFieldElem}, Ref{QQFieldElem}), el, d)
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

# not really a mathematical ring
mutable struct ZZMatrixSpace <: MatSpace{ZZRingElem}
   nrows::Int
   ncols::Int
   base_ring::ZZRing

   function ZZMatrixSpace(r::Int, c::Int, cached::Bool = true)
      return get_cached!(FmpzMatID, (r, c), cached) do
         return new(r, c, FlintZZ)
      end
   end
end

const FmpzMatID = Dict{Tuple{Int, Int}, ZZMatrixSpace}()

mutable struct ZZMatrix <: MatElem{ZZRingElem}
   entries::Ptr{Nothing}
   r::Int
   c::Int
   rows::Ptr{Nothing}
   base_ring::ZZRing
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
      z = new()
      ccall((:fmpz_mat_init, libflint), Nothing,
            (Ref{ZZMatrix}, Int, Int), z, r, c)
      finalizer(_fmpz_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem},
                       (Ref{ZZMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpz_set, libflint), Nothing,
                  (Ptr{ZZRingElem}, Ref{ZZRingElem}), el, arr[i, j])
         end
      end
      return z
   end

   function ZZMatrix(r::Int, c::Int, arr::AbstractVector{ZZRingElem})
      z = new()
      ccall((:fmpz_mat_init, libflint), Nothing,
            (Ref{ZZMatrix}, Int, Int), z, r, c)
      finalizer(_fmpz_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem},
                       (Ref{ZZMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpz_set, libflint), Nothing,
                  (Ptr{ZZRingElem}, Ref{ZZRingElem}), el, arr[(i-1)*c+j])
         end
      end
      return z
   end

   function ZZMatrix(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Integer}
      z = new()
      ccall((:fmpz_mat_init, libflint), Nothing,
            (Ref{ZZMatrix}, Int, Int), z, r, c)
      finalizer(_fmpz_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem},
                       (Ref{ZZMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpz_set, libflint), Nothing,
                  (Ptr{ZZRingElem}, Ref{ZZRingElem}), el, ZZRingElem(arr[i, j]))
         end
      end
      return z
   end

   function ZZMatrix(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Integer}
      z = new()
      ccall((:fmpz_mat_init, libflint), Nothing,
            (Ref{ZZMatrix}, Int, Int), z, r, c)
      finalizer(_fmpz_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem},
                       (Ref{ZZMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpz_set, libflint), Nothing,
                  (Ptr{ZZRingElem}, Ref{ZZRingElem}), el, ZZRingElem(arr[(i-1)*c+j]))
         end
      end
      return z
   end

   function ZZMatrix(r::Int, c::Int, d::ZZRingElem)
      z = new()
      ccall((:fmpz_mat_init, libflint), Nothing,
            (Ref{ZZMatrix}, Int, Int), z, r, c)
      finalizer(_fmpz_mat_clear_fn, z)
      GC.@preserve z for i = 1:min(r, c)
         el = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem},
                    (Ref{ZZMatrix}, Int, Int), z, i - 1, i- 1)
         ccall((:fmpz_set, libflint), Nothing,
               (Ptr{ZZRingElem}, Ref{ZZRingElem}), el, d)
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

mutable struct zzModMatrixSpace <: MatSpace{zzModRingElem}
  base_ring::zzModRing
  n::UInt
  nrows::Int
  ncols::Int

  function zzModMatrixSpace(R::zzModRing, r::Int, c::Int,
                        cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    return get_cached!(NmodMatID, (R, r, c), cached) do
       return new(R, R.n, r, c)
    end
  end
end

const NmodMatID = Dict{Tuple{zzModRing, Int, Int}, zzModMatrixSpace}()

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
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{zzModMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
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
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{zzModMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, mod(arr[(i - 1) * c + j], n), i, j)
      end
    end
    return z
  end

  function zzModMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{ZZRingElem}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{zzModMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
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
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{zzModMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
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
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{zzModMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
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
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{zzModMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
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

mutable struct ZZModMatrixSpace <: MatSpace{ZZModRingElem}
  base_ring::ZZModRing
  n::ZZRingElem
  nrows::Int
  ncols::Int

  function ZZModMatrixSpace(R::ZZModRing, r::Int, c::Int,
                        cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    return get_cached!(FmpzModMatID, (R, r, c), cached) do
       return new(R, R.n, r, c)
    end
  end
end

const FmpzModMatID = Dict{Tuple{ZZModRing, Int, Int}, ZZModMatrixSpace}()

mutable struct ZZModMatrix <: MatElem{ZZModRingElem}
   entries::Ptr{Nothing}
   r::Int
   c::Int
   rows::Ptr{Nothing}
   mod::Int              # ZZRingElem
   # end flint struct

   base_ring::ZZModRing
   view_parent

  # Used by view, not finalised!!
  function ZZModMatrix()
    z = new()
    return z
  end

  function ZZModMatrix(r::Int, c::Int, n::ZZRingElem)
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
            (Ref{ZZModMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    return z
  end

  function ZZModMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractMatrix{ZZRingElem}, transpose::Bool = false)
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
            (Ref{ZZModMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
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

  function ZZModMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractMatrix{T}, transpose::Bool = false) where T <: Integer
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
	  (Ref{ZZModMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    if transpose
       arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
         setindex_raw!(z, mod(ZZRingElem(arr[i, j]), n), i, j)
      end
    end
    return z
  end

  function ZZModMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractMatrix{ZZModRingElem}, transpose::Bool = false)
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
	  (Ref{ZZModMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
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

  function ZZModMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractVector{ZZRingElem})
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
            (Ref{ZZModMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, mod(arr[(i - 1)*c + j], n), i, j)
      end
    end
    return z
  end

  function ZZModMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractVector{T}) where T <: Integer
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
          (Ref{ZZModMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    for i = 1:r
       for j = 1:c
          setindex_raw!(z, mod(ZZRingElem(arr[(i - 1)*c + j]), n), i, j)
       end
    end
    return z
  end

  function ZZModMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractVector{ZZModRingElem})                z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
	  (Ref{ZZModMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    for i = 1:r
       for j = 1:c
          setindex_raw!(z, arr[(i - 1)*c + j].data, i, j)
       end
    end
    return z
  end
end

function _fmpz_mod_mat_clear_fn(mat::ZZModMatrix)
  ccall((:fmpz_mod_mat_clear, libflint), Nothing, (Ref{ZZModMatrix}, ), mat)
end

###############################################################################
#
#   FpMatrixSpace / FpMatrix
#
###############################################################################

mutable struct FpMatrixSpace <: MatSpace{FpFieldElem}
  base_ring::FpField
  n::ZZRingElem
  nrows::Int
  ncols::Int

  function FpMatrixSpace(R::FpField, r::Int, c::Int, cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    return get_cached!(GaloisFmpzMatID, (R, r, c), cached) do
       return new(R, R.n, r, c)
    end
  end
end

const GaloisFmpzMatID = Dict{Tuple{FpField, Int, Int}, FpMatrixSpace}()

mutable struct FpMatrix <: MatElem{FpFieldElem}
   entries::Ptr{Nothing}
   r::Int
   c::Int
   rows::Ptr{Nothing}
   mod::Int              # ZZRingElem
   # end flint struct

   base_ring::FpField
   view_parent

  # Used by view, not finalised!!
  function FpMatrix()
    z = new()
    return z
  end

  function FpMatrix(r::Int, c::Int, n::ZZRingElem)
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
            (Ref{FpMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_gfp_fmpz_mat_clear_fn, z)
    return z
  end

  function FpMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractMatrix{ZZRingElem}, transpose::Bool = false)
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
          (Ref{FpMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_gfp_fmpz_mat_clear_fn, z)
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

  function FpMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractMatrix{T}, transpose::Bool = false) where T <: Integer
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
          (Ref{FpMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_gfp_fmpz_mat_clear_fn, z)
    if transpose
       arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
         setindex_raw!(z, mod(ZZRingElem(arr[i, j]), n), i, j)
      end
    end
    return z
  end

  function FpMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractMatrix{FpFieldElem}, transpose::Bool = false)
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
          (Ref{FpMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_gfp_fmpz_mat_clear_fn, z)
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

  function FpMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractVector{ZZRingElem})
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
          (Ref{FpMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_gfp_fmpz_mat_clear_fn, z)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, mod(arr[(i - 1)*c + j], n), i, j)
      end
    end
    return z
  end

  function FpMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractVector{T}) where T <: Integer
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
          (Ref{FpMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_gfp_fmpz_mat_clear_fn, z)
    for i = 1:r
       for j = 1:c
          setindex_raw!(z, mod(ZZRingElem(arr[(i - 1)*c + j]), n), i, j)
       end
    end
    return z
  end

  function FpMatrix(r::Int, c::Int, n::ZZRingElem, arr::AbstractVector{FpFieldElem})
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
          (Ref{FpMatrix}, Int, Int, Ref{ZZRingElem}), z, r, c, n)
    finalizer(_gfp_fmpz_mat_clear_fn, z)
    for i = 1:r
       for j = 1:c
          setindex_raw!(z, arr[(i - 1)*c + j].data, i, j)
       end
    end
    return z
  end
end

function _gfp_fmpz_mat_clear_fn(mat::FpMatrix)
  ccall((:fmpz_mod_mat_clear, libflint), Nothing, (Ref{FpMatrix}, ), mat)
end

################################################################################
#
#   fpMatrixSpace / fpMatrix
#
###############################################################################

mutable struct fpMatrixSpace <: MatSpace{fpFieldElem}
  base_ring::fpField
  n::UInt
  nrows::Int
  ncols::Int

  function fpMatrixSpace(R::fpField, r::Int, c::Int,
                        cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    return get_cached!(GFPMatID, (R, r, c), cached) do
       return new(R, R.n, r, c)
    end
  end
end

const GFPMatID = Dict{Tuple{fpField, Int, Int}, fpMatrixSpace}()

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
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{fpMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
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
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{fpMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, mod(arr[(i - 1) * c + j], n), i, j)
      end
    end
    return z
  end

  function fpMatrix(r::Int, c::Int, n::UInt, arr::AbstractMatrix{ZZRingElem}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{fpMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
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
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{fpMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
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
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{fpMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
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
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{fpMatrix}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
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
 
const FqDefaultPolyID = Dict{Tuple{FqField, Symbol}, FqPolyRing}()
 
mutable struct FqPolyRingElem <: PolyElem{FqFieldElem}
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
   ccall((:fq_default_poly_factor_clear, libflint), Nothing,
         (Ref{fq_default_poly_factor}, Ref{FqField}),
          a, a.base_field)
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

const FqPolyID = Dict{Tuple{FqPolyRepField, Symbol}, FqPolyRepPolyRing}()

mutable struct FqPolyRepPolyRingElem <: PolyElem{FqPolyRepFieldElem}
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

const FqNmodPolyID = Dict{Tuple{fqPolyRepField, Symbol}, fqPolyRepPolyRing}()

mutable struct fqPolyRepPolyRingElem <: PolyElem{fqPolyRepFieldElem}
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

mutable struct FqMatrixSpace <: MatSpace{FqFieldElem}
   base_ring::FqField
   nrows::Int
   ncols::Int
 
   function FqMatrixSpace(R::FqField, r::Int, c::Int, cached::Bool = true)
     (r < 0 || c < 0) && throw(error_dim_negative)
     return get_cached!(FqDefaultMatID, (R, r, c), cached) do
        return new(R, r, c)
     end
   end
 end
 
 const FqDefaultMatID = Dict{Tuple{FqField, Int, Int}, FqMatrixSpace}()
 
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
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{FqMatrix}, Int, Int, Ref{FqField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set, libflint), Nothing,
                   (Ref{FqMatrix}, Int, Int, Ref{FqFieldElem},
                    Ref{FqField}),
                    z, i - 1, j - 1, arr[i, j], ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function FqMatrix(r::Int, c::Int, arr::AbstractVector{FqFieldElem}, ctx::FqField)
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{FqMatrix}, Int, Int, Ref{FqField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set, libflint), Nothing,
                   (Ref{FqMatrix}, Int, Int, Ref{FqFieldElem},
                    Ref{FqField}),
                    z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function FqMatrix(r::Int, c::Int, arr::AbstractMatrix{ZZRingElem}, ctx::FqField)
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{FqMatrix}, Int, Int, Ref{FqField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set_fmpz, libflint), Nothing,
                  (Ref{FqMatrix}, Int, Int, Ref{ZZRingElem},
                   Ref{FqField}),
                   z, i - 1, j - 1, arr[i, j], ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function FqMatrix(r::Int, c::Int, arr::AbstractVector{ZZRingElem}, ctx::FqField)
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{FqMatrix}, Int, Int, Ref{FqField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set_fmpz, libflint), Nothing,
                   (Ref{FqMatrix}, Int, Int, Ref{ZZRingElem},
                    Ref{FqField}),
                    z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function FqMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, ctx::FqField) where {T <: Integer}
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{FqMatrix}, Int, Int, Ref{FqField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set, libflint), Nothing,
                     (Ref{FqMatrix}, Int, Int, Ref{FqFieldElem},
                      Ref{FqField}),
                      z, i - 1, j - 1, ctx(arr[i, j]), ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function FqMatrix(r::Int, c::Int, arr::AbstractVector{T}, ctx::FqField) where {T <: Integer}
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{FqMatrix}, Int, Int, Ref{FqField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set, libflint), Nothing,
                   (Ref{FqMatrix}, Int, Int, Ref{FqFieldElem},
                    Ref{FqField}),
                    z, i - 1, j - 1, ctx(arr[(i - 1) * c + j]), ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function FqMatrix(r::Int, c::Int, d::FqFieldElem)
       z = new()
       ctx = parent(d)
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{FqMatrix}, Int, Int, Ref{FqField}),
              z, r, c, ctx)
       for i = 1:min(r, c)
          ccall((:fq_default_mat_entry_set, libflint), Nothing,
                (Ref{FqMatrix}, Int, Int, Ref{FqFieldElem},
                 Ref{FqField}), z, i - 1, i - 1, d, ctx)
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function FqMatrix(m::ZZMatrix, ctx::FqField)
       z = new()
       r = nrows(m)
       c = ncols(m)
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{FqMatrix}, Int, Int, Ref{FqField}),
              z, r, c, ctx)
       ccall((:fq_default_mat_set_fmpz_mat, libflint), Nothing,
             (Ref{FqMatrix}, Ref{ZZMatrix}, Ref{FqField}),
             z, m, ctx)
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end

    function FqMatrix(m::ZZModMatrix, ctx::FqField)
      z = new()
      r = nrows(m)
      c = ncols(m)
      ccall((:fq_default_mat_init, libflint), Nothing,
            (Ref{FqMatrix}, Int, Int, Ref{FqField}),
             z, r, c, ctx)
      ccall((:fq_default_mat_set_fmpz_mod_mat, libflint), Nothing,
            (Ref{FqMatrix}, Ref{ZZModMatrix}, Ref{FqField}),
            z, m, ctx)
      z.base_ring = ctx
      finalizer(_fq_default_mat_clear_fn, z)
      return z
   end

   function FqMatrix(m::zzModMatrix, ctx::FqField)
      z = new()
      r = nrows(m)
      c = ncols(m)
      ccall((:fq_default_mat_init, libflint), Nothing,
            (Ref{FqMatrix}, Int, Int, Ref{FqField}),
             z, r, c, ctx)
      ccall((:fq_default_mat_set_nmod_mat, libflint), Nothing,
            (Ref{FqMatrix}, Ref{zzModMatrix}, Ref{FqField}),
            z, m, ctx)
      z.base_ring = ctx
      finalizer(_fq_default_mat_clear_fn, z)
      return z
   end

   function FqMatrix(m::fpMatrix, ctx::FqField)
      z = new()
      r = nrows(m)
      c = ncols(m)
      ccall((:fq_default_mat_init, libflint), Nothing,
            (Ref{FqMatrix}, Int, Int, Ref{FqField}),
             z, r, c, ctx)
      ccall((:fq_default_mat_set_nmod_mat, libflint), Nothing,
            (Ref{FqMatrix}, Ref{fpMatrix}, Ref{FqField}),
            z, m, ctx)
      z.base_ring = ctx
      finalizer(_fq_default_mat_clear_fn, z)
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

mutable struct FqPolyRepMatrixSpace <: MatSpace{FqPolyRepFieldElem}
  base_ring::FqPolyRepField
  nrows::Int
  ncols::Int

  function FqPolyRepMatrixSpace(R::FqPolyRepField, r::Int, c::Int, cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    return get_cached!(FqMatID, (R, r, c), cached) do
       return new(R, r, c)
    end
  end
end

const FqMatID = Dict{Tuple{FqPolyRepField, Int, Int}, FqPolyRepMatrixSpace}()

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
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_mat_entry_set, libflint), Nothing,
                  (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
                   z, i - 1, j - 1, arr[i, j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{FqPolyRepFieldElem}, ctx::FqPolyRepField)
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_mat_entry_set, libflint), Nothing,
                       (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
                        z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractMatrix{ZZRingElem}, ctx::FqPolyRepField)
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fq_mat_entry, libflint), Ptr{FqPolyRepFieldElem},
                       (Ref{FqPolyRepMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fq_set_fmpz, libflint), Nothing,
                  (Ptr{FqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{FqPolyRepField}), el, arr[i, j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{ZZRingElem}, ctx::FqPolyRepField)
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fq_mat_entry, libflint), Ptr{FqPolyRepFieldElem},
                       (Ref{FqPolyRepMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fq_set_fmpz, libflint), Nothing,
                  (Ptr{FqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{FqPolyRepField}), el, arr[(i - 1) * c + j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, ctx::FqPolyRepField) where {T <: Integer}
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_mat_entry_set, libflint), Nothing,
                    (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
                     z, i - 1, j - 1, ctx(arr[i, j]), ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function FqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{T }, ctx::FqPolyRepField) where {T <: Integer}
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_mat_entry_set, libflint), Nothing,
                       (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}),
                        z, i - 1, j - 1, ctx(arr[(i - 1) * c + j]), ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function FqPolyRepMatrix(r::Int, c::Int, d::FqPolyRepFieldElem)
      z = new()
      ctx = parent(d)
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepField}), z, r, c, ctx)
      for i = 1:min(r, c)
         ccall((:fq_mat_entry_set, libflint), Nothing,
               (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepFieldElem}, Ref{FqPolyRepField}), z, i - 1, i- 1, d, ctx)
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function FqPolyRepMatrix(m::ZZMatrix, ctx::FqPolyRepField)
      z = new()
      r = nrows(m)
      c = ncols(m)
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{FqPolyRepMatrix}, Int, Int, Ref{FqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el1 = ccall((:fq_mat_entry, libflint), Ptr{FqPolyRepFieldElem},
                        (Ref{FqPolyRepMatrix}, Int, Int), z, i - 1, j - 1)
            el2 = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem},
                        (Ref{ZZMatrix}, Int, Int), m, i - 1, j - 1)

            ccall((:fq_set_fmpz, libflint), Nothing,
                  (Ptr{FqPolyRepFieldElem}, Ptr{ZZRingElem}, Ref{FqPolyRepField}), el1, el2, ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
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

mutable struct fqPolyRepMatrixSpace <: MatSpace{fqPolyRepFieldElem}
  base_ring::fqPolyRepField
  nrows::Int
  ncols::Int

  function fqPolyRepMatrixSpace(R::fqPolyRepField, r::Int, c::Int, cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    return get_cached!(FqNmodMatID, (R, r, c), cached) do
       return new(R, r, c)
    end
  end
end

const FqNmodMatID = Dict{Tuple{fqPolyRepField, Int, Int}, fqPolyRepMatrixSpace}()

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
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
                  (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                   z, i - 1, j - 1, arr[i, j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{fqPolyRepFieldElem}, ctx::fqPolyRepField)
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
                       (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                        z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractMatrix{ZZRingElem}, ctx::fqPolyRepField)
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fq_nmod_mat_entry, libflint), Ptr{fqPolyRepFieldElem},
                       (Ref{fqPolyRepMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fq_nmod_set_fmpz, libflint), Nothing,
                  (Ptr{fqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{fqPolyRepField}), el, arr[i, j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{ZZRingElem}, ctx::fqPolyRepField)
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fq_nmod_mat_entry, libflint), Ptr{fqPolyRepFieldElem},
                       (Ref{fqPolyRepMatrix}, Int, Int), z, i - 1, j - 1)
            ccall((:fq_nmod_set_fmpz, libflint), Nothing,
                  (Ptr{fqPolyRepFieldElem}, Ref{ZZRingElem}, Ref{fqPolyRepField}), el, arr[(i - 1) * c + j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractMatrix{T}, ctx::fqPolyRepField) where {T <: Integer}
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
                    (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                     z, i - 1, j - 1, ctx(arr[i, j]), ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fqPolyRepMatrix(r::Int, c::Int, arr::AbstractVector{T }, ctx::fqPolyRepField) where {T <: Integer}
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
                       (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}),
                        z, i - 1, j - 1, ctx(arr[(i - 1) * c + j]), ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fqPolyRepMatrix(r::Int, c::Int, d::fqPolyRepFieldElem)
      z = new()
      ctx = parent(d)
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepField}), z, r, c, ctx)
      for i = 1:min(r, c)
         ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
               (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepFieldElem}, Ref{fqPolyRepField}), z, i - 1, i- 1, d, ctx)
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fqPolyRepMatrix(m::ZZMatrix, ctx::fqPolyRepField)
      z = new()
      r = nrows(m)
      c = ncols(m)
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fqPolyRepMatrix}, Int, Int, Ref{fqPolyRepField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el1 = ccall((:fq_nmod_mat_entry, libflint), Ptr{fqPolyRepFieldElem},
                        (Ref{fqPolyRepMatrix}, Int, Int), z, i - 1, j - 1)
            el2 = ccall((:fmpz_mat_entry, libflint), Ptr{ZZRingElem},
                        (Ref{ZZMatrix}, Int, Int), m, i - 1, j - 1)

            ccall((:fq_nmod_set_fmpz, libflint), Nothing,
                  (Ptr{fqPolyRepFieldElem}, Ptr{ZZRingElem}, Ref{fqPolyRepField}), el1, el2, ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
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

mutable struct rand_ctx
   ptr::Ptr{Cvoid}

   function rand_ctx()
      z = new()
      z.ptr = ccall((:flint_rand_alloc, libflint), Ptr{Cvoid}, ( ))
      ccall((:flint_randinit, libflint), Cvoid, (Ptr{Cvoid}, ), z.ptr)
      finalizer(_rand_ctx_clear_fn, z)
      return z
   end
end

function _rand_ctx_clear_fn(a::rand_ctx)
   ccall((:flint_randclear, libflint), Cvoid, (Ptr{Cvoid}, ), a.ptr)
   ccall((:flint_rand_free, libflint), Cvoid, (Ptr{Cvoid}, ), a.ptr)
   nothing
end

################################################################################
#
# Type unions
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
    typ::Int    # keep these in sync with fq_default_mpoly_do_op and
                # the polynomial_ring constructor

    function FqMPolyRing(a, b::FqField, c::Int, cached = true)
        return get_cached!(FqDefaultMPolyID, (a, b, c), cached) do
            return new(a, b, c)
        end::FqMPolyRing
    end
end

const FqDefaultMPolyID = AbstractAlgebra.CacheDictType{
                              Tuple{Any, FqField, Int},
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

