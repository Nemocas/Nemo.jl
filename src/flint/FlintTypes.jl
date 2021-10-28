###############################################################################
#
#   FlintTypes.jl : Parent and object types for Flint
#
###############################################################################

###############################################################################
#
#   FlintIntegerRing / fmpz
#
###############################################################################

mutable struct FlintIntegerRing <: Ring
end

const FlintZZ = FlintIntegerRing()

mutable struct fmpz <: RingElem
    d::Int

    function fmpz()
        z = new()
        ccall((:fmpz_init, libflint), Nothing, (Ref{fmpz},), z)
        finalizer(_fmpz_clear_fn, z)
        return z
    end

    function fmpz(x::Int)
        z = new()
        ccall((:fmpz_init_set_si, libflint), Nothing, (Ref{fmpz}, Int), z, x)
        finalizer(_fmpz_clear_fn, z)
        return z
    end

    function fmpz(x::UInt)
        z = new()
        ccall((:fmpz_init_set_ui, libflint), Nothing, (Ref{fmpz}, UInt), z, x)
        finalizer(_fmpz_clear_fn, z)
        return z
    end

    function fmpz(x::BigInt)
        z = new()
        ccall((:fmpz_init, libflint), Nothing, (Ref{fmpz},), z)
        ccall((:fmpz_set_mpz, libflint), Nothing, (Ref{fmpz}, Ref{BigInt}), z, x)
        finalizer(_fmpz_clear_fn, z)
        return z
    end

    function fmpz(x::Ptr{Culong}, len::Clong)
        z = new()
        ccall((:fmpz_init, libflint), Nothing, (Ref{fmpz},), z)
        ccall((:fmpz_set_ui_array, libflint), Nothing, (Ref{fmpz}, Ptr{Culong}, Clong), z, x, len)
        finalizer(_fmpz_clear_fn, z)
        return z
    end

    function fmpz(x::Float64)
        !isinteger(x) && throw(InexactError(:convert, fmpz, x))
        z = new()
        ccall((:fmpz_init, libflint), Nothing, (Ref{fmpz},), z)
        ccall((:fmpz_set_d, libflint), Nothing, (Ref{fmpz}, Cdouble), z, x)
        finalizer(_fmpz_clear_fn, z)
        return z
    end

    fmpz(x::fmpz) = x
end

function _fmpz_clear_fn(a::fmpz)
   ccall((:fmpz_clear, libflint), Nothing, (Ref{fmpz},), a)
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
#   FlintRationalField / fmpq
#
###############################################################################

mutable struct FlintRationalField <: FracField{fmpz}
end

const FlintQQ = FlintRationalField()

mutable struct fmpq <: FracElem{fmpz}
   num::Int
   den::Int

   function fmpq()
      z = new()
      ccall((:fmpq_init, libflint), Nothing, (Ref{fmpq},), z)
      finalizer(_fmpq_clear_fn, z)
      return z
   end

   function fmpq(a::fmpz, b::fmpz)
      iszero(b) && throw(DivideError())
      z = new()
      ccall((:fmpq_init, libflint), Nothing, (Ref{fmpq},), z)
      ccall((:fmpq_set_fmpz_frac, libflint), Nothing,
            (Ref{fmpq}, Ref{fmpz}, Ref{fmpz}), z, a, b)
      finalizer(_fmpq_clear_fn, z)
      return z
   end

   function fmpq(a::fmpz)
      z = new()
      ccall((:fmpq_init, libflint), Nothing, (Ref{fmpq},), z)
      b = fmpz(1)
      ccall((:fmpq_set_fmpz_frac, libflint), Nothing,
            (Ref{fmpq}, Ref{fmpz}, Ref{fmpz}), z, a, b)
      finalizer(_fmpq_clear_fn, z)
      return z
   end

   function fmpq(a::Int, b::Int)
      b == 0 && throw(DivideError())
      z = new()
      if b == typemin(Int) || (b < 0 && a == typemin(Int))
         bz = -ZZ(b)
         az = -ZZ(a)
         ccall((:fmpq_init, libflint), Nothing, (Ref{fmpq},), z)
         ccall((:fmpq_set_fmpz_frac, libflint), Nothing,
	       (Ref{fmpq}, Ref{fmpz}, Ref{fmpz}), z, az, bz)
      else
         if b < 0 # Flint requires positive denominator
            b = -b
            a = -a
         end
         ccall((:fmpq_init, libflint), Nothing, (Ref{fmpq},), z)
         ccall((:fmpq_set_si, libflint), Nothing,
               (Ref{fmpq}, Int, Int), z, a, b)
      end
      finalizer(_fmpq_clear_fn, z)
      return z
   end

   function fmpq(a::Int)
      z = new()
      ccall((:fmpq_init, libflint), Nothing, (Ref{fmpq},), z)
      ccall((:fmpq_set_si, libflint), Nothing,
            (Ref{fmpq}, Int, Int), z, a, 1)
      finalizer(_fmpq_clear_fn, z)
      return z
   end

   fmpq(a::fmpq) = a
end

_fmpq_clear_fn(a::fmpq) = ccall((:fmpq_clear, libflint), Nothing, (Ref{fmpq},), a)

###############################################################################
#
#   FmpzPolyRing / fmpz_poly
#
###############################################################################

mutable struct FmpzPolyRing <: PolyRing{fmpz}
   base_ring::FlintIntegerRing
   S::Symbol

   function FmpzPolyRing(R::FlintIntegerRing, s::Symbol, cached::Bool = true)
      if cached && haskey(FmpzPolyID, (R, s))
         return FmpzPolyID[R, s]
      else
         z = new(R, s)
         if cached
            FmpzPolyID[R, s] = z
         end
         return z
      end
   end
end

const FmpzPolyID = Dict{Tuple{FlintIntegerRing, Symbol}, FmpzPolyRing}()

mutable struct fmpz_poly <: PolyElem{fmpz}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   parent::FmpzPolyRing

   function fmpz_poly()
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing, (Ref{fmpz_poly},), z)
      finalizer(_fmpz_poly_clear_fn, z)
      return z
   end

   function fmpz_poly(a::Vector{fmpz})
      z = new()
      ccall((:fmpz_poly_init2, libflint), Nothing,
            (Ref{fmpz_poly}, Int), z, length(a))
      for i = 1:length(a)
         ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
                     (Ref{fmpz_poly}, Int, Ref{fmpz}), z, i - 1, a[i])
      end
      finalizer(_fmpz_poly_clear_fn, z)
      return z
   end

   function fmpz_poly(a::Int)
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing, (Ref{fmpz_poly},), z)
      ccall((:fmpz_poly_set_si, libflint), Nothing, (Ref{fmpz_poly}, Int), z, a)
      finalizer(_fmpz_poly_clear_fn, z)
      return z
   end

   function fmpz_poly(a::fmpz)
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing, (Ref{fmpz_poly},), z)
      ccall((:fmpz_poly_set_fmpz, libflint), Nothing,
            (Ref{fmpz_poly}, Ref{fmpz}), z, a)
      finalizer(_fmpz_poly_clear_fn, z)
      return z
   end

   function fmpz_poly(a::fmpz_poly)
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing, (Ref{fmpz_poly},), z)
      ccall((:fmpz_poly_set, libflint), Nothing,
            (Ref{fmpz_poly}, Ref{fmpz_poly}), z, a)
      finalizer(_fmpz_poly_clear_fn, z)
      return z
   end
end

function _fmpz_poly_clear_fn(a::fmpz_poly)
   ccall((:fmpz_poly_clear, libflint), Nothing, (Ref{fmpz_poly},), a)
end

mutable struct fmpz_poly_factor
  d::Int # fmpz
  p::Ptr{fmpz_poly} # array of flint fmpz_poly_struct's
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
#   FmpqPolyRing / fmpq_poly
#
###############################################################################

mutable struct FmpqPolyRing <: PolyRing{fmpq}
   base_ring::FlintRationalField
   S::Symbol

   function FmpqPolyRing(R::FlintRationalField, s::Symbol, cached::Bool = true)
      if cached && haskey(FmpqPolyID, (R, s))
         return FmpqPolyID[R, s]
      else
         z = new(R, s)
         if cached
            FmpqPolyID[R, s] = z
         end
         return z
      end
   end
end

const FmpqPolyID = Dict{Tuple{FlintRationalField, Symbol}, FmpqPolyRing}()

mutable struct fmpq_poly <: PolyElem{fmpq}
   coeffs::Ptr{Int}
   den::Int
   alloc::Int
   length::Int
   parent::FmpqPolyRing

   function fmpq_poly()
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{fmpq_poly},), z)
      finalizer(_fmpq_poly_clear_fn, z)
      return z
   end

   function fmpq_poly(a::Vector{fmpq})
      z = new()
      ccall((:fmpq_poly_init2, libflint), Nothing,
            (Ref{fmpq_poly}, Int), z, length(a))
      for i = 1:length(a)
         ccall((:fmpq_poly_set_coeff_fmpq, libflint), Nothing,
                     (Ref{fmpq_poly}, Int, Ref{fmpq}), z, i - 1, a[i])
      end
      finalizer(_fmpq_poly_clear_fn, z)
      return z
   end

   function fmpq_poly(a::Int)
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{fmpq_poly},), z)
      ccall((:fmpq_poly_set_si, libflint), Nothing, (Ref{fmpq_poly}, Int), z, a)
      finalizer(_fmpq_poly_clear_fn, z)
      return z
   end

   function fmpq_poly(a::fmpz)
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{fmpq_poly},), z)
      ccall((:fmpq_poly_set_fmpz, libflint), Nothing,
            (Ref{fmpq_poly}, Ref{fmpz}), z, a)
      finalizer(_fmpq_poly_clear_fn, z)
      return z
   end

   function fmpq_poly(a::fmpq)
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{fmpq_poly},), z)
      ccall((:fmpq_poly_set_fmpq, libflint), Nothing,
            (Ref{fmpq_poly}, Ref{fmpq}), z, a)
      finalizer(_fmpq_poly_clear_fn, z)
      return z
   end

   function fmpq_poly(a::fmpz_poly)
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{fmpq_poly},), z)
      ccall((:fmpq_poly_set_fmpz_poly, libflint), Nothing,
            (Ref{fmpq_poly}, Ref{fmpz_poly}), z, a)
      finalizer(_fmpq_poly_clear_fn, z)
      return z
   end

   function fmpq_poly(a::fmpq_poly)
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{fmpq_poly},), z)
      ccall((:fmpq_poly_set, libflint), Nothing,
            (Ref{fmpq_poly}, Ref{fmpq_poly}), z, a)
      finalizer(_fmpq_poly_clear_fn, z)
      return z
   end
end

function _fmpq_poly_clear_fn(a::fmpq_poly)
   ccall((:fmpq_poly_clear, libflint), Nothing, (Ref{fmpq_poly},), a)
end

###############################################################################
#
#   NmodRing / nmod
#
###############################################################################

mutable struct NmodRing <: Ring
   n::UInt
   ninv::UInt

   function NmodRing(n::UInt, cached::Bool=true)
      if cached && haskey(NmodRingID, n)
         return NmodRingID[n]
      else
         ninv = ccall((:n_preinvert_limb, libflint), UInt, (UInt,), n)
         z = new(n, ninv)
         if cached
            NmodRingID[n] = z
         end
         return z
      end
   end
end

const NmodRingID = Dict{UInt, NmodRing}()

struct nmod <: ResElem{UInt}
   data::UInt
   parent::NmodRing
end

################################################################################
#
#   GaloisField / gfp
#
###############################################################################

mutable struct GaloisField <: FinField
   n::UInt
   ninv::UInt
   @declare_other

   function GaloisField(n::UInt, cached::Bool=true)
      if cached && haskey(GaloisFieldID, n)
         return GaloisFieldID[n]
      else
         ninv = ccall((:n_preinvert_limb, libflint), UInt, (UInt,), n)
         z = new(n, ninv)
         if cached
            GaloisFieldID[n] = z
         end
         return z
      end
   end
end

const GaloisFieldID = Dict{UInt, GaloisField}()

struct gfp_elem <: FinFieldElem
   data::UInt
   parent::GaloisField
end

###############################################################################
#
#   FmpzModRing / fmpz_mod
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
      return new()
   end
end

mutable struct FmpzModRing <: Ring
   n::fmpz
   ninv::fmpz_mod_ctx_struct

   function FmpzModRing(n::fmpz, cached::Bool=true)
      if cached && haskey(FmpzModRingID, n)
         return FmpzModRingID[n]
      else
         ninv = fmpz_mod_ctx_struct()
         ccall((:fmpz_mod_ctx_init, libflint), Nothing, (Ref{fmpz_mod_ctx_struct}, Ref{fmpz}), ninv, n)
         z = new(n, ninv)
         if cached
            FmpzModRingID[n] = z
         end
         return z
      end
   end
end

const FmpzModRingID = Dict{fmpz, FmpzModRing}()

struct fmpz_mod <: ResElem{fmpz}
   data::fmpz
   parent::FmpzModRing
end

###############################################################################
#
#   GaloisFmpzField / gfp_fmpz_elem
#
###############################################################################

mutable struct GaloisFmpzField <: FinField
   n::fmpz
   ninv::fmpz_mod_ctx_struct
   @declare_other

   function GaloisFmpzField(n::fmpz, cached::Bool=true)
      if cached && haskey(GaloisFmpzFieldID, n)
         return GaloisFmpzFieldID[n]
      else
         ninv = fmpz_mod_ctx_struct()
         ccall((:fmpz_mod_ctx_init, libflint), Nothing,
               (Ref{fmpz_mod_ctx_struct}, Ref{fmpz}), ninv, n)
         z = new(n, ninv)
         if cached
            GaloisFmpzFieldID[n] = z
         end
         return z
      end
   end
end

const GaloisFmpzFieldID = Dict{fmpz, GaloisFmpzField}()

struct gfp_fmpz_elem <: FinFieldElem
   data::fmpz
   parent::GaloisFmpzField
end

###############################################################################
#
#   NmodPolyRing / nmod_poly
#
###############################################################################

mutable struct NmodPolyRing <: PolyRing{nmod}
  base_ring::NmodRing
  S::Symbol
  n::UInt

  function NmodPolyRing(R::NmodRing, s::Symbol, cached::Bool = true)
    m = UInt(modulus(R))
    if cached && haskey(NmodPolyRingID, (R, s))
       return NmodPolyRingID[R, s]
    else
       z = new(R, s, m)
       if cached
          NmodPolyRingID[R, s] = z
       end
       return z
    end
  end
end

const NmodPolyRingID = Dict{Tuple{NmodRing, Symbol}, NmodPolyRing}()

mutable struct nmod_poly <: PolyElem{nmod}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   mod_n::UInt
   mod_ninv::UInt
   mod_norm::UInt
   parent::NmodPolyRing

   function nmod_poly(n::UInt)
      z = new()
      ccall((:nmod_poly_init, libflint), Nothing, (Ref{nmod_poly}, UInt), z, n)
      finalizer(_nmod_poly_clear_fn, z)
      return z
   end

   function nmod_poly(n::UInt, a::UInt)
      z = new()
      ccall((:nmod_poly_init, libflint), Nothing, (Ref{nmod_poly}, UInt), z, n)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
              (Ref{nmod_poly}, Int, UInt), z, 0, a)
      finalizer(_nmod_poly_clear_fn, z)
      return z
   end

   function nmod_poly(n::UInt, a::Int)
      z = new()
      ccall((:nmod_poly_init, libflint), Nothing, (Ref{nmod_poly}, UInt), z, n)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
              (Ref{nmod_poly}, Int, UInt), z, 0, mod(a, n))
      finalizer(_nmod_poly_clear_fn, z)
      return z
   end

   function nmod_poly(n::UInt, arr::Vector{fmpz})
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_poly}, UInt, Int), z, n, length(arr))
      for i in 1:length(arr)
         tt = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), arr[i], n)
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
              (Ref{nmod_poly}, Int, UInt), z, i - 1, tt)
      end
      finalizer(_nmod_poly_clear_fn, z)
      return z
   end

   function nmod_poly(n::UInt, arr::Vector{UInt})
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_poly}, UInt, Int), z, n, length(arr))
      for i in 1:length(arr)
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
              (Ref{nmod_poly}, Int, UInt), z, i - 1, arr[i])
      end
      finalizer(_nmod_poly_clear_fn, z)
      return z
   end

   function nmod_poly(n::UInt, arr::Vector{nmod})
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_poly}, UInt, Int), z, n, length(arr))
      for i in 1:length(arr)
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
              (Ref{nmod_poly}, Int, UInt), z, i-1, arr[i].data)
      end
      finalizer(_nmod_poly_clear_fn, z)
      return z
   end

   function nmod_poly(n::UInt, f::fmpz_poly)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_poly}, UInt, Int), z, n, length(f))
      ccall((:fmpz_poly_get_nmod_poly, libflint), Nothing,
            (Ref{nmod_poly}, Ref{fmpz_poly}), z, f)
      finalizer(_nmod_poly_clear_fn, z)
      return z
   end

   function nmod_poly(n::UInt, f::nmod_poly)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_poly}, UInt, Int), z, n, length(f))
      ccall((:nmod_poly_set, libflint), Nothing,
            (Ref{nmod_poly}, Ref{nmod_poly}), z, f)
      finalizer(_nmod_poly_clear_fn, z)
      return z
   end
end

function _nmod_poly_clear_fn(x::nmod_poly)
  ccall((:nmod_poly_clear, libflint), Nothing, (Ref{nmod_poly}, ), x)
end

mutable struct nmod_poly_factor
  poly::Ptr{nmod_poly}  # array of flint nmod_poly_struct's
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
#   GFPPolyRing / gfp_poly
#
###############################################################################

mutable struct GFPPolyRing <: PolyRing{gfp_elem}
  base_ring::GaloisField
  S::Symbol
  n::UInt

  function GFPPolyRing(R::GaloisField, s::Symbol, cached::Bool = true)
    m = UInt(modulus(R))
    if cached && haskey(GFPPolyRingID, (R, s))
       return GFPPolyRingID[R, s]
    else
       z = new(R, s, m)
       if cached
          GFPPolyRingID[R, s] = z
       end
       return z
    end
  end
end

const GFPPolyRingID = Dict{Tuple{GaloisField, Symbol}, GFPPolyRing}()

mutable struct gfp_poly <: PolyElem{gfp_elem}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   mod_n::UInt
   mod_ninv::UInt
   mod_norm::UInt
   parent::GFPPolyRing

   function gfp_poly(n::UInt)
      z = new()
      ccall((:nmod_poly_init, libflint), Nothing, (Ref{gfp_poly}, UInt), z, n)
      finalizer(_gfp_poly_clear_fn, z)
      return z
   end

   function gfp_poly(n::UInt, a::UInt)
      z = new()
      ccall((:nmod_poly_init, libflint), Nothing, (Ref{gfp_poly}, UInt), z, n)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
              (Ref{gfp_poly}, Int, UInt), z, 0, a)
      finalizer(_gfp_poly_clear_fn, z)
      return z
   end

   function gfp_poly(n::UInt, a::Int)
      z = new()
      ccall((:nmod_poly_init, libflint), Nothing, (Ref{gfp_poly}, UInt), z, n)
      ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
              (Ref{gfp_poly}, Int, UInt), z, 0, mod(a, n))
      finalizer(_gfp_poly_clear_fn, z)
      return z
   end

   function gfp_poly(n::UInt, arr::Vector{fmpz})
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_poly}, UInt, Int), z, n, length(arr))
      for i in 1:length(arr)
         tt = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), arr[i], n)
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
              (Ref{gfp_poly}, Int, UInt), z, i - 1, tt)
      end
      finalizer(_gfp_poly_clear_fn, z)
      return z
   end

   function gfp_poly(n::UInt, arr::Vector{UInt})
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_poly}, UInt, Int), z, n, length(arr))
      for i in 1:length(arr)
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
              (Ref{gfp_poly}, Int, UInt), z, i - 1, arr[i])
      end
      finalizer(_gfp_poly_clear_fn, z)
      return z
   end

   function gfp_poly(n::UInt, arr::Vector{gfp_elem})
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_poly}, UInt, Int), z, n, length(arr))
      for i in 1:length(arr)
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
              (Ref{gfp_poly}, Int, UInt), z, i-1, arr[i].data)
      end
      finalizer(_gfp_poly_clear_fn, z)
      return z
   end

   function gfp_poly(n::UInt, f::fmpz_poly)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_poly}, UInt, Int), z, n, length(f))
      ccall((:fmpz_poly_get_nmod_poly, libflint), Nothing,
            (Ref{gfp_poly}, Ref{fmpz_poly}), z, f)
      finalizer(_gfp_poly_clear_fn, z)
      return z
   end

   function gfp_poly(n::UInt, f::gfp_poly)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_poly}, UInt, Int), z, n, length(f))
      ccall((:nmod_poly_set, libflint), Nothing,
            (Ref{gfp_poly}, Ref{gfp_poly}), z, f)
      finalizer(_gfp_poly_clear_fn, z)
      return z
   end
end

function _gfp_poly_clear_fn(x::gfp_poly)
  ccall((:nmod_poly_clear, libflint), Nothing, (Ref{gfp_poly}, ), x)
end

mutable struct gfp_poly_factor
  poly::Ptr{gfp_poly}  # array of flint nmod_poly_struct's
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
#   FmpzModPolyRing / fmpz_mod_poly
#
###############################################################################

mutable struct FmpzModPolyRing <: PolyRing{fmpz_mod}
  base_ring::FmpzModRing
  S::Symbol
  n::fmpz

  function FmpzModPolyRing(R::FmpzModRing, s::Symbol, cached::Bool = true)
    if cached && haskey(FmpzModPolyRingID, (R, s))
       return FmpzModPolyRingID[R, s]
    else
       z = new(R, s, modulus(R))
       if cached
          FmpzModPolyRingID[R ,s] = z
       end
       return z
    end
  end
end

const FmpzModPolyRingID = Dict{Tuple{FmpzModRing, Symbol}, FmpzModPolyRing}()

mutable struct fmpz_mod_poly <: PolyElem{fmpz_mod}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   # end of flint struct

   parent::FmpzModPolyRing

   function fmpz_mod_poly(n::fmpz_mod_ctx_struct)
      z = new()
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
            z, n)
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end


   function fmpz_mod_poly(R::FmpzModRing)
      return fmpz_mod_poly(R.ninv)
   end

   function fmpz_mod_poly(n::fmpz_mod_ctx_struct, a::fmpz)
      z = new()
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
            z, n)
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Int, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
            z, 0, a, n)
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function fmpz_mod_poly(R::FmpzModRing, a::fmpz)
      return fmpz_mod_poly(R.ninv, a)
   end

   function fmpz_mod_poly(n::fmpz_mod_ctx_struct, a::UInt)
      z = new()
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
            z, n)
      ccall((:fmpz_mod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Int, UInt, Ref{fmpz_mod_ctx_struct}),
            z, 0, a, n)
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function fmpz_mod_poly(R::FmpzModRing, a::UInt)
      return fmpz_mod_poly(R.ninv, a)
   end

   function fmpz_mod_poly(n::fmpz_mod_ctx_struct, arr::Vector{fmpz})
      length(arr) == 0 && error("Array must have length > 0")
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Int, Ref{fmpz_mod_ctx_struct}),
            z, length(arr), n)
      for i in 1:length(arr)
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz_mod_poly}, Int, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
               z, i - 1, arr[i], n)
      end
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function fmpz_mod_poly(R::FmpzModRing, arr::Vector{fmpz})
      return fmpz_mod_poly(R.ninv, arr)
   end

   function fmpz_mod_poly(n::fmpz_mod_ctx_struct, arr::Vector{fmpz_mod})
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Int, Ref{fmpz_mod_ctx_struct}),
            z, length(arr), n)
      for i in 1:length(arr)
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz_mod_poly}, Int, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
               z, i - 1, arr[i].data, n)
      end
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function fmpz_mod_poly(R::FmpzModRing, arr::Vector{fmpz_mod})
      return fmpz_mod_poly(R.ninv, arr)
   end

   function fmpz_mod_poly(n::fmpz_mod_ctx_struct, f::fmpz_poly)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Int, Ref{fmpz_mod_ctx_struct}),
            z, length(f), n)
      ccall((:fmpz_mod_poly_set_fmpz_poly, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Ref{fmpz_poly}, Ref{fmpz_mod_ctx_struct}),
            z, f, n)
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function fmpz_mod_poly(R::FmpzModRing, f::fmpz_poly)
      return fmpz_mod_poly(R.ninv, f)
   end

   function fmpz_mod_poly(n::fmpz_mod_ctx_struct, f::fmpz_mod_poly)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Int, Ref{fmpz_mod_ctx_struct}),
            z, length(f), n)
      ccall((:fmpz_mod_poly_set, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
            z, f, n)
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function fmpz_mod_poly(R::FmpzModRing, f::fmpz_mod_poly)
      return fmpz_mod_poly(R.ninv, f)
   end
end

function _fmpz_mod_poly_clear_fn(x::fmpz_mod_poly)
   ccall((:fmpz_mod_poly_clear, libflint), Nothing,
         (Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
         x, x.parent.base_ring.ninv)
end

mutable struct fmpz_mod_poly_factor
   poly::Ptr{fmpz_mod_poly}
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

   function fmpz_mod_poly_factor(R::FmpzModRing)
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
#   GFPFmpzPolyRing / gfp_fmpz_poly
#
###############################################################################

mutable struct GFPFmpzPolyRing <: PolyRing{gfp_fmpz_elem}
  base_ring::GaloisFmpzField
  S::Symbol
  n::fmpz

  function GFPFmpzPolyRing(R::GaloisFmpzField, s::Symbol, cached::Bool = true)
    m = modulus(R)
    if cached && haskey(GFPFmpzPolyRingID, (R, s))
       return GFPFmpzPolyRingID[R, s]
    else
       z = new(R, s, m)
       if cached
          GFPFmpzPolyRingID[R, s] = z
       end
       return z
    end
  end
end

const GFPFmpzPolyRingID = Dict{Tuple{GaloisFmpzField, Symbol}, GFPFmpzPolyRing}()

mutable struct gfp_fmpz_poly <: PolyElem{gfp_fmpz_elem}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   # end flint struct

   parent::GFPFmpzPolyRing

   function gfp_fmpz_poly(n::fmpz_mod_ctx_struct)
      z = new()
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{gfp_fmpz_poly}, Ref{fmpz_mod_ctx_struct}),
            z, n)
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function gfp_fmpz_poly(R::GaloisFmpzField)
      return gfp_fmpz_poly(R.ninv)
   end

   function gfp_fmpz_poly(n::fmpz_mod_ctx_struct, a::fmpz)
      z = new()
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{gfp_fmpz_poly}, Ref{fmpz_mod_ctx_struct}),
            z, n)
      ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{gfp_fmpz_poly}, Int, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
            z, 0, a, n)
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function gfp_fmpz_poly(R::GaloisFmpzField, a::fmpz)
      return gfp_fmpz_poly(R.ninv, a)
   end

   function gfp_fmpz_poly(n::fmpz_mod_ctx_struct, a::UInt)
      z = new()
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}),
            z, n)
      ccall((:fmpz_mod_poly_set_coeff_ui, libflint), Nothing,
            (Ref{fmpz_mod_poly}, Int, UInt, Ref{fmpz_mod_ctx_struct}),
            z, 0, a, n)
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function gfp_fmpz_poly(R::GaloisFmpzField, a::UInt)
      return gfp_fmpz_poly(R.ninv, a)
   end

   function gfp_fmpz_poly(n::fmpz_mod_ctx_struct, arr::Vector{fmpz})
      length(arr) == 0 && error("Array must have length > 0")
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{gfp_fmpz_poly}, Int, Ref{fmpz_mod_ctx_struct}),
            z, length(arr), n)
      for i in 1:length(arr)
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{gfp_fmpz_poly}, Int, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
               z, i - 1, arr[i], n)
      end
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function gfp_fmpz_poly(R::GaloisFmpzField, arr::Vector{fmpz})
      gfp_fmpz_poly(R.ninv, arr)
   end

   function gfp_fmpz_poly(n::fmpz_mod_ctx_struct, arr::Vector{gfp_fmpz_elem})
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{gfp_fmpz_poly}, Int, Ref{fmpz_mod_ctx_struct}),
            z, length(arr), n)
      for i in 1:length(arr)
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{gfp_fmpz_poly}, Int, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
               z, i - 1, arr[i].data, n)
      end
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function gfp_fmpz_poly(R::GaloisFmpzField, arr::Vector{gfp_fmpz_elem})
      return gfp_fmpz_poly(R.ninv, arr)
   end

   function gfp_fmpz_poly(n::fmpz_mod_ctx_struct, f::fmpz_poly)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{gfp_fmpz_poly}, Int, Ref{fmpz_mod_ctx_struct}),
            z, length(f), n)
      ccall((:fmpz_mod_poly_set_fmpz_poly, libflint), Nothing,
            (Ref{gfp_fmpz_poly}, Ref{fmpz_poly}, Ref{fmpz_mod_ctx_struct}),
            z, f, n)
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function gfp_fmpz_poly(R::GaloisFmpzField, f::fmpz_poly)
      return gfp_fmpz_poly(R.ninv, f)
   end

   function gfp_fmpz_poly(n::fmpz_mod_ctx_struct, f::gfp_fmpz_poly)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{gfp_fmpz_poly}, Int, Ref{fmpz_mod_ctx_struct}),
            z, length(f), n)
      ccall((:fmpz_mod_poly_set, libflint), Nothing,
            (Ref{gfp_fmpz_poly}, Ref{gfp_fmpz_poly}, Ref{fmpz_mod_ctx_struct}),
            z, f, n)
      finalizer(_fmpz_mod_poly_clear_fn, z)
      return z
   end

   function gfp_fmpz_poly(R::GaloisFmpzField, f::gfp_fmpz_poly)
      return gfp_fmpz_poly(R.ninv, f)
   end
end

function _fmpz_mod_poly_clear_fn(x::gfp_fmpz_poly)
   ccall((:fmpz_mod_poly_clear, libflint), Nothing,
         (Ref{gfp_fmpz_poly}, Ref{fmpz_mod_ctx_struct}),
         x, x.parent.base_ring.ninv)
end

mutable struct gfp_fmpz_poly_factor
   poly::Ptr{gfp_fmpz_poly}
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

   function gfp_fmpz_poly_factor(R::GaloisFmpzField)
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
#   FmpzMPolyRing / fmpz_mpoly
#
###############################################################################

const flint_orderings = [:lex, :deglex, :degrevlex]

mutable struct FmpzMPolyRing <: MPolyRing{fmpz}
   nvars::Int
   nfields::Cint
   ord::Int
   deg::Cint
   rev::Cint
   lut::NTuple{Base.GMP.BITS_PER_LIMB, Int}
   lut1::NTuple{Base.GMP.BITS_PER_LIMB, UInt8}

   base_ring::FlintIntegerRing
   S::Vector{Symbol}

   function FmpzMPolyRing(s::Vector{Symbol}, S::Symbol, cached::Bool = true)
      if cached && haskey(FmpzMPolyID, (s, S))
         return FmpzMPolyID[s, S]
      else
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
               (Ref{FmpzMPolyRing}, Int, Int),
               z, length(s), ord)
         z.base_ring = FlintZZ
         z.S = s
         finalizer(_fmpz_mpoly_ctx_clear_fn, z)
         if cached
            FmpzMPolyID[s, S] = z
         end
         return z
      end
   end
end

function _fmpz_mpoly_ctx_clear_fn(a::FmpzMPolyRing)
   ccall((:fmpz_mpoly_ctx_clear, libflint), Nothing,
           (Ref{FmpzMPolyRing},), a)
end

const FmpzMPolyID = Dict{Tuple{Vector{Symbol}, Symbol}, FmpzMPolyRing}()

mutable struct fmpz_mpoly <: MPolyElem{fmpz}
   coeffs::Ptr{Nothing}
   exps::Ptr{Nothing}
   alloc::Int
   length::Int
   bits::Int
   # end flint struct

   parent::FmpzMPolyRing

   function fmpz_mpoly(ctx::FmpzMPolyRing)
      z = new()
      ccall((:fmpz_mpoly_init, libflint), Nothing,
            (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing},), z, ctx)
      z.parent = ctx
      finalizer(_fmpz_mpoly_clear_fn, z)
      return z
   end

   function fmpz_mpoly(ctx::FmpzMPolyRing, a::Vector{fmpz}, b::Vector{Vector{UInt}})
      z = new()
      ccall((:fmpz_mpoly_init2, libflint), Nothing,
            (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_fmpz_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:fmpz_mpoly_push_term_fmpz_ui, libflint), Nothing,
               (Ref{fmpz_mpoly}, Ref{fmpz}, Ptr{UInt}, Ref{FmpzMPolyRing}),
               z, a[i], b[i], ctx)
       end

       ccall((:fmpz_mpoly_sort_terms, libflint), Nothing,
             (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), z, ctx)
       ccall((:fmpz_mpoly_combine_like_terms, libflint), Nothing,
             (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), z, ctx)
       return z
   end

   function fmpz_mpoly(ctx::FmpzMPolyRing, a::Vector{fmpz}, b::Vector{Vector{Int}})
      z = new()
      ccall((:fmpz_mpoly_init2, libflint), Nothing,
            (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_fmpz_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:fmpz_mpoly_push_term_fmpz_ui, libflint), Nothing,
               (Ref{fmpz_mpoly}, Ref{fmpz}, Ptr{Int}, Ref{FmpzMPolyRing}),
               z, a[i], b[i], ctx)
       end

       ccall((:fmpz_mpoly_sort_terms, libflint), Nothing,
             (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), z, ctx)
       ccall((:fmpz_mpoly_combine_like_terms, libflint), Nothing,
             (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), z, ctx)
       return z
   end

   function fmpz_mpoly(ctx::FmpzMPolyRing, a::Vector{fmpz}, b::Vector{Vector{fmpz}})
      z = new()
      ccall((:fmpz_mpoly_init2, libflint), Nothing,
            (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_fmpz_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:fmpz_mpoly_push_term_fmpz_fmpz, libflint), Nothing,
               (Ref{fmpz_mpoly}, Ref{fmpz}, Ptr{Ref{fmpz}}, Ref{FmpzMPolyRing}),
               z, a[i], b[i], ctx)
       end

       ccall((:fmpz_mpoly_sort_terms, libflint), Nothing,
             (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), z, ctx)
       ccall((:fmpz_mpoly_combine_like_terms, libflint), Nothing,
             (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), z, ctx)
       return z
   end

   function fmpz_mpoly(ctx::FmpzMPolyRing, a::fmpz)
      z = new()
      ccall((:fmpz_mpoly_init, libflint), Nothing,
            (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing},), z, ctx)
      ccall((:fmpz_mpoly_set_fmpz, libflint), Nothing,
            (Ref{fmpz_mpoly}, Ref{fmpz}, Ref{FmpzMPolyRing}), z, a, ctx)
      z.parent = ctx
      finalizer(_fmpz_mpoly_clear_fn, z)
      return z
   end

   function fmpz_mpoly(ctx::FmpzMPolyRing, a::Int)
      z = new()
      ccall((:fmpz_mpoly_init, libflint), Nothing,
            (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing},), z, ctx)
      ccall((:fmpz_mpoly_set_si, libflint), Nothing,
            (Ref{fmpz_mpoly}, Int, Ref{FmpzMPolyRing}), z, a, ctx)
      finalizer(_fmpz_mpoly_clear_fn, z)
      z.parent = ctx
      return z
   end

   function fmpz_mpoly(ctx::FmpzMPolyRing, a::UInt)
      z = new()
      ccall((:fmpz_mpoly_init, libflint), Nothing,
            (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing},), z, ctx)
      ccall((:fmpz_mpoly_set_ui, libflint), Nothing,
            (Ref{fmpz_mpoly}, UInt, Ref{FmpzMPolyRing}), z, a, ctx)
      finalizer(_fmpz_mpoly_clear_fn, z)
      z.parent = ctx
      return z
   end
end

function _fmpz_mpoly_clear_fn(a::fmpz_mpoly)
   ccall((:fmpz_mpoly_clear, libflint), Nothing,
          (Ref{fmpz_mpoly}, Ref{FmpzMPolyRing}), a, a.parent)
end

mutable struct fmpz_mpoly_factor
   constant::Int
   constant_den::Int
   poly::Ptr{Nothing}
   exp::Ptr{Nothing}
   num::Int
   alloc::Int
   # end flint struct

   parent::FmpzMPolyRing

   function fmpz_mpoly_factor(ctx::FmpzMPolyRing)
      z = new()
      ccall((:fmpz_mpoly_factor_init, libflint), Nothing,
            (Ref{fmpz_mpoly_factor}, Ref{FmpzMPolyRing}),
            z, ctx)
      z.parent = ctx
      finalizer(_fmpz_mpoly_factor_clear_fn, z)
      return z
  end
end

function _fmpz_mpoly_factor_clear_fn(f::fmpz_mpoly_factor)
   ccall((:fmpz_mpoly_factor_clear, libflint), Nothing,
         (Ref{fmpz_mpoly_factor}, Ref{FmpzMPolyRing}),
         f, f.parent)
end

###############################################################################
#
#   FmpqMPolyRing / fmpq_mpoly
#
###############################################################################

mutable struct FmpqMPolyRing <: MPolyRing{fmpq}
   nvars::Int
   nfields::Cint
   ord::Int
   deg::Cint
   rev::Cint
   lut::NTuple{Base.GMP.BITS_PER_LIMB, Int}
   lut1::NTuple{Base.GMP.BITS_PER_LIMB, UInt8}

   base_ring::FlintRationalField
   S::Vector{Symbol}

   function FmpqMPolyRing(s::Vector{Symbol}, S::Symbol, cached::Bool = true)
      if cached && haskey(FmpqMPolyID, (s, S))
         return FmpqMPolyID[s, S]
      else
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
               (Ref{FmpqMPolyRing}, Int, Int),
               z, length(s), ord)
         z.base_ring = FlintQQ
         z.S = s
         finalizer(_fmpq_mpoly_ctx_clear_fn, z)
         if cached
            FmpqMPolyID[s, S] = z
         end
         return z
      end
   end
end

function _fmpq_mpoly_ctx_clear_fn(a::FmpqMPolyRing)
  ccall((:fmpq_mpoly_ctx_clear, libflint), Nothing,
          (Ref{FmpqMPolyRing},), a)
end

const FmpqMPolyID = Dict{Tuple{Vector{Symbol}, Symbol}, FmpqMPolyRing}()

mutable struct fmpq_mpoly <: MPolyElem{fmpq}
   content_num::Int
   content_den::Int
   coeffs::Ptr{Nothing}
   exps::Ptr{Nothing}
   alloc::Int
   length::Int
   bits::Int

   parent::FmpqMPolyRing

   function fmpq_mpoly(ctx::FmpqMPolyRing)
      z = new()
      ccall((:fmpq_mpoly_init, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing},), z, ctx)
      z.parent = ctx
      finalizer(_fmpq_mpoly_clear_fn, z)
      return z
   end

   function fmpq_mpoly(ctx::FmpqMPolyRing, a::Vector{fmpq}, b::Vector{Vector{UInt}})
      z = new()
      ccall((:fmpq_mpoly_init2, libflint), Nothing,
            (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_fmpq_mpoly_clear_fn, z)

      for i in 1:length(a)
        ccall((:fmpq_mpoly_push_term_fmpq_ui, libflint), Nothing,
              (Ref{fmpq_mpoly}, Ref{fmpq}, Ptr{UInt}, Ref{FmpqMPolyRing}),
              z, a[i], b[i], ctx)
      end

      ccall((:fmpq_mpoly_sort_terms, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), z, ctx)
      ccall((:fmpq_mpoly_combine_like_terms, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), z, ctx)
      return z
   end

   function fmpq_mpoly(ctx::FmpqMPolyRing, a::Vector{fmpq}, b::Vector{Vector{Int}})
      z = new()
      ccall((:fmpq_mpoly_init2, libflint), Nothing,
            (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_fmpq_mpoly_clear_fn, z)

      for i in 1:length(a)
        ccall((:fmpq_mpoly_push_term_fmpq_ui, libflint), Nothing,
              (Ref{fmpq_mpoly}, Ref{fmpq}, Ptr{Int}, Ref{FmpqMPolyRing}),
              z, a[i], b[i], ctx)
      end

      ccall((:fmpq_mpoly_sort_terms, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), z, ctx)
      ccall((:fmpq_mpoly_combine_like_terms, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), z, ctx)
      return z
   end

   function fmpq_mpoly(ctx::FmpqMPolyRing, a::Vector{fmpq}, b::Vector{Vector{fmpz}})
      z = new()
      ccall((:fmpq_mpoly_init2, libflint), Nothing,
            (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_fmpq_mpoly_clear_fn, z)

      for i in 1:length(a)
        ccall((:fmpq_mpoly_push_term_fmpq_fmpz, libflint), Nothing,
              (Ref{fmpq_mpoly}, Ref{fmpq}, Ptr{Ref{fmpz}}, Ref{FmpqMPolyRing}),
              z, a[i], b[i], ctx)
      end

      ccall((:fmpq_mpoly_sort_terms, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), z, ctx)
      ccall((:fmpq_mpoly_combine_like_terms, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), z, ctx)
      return z
   end

   function fmpq_mpoly(ctx::FmpqMPolyRing, a::fmpz)
      z = new()
      ccall((:fmpq_mpoly_init, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing},), z, ctx)
      ccall((:fmpq_mpoly_set_fmpz, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{fmpz}, Ref{FmpqMPolyRing}), z, a, ctx)
      z.parent = ctx
      finalizer(_fmpq_mpoly_clear_fn, z)
      return z
   end

   function fmpq_mpoly(ctx::FmpqMPolyRing, a::fmpq)
      z = new()
      ccall((:fmpq_mpoly_init, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing},), z, ctx)
      ccall((:fmpq_mpoly_set_fmpq, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{fmpq}, Ref{FmpqMPolyRing}), z, a, ctx)
      z.parent = ctx
      finalizer(_fmpq_mpoly_clear_fn, z)
      return z
   end

   function fmpq_mpoly(ctx::FmpqMPolyRing, a::Int)
      z = new()
      ccall((:fmpq_mpoly_init, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing},), z, ctx)
      ccall((:fmpq_mpoly_set_si, libflint), Nothing,
            (Ref{fmpq_mpoly}, Int, Ref{FmpqMPolyRing}), z, a, ctx)
      z.parent = ctx
      finalizer(_fmpq_mpoly_clear_fn, z)
      return z
   end

   function fmpq_mpoly(ctx::FmpqMPolyRing, a::UInt)
      z = new()
      ccall((:fmpq_mpoly_init, libflint), Nothing,
            (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing},), z, ctx)
      ccall((:fmpq_mpoly_set_ui, libflint), Nothing,
            (Ref{fmpq_mpoly}, UInt, Ref{FmpqMPolyRing}), z, a, ctx)
      z.parent = ctx
      finalizer(_fmpq_mpoly_clear_fn, z)
      return z
   end
end

function _fmpq_mpoly_clear_fn(a::fmpq_mpoly)
  ccall((:fmpq_mpoly_clear, libflint), Nothing,
          (Ref{fmpq_mpoly}, Ref{FmpqMPolyRing}), a, a.parent)
end

mutable struct fmpq_mpoly_factor
   constant_num::Int
   constant_den::Int
   poly::Ptr{Nothing}
   exp::Ptr{Nothing}
   num::Int
   alloc::Int
   # end flint struct

   parent::FmpqMPolyRing

   function fmpq_mpoly_factor(ctx::FmpqMPolyRing)
      z = new()
      ccall((:fmpq_mpoly_factor_init, libflint), Nothing,
            (Ref{fmpq_mpoly_factor}, Ref{FmpqMPolyRing}),
            z, ctx)
      z.parent = ctx
      finalizer(_fmpq_mpoly_factor_clear_fn, z)
      return z
  end
end

function _fmpq_mpoly_factor_clear_fn(f::fmpq_mpoly_factor)
   ccall((:fmpq_mpoly_factor_clear, libflint), Nothing,
         (Ref{fmpq_mpoly_factor}, Ref{FmpqMPolyRing}),
         f, f.parent)
end

###############################################################################
#
#   NmodMPolyRing / nmod_mpoly
#
###############################################################################

mutable struct NmodMPolyRing <: MPolyRing{nmod}
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

   base_ring::NmodRing
   S::Vector{Symbol}

   function NmodMPolyRing(R::NmodRing, s::Vector{Symbol}, S::Symbol, cached::Bool = true)
      if cached && haskey(NmodMPolyID, (R, s, S))
         return NmodMPolyID[R, s, S]
      else
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
               (Ref{NmodMPolyRing}, Int, Cint, UInt),
               z, length(s), ord, R.n)
         z.base_ring = R
         z.S = s
         finalizer(_nmod_mpoly_ctx_clear_fn, z)
         if cached
            NmodMPolyID[R, s, S] = z
         end
         return z
      end
   end
end

function _nmod_mpoly_ctx_clear_fn(a::NmodMPolyRing)
   ccall((:nmod_mpoly_ctx_clear, libflint), Nothing,
           (Ref{NmodMPolyRing},), a)
end

const NmodMPolyID = Dict{Tuple{NmodRing, Vector{Symbol}, Symbol}, NmodMPolyRing}()

mutable struct nmod_mpoly <: MPolyElem{nmod}
   coeffs::Ptr{Nothing}
   exps::Ptr{Nothing}
   length::Int
   bits::Int
   coeffs_alloc::Int
   exps_alloc::Int
   # end of flint struct

   parent::NmodMPolyRing

   function nmod_mpoly(ctx::NmodMPolyRing)
      z = new()
      ccall((:nmod_mpoly_init, libflint), Nothing,
            (Ref{nmod_mpoly}, Ref{NmodMPolyRing},), z, ctx)
      z.parent = ctx
      finalizer(_nmod_mpoly_clear_fn, z)
      return z
   end

   function nmod_mpoly(ctx::NmodMPolyRing, a::Vector{nmod}, b::Vector{Vector{UInt}})
      z = new()
      ccall((:nmod_mpoly_init2, libflint), Nothing,
            (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_nmod_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:nmod_mpoly_push_term_ui_ui, libflint), Nothing,
               (Ref{nmod_mpoly}, UInt, Ptr{UInt}, Ref{NmodMPolyRing}),
               z, a[i].data, b[i], ctx)
       end

       ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
             (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), z, ctx)
       ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
             (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), z, ctx)
       return z
   end

   function nmod_mpoly(ctx::NmodMPolyRing, a::Vector{nmod}, b::Vector{Vector{Int}})
      z = new()
      ccall((:nmod_mpoly_init2, libflint), Nothing,
            (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_nmod_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:nmod_mpoly_push_term_ui_ui, libflint), Nothing,
               (Ref{nmod_mpoly}, UInt, Ptr{Int}, Ref{NmodMPolyRing}),
               z, a[i].data, b[i], ctx)
       end

       ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
             (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), z, ctx)
       ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
             (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), z, ctx)
       return z
   end

   function nmod_mpoly(ctx::NmodMPolyRing, a::Vector{nmod}, b::Vector{Vector{fmpz}})
      z = new()
      ccall((:nmod_mpoly_init2, libflint), Nothing,
            (Ref{nmod_mpoly}, Int, Ref{NmodMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_nmod_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:nmod_mpoly_push_term_ui_fmpz, libflint), Nothing,
               (Ref{nmod_mpoly}, UInt, Ptr{Ref{fmpz}}, Ref{NmodMPolyRing}),
               z, a[i].data, b[i], ctx)
       end

       ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
             (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), z, ctx)
       ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
             (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), z, ctx)
       return z
   end

   function nmod_mpoly(ctx::NmodMPolyRing, a::UInt)
      z = new()
      ccall((:nmod_mpoly_init, libflint), Nothing,
            (Ref{nmod_mpoly}, Ref{NmodMPolyRing},), z, ctx)
      ccall((:nmod_mpoly_set_ui, libflint), Nothing,
            (Ref{nmod_mpoly}, UInt, Ref{NmodMPolyRing}), z, a, ctx)
      z.parent = ctx
      finalizer(_nmod_mpoly_clear_fn, z)
      return z
   end

   function nmod_mpoly(ctx::NmodMPolyRing, a::nmod)
      z = new()
      ccall((:nmod_mpoly_init, libflint), Nothing,
            (Ref{nmod_mpoly}, Ref{NmodMPolyRing},), z, ctx)
      ccall((:nmod_mpoly_set_ui, libflint), Nothing,
            (Ref{nmod_mpoly}, UInt, Ref{NmodMPolyRing}), z, a.data, ctx)
      finalizer(_nmod_mpoly_clear_fn, z)
      z.parent = ctx
      return z
   end
end

function _nmod_mpoly_clear_fn(a::nmod_mpoly)
   ccall((:nmod_mpoly_clear, libflint), Nothing,
          (Ref{nmod_mpoly}, Ref{NmodMPolyRing}), a, a.parent)
end

mutable struct nmod_mpoly_factor
   constant::UInt
   poly::Ptr{Nothing}
   exp::Ptr{Nothing}
   num::Int
   alloc::Int
   # end flint struct

   parent::NmodMPolyRing

   function nmod_mpoly_factor(ctx::NmodMPolyRing)
      z = new()
      ccall((:nmod_mpoly_factor_init, libflint), Nothing,
            (Ref{nmod_mpoly_factor}, Ref{NmodMPolyRing}),
            z, ctx)
      z.parent = ctx
      finalizer(_nmod_mpoly_factor_clear_fn, z)
      return z
  end
end

function _nmod_mpoly_factor_clear_fn(f::nmod_mpoly_factor)
   ccall((:nmod_mpoly_factor_clear, libflint), Nothing,
         (Ref{nmod_mpoly_factor}, Ref{NmodMPolyRing}),
         f, f.parent)
end

################################################################################
#
#   GFPMPolyRing / gfp_mpoly
#
###############################################################################

mutable struct GFPMPolyRing <: MPolyRing{gfp_elem}
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

   base_ring::GaloisField
   S::Vector{Symbol}

   function GFPMPolyRing(R::GaloisField, s::Vector{Symbol}, S::Symbol, cached::Bool = true)
      if cached && haskey(GFPMPolyID, (R, s, S))
         return GFPMPolyID[R, s, S]
      else
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
               (Ref{GFPMPolyRing}, Int, Cint, UInt),
               z, length(s), ord, UInt(modulus(R)))
         z.base_ring = R
         z.S = s
         finalizer(_gfp_mpoly_ctx_clear_fn, z)
         if cached
            GFPMPolyID[R, s, S] = z
         end
         return z
      end
   end
end

function _gfp_mpoly_ctx_clear_fn(a::GFPMPolyRing)
   ccall((:nmod_mpoly_ctx_clear, libflint), Nothing,
         (Ref{GFPMPolyRing},), a)
end

const GFPMPolyID = Dict{Tuple{GaloisField, Vector{Symbol}, Symbol}, GFPMPolyRing}()

mutable struct gfp_mpoly <: MPolyElem{gfp_elem}
   coeffs::Ptr{Nothing}
   exps::Ptr{Nothing}
   length::Int
   bits::Int
   coeffs_alloc::Int
   exps_alloc::Int
   # end of flint struct

   parent::GFPMPolyRing

   function gfp_mpoly(ctx::GFPMPolyRing)
      z = new()
      ccall((:nmod_mpoly_init, libflint), Nothing,
            (Ref{gfp_mpoly}, Ref{GFPMPolyRing}),
            z, ctx)
      z.parent = ctx
      finalizer(_gfp_mpoly_clear_fn, z)
      return z
   end

   function gfp_mpoly(ctx::GFPMPolyRing, a::Vector{gfp_elem}, b::Vector{Vector{UInt}})
      z = new()
      ccall((:nmod_mpoly_init2, libflint), Nothing,
            (Ref{gfp_mpoly}, Int, Ref{GFPMPolyRing}),
            z, length(a), ctx)
      z.parent = ctx
      finalizer(_gfp_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:nmod_mpoly_push_term_ui_ui, libflint), Nothing,
               (Ref{gfp_mpoly}, UInt, Ptr{UInt}, Ref{GFPMPolyRing}),
               z, a[i].data, b[i], ctx)
      end

      ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
            (Ref{gfp_mpoly}, Ref{GFPMPolyRing}),
            z, ctx)
      ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
            (Ref{gfp_mpoly}, Ref{GFPMPolyRing}),
            z, ctx)
      return z
   end

   function gfp_mpoly(ctx::GFPMPolyRing, a::Vector{gfp_elem}, b::Vector{Vector{Int}})
      z = new()
      ccall((:nmod_mpoly_init2, libflint), Nothing,
            (Ref{gfp_mpoly}, Int, Ref{GFPMPolyRing}),
            z, length(a), ctx)
      z.parent = ctx
      finalizer(_gfp_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:nmod_mpoly_push_term_ui_ui, libflint), Nothing,
               (Ref{gfp_mpoly}, UInt, Ptr{Int}, Ref{GFPMPolyRing}),
               z, a[i].data, b[i], ctx)
       end

       ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
             (Ref{gfp_mpoly}, Ref{GFPMPolyRing}),
             z, ctx)
       ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
             (Ref{gfp_mpoly}, Ref{GFPMPolyRing}),
             z, ctx)
       return z
   end

   function gfp_mpoly(ctx::GFPMPolyRing, a::Vector{gfp_elem}, b::Vector{Vector{fmpz}})
      z = new()
      ccall((:nmod_mpoly_init2, libflint), Nothing,
            (Ref{gfp_mpoly}, Int, Ref{GFPMPolyRing}),
            z, length(a), ctx)
      z.parent = ctx
      finalizer(_gfp_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:nmod_mpoly_push_term_ui_fmpz, libflint), Nothing,
               (Ref{gfp_mpoly}, UInt, Ptr{Ref{fmpz}}, Ref{GFPMPolyRing}),
               z, a[i].data, b[i], ctx)
      end

      ccall((:nmod_mpoly_sort_terms, libflint), Nothing,
             (Ref{gfp_mpoly}, Ref{GFPMPolyRing}),
             z, ctx)
      ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
             (Ref{gfp_mpoly}, Ref{GFPMPolyRing}),
             z, ctx)
      return z
   end

   function gfp_mpoly(ctx::GFPMPolyRing, a::UInt)
      z = new()
      ccall((:nmod_mpoly_init, libflint), Nothing,
            (Ref{gfp_mpoly}, Ref{GFPMPolyRing}),
            z, ctx)
      ccall((:nmod_mpoly_set_ui, libflint), Nothing,
            (Ref{gfp_mpoly}, UInt, Ref{GFPMPolyRing}),
            z, a, ctx)
      z.parent = ctx
      finalizer(_gfp_mpoly_clear_fn, z)
      return z
   end

   function gfp_mpoly(ctx::GFPMPolyRing, a::gfp_elem)
      z = new()
      ccall((:nmod_mpoly_init, libflint), Nothing,
            (Ref{gfp_mpoly}, Ref{GFPMPolyRing}),
            z, ctx)
      ccall((:nmod_mpoly_set_ui, libflint), Nothing,
            (Ref{gfp_mpoly}, UInt, Ref{GFPMPolyRing}),
            z, a.data, ctx)
      finalizer(_gfp_mpoly_clear_fn, z)
      z.parent = ctx
      return z
   end
end

function _gfp_mpoly_clear_fn(a::gfp_mpoly)
   ccall((:nmod_mpoly_clear, libflint), Nothing,
         (Ref{gfp_mpoly}, Ref{GFPMPolyRing}),
         a, a.parent)
end

mutable struct gfp_mpoly_factor
   constant::UInt
   poly::Ptr{Nothing}
   exp::Ptr{Nothing}
   num::Int
   alloc::Int
   # end flint struct

   parent::GFPMPolyRing

   function gfp_mpoly_factor(ctx::GFPMPolyRing)
      z = new()
      ccall((:nmod_mpoly_factor_init, libflint), Nothing,
            (Ref{gfp_mpoly_factor}, Ref{GFPMPolyRing}),
            z, ctx)
      z.parent = ctx
      finalizer(_gfp_mpoly_factor_clear_fn, z)
      return z
   end
end

function _gfp_mpoly_factor_clear_fn(f::gfp_mpoly_factor)
   ccall((:nmod_mpoly_factor_clear, libflint), Nothing,
         (Ref{gfp_mpoly_factor}, Ref{GFPMPolyRing}),
         f, f.parent)
end

###############################################################################
#
#   FqNmodFiniteField / fq_nmod
#
###############################################################################

mutable struct FqNmodFiniteField <: FinField
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
   @declare_other

   function FqNmodFiniteField(c::fmpz, deg::Int, s::Symbol, cached::Bool = true)
      if cached && haskey(FqNmodFiniteFieldID, (c, deg, s))
         return FqNmodFiniteFieldID[c, deg, s]
      else
         d = new()
         ccall((:fq_nmod_ctx_init, libflint), Nothing,
               (Ref{FqNmodFiniteField}, Ref{fmpz}, Int, Ptr{UInt8}),
			    d, c, deg, string(s))
         d.overfields = Dict{Int, Vector{FinFieldMorphism}}()
         d.subfields = Dict{Int, Vector{FinFieldMorphism}}()
         if cached
            FqNmodFiniteFieldID[c, deg, s] = d
         end
         finalizer(_FqNmodFiniteField_clear_fn, d)
         return d
      end
   end

   function FqNmodFiniteField(f::nmod_poly, s::Symbol, cached::Bool = true; check::Bool = true)
      check && !isprime(modulus(f)) &&
         throw(DomainError(base_ring(f), "the base ring of the polynomial must be a field"))
      if cached && haskey(FqNmodFiniteFieldIDNmodPol, (parent(f), f, s))
         return FqNmodFiniteFieldIDNmodPol[parent(f), f, s]
      else
         z = new()
         ccall((:fq_nmod_ctx_init_modulus, libflint), Nothing,
            (Ref{FqNmodFiniteField}, Ref{nmod_poly}, Ptr{UInt8}),
	      z, f, string(s))
         z.overfields = Dict{Int, Vector{FinFieldMorphism}}()
         z.subfields = Dict{Int, Vector{FinFieldMorphism}}()
         if cached
            FqNmodFiniteFieldIDNmodPol[parent(f), f, s] = z
         end
         finalizer(_FqNmodFiniteField_clear_fn, z)
         return z
      end
   end

   function FqNmodFiniteField(f::gfp_poly, s::Symbol, cached::Bool = true; check::Bool=true)
      # check ignored
      if cached && haskey(FqNmodFiniteFieldIDGFPPol, (parent(f), f, s))
         return FqNmodFiniteFieldIDGFPPol[parent(f), f, s]
      else
         z = new()
         ccall((:fq_nmod_ctx_init_modulus, libflint), Nothing,
            (Ref{FqNmodFiniteField}, Ref{gfp_poly}, Ptr{UInt8}),
	      z, f, string(s))
         z.overfields = Dict{Int, Vector{FinFieldMorphism}}()
         z.subfields = Dict{Int, Vector{FinFieldMorphism}}()
         if cached
            FqNmodFiniteFieldIDGFPPol[parent(f), f, s] = z
         end
         finalizer(_FqNmodFiniteField_clear_fn, z)
         return z
      end
   end

end

const FqNmodFiniteFieldID = Dict{Tuple{fmpz, Int, Symbol}, FqNmodFiniteField}()

const FqNmodFiniteFieldIDNmodPol = Dict{Tuple{NmodPolyRing, nmod_poly, Symbol},
                                    FqNmodFiniteField}()

const FqNmodFiniteFieldIDGFPPol = Dict{Tuple{GFPPolyRing, gfp_poly, Symbol},
                                    FqNmodFiniteField}()


function _FqNmodFiniteField_clear_fn(a :: FqNmodFiniteField)
   ccall((:fq_nmod_ctx_clear, libflint), Nothing, (Ref{FqNmodFiniteField},), a)
end

mutable struct fq_nmod <: FinFieldElem
   coeffs :: Ptr{Nothing}
   alloc :: Int
   length :: Int
   n :: Int
   ninv :: Int
   norm :: Int
   parent::FqNmodFiniteField

   function fq_nmod(ctx::FqNmodFiniteField)
      d = new()
      ccall((:fq_nmod_init2, libflint), Nothing,
            (Ref{fq_nmod}, Ref{FqNmodFiniteField}), d, ctx)
      finalizer(_fq_nmod_clear_fn, d)
      return d
   end

   function fq_nmod(ctx::FqNmodFiniteField, x::Int)
      d = new()
      ccall((:fq_nmod_init2, libflint), Nothing,
            (Ref{fq_nmod}, Ref{FqNmodFiniteField}), d, ctx)
      finalizer(_fq_nmod_clear_fn, d)
      ccall((:fq_nmod_set_si, libflint), Nothing,
                (Ref{fq_nmod}, Int, Ref{FqNmodFiniteField}), d, x, ctx)
      return d
   end

   function fq_nmod(ctx::FqNmodFiniteField, x::fmpz)
      d = new()
      ccall((:fq_nmod_init2, libflint), Nothing,
            (Ref{fq_nmod}, Ref{FqNmodFiniteField}), d, ctx)
      finalizer(_fq_nmod_clear_fn, d)
      ccall((:fq_nmod_set_fmpz, libflint), Nothing,
            (Ref{fq_nmod}, Ref{fmpz}, Ref{FqNmodFiniteField}), d, x, ctx)
      return d
   end

      function fq_nmod(ctx::FqNmodFiniteField, x::fq_nmod)
      d = new()
      ccall((:fq_nmod_init2, libflint), Nothing,
            (Ref{fq_nmod}, Ref{FqNmodFiniteField}), d, ctx)
      finalizer(_fq_nmod_clear_fn, d)
      ccall((:fq_nmod_set, libflint), Nothing,
            (Ref{fq_nmod}, Ref{fq_nmod}, Ref{FqNmodFiniteField}), d, x, ctx)
      return d
   end
end

function _fq_nmod_clear_fn(a::fq_nmod)
   ccall((:fq_nmod_clear, libflint), Nothing,
         (Ref{fq_nmod}, Ref{FqNmodFiniteField}), a, a.parent)
end

###############################################################################
#
#   FqDefaultFiniteField / fq_default
#
###############################################################################

mutable struct FqDefaultFiniteField <: FinField
   # fq_default_ctx_struct is 200 bytes on 64 bit machine
   opaque::NTuple{200, Int8} 
   # end of flint struct

   var::String
   
   overfields :: Dict{Int, Vector{FinFieldMorphism}}
   subfields :: Dict{Int, Vector{FinFieldMorphism}}
   @declare_other

   function FqDefaultFiniteField(char::fmpz, deg::Int, s::Symbol, cached::Bool = true)
      if cached && haskey(FqDefaultFiniteFieldID, (char, deg, s))
         return FqDefaultFiniteFieldID[char, deg, s]
      else
         d = new()
         d.var = string(s)
         finalizer(_FqDefaultFiniteField_clear_fn, d)
         ccall((:fq_default_ctx_init, libflint), Nothing,
               (Ref{FqDefaultFiniteField}, Ref{fmpz}, Int, Ptr{UInt8}),
                  d, char, deg, d.var)
         if cached
            FqDefaultFiniteFieldID[char, deg, s] = d
         end
         return d
      end
   end

   function FqDefaultFiniteField(f::fmpz_mod_poly, s::Symbol, cached::Bool = true; check::Bool = true)
      check && !isprobable_prime(modulus(f)) &&
         throw(DomainError(base_ring(f), "the base ring of the polynomial must be a field"))
      if cached && haskey(FqDefaultFiniteFieldIDFmpzPol, (f, s))
         return FqDefaultFiniteFieldIDFmpzPol[f, s]
      else
         z = new()
         z.var = string(s)
         ccall((:fq_default_ctx_init_modulus, libflint), Nothing,
               (Ref{FqDefaultFiniteField}, Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}, Ptr{UInt8}),
                  z, f, f.parent.base_ring.ninv, string(s))
         if cached
            FqDefaultFiniteFieldIDFmpzPol[f, s] = z
         end
         finalizer(_FqDefaultFiniteField_clear_fn, z)
         return z
      end
   end

   function FqDefaultFiniteField(f::gfp_fmpz_poly, s::Symbol, cached::Bool = true; check::Bool = true)
      # check ignored
      if cached && haskey(FqDefaultFiniteFieldIDGFPPol, (f, s))
         return FqDefaultFiniteFieldIDGFPPol[f, s]
      else
         z = new()
         z.var = string(s)
         ccall((:fq_default_ctx_init_modulus, libflint), Nothing,
               (Ref{FqDefaultFiniteField}, Ref{gfp_fmpz_poly}, Ref{fmpz_mod_ctx_struct}, Ptr{UInt8}),
                  z, f, f.parent.base_ring.ninv, string(s))
         if cached
            FqDefaultFiniteFieldIDGFPPol[f, s] = z
         end
         finalizer(_FqDefaultFiniteField_clear_fn, z)
         return z
      end
   end

   function FqDefaultFiniteField(f::nmod_poly, s::Symbol, cached::Bool = true; check::Bool = true)
      check && !isprime(modulus(f)) &&
         throw(DomainError(base_ring(f), "the base ring of the polynomial must be a field"))
      if cached && haskey(FqDefaultFiniteFieldIDNmodPol, (f, s))
         return FqDefaultFiniteFieldIDNmodPol[f, s]
      else
         z = new()
         z.var = string(s)
         ccall((:fq_default_ctx_init_modulus_nmod, libflint), Nothing,
               (Ref{FqDefaultFiniteField}, Ref{nmod_poly}, Ptr{UInt8}),
                  z, f, string(s))
         if cached
            FqDefaultFiniteFieldIDNmodPol[f, s] = z
         end
         finalizer(_FqDefaultFiniteField_clear_fn, z)
         return z
      end
   end

   function FqDefaultFiniteField(f::gfp_poly, s::Symbol, cached::Bool = true; check::Bool = true)
      # check ignored
      if cached && haskey(FqDefaultFiniteFieldIDGFPNmodPol, (f, s))
         return FqDefaultFiniteFieldIDGFPNmodPol[f, s]
      else
         z = new()
         z.var = string(s)
         ccall((:fq_default_ctx_init_modulus_nmod, libflint), Nothing,
               (Ref{FqDefaultFiniteField}, Ref{gfp_poly}, Ptr{UInt8}),
                  z, f, string(s))
         if cached
            FqDefaultFiniteFieldIDGFPNmodPol[f, s] = z
         end
         finalizer(_FqDefaultFiniteField_clear_fn, z)
         return z
      end
   end
end

const FqDefaultFiniteFieldID = Dict{Tuple{fmpz, Int, Symbol}, FqDefaultFiniteField}()

const FqDefaultFiniteFieldIDFmpzPol = Dict{Tuple{fmpz_mod_poly, Symbol}, FqDefaultFiniteField}()

const FqDefaultFiniteFieldIDGFPPol = Dict{Tuple{gfp_fmpz_poly, Symbol}, FqDefaultFiniteField}()

const FqDefaultFiniteFieldIDNmodPol = Dict{Tuple{nmod_poly, Symbol}, FqDefaultFiniteField}()

const FqDefaultFiniteFieldIDGFPNmodPol = Dict{Tuple{gfp_poly, Symbol}, FqDefaultFiniteField}()

function _FqDefaultFiniteField_clear_fn(a :: FqDefaultFiniteField)
   ccall((:fq_default_ctx_clear, libflint), Nothing, (Ref{FqDefaultFiniteField},), a)
end

mutable struct fq_default <: FinFieldElem
   opaque::NTuple{48, Int8} # fq_default_struct is 48 bytes on a 64 bit machine
   parent::FqDefaultFiniteField

   function fq_default(ctx::FqDefaultFiniteField)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      d.parent = ctx
      return d
   end

   function fq_default(ctx::FqDefaultFiniteField, x::Int)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_si, libflint), Nothing,
                (Ref{fq_default}, Int, Ref{FqDefaultFiniteField}), d, x, ctx)
      d.parent = ctx
      return d
   end

   function fq_default(ctx::FqDefaultFiniteField, x::fmpz)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_fmpz, libflint), Nothing,
            (Ref{fq_default}, Ref{fmpz}, Ref{FqDefaultFiniteField}), d, x, ctx)
      d.parent = ctx
      return d
   end

   function fq_default(ctx::FqDefaultFiniteField, x::fmpz_poly)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_fmpz_poly, libflint), Nothing,
            (Ref{fq_default}, Ref{fmpz_poly}, Ref{FqDefaultFiniteField}), d, x, ctx)
      d.parent = ctx
      return d
   end

   function fq_default(ctx::FqDefaultFiniteField, x::nmod_poly)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_nmod_poly, libflint), Nothing,
            (Ref{fq_default}, Ref{nmod_poly}, Ref{FqDefaultFiniteField}), d, x, ctx)
      d.parent = ctx
      return d
   end

   function fq_default(ctx::FqDefaultFiniteField, x::gfp_poly)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_nmod_poly, libflint), Nothing,
            (Ref{fq_default}, Ref{gfp_poly}, Ref{FqDefaultFiniteField}), d, x, ctx)
      d.parent = ctx
      return d
   end

   function fq_default(ctx::FqDefaultFiniteField, x::fmpz_mod_poly)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_fmpz_mod_poly, libflint), Nothing,
            (Ref{fq_default}, Ref{fmpz_mod_poly}, Ref{FqDefaultFiniteField}), d, x, ctx)
      d.parent = ctx
      return d
   end

   function fq_default(ctx::FqDefaultFiniteField, x::gfp_fmpz_poly)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set_fmpz_mod_poly, libflint), Nothing,
            (Ref{fq_default}, Ref{gfp_fmpz_poly}, Ref{FqDefaultFiniteField}), d, x, ctx)
      d.parent = ctx
      return d
   end

   function fq_default(ctx::FqDefaultFiniteField, x::fq_default)
      d = new()
      ccall((:fq_default_init2, libflint), Nothing,
            (Ref{fq_default}, Ref{FqDefaultFiniteField}), d, ctx)
      finalizer(_fq_default_clear_fn, d)
      ccall((:fq_default_set, libflint), Nothing,
            (Ref{fq_default}, Ref{fq_default}, Ref{FqDefaultFiniteField}), d, x, ctx)
      d.parent = ctx
      return d
   end
end

function _fq_default_clear_fn(a::fq_default)
   ccall((:fq_default_clear, libflint), Nothing,
         (Ref{fq_default}, Ref{FqDefaultFiniteField}), a, a.parent)
end

###############################################################################
#
#   FqFiniteField / fq
#
###############################################################################

mutable struct FqFiniteField <: FinField
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
   @declare_other

   function FqFiniteField(char::fmpz, deg::Int, s::Symbol, cached::Bool = true)
      if cached && haskey(FqFiniteFieldID, (char, deg, s))
         return FqFiniteFieldID[char, deg, s]
      else
         d = new()
         finalizer(_FqFiniteField_clear_fn, d)
         ccall((:fq_ctx_init, libflint), Nothing,
               (Ref{FqFiniteField}, Ref{fmpz}, Int, Ptr{UInt8}),
                  d, char, deg, string(s))
         if cached
            FqFiniteFieldID[char, deg, s] = d
         end
         return d
      end
   end

   function FqFiniteField(f::fmpz_mod_poly, s::Symbol, cached::Bool = true; check::Bool = true)
      check && !isprobable_prime(modulus(f)) &&
         throw(DomainError(base_ring(f), "the base ring of the polynomial must be a field"))
      if cached && haskey(FqFiniteFieldIDFmpzPol, (f, s))
         return FqFiniteFieldIDFmpzPol[f, s]
      else
         z = new()
         ccall((:fq_ctx_init_modulus, libflint), Nothing,
               (Ref{FqFiniteField}, Ref{fmpz_mod_poly}, Ref{fmpz_mod_ctx_struct}, Ptr{UInt8}),
                  z, f, f.parent.base_ring.ninv, string(s))
         if cached
            FqFiniteFieldIDFmpzPol[f, s] = z
         end
         finalizer(_FqFiniteField_clear_fn, z)
         return z
      end
   end

   function FqFiniteField(f::gfp_fmpz_poly, s::Symbol, cached::Bool = true; check::Bool = true)
      # check ignored
      if cached && haskey(FqFiniteFieldIDGFPPol, (f, s))
         return FqFiniteFieldIDGFPPol[f, s]
      else
         z = new()
         ccall((:fq_ctx_init_modulus, libflint), Nothing,
               (Ref{FqFiniteField}, Ref{gfp_fmpz_poly}, Ref{fmpz_mod_ctx_struct}, Ptr{UInt8}),
                  z, f, f.parent.base_ring.ninv, string(s))
         if cached
            FqFiniteFieldIDGFPPol[f, s] = z
         end
         finalizer(_FqFiniteField_clear_fn, z)
         return z
      end
   end

end

const FqFiniteFieldID = Dict{Tuple{fmpz, Int, Symbol}, FqFiniteField}()

const FqFiniteFieldIDFmpzPol = Dict{Tuple{fmpz_mod_poly, Symbol}, FqFiniteField}()

const FqFiniteFieldIDGFPPol = Dict{Tuple{gfp_fmpz_poly, Symbol}, FqFiniteField}()

function _FqFiniteField_clear_fn(a :: FqFiniteField)
   ccall((:fq_ctx_clear, libflint), Nothing, (Ref{FqFiniteField},), a)
end

mutable struct fq <: FinFieldElem
   coeffs :: Ptr{Nothing}
   alloc :: Int
   length :: Int
   parent::FqFiniteField

   function fq(ctx::FqFiniteField)
      d = new()
      ccall((:fq_init2, libflint), Nothing,
            (Ref{fq}, Ref{FqFiniteField}), d, ctx)
      finalizer(_fq_clear_fn, d)
      d.parent = ctx
      return d
   end

   function fq(ctx::FqFiniteField, x::Int)
      d = new()
      ccall((:fq_init2, libflint), Nothing,
            (Ref{fq}, Ref{FqFiniteField}), d, ctx)
      finalizer(_fq_clear_fn, d)
      ccall((:fq_set_si, libflint), Nothing,
                (Ref{fq}, Int, Ref{FqFiniteField}), d, x, ctx)
      d.parent = ctx
      return d
   end

   function fq(ctx::FqFiniteField, x::fmpz)
      d = new()
      ccall((:fq_init2, libflint), Nothing,
            (Ref{fq}, Ref{FqFiniteField}), d, ctx)
      finalizer(_fq_clear_fn, d)
      ccall((:fq_set_fmpz, libflint), Nothing,
            (Ref{fq}, Ref{fmpz}, Ref{FqFiniteField}), d, x, ctx)
      d.parent = ctx
      return d
   end

   function fq(ctx::FqFiniteField, x::fq)
      d = new()
      ccall((:fq_init2, libflint), Nothing,
            (Ref{fq}, Ref{FqFiniteField}), d, ctx)
      finalizer(_fq_clear_fn, d)
      ccall((:fq_set, libflint), Nothing,
            (Ref{fq}, Ref{fq}, Ref{FqFiniteField}), d, x, ctx)
      d.parent = ctx
      return d
   end
end

function _fq_clear_fn(a::fq)
   ccall((:fq_clear, libflint), Nothing,
         (Ref{fq}, Ref{FqFiniteField}), a, a.parent)
end


###############################################################################
#
#   FqNmodMPolyRing / fq_nmod_mpoly
#
###############################################################################

mutable struct FqNmodMPolyRing <: MPolyRing{fq_nmod}
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

   base_ring::FqNmodFiniteField
   S::Vector{Symbol}

   function FqNmodMPolyRing(R::FqNmodFiniteField, s::Vector{Symbol}, S::Symbol, cached::Bool = true)
      isempty(s) && error("variable list is empty")
      if cached && haskey(FqNmodMPolyID, (R, s, S))
         return FqNmodMPolyID[R, s, S]
      else
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
               (Ref{FqNmodMPolyRing}, Int, Cint, Ref{FqNmodFiniteField}),
               z, length(s), ord, R)
         z.base_ring = R
         z.S = s
         finalizer(_fq_nmod_mpoly_ctx_clear_fn, z)
         if cached
            FqNmodMPolyID[R, s, S] = z
         end
         return z
      end
   end
end

function _fq_nmod_mpoly_ctx_clear_fn(a::FqNmodMPolyRing)
   ccall((:fq_nmod_mpoly_ctx_clear, libflint), Nothing,
           (Ref{FqNmodMPolyRing},), a)
end

const FqNmodMPolyID = Dict{Tuple{FqNmodFiniteField, Vector{Symbol}, Symbol}, FqNmodMPolyRing}()

mutable struct fq_nmod_mpoly <: MPolyElem{fq_nmod}
   coeffs::Ptr{Nothing}
   exps::Ptr{Nothing}
   length::Int
   bits::Int
   coeffs_alloc::Int
   exps_alloc::Int
   # end of flint struct

   parent::FqNmodMPolyRing

   function fq_nmod_mpoly(ctx::FqNmodMPolyRing)
      z = new()
      ccall((:fq_nmod_mpoly_init, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing},), z, ctx)
      z.parent = ctx
      finalizer(_fq_nmod_mpoly_clear_fn, z)
      return z
   end

   function fq_nmod_mpoly(ctx::FqNmodMPolyRing, a::Vector{fq_nmod}, b::Vector{Vector{UInt}})
      z = new()
      ccall((:fq_nmod_mpoly_init2, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_fq_nmod_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:fq_nmod_mpoly_push_term_fq_nmod_ui, libflint), Nothing,
               (Ref{fq_nmod_mpoly}, Ref{fq_nmod}, Ptr{UInt}, Ref{FqNmodMPolyRing}),
               z, a[i], b[i], ctx)
       end

       ccall((:fq_nmod_mpoly_sort_terms, libflint), Nothing,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}), z, ctx)
       ccall((:fq_nmod_mpoly_combine_like_terms, libflint), Nothing,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}), z, ctx)
       return z
   end

   function fq_nmod_mpoly(ctx::FqNmodMPolyRing, a::Vector{fq_nmod}, b::Vector{Vector{Int}})
      z = new()
      ccall((:fq_nmod_mpoly_init2, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_fq_nmod_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:fq_nmod_mpoly_push_term_fq_nmod_ui, libflint), Nothing,
               (Ref{fq_nmod_mpoly}, Ref{fq_nmod}, Ptr{Int}, Ref{FqNmodMPolyRing}),
               z, a[i], b[i], ctx)
       end

       ccall((:fq_nmod_mpoly_sort_terms, libflint), Nothing,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}), z, ctx)
       ccall((:fq_nmod_mpoly_combine_like_terms, libflint), Nothing,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}), z, ctx)
       return z
   end

   function fq_nmod_mpoly(ctx::FqNmodMPolyRing, a::Vector{fq_nmod}, b::Vector{Vector{fmpz}})
      z = new()
      ccall((:fq_nmod_mpoly_init2, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Int, Ref{FqNmodMPolyRing},), z, length(a), ctx)
      z.parent = ctx
      finalizer(_fq_nmod_mpoly_clear_fn, z)

      for i in 1:length(a)
         ccall((:fq_nmod_mpoly_push_term_fq_nmod_fmpz, libflint), Nothing,
               (Ref{fq_nmod_mpoly}, Ref{fq_nmod}, Ptr{Ref{fmpz}}, Ref{FqNmodMPolyRing}),
               z, a[i], b[i], ctx)
       end

       ccall((:fq_nmod_mpoly_sort_terms, libflint), Nothing,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}), z, ctx)
       ccall((:nmod_mpoly_combine_like_terms, libflint), Nothing,
             (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}), z, ctx)
       return z
   end

   function fq_nmod_mpoly(ctx::FqNmodMPolyRing, a::UInt)
      z = new()
      ccall((:fq_nmod_mpoly_init, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing},), z, ctx)
      ccall((:fq_nmod_mpoly_set_ui, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, UInt, Ref{FqNmodMPolyRing}), z, a, ctx)
      z.parent = ctx
      finalizer(_fq_nmod_mpoly_clear_fn, z)
      return z
   end

   function fq_nmod_mpoly(ctx::NmodMPolyRing, a::nmod)
      return fq_nmod_mpoly(ctx, a.data)
   end

   function fq_nmod_mpoly(ctx::FqNmodMPolyRing, a::fq_nmod)
      z = new()
      ccall((:fq_nmod_mpoly_init, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing},), z, ctx)
      ccall((:fq_nmod_mpoly_set_fq_nmod, libflint), Nothing,
            (Ref{fq_nmod_mpoly}, Ref{fq_nmod}, Ref{FqNmodMPolyRing}), z, a, ctx)
      z.parent = ctx
      finalizer(_fq_nmod_mpoly_clear_fn, z)
      return z
   end
end

function _fq_nmod_mpoly_clear_fn(a::fq_nmod_mpoly)
   ccall((:fq_nmod_mpoly_clear, libflint), Nothing,
          (Ref{fq_nmod_mpoly}, Ref{FqNmodMPolyRing}), a, a.parent)
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

   parent::FqNmodMPolyRing

   function fq_nmod_mpoly_factor(ctx::FqNmodMPolyRing)
      z = new()
      ccall((:fq_nmod_mpoly_factor_init, libflint), Nothing,
            (Ref{fq_nmod_mpoly_factor}, Ref{FqNmodMPolyRing}),
            z, ctx)
      z.parent = ctx
      finalizer(_fq_nmod_mpoly_factor_clear_fn, z)
      return z
  end
end

function _fq_nmod_mpoly_factor_clear_fn(f::fq_nmod_mpoly_factor)
   ccall((:fq_nmod_mpoly_factor_clear, libflint), Nothing,
         (Ref{fq_nmod_mpoly_factor}, Ref{FqNmodMPolyRing}),
         f, f.parent)
end

###############################################################################
#
#   FlintLocalField / Local field type heirarchy
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

   function FlintPadicField(p::fmpz, prec::Int; cached::Bool = true, check::Bool = true)
      check && !isprobable_prime(p) && throw(DomainError(p, "Characteristic must be prime"))

      if cached
         a = (p, prec)
         if haskey(PadicBase, a)
            return PadicBase[a]
         end
      end

      d = new()

      ccall((:padic_ctx_init, libflint), Nothing,
            (Ref{FlintPadicField}, Ref{fmpz}, Int, Int, Cint),
            d, p, 0, 0, 0)
      finalizer(_padic_ctx_clear_fn, d)
      d.prec_max = prec

      if cached
         @assert !haskey(PadicBase, (p, prec))
         PadicBase[(p, prec)] = d
      end

      return d
   end
end

const PadicBase = Dict{Tuple{fmpz, Int}, FlintPadicField}()

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
   a::Int         # fmpz
   j::Ptr{Nothing}   # slong*
   len::Int
   var::Cstring   # char*
   prec_max::Int

   function FlintQadicField(p::fmpz, d::Int, prec::Int, var::String = "a"; cached::Bool = true, check::Bool = true)

      check && !isprobable_prime(p) && throw(DomainError(p, "Characteristic must be prime"))

      if cached
         a = (p, d, prec)
         if haskey(QadicBase, a)
            Q = QadicBase[a]
            return Q, gen(Q)
         end
      end

      z = new()
      ccall((:qadic_ctx_init, libflint), Nothing,
           (Ref{FlintQadicField}, Ref{fmpz}, Int, Int, Int, Cstring, Cint),
                                     z, p, d, 0, 0, var, 0)
      finalizer(_qadic_ctx_clear_fn, z)
      z.prec_max = prec

      if cached
         @assert !haskey(QadicBase, (p, d, prec))
         QadicBase[(p, d, prec)] = z
      end

      return z, gen(z)
   end
end

const QadicBase = Dict{Tuple{fmpz, Int, Int}, FlintQadicField}()

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
#   FmpzRelSeriesRing / fmpz_rel_series
#
###############################################################################

mutable struct FmpzRelSeriesRing <: SeriesRing{fmpz}
   base_ring::FlintIntegerRing
   prec_max::Int
   S::Symbol

   function FmpzRelSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
      if cached && haskey(FmpzRelSeriesID, (prec, s))
         FmpzRelSeriesID[prec, s]
      else
         z = new(FlintZZ, prec, s)
         if cached
            FmpzRelSeriesID[prec, s] = z
         end
         return z
      end
   end
end

const FmpzRelSeriesID = Dict{Tuple{Int, Symbol}, FmpzRelSeriesRing}()

mutable struct fmpz_rel_series <: RelSeriesElem{fmpz}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   # end flint struct

   prec::Int
   val::Int
   parent::FmpzRelSeriesRing

   function fmpz_rel_series()
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing,
            (Ref{fmpz_rel_series},), z)
      finalizer(_fmpz_rel_series_clear_fn, z)
      return z
   end

   function fmpz_rel_series(a::Vector{fmpz}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fmpz_poly_init2, libflint), Nothing,
            (Ref{fmpz_rel_series}, Int), z, len)
      for i = 1:len
         ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
                     (Ref{fmpz_rel_series}, Int, Ref{fmpz}), z, i - 1, a[i])
      end
      z.prec = prec
      z.val = val
      finalizer(_fmpz_rel_series_clear_fn, z)
      return z
   end

   function fmpz_rel_series(a::fmpz_rel_series)
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing, (Ref{fmpz_rel_series},), z)
      ccall((:fmpz_poly_set, libflint), Nothing,
            (Ref{fmpz_rel_series}, Ref{fmpz_rel_series}), z, a)
      finalizer(_fmpz_rel_series_clear_fn, z)
      return z
   end
end

function _fmpz_rel_series_clear_fn(a::fmpz_rel_series)
   ccall((:fmpz_poly_clear, libflint), Nothing, (Ref{fmpz_rel_series},), a)
end

###############################################################################
#
#   FmpzAbsSeriesRing / fmpz_abs_series
#
###############################################################################

mutable struct FmpzAbsSeriesRing <: SeriesRing{fmpz}
   base_ring::FlintIntegerRing
   prec_max::Int
   S::Symbol

   function FmpzAbsSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
      if cached && haskey(FmpzAbsSeriesID, (prec, s))
         FmpzAbsSeriesID[prec, s]
      else
         z = new(FlintZZ, prec, s)
         if cached
            FmpzAbsSeriesID[prec, s] = z
         end
         return z
      end
   end
end

const FmpzAbsSeriesID = Dict{Tuple{Int, Symbol}, FmpzAbsSeriesRing}()

mutable struct fmpz_abs_series <: AbsSeriesElem{fmpz}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   prec :: Int
   parent::FmpzAbsSeriesRing

   function fmpz_abs_series()
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing,
            (Ref{fmpz_abs_series},), z)
      finalizer(_fmpz_abs_series_clear_fn, z)
      return z
   end

   function fmpz_abs_series(a::Vector{fmpz}, len::Int, prec::Int)
      z = new()
      ccall((:fmpz_poly_init2, libflint), Nothing,
            (Ref{fmpz_abs_series}, Int), z, len)
      for i = 1:len
         ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
                     (Ref{fmpz_abs_series}, Int, Ref{fmpz}), z, i - 1, a[i])
      end
      z.prec = prec
      finalizer(_fmpz_abs_series_clear_fn, z)
      return z
   end

   function fmpz_abs_series(a::fmpz_abs_series)
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing, (Ref{fmpz_abs_series},), z)
      ccall((:fmpz_poly_set, libflint), Nothing,
            (Ref{fmpz_abs_series}, Ref{fmpz_abs_series}), z, a)
      finalizer(_fmpz_abs_series_clear_fn, z)
      return z
   end
end

function _fmpz_abs_series_clear_fn(a::fmpz_abs_series)
   ccall((:fmpz_poly_clear, libflint), Nothing, (Ref{fmpz_abs_series},), a)
end

###############################################################################
#
#   FlintPuiseuxSeriesRing / FlintPuiseuxSeriesRingElem
#
###############################################################################

mutable struct FlintPuiseuxSeriesRing{T <: RingElem} <: Ring where T
   laurent_ring::Ring

   function FlintPuiseuxSeriesRing{T}(R::Ring, cached::Bool = true) where T
      if cached && haskey(FlintPuiseuxSeriesID, R)
         return FlintPuiseuxSeriesID[R]::FlintPuiseuxSeriesRing{T}
      else
         z = new{T}(R)
         if cached
            FlintPuiseuxSeriesID[R] = z
         end
         return z
      end
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

mutable struct FlintPuiseuxSeriesField{T <: RingElem} <: Field
   laurent_ring::Ring

   function FlintPuiseuxSeriesField{T}(R::Field, cached::Bool = true) where T
      if cached && haskey(FlintPuiseuxSeriesID, R)
         return FlintPuiseuxSeriesID[R]::FlintPuiseuxSeriesField{T}
      else
         z = new{T}(R)
         if cached
            FlintPuiseuxSeriesID[R] = z
         end
         return z
      end
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
#   FmpzLaurentSeriesRing / fmpz_laurent_series
#
###############################################################################

mutable struct FmpzLaurentSeriesRing <: Ring
   base_ring::FlintIntegerRing
   prec_max::Int
   S::Symbol

   function FmpzLaurentSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
      if cached && haskey(FmpzLaurentSeriesID, (prec, s))
         FmpzLaurentSeriesID[prec, s]
      else
         z = new(FlintZZ, prec, s)
         if cached
            FmpzLaurentSeriesID[prec, s] = z
         end
         return z
      end
   end
end

const FmpzLaurentSeriesID = Dict{Tuple{Int, Symbol}, FmpzLaurentSeriesRing}()

mutable struct fmpz_laurent_series <: RingElem
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   prec::Int
   val::Int
   scale::Int
   parent::FmpzLaurentSeriesRing

   function fmpz_laurent_series()
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing,
            (Ref{fmpz_laurent_series},), z)
      finalizer(_fmpz_laurent_series_clear_fn, z)
      return z
   end

   function fmpz_laurent_series(a::Vector{fmpz}, len::Int, prec::Int, val::Int, scale::Int)
      z = new()
      ccall((:fmpz_poly_init2, libflint), Nothing,
            (Ref{fmpz_laurent_series}, Int), z, len)
      for i = 1:len
         ccall((:fmpz_poly_set_coeff_fmpz, libflint), Nothing,
                     (Ref{fmpz_laurent_series}, Int, Ref{fmpz}), z, i - 1, a[i])
      end
      z.prec = prec
      z.val = val
      z.scale = scale
      finalizer(_fmpz_laurent_series_clear_fn, z)
      return z
   end

   function fmpz_laurent_series(a::fmpz_laurent_series)
      z = new()
      ccall((:fmpz_poly_init, libflint), Nothing, (Ref{fmpz_laurent_series},), z)
      ccall((:fmpz_poly_set, libflint), Nothing,
            (Ref{fmpz_laurent_series}, Ref{fmpz_laurent_series}), z, a)
      finalizer(_fmpz_laurent_series_clear_fn, z)
      return z
   end
end

function _fmpz_laurent_series_clear_fn(a::fmpz_laurent_series)
   ccall((:fmpz_poly_clear, libflint), Nothing, (Ref{fmpz_laurent_series},), a)
end

###############################################################################
#
#   FmpqRelSeriesRing / fmpq_rel_series
#
###############################################################################

mutable struct FmpqRelSeriesRing <: SeriesRing{fmpq}
   base_ring::FlintRationalField
   prec_max::Int
   S::Symbol

   function FmpqRelSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
      if cached && haskey(FmpqRelSeriesID, (prec, s))
         return FmpqRelSeriesID[prec, s]
      else
         z = new(FlintQQ, prec, s)
         if cached
            FmpqRelSeriesID[prec, s] = z
         end
         return z
      end
   end
end

const FmpqRelSeriesID = Dict{Tuple{Int, Symbol}, FmpqRelSeriesRing}()

mutable struct fmpq_rel_series <: RelSeriesElem{fmpq}
   coeffs::Ptr{Nothing}
   den::Int
   alloc::Int
   length::Int
   prec::Int
   val::Int
   parent::FmpqRelSeriesRing

   function fmpq_rel_series()
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing,
            (Ref{fmpq_rel_series},), z)
      finalizer(_fmpq_rel_series_clear_fn, z)
      return z
   end

   function fmpq_rel_series(a::Vector{fmpq}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fmpq_poly_init2, libflint), Nothing,
            (Ref{fmpq_rel_series}, Int), z, len)
      for i = 1:len
         ccall((:fmpq_poly_set_coeff_fmpq, libflint), Nothing,
                     (Ref{fmpq_rel_series}, Int, Ref{fmpq}), z, i - 1, a[i])
      end
      z.prec = prec
      z.val = val
      finalizer(_fmpq_rel_series_clear_fn, z)
      return z
   end

   function fmpq_rel_series(a::fmpq_rel_series)
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{fmpq_rel_series},), z)
      ccall((:fmpq_poly_set, libflint), Nothing,
            (Ref{fmpq_rel_series}, Ref{fmpq_rel_series}), z, a)
      finalizer(_fmpq_rel_series_clear_fn, z)
      return z
   end
end

function _fmpq_rel_series_clear_fn(a::fmpq_rel_series)
   ccall((:fmpq_poly_clear, libflint), Nothing, (Ref{fmpq_rel_series},), a)
end

###############################################################################
#
#   FmpqAbsSeriesRing / fmpq_abs_series
#
###############################################################################

mutable struct FmpqAbsSeriesRing <: SeriesRing{fmpq}
   base_ring::FlintRationalField
   prec_max::Int
   S::Symbol

   function FmpqAbsSeriesRing(prec::Int, s::Symbol, cached::Bool = true)
      if cached && haskey(FmpqAbsSeriesID, (prec, s))
         return FmpqAbsSeriesID[prec, s]
      else
         z = new(FlintQQ, prec, s)
         if cached
            FmpqAbsSeriesID[prec, s] = z
         end
         return z
      end
   end
end

const FmpqAbsSeriesID = Dict{Tuple{Int, Symbol}, FmpqAbsSeriesRing}()

mutable struct fmpq_abs_series <: AbsSeriesElem{fmpq}
   coeffs::Ptr{Nothing}
   den::Int
   alloc::Int
   length::Int
   prec :: Int
   parent::FmpqAbsSeriesRing

   function fmpq_abs_series()
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing,
            (Ref{fmpq_abs_series},), z)
      finalizer(_fmpq_abs_series_clear_fn, z)
      return z
   end

   function fmpq_abs_series(a::Vector{fmpq}, len::Int, prec::Int)
      z = new()
      ccall((:fmpq_poly_init2, libflint), Nothing,
            (Ref{fmpq_abs_series}, Int), z, len)
      for i = 1:len
         ccall((:fmpq_poly_set_coeff_fmpq, libflint), Nothing,
                     (Ref{fmpq_abs_series}, Int, Ref{fmpq}), z, i - 1, a[i])
      end
      z.prec = prec
      finalizer(_fmpq_abs_series_clear_fn, z)
      return z
   end

   function fmpq_abs_series(a::fmpq_abs_series)
      z = new()
      ccall((:fmpq_poly_init, libflint), Nothing, (Ref{fmpq_abs_series},), z)
      ccall((:fmpq_poly_set, libflint), Nothing,
            (Ref{fmpq_abs_series}, Ref{fmpq_abs_series}), z, a)
      finalizer(_fmpq_abs_series_clear_fn, z)
      return z
   end
end

function _fmpq_abs_series_clear_fn(a::fmpq_abs_series)
   ccall((:fmpq_poly_clear, libflint), Nothing, (Ref{fmpq_abs_series},), a)
end

###############################################################################
#
#   GFPRelSeriesRing / gfp_rel_series
#
###############################################################################

mutable struct GFPRelSeriesRing <: SeriesRing{nmod}
   base_ring::GaloisField
   prec_max::Int
   S::Symbol

   function GFPRelSeriesRing(R::GaloisField, prec::Int, s::Symbol,
                                 cached::Bool = true)
      if cached && haskey(GFPRelSeriesID, (R, prec, s))
         return GFPRelSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            GFPRelSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

const GFPRelSeriesID = Dict{Tuple{GaloisField, Int, Symbol},
                                GFPRelSeriesRing}()

mutable struct gfp_rel_series <: RelSeriesElem{gfp_elem}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   mod_n::UInt
   mod_ninv::UInt
   mod_norm::UInt
   prec::Int
   val::Int
   parent::GFPRelSeriesRing

   function gfp_rel_series(p::UInt)
      z = new()
      ccall((:nmod_poly_init, libflint), Nothing,
            (Ref{gfp_rel_series}, UInt), z, p)
      finalizer(_gfp_rel_series_clear_fn, z)
      return z
   end

   function gfp_rel_series(p::UInt, a::Vector{fmpz}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_rel_series}, UInt, Int), z, p, len)
      for i = 1:len
         tt = ccall((:fmpz_fdiv_ui, libflint), UInt,
                    (Ref{fmpz}, UInt), a[i], p)
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                     (Ref{gfp_rel_series}, Int, UInt), z, i - 1, tt)
      end
      z.prec = prec
      z.val = val
      finalizer(_gfp_rel_series_clear_fn, z)
      return z
   end

   function gfp_rel_series(p::UInt, a::Vector{UInt}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_rel_series}, UInt, Int), z, p, len)
      for i = 1:len
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                     (Ref{gfp_rel_series}, Int, UInt), z, i - 1, a[i])
      end
      z.prec = prec
      z.val = val
      finalizer(_gfp_rel_series_clear_fn, z)
      return z
   end

   function gfp_rel_series(p::UInt, a::Vector{gfp_elem}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_rel_series}, UInt, Int), z, p, len)
      for i = 1:len
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
               (Ref{gfp_rel_series}, Int, UInt), z, i - 1, data(a[i]))
      end
      z.prec = prec
      z.val = val
      finalizer(_gfp_rel_series_clear_fn, z)
      return z
   end

   function gfp_rel_series(a::gfp_rel_series)
      z = new()
      p = modulus(base_ring(parent(a)))
      ccall((:nmod_poly_init, libflint), Nothing,
            (Ref{gfp_rel_series}, UInt), z, p)
      ccall((:nmod_poly_set, libflint), Nothing,
            (Ref{gfp_rel_series}, Ref{gfp_rel_series}), z, a)
      finalizer(_gfp_rel_series_clear_fn, z)
      return z
   end
end

function _gfp_rel_series_clear_fn(a::gfp_rel_series)
   ccall((:nmod_poly_clear, libflint), Nothing, (Ref{gfp_rel_series},), a)
end

###############################################################################
#
#   NmodRelSeriesRing / nmod_rel_series
#
###############################################################################

mutable struct NmodRelSeriesRing <: SeriesRing{nmod}
   base_ring::NmodRing
   prec_max::Int
   S::Symbol

   function NmodRelSeriesRing(R::NmodRing, prec::Int, s::Symbol,
                                 cached::Bool = true)
      if cached && haskey(NmodRelSeriesID, (R, prec, s))
         return NmodRelSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            NmodRelSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

const NmodRelSeriesID = Dict{Tuple{NmodRing, Int, Symbol},
                                NmodRelSeriesRing}()

mutable struct nmod_rel_series <: RelSeriesElem{nmod}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   mod_n::UInt
   mod_ninv::UInt
   mod_norm::UInt
   prec::Int
   val::Int
   parent::NmodRelSeriesRing

   function nmod_rel_series(p::UInt)
      z = new()
      ccall((:nmod_poly_init, libflint), Nothing,
            (Ref{nmod_rel_series}, UInt), z, p)
      finalizer(_nmod_rel_series_clear_fn, z)
      return z
   end

   function nmod_rel_series(p::UInt, a::Vector{fmpz}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_rel_series}, UInt, Int), z, p, len)
      for i = 1:len
         tt = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), a[i], p)
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                     (Ref{nmod_rel_series}, Int, UInt), z, i - 1, tt)
      end
      z.prec = prec
      z.val = val
      finalizer(_nmod_rel_series_clear_fn, z)
      return z
   end

   function nmod_rel_series(p::UInt, a::Vector{UInt}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_rel_series}, UInt, Int), z, p, len)
      for i = 1:len
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                     (Ref{nmod_rel_series}, Int, UInt), z, i - 1, a[i])
      end
      z.prec = prec
      z.val = val
      finalizer(_nmod_rel_series_clear_fn, z)
      return z
   end

   function nmod_rel_series(p::UInt, a::Vector{nmod}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_rel_series}, UInt, Int), z, p, len)
      for i = 1:len
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
                     (Ref{nmod_rel_series}, Int, UInt), z, i - 1, data(a[i]))
      end
      z.prec = prec
      z.val = val
      finalizer(_nmod_rel_series_clear_fn, z)
      return z
   end

   function nmod_rel_series(a::nmod_rel_series)
      z = new()
      p = modulus(base_ring(parent(a)))
      ccall((:nmod_poly_init, libflint), Nothing,
            (Ref{nmod_rel_series}, UInt), z, p)
      ccall((:nmod_poly_set, libflint), Nothing,
            (Ref{nmod_rel_series}, Ref{nmod_rel_series}), z, a)
      finalizer(_nmod_rel_series_clear_fn, z)
      return z
   end
end

function _nmod_rel_series_clear_fn(a::nmod_rel_series)
   ccall((:nmod_poly_clear, libflint), Nothing, (Ref{nmod_rel_series},), a)
end

###############################################################################
#
#   GFPFmpzRelSeriesRing / gfp_fmpz_rel_series
#
###############################################################################

mutable struct GFPFmpzRelSeriesRing <: SeriesRing{gfp_fmpz_elem}
   base_ring::GaloisFmpzField
   prec_max::Int
   S::Symbol

   function GFPFmpzRelSeriesRing(R::Ring, prec::Int, s::Symbol,
                                 cached::Bool = true)
      if cached && haskey(GFPFmpzRelSeriesID, (R, prec, s))
         return GFPFmpzRelSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            GFPFmpzRelSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

const GFPFmpzRelSeriesID = Dict{Tuple{GaloisFmpzField, Int, Symbol},
                                GFPFmpzRelSeriesRing}()

mutable struct gfp_fmpz_rel_series <: RelSeriesElem{gfp_fmpz_elem}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   # end flint struct

   prec::Int
   val::Int
   parent::GFPFmpzRelSeriesRing

   function gfp_fmpz_rel_series(p::fmpz_mod_ctx_struct)
      z = new()
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{gfp_fmpz_rel_series}, Ref{fmpz_mod_ctx_struct}),
            z, p)
      finalizer(_gfp_fmpz_rel_series_clear_fn, z)
      return z
   end

   function gfp_fmpz_rel_series(R::GaloisFmpzField)
      return gfp_fmpz_rel_series(R.ninv)
   end

   function gfp_fmpz_rel_series(p::fmpz_mod_ctx_struct, a::Vector{fmpz},
                                len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{gfp_fmpz_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, len, p)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{gfp_fmpz_rel_series}, Int, Ref{fmpz},
                Ref{fmpz_mod_ctx_struct}),
               z, i - 1, a[i], p)
      end
      z.prec = prec
      z.val = val
      finalizer(_gfp_fmpz_rel_series_clear_fn, z)
      return z
   end

   function gfp_fmpz_rel_series(R::GaloisFmpzField, a::Vector{fmpz},
                                len::Int, prec::Int, val::Int)
      return gfp_fmpz_rel_series(R.ninv, a, len, prec, val)
   end

   function gfp_fmpz_rel_series(p::fmpz_mod_ctx_struct, a::Vector{gfp_fmpz_elem},
                                len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{gfp_fmpz_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, len, p)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{gfp_fmpz_rel_series}, Int, Ref{fmpz},
                Ref{fmpz_mod_ctx_struct}),
               z, i - 1, data(a[i]), p)
      end
      z.prec = prec
      z.val = val
      finalizer(_gfp_fmpz_rel_series_clear_fn, z)
      return z
   end

   function gfp_fmpz_rel_series(R::GaloisFmpzField, a::Vector{gfp_fmpz_elem},
                                len::Int, prec::Int, val::Int)
      return gfp_fmpz_rel_series(R.ninv, a, len, prec, val)
   end

   function gfp_fmpz_rel_series(a::gfp_fmpz_rel_series)
      z = new()
      p = a.parent.base_ring.ninv
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{gfp_fmpz_rel_series}, Ref{fmpz_mod_ctx_struct}),
            z, p)
      ccall((:fmpz_mod_poly_set, libflint), Nothing,
            (Ref{gfp_fmpz_rel_series}, Ref{gfp_fmpz_rel_series},
             Ref{fmpz_mod_ctx_struct}),
            z, a, p)
      finalizer(_gfp_fmpz_rel_series_clear_fn, z)
      return z
   end
end

function _gfp_fmpz_rel_series_clear_fn(a::gfp_fmpz_rel_series)
   ccall((:fmpz_mod_poly_clear, libflint), Nothing,
         (Ref{gfp_fmpz_rel_series}, Ref{fmpz_mod_ctx_struct}),
         a, a.parent.base_ring.ninv)
end

###############################################################################
#
#   FmpzModRelSeriesRing / fmpz_mod_rel_series
#
###############################################################################

mutable struct FmpzModRelSeriesRing <: SeriesRing{fmpz_mod}
   base_ring::FmpzModRing
   prec_max::Int
   S::Symbol

   function FmpzModRelSeriesRing(R::Ring, prec::Int, s::Symbol,
                                 cached::Bool = true)
      if cached && haskey(FmpzModRelSeriesID, (R, prec, s))
         return FmpzModRelSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            FmpzModRelSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

const FmpzModRelSeriesID = Dict{Tuple{FmpzModRing, Int, Symbol},
                                FmpzModRelSeriesRing}()

mutable struct fmpz_mod_rel_series <: RelSeriesElem{fmpz_mod}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   # end flint struct

   prec::Int
   val::Int
   parent::FmpzModRelSeriesRing

   function fmpz_mod_rel_series(p::fmpz_mod_ctx_struct)
      z = new()
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_ctx_struct}),
            z, p)
      finalizer(_fmpz_mod_rel_series_clear_fn, z)
      return z
   end

   function fmpz_mod_rel_series(R::FmpzModRing)
      return fmpz_mod_rel_series(R.ninv)
   end

   function fmpz_mod_rel_series(p::fmpz_mod_ctx_struct, a::Vector{fmpz},
                                len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, len, p)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz_mod_rel_series}, Int, Ref{fmpz},
                Ref{fmpz_mod_ctx_struct}),
               z, i - 1, a[i], p)
      end
      z.prec = prec
      z.val = val
      finalizer(_fmpz_mod_rel_series_clear_fn, z)
      return z
   end

   function fmpz_mod_rel_series(R::FmpzModRing, a::Vector{fmpz},
                                len::Int, prec::Int, val::Int)
      return fmpz_mod_rel_series(R.ninv, a, len, prec, val)
   end

   function fmpz_mod_rel_series(p::fmpz_mod_ctx_struct, a::Vector{fmpz_mod},
                                len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, len, p)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz_mod_rel_series}, Int, Ref{fmpz},
                Ref{fmpz_mod_ctx_struct}),
               z, i - 1, data(a[i]), p)
      end
      z.prec = prec
      z.val = val
      finalizer(_fmpz_mod_rel_series_clear_fn, z)
      return z
   end

   function fmpz_mod_rel_series(R::FmpzModRing, a::Vector{fmpz_mod},
                                len::Int, prec::Int, val::Int)
      return fmpz_mod_rel_series(R.ninv, a, len, prec, val)
   end

   function fmpz_mod_rel_series(a::fmpz_mod_rel_series)
      z = new()
      p = a.parent.base_ring.ninv
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_ctx_struct}),
            z, p)
      ccall((:fmpz_mod_poly_set, libflint), Nothing,
            (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_rel_series},
             Ref{fmpz_mod_ctx_struct}),
            z, a, p)
      finalizer(_fmpz_mod_rel_series_clear_fn, z)
      return z
   end
end

function _fmpz_mod_rel_series_clear_fn(a::fmpz_mod_rel_series)
   ccall((:fmpz_mod_poly_clear, libflint), Nothing,
         (Ref{fmpz_mod_rel_series}, Ref{fmpz_mod_ctx_struct}),
         a, a.parent.base_ring.ninv)
end

###############################################################################
#
#   GFPFmpzAbsSeriesRing / gfp_fmpz_abs_series
#
###############################################################################

mutable struct GFPFmpzAbsSeriesRing <: SeriesRing{gfp_fmpz_elem}
   base_ring::GaloisFmpzField
   prec_max::Int
   S::Symbol

   function GFPFmpzAbsSeriesRing(R::Ring, prec::Int, s::Symbol,
                                 cached::Bool = true)
      if cached && haskey(GFPFmpzAbsSeriesID, (R, prec, s))
         return GFPFmpzAbsSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            GFPFmpzAbsSeriesID[R, prec, s]  = z
         end
         return z
      end
   end
end

const GFPFmpzAbsSeriesID = Dict{Tuple{GaloisFmpzField, Int, Symbol},
                                GFPFmpzAbsSeriesRing}()

mutable struct gfp_fmpz_abs_series <: AbsSeriesElem{gfp_fmpz_elem}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   # end flint struct

   prec::Int
   parent::GFPFmpzAbsSeriesRing

   function gfp_fmpz_abs_series(p::fmpz_mod_ctx_struct)
      z = new()
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{gfp_fmpz_abs_series}, Ref{fmpz_mod_ctx_struct}),
            z, p)
      finalizer(_gfp_fmpz_abs_series_clear_fn, z)
      return z
   end

   function gfp_fmpz_abs_series(R::GaloisFmpzField)
      return gfp_fmpz_abs_series(R.ninv)
   end

   function gfp_fmpz_abs_series(p::fmpz_mod_ctx_struct, a::Vector{fmpz},
                                len::Int, prec::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{gfp_fmpz_abs_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, len, p)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{gfp_fmpz_abs_series}, Int, Ref{fmpz},
                Ref{fmpz_mod_ctx_struct}),
               z, i - 1, a[i], p)
      end
      z.prec = prec
      finalizer(_gfp_fmpz_abs_series_clear_fn, z)
      return z
   end

   function gfp_fmpz_abs_series(R::GaloisFmpzField, a::Vector{fmpz}, len::Int, prec::Int)
      return gfp_fmpz_abs_series(R.ninv, a, len, prec)
   end

   function gfp_fmpz_abs_series(p::fmpz_mod_ctx_struct, a::Vector{gfp_fmpz_elem},
                                len::Int, prec::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{gfp_fmpz_abs_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, len, p)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{gfp_fmpz_abs_series}, Int, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
             z, i - 1, data(a[i]), p)
      end
      z.prec = prec
      finalizer(_gfp_fmpz_abs_series_clear_fn, z)
      return z
   end

   function gfp_fmpz_abs_series(R::GaloisFmpzField, a::Vector{gfp_fmpz_elem},
                                len::Int, prec::Int)
      return gfp_fmpz_abs_series(R.ninv, a, len, prec)
   end

   function gfp_fmpz_abs_series(a::gfp_fmpz_abs_series)
      z = new()
      p = a.parent.base_ring.ninv
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{gfp_fmpz_abs_series}, Ref{fmpz_mod_ctx_struct}),
            z, p)
      ccall((:fmpz_mod_poly_set, libflint), Nothing,
            (Ref{gfp_fmpz_abs_series}, Ref{gfp_fmpz_abs_series},
             Ref{fmpz_mod_ctx_struct}),
            z, a, p)
      finalizer(_gfp_fmpz_abs_series_clear_fn, z)
      return z
   end
end

function _gfp_fmpz_abs_series_clear_fn(a::gfp_fmpz_abs_series)
   ccall((:fmpz_mod_poly_clear, libflint), Nothing,
         (Ref{gfp_fmpz_abs_series}, Ref{fmpz_mod_ctx_struct}),
         a, a.parent.base_ring.ninv)
end

###############################################################################
#
#   NmodAbsSeriesRing / nmod_abs_series
#
###############################################################################

mutable struct NmodAbsSeriesRing <: SeriesRing{nmod}
   base_ring::NmodRing
   prec_max::Int
   n::UInt
   S::Symbol
 
   function NmodAbsSeriesRing(R::Ring, prec::Int, s::Symbol,
                                  cached::Bool = true)
      m = modulus(R)
      if cached && haskey(NmodAbsSeriesID, (R, prec, s))
         return NmodAbsSeriesID[R, prec, s]
      else
         z = new(R, prec, m, s)
         if cached
            NmodAbsSeriesID[R, prec, s]  = z
         end
         return z
      end
   end
end
 
const NmodAbsSeriesID = Dict{Tuple{NmodRing, Int, Symbol},
                                 NmodAbsSeriesRing}()
  
mutable struct nmod_abs_series <: AbsSeriesElem{nmod}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   mod_n::UInt
   mod_ninv::UInt
   mod_norm::UInt
   # end of flint struct

   prec::Int
   parent::NmodAbsSeriesRing
  
   function nmod_abs_series(n::UInt)
      z = new()
      ccall((:nmod_poly_init, libflint), Nothing,
            (Ref{nmod_abs_series}, UInt), z, n)
      finalizer(_nmod_abs_series_clear_fn, z)
      return z
   end
  
   function nmod_abs_series(n::UInt, arr::Vector{fmpz}, len::Int, prec::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_abs_series}, UInt, Int), z, n, length(arr))
      for i in 1:len
         tt = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), arr[i], n)
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
               (Ref{nmod_abs_series}, Int, UInt), z, i - 1, tt)
      end
      z.prec = prec
      finalizer(_nmod_abs_series_clear_fn, z)
      return z
   end
  
   function nmod_abs_series(n::UInt, arr::Vector{UInt}, len::Int, prec::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_abs_series}, UInt, Int), z, n, length(arr))
      for i in 1:len
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
               (Ref{nmod_abs_series}, Int, UInt), z, i - 1, arr[i])
      end
      z.prec = prec
      finalizer(_nmod_abs_series_clear_fn, z)
      return z
   end
  
   function nmod_abs_series(n::UInt, arr::Vector{nmod}, len::Int, prec::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_abs_series}, UInt, Int), z, n, length(arr))
      for i in 1:len
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
               (Ref{nmod_abs_series}, Int, UInt), z, i-1, arr[i].data)
      end
      z.prec = prec
      finalizer(_nmod_abs_series_clear_fn, z)
      return z
   end

   function nmod_abs_series(a::nmod_abs_series)
      z = new()
      R = base_ring(a)
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{nmod_abs_series}, UInt, Int), z, R.n, length(a))
      ccall((:nmod_poly_set, libflint), Nothing,
            (Ref{nmod_abs_series}, Ref{nmod_abs_series}), z, a)
      z.prec = a.prec
      finalizer(_nmod_abs_series_clear_fn, z)
      return z
   end      
end

function _nmod_abs_series_clear_fn(x::nmod_abs_series)
   ccall((:nmod_poly_clear, libflint), Nothing, (Ref{nmod_abs_series}, ), x)
end

###############################################################################
#
#   GFPAbsSeriesRing / gfp_abs_series
#
###############################################################################

mutable struct GFPAbsSeriesRing <: SeriesRing{gfp_elem}
   base_ring::GaloisField
   prec_max::Int
   n::UInt
   S::Symbol
 
   function GFPAbsSeriesRing(R::Ring, prec::Int, s::Symbol,
                                  cached::Bool = true)
      m = modulus(R)
      if cached && haskey(GFPAbsSeriesID, (R, prec, s))
         return GFPAbsSeriesID[R, prec, s]
      else
         z = new(R, prec, m, s)
         if cached
            GFPAbsSeriesID[R, prec, s]  = z
         end
         return z
      end
   end
end
 
const GFPAbsSeriesID = Dict{Tuple{GaloisField, Int, Symbol},
                                 GFPAbsSeriesRing}()
  
mutable struct gfp_abs_series <: AbsSeriesElem{gfp_elem}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   mod_n::UInt
   mod_ninv::UInt
   mod_norm::UInt
   # end of flint struct

   prec::Int
   parent::GFPAbsSeriesRing
  
   function gfp_abs_series(n::UInt)
      z = new()
      ccall((:nmod_poly_init, libflint), Nothing,
            (Ref{gfp_abs_series}, UInt), z, n)
      finalizer(_gfp_abs_series_clear_fn, z)
      return z
   end
  
   function gfp_abs_series(n::UInt, arr::Vector{fmpz}, len::Int, prec::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_abs_series}, UInt, Int), z, n, length(arr))
      for i in 1:len
         tt = ccall((:fmpz_fdiv_ui, libflint), UInt, (Ref{fmpz}, UInt), arr[i], n)
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
               (Ref{gfp_abs_series}, Int, UInt), z, i - 1, tt)
      end
      z.prec = prec
      finalizer(_gfp_abs_series_clear_fn, z)
      return z
   end
  
   function gfp_abs_series(n::UInt, arr::Vector{UInt}, len::Int, prec::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_abs_series}, UInt, Int), z, n, length(arr))
      for i in 1:len
         ccall((:nmod_poly_series_set_coeff_ui, libflint), Nothing,
               (Ref{gfp_abs_series}, Int, UInt), z, i - 1, arr[i])
      end
      z.prec = prec
      finalizer(_gfp_abs_series_clear_fn, z)
      return z
   end
  
   function gfp_abs_series(n::UInt, arr::Vector{gfp_elem}, len::Int, prec::Int)
      z = new()
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_abs_series}, UInt, Int), z, n, length(arr))
      for i in 1:len
         ccall((:nmod_poly_set_coeff_ui, libflint), Nothing,
               (Ref{gfp_abs_series}, Int, UInt), z, i-1, arr[i].data)
      end
      z.prec = prec
      finalizer(_gfp_abs_series_clear_fn, z)
      return z
   end

   function gfp_abs_series(a::gfp_abs_series)
      z = new()
      R = base_ring(a)
      ccall((:nmod_poly_init2, libflint), Nothing,
            (Ref{gfp_abs_series}, UInt, Int), z, R.n, length(a))
      ccall((:nmod_poly_set, libflint), Nothing,
            (Ref{gfp_abs_series}, Ref{gfp_abs_series}), z, a)
      z.prec = a.prec
      finalizer(_gfp_abs_series_clear_fn, z)
      return z
   end
end

function _gfp_abs_series_clear_fn(x::gfp_abs_series)
   ccall((:nmod_poly_clear, libflint), Nothing, (Ref{gfp_abs_series}, ), x)
end

###############################################################################
#
#   FmpzModAbsSeriesRing / fmpz_mod_abs_series
#
###############################################################################

mutable struct FmpzModAbsSeriesRing <: SeriesRing{fmpz_mod}
   base_ring::FmpzModRing
   prec_max::Int
   S::Symbol

   function FmpzModAbsSeriesRing(R::Ring, prec::Int, s::Symbol,
                                 cached::Bool = true)
      if cached && haskey(FmpzModAbsSeriesID, (R, prec, s))
         return FmpzModAbsSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            FmpzModAbsSeriesID[R, prec, s]  = z
         end
         return z
      end
   end
end

const FmpzModAbsSeriesID = Dict{Tuple{FmpzModRing, Int, Symbol},
                                FmpzModAbsSeriesRing}()

mutable struct fmpz_mod_abs_series <: AbsSeriesElem{fmpz_mod}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   # end flint struct

   prec::Int
   parent::FmpzModAbsSeriesRing

   function fmpz_mod_abs_series(p::fmpz_mod_ctx_struct)
      z = new()
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{fmpz_mod_abs_series}, Ref{fmpz_mod_ctx_struct}),
            z, p)
      finalizer(_fmpz_mod_abs_series_clear_fn, z)
      return z
   end

   function fmpz_mod_abs_series(R::FmpzModRing)
      return fmpz_mod_abs_series(R.ninv)
   end

   function fmpz_mod_abs_series(p::fmpz_mod_ctx_struct, a::Vector{fmpz},
                                len::Int, prec::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{fmpz_mod_abs_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, len, p)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
               (Ref{fmpz_mod_abs_series}, Int, Ref{fmpz},
                Ref{fmpz_mod_ctx_struct}),
               z, i - 1, a[i], p)
      end
      z.prec = prec
      finalizer(_fmpz_mod_abs_series_clear_fn, z)
      return z
   end

   function fmpz_mod_abs_series(R::FmpzModRing, a::Vector{fmpz}, len::Int, prec::Int)
      return fmpz_mod_abs_series(R.ninv, a, len, prec)
   end

   function fmpz_mod_abs_series(p::fmpz_mod_ctx_struct, a::Vector{fmpz_mod},
                                len::Int, prec::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, libflint), Nothing,
            (Ref{fmpz_mod_abs_series}, Int, Ref{fmpz_mod_ctx_struct}),
            z, len, p)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, libflint), Nothing,
            (Ref{fmpz_mod_abs_series}, Int, Ref{fmpz}, Ref{fmpz_mod_ctx_struct}),
		     z, i - 1, data(a[i]), p)
      end
      z.prec = prec
      finalizer(_fmpz_mod_abs_series_clear_fn, z)
      return z
   end

   function fmpz_mod_abs_series(R::FmpzModRing, a::Vector{fmpz_mod},
                                len::Int, prec::Int)
      return fmpz_mod_abs_series(R.ninv, a, len, prec)
   end

   function fmpz_mod_abs_series(a::fmpz_mod_abs_series)
      z = new()
      p = a.parent.base_ring.ninv
      ccall((:fmpz_mod_poly_init, libflint), Nothing,
            (Ref{fmpz_mod_abs_series}, Ref{fmpz_mod_ctx_struct}),
            z, p)
      ccall((:fmpz_mod_poly_set, libflint), Nothing,
            (Ref{fmpz_mod_abs_series}, Ref{fmpz_mod_abs_series},
             Ref{fmpz_mod_ctx_struct}),
            z, a, p)
      z.prec = a.prec
      finalizer(_fmpz_mod_abs_series_clear_fn, z)
      return z
   end
end

function _fmpz_mod_abs_series_clear_fn(a::fmpz_mod_abs_series)
   ccall((:fmpz_mod_poly_clear, libflint), Nothing,
         (Ref{fmpz_mod_abs_series}, Ref{fmpz_mod_ctx_struct}),
         a, a.parent.base_ring.ninv)
end


###############################################################################
#
#   FqDefaultRelSeriesRing / fq_default_rel_series
#
###############################################################################

mutable struct FqDefaultRelSeriesRing <: SeriesRing{fq_default}
   base_ring::FqDefaultFiniteField
   prec_max::Int
   S::Symbol
 
   function FqDefaultRelSeriesRing(R::FqDefaultFiniteField, prec::Int, s::Symbol,
                             cached::Bool = true)
      if cached && haskey(FqDefaultRelSeriesID, (R, prec, s))
         return FqDefaultRelSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            FqDefaultRelSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end
 
const FqDefaultRelSeriesID = Dict{Tuple{FqDefaultFiniteField, Int, Symbol}, FqDefaultRelSeriesRing}()
 
mutable struct fq_default_rel_series <: RelSeriesElem{fq_default}
   # fq_default_poly_struct is 24 bytes on 64 bit machine
   opaque::NTuple{24, Int8} 
   # end of flint struct

   prec::Int
   val::Int
   parent::FqDefaultRelSeriesRing
 
   function fq_default_rel_series(ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_init, libflint), Nothing,
            (Ref{fq_default_rel_series}, Ref{FqDefaultFiniteField}), z, ctx)
      finalizer(_fq_default_rel_series_clear_fn, z)
      return z
   end
 
   function fq_default_rel_series(ctx::FqDefaultFiniteField, a::Vector{fq_default}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fq_default_poly_init2, libflint), Nothing,
            (Ref{fq_default_rel_series}, Int, Ref{FqDefaultFiniteField}), z, len, ctx)
      for i = 1:len
         ccall((:fq_default_poly_set_coeff, libflint), Nothing,
               (Ref{fq_default_rel_series}, Int, Ref{fq_default},
                Ref{FqDefaultFiniteField}), z, i - 1, a[i], ctx)
      end
      z.prec = prec
      z.val = val
      finalizer(_fq_default_rel_series_clear_fn, z)
      return z
   end
 
   function fq_default_rel_series(ctx::FqDefaultFiniteField, a::fq_default_rel_series)
      z = new()
      ccall((:fq_default_poly_init, libflint), Nothing,
            (Ref{fq_default_rel_series}, Ref{FqDefaultFiniteField}), z, ctx)
      ccall((:fq_default_poly_set, libflint), Nothing,
            (Ref{fq_default_rel_series}, Ref{fq_default_rel_series},
             Ref{FqDefaultFiniteField}), z, a, ctx)
      finalizer(_fq_default_rel_series_clear_fn, z)
      return z
   end
end
 
function _fq_default_rel_series_clear_fn(a::fq_default_rel_series)
   ctx = base_ring(a)
   ccall((:fq_default_poly_clear, libflint), Nothing,
         (Ref{fq_default_rel_series}, Ref{FqDefaultFiniteField}), a, ctx)
end

###############################################################################
#
#   FqRelSeriesRing / fq_rel_series
#
###############################################################################

mutable struct FqRelSeriesRing <: SeriesRing{fq}
   base_ring::FqFiniteField
   prec_max::Int
   S::Symbol

   function FqRelSeriesRing(R::FqFiniteField, prec::Int, s::Symbol,
                            cached::Bool = true)
      if cached && haskey(FqRelSeriesID, (R, prec, s))
         return FqRelSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            FqRelSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

const FqRelSeriesID = Dict{Tuple{FqFiniteField, Int, Symbol}, FqRelSeriesRing}()

mutable struct fq_rel_series <: RelSeriesElem{fq}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   prec::Int
   val::Int
   parent::FqRelSeriesRing

   function fq_rel_series(ctx::FqFiniteField)
      z = new()
      ccall((:fq_poly_init, libflint), Nothing,
            (Ref{fq_rel_series}, Ref{FqFiniteField}), z, ctx)
      finalizer(_fq_rel_series_clear_fn, z)
      return z
   end

   function fq_rel_series(ctx::FqFiniteField, a::Vector{fq}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fq_poly_init2, libflint), Nothing,
            (Ref{fq_rel_series}, Int, Ref{FqFiniteField}), z, len, ctx)
      for i = 1:len
         ccall((:fq_poly_set_coeff, libflint), Nothing,
               (Ref{fq_rel_series}, Int, Ref{fq}, Ref{FqFiniteField}),
                                               z, i - 1, a[i], ctx)
      end
      z.prec = prec
      z.val = val
      finalizer(_fq_rel_series_clear_fn, z)
      return z
   end

   function fq_rel_series(ctx::FqFiniteField, a::fq_rel_series)
      z = new()
      ccall((:fq_poly_init, libflint), Nothing,
            (Ref{fq_rel_series}, Ref{FqFiniteField}), z, ctx)
      ccall((:fq_poly_set, libflint), Nothing,
            (Ref{fq_rel_series}, Ref{fq_rel_series}, Ref{FqFiniteField}), z, a, ctx)
      finalizer(_fq_rel_series_clear_fn, z)
      return z
   end
end

function _fq_rel_series_clear_fn(a::fq_rel_series)
   ctx = base_ring(a)
   ccall((:fq_poly_clear, libflint), Nothing,
         (Ref{fq_rel_series}, Ref{FqFiniteField}), a, ctx)
end

###############################################################################
#
#   FqDefaultAbsSeriesRing / fq_default_abs_series
#
###############################################################################

mutable struct FqDefaultAbsSeriesRing <: SeriesRing{fq_default}
   base_ring::FqDefaultFiniteField
   prec_max::Int
   S::Symbol
 
   function FqDefaultAbsSeriesRing(R::FqDefaultFiniteField, prec::Int, s::Symbol,
                             cached::Bool = true)
      if cached && haskey(FqDefaultAbsSeriesID, (R, prec, s))
         return FqDefaultAbsSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            FqDefaultAbsSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end
 
const FqDefaultAbsSeriesID = Dict{Tuple{FqDefaultFiniteField, Int, Symbol}, FqDefaultAbsSeriesRing}()
 
mutable struct fq_default_abs_series <: AbsSeriesElem{fq_default}
   # fq_default_poly_struct is 24 bytes on 64 bit machine
   opaque::NTuple{24, Int8} 
   # end of flint struct

   prec::Int
   parent::FqDefaultAbsSeriesRing
 
   function fq_default_abs_series(ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_init, libflint), Nothing,
            (Ref{fq_default_abs_series}, Ref{FqDefaultFiniteField}), z, ctx)
      finalizer(_fq_default_abs_series_clear_fn, z)
      return z
   end
 
   function fq_default_abs_series(ctx::FqDefaultFiniteField, a::Vector{fq_default}, len::Int, prec::Int)
      z = new()
      ccall((:fq_default_poly_init2, libflint), Nothing,
            (Ref{fq_default_abs_series}, Int, Ref{FqDefaultFiniteField}), z, len, ctx)
      for i = 1:len
         ccall((:fq_default_poly_set_coeff, libflint), Nothing,
               (Ref{fq_default_abs_series}, Int, Ref{fq_default}, Ref{FqDefaultFiniteField}),
                                                z, i - 1, a[i], ctx)
      end
      z.prec = prec
      finalizer(_fq_default_abs_series_clear_fn, z)
      return z
   end
 
   function fq_default_abs_series(ctx::FqDefaultFiniteField, a::fq_default_abs_series)
      z = new()
      ccall((:fq_default_poly_init, libflint), Nothing,
            (Ref{fq_default_abs_series}, Ref{FqDefaultFiniteField}), z, ctx)
      ccall((:fq_default_poly_set, libflint), Nothing,
            (Ref{fq_default_abs_series}, Ref{fq_default_abs_series}, Ref{FqDefaultFiniteField}), z, a, ctx)
      finalizer(_fq_default_abs_series_clear_fn, z)
      return z
   end
end
 
function _fq_default_abs_series_clear_fn(a::fq_default_abs_series)
   ctx = base_ring(a)
   ccall((:fq_default_poly_clear, libflint), Nothing,
         (Ref{fq_default_abs_series}, Ref{FqDefaultFiniteField}), a, ctx)
end

###############################################################################
#
#   FqAbsSeriesRing / fq_abs_series
#
###############################################################################

mutable struct FqAbsSeriesRing <: SeriesRing{fq}
   base_ring::FqFiniteField
   prec_max::Int
   S::Symbol

   function FqAbsSeriesRing(R::FqFiniteField, prec::Int, s::Symbol,
                            cached::Bool = true)
      if cached && haskey(FqAbsSeriesID, (R, prec, s))
         return FqAbsSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            FqAbsSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

const FqAbsSeriesID = Dict{Tuple{FqFiniteField, Int, Symbol}, FqAbsSeriesRing}()

mutable struct fq_abs_series <: AbsSeriesElem{fq}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   prec::Int
   parent::FqAbsSeriesRing

   function fq_abs_series(ctx::FqFiniteField)
      z = new()
      ccall((:fq_poly_init, libflint), Nothing,
            (Ref{fq_abs_series}, Ref{FqFiniteField}), z, ctx)
      finalizer(_fq_abs_series_clear_fn, z)
      return z
   end

   function fq_abs_series(ctx::FqFiniteField, a::Vector{fq}, len::Int, prec::Int)
      z = new()
      ccall((:fq_poly_init2, libflint), Nothing,
            (Ref{fq_abs_series}, Int, Ref{FqFiniteField}), z, len, ctx)
      for i = 1:len
         ccall((:fq_poly_set_coeff, libflint), Nothing,
               (Ref{fq_abs_series}, Int, Ref{fq}, Ref{FqFiniteField}),
                                               z, i - 1, a[i], ctx)
      end
      z.prec = prec
      finalizer(_fq_abs_series_clear_fn, z)
      return z
   end

   function fq_abs_series(ctx::FqFiniteField, a::fq_abs_series)
      z = new()
      ccall((:fq_poly_init, libflint), Nothing,
            (Ref{fq_abs_series}, Ref{FqFiniteField}), z, ctx)
      ccall((:fq_poly_set, libflint), Nothing,
            (Ref{fq_abs_series}, Ref{fq_abs_series}, Ref{FqFiniteField}), z, a, ctx)
      finalizer(_fq_abs_series_clear_fn, z)
      return z
   end
end

function _fq_abs_series_clear_fn(a::fq_abs_series)
   ctx = base_ring(a)
   ccall((:fq_poly_clear, libflint), Nothing,
         (Ref{fq_abs_series}, Ref{FqFiniteField}), a, ctx)
end

###############################################################################
#
#   FqNmodRelSeriesRing / fq_nmod_rel_series
#
###############################################################################

mutable struct FqNmodRelSeriesRing <: SeriesRing{fq_nmod}
   base_ring::FqNmodFiniteField
   prec_max::Int
   S::Symbol

   function FqNmodRelSeriesRing(R::FqNmodFiniteField, prec::Int, s::Symbol,
                                cached::Bool = true)
      if cached && haskey(FqNmodRelSeriesID, (R, prec, s))
         return FqNmodRelSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            FqNmodRelSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

const FqNmodRelSeriesID = Dict{Tuple{FqNmodFiniteField, Int, Symbol},
                               FqNmodRelSeriesRing}()

mutable struct fq_nmod_rel_series <: RelSeriesElem{fq_nmod}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   prec::Int
   val::Int
   parent::FqNmodRelSeriesRing

   function fq_nmod_rel_series(ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_poly_init, libflint), Nothing,
            (Ref{fq_nmod_rel_series}, Ref{FqNmodFiniteField}), z, ctx)
      finalizer(_fq_nmod_rel_series_clear_fn, z)
      return z
   end

   function fq_nmod_rel_series(ctx::FqNmodFiniteField, a::Vector{fq_nmod}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fq_nmod_poly_init2, libflint), Nothing,
            (Ref{fq_nmod_rel_series}, Int, Ref{FqNmodFiniteField}), z, len, ctx)
      for i = 1:len
         ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
               (Ref{fq_nmod_rel_series}, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                                               z, i - 1, a[i], ctx)
      end
      z.prec = prec
      z.val = val
      finalizer(_fq_nmod_rel_series_clear_fn, z)
      return z
   end

   function fq_nmod_rel_series(ctx::FqNmodFiniteField, a::fq_nmod_rel_series)
      z = new()
      ccall((:fq_nmod_poly_init, libflint), Nothing,
            (Ref{fq_nmod_rel_series}, Ref{FqNmodFiniteField}), z, ctx)
      ccall((:fq_nmod_poly_set, libflint), Nothing,
            (Ref{fq_nmod_rel_series}, Ref{fq_nmod_rel_series}, Ref{FqNmodFiniteField}), z, a, ctx)
      finalizer(_fq_nmod_rel_series_clear_fn, z)
      return z
   end
end

function _fq_nmod_rel_series_clear_fn(a::fq_nmod_rel_series)
   ctx = base_ring(a)
   ccall((:fq_nmod_poly_clear, libflint), Nothing,
         (Ref{fq_nmod_rel_series}, Ref{FqNmodFiniteField}), a, ctx)
end

###############################################################################
#
#   FqNmodAbsSeriesRing / fq_nmod_abs_series
#
###############################################################################

mutable struct FqNmodAbsSeriesRing <: SeriesRing{fq_nmod}
   base_ring::FqNmodFiniteField
   prec_max::Int
   S::Symbol

   function FqNmodAbsSeriesRing(R::FqNmodFiniteField, prec::Int, s::Symbol,
                                cached::Bool = true)
      if cached && haskey(FqNmodAbsSeriesID, (R, prec, s))
         return FqNmodAbsSeriesID[R, prec, s]
      else
         z = new(R, prec, s)
         if cached
            FqNmodAbsSeriesID[R, prec, s] = z
         end
         return z
      end
   end
end

const FqNmodAbsSeriesID = Dict{Tuple{FqNmodFiniteField, Int, Symbol},
                               FqNmodAbsSeriesRing}()

mutable struct fq_nmod_abs_series <: AbsSeriesElem{fq_nmod}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   prec::Int
   parent::FqNmodAbsSeriesRing

   function fq_nmod_abs_series(ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_poly_init, libflint), Nothing,
            (Ref{fq_nmod_abs_series}, Ref{FqNmodFiniteField}), z, ctx)
      finalizer(_fq_nmod_abs_series_clear_fn, z)
      return z
   end

   function fq_nmod_abs_series(ctx::FqNmodFiniteField, a::Vector{fq_nmod}, len::Int, prec::Int)
      z = new()
      ccall((:fq_nmod_poly_init2, libflint), Nothing,
            (Ref{fq_nmod_abs_series}, Int, Ref{FqNmodFiniteField}), z, len, ctx)
      for i = 1:len
         ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
               (Ref{fq_nmod_abs_series}, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                                               z, i - 1, a[i], ctx)
      end
      z.prec = prec
      finalizer(_fq_nmod_abs_series_clear_fn, z)
      return z
   end

   function fq_nmod_abs_series(ctx::FqNmodFiniteField, a::fq_nmod_abs_series)
      z = new()
      ccall((:fq_nmod_poly_init, libflint), Nothing,
            (Ref{fq_nmod_abs_series}, Ref{FqNmodFiniteField}), z, ctx)
      ccall((:fq_nmod_poly_set, libflint), Nothing,
            (Ref{fq_nmod_abs_series}, Ref{fq_nmod_abs_series}, Ref{FqNmodFiniteField}), z, a, ctx)
      finalizer(_fq_nmod_abs_series_clear_fn, z)
      return z
   end
end

function _fq_nmod_abs_series_clear_fn(a::fq_nmod_abs_series)
   ctx = base_ring(a)
   ccall((:fq_nmod_poly_clear, libflint), Nothing,
         (Ref{fq_nmod_abs_series}, Ref{FqNmodFiniteField}), a, ctx)
end

###############################################################################
#
#   FmpqMatSpace / fmpq_mat
#
###############################################################################

# not really a mathematical ring
mutable struct FmpqMatSpace <: MatSpace{fmpq}
   nrows::Int
   ncols::Int
   base_ring::FlintRationalField

   function FmpqMatSpace(r::Int, c::Int, cached::Bool = true)
      if cached && haskey(FmpqMatID, (r, c))
         return FmpqMatID[r, c]
      else
         z = new(r, c, FlintQQ)
         if cached
            FmpqMatID[r, c] = z
         end
         return z
      end
   end
end

const FmpqMatID = Dict{Tuple{Int, Int}, FmpqMatSpace}()

mutable struct fmpq_mat <: MatElem{fmpq}
   entries::Ptr{Nothing}
   r::Int
   c::Int
   rows::Ptr{Nothing}
   base_ring::FlintRationalField
   view_parent

   # used by windows, not finalised!!
   function fmpq_mat()
      return new()
   end

   function fmpq_mat(r::Int, c::Int)
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{fmpq_mat}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      return z
   end

   function fmpq_mat(r::Int, c::Int, arr::AbstractMatrix{fmpq})
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{fmpq_mat}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{fmpq},
                       (Ref{fmpq_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set, libflint), Nothing,
                  (Ptr{fmpq}, Ref{fmpq}), el, arr[i, j])
         end
      end
      return z
   end

   function fmpq_mat(r::Int, c::Int, arr::AbstractMatrix{fmpz})
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{fmpq_mat}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      b = fmpz(1)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{fmpq},
                       (Ref{fmpq_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set_fmpz_frac, libflint), Nothing,
                  (Ptr{fmpq}, Ref{fmpz}, Ref{fmpz}), el, arr[i, j], b)
         end
      end
      return z
   end


   function fmpq_mat(r::Int, c::Int, arr::AbstractVector{fmpq})
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{fmpq_mat}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{fmpq},
                       (Ref{fmpq_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set, libflint), Nothing,
                  (Ptr{fmpq}, Ref{fmpq}), el, arr[(i-1)*c+j])
         end
      end
      return z
   end

   function fmpq_mat(r::Int, c::Int, arr::AbstractVector{fmpz})
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{fmpq_mat}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      b = fmpz(1)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{fmpq},
                       (Ref{fmpq_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set_fmpz_frac, libflint), Nothing,
                  (Ptr{fmpq}, Ref{fmpz}, Ref{fmpz}), el, arr[(i-1)*c+j], b)
         end
      end
      return z
   end


   function fmpq_mat(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Integer}
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{fmpq_mat}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{fmpq},
                       (Ref{fmpq_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set, libflint), Nothing,
                  (Ptr{fmpq}, Ref{fmpq}), el, fmpq(arr[i, j]))
         end
      end
      return z
   end

   function fmpq_mat(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Integer}
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{fmpq_mat}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, libflint), Ptr{fmpq},
                       (Ref{fmpq_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpq_set, libflint), Nothing,
                  (Ptr{fmpq}, Ref{fmpq}), el, fmpq(arr[(i-1)*c+j]))
         end
      end
      return z
   end

   function fmpq_mat(r::Int, c::Int, d::fmpq)
      z = new()
      ccall((:fmpq_mat_init, libflint), Nothing,
            (Ref{fmpq_mat}, Int, Int), z, r, c)
      finalizer(_fmpq_mat_clear_fn, z)
      GC.@preserve z for i = 1:min(r, c)
         el = ccall((:fmpq_mat_entry, libflint), Ptr{fmpq},
                    (Ref{fmpq_mat}, Int, Int), z, i - 1, i - 1)
         ccall((:fmpq_set, libflint), Nothing,
               (Ptr{fmpq}, Ref{fmpq}), el, d)
      end
      return z
   end

   function fmpq_mat(m::fmpq_mat)
      z = new()
      ccall((:fmpq_mat_init_set, libflint), Nothing,
            (Ref{fmpq_mat}, Ref{fmpq_mat}), z, m)
      finalizer(_fmpq_mat_clear_fn, z)
      return z
   end
end

function _fmpq_mat_clear_fn(a::fmpq_mat)
   ccall((:fmpq_mat_clear, libflint), Nothing, (Ref{fmpq_mat},), a)
end

###############################################################################
#
#   FmpzMatSpace / fmpz_mat
#
###############################################################################

# not really a mathematical ring
mutable struct FmpzMatSpace <: MatSpace{fmpz}
   nrows::Int
   ncols::Int
   base_ring::FlintIntegerRing

   function FmpzMatSpace(r::Int, c::Int, cached::Bool = true)
      if cached && haskey(FmpzMatID, (r, c))
         return FmpzMatID[r, c]
      else
         z = new(r, c, FlintZZ)
         if cached
            FmpzMatID[r, c] = z
         end
         return z
      end
   end
end

const FmpzMatID = Dict{Tuple{Int, Int}, FmpzMatSpace}()

mutable struct fmpz_mat <: MatElem{fmpz}
   entries::Ptr{Nothing}
   r::Int
   c::Int
   rows::Ptr{Nothing}
   base_ring::FlintIntegerRing
   view_parent

   # Used by view, not finalised!!
   function fmpz_mat()
      return new()
   end

   function fmpz_mat(r::Int, c::Int)
      z = new()
      ccall((:fmpz_mat_init, libflint), Nothing,
            (Ref{fmpz_mat}, Int, Int), z, r, c)
      finalizer(_fmpz_mat_clear_fn, z)
      return z
   end

   function fmpz_mat(r::Int, c::Int, arr::AbstractMatrix{fmpz})
      z = new()
      ccall((:fmpz_mat_init, libflint), Nothing,
            (Ref{fmpz_mat}, Int, Int), z, r, c)
      finalizer(_fmpz_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, libflint), Ptr{fmpz},
                       (Ref{fmpz_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpz_set, libflint), Nothing,
                  (Ptr{fmpz}, Ref{fmpz}), el, arr[i, j])
         end
      end
      return z
   end

   function fmpz_mat(r::Int, c::Int, arr::AbstractVector{fmpz})
      z = new()
      ccall((:fmpz_mat_init, libflint), Nothing,
            (Ref{fmpz_mat}, Int, Int), z, r, c)
      finalizer(_fmpz_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, libflint), Ptr{fmpz},
                       (Ref{fmpz_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpz_set, libflint), Nothing,
                  (Ptr{fmpz}, Ref{fmpz}), el, arr[(i-1)*c+j])
         end
      end
      return z
   end

   function fmpz_mat(r::Int, c::Int, arr::AbstractMatrix{T}) where {T <: Integer}
      z = new()
      ccall((:fmpz_mat_init, libflint), Nothing,
            (Ref{fmpz_mat}, Int, Int), z, r, c)
      finalizer(_fmpz_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, libflint), Ptr{fmpz},
                       (Ref{fmpz_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpz_set, libflint), Nothing,
                  (Ptr{fmpz}, Ref{fmpz}), el, fmpz(arr[i, j]))
         end
      end
      return z
   end

   function fmpz_mat(r::Int, c::Int, arr::AbstractVector{T}) where {T <: Integer}
      z = new()
      ccall((:fmpz_mat_init, libflint), Nothing,
            (Ref{fmpz_mat}, Int, Int), z, r, c)
      finalizer(_fmpz_mat_clear_fn, z)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, libflint), Ptr{fmpz},
                       (Ref{fmpz_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fmpz_set, libflint), Nothing,
                  (Ptr{fmpz}, Ref{fmpz}), el, fmpz(arr[(i-1)*c+j]))
         end
      end
      return z
   end

   function fmpz_mat(r::Int, c::Int, d::fmpz)
      z = new()
      ccall((:fmpz_mat_init, libflint), Nothing,
            (Ref{fmpz_mat}, Int, Int), z, r, c)
      finalizer(_fmpz_mat_clear_fn, z)
      GC.@preserve z for i = 1:min(r, c)
         el = ccall((:fmpz_mat_entry, libflint), Ptr{fmpz},
                    (Ref{fmpz_mat}, Int, Int), z, i - 1, i- 1)
         ccall((:fmpz_set, libflint), Nothing,
               (Ptr{fmpz}, Ref{fmpz}), el, d)
      end
      return z
   end

   function fmpz_mat(m::fmpz_mat)
      z = new()
      ccall((:fmpz_mat_init_set, libflint), Nothing,
            (Ref{fmpz_mat}, Ref{fmpz_mat}), z, m)
      finalizer(_fmpz_mat_clear_fn, z)
      return z
   end
end

function _fmpz_mat_clear_fn(a::fmpz_mat)
   ccall((:fmpz_mat_clear, libflint), Nothing, (Ref{fmpz_mat},), a)
end

###############################################################################
#
#   NmodMatSpace / nmod_mat
#
###############################################################################

mutable struct NmodMatSpace <: MatSpace{nmod}
  base_ring::NmodRing
  n::UInt
  nrows::Int
  ncols::Int

  function NmodMatSpace(R::NmodRing, r::Int, c::Int,
                        cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    if cached && haskey(NmodMatID, (R, r, c))
      return NmodMatID[R, r, c]
    else
      z = new(R, R.n, r, c)
      if cached
        NmodMatID[R, r, c] = z
      end
      return z
    end
  end
end

const NmodMatID = Dict{Tuple{NmodRing, Int, Int}, NmodMatSpace}()

mutable struct nmod_mat <: MatElem{nmod}
  entries::Ptr{Nothing}
  r::Int                  # Int
  c::Int                  # Int
  rows::Ptr{Nothing}
  n::UInt                # mp_limb_t / Culong
  ninv::UInt             # mp_limb_t / Culong
  norm::UInt             # mp_limb_t / Culong
  base_ring::NmodRing
  view_parent

  # Used by view, not finalised!!
  function nmod_mat()
    z = new()
    return z
  end

  function nmod_mat(r::Int, c::Int, n::UInt)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{nmod_mat}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
    return z
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::AbstractMatrix{UInt}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{nmod_mat}, Int, Int, UInt), z, r, c, n)
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

  function nmod_mat(r::Int, c::Int, n::UInt, arr::AbstractVector{UInt})
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{nmod_mat}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, mod(arr[(i - 1) * c + j], n), i, j)
      end
    end
    return z
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::AbstractMatrix{fmpz}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{nmod_mat}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
    if transpose
       arr = Base.transpose(arr)
    end
    t = fmpz()
    for i = 1:r
      for j = 1:c
        ccall((:fmpz_mod_ui, libflint), Nothing,
	      (Ref{fmpz}, Ref{fmpz}, UInt), t, arr[i, j], n)
	      setindex_raw!(z, t, i, j)
      end
    end
    return z
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::AbstractVector{fmpz})
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{nmod_mat}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
    t = fmpz()
    for i = 1:r
      for j = 1:c
        ccall((:fmpz_mod_ui, libflint), Nothing,
              (Ref{fmpz}, Ref{fmpz}, UInt), t, arr[(i - 1) * c + j], n)
        setindex!(z, t, i, j)
      end
    end
    return z
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::AbstractMatrix{T}, transpose::Bool = false) where {T <: Integer}
    arr_fmpz = map(fmpz, arr)
    return nmod_mat(r, c, n, arr_fmpz, transpose)
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::AbstractVector{T}) where {T <: Integer}
    arr_fmpz = map(fmpz, arr)
    return nmod_mat(r, c, n, arr_fmpz)
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::AbstractMatrix{nmod}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{nmod_mat}, Int, Int, UInt), z, r, c, n)
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

  function nmod_mat(r::Int, c::Int, n::UInt, arr::AbstractVector{nmod})
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{nmod_mat}, Int, Int, UInt), z, r, c, n)
    finalizer(_nmod_mat_clear_fn, z)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, arr[(i - 1) * c + j].data, i, j) # no reduction necessary
      end
    end
    return z
  end

  function nmod_mat(n::UInt, b::fmpz_mat)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{nmod_mat}, Int, Int, UInt), z, b.r, b.c, n)
    finalizer(_nmod_mat_clear_fn, z)
    ccall((:fmpz_mat_get_nmod_mat, libflint), Nothing,
            (Ref{nmod_mat}, Ref{fmpz_mat}), z, b)
    return z
  end

  function nmod_mat(n::Int, b::fmpz_mat)
    (n < 0) && error("Modulus must be positive")
    return nmod_mat(UInt(n), b)
  end

  function nmod_mat(n::fmpz, b::fmpz_mat)
    (n < 0) && error("Modulus must be positive")
    (n > typemax(UInt)) &&
          error("Modulus must be smaller than ", fmpz(typemax(UInt)))
    return nmod_mat(UInt(n), b)
  end
end

function _nmod_mat_clear_fn(mat::nmod_mat)
  ccall((:nmod_mat_clear, libflint), Nothing, (Ref{nmod_mat}, ), mat)
end

###############################################################################          
#
#   FmpzModMatSpace / fmpz_mod_mat
#
###############################################################################

mutable struct FmpzModMatSpace <: MatSpace{fmpz_mod}
  base_ring::FmpzModRing
  n::fmpz
  nrows::Int
  ncols::Int

  function FmpzModMatSpace(R::FmpzModRing, r::Int, c::Int,
                        cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    if cached && haskey(FmpzModMatID, (R, r, c))
      return FmpzModMatID[R, r, c]
    else
      z = new(R, R.n, r, c)
      if cached
        FmpzModMatID[R, r, c] = z
      end
      return z
    end
  end
end

const FmpzModMatID = Dict{Tuple{FmpzModRing, Int, Int}, FmpzModMatSpace}()

mutable struct fmpz_mod_mat <: MatElem{fmpz_mod}
   entries::Ptr{Nothing}
   r::Int
   c::Int
   rows::Ptr{Nothing}
   mod::Int              # fmpz
   base_ring::FmpzModRing
   view_parent

  # Used by view, not finalised!!
  function fmpz_mod_mat()
    z = new()
    return z
  end

  function fmpz_mod_mat(r::Int, c::Int, n::fmpz)
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
            (Ref{fmpz_mod_mat}, Int, Int, Ref{fmpz}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    return z
  end

  function fmpz_mod_mat(r::Int, c::Int, n::fmpz, arr::AbstractMatrix{fmpz}, transpose::Bool = false)
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
            (Ref{fmpz_mod_mat}, Int, Int, Ref{fmpz}), z, r, c, n)
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

  function fmpz_mod_mat(r::Int, c::Int, n::fmpz, arr::AbstractMatrix{T}, transpose::Bool = false) where T <: Integer
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
	  (Ref{fmpz_mod_mat}, Int, Int, Ref{fmpz}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    if transpose
       arr = Base.transpose(arr)
    end
    for i = 1:r
      for j = 1:c
         setindex_raw!(z, mod(fmpz(arr[i, j]), n), i, j)
      end
    end
    return z
  end

  function fmpz_mod_mat(r::Int, c::Int, n::fmpz, arr::AbstractMatrix{fmpz_mod}, transpose::Bool = false)
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
	  (Ref{fmpz_mod_mat}, Int, Int, Ref{fmpz}), z, r, c, n)
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

  function fmpz_mod_mat(r::Int, c::Int, n::fmpz, arr::AbstractVector{fmpz})
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
            (Ref{fmpz_mod_mat}, Int, Int, Ref{fmpz}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, mod(arr[(i - 1)*c + j], n), i, j)
      end
    end
    return z
  end

  function fmpz_mod_mat(r::Int, c::Int, n::fmpz, arr::AbstractVector{T}) where T <: Integer
    z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
          (Ref{fmpz_mod_mat}, Int, Int, Ref{fmpz}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    for i = 1:r
       for j = 1:c
          setindex_raw!(z, mod(fmpz(arr[(i - 1)*c + j]), n), i, j)
       end
    end
    return z
  end

  function fmpz_mod_mat(r::Int, c::Int, n::fmpz, arr::AbstractVector{fmpz_mod})                z = new()
    ccall((:fmpz_mod_mat_init, libflint), Nothing,
	  (Ref{fmpz_mod_mat}, Int, Int, Ref{fmpz}), z, r, c, n)
    finalizer(_fmpz_mod_mat_clear_fn, z)
    for i = 1:r
       for j = 1:c
          setindex_raw!(z, arr[(i - 1)*c + j].data, i, j)
       end
    end
    return z
  end
end

function _fmpz_mod_mat_clear_fn(mat::fmpz_mod_mat)
  ccall((:fmpz_mod_mat_clear, libflint), Nothing, (Ref{fmpz_mod_mat}, ), mat)
end

const Zmod_fmpz_mat = fmpz_mod_mat # eventually a union with future gfp_fmpz_mat

################################################################################
#
#   GFPMatSpace / gfp_mat
#
###############################################################################

mutable struct GFPMatSpace <: MatSpace{gfp_elem}
  base_ring::GaloisField
  n::UInt
  nrows::Int
  ncols::Int

  function GFPMatSpace(R::GaloisField, r::Int, c::Int,
                        cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    if cached && haskey(GFPMatID, (R, r, c))
      return GFPMatID[R, r, c]
    else
      z = new(R, R.n, r, c)
      if cached
        GFPMatID[R, r, c] = z
      end
      return z
    end
  end
end

const GFPMatID = Dict{Tuple{GaloisField, Int, Int}, GFPMatSpace}()

mutable struct gfp_mat <: MatElem{gfp_elem}
  entries::Ptr{Nothing}
  r::Int                  # Int
  c::Int                  # Int
  rows::Ptr{Nothing}
  n::UInt                # mp_limb_t / Culong
  ninv::UInt             # mp_limb_t / Culong
  norm::UInt             # mp_limb_t / Culong
  base_ring::GaloisField
  view_parent

  # Used by view, not finalised!!
  function gfp_mat()
    z = new()
    return z
  end

  function gfp_mat(r::Int, c::Int, n::UInt)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{gfp_mat}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
    return z
  end

  function gfp_mat(r::Int, c::Int, n::UInt, arr::AbstractMatrix{UInt}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{gfp_mat}, Int, Int, UInt), z, r, c, n)
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

  function gfp_mat(r::Int, c::Int, n::UInt, arr::AbstractVector{UInt})
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{gfp_mat}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, mod(arr[(i - 1) * c + j], n), i, j)
      end
    end
    return z
  end

  function gfp_mat(r::Int, c::Int, n::UInt, arr::AbstractMatrix{fmpz}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{gfp_mat}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
    if transpose
       arr = Base.transpose(arr)
    end
    t = fmpz()
    for i = 1:r
      for j = 1:c
        ccall((:fmpz_mod_ui, libflint), Nothing,
              (Ref{fmpz}, Ref{fmpz}, UInt), t, arr[i, j], n)
        setindex_raw!(z, t, i, j)
      end
    end
    return z
  end

  function gfp_mat(r::Int, c::Int, n::UInt, arr::AbstractVector{fmpz})
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{gfp_mat}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
    t = fmpz()
    for i = 1:r
      for j = 1:c
        ccall((:fmpz_mod_ui, libflint), Nothing,
              (Ref{fmpz}, Ref{fmpz}, UInt), t, arr[(i - 1) * c + j], n)
        setindex!(z, t, i, j)
      end
    end
    return z
  end

  function gfp_mat(r::Int, c::Int, n::UInt, arr::AbstractMatrix{T}, transpose::Bool = false) where {T <: Integer}
    arr_fmpz = map(fmpz, arr)
    return gfp_mat(r, c, n, arr_fmpz, transpose)
  end

  function gfp_mat(r::Int, c::Int, n::UInt, arr::AbstractVector{T}) where {T <: Integer}
    arr_fmpz = map(fmpz, arr)
    return gfp_mat(r, c, n, arr_fmpz)
  end

  function gfp_mat(r::Int, c::Int, n::UInt, arr::AbstractMatrix{gfp_elem}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{gfp_mat}, Int, Int, UInt), z, r, c, n)
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

  function gfp_mat(r::Int, c::Int, n::UInt, arr::AbstractVector{gfp_elem})
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{gfp_mat}, Int, Int, UInt), z, r, c, n)
    finalizer(_gfp_mat_clear_fn, z)
    for i = 1:r
      for j = 1:c
        setindex_raw!(z, arr[(i - 1) * c + j].data, i, j) # no reduction necessary
      end
    end
    return z
  end

  function gfp_mat(n::UInt, b::fmpz_mat)
    z = new()
    ccall((:nmod_mat_init, libflint), Nothing,
            (Ref{gfp_mat}, Int, Int, UInt), z, b.r, b.c, n)
    finalizer(_gfp_mat_clear_fn, z)
    ccall((:fmpz_mat_get_nmod_mat, libflint), Nothing,
            (Ref{gfp_mat}, Ref{fmpz_mat}), z, b)
    return z
  end

  function gfp_mat(n::Int, b::fmpz_mat)
    (n < 0) && error("Modulus must be positive")
    return gfp_mat(UInt(n), b)
  end

  function gfp_mat(n::fmpz, b::fmpz_mat)
    (n < 0) && error("Modulus must be positive")
    (n > typemax(UInt)) &&
          error("Modulus must be smaller than ", fmpz(typemax(UInt)))
    return gfp_mat(UInt(n), b)
  end
end

function _gfp_mat_clear_fn(mat::gfp_mat)
  ccall((:nmod_mat_clear, libflint), Nothing, (Ref{gfp_mat}, ), mat)
end

###############################################################################
#
#   FqDefaultPolyRing / fq_default_poly
#
###############################################################################

mutable struct FqDefaultPolyRing <: PolyRing{fq_default}
   base_ring::FqDefaultFiniteField
   S::Symbol
 
   function FqDefaultPolyRing(R::FqDefaultFiniteField, s::Symbol, cached::Bool = true)
      if cached && haskey(FqDefaultPolyID, (R, s))
         return FqDefaultPolyID[(R, s)]
      else
         z = new(R,s)
         if cached
            FqDefaultPolyID[(R, s)] = z
         end
         return z
      end
   end
end
 
const FqDefaultPolyID = Dict{Tuple{FqDefaultFiniteField, Symbol}, FqDefaultPolyRing}()
 
mutable struct fq_default_poly <: PolyElem{fq_default}
   # fq_default_poly_struct is 24 bytes on 64 bit machine
   opaque::NTuple{24, Int8} 
   # end of flint struct

   parent::FqDefaultPolyRing
 
   function fq_default_poly(ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_init, libflint), Nothing,
            (Ref{fq_default_poly}, Ref{FqDefaultFiniteField}), z, ctx)
      finalizer(_fq_default_poly_clear_fn, z)
      return z
   end
 
   function fq_default_poly(a::fq_default_poly, ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_init, libflint), Nothing,
            (Ref{fq_default_poly}, Ref{FqDefaultFiniteField}), z, ctx)
      ccall((:fq_default_poly_set, libflint), Nothing,
            (Ref{fq_default_poly}, Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
            z, a, ctx)
       finalizer(_fq_default_poly_clear_fn, z)
       return z
   end
 
   function fq_default_poly(a::fq_default, ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_init, libflint), Nothing,
            (Ref{fq_default_poly}, Ref{FqDefaultFiniteField}), z, ctx)
      ccall((:fq_default_poly_set_fq_default, libflint), Nothing,
            (Ref{fq_default_poly}, Ref{fq_default}, Ref{FqDefaultFiniteField}),
             z, a, ctx)
      finalizer(_fq_default_poly_clear_fn, z)
      return z
   end
 
   function fq_default_poly(a::Vector{fq_default}, ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_init2, libflint), Nothing,
            (Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
            z, length(a), ctx)
      for i = 1:length(a)
         ccall((:fq_default_poly_set_coeff, libflint), Nothing,
               (Ref{fq_default_poly}, Int, Ref{fq_default}, Ref{FqDefaultFiniteField}),
                z, i - 1, a[i], ctx)
      end
      finalizer(_fq_default_poly_clear_fn, z)
      return z
   end
 
   function fq_default_poly(a::Vector{fmpz}, ctx::FqDefaultFiniteField)
      z = new()
      temp = ctx()
      ccall((:fq_default_poly_init2, libflint), Nothing,
            (Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
             z, length(a), ctx)
      for i = 1:length(a)
         temp = ctx(a[i])
         ccall((:fq_default_poly_set_coeff, libflint), Nothing,
               (Ref{fq_default_poly}, Int, Ref{fq_default}, Ref{FqDefaultFiniteField}),
                z, i - 1, temp, ctx)
      end
      finalizer(_fq_default_poly_clear_fn, z)
      return z
   end
 
   function fq_default_poly(a::fmpz_poly, ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_init2, libflint), Nothing,
             (Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
              z, length(a), ctx)
      ccall((:fq_default_poly_set_fmpz_poly, libflint), Nothing,
               (Ref{fq_default_poly}, Ref{fmpz_poly}, Ref{FqDefaultFiniteField}),
                z, a, ctx)
      finalizer(_fq_default_poly_clear_fn, z)
      return z
   end

   function fq_default_poly(a::nmod_poly, ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_init2, libflint), Nothing,
             (Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
              z, length(a), ctx)
      ccall((:fq_default_poly_set_nmod_poly, libflint), Nothing,
               (Ref{fq_default_poly}, Ref{nmod_poly}, Ref{FqDefaultFiniteField}),
                z, a, ctx)
      finalizer(_fq_default_poly_clear_fn, z)
      return z
   end

   function fq_default_poly(a::gfp_poly, ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_init2, libflint), Nothing,
             (Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
              z, length(a), ctx)
      ccall((:fq_default_poly_set_nmod_poly, libflint), Nothing,
               (Ref{fq_default_poly}, Ref{gfp_poly}, Ref{FqDefaultFiniteField}),
                z, a, ctx)
      finalizer(_fq_default_poly_clear_fn, z)
      return z
   end

   function fq_default_poly(a::fmpz_mod_poly, ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_init2, libflint), Nothing,
             (Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
              z, length(a), ctx)
      ccall((:fq_default_poly_set_fmpz_mod_poly, libflint), Nothing,
               (Ref{fq_default_poly}, Ref{fmpz_mod_poly}, Ref{FqDefaultFiniteField}),
                z, a, ctx)
      finalizer(_fq_default_poly_clear_fn, z)
      return z
   end

   function fq_default_poly(a::gfp_fmpz_poly, ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_init2, libflint), Nothing,
             (Ref{fq_default_poly}, Int, Ref{FqDefaultFiniteField}),
              z, length(a), ctx)
      ccall((:fq_default_poly_set_fmpz_mod_poly, libflint), Nothing,
               (Ref{fq_default_poly}, Ref{gfp_fmpz_poly}, Ref{FqDefaultFiniteField}),
                z, a, ctx)
      finalizer(_fq_default_poly_clear_fn, z)
      return z
   end
end
 
function _fq_default_poly_clear_fn(a::fq_default_poly)
   ccall((:fq_default_poly_clear, libflint), Nothing,
         (Ref{fq_default_poly}, Ref{FqDefaultFiniteField}),
          a, base_ring(a))
end
 
mutable struct fq_default_poly_factor
   # fq_default_ctx_struct is 32 bytes on 64 bit machine
   opaque::NTuple{32, Int8} 
   # end of flint struct
   base_field::FqDefaultFiniteField
 
   function fq_default_poly_factor(ctx::FqDefaultFiniteField)
      z = new()
      ccall((:fq_default_poly_factor_init, libflint), Nothing,
            (Ref{fq_default_poly_factor}, Ref{FqDefaultFiniteField}), z, ctx)
      z.base_field = ctx
      finalizer(_fq_default_poly_factor_clear_fn, z)
      return z
   end
end
 
function _fq_default_poly_factor_clear_fn(a::fq_default_poly_factor)
   ccall((:fq_default_poly_factor_clear, libflint), Nothing,
         (Ref{fq_default_poly_factor}, Ref{FqDefaultFiniteField}),
          a, a.base_field)
end

###############################################################################
#
#   FqPolyRing / fq_poly
#
###############################################################################

mutable struct FqPolyRing <: PolyRing{fq}
   base_ring::FqFiniteField
   S::Symbol

   function FqPolyRing(R::FqFiniteField, s::Symbol, cached::Bool = true)
      if cached && haskey(FqPolyID, (R, s))
         return FqPolyID[(R, s)]
      else
         z = new(R,s)
         if cached
            FqPolyID[(R, s)] = z
         end
         return z
      end
   end
end

const FqPolyID = Dict{Tuple{FqFiniteField, Symbol}, FqPolyRing}()

mutable struct fq_poly <: PolyElem{fq}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   parent::FqPolyRing

   function fq_poly()
      z = new()
      ccall((:fq_poly_init, libflint), Nothing, (Ref{fq_poly},), z)
      finalizer(_fq_poly_clear_fn, z)
      return z
   end

   function fq_poly(a::fq_poly)
      z = new()
      ctx = base_ring(parent(a))
      ccall((:fq_poly_init, libflint), Nothing,
            (Ref{fq_poly}, Ref{FqFiniteField}), z, ctx)
      ccall((:fq_poly_set, libflint), Nothing,
            (Ref{fq_poly}, Ref{fq_poly}, Ref{FqFiniteField}),
            z, a, ctx)
      finalizer(_fq_poly_clear_fn, z)
      return z
   end

   function fq_poly(a::fq)
      z = new()
      ctx = parent(a)
      ccall((:fq_poly_init, libflint), Nothing,
            (Ref{fq_poly}, Ref{FqFiniteField}), z, ctx)
      ccall((:fq_poly_set_fq, libflint), Nothing,
            (Ref{fq_poly}, Ref{fq}, Ref{FqFiniteField}),
            z, a, ctx)
      finalizer(_fq_poly_clear_fn, z)
      return z
   end

   function fq_poly(a::Vector{fq})
      z = new()
      ctx = parent(a[1])
      ccall((:fq_poly_init2, libflint), Nothing,
            (Ref{fq_poly}, Int, Ref{FqFiniteField}),
            z, length(a), ctx)
      for i = 1:length(a)
         ccall((:fq_poly_set_coeff, libflint), Nothing,
               (Ref{fq_poly}, Int, Ref{fq}, Ref{FqFiniteField}),
               z, i - 1, a[i], ctx)
      end
      finalizer(_fq_poly_clear_fn, z)
      return z
   end

   function fq_poly(a::Vector{fmpz}, ctx::FqFiniteField)
      z = new()
      temp = ctx()
      ccall((:fq_poly_init2, libflint), Nothing,
            (Ref{fq_poly}, Int, Ref{FqFiniteField}),
            z, length(a), ctx)
      for i = 1:length(a)
         temp = ctx(a[i])
         ccall((:fq_poly_set_coeff, libflint), Nothing,
               (Ref{fq_poly}, Int, Ref{fq}, Ref{FqFiniteField}),
               z, i - 1, temp, ctx)
      end
      finalizer(_fq_poly_clear_fn, z)
      return z
   end

   function fq_poly(a::fmpz_poly, ctx::FqFiniteField)
      z = new()
      ccall((:fq_poly_init2, libflint), Nothing,
            (Ref{fq_poly}, Int, Ref{FqFiniteField}),
            z, length(a), ctx)
      for i = 1:length(a)
         temp = ctx(coeff(a, i-1))
         ccall((:fq_poly_set_coeff, libflint), Nothing,
               (Ref{fq_poly}, Int, Ref{fq}, Ref{FqFiniteField}),
               z, i - 1, temp, ctx)
      end
      finalizer(_fq_poly_clear_fn, z)
      return z
   end
end

function _fq_poly_clear_fn(a::fq_poly)
   ccall((:fq_poly_clear, libflint), Nothing, (Ref{fq_poly},), a)
end

mutable struct fq_poly_factor
  poly::Ptr{fq_poly}
  exp::Ptr{Int}
  num::Int
  alloc::Int
  base_field::FqFiniteField

  function fq_poly_factor(ctx::FqFiniteField)
    z = new()
    ccall((:fq_poly_factor_init, libflint), Nothing,
         (Ref{fq_poly_factor}, Ref{FqFiniteField}), z, ctx)
    z.base_field = ctx
    finalizer(_fq_poly_factor_clear_fn, z)
    return z
  end
end

function _fq_poly_factor_clear_fn(a::fq_poly_factor)
   ccall((:fq_poly_factor_clear, libflint), Nothing,
         (Ref{fq_poly_factor}, Ref{FqFiniteField}),
         a, a.base_field)
end

###############################################################################
#
#   FqNmodPolyRing / fq_nmod_poly
#
###############################################################################

mutable struct FqNmodPolyRing <: PolyRing{fq_nmod}
   base_ring::FqNmodFiniteField
   S::Symbol

   function FqNmodPolyRing(R::FqNmodFiniteField, s::Symbol, cached::Bool = true)
      if cached && haskey(FqNmodPolyID, (R, s))
         return FqNmodPolyID[(R, s)]
      else
         z = new(R,s)
         if cached
            FqNmodPolyID[(R, s)] = z
         end
         return z
      end
   end
end

const FqNmodPolyID = Dict{Tuple{FqNmodFiniteField, Symbol}, FqNmodPolyRing}()

mutable struct fq_nmod_poly <: PolyElem{fq_nmod}
   coeffs::Ptr{Nothing}
   alloc::Int
   length::Int
   parent::FqNmodPolyRing

   function fq_nmod_poly()
      z = new()
      ccall((:fq_nmod_poly_init, libflint), Nothing, (Ref{fq_nmod_poly},), z)
      finalizer(_fq_nmod_poly_clear_fn, z)
      return z
   end

   function fq_nmod_poly(a::fq_nmod_poly)
      z = new()
      ctx = base_ring(parent(a))
      ccall((:fq_nmod_poly_init, libflint), Nothing,
            (Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}), z, ctx)
      ccall((:fq_nmod_poly_set, libflint), Nothing,
            (Ref{fq_nmod_poly}, Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}),
            z, a, ctx)
      finalizer(_fq_nmod_poly_clear_fn, z)
      return z
   end

   function fq_nmod_poly(a::fq_nmod)
      z = new()
      ctx = parent(a)
      ccall((:fq_nmod_poly_init, libflint), Nothing,
            (Ref{fq_nmod_poly}, Ref{FqNmodFiniteField}), z, ctx)
      ccall((:fq_nmod_poly_set_fq_nmod, libflint), Nothing,
            (Ref{fq_nmod_poly}, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
            z, a, ctx)
      finalizer(_fq_nmod_poly_clear_fn, z)
      return z
   end

   function fq_nmod_poly(a::Vector{fq_nmod})
      z = new()
      ctx = parent(a[1])
      ccall((:fq_nmod_poly_init2, libflint), Nothing,
            (Ref{fq_nmod_poly}, Int, Ref{FqNmodFiniteField}),
            z, length(a), ctx)
      for i = 1:length(a)
         ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
               (Ref{fq_nmod_poly}, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
               z, i - 1, a[i], ctx)
      end
      finalizer(_fq_nmod_poly_clear_fn, z)
      return z
   end

   function fq_nmod_poly(a::Vector{fmpz}, ctx::FqNmodFiniteField)
      z = new()
      temp = ctx()
      ccall((:fq_nmod_poly_init2, libflint), Nothing,
            (Ref{fq_nmod_poly}, Int, Ref{FqNmodFiniteField}),
            z, length(a), ctx)
      for i = 1:length(a)
         temp = ctx(a[i])
         ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
               (Ref{fq_nmod_poly}, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
               z, i - 1, temp, ctx)
      end
      finalizer(_fq_nmod_poly_clear_fn, z)
      return z
   end

   function fq_nmod_poly(a::fmpz_poly, ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_poly_init2, libflint), Nothing,
            (Ref{fq_nmod_poly}, Int, Ref{FqNmodFiniteField}),
            z, length(a), ctx)
      for i = 1:length(a)
         temp = ctx(coeff(a,i-1))
         ccall((:fq_nmod_poly_set_coeff, libflint), Nothing,
               (Ref{fq_nmod_poly}, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
               z, i - 1, temp, ctx)
      end
      finalizer(_fq_nmod_poly_clear_fn, z)
      return z
   end
end

function _fq_nmod_poly_clear_fn(a::fq_nmod_poly)
   ccall((:fq_nmod_poly_clear, libflint), Nothing, (Ref{fq_nmod_poly},), a)
end

mutable struct fq_nmod_poly_factor
  poly::Ptr{fq_nmod_poly}
  exp::Ptr{Int}
  num::Int
  alloc::Int
  base_field::FqNmodFiniteField

  function fq_nmod_poly_factor(ctx::FqNmodFiniteField)
    z = new()
    ccall((:fq_nmod_poly_factor_init, libflint), Nothing,
         (Ref{fq_nmod_poly_factor}, Ref{FqNmodFiniteField}), z, ctx)
    z.base_field = ctx
    finalizer(_fq_nmod_poly_factor_clear_fn, z)
    return z
  end
end

function _fq_nmod_poly_factor_clear_fn(a::fq_nmod_poly_factor)
   ccall((:fq_nmod_poly_factor_clear, libflint), Nothing,
         (Ref{fq_nmod_poly_factor}, Ref{FqNmodFiniteField}),
         a, a.base_field)
end

###############################################################################
#
#   FqDefaultMatSpace/fq_default_mat
#
###############################################################################

mutable struct FqDefaultMatSpace <: MatSpace{fq_default}
   base_ring::FqDefaultFiniteField
   nrows::Int
   ncols::Int
 
   function FqDefaultMatSpace(R::FqDefaultFiniteField, r::Int, c::Int, cached::Bool = true)
     (r < 0 || c < 0) && throw(error_dim_negative)
     if cached && haskey(FqDefaultMatID, (R, r, c))
       return FqDefaultMatID[R, r, c]
     else
       z = new(R, r, c)
       if cached
         FqDefaultMatID[R, r, c] = z
       end
       return z
     end
   end
 end
 
 const FqDefaultMatID = Dict{Tuple{FqDefaultFiniteField, Int, Int}, FqDefaultMatSpace}()
 
 mutable struct fq_default_mat <: MatElem{fq_default}
    # fq_default_mat_struct is 32 bytes on 64 bit machine
    opaque::NTuple{32, Int8} 
    # end of flint struct
    
    base_ring::FqDefaultFiniteField
    view_parent
 
    # used by windows, not finalised!!
    function fq_default_mat()
       return new()
    end
 
    function fq_default_mat(r::Int, c::Int, ctx::FqDefaultFiniteField)
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
              z, r, c, ctx)
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function fq_default_mat(r::Int, c::Int, arr::AbstractMatrix{fq_default}, ctx::FqDefaultFiniteField)
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set, libflint), Nothing,
                   (Ref{fq_default_mat}, Int, Int, Ref{fq_default},
                    Ref{FqDefaultFiniteField}),
                    z, i - 1, j - 1, arr[i, j], ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function fq_default_mat(r::Int, c::Int, arr::AbstractVector{fq_default}, ctx::FqDefaultFiniteField)
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set, libflint), Nothing,
                   (Ref{fq_default_mat}, Int, Int, Ref{fq_default},
                    Ref{FqDefaultFiniteField}),
                    z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function fq_default_mat(r::Int, c::Int, arr::AbstractMatrix{fmpz}, ctx::FqDefaultFiniteField)
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set_fmpz, libflint), Nothing,
                  (Ref{fq_default_mat}, Int, Int, Ref{fmpz},
                   Ref{FqDefaultFiniteField}),
                   z, i - 1, j - 1, arr[i, j], ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function fq_default_mat(r::Int, c::Int, arr::AbstractVector{fmpz}, ctx::FqDefaultFiniteField)
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set_fmpz, libflint), Nothing,
                   (Ref{fq_default_mat}, Int, Int, Ref{fmpz},
                    Ref{FqDefaultFiniteField}),
                    z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function fq_default_mat(r::Int, c::Int, arr::AbstractMatrix{T}, ctx::FqDefaultFiniteField) where {T <: Integer}
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set, libflint), Nothing,
                     (Ref{fq_default_mat}, Int, Int, Ref{fq_default},
                      Ref{FqDefaultFiniteField}),
                      z, i - 1, j - 1, ctx(arr[i, j]), ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function fq_default_mat(r::Int, c::Int, arr::AbstractVector{T}, ctx::FqDefaultFiniteField) where {T <: Integer}
       z = new()
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
              z, r, c, ctx)
       for i = 1:r
          for j = 1:c
             ccall((:fq_default_mat_entry_set, libflint), Nothing,
                   (Ref{fq_default_mat}, Int, Int, Ref{fq_default},
                    Ref{FqDefaultFiniteField}),
                    z, i - 1, j - 1, ctx(arr[(i - 1) * c + j]), ctx)
          end
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function fq_default_mat(r::Int, c::Int, d::fq_default)
       z = new()
       ctx = parent(d)
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
              z, r, c, ctx)
       for i = 1:min(r, c)
          ccall((:fq_default_mat_entry_set, libflint), Nothing,
                (Ref{fq_default_mat}, Int, Int, Ref{fq_default},
                 Ref{FqDefaultFiniteField}), z, i - 1, i - 1, d, ctx)
       end
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end
 
    function fq_default_mat(m::fmpz_mat, ctx::FqDefaultFiniteField)
       z = new()
       r = nrows(m)
       c = ncols(m)
       ccall((:fq_default_mat_init, libflint), Nothing,
             (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
              z, r, c, ctx)
       ccall((:fq_default_mat_set_fmpz_mat, libflint), Nothing,
             (Ref{fq_default_mat}, Ref{fmpz_mat}, Ref{FqDefaultFiniteField}),
             z, m, ctx)
       z.base_ring = ctx
       finalizer(_fq_default_mat_clear_fn, z)
       return z
    end

    function fq_default_mat(m::fmpz_mod_mat, ctx::FqDefaultFiniteField)
      z = new()
      r = nrows(m)
      c = ncols(m)
      ccall((:fq_default_mat_init, libflint), Nothing,
            (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
             z, r, c, ctx)
      ccall((:fq_default_mat_set_fmpz_mod_mat, libflint), Nothing,
            (Ref{fq_default_mat}, Ref{fmpz_mod_mat}, Ref{FqDefaultFiniteField}),
            z, m, ctx)
      z.base_ring = ctx
      finalizer(_fq_default_mat_clear_fn, z)
      return z
   end

   function fq_default_mat(m::nmod_mat, ctx::FqDefaultFiniteField)
      z = new()
      r = nrows(m)
      c = ncols(m)
      ccall((:fq_default_mat_init, libflint), Nothing,
            (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
             z, r, c, ctx)
      ccall((:fq_default_mat_set_nmod_mat, libflint), Nothing,
            (Ref{fq_default_mat}, Ref{nmod_mat}, Ref{FqDefaultFiniteField}),
            z, m, ctx)
      z.base_ring = ctx
      finalizer(_fq_default_mat_clear_fn, z)
      return z
   end

   function fq_default_mat(m::gfp_mat, ctx::FqDefaultFiniteField)
      z = new()
      r = nrows(m)
      c = ncols(m)
      ccall((:fq_default_mat_init, libflint), Nothing,
            (Ref{fq_default_mat}, Int, Int, Ref{FqDefaultFiniteField}),
             z, r, c, ctx)
      ccall((:fq_default_mat_set_nmod_mat, libflint), Nothing,
            (Ref{fq_default_mat}, Ref{gfp_mat}, Ref{FqDefaultFiniteField}),
            z, m, ctx)
      z.base_ring = ctx
      finalizer(_fq_default_mat_clear_fn, z)
      return z
   end
 end
 
 function _fq_default_mat_clear_fn(a::fq_default_mat)
    ccall((:fq_default_mat_clear, libflint), Nothing,
          (Ref{fq_default_mat}, Ref{FqDefaultFiniteField}), a, base_ring(a))
 end

###############################################################################
#
#   FqMatSpace/fq_mat
#
###############################################################################

mutable struct FqMatSpace <: MatSpace{fq}
  base_ring::FqFiniteField
  nrows::Int
  ncols::Int

  function FqMatSpace(R::FqFiniteField, r::Int, c::Int, cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    if cached && haskey(FqMatID, (R, r, c))
      return FqMatID[R, r, c]
    else
      z = new(R, r, c)
      if cached
        FqMatID[R, r, c] = z
      end
      return z
    end
  end
end

const FqMatID = Dict{Tuple{FqFiniteField, Int, Int}, FqMatSpace}()

mutable struct fq_mat <: MatElem{fq}
   entries::Ptr{Nothing}
   r::Int
   c::Int
   rows::Ptr{Nothing}
   base_ring::FqFiniteField
   view_parent

   # used by windows, not finalised!!
   function fq_mat()
      return new()
   end

   function fq_mat(r::Int, c::Int, ctx::FqFiniteField)
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{fq_mat}, Int, Int, Ref{FqFiniteField}), z, r, c, ctx)
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function fq_mat(r::Int, c::Int, arr::AbstractMatrix{fq}, ctx::FqFiniteField)
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{fq_mat}, Int, Int, Ref{FqFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_mat_entry_set, libflint), Nothing,
                  (Ref{fq_mat}, Int, Int, Ref{fq}, Ref{FqFiniteField}),
                   z, i - 1, j - 1, arr[i, j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function fq_mat(r::Int, c::Int, arr::AbstractVector{fq}, ctx::FqFiniteField)
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{fq_mat}, Int, Int, Ref{FqFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_mat_entry_set, libflint), Nothing,
                       (Ref{fq_mat}, Int, Int, Ref{fq}, Ref{FqFiniteField}),
                        z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function fq_mat(r::Int, c::Int, arr::AbstractMatrix{fmpz}, ctx::FqFiniteField)
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{fq_mat}, Int, Int, Ref{FqFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fq_mat_entry, libflint), Ptr{fq},
                       (Ref{fq_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fq_set_fmpz, libflint), Nothing,
                  (Ptr{fq}, Ref{fmpz}, Ref{FqFiniteField}), el, arr[i, j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function fq_mat(r::Int, c::Int, arr::AbstractVector{fmpz}, ctx::FqFiniteField)
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{fq_mat}, Int, Int, Ref{FqFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fq_mat_entry, libflint), Ptr{fq},
                       (Ref{fq_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fq_set_fmpz, libflint), Nothing,
                  (Ptr{fq}, Ref{fmpz}, Ref{FqFiniteField}), el, arr[(i - 1) * c + j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function fq_mat(r::Int, c::Int, arr::AbstractMatrix{T}, ctx::FqFiniteField) where {T <: Integer}
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{fq_mat}, Int, Int, Ref{FqFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_mat_entry_set, libflint), Nothing,
                    (Ref{fq_mat}, Int, Int, Ref{fq}, Ref{FqFiniteField}),
                     z, i - 1, j - 1, ctx(arr[i, j]), ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function fq_mat(r::Int, c::Int, arr::AbstractVector{T }, ctx::FqFiniteField) where {T <: Integer}
      z = new()
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{fq_mat}, Int, Int, Ref{FqFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_mat_entry_set, libflint), Nothing,
                       (Ref{fq_mat}, Int, Int, Ref{fq}, Ref{FqFiniteField}),
                        z, i - 1, j - 1, ctx(arr[(i - 1) * c + j]), ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function fq_mat(r::Int, c::Int, d::fq)
      z = new()
      ctx = parent(d)
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{fq_mat}, Int, Int, Ref{FqFiniteField}), z, r, c, ctx)
      for i = 1:min(r, c)
         ccall((:fq_mat_entry_set, libflint), Nothing,
               (Ref{fq_mat}, Int, Int, Ref{fq}, Ref{FqFiniteField}), z, i - 1, i- 1, d, ctx)
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end

   function fq_mat(m::fmpz_mat, ctx::FqFiniteField)
      z = new()
      r = nrows(m)
      c = ncols(m)
      ccall((:fq_mat_init, libflint), Nothing,
            (Ref{fq_mat}, Int, Int, Ref{FqFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el1 = ccall((:fq_mat_entry, libflint), Ptr{fq},
                        (Ref{fq_mat}, Int, Int), z, i - 1, j - 1)
            el2 = ccall((:fmpz_mat_entry, libflint), Ptr{fmpz},
                        (Ref{fmpz_mat}, Int, Int), m, i - 1, j - 1)

            ccall((:fq_set_fmpz, libflint), Nothing,
                  (Ptr{fq}, Ptr{fmpz}, Ref{FqFiniteField}), el1, el2, ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_mat_clear_fn, z)
      return z
   end
end

function _fq_mat_clear_fn(a::fq_mat)
   ccall((:fq_mat_clear, libflint), Nothing, (Ref{fq_mat}, Ref{FqFiniteField}), a, base_ring(a))
end

###############################################################################
#
#   FqNmodMatSpace/fq_nmod_mat
#
###############################################################################

mutable struct FqNmodMatSpace <: MatSpace{fq_nmod}
  base_ring::FqNmodFiniteField
  nrows::Int
  ncols::Int

  function FqNmodMatSpace(R::FqNmodFiniteField, r::Int, c::Int, cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    if cached && haskey(FqNmodMatID, (R, r, c))
      return FqNmodMatID[R, r, c]
    else
      z = new(R, r, c)
      if cached
        FqNmodMatID[R, r, c] = z
      end
      return z
    end
  end
end

const FqNmodMatID = Dict{Tuple{FqNmodFiniteField, Int, Int}, FqNmodMatSpace}()

mutable struct fq_nmod_mat <: MatElem{fq_nmod}
   entries::Ptr{Nothing}
   r::Int
   c::Int
   rows::Ptr{Nothing}
   base_ring::FqNmodFiniteField
   view_parent

   # used by windows, not finalised!!
   function fq_nmod_mat()
      return new()
   end

   function fq_nmod_mat(r::Int, c::Int, ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fq_nmod_mat}, Int, Int, Ref{FqNmodFiniteField}), z, r, c, ctx)
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fq_nmod_mat(r::Int, c::Int, arr::AbstractMatrix{fq_nmod}, ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fq_nmod_mat}, Int, Int, Ref{FqNmodFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
                  (Ref{fq_nmod_mat}, Int, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                   z, i - 1, j - 1, arr[i, j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fq_nmod_mat(r::Int, c::Int, arr::AbstractVector{fq_nmod}, ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fq_nmod_mat}, Int, Int, Ref{FqNmodFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
                       (Ref{fq_nmod_mat}, Int, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                        z, i - 1, j - 1, arr[(i - 1) * c + j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fq_nmod_mat(r::Int, c::Int, arr::AbstractMatrix{fmpz}, ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fq_nmod_mat}, Int, Int, Ref{FqNmodFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fq_nmod_mat_entry, libflint), Ptr{fq_nmod},
                       (Ref{fq_nmod_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fq_nmod_set_fmpz, libflint), Nothing,
                  (Ptr{fq_nmod}, Ref{fmpz}, Ref{FqNmodFiniteField}), el, arr[i, j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fq_nmod_mat(r::Int, c::Int, arr::AbstractVector{fmpz}, ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fq_nmod_mat}, Int, Int, Ref{FqNmodFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el = ccall((:fq_nmod_mat_entry, libflint), Ptr{fq_nmod},
                       (Ref{fq_nmod_mat}, Int, Int), z, i - 1, j - 1)
            ccall((:fq_nmod_set_fmpz, libflint), Nothing,
                  (Ptr{fq_nmod}, Ref{fmpz}, Ref{FqNmodFiniteField}), el, arr[(i - 1) * c + j], ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fq_nmod_mat(r::Int, c::Int, arr::AbstractMatrix{T}, ctx::FqNmodFiniteField) where {T <: Integer}
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fq_nmod_mat}, Int, Int, Ref{FqNmodFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
                    (Ref{fq_nmod_mat}, Int, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                     z, i - 1, j - 1, ctx(arr[i, j]), ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fq_nmod_mat(r::Int, c::Int, arr::AbstractVector{T }, ctx::FqNmodFiniteField) where {T <: Integer}
      z = new()
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fq_nmod_mat}, Int, Int, Ref{FqNmodFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
                       (Ref{fq_nmod_mat}, Int, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}),
                        z, i - 1, j - 1, ctx(arr[(i - 1) * c + j]), ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fq_nmod_mat(r::Int, c::Int, d::fq_nmod)
      z = new()
      ctx = parent(d)
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fq_nmod_mat}, Int, Int, Ref{FqNmodFiniteField}), z, r, c, ctx)
      for i = 1:min(r, c)
         ccall((:fq_nmod_mat_entry_set, libflint), Nothing,
               (Ref{fq_nmod_mat}, Int, Int, Ref{fq_nmod}, Ref{FqNmodFiniteField}), z, i - 1, i- 1, d, ctx)
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end

   function fq_nmod_mat(m::fmpz_mat, ctx::FqNmodFiniteField)
      z = new()
      r = nrows(m)
      c = ncols(m)
      ccall((:fq_nmod_mat_init, libflint), Nothing,
            (Ref{fq_nmod_mat}, Int, Int, Ref{FqNmodFiniteField}), z, r, c, ctx)
      GC.@preserve z for i = 1:r
         for j = 1:c
            el1 = ccall((:fq_nmod_mat_entry, libflint), Ptr{fq_nmod},
                        (Ref{fq_nmod_mat}, Int, Int), z, i - 1, j - 1)
            el2 = ccall((:fmpz_mat_entry, libflint), Ptr{fmpz},
                        (Ref{fmpz_mat}, Int, Int), m, i - 1, j - 1)

            ccall((:fq_nmod_set_fmpz, libflint), Nothing,
                  (Ptr{fq_nmod}, Ptr{fmpz}, Ref{FqNmodFiniteField}), el1, el2, ctx)
         end
      end
      z.base_ring = ctx
      finalizer(_fq_nmod_mat_clear_fn, z)
      return z
   end
end

function _fq_nmod_mat_clear_fn(a::fq_nmod_mat)
   ccall((:fq_nmod_mat_clear, libflint), Nothing, (Ref{fq_nmod_mat}, Ref{FqNmodFiniteField}), a, base_ring(a))
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

const IntegerUnion = Union{Integer, fmpz}

const ZmodNFmpzPolyRing = Union{FmpzModPolyRing, GFPFmpzPolyRing}

const Zmodn_poly = Union{nmod_poly, gfp_poly}

const Zmodn_fmpz_poly = Union{fmpz_mod_poly, gfp_fmpz_poly}

const Zmodn_mpoly = Union{nmod_mpoly, gfp_mpoly}

const FlintPuiseuxSeriesElem{T} = Union{FlintPuiseuxSeriesRingElem{T},
                            FlintPuiseuxSeriesFieldElem{T}} where T <: RingElem

const Zmodn_mat = Union{nmod_mat, gfp_mat}

const FlintMPolyUnion = Union{fmpz_mpoly, fmpq_mpoly, nmod_mpoly, gfp_mpoly,
                              fq_nmod_mpoly}

