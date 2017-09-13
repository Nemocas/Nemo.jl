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
        ccall((:fmpz_init, :libflint), Void, (Ptr{fmpz},), &z)
        finalizer(z, _fmpz_clear_fn)
        return z
    end

    function fmpz(x::Int)
        z = new()
        ccall((:fmpz_init_set_si, :libflint), Void, (Ptr{fmpz}, Int), &z, x)
        finalizer(z, _fmpz_clear_fn)
        return z
    end

    function fmpz(x::UInt)
        z = new()
        ccall((:fmpz_init_set_ui, :libflint), Void, (Ptr{fmpz}, UInt), &z, x)
        finalizer(z, _fmpz_clear_fn)
        return z
    end

    function fmpz(x::BigInt)
        z = new()
        ccall((:fmpz_init, :libflint), Void, (Ptr{fmpz},), &z)
        ccall((:fmpz_set_mpz, :libflint), Void, (Ptr{fmpz}, Ptr{BigInt}), &z, &x)
        finalizer(z, _fmpz_clear_fn)
        return z
    end

    function fmpz(x::Float64)
        !isinteger(x) && throw(InexactError())
        z = new()
        ccall((:fmpz_init, :libflint), Void, (Ptr{fmpz},), &z)
        ccall((:fmpz_set_d, :libflint), Void, (Ptr{fmpz}, Cdouble), &z, x)
        finalizer(z, _fmpz_clear_fn)
        return z
    end

    fmpz(x::fmpz) = x
end

function _fmpz_clear_fn(a::fmpz)
   ccall((:fmpz_clear, :libflint), Void, (Ptr{fmpz},), &a)
end

mutable struct fmpz_factor
   sign::Cint
   p::Ptr{Void} # Array of fmpz_struct's
   exp::Ptr{UInt}
   alloc::Int
   num::Int

   function fmpz_factor()
      z = new()
      ccall((:fmpz_factor_init, :libflint), Void, (Ptr{fmpz_factor}, ), &z)
      finalizer(z, _fmpz_factor_clear_fn)
      return z
   end
end

function _fmpz_factor_clear_fn(a::fmpz_factor)
   ccall((:fmpz_factor_clear, :libflint), Void,
         (Ptr{fmpz_factor}, ), &a)
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
      ccall((:fmpq_init, :libflint), Void, (Ptr{fmpq},), &z)
      finalizer(z, _fmpq_clear_fn)
      return z
   end

   function fmpq(a::fmpz, b::fmpz)
      iszero(b) && throw(DivideError())
      z = new()
      ccall((:fmpq_init, :libflint), Void, (Ptr{fmpq},), &z)
      ccall((:fmpq_set_fmpz_frac, :libflint), Void,
            (Ptr{fmpq}, Ptr{fmpz}, Ptr{fmpz}), &z, &a, &b)
      finalizer(z, _fmpq_clear_fn)
      return z
   end

   function fmpq(a::fmpz)
      z = new()
      ccall((:fmpq_init, :libflint), Void, (Ptr{fmpq},), &z)
      b = fmpz(1)
      ccall((:fmpq_set_fmpz_frac, :libflint), Void,
            (Ptr{fmpq}, Ptr{fmpz}, Ptr{fmpz}), &z, &a, &b)
      finalizer(z, _fmpq_clear_fn)
      return z
   end

   function fmpq(a::Int, b::Int)
      b == 0 && throw(DivideError())
      z = new()
      ccall((:fmpq_init, :libflint), Void, (Ptr{fmpq},), &z)
      ccall((:fmpq_set_si, :libflint), Void,
            (Ptr{fmpq}, Int, Int), &z, a, b)
      finalizer(z, _fmpq_clear_fn)
      return z
   end

   function fmpq(a::Int)
      z = new()
      ccall((:fmpq_init, :libflint), Void, (Ptr{fmpq},), &z)
      ccall((:fmpq_set_si, :libflint), Void,
            (Ptr{fmpq}, Int, Int), &z, a, 1)
      finalizer(z, _fmpq_clear_fn)
      return z
   end
end

_fmpq_clear_fn(a::fmpq) = ccall((:fmpq_clear, :libflint), Void, (Ptr{fmpq},), &a)

###############################################################################
#
#   FmpzPolyRing / fmpz_poly
#
###############################################################################

mutable struct FmpzPolyRing <: PolyRing{fmpz}
   base_ring::FlintIntegerRing
   S::Symbol

   function FmpzPolyRing(s::Symbol, cached::Bool = true)
      if haskey(FmpzPolyID, s)
         return FmpzPolyID[s]
      else
         z = new(FlintZZ, s)
         if cached
            FmpzPolyID[s] = z
         end
         return z
      end
   end
end

const FmpzPolyID = Dict{Symbol, FmpzPolyRing}()

mutable struct fmpz_poly <: PolyElem{fmpz}
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   parent::FmpzPolyRing

   function fmpz_poly()
      z = new()
      ccall((:fmpz_poly_init, :libflint), Void, (Ptr{fmpz_poly},), &z)
      finalizer(z, _fmpz_poly_clear_fn)
      return z
   end

   function fmpz_poly(a::Array{fmpz, 1})
      z = new()
      ccall((:fmpz_poly_init2, :libflint), Void, 
            (Ptr{fmpz_poly}, Int), &z, length(a))
      for i = 1:length(a)
         ccall((:fmpz_poly_set_coeff_fmpz, :libflint), Void, 
                     (Ptr{fmpz_poly}, Int, Ptr{fmpz}), &z, i - 1, &a[i])
      end
      finalizer(z, _fmpz_poly_clear_fn)
      return z
   end

   function fmpz_poly(a::Int)
      z = new()
      ccall((:fmpz_poly_init, :libflint), Void, (Ptr{fmpz_poly},), &z)
      ccall((:fmpz_poly_set_si, :libflint), Void, (Ptr{fmpz_poly}, Int), &z, a)
      finalizer(z, _fmpz_poly_clear_fn)
      return z
   end

   function fmpz_poly(a::fmpz)
      z = new()
      ccall((:fmpz_poly_init, :libflint), Void, (Ptr{fmpz_poly},), &z)
      ccall((:fmpz_poly_set_fmpz, :libflint), Void, 
            (Ptr{fmpz_poly}, Ptr{fmpz}), &z, &a)
      finalizer(z, _fmpz_poly_clear_fn)
      return z
   end

   function fmpz_poly(a::fmpz_poly)
      z = new()
      ccall((:fmpz_poly_init, :libflint), Void, (Ptr{fmpz_poly},), &z)
      ccall((:fmpz_poly_set, :libflint), Void, 
            (Ptr{fmpz_poly}, Ptr{fmpz_poly}), &z, &a)
      finalizer(z, _fmpz_poly_clear_fn)
      return z
   end
end

function _fmpz_poly_clear_fn(a::fmpz_poly)
   ccall((:fmpz_poly_clear, :libflint), Void, (Ptr{fmpz_poly},), &a)
end

mutable struct fmpz_poly_factor
  d::Int # fmpz
  p::Ptr{fmpz_poly} # array of flint fmpz_poly_struct's
  exp::Ptr{Int}
  num::Int
  alloc::Int

  function fmpz_poly_factor()
    z = new()
    ccall((:fmpz_poly_factor_init, :libflint), Void,
                (Ptr{fmpz_poly_factor}, ), &z)
    finalizer(z, _fmpz_poly_factor_clear_fn)
    return z
  end
end

function _fmpz_poly_factor_clear_fn(f::fmpz_poly_factor)
  ccall((:fmpz_poly_factor_clear, :libflint), Void,
            (Ptr{fmpz_poly_factor}, ), &f)
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
      if haskey(FmpqPolyID, s)
         return FmpqPolyID[s]
      else
         z = new(R, s)
         if cached
            FmpqPolyID[s] = z
         end
         return z
      end
   end
end

const FmpqPolyID = Dict{Symbol, FmpqPolyRing}()

mutable struct fmpq_poly <: PolyElem{fmpq}
   coeffs::Ptr{Int}
   den::Int 
   alloc::Int
   length::Int
   parent::FmpqPolyRing

   function fmpq_poly()
      z = new()
      ccall((:fmpq_poly_init, :libflint), Void, (Ptr{fmpq_poly},), &z)
      finalizer(z, _fmpq_poly_clear_fn)
      return z
   end

   function fmpq_poly(a::Array{fmpq, 1})
      z = new()
      ccall((:fmpq_poly_init2, :libflint), Void, 
            (Ptr{fmpq_poly}, Int), &z, length(a))
      for i = 1:length(a)
         ccall((:fmpq_poly_set_coeff_fmpq, :libflint), Void, 
                     (Ptr{fmpq_poly}, Int, Ptr{fmpq}), &z, i - 1, &a[i])
      end
      finalizer(z, _fmpq_poly_clear_fn)
      return z
   end

   function fmpq_poly(a::Int)
      z = new()
      ccall((:fmpq_poly_init, :libflint), Void, (Ptr{fmpq_poly},), &z)
      ccall((:fmpq_poly_set_si, :libflint), Void, (Ptr{fmpq_poly}, Int), &z, a)
      finalizer(z, _fmpq_poly_clear_fn)
      return z
   end

   function fmpq_poly(a::fmpz)
      z = new()
      ccall((:fmpq_poly_init, :libflint), Void, (Ptr{fmpq_poly},), &z)
      ccall((:fmpq_poly_set_fmpz, :libflint), Void, 
            (Ptr{fmpq_poly}, Ptr{fmpz}), &z, &a)
      finalizer(z, _fmpq_poly_clear_fn)
      return z
   end

   function fmpq_poly(a::fmpq)
      z = new()
      ccall((:fmpq_poly_init, :libflint), Void, (Ptr{fmpq_poly},), &z)
      ccall((:fmpq_poly_set_fmpq, :libflint), Void,
            (Ptr{fmpq_poly}, Ptr{fmpq}), &z, &a)
      finalizer(z, _fmpq_poly_clear_fn)
      return z
   end

   function fmpq_poly(a::fmpz_poly)
      z = new()
      ccall((:fmpq_poly_init, :libflint), Void, (Ptr{fmpq_poly},), &z)
      ccall((:fmpq_poly_set_fmpz_poly, :libflint), Void, 
            (Ptr{fmpq_poly}, Ptr{fmpz_poly}), &z, &a)
      finalizer(z, _fmpq_poly_clear_fn)
      return z
   end

   function fmpq_poly(a::fmpq_poly)
      z = new()
      ccall((:fmpq_poly_init, :libflint), Void, (Ptr{fmpq_poly},), &z)
      ccall((:fmpq_poly_set, :libflint), Void, 
            (Ptr{fmpq_poly}, Ptr{fmpq_poly}), &z, &a)
      finalizer(z, _fmpq_poly_clear_fn)
      return z
   end
end

function _fmpq_poly_clear_fn(a::fmpq_poly)
   ccall((:fmpq_poly_clear, :libflint), Void, (Ptr{fmpq_poly},), &a)
end

###############################################################################
#
#   NmodPolyRing / nmod_poly
#
###############################################################################

mutable struct NmodPolyRing <: PolyRing{Generic.Res{fmpz}}
  base_ring::Generic.ResRing{fmpz}
  S::Symbol
  n::UInt

  function NmodPolyRing(R::Generic.ResRing{fmpz}, s::Symbol, cached::Bool = true)
    m = UInt(modulus(R))
    if haskey(NmodPolyRingID, (m, s))
       return NmodPolyRingID[m, s]
    else
       z = new(R, s, m)
       if cached
          NmodPolyRingID[m, s] = z
       end
       return z
    end
  end
end

const NmodPolyRingID = Dict{Tuple{UInt, Symbol}, NmodPolyRing}()

mutable struct nmod_poly <: PolyElem{Generic.Res{fmpz}}
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   mod_n::UInt
   mod_ninv::UInt
   mod_norm::UInt
   parent::NmodPolyRing

   function nmod_poly(n::UInt)
      z = new()
      ccall((:nmod_poly_init, :libflint), Void, (Ptr{nmod_poly}, UInt), &z, n)
      finalizer(z, _nmod_poly_clear_fn)
      return z
   end

   function nmod_poly(n::UInt, a::UInt)
      z = new()
      ccall((:nmod_poly_init, :libflint), Void, (Ptr{nmod_poly}, UInt), &z, n)
      ccall((:nmod_poly_set_coeff_ui, :libflint), Void,
              (Ptr{nmod_poly}, Int, UInt), &z, 0, a)
      finalizer(z, _nmod_poly_clear_fn)
      return z
   end

   function nmod_poly(n::UInt, a::Int)
      z = new()
      ccall((:nmod_poly_init, :libflint), Void, (Ptr{nmod_poly}, UInt), &z, n)
      ccall((:nmod_poly_set_coeff_ui, :libflint), Void,
              (Ptr{nmod_poly}, Int, UInt), &z, 0, mod(a, n))
      finalizer(z, _nmod_poly_clear_fn)
      return z
   end

   function nmod_poly(n::UInt, arr::Array{fmpz, 1})
      z = new()
      ccall((:nmod_poly_init2, :libflint), Void,
            (Ptr{nmod_poly}, UInt, Int), &z, n, length(arr))
      for i in 1:length(arr)
         tt = ccall((:fmpz_fdiv_ui, :libflint), UInt, (Ptr{fmpz}, UInt), &arr[i], n)
         ccall((:nmod_poly_set_coeff_ui, :libflint), Void,
              (Ptr{nmod_poly}, Int, UInt), &z, i - 1, tt)
      end
      finalizer(z, _nmod_poly_clear_fn)
      return z
   end

   function nmod_poly(n::UInt, arr::Array{UInt, 1})
      z = new()
      ccall((:nmod_poly_init2, :libflint), Void,
            (Ptr{nmod_poly}, UInt, Int), &z, n, length(arr))
      for i in 1:length(arr)
         ccall((:nmod_poly_set_coeff_ui, :libflint), Void,
              (Ptr{nmod_poly}, Int, UInt), &z, i - 1, arr[i])
      end
      finalizer(z, _nmod_poly_clear_fn)
      return z
   end

   function nmod_poly(n::UInt, arr::Array{Generic.Res{fmpz}, 1})
      z = new()
      ccall((:nmod_poly_init2, :libflint), Void,
            (Ptr{nmod_poly}, UInt, Int), &z, n, length(arr))
      for i in 1:length(arr)
         tt = ccall((:fmpz_fdiv_ui, :libflint), UInt, (Ptr{fmpz}, UInt), &(arr[i]).data, n)
         ccall((:nmod_poly_set_coeff_ui, :libflint), Void,
              (Ptr{nmod_poly}, Int, UInt), &z, i-1, tt)
      end
      finalizer(z, _nmod_poly_clear_fn)
      return z
   end

   function nmod_poly(n::UInt, f::fmpz_poly)
      z = new()
      ccall((:nmod_poly_init2, :libflint), Void,
            (Ptr{nmod_poly}, UInt, Int), &z, n, length(f))
      ccall((:fmpz_poly_get_nmod_poly, :libflint), Void,
            (Ptr{nmod_poly}, Ptr{fmpz_poly}), &z, &f)
      finalizer(z, _nmod_poly_clear_fn)
      return z
   end

   function nmod_poly(n::UInt, f::nmod_poly)
      z = new()
      ccall((:nmod_poly_init2, :libflint), Void, 
            (Ptr{nmod_poly}, UInt, Int), &z, n, length(f))
      ccall((:nmod_poly_set, :libflint), Void,
            (Ptr{nmod_poly}, Ptr{nmod_poly}), &z, &f)
      finalizer(z, _nmod_poly_clear_fn)
      return z
   end
end

function _nmod_poly_clear_fn(x::nmod_poly)
  ccall((:nmod_poly_clear, :libflint), Void, (Ptr{nmod_poly}, ), &x)
end

mutable struct nmod_poly_factor
  poly::Ptr{nmod_poly}  # array of flint nmod_poly_struct's
  exp::Ptr{Int} 
  num::Int
  alloc::Int
  n::UInt
    
  function nmod_poly_factor(n::UInt)
    z = new()
    ccall((:nmod_poly_factor_init, :libflint), Void,
            (Ptr{nmod_poly_factor}, ), &z)
    z.n = n
    finalizer(z, _nmod_poly_factor_clear_fn)
    return z
  end
end

function _nmod_poly_factor_clear_fn(a::nmod_poly_factor)
  ccall((:nmod_poly_factor_clear, :libflint), Void,
          (Ptr{nmod_poly_factor}, ), &a)
end

###############################################################################
#
#   FmpzModPolyRing / fmpz_mod_poly
#
###############################################################################

mutable struct FmpzModPolyRing <: PolyRing{Generic.Res{fmpz}}
  base_ring::Generic.ResRing{fmpz}
  S::Symbol
  n::fmpz

  function FmpzModPolyRing(R::Generic.ResRing{fmpz}, s::Symbol, cached::Bool = true)
    m = modulus(R)
    if haskey(FmpzModPolyRingID, (m, s))
       return FmpzModPolyRingID[m, s]
    else
       z = new(R, s, m)
       if cached
          FmpzModPolyRingID[m ,s] = z
       end
       return z
    end
  end
end

const FmpzModPolyRingID = Dict{Tuple{fmpz, Symbol}, FmpzModPolyRing}()

mutable struct fmpz_mod_poly <: PolyElem{Generic.Res{fmpz}}
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   p::Int
   parent::FmpzModPolyRing

   function fmpz_mod_poly(n::fmpz)
      z = new()
      ccall((:fmpz_mod_poly_init, :libflint), Void,
            (Ptr{fmpz_mod_poly}, Ptr{fmpz}), &z, &n)
      finalizer(z, _fmpz_mod_poly_clear_fn)
      return z
   end

   function fmpz_mod_poly(n::fmpz, a::UInt)
      z = new()
      ccall((:fmpz_mod_poly_init, :libflint), Void, 
            (Ptr{fmpz_mod_poly}, Ptr{fmpz}), &z, &n)
      ccall((:fmod_poly_set_coeff_ui, :libflint), Void,
              (Ptr{fmpz_mod_poly}, Int, UInt), &z, 0, a)
      finalizer(z, _fmpz_mod_poly_clear_fn)
      return z
   end

   function fmpz_mod_poly(n::fmpz, a::fmpz)
      z = new()
      ccall((:fmpz_mod_poly_init, :libflint), Void, 
            (Ptr{fmpz_mod_poly}, Ptr{fmpz}), &z, &n)
      ccall((:fmpz_mod_poly_set_coeff_fmpz, :libflint), Void,
              (Ptr{fmpz_mod_poly}, Int, Ptr{fmpz}), &z, 0, &a)
      finalizer(z, _fmpz_mod_poly_clear_fn)
      return z
   end

   function fmpz_mod_poly(n::fmpz, arr::Array{fmpz, 1})
      length(arr) == 0 && error("Array must have length > 0")
      z = new()
      ccall((:fmpz_mod_poly_init2, :libflint), Void,
            (Ptr{fmpz_mod_poly}, Ptr{fmpz}, Int), &z, &n, length(arr))
      for i in 1:length(arr)
         ccall((:fmpz_mod_poly_set_coeff_fmpz, :libflint), Void,
              (Ptr{fmpz_mod_poly}, Int, Ptr{fmpz}), &z, i - 1, &arr[i])
      end
      finalizer(z, _fmpz_mod_poly_clear_fn)
      return z
   end

   function fmpz_mod_poly(n::fmpz, arr::Array{Generic.Res{fmpz}, 1})
      z = new()
      ccall((:fmpz_mod_poly_init2, :libflint), Void,
            (Ptr{fmpz_mod_poly}, Ptr{fmpz}, Int), &z, &n, length(arr))
      for i in 1:length(arr)
         ccall((:fmpz_mod_poly_set_coeff_fmpz, :libflint), Void,
              (Ptr{fmpz_mod_poly}, Int, Ptr{fmpz}), &z, i - 1, &(arr[i].data))
      end
      finalizer(z, _fmpz_mod_poly_clear_fn)
      return z
   end

   function fmpz_mod_poly(n::fmpz, f::fmpz_poly)
      z = new()
      ccall((:fmpz_mod_poly_init2, :libflint), Void,
            (Ptr{fmpz_mod_poly}, Ptr{fmpz}, Int), &z, &n, length(f))
      ccall((:fmpz_mod_poly_set_fmpz_poly, :libflint), Void,
            (Ptr{fmpz_mod_poly}, Ptr{fmpz_poly}), &z, &f)
      finalizer(z, _fmpz_mod_poly_clear_fn)
      return z
   end

   function fmpz_mod_poly(n::fmpz, f::fmpz_mod_poly)
      z = new()
      ccall((:fmpz_mod_poly_init2, :libflint), Void,
            (Ptr{fmpz_mod_poly}, Ptr{fmpz}, Int), &z, &n, length(f))
      ccall((:fmpz_mod_poly_set, :libflint), Void,
            (Ptr{fmpz_mod_poly}, Ptr{fmpz_mod_poly}), &z, &f)
      finalizer(z, _fmpz_mod_poly_clear_fn)
      return z
   end
end

function _fmpz_mod_poly_clear_fn(x::fmpz_mod_poly)
  ccall((:fmpz_mod_poly_clear, :libflint), Void, (Ptr{fmpz_mod_poly}, ), &x)
end

mutable struct fmpz_mod_poly_factor
  poly::Ptr{fmpz_mod_poly}
  exp::Ptr{Int} 
  num::Int
  alloc::Int
  n::fmpz
    
  function fmpz_mod_poly_factor(n::fmpz)
    z = new()
    ccall((:fmpz_mod_poly_factor_init, :libflint), Void,
            (Ptr{fmpz_mod_poly_factor}, ), &z)
    z.n = n
    finalizer(z, _fmpz_mod_poly_factor_clear_fn)
    return z
  end
end

function _fmpz_mod_poly_factor_clear_fn(a::fmpz_mod_poly_factor)
  ccall((:fmpz_mod_poly_factor_clear, :libflint), Void,
          (Ptr{fmpz_mod_poly_factor}, ), &a)
end

###############################################################################
#
#   FmpzMPolyRing / fmpz_mpoly
#
###############################################################################

# S is a Symbol which can take the values:
# :lex
# :deglex
# :degrevlex
# 
# T is an Int which is the number of variables
# (plus one if ordered by total degree)

mutable struct FmpzMPolyRing{S, N} <: PolyRing{fmpz}
   n::Int
   ord::Cint
   base_ring::FlintIntegerRing
   S::Array{Symbol, 1}
   num_vars::Int

   function FmpzMPolyRing{S, N}(s::Array{Symbol, 1}, cached::Bool = true) where {S, N}
      if haskey(FmpzMPolyID, (s, S, N))
         return FmpzMPolyID[s, S, N]
      else 
         if S == :lex
            ord = 0
         elseif S == :deglex
            ord = 1
         elseif S == :degrevlex
            ord = 2
         end

         z = new{S, N}()
         ccall((:fmpz_mpoly_ctx_init, :libflint), Void,
               (Ptr{FmpzMPolyRing}, Int, Int),
               &z, length(s), ord)
         z.base_ring = FlintZZ
         z.S = s
         z.num_vars = length(s)
         finalizer(z, _fmpz_mpoly_ctx_clear_fn)
         if cached
            FmpzMPolyID[s, S, N] = z
         end
         return z
      end
   end
end

const FmpzMPolyID = Dict{Tuple{Array{Symbol, 1}, Symbol, Int}, PolyRing{fmpz}}()

mutable struct fmpz_mpoly{S, N} <: PolyElem{fmpz}
   coeffs::Ptr{Void}
   exps::Ptr{Void}
   alloc::Int
   length::Int
   bits::Int
   parent::FmpzMPolyRing

   function fmpz_mpoly(ctx::FmpzMPolyRing{S, N}) where {S, N}
      z = new{S, N}()
      ccall((:fmpz_mpoly_init, :libflint), Void, 
            (Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing},), &z, &ctx)
      finalizer(z, _fmpz_mpoly_clear_fn)
      return z
   end
   
   function fmpz_mpoly(ctx::FmpzMPolyRing{S, N}, a::Array{fmpz, 1}, b::Array{NTuple{N, Int}, 1}) where {S, N}
      z = new{S, N}()
      ccall((:fmpz_mpoly_init, :libflint), Void, 
            (Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing},), &z, &ctx)
      m = 0
      for i = 1:length(b)
         for j = 1:N
            if b[i][j] > m
               m = b[i][j]
            end
         end
      end
      bits = 8
      while ndigits(m, 2) >= bits
         bits *= 2
      end
      deg = ctx.ord == :deglex || ctx.ord == :degrevlex ? 1 : 0
      ccall((:fmpz_mpoly_fit_length, :libflint), Void,
            (Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}), &z, length(a), &ctx)
      ccall((:fmpz_mpoly_fit_bits, :libflint), Void,
            (Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}), &z, bits, &ctx)
      for i = 1:length(a)
         ccall((:fmpz_mpoly_set_coeff_fmpz, :libflint), Void,
            (Ptr{fmpz_mpoly}, Int, Ptr{fmpz}, Ptr{FmpzMPolyRing}),
                                                        &z, i - 1, &a[i], &ctx)
         A = [b[i][j + deg] for j = 1:N - deg]
         ccall((:fmpz_mpoly_set_monomial, :libflint), Void,
            (Ptr{fmpz_mpoly}, Int, Ptr{Int}, Ptr{FmpzMPolyRing}),
                                                            &z, i - 1, A, &ctx)
      end
      ccall((:_fmpz_mpoly_set_length, :libflint), Void,
            (Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}), &z, length(a), &ctx)
      finalizer(z, _fmpz_mpoly_clear_fn)
      return z
   end

   function fmpz_mpoly(ctx::FmpzMPolyRing{S, N}, a::Int) where {S, N}
      z = new{S, N}()
      ccall((:fmpz_mpoly_init, :libflint), Void, 
            (Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing},), &z, &ctx)
      ccall((:fmpz_mpoly_set_si, :libflint), Void,
            (Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}), &z, a, &ctx)
      finalizer(z, _fmpz_mpoly_clear_fn)
      return z
   end

   function fmpz_mpoly(ctx::FmpzMPolyRing{S, N}, a::fmpz) where {S, N}
      z = new{S, N}()
      ccall((:fmpz_mpoly_init, :libflint), Void, 
            (Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing},), &z, &ctx)
      ccall((:fmpz_mpoly_set_fmpz, :libflint), Void,
            (Ptr{fmpz_mpoly}, Ptr{fmpz}, Ptr{FmpzMPolyRing}), &z, &a, &ctx)
      finalizer(z, _fmpz_mpoly_clear_fn)
      return z
   end
end

function _fmpz_mpoly_clear_fn(a::fmpz_mpoly)
  ccall((:fmpz_mpoly_clear, :libflint), Void,
          (Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}), &a, &a.parent)
end

function _fmpz_mpoly_ctx_clear_fn(a::FmpzMPolyRing)
  ccall((:fmpz_mpoly_ctx_clear, :libflint), Void,
          (Ptr{FmpzMPolyRing},), &a)
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
   sparse_modulus :: Int
   a :: Ptr{Void}
   j :: Ptr{Void}
   len :: Int
   mod_coeffs :: Ptr{Void}
   mod_alloc :: Int
   mod_length :: Int
   mod_n :: Int
   mod_ninv :: Int
   mod_norm :: Int
   inv_coeffs :: Ptr{Void}
   inv_alloc :: Int
   inv_length :: Int
   inv_n :: Int
   inv_ninv :: Int
   inv_norm :: Int
   var :: Ptr{Void}

   function FqNmodFiniteField(c::fmpz, deg::Int, s::Symbol, cached::Bool = true)
      if haskey(FqNmodFiniteFieldID, (c, deg, s))
         return FqNmodFiniteFieldID[c, deg, s]
      else
         d = new()
         ccall((:fq_nmod_ctx_init, :libflint), Void, 
               (Ptr{FqNmodFiniteField}, Ptr{fmpz}, Int, Ptr{UInt8}), 
			    &d, &c, deg, string(s))
         if cached
            FqNmodFiniteFieldID[c, deg, s] = d
         end
         finalizer(d, _FqNmodFiniteField_clear_fn)
         return d
      end
   end

   function FqNmodFiniteField(f::nmod_poly, s::Symbol, cached::Bool = true)
      if haskey(FqNmodFiniteFieldIDPol, (parent(f), f, s))
         return FqNmodFiniteFieldIDPol[parent(f), f, s]
      else
         z = new()
         ccall((:fq_nmod_ctx_init_modulus, :libflint), Void, 
            (Ptr{FqNmodFiniteField}, Ptr{nmod_poly}, Ptr{UInt8}), 
	      &z, &f, string(s))
         if cached
            FqNmodFiniteFieldIDPol[parent(f), f, s] = z
         end
         finalizer(z, _FqNmodFiniteField_clear_fn)
         return z
      end
   end
end

const FqNmodFiniteFieldID = Dict{Tuple{fmpz, Int, Symbol}, FqNmodFiniteField}()

const FqNmodFiniteFieldIDPol = Dict{Tuple{NmodPolyRing, nmod_poly, Symbol},
                                    FqNmodFiniteField}()

function _FqNmodFiniteField_clear_fn(a :: FqNmodFiniteField)
   ccall((:fq_nmod_ctx_clear, :libflint), Void, (Ptr{FqNmodFiniteField},), &a)
end

mutable struct fq_nmod <: FinFieldElem
   coeffs :: Ptr{Void}
   alloc :: Int
   length :: Int
   n :: Int
   ninv :: Int
   norm :: Int
   parent::FqNmodFiniteField

   function fq_nmod(ctx::FqNmodFiniteField)
      d = new()
      ccall((:fq_nmod_init2, :libflint), Void, 
            (Ptr{fq_nmod}, Ptr{FqNmodFiniteField}), &d, &ctx)
      finalizer(d, _fq_nmod_clear_fn)
      return d
   end

   function fq_nmod(ctx::FqNmodFiniteField, x::Int)
      d = new()
      ccall((:fq_nmod_init2, :libflint), Void,
            (Ptr{fq_nmod}, Ptr{FqNmodFiniteField}), &d, &ctx)
      finalizer(d, _fq_nmod_clear_fn)
      ccall((:fq_nmod_set_si, :libflint), Void, 
                (Ptr{fq_nmod}, Int, Ptr{FqNmodFiniteField}), &d, x, &ctx)
      return d
   end

   function fq_nmod(ctx::FqNmodFiniteField, x::fmpz)
      d = new()
      ccall((:fq_nmod_init2, :libflint), Void, 
            (Ptr{fq_nmod}, Ptr{FqNmodFiniteField}), &d, &ctx)
      finalizer(d, _fq_nmod_clear_fn)
      ccall((:fq_nmod_set_fmpz, :libflint), Void, 
            (Ptr{fq_nmod}, Ptr{fmpz}, Ptr{FqNmodFiniteField}), &d, &x, &ctx)
      return d
   end

      function fq_nmod(ctx::FqNmodFiniteField, x::fq_nmod)
      d = new()
      ccall((:fq_nmod_init2, :libflint), Void, 
            (Ptr{fq_nmod}, Ptr{FqNmodFiniteField}), &d, &ctx)
      finalizer(d, _fq_nmod_clear_fn)
      ccall((:fq_nmod_set, :libflint), Void, 
            (Ptr{fq_nmod}, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}), &d, &x, &ctx)
      return d
   end
end

function _fq_nmod_clear_fn(a::fq_nmod)
   ccall((:fq_nmod_clear, :libflint), Void, 
         (Ptr{fq_nmod}, Ptr{FqNmodFiniteField}), &a, &a.parent)
end

###############################################################################
#
#   FqFiniteField / fq
#
###############################################################################

mutable struct FqFiniteField <: FinField
   p::Int # fmpz
   sparse_modulus::Int
   a::Ptr{Void}
   j::Ptr{Void}
   len::Int
   mod_coeffs::Ptr{Void}
   mod_alloc::Int
   mod_length::Int
   mod_p::Int # fmpz
   inv_coeffs::Ptr{Void}
   inv_alloc::Int
   inv_length::Int
   inv_p::Int # fmpz
   var::Ptr{Void}

   function FqFiniteField(char::fmpz, deg::Int, s::Symbol, cached::Bool = true)
      if haskey(FqFiniteFieldID, (char, deg, s))
         return FqFiniteFieldID[char, deg, s]
      else
         d = new()
         finalizer(d, _FqFiniteField_clear_fn)
         ccall((:fq_ctx_init, :libflint), Void,
               (Ptr{FqFiniteField}, Ptr{fmpz}, Int, Ptr{UInt8}),
                  &d, &char, deg, string(s))
         if cached
            FqFiniteFieldID[char, deg, s] = d
         end
         return d
      end
   end
   
   function FqFiniteField(f::fmpz_mod_poly, s::Symbol, cached::Bool = true)
      if haskey(FqFiniteFieldIDPol, (f, s))
         return FqFiniteFieldIDPol[f, s]
      else
         z = new()
         ccall((:fq_ctx_init_modulus, :libflint), Void,
               (Ptr{FqFiniteField}, Ptr{fmpz_mod_poly}, Ptr{UInt8}),
                  &z, &f, string(s))
         if cached
            FqFiniteFieldIDPol[f, s] = z
         end
         finalizer(z, _FqFiniteField_clear_fn)
         return z
      end
   end
end

const FqFiniteFieldID = Dict{Tuple{fmpz, Int, Symbol}, FqFiniteField}()

const FqFiniteFieldIDPol = Dict{Tuple{fmpz_mod_poly, Symbol}, FqFiniteField}()

function _FqFiniteField_clear_fn(a :: FqFiniteField)
   ccall((:fq_ctx_clear, :libflint), Void, (Ptr{FqFiniteField},), &a)
end

mutable struct fq <: FinFieldElem
   coeffs :: Ptr{Void}
   alloc :: Int
   length :: Int
   parent::FqFiniteField

   function fq(ctx::FqFiniteField)
      d = new()
      ccall((:fq_init2, :libflint), Void, 
            (Ptr{fq}, Ptr{FqFiniteField}), &d, &ctx)
      finalizer(d, _fq_clear_fn)
      d.parent = ctx
      return d
   end

   function fq(ctx::FqFiniteField, x::Int)
      d = new()
      ccall((:fq_init2, :libflint), Void, 
            (Ptr{fq}, Ptr{FqFiniteField}), &d, &ctx)
      finalizer(d, _fq_clear_fn)
      ccall((:fq_set_si, :libflint), Void, 
                (Ptr{fq}, Int, Ptr{FqFiniteField}), &d, x, &ctx)
      d.parent = ctx
      return d
   end

   function fq(ctx::FqFiniteField, x::fmpz)
      d = new()
      ccall((:fq_init2, :libflint), Void, 
            (Ptr{fq}, Ptr{FqFiniteField}), &d, &ctx)
      finalizer(d, _fq_clear_fn)
      ccall((:fq_set_fmpz, :libflint), Void, 
            (Ptr{fq}, Ptr{fmpz}, Ptr{FqFiniteField}), &d, &x, &ctx)
      d.parent = ctx
      return d
   end

   function fq(ctx::FqFiniteField, x::fq)
      d = new()
      ccall((:fq_init2, :libflint), Void, 
            (Ptr{fq}, Ptr{FqFiniteField}), &d, &ctx)
      finalizer(d, _fq_clear_fn)
      ccall((:fq_set, :libflint), Void, 
            (Ptr{fq}, Ptr{fq}, Ptr{FqFiniteField}), &d, &x, &ctx)
      d.parent = ctx
      return d
   end
end

function _fq_clear_fn(a::fq)
   ccall((:fq_clear, :libflint), Void, 
         (Ptr{fq}, Ptr{FqFiniteField}), &a, &a.parent)
end

###############################################################################
#
#   FlintPadicField / padic
#
###############################################################################


mutable struct FlintPadicField <: Field
   p::Int 
   pinv::Float64
   pow::Ptr{Void}
   minpre::Int
   maxpre::Int
   mode::Int
   prec_max::Int

   function FlintPadicField(p::fmpz, prec::Int)
      !isprime(p) && error("Prime base required in FlintPadicField")
      d = new()
      ccall((:padic_ctx_init, :libflint), Void, 
           (Ptr{FlintPadicField}, Ptr{fmpz}, Int, Int, Cint), 
                                     &d, &p, 0, 0, 1)
      finalizer(d, _padic_ctx_clear_fn)
      d.prec_max = prec
      return d
   end
end

const PadicBase = Dict{Tuple{fmpz, Int}, FlintPadicField}()

function _padic_ctx_clear_fn(a::FlintPadicField)
   ccall((:padic_ctx_clear, :libflint), Void, (Ptr{FlintPadicField},), &a)
end

mutable struct padic <: FieldElem
   u :: Int
   v :: Int
   N :: Int
   parent::FlintPadicField

   function padic(prec::Int)
      d = new()
      ccall((:padic_init2, :libflint), Void, (Ptr{padic}, Int), &d, prec)
      finalizer(d, _padic_clear_fn)
      return d
   end
end

function _padic_clear_fn(a::padic)
   ccall((:padic_clear, :libflint), Void, (Ptr{padic},), &a)
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
      if haskey(FmpzRelSeriesID, (prec, s))
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
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   prec::Int
   val::Int
   parent::FmpzRelSeriesRing

   function fmpz_rel_series()
      z = new()
      ccall((:fmpz_poly_init, :libflint), Void, 
            (Ptr{fmpz_rel_series},), &z)
      finalizer(z, _fmpz_rel_series_clear_fn)
      return z
   end
   
   function fmpz_rel_series(a::Array{fmpz, 1}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fmpz_poly_init2, :libflint), Void, 
            (Ptr{fmpz_rel_series}, Int), &z, len)
      for i = 1:len
         ccall((:fmpz_poly_set_coeff_fmpz, :libflint), Void, 
                     (Ptr{fmpz_rel_series}, Int, Ptr{fmpz}), &z, i - 1, &a[i])
      end
      z.prec = prec
      z.val = val
      finalizer(z, _fmpz_rel_series_clear_fn)
      return z
   end
   
   function fmpz_rel_series(a::fmpz_rel_series)
      z = new()
      ccall((:fmpz_poly_init, :libflint), Void, (Ptr{fmpz_rel_series},), &z)
      ccall((:fmpz_poly_set, :libflint), Void, 
            (Ptr{fmpz_rel_series}, Ptr{fmpz_rel_series}), &z, &a)
      finalizer(z, _fmpz_rel_series_clear_fn)
      return z
   end
end

function _fmpz_rel_series_clear_fn(a::fmpz_rel_series)
   ccall((:fmpz_poly_clear, :libflint), Void, (Ptr{fmpz_rel_series},), &a)
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
      if haskey(FmpzAbsSeriesID, (prec, s))
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
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   prec :: Int
   parent::FmpzAbsSeriesRing

   function fmpz_abs_series()
      z = new()
      ccall((:fmpz_poly_init, :libflint), Void, 
            (Ptr{fmpz_abs_series},), &z)
      finalizer(z, _fmpz_abs_series_clear_fn)
      return z
   end
   
   function fmpz_abs_series(a::Array{fmpz, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fmpz_poly_init2, :libflint), Void, 
            (Ptr{fmpz_abs_series}, Int), &z, len)
      for i = 1:len
         ccall((:fmpz_poly_set_coeff_fmpz, :libflint), Void, 
                     (Ptr{fmpz_abs_series}, Int, Ptr{fmpz}), &z, i - 1, &a[i])
      end
      z.prec = prec
      finalizer(z, _fmpz_abs_series_clear_fn)
      return z
   end
   
   function fmpz_abs_series(a::fmpz_abs_series)
      z = new()
      ccall((:fmpz_poly_init, :libflint), Void, (Ptr{fmpz_abs_series},), &z)
      ccall((:fmpz_poly_set, :libflint), Void, 
            (Ptr{fmpz_abs_series}, Ptr{fmpz_abs_series}), &z, &a)
      finalizer(z, _fmpz_abs_series_clear_fn)
      return z
   end
end

function _fmpz_abs_series_clear_fn(a::fmpz_abs_series)
   ccall((:fmpz_poly_clear, :libflint), Void, (Ptr{fmpz_abs_series},), &a)
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
      if haskey(FmpqRelSeriesID, (prec, s))
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
   coeffs::Ptr{Void}
   den::Int
   alloc::Int
   length::Int
   prec::Int
   val::Int
   parent::FmpqRelSeriesRing

   function fmpq_rel_series()
      z = new()
      ccall((:fmpq_poly_init, :libflint), Void, 
            (Ptr{fmpq_rel_series},), &z)
      finalizer(z, _fmpq_rel_series_clear_fn)
      return z
   end
   
   function fmpq_rel_series(a::Array{fmpq, 1}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fmpq_poly_init2, :libflint), Void, 
            (Ptr{fmpq_rel_series}, Int), &z, len)
      for i = 1:len
         ccall((:fmpq_poly_set_coeff_fmpq, :libflint), Void, 
                     (Ptr{fmpq_rel_series}, Int, Ptr{fmpq}), &z, i - 1, &a[i])
      end
      z.prec = prec
      z.val = val
      finalizer(z, _fmpq_rel_series_clear_fn)
      return z
   end
   
   function fmpq_rel_series(a::fmpq_rel_series)
      z = new()
      ccall((:fmpq_poly_init, :libflint), Void, (Ptr{fmpq_rel_series},), &z)
      ccall((:fmpq_poly_set, :libflint), Void, 
            (Ptr{fmpq_rel_series}, Ptr{fmpq_rel_series}), &z, &a)
      finalizer(z, _fmpq_rel_series_clear_fn)
      return z
   end
end

function _fmpq_rel_series_clear_fn(a::fmpq_rel_series)
   ccall((:fmpq_poly_clear, :libflint), Void, (Ptr{fmpq_rel_series},), &a)
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
      if haskey(FmpqAbsSeriesID, (prec, s))
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
   coeffs::Ptr{Void}
   den::Int
   alloc::Int
   length::Int
   prec :: Int
   parent::FmpqAbsSeriesRing

   function fmpq_abs_series()
      z = new()
      ccall((:fmpq_poly_init, :libflint), Void, 
            (Ptr{fmpq_abs_series},), &z)
      finalizer(z, _fmpq_abs_series_clear_fn)
      return z
   end
   
   function fmpq_abs_series(a::Array{fmpq, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fmpq_poly_init2, :libflint), Void, 
            (Ptr{fmpq_abs_series}, Int), &z, len)
      for i = 1:len
         ccall((:fmpq_poly_set_coeff_fmpq, :libflint), Void, 
                     (Ptr{fmpq_abs_series}, Int, Ptr{fmpq}), &z, i - 1, &a[i])
      end
      z.prec = prec
      finalizer(z, _fmpq_abs_series_clear_fn)
      return z
   end
   
   function fmpq_abs_series(a::fmpq_abs_series)
      z = new()
      ccall((:fmpq_poly_init, :libflint), Void, (Ptr{fmpq_abs_series},), &z)
      ccall((:fmpq_poly_set, :libflint), Void, 
            (Ptr{fmpq_abs_series}, Ptr{fmpq_abs_series}), &z, &a)
      finalizer(z, _fmpq_abs_series_clear_fn)
      return z
   end
end

function _fmpq_abs_series_clear_fn(a::fmpq_abs_series)
   ccall((:fmpq_poly_clear, :libflint), Void, (Ptr{fmpq_abs_series},), &a)
end

###############################################################################
#
#   FmpzModRelSeriesRing / fmpz_mod_rel_series
#
###############################################################################

mutable struct FmpzModRelSeriesRing <: SeriesRing{Generic.Res{fmpz}}
   base_ring::Generic.ResRing{fmpz}
   prec_max::Int
   S::Symbol

   function FmpzModRelSeriesRing(R::Ring, prec::Int, s::Symbol,
                                 cached::Bool = true)
      if haskey(FmpzModRelSeriesID, (R, prec, s))
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

const FmpzModRelSeriesID = Dict{Tuple{Generic.ResRing{fmpz}, Int, Symbol},
                                FmpzModRelSeriesRing}()

mutable struct fmpz_mod_rel_series <: RelSeriesElem{Generic.Res{fmpz}}
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   p::Int
   prec::Int
   val::Int
   parent::FmpzModRelSeriesRing

   function fmpz_mod_rel_series(p::fmpz)
      z = new()
      ccall((:fmpz_mod_poly_init, :libflint), Void, 
            (Ptr{fmpz_mod_rel_series}, Ptr{fmpz}), &z, &p)
      finalizer(z, _fmpz_mod_rel_series_clear_fn)
      return z
   end
   
   function fmpz_mod_rel_series(p::fmpz, a::Array{fmpz, 1}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, :libflint), Void, 
            (Ptr{fmpz_mod_rel_series}, Ptr{fmpz}, Int), &z, &p, len)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, :libflint), Void, 
                     (Ptr{fmpz_mod_rel_series}, Int, Ptr{fmpz}), &z, i - 1, &a[i])
      end
      z.prec = prec
      z.val = val
      finalizer(z, _fmpz_mod_rel_series_clear_fn)
      return z
   end
   
   function fmpz_mod_rel_series(p::fmpz, a::Array{Generic.Res{fmpz}, 1}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, :libflint), Void, 
            (Ptr{fmpz_mod_rel_series}, Ptr{fmpz}, Int), &z, &p, len)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, :libflint), Void, 
                     (Ptr{fmpz_mod_rel_series}, Int, Ptr{fmpz}), &z, i - 1, &data(a[i]))
      end
      z.prec = prec
      z.val = val
      finalizer(z, _fmpz_mod_rel_series_clear_fn)
      return z
   end
   
   function fmpz_mod_rel_series(a::fmpz_mod_rel_series)
      z = new()
      p = modulus(base_ring(parent(a)))
      ccall((:fmpz_mod_poly_init, :libflint), Void, 
            (Ptr{fmpz_mod_rel_series}, Ptr{fmpz}), &z, &p)
      ccall((:fmpz_mod_poly_set, :libflint), Void, 
            (Ptr{fmpz_mod_rel_series}, Ptr{fmpz_mod_rel_series}), &z, &a)
      finalizer(z, _fmpz_mod_rel_series_clear_fn)
      return z
   end
end

function _fmpz_mod_rel_series_clear_fn(a::fmpz_mod_rel_series)
   ccall((:fmpz_mod_poly_clear, :libflint), Void, (Ptr{fmpz_mod_rel_series},), &a)
end

###############################################################################
#
#   FmpzModAbsSeriesRing / fmpz_mod_abs_series
#
###############################################################################

mutable struct FmpzModAbsSeriesRing <: SeriesRing{Generic.Res{fmpz}}
   base_ring::Generic.ResRing{fmpz}
   prec_max::Int
   S::Symbol

   function FmpzModAbsSeriesRing(R::Ring, prec::Int, s::Symbol,
                                 cached::Bool = true)
      if haskey(FmpzModAbsSeriesID, (R, prec, s))
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

const FmpzModAbsSeriesID = Dict{Tuple{Generic.ResRing{fmpz}, Int, Symbol},
                                FmpzModAbsSeriesRing}()

mutable struct fmpz_mod_abs_series <: AbsSeriesElem{Generic.Res{fmpz}}
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   p::Int
   prec::Int
   parent::FmpzModAbsSeriesRing

   function fmpz_mod_abs_series(p::fmpz)
      z = new()
      ccall((:fmpz_mod_poly_init, :libflint), Void, 
            (Ptr{fmpz_mod_abs_series}, Ptr{fmpz}), &z, &p)
      finalizer(z, _fmpz_mod_abs_series_clear_fn)
      return z
   end
   
   function fmpz_mod_abs_series(p::fmpz, a::Array{fmpz, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, :libflint), Void, 
            (Ptr{fmpz_mod_abs_series}, Ptr{fmpz}, Int), &z, &p, len)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, :libflint), Void, 
                     (Ptr{fmpz_mod_abs_series}, Int, Ptr{fmpz}), &z, i - 1, &a[i])
      end
      z.prec = prec
      finalizer(z, _fmpz_mod_abs_series_clear_fn)
      return z
   end
   
   function fmpz_mod_abs_series(p::fmpz, a::Array{Generic.Res{fmpz}, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, :libflint), Void, 
            (Ptr{fmpz_mod_abs_series}, Ptr{fmpz}, Int), &z, &p, len)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, :libflint), Void, 
                     (Ptr{fmpz_mod_abs_series}, Int, Ptr{fmpz}), &z, i - 1, &data(a[i]))
      end
      z.prec = prec
      finalizer(z, _fmpz_mod_abs_series_clear_fn)
      return z
   end
   
   function fmpz_mod_abs_series(a::fmpz_mod_abs_series)
      z = new()
      p = modulus(base_ring(parent(a)))
      ccall((:fmpz_mod_poly_init, :libflint), Void, 
            (Ptr{fmpz_mod_abs_series}, Ptr{fmpz}), &z, &p)
      ccall((:fmpz_mod_poly_set, :libflint), Void, 
            (Ptr{fmpz_mod_abs_series}, Ptr{fmpz_mod_abs_series}), &z, &a)
      finalizer(z, _fmpz_mod_abs_series_clear_fn)
      return z
   end
end

function _fmpz_mod_abs_series_clear_fn(a::fmpz_mod_abs_series)
   ccall((:fmpz_mod_poly_clear, :libflint), Void, (Ptr{fmpz_mod_abs_series},), &a)
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
      if haskey(FqRelSeriesID, (R, prec, s))
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
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   prec::Int
   val::Int
   parent::FqRelSeriesRing

   function fq_rel_series(ctx::FqFiniteField)
      z = new()
      ccall((:fq_poly_init, :libflint), Void, 
            (Ptr{fq_rel_series}, Ptr{FqFiniteField}), &z, &ctx)
      finalizer(z, _fq_rel_series_clear_fn)
      return z
   end
   
   function fq_rel_series(ctx::FqFiniteField, a::Array{fq, 1}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fq_poly_init2, :libflint), Void, 
            (Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}), &z, len, &ctx)
      for i = 1:len
         ccall((:fq_poly_set_coeff, :libflint), Void, 
               (Ptr{fq_rel_series}, Int, Ptr{fq}, Ptr{FqFiniteField}),
                                               &z, i - 1, &a[i], &ctx)
      end
      z.prec = prec
      z.val = val
      finalizer(z, _fq_rel_series_clear_fn)
      return z
   end
   
   function fq_rel_series(ctx::FqFiniteField, a::fq_rel_series)
      z = new()
      ccall((:fq_poly_init, :libflint), Void, 
            (Ptr{fq_rel_series}, Ptr{FqFiniteField}), &z, &ctx)
      ccall((:fq_poly_set, :libflint), Void, 
            (Ptr{fq_rel_series}, Ptr{fq_rel_series}, Ptr{FqFiniteField}), &z, &a, &ctx)
      finalizer(z, _fq_rel_series_clear_fn)
      return z
   end
end

function _fq_rel_series_clear_fn(a::fq_rel_series)
   ctx = base_ring(a)
   ccall((:fq_poly_clear, :libflint), Void,
         (Ptr{fq_rel_series}, Ptr{FqFiniteField}), &a, &ctx)
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
      if haskey(FqAbsSeriesID, (R, prec, s))
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
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   prec::Int
   parent::FqAbsSeriesRing

   function fq_abs_series(ctx::FqFiniteField)
      z = new()
      ccall((:fq_poly_init, :libflint), Void, 
            (Ptr{fq_abs_series}, Ptr{FqFiniteField}), &z, &ctx)
      finalizer(z, _fq_abs_series_clear_fn)
      return z
   end
   
   function fq_abs_series(ctx::FqFiniteField, a::Array{fq, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fq_poly_init2, :libflint), Void, 
            (Ptr{fq_abs_series}, Int, Ptr{FqFiniteField}), &z, len, &ctx)
      for i = 1:len
         ccall((:fq_poly_set_coeff, :libflint), Void, 
               (Ptr{fq_abs_series}, Int, Ptr{fq}, Ptr{FqFiniteField}),
                                               &z, i - 1, &a[i], &ctx)
      end
      z.prec = prec
      finalizer(z, _fq_abs_series_clear_fn)
      return z
   end
   
   function fq_abs_series(ctx::FqFiniteField, a::fq_abs_series)
      z = new()
      ccall((:fq_poly_init, :libflint), Void, 
            (Ptr{fq_abs_series}, Ptr{FqFiniteField}), &z, &ctx)
      ccall((:fq_poly_set, :libflint), Void, 
            (Ptr{fq_abs_series}, Ptr{fq_abs_series}, Ptr{FqFiniteField}), &z, &a, &ctx)
      finalizer(z, _fq_abs_series_clear_fn)
      return z
   end
end

function _fq_abs_series_clear_fn(a::fq_abs_series)
   ctx = base_ring(a)
   ccall((:fq_poly_clear, :libflint), Void,
         (Ptr{fq_abs_series}, Ptr{FqFiniteField}), &a, &ctx)
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
      if haskey(FqNmodRelSeriesID, (R, prec, s))
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
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   prec::Int
   val::Int
   parent::FqNmodRelSeriesRing

   function fq_nmod_rel_series(ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_poly_init, :libflint), Void, 
            (Ptr{fq_nmod_rel_series}, Ptr{FqNmodFiniteField}), &z, &ctx)
      finalizer(z, _fq_nmod_rel_series_clear_fn)
      return z
   end
   
   function fq_nmod_rel_series(ctx::FqNmodFiniteField, a::Array{fq_nmod, 1}, len::Int, prec::Int, val::Int)
      z = new()
      ccall((:fq_nmod_poly_init2, :libflint), Void, 
            (Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), &z, len, &ctx)
      for i = 1:len
         ccall((:fq_nmod_poly_set_coeff, :libflint), Void, 
               (Ptr{fq_nmod_rel_series}, Int, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}),
                                               &z, i - 1, &a[i], &ctx)
      end
      z.prec = prec
      z.val = val
      finalizer(z, _fq_nmod_rel_series_clear_fn)
      return z
   end
   
   function fq_nmod_rel_series(ctx::FqNmodFiniteField, a::fq_nmod_rel_series)
      z = new()
      ccall((:fq_nmod_poly_init, :libflint), Void, 
            (Ptr{fq_nmod_rel_series}, Ptr{FqNmodFiniteField}), &z, &ctx)
      ccall((:fq_nmod_poly_set, :libflint), Void, 
            (Ptr{fq_nmod_rel_series}, Ptr{fq_nmod_rel_series}, Ptr{FqNmodFiniteField}), &z, &a, &ctx)
      finalizer(z, _fq_nmod_rel_series_clear_fn)
      return z
   end
end

function _fq_nmod_rel_series_clear_fn(a::fq_nmod_rel_series)
   ctx = base_ring(a)
   ccall((:fq_nmod_poly_clear, :libflint), Void,
         (Ptr{fq_nmod_rel_series}, Ptr{FqNmodFiniteField}), &a, &ctx)
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
      if haskey(FqNmodAbsSeriesID, (R, prec, s))
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
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   prec::Int
   parent::FqNmodAbsSeriesRing

   function fq_nmod_abs_series(ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_poly_init, :libflint), Void, 
            (Ptr{fq_nmod_abs_series}, Ptr{FqNmodFiniteField}), &z, &ctx)
      finalizer(z, _fq_nmod_abs_series_clear_fn)
      return z
   end
   
   function fq_nmod_abs_series(ctx::FqNmodFiniteField, a::Array{fq_nmod, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fq_nmod_poly_init2, :libflint), Void, 
            (Ptr{fq_nmod_abs_series}, Int, Ptr{FqNmodFiniteField}), &z, len, &ctx)
      for i = 1:len
         ccall((:fq_nmod_poly_set_coeff, :libflint), Void, 
               (Ptr{fq_nmod_abs_series}, Int, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}),
                                               &z, i - 1, &a[i], &ctx)
      end
      z.prec = prec
      finalizer(z, _fq_nmod_abs_series_clear_fn)
      return z
   end
   
   function fq_nmod_abs_series(ctx::FqNmodFiniteField, a::fq_nmod_abs_series)
      z = new()
      ccall((:fq_nmod_poly_init, :libflint), Void, 
            (Ptr{fq_nmod_abs_series}, Ptr{FqNmodFiniteField}), &z, &ctx)
      ccall((:fq_nmod_poly_set, :libflint), Void, 
            (Ptr{fq_nmod_abs_series}, Ptr{fq_nmod_abs_series}, Ptr{FqNmodFiniteField}), &z, &a, &ctx)
      finalizer(z, _fq_nmod_abs_series_clear_fn)
      return z
   end
end

function _fq_nmod_abs_series_clear_fn(a::fq_nmod_abs_series)
   ctx = base_ring(a)
   ccall((:fq_nmod_poly_clear, :libflint), Void,
         (Ptr{fq_nmod_abs_series}, Ptr{FqNmodFiniteField}), &a, &ctx)
end

###############################################################################
#
#   FmpqMatSpace / fmpq_mat
#
###############################################################################

# not really a mathematical ring
mutable struct FmpqMatSpace <: MatSpace{fmpq}
   rows::Int
   cols::Int
   base_ring::FlintRationalField

   function FmpqMatSpace(r::Int, c::Int, cached::Bool = true)
      if haskey(FmpqMatID, (r, c))
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
   entries::Ptr{Void}
   r::Int
   c::Int
   rows::Ptr{Void}
   base_ring::FlintRationalField

   # used by windows, not finalised!!
   function fmpq_mat()
      return new() 
   end

   function fmpq_mat(r::Int, c::Int)
      z = new()
      ccall((:fmpq_mat_init, :libflint), Void, 
            (Ptr{fmpq_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpq_mat_clear_fn)
      return z
   end

   function fmpq_mat(r::Int, c::Int, arr::Array{fmpq, 2})
      z = new()
      ccall((:fmpq_mat_init, :libflint), Void, 
            (Ptr{fmpq_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpq_mat_clear_fn)
      for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, :libflint), Ptr{fmpq},
                       (Ptr{fmpq_mat}, Int, Int), &z, i - 1, j - 1)
            ccall((:fmpq_set, :libflint), Void,
                  (Ptr{fmpq}, Ptr{fmpq}), el, &arr[i, j])
         end
      end
      return z
   end

   function fmpq_mat(r::Int, c::Int, arr::Array{fmpz, 2})
      z = new()
      ccall((:fmpq_mat_init, :libflint), Void, 
            (Ptr{fmpq_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpq_mat_clear_fn)
      b = fmpz(1)
      for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, :libflint), Ptr{fmpq},
                       (Ptr{fmpq_mat}, Int, Int), &z, i - 1, j - 1)
            ccall((:fmpq_set_fmpz_frac, :libflint), Void,
                  (Ptr{fmpq}, Ptr{fmpz}, Ptr{fmpz}), el, &arr[i, j], &b)
         end
      end
      return z
   end


   function fmpq_mat(r::Int, c::Int, arr::Array{fmpq, 1})
      z = new()
      ccall((:fmpq_mat_init, :libflint), Void, 
            (Ptr{fmpq_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpq_mat_clear_fn)
      for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, :libflint), Ptr{fmpq},
                       (Ptr{fmpq_mat}, Int, Int), &z, i - 1, j - 1)
            ccall((:fmpq_set, :libflint), Void,
                  (Ptr{fmpq}, Ptr{fmpq}), el, &arr[(i-1)*c+j])
         end
      end
      return z
   end

   function fmpq_mat(r::Int, c::Int, arr::Array{fmpz, 1})
      z = new()
      ccall((:fmpq_mat_init, :libflint), Void, 
            (Ptr{fmpq_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpq_mat_clear_fn)
      b = fmpz(1)
      for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, :libflint), Ptr{fmpq},
                       (Ptr{fmpq_mat}, Int, Int), &z, i - 1, j - 1)
            ccall((:fmpq_set_fmpz_frac, :libflint), Void,
                  (Ptr{fmpq}, Ptr{fmpz}, Ptr{fmpz}), el, &arr[(i-1)*c+j], &b)
         end
      end
      return z
   end


   function fmpq_mat(r::Int, c::Int, arr::Array{T, 2}) where {T <: Integer}
      z = new()
      ccall((:fmpq_mat_init, :libflint), Void, 
            (Ptr{fmpq_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpq_mat_clear_fn)
      for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, :libflint), Ptr{fmpq},
                       (Ptr{fmpq_mat}, Int, Int), &z, i - 1, j - 1)
            ccall((:fmpq_set, :libflint), Void,
                  (Ptr{fmpq}, Ptr{fmpq}), el, &fmpq(arr[i, j]))
         end
      end
      return z
   end

   function fmpq_mat(r::Int, c::Int, arr::Array{T, 1}) where {T <: Integer}
      z = new()
      ccall((:fmpq_mat_init, :libflint), Void, 
            (Ptr{fmpq_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpq_mat_clear_fn)
      for i = 1:r
         for j = 1:c
            el = ccall((:fmpq_mat_entry, :libflint), Ptr{fmpq},
                       (Ptr{fmpq_mat}, Int, Int), &z, i - 1, j - 1)
            ccall((:fmpq_set, :libflint), Void,
                  (Ptr{fmpq}, Ptr{fmpq}), el, &fmpq(arr[(i-1)*c+j]))
         end
      end
      return z
   end

   function fmpq_mat(r::Int, c::Int, d::fmpq)
      z = new()
      ccall((:fmpq_mat_init, :libflint), Void, 
            (Ptr{fmpq_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpq_mat_clear_fn)
      for i = 1:min(r, c)
         el = ccall((:fmpq_mat_entry, :libflint), Ptr{fmpq},
                    (Ptr{fmpq_mat}, Int, Int), &z, i - 1, i - 1)
         ccall((:fmpq_set, :libflint), Void,
               (Ptr{fmpq}, Ptr{fmpq}), el, &d)
      end
      return z
   end

   function fmpq_mat(m::fmpq_mat)
      z = new()
      ccall((:fmpq_mat_init_set, :libflint), Void, 
            (Ptr{fmpq_mat}, Ptr{fmpq_mat}), &z, &m)
      finalizer(z, _fmpq_mat_clear_fn)
      return z
   end
end

function _fmpq_mat_clear_fn(a::fmpq_mat)
   ccall((:fmpq_mat_clear, :libflint), Void, (Ptr{fmpq_mat},), &a)
end

###############################################################################
#
#   FmpzMatSpace / fmpz_mat
#
###############################################################################

# not really a mathematical ring
mutable struct FmpzMatSpace <: MatSpace{fmpz}
   rows::Int
   cols::Int
   base_ring::FlintIntegerRing

   function FmpzMatSpace(r::Int, c::Int, cached::Bool = true)
      if haskey(FmpzMatID, (r, c))
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
   entries::Ptr{Void}
   r::Int
   c::Int
   rows::Ptr{Void}
   base_ring::FlintIntegerRing

   # used by windows, not finalised!!
   function fmpz_mat()
      return new() 
   end

   function fmpz_mat(r::Int, c::Int)
      z = new()
      ccall((:fmpz_mat_init, :libflint), Void, 
            (Ptr{fmpz_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpz_mat_clear_fn)
      return z
   end

   function fmpz_mat(r::Int, c::Int, arr::Array{fmpz, 2})
      z = new()
      ccall((:fmpz_mat_init, :libflint), Void, 
            (Ptr{fmpz_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpz_mat_clear_fn)
      for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, :libflint), Ptr{fmpz},
                       (Ptr{fmpz_mat}, Int, Int), &z, i - 1, j - 1)
            ccall((:fmpz_set, :libflint), Void,
                  (Ptr{fmpz}, Ptr{fmpz}), el, &arr[i, j])
         end
      end
      return z
   end

   function fmpz_mat(r::Int, c::Int, arr::Array{fmpz, 1})
      z = new()
      ccall((:fmpz_mat_init, :libflint), Void, 
            (Ptr{fmpz_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpz_mat_clear_fn)
      for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, :libflint), Ptr{fmpz},
                       (Ptr{fmpz_mat}, Int, Int), &z, i - 1, j - 1)
            ccall((:fmpz_set, :libflint), Void,
                  (Ptr{fmpz}, Ptr{fmpz}), el, &arr[(i-1)*c+j])
         end
      end
      return z
   end

   function fmpz_mat(r::Int, c::Int, arr::Array{T, 2}) where {T <: Integer}
      z = new()
      ccall((:fmpz_mat_init, :libflint), Void, 
            (Ptr{fmpz_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpz_mat_clear_fn)
      for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, :libflint), Ptr{fmpz},
                       (Ptr{fmpz_mat}, Int, Int), &z, i - 1, j - 1)
            ccall((:fmpz_set, :libflint), Void,
                  (Ptr{fmpz}, Ptr{fmpz}), el, &fmpz(arr[i, j]))
         end
      end
      return z
   end

   function fmpz_mat(r::Int, c::Int, arr::Array{T,1}) where {T <: Integer}
      z = new()
      ccall((:fmpz_mat_init, :libflint), Void,
            (Ptr{fmpz_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpz_mat_clear_fn)
      for i = 1:r
         for j = 1:c
            el = ccall((:fmpz_mat_entry, :libflint), Ptr{fmpz},
                       (Ptr{fmpz_mat}, Int, Int), &z, i - 1, j - 1)
            ccall((:fmpz_set, :libflint), Void,
                  (Ptr{fmpz}, Ptr{fmpz}), el, &fmpz(arr[(i-1)*c+j]))
         end
      end
      return z
   end

   function fmpz_mat(r::Int, c::Int, d::fmpz)
      z = new()
      ccall((:fmpz_mat_init, :libflint), Void, 
            (Ptr{fmpz_mat}, Int, Int), &z, r, c)
      finalizer(z, _fmpz_mat_clear_fn)
      for i = 1:min(r, c)
         el = ccall((:fmpz_mat_entry, :libflint), Ptr{fmpz},
                    (Ptr{fmpz_mat}, Int, Int), &z, i - 1, i- 1)
         ccall((:fmpz_set, :libflint), Void,
               (Ptr{fmpz}, Ptr{fmpz}), el, &d)
      end
      return z
   end

   function fmpz_mat(m::fmpz_mat)
      z = new()
      ccall((:fmpz_mat_init_set, :libflint), Void, 
            (Ptr{fmpz_mat}, Ptr{fmpz_mat}), &z, &m)
      finalizer(z, _fmpz_mat_clear_fn)
      return z
   end
end

function _fmpz_mat_clear_fn(a::fmpz_mat)
   ccall((:fmpz_mat_clear, :libflint), Void, (Ptr{fmpz_mat},), &a)
end

###############################################################################
#
#   NmodMatSpace / nmod_mat
#
###############################################################################

mutable struct NmodMatSpace <: MatSpace{Generic.Res{fmpz}}
  base_ring::Generic.ResRing{fmpz}
  n::UInt
  rows::Int
  cols::Int

  function NmodMatSpace(R::Generic.ResRing{fmpz}, r::Int, c::Int,
                        cached::Bool = true)
    (r < 0 || c < 0) && throw(error_dim_negative)
    R.modulus > typemax(UInt) && 
      error("Modulus of ResidueRing must less then ", fmpz(typemax(UInt)))
    if haskey(NmodMatID, (R, r, c))
      return NmodMatID[R, r, c]
    else
      z = new(R, UInt(R.modulus), r, c)
      if cached
        NmodMatID[R, r, c] = z
      end
      return z
    end
  end
end

const NmodMatID = Dict{Tuple{Generic.ResRing{fmpz}, Int, Int}, NmodMatSpace}()

mutable struct nmod_mat <: MatElem{Generic.Res{fmpz}}
  entries::Ptr{Void}
  r::Int                  # Int
  c::Int                  # Int
  rows::Ptr{Void}
  n::UInt                # mp_limb_t / Culong
  ninv::UInt             # mp_limb_t / Culong
  norm::UInt             # mp_limb_t / Culong
  base_ring::Generic.ResRing{fmpz}

  function nmod_mat(r::Int, c::Int, n::UInt)
    z = new()
    ccall((:nmod_mat_init, :libflint), Void,
            (Ptr{nmod_mat}, Int, Int, UInt), &z, r, c, n)
    finalizer(z, _nmod_mat_clear_fn)
    return z
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::Array{UInt, 2}, transpose::Bool = false)
    _check_dim(r, c, arr, transpose)
    z = new()
    ccall((:nmod_mat_init, :libflint), Void,
            (Ptr{nmod_mat}, Int, Int, UInt), &z, r, c, n)
    finalizer(z, _nmod_mat_clear_fn)
    if transpose 
      se = set_entry_t!
      r,c = c,r
    else
      se = set_entry!
    end
    for i = 1:r
      for j = 1:c
        se(z, i, j, arr[i,j])
      end
    end
    return z
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::Array{UInt, 1}, transpose::Bool = false)
    _check_dim(r, c, arr, transpose)
    z = new()
    ccall((:nmod_mat_init, :libflint), Void,
            (Ptr{nmod_mat}, Int, Int, UInt), &z, r, c, n)
    finalizer(z, _nmod_mat_clear_fn)
    if transpose 
      se = set_entry_t!
      r,c = c,r
    else
      se = set_entry!
    end
    for i = 1:r
      for j = 1:c
        se(z, i, j, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::Array{fmpz, 2}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, :libflint), Void,
            (Ptr{nmod_mat}, Int, Int, UInt), &z, r, c, n)
    finalizer(z, _nmod_mat_clear_fn)
    if transpose 
      se = set_entry_t!
      r,c = c,r
    else
      se = set_entry!
    end
    for i = 1:r
      for j = 1:c
        se(z, i, j, arr[i,j])
      end
    end
    return z
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::Array{fmpz, 1}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, :libflint), Void,
            (Ptr{nmod_mat}, Int, Int, UInt), &z, r, c, n)
    finalizer(z, _nmod_mat_clear_fn)
    if transpose 
      se = set_entry_t!
      r,c = c,r
    else
      se = set_entry!
    end
    for i = 1:r
      for j = 1:c
        se(z, i, j, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::Array{T, 2}, transpose::Bool = false) where {T <: Integer}
    arr = map(fmpz, arr)
    return nmod_mat(r, c, n, arr, transpose)
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::Array{T, 1}, transpose::Bool = false) where {T <: Integer}
    arr = map(fmpz, arr)
    return nmod_mat(r, c, n, arr, transpose)
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::Array{Generic.Res{fmpz}, 2}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, :libflint), Void,
            (Ptr{nmod_mat}, Int, Int, UInt), &z, r, c, n)
    finalizer(z, _nmod_mat_clear_fn)
    if transpose 
      se = set_entry_t!
      r,c = c,r
    else
      se = set_entry!
    end
    for i = 1:r
      for j = 1:c
        se(z, i, j, arr[i,j])
      end
    end
    return z
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::Array{Generic.Res{fmpz}, 1}, transpose::Bool = false)
    z = new()
    ccall((:nmod_mat_init, :libflint), Void,
            (Ptr{nmod_mat}, Int, Int, UInt), &z, r, c, n)
    finalizer(z, _nmod_mat_clear_fn)
    if transpose 
      se = set_entry_t!
      r,c = c,r
    else
      se = set_entry!
    end
    for i = 1:r
      for j = 1:c
        se(z, i, j, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function nmod_mat(n::UInt, b::fmpz_mat)
    z = new()
    ccall((:nmod_mat_init, :libflint), Void,
            (Ptr{nmod_mat}, Int, Int, UInt), &z, b.r, b.c, n)
    finalizer(z, _nmod_mat_clear_fn)
    ccall((:fmpz_mat_get_nmod_mat, :libflint), Void,
            (Ptr{nmod_mat}, Ptr{fmpz_mat}), &z, &b)
    return z
  end

  function nmod_mat(n::Int, b::fmpz_mat)
    (n < 0) && error("Modulus must be positive")
    return nmod_mat(UInt(n), b)
  end

  function nmod_mat(n::fmpz, b::fmpz_mat)
    (n < 0) && error("Modulus must be positive")
    (n > typemax(UInt)) &&
          error("Exponent must be smaller than ", fmpz(typemax(UInt)))
    return nmod_mat(UInt(n), b) 
  end
end

function _nmod_mat_clear_fn(mat::nmod_mat)
  ccall((:nmod_mat_clear, :libflint), Void, (Ptr{nmod_mat}, ), &mat)
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
      if haskey(FqPolyID, (R, s))
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
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   parent::FqPolyRing

   function fq_poly()
      z = new()
      ccall((:fq_poly_init, :libflint), Void, (Ptr{fq_poly},), &z)   
      finalizer(z, _fq_poly_clear_fn)
      return z
   end

   function fq_poly(a::fq_poly)
      z = new()
      ctx = base_ring(parent(a))
      ccall((:fq_poly_init, :libflint), Void,
            (Ptr{fq_poly}, Ptr{FqFiniteField}), &z, &ctx)
      ccall((:fq_poly_set, :libflint), Void,
            (Ptr{fq_poly}, Ptr{fq_poly}, Ptr{FqFiniteField}),
            &z, &a, &ctx)
      finalizer(z, _fq_poly_clear_fn)
      return z
   end

   function fq_poly(a::fq)
      z = new()
      ctx = parent(a)
      ccall((:fq_poly_init, :libflint), Void,
            (Ptr{fq_poly}, Ptr{FqFiniteField}), &z, &ctx)
      ccall((:fq_poly_set_fq, :libflint), Void,
            (Ptr{fq_poly}, Ptr{fq}, Ptr{FqFiniteField}),
            &z, &a, &ctx)
      finalizer(z, _fq_poly_clear_fn)
      return z
   end

   function fq_poly(a::Array{fq, 1})
      z = new()
      ctx = parent(a[1])
      ccall((:fq_poly_init2, :libflint), Void,
            (Ptr{fq_poly}, Int, Ptr{FqFiniteField}),
            &z, length(a), &ctx)
      for i = 1:length(a)
         ccall((:fq_poly_set_coeff, :libflint), Void, 
               (Ptr{fq_poly}, Int, Ptr{fq}, Ptr{FqFiniteField}),
               &z, i - 1, &a[i], &ctx)
      end
      finalizer(z, _fq_poly_clear_fn)
      return z
   end

   function fq_poly(a::Array{fmpz, 1}, ctx::FqFiniteField)
      z = new()
      temp = ctx()
      ccall((:fq_poly_init2, :libflint), Void,
            (Ptr{fq_poly}, Int, Ptr{FqFiniteField}),
            &z, length(a), &ctx)
      for i = 1:length(a)
         temp = ctx(a[i])
         ccall((:fq_poly_set_coeff, :libflint), Void,
               (Ptr{fq_poly}, Int, Ptr{fq}, Ptr{FqFiniteField}),
               &z, i - 1, &temp, &ctx)
      end
      finalizer(z, _fq_poly_clear_fn)
      return z
   end

   function fq_poly(a::fmpz_poly, ctx::FqFiniteField)
      z = new()
      ccall((:fq_poly_init2, :libflint), Void,
            (Ptr{fq_poly}, Int, Ptr{FqFiniteField}),
            &z, length(a), &ctx)
      for i = 1:length(a)
         temp = ctx(coeff(a, i-1))
         ccall((:fq_poly_set_coeff, :libflint), Void,
               (Ptr{fq_poly}, Int, Ptr{fq}, Ptr{FqFiniteField}),
               &z, i - 1, &temp, &ctx)
      end
      finalizer(z, _fq_poly_clear_fn)
      return z
   end
end

function _fq_poly_clear_fn(a::fq_poly)
   ccall((:fq_poly_clear, :libflint), Void, (Ptr{fq_poly},), &a)
end

mutable struct fq_poly_factor
  poly::Ptr{fq_poly}
  exp::Ptr{Int} 
  num::Int
  alloc::Int
  base_field::FqFiniteField
    
  function fq_poly_factor(ctx::FqFiniteField)
    z = new()
    ccall((:fq_poly_factor_init, :libflint), Void,
         (Ptr{fq_poly_factor}, Ptr{FqFiniteField}), &z, &ctx)
    z.base_field = ctx
    finalizer(z, _fq_poly_factor_clear_fn)
    return z
  end
end

function _fq_poly_factor_clear_fn(a::fq_poly_factor)
   ccall((:fq_poly_factor_clear, :libflint), Void,
         (Ptr{fq_poly_factor}, Ptr{FqFiniteField}),
         &a, &(a.base_field))
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
      if haskey(FqNmodPolyID, (R, s))
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
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   parent::FqNmodPolyRing

   function fq_nmod_poly()
      z = new()
      ccall((:fq_nmod_poly_init, :libflint), Void, (Ptr{fq_nmod_poly},), &z)   
      finalizer(z, _fq_nmod_poly_clear_fn)
      return z
   end

   function fq_nmod_poly(a::fq_nmod_poly)
      z = new()
      ctx = base_ring(parent(a))
      ccall((:fq_nmod_poly_init, :libflint), Void,
            (Ptr{fq_nmod_poly}, Ptr{FqNmodFiniteField}), &z, &ctx)
      ccall((:fq_nmod_poly_set, :libflint), Void,
            (Ptr{fq_nmod_poly}, Ptr{fq_nmod_poly}, Ptr{FqNmodFiniteField}),
            &z, &a, &ctx)
      finalizer(z, _fq_nmod_poly_clear_fn)
      return z
   end

   function fq_nmod_poly(a::fq_nmod)
      z = new()
      ctx = parent(a)
      ccall((:fq_nmod_poly_init, :libflint), Void,
            (Ptr{fq_nmod_poly}, Ptr{FqNmodFiniteField}), &z, &ctx)
      ccall((:fq_nmod_poly_set_fq_nmod, :libflint), Void,
            (Ptr{fq_nmod_poly}, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}),
            &z, &a, &ctx)
      finalizer(z, _fq_nmod_poly_clear_fn)
      return z
   end

   function fq_nmod_poly(a::Array{fq_nmod, 1})
      z = new()
      ctx = parent(a[1])
      ccall((:fq_nmod_poly_init2, :libflint), Void,
            (Ptr{fq_nmod_poly}, Int, Ptr{FqNmodFiniteField}),
            &z, length(a), &ctx)
      for i = 1:length(a)
         ccall((:fq_nmod_poly_set_coeff, :libflint), Void, 
               (Ptr{fq_nmod_poly}, Int, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}),
               &z, i - 1, &a[i], &ctx)
      end
      finalizer(z, _fq_nmod_poly_clear_fn)
      return z
   end

   function fq_nmod_poly(a::Array{fmpz, 1}, ctx::FqNmodFiniteField)
      z = new()
      temp = ctx()
      ccall((:fq_nmod_poly_init2, :libflint), Void,
            (Ptr{fq_nmod_poly}, Int, Ptr{FqNmodFiniteField}),
            &z, length(a), &ctx)
      for i = 1:length(a)
         temp = ctx(a[i])
         ccall((:fq_nmod_poly_set_coeff, :libflint), Void,
               (Ptr{fq_nmod_poly}, Int, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}),
               &z, i - 1, &temp, &ctx)
      end
      finalizer(z, _fq_nmod_poly_clear_fn)
      return z
   end

   function fq_nmod_poly(a::fmpz_poly, ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_poly_init2, :libflint), Void,
            (Ptr{fq_nmod_poly}, Int, Ptr{FqNmodFiniteField}),
            &z, length(a), &ctx)
      for i = 1:length(a)
         temp = ctx(coeff(a,i-1))
         ccall((:fq_nmod_poly_set_coeff, :libflint), Void,
               (Ptr{fq_nmod_poly}, Int, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}),
               &z, i - 1, &temp, &ctx)
      end
      finalizer(z, _fq_nmod_poly_clear_fn)
      return z
   end
end

function _fq_nmod_poly_clear_fn(a::fq_nmod_poly)
   ccall((:fq_nmod_poly_clear, :libflint), Void, (Ptr{fq_nmod_poly},), &a)
end

mutable struct fq_nmod_poly_factor
  poly::Ptr{fq_nmod_poly}
  exp::Ptr{Int} 
  num::Int
  alloc::Int
  base_field::FqNmodFiniteField
    
  function fq_nmod_poly_factor(ctx::FqNmodFiniteField)
    z = new()
    ccall((:fq_nmod_poly_factor_init, :libflint), Void,
         (Ptr{fq_nmod_poly_factor}, Ptr{FqNmodFiniteField}), &z, &ctx)
    z.base_field = ctx
    finalizer(z, _fq_nmod_poly_factor_clear_fn)
    return z
  end
end

function _fq_nmod_poly_factor_clear_fn(a::fq_nmod_poly_factor)
   ccall((:fq_nmod_poly_factor_clear, :libflint), Void,
         (Ptr{fq_nmod_poly_factor}, Ptr{FqNmodFiniteField}),
         &a, &(a.base_field))
end
