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

type FlintIntegerRing <: Ring
end

const FlintZZ = FlintIntegerRing()

type fmpz <: RingElem
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
        ccall((:fmpz_set_d, :libflint), Void, (Ptr{fmpz}, Cdouble), &z, x)
        finalizer(z, _fmpz_clear_fn)
        return z
    end

    fmpz(x::fmpz) = x
end

function _fmpz_clear_fn(a::fmpz)
   ccall((:fmpz_clear, :libflint), Void, (Ptr{fmpz},), &a)
end

###############################################################################
#
#   FlintRationalField / fmpq
#
###############################################################################

type FlintRationalField <: FracField{fmpz}
end

const FlintQQ = FlintRationalField()

type fmpq <: FracElem{fmpz}
   num::Int
   den::Int

   function fmpq()
      z = new()
      ccall((:fmpq_init, :libflint), Void, (Ptr{fmpq},), &z)
      finalizer(z, _fmpq_clear_fn)
      return z
   end

   function fmpq(a::fmpz, b::fmpz)
      b == 0 && throw(DivideError())
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

const FmpzPolyID = ObjectIdDict()

type FmpzPolyRing <: PolyRing{fmpz}
   base_ring::FlintIntegerRing
   S::Symbol

   function FmpzPolyRing(s::Symbol)
      if haskey(FmpzPolyID, s)
         return FmpzPolyID[s]::FmpzPolyRing
      else
         z = new(FlintZZ, s)
         FmpzPolyID[s] = z
         return z
      end
   end
end

type fmpz_poly <: PolyElem{fmpz}
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

###############################################################################
#
#   FmpqPolyRing / fmpq_poly
#
###############################################################################

const FmpqPolyID = ObjectIdDict()

type FmpqPolyRing <: PolyRing{fmpq}
   base_ring::FlintRationalField
   S::Symbol

   function FmpqPolyRing(R::FlintRationalField, s::Symbol)
      if haskey(FmpqPolyID, s)
         return FmpqPolyID[s]::FmpqPolyRing
      else
         z = new(R, s)
         FmpqPolyID[s] = z
         return z
      end
   end
end

type fmpq_poly <: PolyElem{fmpq}
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

const NmodPolyRingID = ObjectIdDict()

type NmodPolyRing <: PolyRing{GenRes{fmpz}}
  base_ring::GenResRing{fmpz}
  S::Symbol
  n::UInt

  function NmodPolyRing(R::GenResRing{fmpz}, s::Symbol, cached=true)
    m = UInt(modulus(R))
    if haskey(NmodPolyRingID, (m, s))
       return NmodPolyRingID[m, s]::NmodPolyRing
    else
       z = new(R, s, m)
       if cached
          NmodPolyRingID[m, s] = z
       end
       return z
    end
  end
end

type nmod_poly <: PolyElem{GenRes{fmpz}}
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

   function nmod_poly(n::UInt, arr::Array{GenRes{fmpz}, 1})
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

type nmod_poly_factor
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

const FmpzModPolyRingID = ObjectIdDict()

type FmpzModPolyRing <: PolyRing{GenRes{fmpz}}
  base_ring::GenResRing{fmpz}
  S::Symbol
  n::fmpz

  function FmpzModPolyRing(R::GenResRing{fmpz}, s::Symbol, cached=true)
    m = modulus(R)
    if haskey(FmpzModPolyRingID, (m, s))
       return FmpzModPolyRingID[m, s]::FmpzModPolyRing
    else
       z = new(R, s, m)
       if cached
          FmpzModPolyRingID[m ,s] = z
       end
       return z
    end
  end
end

type fmpz_mod_poly <: PolyElem{GenRes{fmpz}}
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
      length(arr) == 0  && error("Array must have length > 0")
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

   function fmpz_mod_poly(n::fmpz, arr::Array{GenRes{fmpz}, 1})
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

type fmpz_mod_poly_factor
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
#   FqNmodFiniteField / fq_nmod
#
###############################################################################

const FqNmodFiniteFieldID = Dict{Tuple{fmpz, Int, Symbol}, Field}()
const FqNmodFiniteFieldIDPol = Dict{Tuple{NmodPolyRing, nmod_poly, Symbol}, Field}()

type FqNmodFiniteField <: FinField
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

   function FqNmodFiniteField(c::fmpz, deg::Int, s::Symbol)
      if haskey(FqNmodFiniteFieldID, (c, deg, s))
         return FqNmodFiniteFieldID[c, deg, s]::FqNmodFiniteField
      else
         d = new()
         ccall((:fq_nmod_ctx_init, :libflint), Void, 
               (Ptr{FqNmodFiniteField}, Ptr{fmpz}, Int, Ptr{UInt8}), 
			    &d, &c, deg, bytestring(string(s)))
         FqNmodFiniteFieldID[c, deg, s] = d
         finalizer(d, _FqNmodFiniteField_clear_fn)
         return d
      end
   end

   function FqNmodFiniteField(f::nmod_poly, s::Symbol)
      if haskey(FqNmodFiniteFieldIDPol, (parent(f), f, s))
         return FqNmodFiniteFieldIDPol[parent(f), f, s]::FqNmodFiniteField
      else
         z = new()
         ccall((:fq_nmod_ctx_init_modulus, :libflint), Void, 
            (Ptr{FqNmodFiniteField}, Ptr{nmod_poly}, Ptr{UInt8}), 
	      &z, &f, bytestring(string(s)))
         FqNmodFiniteFieldIDPol[parent(f), f, s] = z
         finalizer(z, _FqNmodFiniteField_clear_fn)
         return z
      end
   end
end

function _FqNmodFiniteField_clear_fn(a :: FqNmodFiniteField)
   ccall((:fq_nmod_ctx_clear, :libflint), Void, (Ptr{FqNmodFiniteField},), &a)
end

type fq_nmod <: FinFieldElem
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

const FqFiniteFieldID = Dict{Tuple{fmpz, Int, Symbol}, Field}()

const FqFiniteFieldIDPol = Dict{Tuple{fmpz_mod_poly, Symbol}, Field}()

type FqFiniteField <: FinField
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

   function FqFiniteField(char::fmpz, deg::Int, s::Symbol)
      if haskey(FqFiniteFieldID, (char, deg, s))
         return FqFiniteFieldID[char, deg, s]::FqFiniteField
      else
         d = new()
         FqFiniteFieldID[char, deg, s] = d
         finalizer(d, _FqFiniteField_clear_fn)
         ccall((:fq_ctx_init, :libflint), Void,
               (Ptr{FqFiniteField}, Ptr{fmpz}, Int, Ptr{UInt8}),
                  &d, &char, deg, bytestring(string(s)))
         return d
      end
   end
   
   function FqFiniteField(f::fmpz_mod_poly, s::Symbol)
      if haskey(FqFiniteFieldIDPol, (f, s))
         return FqFiniteFieldIDPol[f, s]::FqFiniteField
      else
         z = new()
         ccall((:fq_ctx_init_modulus, :libflint), Void,
               (Ptr{FqFiniteField}, Ptr{fmpz_mod_poly}, Ptr{UInt8}),
                  &z, &f, bytestring(string(s)))
         FqFiniteFieldIDPol[f, s] = z
         finalizer(z, _FqFiniteField_clear_fn)
         return z
      end
   end
end

function _FqFiniteField_clear_fn(a :: FqFiniteField)
   ccall((:fq_ctx_clear, :libflint), Void, (Ptr{FqFiniteField},), &a)
end

type fq <: FinFieldElem
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

const PadicBase = ObjectIdDict()

type FlintPadicField <: Field
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

function _padic_ctx_clear_fn(a::FlintPadicField)
   ccall((:padic_ctx_clear, :libflint), Void, (Ptr{FlintPadicField},), &a)
end

type padic <: FieldElem
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

const FmpzSeriesID = ObjectIdDict()

type FmpzRelSeriesRing <: SeriesRing{fmpz}
   base_ring::FlintIntegerRing
   prec_max::Int
   S::Symbol

   function FmpzRelSeriesRing(prec::Int, s::Symbol)
      if haskey(FmpzSeriesID, (prec, s))
         FmpzSeriesID[prec, s]::FmpzRelSeriesRing
      else
         z = new(FlintZZ, prec, s)
         FmpzSeriesID[prec, s] = z
         return z
      end
   end
end

type fmpz_rel_series <: SeriesElem{fmpz}
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   prec :: Int
   parent::FmpzRelSeriesRing

   function fmpz_rel_series()
      z = new()
      ccall((:fmpz_poly_init, :libflint), Void, 
            (Ptr{fmpz_rel_series},), &z)
      finalizer(z, _fmpz_rel_series_clear_fn)
      return z
   end
   
   function fmpz_rel_series(a::Array{fmpz, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fmpz_poly_init2, :libflint), Void, 
            (Ptr{fmpz_rel_series}, Int), &z, len)
      for i = 1:len
         ccall((:fmpz_poly_set_coeff_fmpz, :libflint), Void, 
                     (Ptr{fmpz_rel_series}, Int, Ptr{fmpz}), &z, i - 1, &a[i])
      end
      z.prec = prec
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
#   FmpqRelSeriesRing / fmpq_rel_series
#
###############################################################################

const FmpqSeriesID = ObjectIdDict()

type FmpqRelSeriesRing <: SeriesRing{fmpq}
   base_ring::FlintRationalField
   prec_max::Int
   S::Symbol

   function FmpqRelSeriesRing(prec::Int, s::Symbol)
      if haskey(FmpqSeriesID, (prec, s))
         return FmpqSeriesID[prec, s]::FmpqRelSeriesRing
      else
         z = new(FlintQQ, prec, s)
         FmpqSeriesID[prec, s] = z
         return z
      end
   end
end

type fmpq_rel_series <: SeriesElem{fmpq}
   coeffs::Ptr{Void}
   den::Int
   alloc::Int
   length::Int
   prec :: Int
   parent::FmpqRelSeriesRing

   function fmpq_rel_series()
      z = new()
      ccall((:fmpq_poly_init, :libflint), Void, 
            (Ptr{fmpq_rel_series},), &z)
      finalizer(z, _fmpq_rel_series_clear_fn)
      return z
   end
   
   function fmpq_rel_series(a::Array{fmpq, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fmpq_poly_init2, :libflint), Void, 
            (Ptr{fmpq_rel_series}, Int), &z, len)
      for i = 1:len
         ccall((:fmpq_poly_set_coeff_fmpq, :libflint), Void, 
                     (Ptr{fmpq_rel_series}, Int, Ptr{fmpq}), &z, i - 1, &a[i])
      end
      z.prec = prec
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
#   FmpzModRelSeriesRing / fmpz_mod_rel_series
#
###############################################################################

const FmpzModSeriesID = ObjectIdDict()

type FmpzModRelSeriesRing <: SeriesRing{GenRes{fmpz}}
   base_ring::GenResRing{fmpz}
   prec_max::Int
   S::Symbol

   function FmpzModRelSeriesRing(R::Ring, prec::Int, s::Symbol)
      if haskey(FmpzModSeriesID, (R, prec, s))
         return FmpzModSeriesID[R, prec, s]::FmpzModRelSeriesRing
      else
         FmpzModSeriesID[R, prec, s] = new(R, prec, s)
         return FmpzModSeriesID[R, prec, s]
      end
   end
end

type fmpz_mod_rel_series <: SeriesElem{GenRes{fmpz}}
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   p::Int
   prec :: Int
   parent::FmpzModRelSeriesRing

   function fmpz_mod_rel_series(p::fmpz)
      z = new()
      ccall((:fmpz_mod_poly_init, :libflint), Void, 
            (Ptr{fmpz_mod_rel_series}, Ptr{fmpz}), &z, &p)
      finalizer(z, _fmpz_mod_rel_series_clear_fn)
      return z
   end
   
   function fmpz_mod_rel_series(p::fmpz, a::Array{fmpz, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, :libflint), Void, 
            (Ptr{fmpz_mod_rel_series}, Ptr{fmpz}, Int), &z, &p, len)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, :libflint), Void, 
                     (Ptr{fmpz_mod_rel_series}, Int, Ptr{fmpz}), &z, i - 1, &a[i])
      end
      z.prec = prec
      finalizer(z, _fmpz_mod_rel_series_clear_fn)
      return z
   end
   
   function fmpz_mod_rel_series(p::fmpz, a::Array{GenRes{fmpz}, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fmpz_mod_poly_init2, :libflint), Void, 
            (Ptr{fmpz_mod_rel_series}, Ptr{fmpz}, Int), &z, &p, len)
      for i = 1:len
         ccall((:fmpz_mod_poly_set_coeff_fmpz, :libflint), Void, 
                     (Ptr{fmpz_mod_rel_series}, Int, Ptr{fmpz}), &z, i - 1, &data(a[i]))
      end
      z.prec = prec
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
#   FqRelSeriesRing / fq_rel_series
#
###############################################################################

const FqSeriesID = ObjectIdDict()

type FqRelSeriesRing <: SeriesRing{fq}
   base_ring::FqFiniteField
   prec_max::Int
   S::Symbol

   function FqRelSeriesRing(R::FqFiniteField, prec::Int, s::Symbol)
      if haskey(FqSeriesID, (R, prec, s))
         return FqSeriesID[R, prec, s]::FqRelSeriesRing
      else
         z = new(R, prec, s)
         FqSeriesID[R, prec, s] = z
         return z
      end
   end
end

type fq_rel_series <: SeriesElem{fq}
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   prec :: Int
   parent::FqRelSeriesRing

   function fq_rel_series(ctx::FqFiniteField)
      z = new()
      ccall((:fq_poly_init, :libflint), Void, 
            (Ptr{fq_rel_series}, Ptr{FqFiniteField}), &z, &ctx)
      finalizer(z, _fq_rel_series_clear_fn)
      return z
   end
   
   function fq_rel_series(ctx::FqFiniteField, a::Array{fq, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fq_poly_init2, :libflint), Void, 
            (Ptr{fq_rel_series}, Int, Ptr{FqFiniteField}), &z, len, &ctx)
      for i = 1:len
         ccall((:fq_poly_set_coeff, :libflint), Void, 
               (Ptr{fq_rel_series}, Int, Ptr{fq}, Ptr{FqFiniteField}),
                                               &z, i - 1, &a[i], &ctx)
      end
      z.prec = prec
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
#   FqNmodRelSeriesRing / fq_nmod_rel_series
#
###############################################################################

const FqNmodSeriesID = ObjectIdDict()

type FqNmodRelSeriesRing <: SeriesRing{fq_nmod}
   base_ring::FqNmodFiniteField
   prec_max::Int
   S::Symbol

   function FqNmodRelSeriesRing(R::FqNmodFiniteField, prec::Int, s::Symbol)
      if haskey(FqNmodSeriesID, (R, prec, s))
         return FqNmodSeriesID[R, prec, s]::FqNmodRelSeriesRing
      else
         z = new(R, prec, s)
         FqNmodSeriesID[R, prec, s] = z
         return z
      end
   end
end

type fq_nmod_rel_series <: SeriesElem{fq_nmod}
   coeffs::Ptr{Void}
   alloc::Int
   length::Int
   prec :: Int
   parent::FqNmodRelSeriesRing

   function fq_nmod_rel_series(ctx::FqNmodFiniteField)
      z = new()
      ccall((:fq_nmod_poly_init, :libflint), Void, 
            (Ptr{fq_nmod_rel_series}, Ptr{FqNmodFiniteField}), &z, &ctx)
      finalizer(z, _fq_nmod_rel_series_clear_fn)
      return z
   end
   
   function fq_nmod_rel_series(ctx::FqNmodFiniteField, a::Array{fq_nmod, 1}, len::Int, prec::Int)
      z = new()
      ccall((:fq_nmod_poly_init2, :libflint), Void, 
            (Ptr{fq_nmod_rel_series}, Int, Ptr{FqNmodFiniteField}), &z, len, &ctx)
      for i = 1:len
         ccall((:fq_nmod_poly_set_coeff, :libflint), Void, 
               (Ptr{fq_nmod_rel_series}, Int, Ptr{fq_nmod}, Ptr{FqNmodFiniteField}),
                                               &z, i - 1, &a[i], &ctx)
      end
      z.prec = prec
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
#   FmpqMatSpace / fmpq_mat
#
###############################################################################

const FmpqMatID = ObjectIdDict()

# not really a mathematical ring
type FmpqMatSpace <: MatSpace{fmpq}
   rows::Int
   cols::Int
   base_ring::FlintRationalField

   function FmpqMatSpace(r::Int, c::Int)
      if haskey(FmpqMatID, (r, c))
         return FmpqMatID[r, c]::FmpqMatSpace
      else
         z = new(r, c, FlintQQ)
         FmpqMatID[r, c] = z
         return z
      end
   end
end

type fmpq_mat <: MatElem{fmpq}
   entries::Ptr{Void}
   r::Int
   c::Int
   rows::Ptr{Void}
   parent::FmpqMatSpace

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

   function fmpq_mat{T <: Integer}(r::Int, c::Int, arr::Array{T, 2})
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

const FmpzMatID = ObjectIdDict()

# not really a mathematical ring
type FmpzMatSpace <: MatSpace{fmpz}
   rows::Int
   cols::Int
   base_ring::FlintIntegerRing

   function FmpzMatSpace(r::Int, c::Int)
      if haskey(FmpzMatID, (r, c))
         return FmpzMatID[r, c]::FmpzMatSpace
      else
         z = new(r, c, FlintZZ)
         FmpzMatID[r, c] = z
         return z
      end
   end
end

type fmpz_mat <: MatElem{fmpz}
   entries::Ptr{Void}
   r::Int
   c::Int
   rows::Ptr{Void}
   parent::FmpzMatSpace

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

   function fmpz_mat{T <: Integer}(r::Int, c::Int, arr::Array{T, 2})
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

const NmodMatID = ObjectIdDict()

type NmodMatSpace <: MatSpace{GenRes{fmpz}}
  base_ring::GenResRing{fmpz}
  n::UInt
  rows::Int
  cols::Int

  function NmodMatSpace(R::GenResRing{fmpz}, r::Int, c::Int)
    (r < 0 || c < 0) && throw(error_dim_negative)
    R.modulus > typemax(UInt) && 
      error("Modulus of ResidueRing must less then ", fmpz(typemax(UInt)))
    if haskey(NmodMatID, (R, r, c))
      return NmodMatID[R, r, c]::NmodMatSpace
    else
      z = new(R, UInt(R.modulus), r, c)
      NmodMatID[R, r, c] = z
      return z
    end
  end
end

function _check_dim{T}(r::Int, c::Int, arr::Array{T, 2}, transpose::Bool)
  (r < 0 || c < 0) && throw(error_dim_negative)
  (size(arr) != (transpose ? (c,r) : (r,c))) && error("Array of wrong dimension")
end

type nmod_mat <: MatElem{GenRes{fmpz}}
  entries::Ptr{Void}
  r::Int                  # Int
  c::Int                  # Int
  rows::Ptr{Void}
  n::UInt                # mp_limb_t / Culong
  ninv::UInt             # mp_limb_t / Culong
  norm::UInt             # mp_limb_t / Culong
  parent::NmodMatSpace

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

  function nmod_mat{T <: Integer}(r::Int, c::Int, n::UInt, arr::Array{T, 2}, transpose::Bool = false)
    arr = map(fmpz, arr)
    return nmod_mat(r, c, n, arr, transpose)
  end

  function nmod_mat(r::Int, c::Int, n::UInt, arr::Array{GenRes{fmpz}, 2}, transpose::Bool = false)
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

  function nmod_mat(r::Int, c::Int, n::UInt, arr::Array{GenRes{fmpz}, 1}, transpose::Bool = false)
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

const FqPolyID = ObjectIdDict()

type FqPolyRing <: PolyRing{fq}
   base_ring::FqFiniteField
   S::Symbol

   function FqPolyRing(R::FqFiniteField, s::Symbol)
      if haskey(FqPolyID, (R, s))
         return FqPolyID[(R,s)]::FqPolyRing
      else
         z = new(R,s)
         FqPolyID[(R,s)] = z
         return z
      end
   end
end

type fq_poly <: PolyElem{fq}
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

type fq_poly_factor
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

const FqNmodPolyID = ObjectIdDict()

type FqNmodPolyRing <: PolyRing{fq_nmod}
   base_ring::FqNmodFiniteField
   S::Symbol

   function FqNmodPolyRing(R::FqNmodFiniteField, s::Symbol)
      if haskey(FqNmodPolyID, (R, s))
         return FqNmodPolyID[(R,s)]::FqNmodPolyRing
      else
         z = new(R,s)
         FqNmodPolyID[(R,s)] = z
         return z
      end
   end
end

type fq_nmod_poly <: PolyElem{fq_nmod}
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

type fq_nmod_poly_factor
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

###############################################################################
#
#   FlintPermGroup / perm
#
###############################################################################

const FlintPermID = ObjectIdDict()

type FlintPermGroup <: Group
   n::Int

   function FlintPermGroup(n::Int)
      if haskey(FlintPermID, n)
         return FlintPermID[n]::FlintPermGroup
      else
         z = new(n)
         FlintPermID[n] = z
         return z
      end
   end
end

type perm <: GroupElem
   d::Array{Int, 1}
   parent::FlintPermGroup

   function perm(n::Int)
      p = new(Array(Int, n))
      ccall((:_perm_set_one, :libflint), Void,
            (Ref{Int}, Int), p.d, length(p.d))
      return p
   end

   function perm(a::Array{Int, 1})
      n = length(a)
      d = Array(Int, n)
      for i = 1:n
         d[i] = a[i] - 1
      end
      return new(d)
   end
end
