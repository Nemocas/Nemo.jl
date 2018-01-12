###############################################################################
#
#   ArbTypes.jl : Parent and object types for Arb
#
#   Copyright (C) 2015 Tommy Hofmann
#   Copyright (C) 2015 Fredrik Johansson
#
###############################################################################

export ArbField, arb
export AcbField, acb
export ArbPolyRing, arb_poly
export AcbPolyRing, acb_poly
export ArbMatSpace, arb_mat
export AcbMatSpace, acb_mat

arb_check_prec(p::Int) = (p >= 2 && p < (typemax(Int) >> 4)) || throw(ArgumentError("invalid precision"))

# Rounding modes

const ARB_RND_DOWN = Cint(0)   # towards zero
const ARB_RND_UP = Cint(1)     # away from zero
const ARB_RND_FLOOR = Cint(2)  # towards -infinity
const ARB_RND_CEIL = Cint(3)   # towards +infinity
const ARB_RND_NEAR = Cint(4)   # to nearest

################################################################################
#
#  Types and memory management for ArbField
#
################################################################################

mutable struct ArbField <: Field
  prec::Int

  function ArbField(p::Int = 256, cached::Bool = true)
    arb_check_prec(p)
    if haskey(ArbFieldID, p)
      return ArbFieldID[p]
    else
      z = new(p)
      if cached
        ArbFieldID[p] = z
      end
      return z
    end
  end
end

const ArbFieldID = Dict{Int, ArbField}()

prec(x::ArbField) = x.prec

# these may be used for shallow operations
mutable struct arf_struct
  exp::Int # fmpz
  size::UInt # mp_size_t
  d1::UInt # mantissa_struct
  d2::UInt
end

mutable struct mag_struct
  exp::Int # fmpz
  man::UInt # mp_limb_t
end

mutable struct arb_struct
  mid_exp::Int # fmpz
  mid_size::UInt # mp_size_t
  mid_d1::UInt # mantissa_struct
  mid_d2::UInt
  rad_exp::Int # fmpz
  rad_man::UInt
end

mutable struct acb_struct
  real_mid_exp::Int # fmpz
  real_mid_size::UInt # mp_size_t
  real_mid_d1::Int # mantissa_struct
  real_mid_d2::Int
  real_rad_exp::Int # fmpz
  real_rad_man::UInt
  imag_mid_exp::Int # fmpz
  imag_mid_size::UInt # mp_size_t
  imag_mid_d1::Int # mantissa_struct
  imag_mid_d2::Int
  imag_rad_exp::Int # fmpz
  imag_rad_man::UInt
end

mutable struct arb <: FieldElem
  mid_exp::Int # fmpz
  mid_size::UInt # mp_size_t
  mid_d1::UInt # mantissa_struct
  mid_d2::UInt
  rad_exp::Int # fmpz
  rad_man::UInt
  parent::ArbField

  function arb()
    z = new()
    ccall((:arb_init, :libarb), Void, (Ref{arb}, ), z)
    finalizer(z, _arb_clear_fn)
    return z
  end

  function arb(x::Union{Int, UInt, Float64, fmpz, fmpq,
                        BigFloat, AbstractString, arb}, p::Int)
    z = new()
    ccall((:arb_init, :libarb), Void, (Ref{arb}, ), z)
    _arb_set(z, x, p)
    finalizer(z, _arb_clear_fn)
    return z
  end

  function arb(x::Union{Int, UInt, Float64, fmpz, BigFloat, arb})
    z = new()
    ccall((:arb_init, :libarb), Void, (Ref{arb}, ), z)
    _arb_set(z, x)
    finalizer(z, _arb_clear_fn)
    return z
  end

  function arb(mid::arb, rad::arb)
    z = new()
    ccall((:arb_init, :libarb), Void, (Ref{arb}, ), z)
    ccall((:arb_set, :libarb), Void, (Ref{arb}, Ref{arb}), z, mid)
    ccall((:arb_add_error, :libarb), Void, (Ref{arb}, Ref{arb}), z, rad)
    finalizer(z, _arb_clear_fn)
    return z
  end

  #function arb(x::arf)
  #  z = new()
  #  ccall((:arb_init, :libarb), Void, (Ref{arb}, ), z)
  #  ccall((:arb_set_arf, :libarb), Void, (Ref{arb}, Ptr{arf}), z, x)
  #  finalizer(z, _arb_clear_fn)
  #  return z
  #end
end

function _arb_clear_fn(x::arb)
  ccall((:arb_clear, :libarb), Void, (Ref{arb}, ), x)
end

################################################################################
#
#  Types and memory management for AcbField
#
################################################################################

mutable struct AcbField <: Field
  prec::Int

  function AcbField(p::Int = 256, cached::Bool = true)
    arb_check_prec(p)
    if haskey(AcbFieldID, p)
      return AcbFieldID[p]
    else
      z = new(p)
      if cached
        AcbFieldID[p] = z
      end
      return z
    end
  end
end

const AcbFieldID = Dict{Int, AcbField}()

prec(x::AcbField) = x.prec

mutable struct acb <: FieldElem
  real_mid_exp::Int     # fmpz
  real_mid_size::UInt # mp_size_t
  real_mid_d1::UInt    # mantissa_struct
  real_mid_d2::UInt
  real_rad_exp::Int     # fmpz
  real_rad_man::UInt
  imag_mid_exp::Int     # fmpz
  imag_mid_size::UInt # mp_size_t
  imag_mid_d1::UInt    # mantissa_struct
  imag_mid_d2::UInt
  imag_rad_exp::Int     # fmpz
  imag_rad_man::UInt
  parent::AcbField

  function acb()
    z = new()
    ccall((:acb_init, :libarb), Void, (Ref{acb}, ), z)
    finalizer(z, _acb_clear_fn)
    return z
  end

  function acb(x::Union{Int, UInt, Float64, fmpz, BigFloat, arb, acb})
    z = new()
    ccall((:acb_init, :libarb), Void, (Ref{acb}, ), z)
    _acb_set(z, x)
    finalizer(z, _acb_clear_fn)
    return z
  end

  function acb(x::Union{Int, UInt, Float64, fmpz, fmpq,
                        BigFloat, arb, acb, AbstractString}, p::Int)
    z = new()
    ccall((:acb_init, :libarb), Void, (Ref{acb}, ), z)
    _acb_set(z, x, p)
    finalizer(z, _acb_clear_fn)
    return z
  end

  #function acb{T <: Union{Int, UInt, Float64, fmpz, BigFloat, arb}}(x::T, y::T)
  #  z = new()
  #  ccall((:acb_init, :libarb), Void, (Ref{acb}, ), z)
  #  _acb_set(z, x, y)
  #  finalizer(z, _acb_clear_fn)
  #  return z
  #end

  function acb(x::T, y::T, p::Int) where {T <: Union{Int, UInt, Float64, fmpz, fmpq, BigFloat, AbstractString, arb}}
    z = new()
    ccall((:acb_init, :libarb), Void, (Ref{acb}, ), z)
    _acb_set(z, x, y, p)
    finalizer(z, _acb_clear_fn)
    return z
  end
end

function _acb_clear_fn(x::acb)
  ccall((:acb_clear, :libarb), Void, (Ref{acb}, ), x)
end


################################################################################
#
#  Types and memory management for ArbPolyRing
#
################################################################################

mutable struct ArbPolyRing <: PolyRing{arb}
  base_ring::ArbField
  S::Symbol

  function ArbPolyRing(R::ArbField, S::Symbol, cached::Bool = true)
    if haskey(ArbPolyRingID, (R, S))
      return ArbPolyRingID[R, S]
    else
      z = new(R, S)
      if cached
        ArbPolyRingID[R, S] = z
      end
      return z
    end
  end
end

const ArbPolyRingID = Dict{Tuple{ArbField, Symbol}, ArbPolyRing}()

mutable struct arb_poly <: PolyElem{arb}
  coeffs::Ptr{Void}
  length::Int
  alloc::Int
  parent::ArbPolyRing

  function arb_poly()
    z = new()
    ccall((:arb_poly_init, :libarb), Void, (Ref{arb_poly}, ), z)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::arb, p::Int)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ref{arb_poly}, ), z)
    ccall((:arb_poly_set_coeff_arb, :libarb), Void,
                (Ref{arb_poly}, Int, Ref{arb}), z, 0, x)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::Array{arb, 1}, p::Int)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ref{arb_poly}, ), z)
    for i = 1:length(x)
        ccall((:arb_poly_set_coeff_arb, :libarb), Void,
                (Ref{arb_poly}, Int, Ref{arb}), z, i - 1, x[i])
    end
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::arb_poly)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ref{arb_poly}, ), z)
    ccall((:arb_poly_set, :libarb), Void, (Ref{arb_poly}, Ref{arb_poly}), z, x)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::arb_poly, p::Int)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ref{arb_poly}, ), z)
    ccall((:arb_poly_set_round, :libarb), Void,
                (Ref{arb_poly}, Ref{arb_poly}, Int), z, x, p)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::fmpz_poly, p::Int)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ref{arb_poly}, ), z)
    ccall((:arb_poly_set_fmpz_poly, :libarb), Void,
                (Ref{arb_poly}, Ref{fmpz_poly}, Int), z, x, p)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::fmpq_poly, p::Int)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ref{arb_poly}, ), z)
    ccall((:arb_poly_set_fmpq_poly, :libarb), Void,
                (Ref{arb_poly}, Ref{fmpq_poly}, Int), z, x, p)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end
end

function _arb_poly_clear_fn(x::arb_poly)
  ccall((:arb_poly_clear, :libarb), Void, (Ref{arb_poly}, ), x)
end

parent(x::arb_poly) = x.parent

elem_type(x::ArbPolyRing) = arb_poly

var(x::ArbPolyRing) = x.S

prec(x::ArbPolyRing) = prec(x.base_ring)

base_ring(a::ArbPolyRing) = a.base_ring

################################################################################
#
#  Types and memory management for AcbPolyRing
#
################################################################################

mutable struct AcbPolyRing <: PolyRing{acb}
  base_ring::AcbField
  S::Symbol

  function AcbPolyRing(R::AcbField, S::Symbol, cached::Bool = true)
    if haskey(AcbPolyRingID, (R, S))
      return AcbPolyRingID[R, S]
    else
      z = new(R, S)
      if cached
        AcbPolyRingID[R, S] = z
      end
      return z
    end
  end
end

const AcbPolyRingID = Dict{Tuple{AcbField, Symbol}, AcbPolyRing}()

mutable struct acb_poly <: PolyElem{acb}
  coeffs::Ptr{Void}
  length::Int
  alloc::Int
  parent::AcbPolyRing

  function acb_poly()
    z = new()
    ccall((:acb_poly_init, :libarb), Void, (Ref{acb_poly}, ), z)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::acb, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ref{acb_poly}, ), z)
    ccall((:acb_poly_set_coeff_acb, :libarb), Void,
                (Ref{acb_poly}, Int, Ref{acb}), z, 0, x)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::Array{acb, 1}, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ref{acb_poly}, ), z)
    for i = 1:length(x)
        ccall((:acb_poly_set_coeff_acb, :libarb), Void,
                (Ref{acb_poly}, Int, Ref{acb}), z, i - 1, x[i])
    end
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::acb_poly)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ref{acb_poly}, ), z)
    ccall((:acb_poly_set, :libarb), Void, (Ref{acb_poly}, Ref{acb_poly}), z, x)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::arb_poly, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ref{acb_poly}, ), z)
    ccall((:acb_poly_set_arb_poly, :libarb), Void,
                (Ref{acb_poly}, Ref{arb_poly}, Int), z, x, p)
    ccall((:acb_poly_set_round, :libarb), Void,
                (Ref{acb_poly}, Ref{acb_poly}, Int), z, z, p)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::acb_poly, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ref{acb_poly}, ), z)
    ccall((:acb_poly_set_round, :libarb), Void,
                (Ref{acb_poly}, Ref{acb_poly}, Int), z, x, p)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::fmpz_poly, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ref{acb_poly}, ), z)
    ccall((:acb_poly_set_fmpz_poly, :libarb), Void,
                (Ref{acb_poly}, Ref{fmpz_poly}, Int), z, x, p)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::fmpq_poly, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ref{acb_poly}, ), z)
    ccall((:acb_poly_set_fmpq_poly, :libarb), Void,
                (Ref{acb_poly}, Ref{fmpq_poly}, Int), z, x, p)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end
end

function _acb_poly_clear_fn(x::acb_poly)
  ccall((:acb_poly_clear, :libarb), Void, (Ref{acb_poly}, ), x)
end

parent(x::acb_poly) = x.parent

elem_type(x::AcbPolyRing) = acb_poly

var(x::AcbPolyRing) = x.S

prec(x::AcbPolyRing) = prec(x.base_ring)

base_ring(a::AcbPolyRing) = a.base_ring

################################################################################
#
#  Types and memory management for ArbMatSpace
#
################################################################################

mutable struct ArbMatSpace <: MatSpace{arb}
  rows::Int
  cols::Int
  base_ring::ArbField

  function ArbMatSpace(R::ArbField, r::Int, c::Int, cached::Bool = true)
    if haskey(ArbMatSpaceID, (R, r, c))
      return ArbMatSpaceID[(R, r, c)]
    else
      z = new(r, c, R)
      if cached
        ArbMatSpaceID[(R, r, c)] = z
      end
      return z::ArbMatSpace
    end
  end
end

const ArbMatSpaceID = Dict{Tuple{ArbField, Int, Int}, ArbMatSpace}()

mutable struct arb_mat <: MatElem{arb}
  entries::Ptr{Void}
  r::Int
  c::Int
  rows::Ptr{Void}
  base_ring::ArbField

  function arb_mat(r::Int, c::Int)
    z = new()
    ccall((:arb_mat_init, :libarb), Void, (Ref{arb_mat}, Int, Int), z, r, c)
    finalizer(z, _arb_mat_clear_fn)
    return z
  end

  function arb_mat(a::fmpz_mat)
    z = new()
    ccall((:arb_mat_init, :libarb), Void,
                (Ref{arb_mat}, Int, Int), z, a.r, a.c)
    ccall((:arb_mat_set_fmpz_mat, :libarb), Void,
                (Ref{arb_mat}, Ref{fmpz_mat}), z, a)
    finalizer(z, _arb_mat_clear_fn)
    return z
  end
  
  function arb_mat(a::fmpz_mat, prec::Int)
    z = new()
    ccall((:arb_mat_init, :libarb), Void,
                (Ref{arb_mat}, Int, Int), z, a.r, a.c)
    ccall((:arb_mat_set_round_fmpz_mat, :libarb), Void,
                (Ref{arb_mat}, Ref{fmpz_mat}, Int), z, a, prec)
    finalizer(z, _arb_mat_clear_fn)
    return z
  end

  function arb_mat(r::Int, c::Int, arr::Array{T, 2}) where {T <: Union{Int, UInt, fmpz, Float64, BigFloat, arb}}
    z = new()
    ccall((:arb_mat_init, :libarb), Void, 
                (Ref{arb_mat}, Int, Int), z, r, c)
    finalizer(z, _arb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, :libarb), Ptr{arb},
                    (Ref{arb_mat}, Int, Int), z, i - 1, j - 1)
        Nemo._arb_set(el, arr[i, j])
      end
    end
    return z
  end

  function arb_mat(r::Int, c::Int, arr::Array{T, 1}) where {T <: Union{Int, UInt, fmpz, Float64, BigFloat, arb}}
    z = new()
    ccall((:arb_mat_init, :libarb), Void, 
                (Ref{arb_mat}, Int, Int), z, r, c)
    finalizer(z, _arb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, :libarb), Ptr{arb},
                    (Ref{arb_mat}, Int, Int), z, i - 1, j - 1)
        Nemo._arb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function arb_mat(r::Int, c::Int, arr::Array{T, 2}, prec::Int) where {T <: Union{Int, UInt, fmpz, fmpq, Float64, BigFloat, arb, AbstractString}}
    z = new()
    ccall((:arb_mat_init, :libarb), Void, 
                (Ref{arb_mat}, Int, Int), z, r, c)
    finalizer(z, _arb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, :libarb), Ptr{arb},
                    (Ref{arb_mat}, Int, Int), z, i - 1, j - 1)
        _arb_set(el, arr[i, j], prec)
      end
    end
    return z
  end
     
  function arb_mat(r::Int, c::Int, arr::Array{T, 1}, prec::Int) where {T <: Union{Int, UInt, fmpz, fmpq, Float64, BigFloat, arb, AbstractString}}
    z = new()
    ccall((:arb_mat_init, :libarb), Void, 
                (Ref{arb_mat}, Int, Int), z, r, c)
    finalizer(z, _arb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, :libarb), Ptr{arb},
                    (Ref{arb_mat}, Int, Int), z, i - 1, j - 1)
        _arb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function arb_mat(a::fmpq_mat, prec::Int)
    z = new()
    ccall((:arb_mat_init, :libarb), Void,
                (Ref{arb_mat}, Int, Int), z, a.r, a.c)
    ccall((:arb_mat_set_fmpq_mat, :libarb), Void,
                (Ref{arb_mat}, Ref{fmpq_mat}, Int), z, a, prec)
    finalizer(z, _arb_mat_clear_fn)
    return z
  end
end

function _arb_mat_clear_fn(x::arb_mat)
  ccall((:arb_mat_clear, :libarb), Void, (Ref{arb_mat}, ), x)
end

################################################################################
#
#  Types and memory management for AcbMatSpace
#
################################################################################

mutable struct AcbMatSpace <: MatSpace{acb}
  rows::Int
  cols::Int
  base_ring::AcbField

  function AcbMatSpace(R::AcbField, r::Int, c::Int, cached::Bool = true)
    if haskey(AcbMatSpaceID, (R, r, c))
      return AcbMatSpaceID[(R, r, c)]
    else
      z = new(r, c, R)
      if cached
        AcbMatSpaceID[(R, r, c)] = z
      end
      return z::AcbMatSpace
    end
  end
end

const AcbMatSpaceID = Dict{Tuple{AcbField, Int, Int}, AcbMatSpace}()

mutable struct acb_mat <: MatElem{acb}
  entries::Ptr{Void}
  r::Int
  c::Int
  rows::Ptr{Void}
  base_ring::AcbField

  function acb_mat(r::Int, c::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void, (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end

  function acb_mat(a::fmpz_mat)
    z = new()
    ccall((:acb_mat_init, :libarb), Void,
                (Ref{acb_mat}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_fmpz_mat, :libarb), Void,
                (Ref{acb_mat}, Ref{fmpz_mat}), z, a)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end
  
  function acb_mat(a::fmpz_mat, prec::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void,
                (Ref{acb_mat}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_round_fmpz_mat, :libarb), Void,
                (Ref{acb_mat}, Ref{fmpz_mat}, Int), z, a, prec)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end

  function acb_mat(a::arb_mat)
    z = new()
    ccall((:acb_mat_init, :libarb), Void,
                (Ref{acb_mat}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_arb_mat, :libarb), Void,
                (Ref{acb_mat}, Ref{arb_mat}), z, a)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end

  function acb_mat(a::arb_mat, prec::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void,
                (Ref{acb_mat}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_round_arb_mat, :libarb), Void,
                (Ref{acb_mat}, Ref{arb_mat}, Int), z, a, prec)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end
   
  function acb_mat(r::Int, c::Int, arr::Array{T, 2}) where {T <: Union{Int, UInt, Float64, fmpz}}
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function acb_mat(r::Int, c::Int, arr::Array{T, 2}) where {T <: Union{BigFloat, acb, arb}}
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function acb_mat(r::Int, c::Int, arr::Array{T, 1}) where {T <: Union{Int, UInt, Float64, fmpz}}
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function acb_mat(r::Int, c::Int, arr::Array{T, 1}) where {T <: Union{BigFloat, acb, arb}}
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function acb_mat(r::Int, c::Int, arr::Array{T, 2}, prec::Int) where {T <: Union{Int, UInt, fmpz, fmpq, Float64}}
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function acb_mat(r::Int, c::Int, arr::Array{T, 2}, prec::Int) where {T <: Union{BigFloat, arb, AbstractString, acb}}
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function acb_mat(r::Int, c::Int, arr::Array{T, 1}, prec::Int) where {T <: Union{Int, UInt, fmpz, fmpq, Float64}}
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function acb_mat(r::Int, c::Int, arr::Array{T, 1}, prec::Int) where {T <: Union{BigFloat, arb, AbstractString, acb}}
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function acb_mat(r::Int, c::Int, arr::Array{Tuple{T, T}, 2}, prec::Int) where {T <: Union{Int, UInt, Float64, fmpz}}

    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function acb_mat(r::Int, c::Int, arr::Array{Tuple{T, T}, 2}, prec::Int) where {T <: Union{fmpq, BigFloat, arb, AbstractString}}

    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function acb_mat(r::Int, c::Int, arr::Array{Tuple{T, T}, 1}, prec::Int) where {T <: Union{Int, UInt, Float64, fmpz}}

    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function acb_mat(r::Int, c::Int, arr::Array{Tuple{T, T}, 1}, prec::Int) where {T <: Union{fmpq, BigFloat, arb, AbstractString}}

    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ref{acb_mat}, Int, Int), z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ref{acb_mat}, Int, Int), z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function acb_mat(a::fmpq_mat, prec::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void,
                (Ref{acb_mat}, Int, Int), z, a.r, a.c)
    ccall((:acb_mat_set_fmpq_mat, :libarb), Void,
                (Ref{acb_mat}, Ref{fmpq_mat}, Int), z, a, prec)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end
end

function _acb_mat_clear_fn(x::acb_mat)
  ccall((:acb_mat_clear, :libarb), Void, (Ref{acb_mat}, ), x)
end

