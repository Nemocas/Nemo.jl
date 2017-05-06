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

################################################################################
#
#  Types and memory management for ArbField
#
################################################################################

const ArbFieldID = ObjectIdDict()

type ArbField <: Field
  prec::Int

  function ArbField(p::Int = 256; cached = true)
    arb_check_prec(p)
    if haskey(ArbFieldID, p)
      return ArbFieldID[p]::ArbField
    else
      z = new(p)
      if cached
        ArbFieldID[p] = z
      end
      return z
    end
  end
end

prec(x::ArbField) = x.prec

# these may be used for shallow operations
type arf_struct
  exp::Int # fmpz
  size::UInt # mp_size_t
  d1::UInt # mantissa_struct
  d2::UInt
end

type mag_struct
  exp::Int # fmpz
  man::UInt # mp_limb_t
end

type arb_struct
  mid_exp::Int # fmpz
  mid_size::UInt # mp_size_t
  mid_d1::UInt # mantissa_struct
  mid_d2::UInt
  rad_exp::Int # fmpz
  rad_man::UInt
end

type acb_struct
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

type arb <: FieldElem
  mid_exp::Int # fmpz
  mid_size::UInt # mp_size_t
  mid_d1::UInt # mantissa_struct
  mid_d2::UInt
  rad_exp::Int # fmpz
  rad_man::UInt
  parent::ArbField

  function arb()
    z = new()
    ccall((:arb_init, :libarb), Void, (Ptr{arb}, ), &z)
    finalizer(z, _arb_clear_fn)
    return z
  end

  function arb(x::Union{Int, UInt, Float64, fmpz, fmpq,
                        BigFloat, AbstractString, arb}, p::Int)
    z = new()
    ccall((:arb_init, :libarb), Void, (Ptr{arb}, ), &z)
    _arb_set(z, x, p)
    finalizer(z, _arb_clear_fn)
    return z
  end

  function arb(x::Union{Int, UInt, Float64, fmpz, BigFloat, arb})
    z = new()
    ccall((:arb_init, :libarb), Void, (Ptr{arb}, ), &z)
    _arb_set(z, x)
    finalizer(z, _arb_clear_fn)
    return z
  end

  function arb(mid::arb, rad::arb)
    z = new()
    ccall((:arb_init, :libarb), Void, (Ptr{arb}, ), &z)
    ccall((:arb_set, :libarb), Void, (Ptr{arb}, Ptr{arb}), &z, &mid)
    ccall((:arb_add_error, :libarb), Void, (Ptr{arb}, Ptr{arb}), &z, &rad)
    finalizer(z, _arb_clear_fn)
    return z
  end

  #function arb(x::arf)
  #  z = new()
  #  ccall((:arb_init, :libarb), Void, (Ptr{arb}, ), &z)
  #  ccall((:arb_set_arf, :libarb), Void, (Ptr{arb}, Ptr{arf}), &z, &x)
  #  finalizer(z, _arb_clear_fn)
  #  return z
  #end
end

function _arb_clear_fn(x::arb)
  ccall((:arb_clear, :libarb), Void, (Ptr{arb}, ), &x)
end

################################################################################
#
#  Types and memory management for AcbField
#
################################################################################

const AcbFieldID = ObjectIdDict()

type AcbField <: Field
  prec::Int

  function AcbField(p::Int = 256; cached = true)
    arb_check_prec(p)
    if haskey(AcbFieldID, p)
      return AcbFieldID[p]::AcbField
    else
      z = new(p)
      if cached
        AcbFieldID[p] = z
      end
      return z
    end
  end
end

prec(x::AcbField) = x.prec

type acb <: FieldElem
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
    ccall((:acb_init, :libarb), Void, (Ptr{acb}, ), &z)
    finalizer(z, _acb_clear_fn)
    return z
  end

  function acb(x::Union{Int, UInt, Float64, fmpz, BigFloat, arb, acb})
    z = new()
    ccall((:acb_init, :libarb), Void, (Ptr{acb}, ), &z)
    _acb_set(z, x)
    finalizer(z, _acb_clear_fn)
    return z
  end

  function acb(x::Union{Int, UInt, Float64, fmpz, fmpq,
                        BigFloat, arb, acb, AbstractString}, p::Int)
    z = new()
    ccall((:acb_init, :libarb), Void, (Ptr{acb}, ), &z)
    _acb_set(z, x, p)
    finalizer(z, _acb_clear_fn)
    return z
  end

  #function acb{T <: Union{Int, UInt, Float64, fmpz, BigFloat, arb}}(x::T, y::T)
  #  z = new()
  #  ccall((:acb_init, :libarb), Void, (Ptr{acb}, ), &z)
  #  _acb_set(z, x, y)
  #  finalizer(z, _acb_clear_fn)
  #  return z
  #end

  function acb{T <: Union{Int, UInt, Float64, fmpz, fmpq,
                          BigFloat, AbstractString, arb}}(x::T, y::T, p::Int)
    z = new()
    ccall((:acb_init, :libarb), Void, (Ptr{acb}, ), &z)
    _acb_set(z, x, y, p)
    finalizer(z, _acb_clear_fn)
    return z
  end
end

function _acb_clear_fn(x::acb)
  ccall((:acb_clear, :libarb), Void, (Ptr{acb}, ), &x)
end


################################################################################
#
#  Types and memory management for ArbPolyRing
#
################################################################################

const ArbPolyRingID = ObjectIdDict()

type ArbPolyRing <: PolyRing{arb}
  base_ring::ArbField
  S::Symbol

  function ArbPolyRing(R::ArbField, S::Symbol, cached = true)
    if haskey(ArbPolyRingID, (R, S))
      return ArbPolyRingID[R, S]::ArbPolyRing
    else
      z = new(R, S)
      if cached
        ArbPolyRingID[R, S] = z
      end
      return z
    end
  end
end

type arb_poly <: PolyElem{arb}
  coeffs::Ptr{Void}
  length::Int
  alloc::Int
  parent::ArbPolyRing

  function arb_poly()
    z = new()
    ccall((:arb_poly_init, :libarb), Void, (Ptr{arb_poly}, ), &z)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::arb, p::Int)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ptr{arb_poly}, ), &z)
    ccall((:arb_poly_set_coeff_arb, :libarb), Void,
                (Ptr{arb_poly}, Int, Ptr{arb}), &z, 0, &x)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::Array{arb, 1}, p::Int)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ptr{arb_poly}, ), &z)
    for i = 1:length(x)
        ccall((:arb_poly_set_coeff_arb, :libarb), Void,
                (Ptr{arb_poly}, Int, Ptr{arb}), &z, i - 1, &x[i])
    end
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::arb_poly)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ptr{arb_poly}, ), &z)
    ccall((:arb_poly_set, :libarb), Void, (Ptr{arb_poly}, Ptr{arb_poly}), &z, &x)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::arb_poly, p::Int)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ptr{arb_poly}, ), &z)
    ccall((:arb_poly_set_round, :libarb), Void,
                (Ptr{arb_poly}, Ptr{arb_poly}, Int), &z, &x, p)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::fmpz_poly, p::Int)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ptr{arb_poly}, ), &z)
    ccall((:arb_poly_set_fmpz_poly, :libarb), Void,
                (Ptr{arb_poly}, Ptr{fmpz_poly}, Int), &z, &x, p)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end

  function arb_poly(x::fmpq_poly, p::Int)
    z = new() 
    ccall((:arb_poly_init, :libarb), Void, (Ptr{arb_poly}, ), &z)
    ccall((:arb_poly_set_fmpq_poly, :libarb), Void,
                (Ptr{arb_poly}, Ptr{fmpq_poly}, Int), &z, &x, p)
    finalizer(z, _arb_poly_clear_fn)
    return z
  end
end

function _arb_poly_clear_fn(x::arb_poly)
  ccall((:arb_poly_clear, :libarb), Void, (Ptr{arb_poly}, ), &x)
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

const AcbPolyRingID = ObjectIdDict()

type AcbPolyRing <: PolyRing{acb}
  base_ring::AcbField
  S::Symbol

  function AcbPolyRing(R::AcbField, S::Symbol, cached = true)
    if haskey(AcbPolyRingID, (R, S))
      return AcbPolyRingID[R, S]::AcbPolyRing
    else
      z = new(R, S)
      if cached
        AcbPolyRingID[R, S] = z
      end
      return z
    end
  end
end

type acb_poly <: PolyElem{acb}
  coeffs::Ptr{Void}
  length::Int
  alloc::Int
  parent::AcbPolyRing

  function acb_poly()
    z = new()
    ccall((:acb_poly_init, :libarb), Void, (Ptr{acb_poly}, ), &z)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::acb, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ptr{acb_poly}, ), &z)
    ccall((:acb_poly_set_coeff_acb, :libarb), Void,
                (Ptr{acb_poly}, Int, Ptr{acb}), &z, 0, &x)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::Array{acb, 1}, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ptr{acb_poly}, ), &z)
    for i = 1:length(x)
        ccall((:acb_poly_set_coeff_acb, :libarb), Void,
                (Ptr{acb_poly}, Int, Ptr{acb}), &z, i - 1, &x[i])
    end
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::acb_poly)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ptr{acb_poly}, ), &z)
    ccall((:acb_poly_set, :libarb), Void, (Ptr{acb_poly}, Ptr{acb_poly}), &z, &x)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::arb_poly, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ptr{acb_poly}, ), &z)
    ccall((:acb_poly_set_arb_poly, :libarb), Void,
                (Ptr{acb_poly}, Ptr{arb_poly}, Int), &z, &x, p)
    ccall((:acb_poly_set_round, :libarb), Void,
                (Ptr{acb_poly}, Ptr{acb_poly}, Int), &z, &z, p)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::acb_poly, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ptr{acb_poly}, ), &z)
    ccall((:acb_poly_set_round, :libarb), Void,
                (Ptr{acb_poly}, Ptr{acb_poly}, Int), &z, &x, p)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::fmpz_poly, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ptr{acb_poly}, ), &z)
    ccall((:acb_poly_set_fmpz_poly, :libarb), Void,
                (Ptr{acb_poly}, Ptr{fmpz_poly}, Int), &z, &x, p)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end

  function acb_poly(x::fmpq_poly, p::Int)
    z = new() 
    ccall((:acb_poly_init, :libarb), Void, (Ptr{acb_poly}, ), &z)
    ccall((:acb_poly_set_fmpq_poly, :libarb), Void,
                (Ptr{acb_poly}, Ptr{fmpq_poly}, Int), &z, &x, p)
    finalizer(z, _acb_poly_clear_fn)
    return z
  end
end

function _acb_poly_clear_fn(x::acb_poly)
  ccall((:acb_poly_clear, :libarb), Void, (Ptr{acb_poly}, ), &x)
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

const ArbMatSpaceID = ObjectIdDict()

type ArbMatSpace <: MatSpace{arb}
  rows::Int
  cols::Int
  base_ring::ArbField

  function ArbMatSpace(R::ArbField, r::Int, c::Int, cached = true)
    if haskey(ArbMatSpaceID, (R, r, c))
      return ArbMatSpaceID[(R, r, c)]::ArbMatSpace
    else
      z = new(r, c, R)
      if cached
        ArbMatSpaceID[(R, r, c)] = z
      end
      return z::ArbMatSpace
    end
  end
end

type arb_mat <: MatElem{arb}
  entries::Ptr{Void}
  r::Int
  c::Int
  rows::Ptr{Void}
  parent::ArbMatSpace

  function arb_mat(r::Int, c::Int)
    z = new()
    ccall((:arb_mat_init, :libarb), Void, (Ptr{arb_mat}, Int, Int), &z, r, c)
    finalizer(z, _arb_mat_clear_fn)
    return z
  end

  function arb_mat(a::fmpz_mat)
    z = new()
    ccall((:arb_mat_init, :libarb), Void,
                (Ptr{arb_mat}, Int, Int), &z, a.r, a.c)
    ccall((:arb_mat_set_fmpz_mat, :libarb), Void,
                (Ptr{arb_mat}, Ptr{fmpz_mat}), &z, &a)
    finalizer(z, _arb_mat_clear_fn)
    return z
  end
  
  function arb_mat(a::fmpz_mat, prec::Int)
    z = new()
    ccall((:arb_mat_init, :libarb), Void,
                (Ptr{arb_mat}, Int, Int), &z, a.r, a.c)
    ccall((:arb_mat_set_round_fmpz_mat, :libarb), Void,
                (Ptr{arb_mat}, Ptr{fmpz_mat}, Int), &z, &a, prec)
    finalizer(z, _arb_mat_clear_fn)
    return z
  end

  function arb_mat{T <: Union{Int, UInt, fmpz, Float64, BigFloat,
                              arb}}(r::Int, c::Int, arr::Array{T, 2})
    z = new()
    ccall((:arb_mat_init, :libarb), Void, 
                (Ptr{arb_mat}, Int, Int), &z, r, c)
    finalizer(z, _arb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, :libarb), Ptr{arb},
                    (Ptr{arb_mat}, Int, Int), &z, i - 1, j - 1)
        Nemo._arb_set(el, arr[i, j])
      end
    end
    return z
  end

  function arb_mat{T <: Union{Int, UInt, fmpz, Float64, BigFloat,
                              arb}}(r::Int, c::Int, arr::Array{T, 1})
    z = new()
    ccall((:arb_mat_init, :libarb), Void, 
                (Ptr{arb_mat}, Int, Int), &z, r, c)
    finalizer(z, _arb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, :libarb), Ptr{arb},
                    (Ptr{arb_mat}, Int, Int), &z, i - 1, j - 1)
        Nemo._arb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function arb_mat{T <: Union{Int, UInt, fmpz, fmpq, Float64, BigFloat, arb,
                              AbstractString}}(r::Int, c::Int, arr::Array{T, 2},
                                               prec::Int)
    z = new()
    ccall((:arb_mat_init, :libarb), Void, 
                (Ptr{arb_mat}, Int, Int), &z, r, c)
    finalizer(z, _arb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, :libarb), Ptr{arb},
                    (Ptr{arb_mat}, Int, Int), &z, i - 1, j - 1)
        _arb_set(el, arr[i, j], prec)
      end
    end
    return z
  end
     
  function arb_mat{T <: Union{Int, UInt, fmpz, fmpq, Float64, BigFloat, arb,
                              AbstractString}}(r::Int, c::Int, arr::Array{T, 1},
                                               prec::Int)
    z = new()
    ccall((:arb_mat_init, :libarb), Void, 
                (Ptr{arb_mat}, Int, Int), &z, r, c)
    finalizer(z, _arb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:arb_mat_entry_ptr, :libarb), Ptr{arb},
                    (Ptr{arb_mat}, Int, Int), &z, i - 1, j - 1)
        _arb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function arb_mat(a::fmpq_mat, prec::Int)
    z = new()
    ccall((:arb_mat_init, :libarb), Void,
                (Ptr{arb_mat}, Int, Int), &z, a.r, a.c)
    ccall((:arb_mat_set_fmpq_mat, :libarb), Void,
                (Ptr{arb_mat}, Ptr{fmpq_mat}, Int), &z, &a, prec)
    finalizer(z, _arb_mat_clear_fn)
    return z
  end
end

function _arb_mat_clear_fn(x::arb_mat)
  ccall((:arb_mat_clear, :libarb), Void, (Ptr{arb_mat}, ), &x)
end

parent(x::arb_mat) = x.parent

elem_type(x::ArbMatSpace) = arb_mat

prec(x::ArbMatSpace) = prec(x.base_ring)

base_ring(a::ArbMatSpace) = a.base_ring

################################################################################
#
#  Types and memory management for AcbMatSpace
#
################################################################################

const AcbMatSpaceID = ObjectIdDict()

type AcbMatSpace <: MatSpace{acb}
  rows::Int
  cols::Int
  base_ring::AcbField

  function AcbMatSpace(R::AcbField, r::Int, c::Int, cached = true)
    if haskey(AcbMatSpaceID, (R, r, c))
      return AcbMatSpaceID[(R, r, c)]::AcbMatSpace
    else
      z = new(r, c, R)
      if cached
        AcbMatSpaceID[(R, r, c)] = z
      end
      return z::AcbMatSpace
    end
  end
end

type acb_mat <: MatElem{acb}
  entries::Ptr{Void}
  r::Int
  c::Int
  rows::Ptr{Void}
  parent::AcbMatSpace

  function acb_mat(r::Int, c::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void, (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end

  function acb_mat(a::fmpz_mat)
    z = new()
    ccall((:acb_mat_init, :libarb), Void,
                (Ptr{acb_mat}, Int, Int), &z, a.r, a.c)
    ccall((:acb_mat_set_fmpz_mat, :libarb), Void,
                (Ptr{acb_mat}, Ptr{fmpz_mat}), &z, &a)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end
  
  function acb_mat(a::fmpz_mat, prec::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void,
                (Ptr{acb_mat}, Int, Int), &z, a.r, a.c)
    ccall((:acb_mat_set_round_fmpz_mat, :libarb), Void,
                (Ptr{acb_mat}, Ptr{fmpz_mat}, Int), &z, &a, prec)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end

  function acb_mat(a::arb_mat)
    z = new()
    ccall((:acb_mat_init, :libarb), Void,
                (Ptr{acb_mat}, Int, Int), &z, a.r, a.c)
    ccall((:acb_mat_set_arb_mat, :libarb), Void,
                (Ptr{acb_mat}, Ptr{arb_mat}), &z, &a)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end

  function acb_mat(a::arb_mat, prec::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void,
                (Ptr{acb_mat}, Int, Int), &z, a.r, a.c)
    ccall((:acb_mat_set_round_arb_mat, :libarb), Void,
                (Ptr{acb_mat}, Ptr{arb_mat}, Int), &z, &a, prec)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end
   
  function acb_mat{T <: Union{Int, UInt, Float64, fmpz}}(r::Int,
                                          c::Int, arr::Array{T, 2})
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function acb_mat{T <: Union{BigFloat, acb, arb}}(r::Int,
                                          c::Int, arr::Array{T, 2})
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[i, j])
      end
    end
    return z
  end

  function acb_mat{T <: Union{Int, UInt, Float64, fmpz}}(r::Int,
                                          c::Int, arr::Array{T, 1})
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function acb_mat{T <: Union{BigFloat, acb, arb}}(r::Int, c::Int,
                                                  arr::Array{T, 1})
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j])
      end
    end
    return z
  end

  function acb_mat{T <: Union{Int, UInt, fmpz, fmpq, Float64}}(r::Int, c::Int,
                                                    arr::Array{T, 2}, prec::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function acb_mat{T <: Union{BigFloat, arb, AbstractString, acb}}(r::Int, c::Int,
                                                    arr::Array{T, 2}, prec::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[i, j], prec)
      end
    end
    return z
  end

  function acb_mat{T <: Union{Int, UInt, fmpz, fmpq, Float64}}(r::Int, c::Int,
                                                    arr::Array{T, 1}, prec::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function acb_mat{T <: Union{BigFloat, arb, AbstractString, acb}}(r::Int, c::Int,
                                                    arr::Array{T, 1}, prec::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j], prec)
      end
    end
    return z
  end

  function acb_mat{T <: Union{Int, UInt, Float64, fmpz}}(r::Int, c::Int,
                                               arr::Array{Tuple{T, T}, 2},
                                               prec::Int)

    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function acb_mat{T <: Union{fmpq, BigFloat, arb, AbstractString}}(r::Int, c::Int,
                                               arr::Array{Tuple{T, T}, 2},
                                               prec::Int)

    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[i, j][1], arr[i,j][2], prec)
      end
    end
    return z
  end

  function acb_mat{T <: Union{Int, UInt, Float64, fmpz}}(r::Int, c::Int,
                                               arr::Array{Tuple{T, T}, 1},
                                               prec::Int)

    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function acb_mat{T <: Union{fmpq, BigFloat, arb, AbstractString}}(r::Int, c::Int,
                                               arr::Array{Tuple{T, T}, 1},
                                               prec::Int)

    z = new()
    ccall((:acb_mat_init, :libarb), Void, 
                (Ptr{acb_mat}, Int, Int), &z, r, c)
    finalizer(z, _acb_mat_clear_fn)
    for i = 1:r
      for j = 1:c
        el = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
                    (Ptr{acb_mat}, Int, Int), &z, i - 1, j - 1)
        _acb_set(el, arr[(i-1)*c+j][1], arr[(i-1)*c+j][2], prec)
      end
    end
    return z
  end

  function acb_mat(a::fmpq_mat, prec::Int)
    z = new()
    ccall((:acb_mat_init, :libarb), Void,
                (Ptr{acb_mat}, Int, Int), &z, a.r, a.c)
    ccall((:acb_mat_set_fmpq_mat, :libarb), Void,
                (Ptr{acb_mat}, Ptr{fmpq_mat}, Int), &z, &a, prec)
    finalizer(z, _acb_mat_clear_fn)
    return z
  end
end

function _acb_mat_clear_fn(x::acb_mat)
  ccall((:acb_mat_clear, :libarb), Void, (Ptr{acb_mat}, ), &x)
end

parent(x::acb_mat) = x.parent

elem_type(x::AcbMatSpace) = acb_mat

prec(x::AcbMatSpace) = prec(x.base_ring)

base_ring(a::AcbMatSpace) = a.base_ring

