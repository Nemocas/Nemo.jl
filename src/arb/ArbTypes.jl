###############################################################################
#
#   ArbTypes.jl : Parent and object types for Arb
#
#   Copyright (C) 2015 Tommy Hofmann
#   Copyright (C) 2015 Fredrik Johansson
#
###############################################################################

export ArbField, arb, AcbField, acb

arb_check_prec(p::Int) = (p >= 2 && p < (typemax(Int) >> 4)) || throw(ArgumentError("invalid precision"))

################################################################################
#
#  Types and memory management for ArbField
#
################################################################################

const ArbFieldID = ObjectIdDict()

type ArbField <: Field
  prec::Int

  function ArbField(p::Int = 256)
    arb_check_prec(p)
    try
      return ArbFieldID[p]::ArbField
    catch
      ArbFieldID[p] = new(p)
      return ArbFieldID[p]::ArbField
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

parent(x::arb) = x.parent

function deepcopy(a::arb)
  b = parent(a)()
  ccall((:arb_set, :libarb), Void, (Ptr{arb}, Ptr{arb}), &b, &a)
  return b
end

################################################################################
#
#  Types and memory management for AcbField
#
################################################################################

const AcbFieldID = ObjectIdDict()

type AcbField <: Field
  prec::Int

  function AcbField(p::Int = 256)
    arb_check_prec(p)
    try
      return AcbFieldID[p]::AcbField
    catch
      AcbFieldID[p] = new(p)::AcbField
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

  function acb{T <: Union{Int, UInt, Float64, fmpz, BigFloat, arb}}(x::T, y::T)
    z = new()
    ccall((:acb_init, :libarb), Void, (Ptr{acb}, ), &z)
    _acb_set(z, x, y)
    finalizer(z, _acb_clear_fn)
    return z
  end

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

parent(x::acb) = x.parent

function deepcopy(a::acb)
  b = parent(a)()
  ccall((:acb_set, :libarb), Void, (Ptr{acb}, Ptr{acb}), &b, &a)
  return b
end

