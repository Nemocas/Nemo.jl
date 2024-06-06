###############################################################################
#
#   ca.jl : Calcium field elements
#
###############################################################################

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent(a::CalciumFieldElem) = a.parent

parent_type(::Type{CalciumFieldElem}) = CalciumField

elem_type(::Type{CalciumField}) = CalciumFieldElem

base_ring_type(::Type{CalciumField}) = typeof(Union{})

base_ring(a::CalciumField) = Union{}

is_domain_type(::Type{CalciumFieldElem}) = true

function deepcopy_internal(a::CalciumFieldElem, dict::IdDict)
  C = a.parent
  r = C()
  ccall((:ca_set, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  return r
end

function check_parent(a::CalciumFieldElem, b::CalciumFieldElem, throw::Bool = true)
  b = (parent(a) != parent(b))
  b && throw && error("Different parents")
  return !b
end

function _isspecial(a::CalciumFieldElem)
  return (a.field & 3) != 0
end

# todo: distinguish unknown
function check_special(a::CalciumFieldElem)
  if !a.parent.extended && _isspecial(a)
    throw(DomainError(a, "Non-number result"))
  end
end

function same_parent(a::CalciumFieldElem, b::CalciumFieldElem)
  if a.parent == b.parent
    return (a, b)
  else
    C = a.parent
    r = C()
    ccall((:ca_transfer, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumField}, Ref{CalciumFieldElem}, Ref{CalciumField}),
          r, a.parent, b, b.parent)
    check_special(r)
    return (a, r)
  end
end

###############################################################################
#
#   Hashing
#
###############################################################################

# todo: implement nontrivial hash functions on C
function Base.hash(a::CalciumFieldElem, h::UInt)
  return h
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::CalciumFieldElem) = a

###############################################################################
#
#   I/O
#
###############################################################################

function show(io::IO, C::CalciumField)
  @show_name(io, C)
  @show_special(io, C)
  if C.extended
    print(io, "Exact complex field (extended)")
  else
    print(io, "Exact complex field")
  end
end

function native_string(x::CalciumFieldElem)
  cstr = ccall((:ca_get_str, libflint),
               Ptr{UInt8}, (Ref{CalciumFieldElem}, Ref{CalciumField}), x, x.parent)
  res = unsafe_string(cstr)
  ccall((:flint_free, libflint), Nothing, (Ptr{UInt8},), cstr)

  return res
end

function show(io::IO, x::CalciumFieldElem)
  print(io, native_string(x))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

zero(C::CalciumField) = C()

function one(C::CalciumField)
  z = CalciumFieldElem(C)
  ccall((:ca_one, libflint), Nothing, (Ref{CalciumFieldElem}, Ref{CalciumField}), z, C)
  return z
end

###############################################################################
#
#   Random generation
#
###############################################################################

function rand(C::CalciumField; depth::Int, bits::Int,
    randtype::Symbol=:null)
  state = _flint_rand_states[Threads.threadid()]
  x = C()

  depth = max(depth, 0)
  bits = max(bits, 1)

  if randtype == :null
    ccall((:ca_randtest, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{rand_ctx}, Int, Int, Ref{CalciumField}),
          x, state, depth, bits, C)
  elseif randtype == :rational
    ccall((:ca_randtest_rational, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{rand_ctx}, Int, Ref{CalciumField}),
          x, state, bits, C)
  elseif randtype == :special
    ccall((:ca_randtest_special, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{rand_ctx}, Int, Int, Ref{CalciumField}),
          x, state, depth, bits, C)
  else
    error("randtype not defined")
  end

  check_special(x)
  return x
end

###############################################################################
#
#   Comparison and predicates
#
###############################################################################

function ==(a::CalciumFieldElem, b::CalciumFieldElem)
  a, b = same_parent(a, b)
  C = a.parent
  t = ccall((:ca_check_equal, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), a, b, C)
  return truth_as_bool(t, :isequal)
end

function isless(a::CalciumFieldElem, b::CalciumFieldElem)
  a, b = same_parent(a, b)
  C = a.parent
  t = ccall((:ca_check_lt, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), a, b, C)
  return truth_as_bool(t, :isless)
end

isless(a::CalciumFieldElem, b::QQBarFieldElem) = isless(a, parent(a)(b))
isless(a::CalciumFieldElem, b::ZZRingElem) = isless(a, parent(a)(b))
isless(a::CalciumFieldElem, b::QQFieldElem) = isless(a, parent(a)(b))
isless(a::CalciumFieldElem, b::Int) = isless(a, parent(a)(b))
isless(a::QQBarFieldElem, b::CalciumFieldElem) = isless(parent(b)(a), b)
isless(a::QQFieldElem, b::CalciumFieldElem) = isless(parent(b)(a), b)
isless(a::ZZRingElem, b::CalciumFieldElem) = isless(parent(b)(a), b)
isless(a::Int, b::CalciumFieldElem) = isless(parent(b)(a), b)

@doc raw"""
    is_number(a::CalciumFieldElem)

Return whether `a` is a number, i.e. not an infinity or undefined.
"""
function is_number(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_number, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :is_number)
end

@doc raw"""
    iszero(a::CalciumFieldElem)

Return whether `a` is the number 0.
"""
function iszero(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_zero, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :iszero)
end

@doc raw"""
    isone(a::CalciumFieldElem)

Return whether `a` is the number 1.
"""
function isone(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_one, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :isone)
end

@doc raw"""
    is_algebraic(a::CalciumFieldElem)

Return whether `a` is an algebraic number.
"""
function is_algebraic(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_algebraic, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :is_algebraic)
end

@doc raw"""
    is_rational(a::CalciumFieldElem)

Return whether `a` is a rational number.
"""
function is_rational(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_rational, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :is_rational)
end

@doc raw"""
    isinteger(a::CalciumFieldElem)

Return whether `a` is an integer.
"""
function isinteger(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_integer, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :isinteger)
end

@doc raw"""
    isreal(a::CalciumFieldElem)

Return whether `a` is a real number. This returns `false`
if `a` is a pure real infinity.
"""
function isreal(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_real, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :isreal)
end

@doc raw"""
    is_imaginary(a::CalciumFieldElem)

Return whether `a` is an imaginary number. This returns `false`
if `a` is a pure imaginary infinity.
"""
function is_imaginary(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_imaginary, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :is_imaginary)
end

@doc raw"""
    is_undefined(a::CalciumFieldElem)

Return whether `a` is the special value *Undefined*.
"""
function is_undefined(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_undefined, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :is_undefined)
end

@doc raw"""
    isinf(a::CalciumFieldElem)

Return whether `a` is any infinity (signed or unsigned).
"""
function isinf(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_infinity, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :isinf)
end

@doc raw"""
    is_uinf(a::CalciumFieldElem)

Return whether `a` is unsigned infinity.
"""
function is_uinf(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_uinf, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :is_uinf)
end

@doc raw"""
    is_signed_inf(a::CalciumFieldElem)

Return whether `a` is any signed infinity.
"""
function is_signed_inf(a::CalciumFieldElem)
  C = a.parent
  t = ccall((:ca_check_is_signed_inf, libflint), Cint,
            (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C)
  return truth_as_bool(t, :is_signed_inf)
end

@doc raw"""
    is_unknown(a::CalciumFieldElem)

Return whether `a` is the special value *Unknown*. This is a representation
property and not a mathematical predicate.
"""
function is_unknown(a::CalciumFieldElem)
  C = a.parent
  t = Bool(ccall((:ca_is_unknown, libflint), Cint,
                 (Ref{CalciumFieldElem}, Ref{CalciumField}), a, C))
  return t
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_neg, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(a::CalciumFieldElem, b::CalciumFieldElem)
  a, b = same_parent(a, b)
  C = a.parent
  r = C()
  ccall((:ca_add, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function -(a::Int, b::CalciumFieldElem)
  C = b.parent
  r = C()
  ccall((:ca_si_sub, libflint), Nothing,
        (Ref{CalciumFieldElem}, Int, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function *(a::CalciumFieldElem, b::CalciumFieldElem)
  a, b = same_parent(a, b)
  C = a.parent
  r = C()
  ccall((:ca_mul, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

###############################################################################
#
#   Ad hoc binary operations
#
###############################################################################

function +(a::CalciumFieldElem, b::Int)
  C = a.parent
  r = C()
  ccall((:ca_add_si, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Int, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function +(a::CalciumFieldElem, b::ZZRingElem)
  C = a.parent
  r = C()
  ccall((:ca_add_fmpz, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{ZZRingElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function +(a::CalciumFieldElem, b::QQFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_add_fmpq, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{QQFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

+(a::CalciumFieldElem, b::QQBarFieldElem) = a + parent(a)(b)

+(a::Int, b::CalciumFieldElem) = b + a
+(a::ZZRingElem, b::CalciumFieldElem) = b + a
+(a::QQFieldElem, b::CalciumFieldElem) = b + a
+(a::QQBarFieldElem, b::CalciumFieldElem) = b + a

function -(a::CalciumFieldElem, b::CalciumFieldElem)
  a, b = same_parent(a, b)
  C = a.parent
  r = C()
  ccall((:ca_sub, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function -(a::CalciumFieldElem, b::Int)
  C = a.parent
  r = C()
  ccall((:ca_sub_si, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Int, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function -(a::CalciumFieldElem, b::ZZRingElem)
  C = a.parent
  r = C()
  ccall((:ca_sub_fmpz, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{ZZRingElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function -(a::CalciumFieldElem, b::QQFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_sub_fmpq, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{QQFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

-(a::CalciumFieldElem, b::QQBarFieldElem) = a - parent(a)(b)

function -(a::ZZRingElem, b::CalciumFieldElem)
  C = b.parent
  r = C()
  ccall((:ca_fmpz_sub, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{ZZRingElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function -(a::QQFieldElem, b::CalciumFieldElem)
  C = b.parent
  r = C()
  ccall((:ca_fmpq_sub, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{QQFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

-(a::QQBarFieldElem, b::CalciumFieldElem) = parent(b)(a) - b


function *(a::CalciumFieldElem, b::Int)
  C = a.parent
  r = C()
  ccall((:ca_mul_si, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Int, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function *(a::CalciumFieldElem, b::ZZRingElem)
  C = a.parent
  r = C()
  ccall((:ca_mul_fmpz, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{ZZRingElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function *(a::CalciumFieldElem, b::QQFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_mul_fmpq, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{QQFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

*(a::CalciumFieldElem, b::QQBarFieldElem) = a * parent(a)(b)

*(a::Int, b::CalciumFieldElem) = b * a
*(a::ZZRingElem, b::CalciumFieldElem) = b * a
*(a::QQFieldElem, b::CalciumFieldElem) = b * a
*(a::QQBarFieldElem, b::CalciumFieldElem) = b * a

###############################################################################
#
#   Division
#
###############################################################################

function //(a::CalciumFieldElem, b::CalciumFieldElem)
  a, b = same_parent(a, b)
  C = a.parent
  r = C()
  ccall((:ca_div, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

divexact(a::CalciumFieldElem, b::CalciumFieldElem; check::Bool=true) = a // b

function inv(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_inv, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

###############################################################################
#
#   Ad hoc division
#
###############################################################################

function //(a::CalciumFieldElem, b::Int)
  C = a.parent
  r = C()
  ccall((:ca_div_si, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Int, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function //(a::CalciumFieldElem, b::ZZRingElem)
  C = a.parent
  r = C()
  ccall((:ca_div_fmpz, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{ZZRingElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function //(a::CalciumFieldElem, b::QQFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_div_fmpq, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{QQFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

//(a::CalciumFieldElem, b::QQBarFieldElem) = a // parent(a)(b)

function //(a::Int, b::CalciumFieldElem)
  C = b.parent
  r = C()
  ccall((:ca_si_div, libflint), Nothing,
        (Ref{CalciumFieldElem}, Int, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function //(a::ZZRingElem, b::CalciumFieldElem)
  C = b.parent
  r = C()
  ccall((:ca_fmpz_div, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{ZZRingElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function //(a::QQFieldElem, b::CalciumFieldElem)
  C = b.parent
  r = C()
  ccall((:ca_fmpq_div, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{QQFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

//(a::QQBarFieldElem, b::CalciumFieldElem) = parent(b)(a) // b

divexact(a::CalciumFieldElem, b::Int; check::Bool=true) = a // b
divexact(a::CalciumFieldElem, b::ZZRingElem; check::Bool=true) = a // b
divexact(a::CalciumFieldElem, b::QQFieldElem; check::Bool=true) = a // b
divexact(a::CalciumFieldElem, b::QQBarFieldElem; check::Bool=true) = a // b
divexact(a::Int, b::CalciumFieldElem; check::Bool=true) = a // b
divexact(a::ZZRingElem, b::CalciumFieldElem; check::Bool=true) = a // b
divexact(a::QQFieldElem, b::CalciumFieldElem; check::Bool=true) = a // b
divexact(a::QQBarFieldElem, b::CalciumFieldElem; check::Bool=true) = a // b

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::CalciumFieldElem, b::CalciumFieldElem)
  a, b = same_parent(a, b)
  C = a.parent
  r = C()
  ccall((:ca_pow, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function ^(a::CalciumFieldElem, b::Int)
  C = a.parent
  r = C()
  ccall((:ca_pow_si, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Int, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function ^(a::CalciumFieldElem, b::ZZRingElem)
  C = a.parent
  r = C()
  ccall((:ca_pow_fmpz, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{ZZRingElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

function ^(a::CalciumFieldElem, b::QQFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_pow_fmpq, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{QQFieldElem}, Ref{CalciumField}), r, a, b, C)
  check_special(r)
  return r
end

^(a::CalciumFieldElem, b::QQBarFieldElem) = a ^ parent(a)(b)

^(a::Int, b::CalciumFieldElem) = parent(b)(a) ^ b
^(a::ZZRingElem, b::CalciumFieldElem) = parent(b)(a) ^ b
^(a::QQFieldElem, b::CalciumFieldElem) = parent(b)(a) ^ b
^(a::QQBarFieldElem, b::CalciumFieldElem) = parent(b)(a) ^ b


###############################################################################
#
#   Special values and constants
#
###############################################################################

@doc raw"""
    const_pi(C::CalciumField)

Return the constant $\pi$ as an element of `C`.
"""
function const_pi(C::CalciumField)
  r = C()
  ccall((:ca_pi, libflint), Nothing, (Ref{CalciumFieldElem}, Ref{CalciumField}), r, C)
  return r
end

@doc raw"""
    const_euler(C::CalciumField)

Return Euler's constant $\gamma$ as an element of `C`.
"""
function const_euler(C::CalciumField)
  r = C()
  ccall((:ca_euler, libflint), Nothing, (Ref{CalciumFieldElem}, Ref{CalciumField}), r, C)
  return r
end

@doc raw"""
    onei(C::CalciumField)

Return the imaginary unit $i$ as an element of `C`.
"""
function onei(C::CalciumField)
  r = C()
  ccall((:ca_i, libflint), Nothing, (Ref{CalciumFieldElem}, Ref{CalciumField}), r, C)
  return r
end

@doc raw"""
    unsigned_infinity(C::CalciumField)

Return unsigned infinity ($\hat \infty$) as an element of `C`.
This throws an exception if `C` does not allow special values.
"""
function unsigned_infinity(C::CalciumField)
  r = C()
  ccall((:ca_uinf, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumField}), r, C)
  check_special(r)
  return r
end

@doc raw"""
    infinity(C::CalciumField)

Return positive infinity ($+\infty$) as an element of `C`.
This throws an exception if `C` does not allow special values.
"""
function infinity(C::CalciumField)
  r = C()
  ccall((:ca_pos_inf, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumField}), r, C)
  check_special(r)
  return r
end

@doc raw"""
    infinity(a::CalciumFieldElem)

Return the signed infinity ($a \cdot \infty$).
This throws an exception if the parent of `a`
does not allow special values.
"""
function infinity(a::CalciumFieldElem)
  C = parent(a)
  r = C()
  ccall((:ca_pos_inf, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumField}), r, C)
  r *= a
  check_special(r)
  return r
end

@doc raw"""
    undefined(C::CalciumField)

Return the special value Undefined as an element of `C`.
This throws an exception if `C` does not allow special values.
"""
function undefined(C::CalciumField)
  r = C()
  ccall((:ca_undefined, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumField}), r, C)
  check_special(r)
  return r
end

@doc raw"""
    unknown(C::CalciumField)

Return the special meta-value Unknown as an element of `C`.
This throws an exception if `C` does not allow special values.
"""
function unknown(C::CalciumField)
  r = C()
  ccall((:ca_unknown, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumField}), r, C)
  check_special(r)
  return r
end

###############################################################################
#
#   Complex parts
#
###############################################################################

@doc raw"""
    real(a::CalciumFieldElem)

Return the real part of `a`.
"""
function real(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_re, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    imag(a::CalciumFieldElem)

Return the imaginary part of `a`.
"""
function imag(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_im, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    angle(a::CalciumFieldElem)

Return the complex argument of `a`.
"""
function angle(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_arg, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    csgn(a::CalciumFieldElem)

Return the extension of the real sign function taking the value 1
strictly in the right half plane, -1 strictly in the left half plane,
and the sign of the imaginary part when on the imaginary axis.
Equivalently, $\operatorname{csgn}(x) = x / \sqrt{x^2}$ except that the value is 0
at zero.
"""
function csgn(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_csgn, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    sign(a::CalciumFieldElem)

Return the complex sign of `a`, defined as zero if `a` is zero
and as $a / |a|$ for any other complex number. This function also
extracts the sign when `a` is a signed infinity.
"""
function sign(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_sgn, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    abs(a::CalciumFieldElem)

Return the absolute value of `a`.
"""
function abs(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_abs, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    conj(a::CalciumFieldElem; form::Symbol=:default)

Return the complex conjugate of `a`. The optional `form` argument allows
specifying the representation. In `:shallow` form, $\overline{a}$ is
introduced as a new extension number if it no straightforward
simplifications are possible.
In `:deep` form, complex conjugation is performed recursively.
"""
function conj(a::CalciumFieldElem; form::Symbol=:default)
  C = a.parent
  r = C()
  if form == :default
    ccall((:ca_conj, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :deep
    ccall((:ca_conj_deep, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :shallow
    ccall((:ca_conj_shallow, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  else
    error("unknown form: ", form)
  end
  check_special(r)
  return r
end

@doc raw"""
    floor(a::CalciumFieldElem)

Return the floor function of `a`.
"""
function floor(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_floor, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    ceil(a::CalciumFieldElem)

Return the ceiling function of `a`.
"""
function ceil(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_ceil, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

###############################################################################
#
#   Elementary functions
#
###############################################################################

@doc raw"""
    Base.sqrt(a::CalciumFieldElem; check::Bool=true)

Return the principal square root of `a`.
"""
function Base.sqrt(a::CalciumFieldElem; check::Bool=true)
  C = a.parent
  r = C()
  ccall((:ca_sqrt, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    exp(a::CalciumFieldElem)

Return the exponential function of `a`.
"""
function exp(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_exp, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    log(a::CalciumFieldElem)

Return the natural logarithm of `a`.
"""
function log(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_log, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    pow(a::CalciumFieldElem, b::Int; form::Symbol=:default)

Return *a* raised to the integer power `b`. The optional `form` argument allows
specifying the representation. In `:default` form, this is equivalent
to `a ^ b`, which may create a new extension number $a^b$ if the exponent `b`
is too large (as determined by the parent option `:pow_limit` or `:prec_limit`
depending on the case). In `:arithmetic` form, the exponentiation is
performed arithmetically in the field of `a`, regardless of the size
of the exponent `b`.
"""
function pow(a::CalciumFieldElem, b::Int; form::Symbol=:default)
  C = a.parent
  r = C()
  if form == :default
    ccall((:ca_pow_si, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Int, Ref{CalciumField}), r, a, b, C)
  elseif form == :arithmetic
    ccall((:ca_pow_si_arithmetic, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Int, Ref{CalciumField}), r, a, b, C)
  else
    error("unknown form: ", form)
  end
  check_special(r)
  return r
end

@doc raw"""
    sin(a::CalciumFieldElem; form::Symbol=:default)

Return the sine of `a`.
The optional `form` argument allows specifying the representation.
In `:default` form, the result is determined by the `:trig_form` option
of the parent object. In `:exponential` form, the value is represented
using complex exponentials. In `:tangent` form, the value is represented
using tangents. In `:direct` form, the value is represented directly
using a sine or cosine.
"""
function sin(a::CalciumFieldElem; form::Symbol=:default)
  C = a.parent
  r = C()
  if form == :default
    ccall((:ca_sin, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :exponential
    ccall((:ca_sin_cos_exponential, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ptr{Nothing}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, C_NULL, a, C)
  elseif form == :tangent
    ccall((:ca_sin_cos_tangent, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ptr{Nothing}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, C_NULL, a, C)
  elseif form == :direct
    ccall((:ca_sin_cos_direct, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ptr{Nothing}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, C_NULL, a, C)
  else
    error("unknown form: ", form)
  end
  check_special(r)
  return r
end

@doc raw"""
    cos(a::CalciumFieldElem; form::Symbol=:default)

Return the cosine of `a`.
The optional `form` argument allows specifying the representation.
In `:default` form, the result is determined by the `:trig_form` option
of the parent object. In `:exponential` form, the value is represented
using complex exponentials. In `:tangent` form, the value is represented
using tangents. In `:direct` form, the value is represented directly
using a sine or cosine.
"""
function cos(a::CalciumFieldElem; form::Symbol=:default)
  C = a.parent
  r = C()
  if form == :default
    ccall((:ca_cos, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :exponential
    ccall((:ca_sin_cos_exponential, libflint), Nothing,
          (Ptr{Nothing}, Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), C_NULL, r, a, C)
  elseif form == :tangent
    ccall((:ca_sin_cos_tangent, libflint), Nothing,
          (Ptr{Nothing}, Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), C_NULL, r, a, C)
  elseif form == :direct || form == :sine_cosine
    ccall((:ca_sin_cos_direct, libflint), Nothing,
          (Ptr{Nothing}, Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), C_NULL, r, a, C)
  else
    error("unknown form: ", form)
  end
  check_special(r)
  return r
end

@doc raw"""
    tan(a::CalciumFieldElem; form::Symbol=:default)

Return the tangent of `a`.
The optional `form` argument allows specifying the representation.
In `:default` form, the result is determined by the `:trig_form` option
of the parent object. In `:exponential` form, the value is represented
using complex exponentials. In `:direct` or `:tangent` form, the value is
represented directly using tangents. In `:sine_cosine` form, the value is
represented using sines or cosines.
"""
function tan(a::CalciumFieldElem; form::Symbol=:default)
  C = a.parent
  r = C()
  if form == :default
    ccall((:ca_tan, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :exponential
    ccall((:ca_tan_exponential, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :direct || form == :tangent
    ccall((:ca_tan_direct, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :sine_cosine
    ccall((:ca_tan_sine_cosine, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  else
    error("unknown form: ", form)
  end
  check_special(r)
  return r
end

@doc raw"""
    atan(a::CalciumFieldElem; form::Symbol=:default)

Return the inverse tangent of `a`.
The optional `form` argument allows specifying the representation.
In `:default` form, the result is determined by the `:trig_form` option
of the parent object. In `:logarithm` form, the value is represented
using complex logarithms. In `:direct` or `:arctangent` form, the value is
represented directly using arctangents.
"""
function atan(a::CalciumFieldElem; form::Symbol=:default)
  C = a.parent
  r = C()
  if form == :default
    ccall((:ca_atan, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :logarithm
    ccall((:ca_atan_logarithm, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :direct || form == :arctangent
    ccall((:ca_atan_direct, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  else
    error("unknown form: ", form)
  end
  check_special(r)
  return r
end

@doc raw"""
    asin(a::CalciumFieldElem; form::Symbol=:default)

Return the inverse sine of `a`.
The optional `form` argument allows specifying the representation.
In `:default` form, the result is determined by the `:trig_form` option
of the parent object. In `:logarithm` form, the value is represented
using complex logarithms. In `:direct` form, the value is
represented directly using an inverse sine or cosine.
"""
function asin(a::CalciumFieldElem; form::Symbol=:default)
  C = a.parent
  r = C()
  if form == :default
    ccall((:ca_asin, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :logarithm
    ccall((:ca_asin_logarithm, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :direct
    ccall((:ca_asin_direct, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  else
    error("unknown form: ", form)
  end
  check_special(r)
  return r
end

@doc raw"""
    acos(a::CalciumFieldElem; form::Symbol=:default)

Return the inverse cosine of `a`.
The optional `form` argument allows specifying the representation.
In `:default` form, the result is determined by the `:trig_form` option
of the parent object. In `:logarithm` form, the value is represented
using complex logarithms. In `:direct` form, the value is
represented directly using an inverse sine or cosine.
"""
function acos(a::CalciumFieldElem; form::Symbol=:default)
  C = a.parent
  r = C()
  if form == :default
    ccall((:ca_acos, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :logarithm
    ccall((:ca_acos_logarithm, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  elseif form == :direct
    ccall((:ca_acos_direct, libflint), Nothing,
          (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  else
    error("unknown form: ", form)
  end
  check_special(r)
  return r
end

@doc raw"""
    gamma(a::CalciumFieldElem)

Return the gamma function of `a`.
"""
function gamma(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_gamma, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    erf(a::CalciumFieldElem)

Return the error function of `a`.
"""
function erf(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_erf, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    erfi(a::CalciumFieldElem)

Return the imaginary error function of `a`.
"""
function erfi(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_erfi, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

@doc raw"""
    erfc(a::CalciumFieldElem)

Return the complementary error function of `a`.
"""
function erfc(a::CalciumFieldElem)
  C = a.parent
  r = C()
  ccall((:ca_erfc, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), r, a, C)
  check_special(r)
  return r
end

###############################################################################
#
#   Rewriting and normal forms
#
###############################################################################

@doc raw"""
    complex_normal_form(a::CalciumFieldElem, deep::Bool=true)

Returns the input rewritten using standardizing transformations over the
complex numbers:

* Elementary functions are rewritten in terms of exponentials, roots
  and logarithms.

* Complex parts are rewritten using logarithms, square roots, and (deep)
  complex conjugates.

* Algebraic numbers are rewritten in terms of cyclotomic fields where
  applicable.

If deep is set, the rewriting is applied recursively to the tower of
extension numbers; otherwise, the rewriting is only applied to the
top-level extension numbers.

The result is not a normal form in the strong sense (the same number
can have many possible representations even after applying this
transformation), but this transformation can nevertheless be a useful
heuristic for simplification.
"""
function complex_normal_form(a::CalciumFieldElem; deep::Bool=true)
  C = a.parent
  r = C()
  ccall((:ca_rewrite_complex_normal_form, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Cint, Ref{CalciumField}), r, a, deep, C)
  check_special(r)
  return r
end

###############################################################################
#
#   Conversions
#
###############################################################################

function QQFieldElem(a::CalciumFieldElem)
  C = a.parent
  res = QQFieldElem()
  ok = Bool(ccall((:ca_get_fmpq, libflint), Cint,
                  (Ref{QQFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), res, a, C))
  !ok && error("unable to convert to a rational number")
  return res
end

function ZZRingElem(a::CalciumFieldElem)
  C = a.parent
  res = ZZRingElem()
  ok = Bool(ccall((:ca_get_fmpz, libflint), Cint,
                  (Ref{ZZRingElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), res, a, C))
  !ok && error("unable to convert to an integer")
  return res
end

function QQBarFieldElem(a::CalciumFieldElem)
  C = a.parent
  res = QQBarFieldElem()
  ok = Bool(ccall((:ca_get_qqbar, libflint), Cint,
                  (Ref{QQBarFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), res, a, C))
  !ok && error("unable to convert to an algebraic number")
  return res
end

(R::QQField)(a::CalciumFieldElem) = QQFieldElem(a)
(R::ZZRing)(a::CalciumFieldElem) = ZZRingElem(a)
(R::QQBarField)(a::CalciumFieldElem) = QQBarFieldElem(a)

function (R::AcbField)(a::CalciumFieldElem; parts::Bool=false)
  C = a.parent
  prec = precision(R)
  z = R()
  if parts
    ccall((:ca_get_acb_accurate_parts, libflint),
          Nothing, (Ref{AcbFieldElem}, Ref{CalciumFieldElem}, Int, Ref{CalciumField}), z, a, prec, C)
  else
    ccall((:ca_get_acb, libflint),
          Nothing, (Ref{AcbFieldElem}, Ref{CalciumFieldElem}, Int, Ref{CalciumField}), z, a, prec, C)
  end
  return z
end

function (R::ArbField)(a::CalciumFieldElem; check::Bool=true)
  C = a.parent
  prec = precision(R)
  if check
    z = AcbField(prec)(a, parts=true)
    if isreal(z)
      return real(z)
    else
      error("unable to convert to a real number")
    end
  else
    z = AcbField(prec)(a, parts=false)
    if accuracy_bits(real(z)) < prec - 5
      z = AcbField()(a, parts=true)
    end
    return real(z)
  end
end

function (::Type{ComplexF64})(x::CalciumFieldElem)
  z = AcbField(53, cached = false)(x)
  x = ArbFieldElem()
  ccall((:acb_get_real, libflint), Nothing, (Ref{ArbFieldElem}, Ref{AcbFieldElem}), x, z)
  xx = Float64(x)
  y = ArbFieldElem()
  ccall((:acb_get_imag, libflint), Nothing, (Ref{ArbFieldElem}, Ref{AcbFieldElem}), y, z)
  yy = Float64(y)
  return ComplexF64(xx, yy)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::CalciumFieldElem)
  C = z.parent
  ccall((:ca_zero, libflint), Nothing, (Ref{CalciumFieldElem}, Ref{CalciumField}), z, C)
  return z
end

function mul!(z::CalciumFieldElem, x::CalciumFieldElem, y::CalciumFieldElem)
  if z.parent != x.parent || x.parent != y.parent
    error("different parents in in-place operation")
  end
  C = z.parent
  ccall((:ca_mul, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), z, x, y, C)
  check_special(z)
  return z
end

function addeq!(z::CalciumFieldElem, x::CalciumFieldElem)
  if z.parent != x.parent
    error("different parents in in-place operation")
  end
  C = z.parent
  ccall((:ca_add, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), z, z, x, C)
  check_special(z)
  return z
end

function add!(z::CalciumFieldElem, x::CalciumFieldElem, y::CalciumFieldElem)
  if z.parent != x.parent || x.parent != y.parent
    error("different parents in in-place operation")
  end
  C = z.parent
  ccall((:ca_add, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumFieldElem}, Ref{CalciumField}), z, x, y, C)
  check_special(z)
  return z
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (C::CalciumField)()
  z = CalciumFieldElem(C)
  return z
end

function (C::CalciumField)(v::CalciumFieldElem)
  D = v.parent
  if C == D
    return v
  end
  r = C()
  ccall((:ca_transfer, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{CalciumField}, Ref{CalciumFieldElem}, Ref{CalciumField}),
        r, C, v, D)
  check_special(r)
  return r
end

function (C::CalciumField)(v::Int)
  z = CalciumFieldElem(C)
  ccall((:ca_set_si, libflint), Nothing,
        (Ref{CalciumFieldElem}, Int, Ref{CalciumField}), z, v, C)
  return z
end

function (C::CalciumField)(v::ZZRingElem)
  z = CalciumFieldElem(C)
  ccall((:ca_set_fmpz, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{ZZRingElem}, Ref{CalciumField}), z, v, C)
  return z
end

function (C::CalciumField)(v::QQFieldElem)
  z = CalciumFieldElem(C)
  ccall((:ca_set_fmpq, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{QQFieldElem}, Ref{CalciumField}), z, v, C)
  return z
end

function (C::CalciumField)(v::QQBarFieldElem)
  z = CalciumFieldElem(C)
  ccall((:ca_set_qqbar, libflint), Nothing,
        (Ref{CalciumFieldElem}, Ref{QQBarFieldElem}, Ref{CalciumField}), z, v, C)
  return z
end

# todo: optimize
function (C::CalciumField)(v::Complex{Int})
  return C(QQBarFieldElem(v))
end

function (C::CalciumField)(x::Irrational)
  if x == pi
    return const_pi(C)
  elseif x == MathConstants.eulergamma
    return const_euler(C)
  else
    error("constant not supported")
  end
end

