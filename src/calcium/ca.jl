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

characteristic(a::CalciumField) = 0

function deepcopy_internal(a::CalciumFieldElem, dict::IdDict)
  C = a.parent
  r = C()
  @ccall libflint.ca_set(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  return r
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
    @ccall libflint.ca_transfer(r::Ref{CalciumFieldElem}, a.parent::Ref{CalciumField}, b::Ref{CalciumFieldElem}, b.parent::Ref{CalciumField})::Nothing
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
  cstr = @ccall libflint.ca_get_str(x::Ref{CalciumFieldElem}, x.parent::Ref{CalciumField})::Ptr{UInt8}
  res = unsafe_string(cstr)
  @ccall libflint.flint_free(cstr::Ptr{UInt8})::Nothing

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

one(C::CalciumField) = one!(CalciumFieldElem(C))

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
    @ccall libflint.ca_randtest(x::Ref{CalciumFieldElem}, state::Ref{rand_ctx}, depth::Int, bits::Int, C::Ref{CalciumField})::Nothing
  elseif randtype == :rational
    @ccall libflint.ca_randtest_rational(x::Ref{CalciumFieldElem}, state::Ref{rand_ctx}, bits::Int, C::Ref{CalciumField})::Nothing
  elseif randtype == :special
    @ccall libflint.ca_randtest_special(x::Ref{CalciumFieldElem}, state::Ref{rand_ctx}, depth::Int, bits::Int, C::Ref{CalciumField})::Nothing
  else
    error("randtype not defined")
  end

  check_special(x)
  return x
end

###############################################################################
#
#   Conformance test element generation
#
###############################################################################

function ConformanceTests.generate_element(R::CalciumField)
  return rand(R, depth=5, bits=5)
end

###############################################################################
#
#   Comparison and predicates
#
###############################################################################

function ==(a::CalciumFieldElem, b::CalciumFieldElem)
  a, b = same_parent(a, b)
  C = a.parent
  t = @ccall libflint.ca_check_equal(a::Ref{CalciumFieldElem}, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :isequal)
end

function isless(a::CalciumFieldElem, b::CalciumFieldElem)
  a, b = same_parent(a, b)
  C = a.parent
  t = @ccall libflint.ca_check_lt(a::Ref{CalciumFieldElem}, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
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
  t = @ccall libflint.ca_check_is_number(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :is_number)
end

@doc raw"""
    iszero(a::CalciumFieldElem)

Return whether `a` is the number 0.
"""
function iszero(a::CalciumFieldElem)
  C = a.parent
  t = @ccall libflint.ca_check_is_zero(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :iszero)
end

@doc raw"""
    isone(a::CalciumFieldElem)

Return whether `a` is the number 1.
"""
function isone(a::CalciumFieldElem)
  C = a.parent
  t = @ccall libflint.ca_check_is_one(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :isone)
end

@doc raw"""
    is_algebraic(a::CalciumFieldElem)

Return whether `a` is an algebraic number.
"""
function is_algebraic(a::CalciumFieldElem)
  C = a.parent
  t = @ccall libflint.ca_check_is_algebraic(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :is_algebraic)
end

@doc raw"""
    is_rational(a::CalciumFieldElem)

Return whether `a` is a rational number.
"""
function is_rational(a::CalciumFieldElem)
  C = a.parent
  t = @ccall libflint.ca_check_is_rational(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :is_rational)
end

@doc raw"""
    isinteger(a::CalciumFieldElem)

Return whether `a` is an integer.
"""
function isinteger(a::CalciumFieldElem)
  C = a.parent
  t = @ccall libflint.ca_check_is_integer(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :isinteger)
end

@doc raw"""
    isreal(a::CalciumFieldElem)

Return whether `a` is a real number. This returns `false`
if `a` is a pure real infinity.
"""
function isreal(a::CalciumFieldElem)
  C = a.parent
  t = @ccall libflint.ca_check_is_real(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :isreal)
end

@doc raw"""
    is_imaginary(a::CalciumFieldElem)

Return whether `a` is an imaginary number. This returns `false`
if `a` is a pure imaginary infinity.
"""
function is_imaginary(a::CalciumFieldElem)
  C = a.parent
  t = @ccall libflint.ca_check_is_imaginary(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :is_imaginary)
end

@doc raw"""
    is_undefined(a::CalciumFieldElem)

Return whether `a` is the special value *Undefined*.
"""
function is_undefined(a::CalciumFieldElem)
  C = a.parent
  t = @ccall libflint.ca_check_is_undefined(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :is_undefined)
end

@doc raw"""
    isinf(a::CalciumFieldElem)

Return whether `a` is any infinity (signed or unsigned).
"""
function isinf(a::CalciumFieldElem)
  C = a.parent
  t = @ccall libflint.ca_check_is_infinity(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :isinf)
end

@doc raw"""
    is_uinf(a::CalciumFieldElem)

Return whether `a` is unsigned infinity.
"""
function is_uinf(a::CalciumFieldElem)
  C = a.parent
  t = @ccall libflint.ca_check_is_uinf(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :is_uinf)
end

@doc raw"""
    is_signed_inf(a::CalciumFieldElem)

Return whether `a` is any signed infinity.
"""
function is_signed_inf(a::CalciumFieldElem)
  C = a.parent
  t = @ccall libflint.ca_check_is_signed_inf(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint
  return truth_as_bool(t, :is_signed_inf)
end

@doc raw"""
    is_unknown(a::CalciumFieldElem)

Return whether `a` is the special value *Unknown*. This is a representation
property and not a mathematical predicate.
"""
function is_unknown(a::CalciumFieldElem)
  C = a.parent
  t = Bool(@ccall libflint.ca_is_unknown(a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint)
  return t
end

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(a::CalciumFieldElem)
  r = neg!(a.parent(), a)
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
  @ccall libflint.ca_add(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function -(a::Int, b::CalciumFieldElem)
  C = b.parent
  r = C()
  @ccall libflint.ca_si_sub(r::Ref{CalciumFieldElem}, a::Int, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function *(a::CalciumFieldElem, b::CalciumFieldElem)
  a, b = same_parent(a, b)
  C = a.parent
  r = C()
  @ccall libflint.ca_mul(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_add_si(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Int, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function +(a::CalciumFieldElem, b::ZZRingElem)
  C = a.parent
  r = C()
  @ccall libflint.ca_add_fmpz(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{ZZRingElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function +(a::CalciumFieldElem, b::QQFieldElem)
  C = a.parent
  r = C()
  @ccall libflint.ca_add_fmpq(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{QQFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_sub(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function -(a::CalciumFieldElem, b::Int)
  C = a.parent
  r = C()
  @ccall libflint.ca_sub_si(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Int, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function -(a::CalciumFieldElem, b::ZZRingElem)
  C = a.parent
  r = C()
  @ccall libflint.ca_sub_fmpz(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{ZZRingElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function -(a::CalciumFieldElem, b::QQFieldElem)
  C = a.parent
  r = C()
  @ccall libflint.ca_sub_fmpq(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{QQFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

-(a::CalciumFieldElem, b::QQBarFieldElem) = a - parent(a)(b)

function -(a::ZZRingElem, b::CalciumFieldElem)
  C = b.parent
  r = C()
  @ccall libflint.ca_fmpz_sub(r::Ref{CalciumFieldElem}, a::Ref{ZZRingElem}, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function -(a::QQFieldElem, b::CalciumFieldElem)
  C = b.parent
  r = C()
  @ccall libflint.ca_fmpq_sub(r::Ref{CalciumFieldElem}, a::Ref{QQFieldElem}, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

-(a::QQBarFieldElem, b::CalciumFieldElem) = parent(b)(a) - b


function *(a::CalciumFieldElem, b::Int)
  C = a.parent
  r = C()
  @ccall libflint.ca_mul_si(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Int, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function *(a::CalciumFieldElem, b::ZZRingElem)
  C = a.parent
  r = C()
  @ccall libflint.ca_mul_fmpz(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{ZZRingElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function *(a::CalciumFieldElem, b::QQFieldElem)
  C = a.parent
  r = C()
  @ccall libflint.ca_mul_fmpq(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{QQFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_div(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

divexact(a::CalciumFieldElem, b::CalciumFieldElem; check::Bool=true) = a // b

function inv(a::CalciumFieldElem)
  C = a.parent
  r = C()
  @ccall libflint.ca_inv(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_div_si(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Int, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function //(a::CalciumFieldElem, b::ZZRingElem)
  C = a.parent
  r = C()
  @ccall libflint.ca_div_fmpz(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{ZZRingElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function //(a::CalciumFieldElem, b::QQFieldElem)
  C = a.parent
  r = C()
  @ccall libflint.ca_div_fmpq(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{QQFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

//(a::CalciumFieldElem, b::QQBarFieldElem) = a // parent(a)(b)

function //(a::Int, b::CalciumFieldElem)
  C = b.parent
  r = C()
  @ccall libflint.ca_si_div(r::Ref{CalciumFieldElem}, a::Int, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function //(a::ZZRingElem, b::CalciumFieldElem)
  C = b.parent
  r = C()
  @ccall libflint.ca_fmpz_div(r::Ref{CalciumFieldElem}, a::Ref{ZZRingElem}, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function //(a::QQFieldElem, b::CalciumFieldElem)
  C = b.parent
  r = C()
  @ccall libflint.ca_fmpq_div(r::Ref{CalciumFieldElem}, a::Ref{QQFieldElem}, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_pow(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function ^(a::CalciumFieldElem, b::Int)
  C = a.parent
  r = C()
  @ccall libflint.ca_pow_si(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Int, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function ^(a::CalciumFieldElem, b::ZZRingElem)
  C = a.parent
  r = C()
  @ccall libflint.ca_pow_fmpz(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{ZZRingElem}, C::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function ^(a::CalciumFieldElem, b::QQFieldElem)
  C = a.parent
  r = C()
  @ccall libflint.ca_pow_fmpq(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Ref{QQFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_pi(r::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  return r
end

@doc raw"""
    const_euler(C::CalciumField)

Return Euler's constant $\gamma$ as an element of `C`.
"""
function const_euler(C::CalciumField)
  r = C()
  @ccall libflint.ca_euler(r::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  return r
end

@doc raw"""
    onei(C::CalciumField)

Return the imaginary unit $i$ as an element of `C`.
"""
function onei(C::CalciumField)
  r = C()
  @ccall libflint.ca_i(r::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  return r
end

@doc raw"""
    unsigned_infinity(C::CalciumField)

Return unsigned infinity ($\hat \infty$) as an element of `C`.
This throws an exception if `C` does not allow special values.
"""
function unsigned_infinity(C::CalciumField)
  r = C()
  @ccall libflint.ca_uinf(r::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_pos_inf(r::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_pos_inf(r::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_undefined(r::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_unknown(r::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_re(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_im(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_arg(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_csgn(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_sgn(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_abs(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
    @ccall libflint.ca_conj(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :deep
    @ccall libflint.ca_conj_deep(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :shallow
    @ccall libflint.ca_conj_shallow(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_floor(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_ceil(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_sqrt(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_exp(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_log(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
    @ccall libflint.ca_pow_si(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Int, C::Ref{CalciumField})::Nothing
  elseif form == :arithmetic
    @ccall libflint.ca_pow_si_arithmetic(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, b::Int, C::Ref{CalciumField})::Nothing
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
    @ccall libflint.ca_sin(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :exponential
    @ccall libflint.ca_sin_cos_exponential(r::Ref{CalciumFieldElem}, C_NULL::Ptr{Nothing}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :tangent
    @ccall libflint.ca_sin_cos_tangent(r::Ref{CalciumFieldElem}, C_NULL::Ptr{Nothing}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :direct
    @ccall libflint.ca_sin_cos_direct(r::Ref{CalciumFieldElem}, C_NULL::Ptr{Nothing}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
    @ccall libflint.ca_cos(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :exponential
    @ccall libflint.ca_sin_cos_exponential(C_NULL::Ptr{Nothing}, r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :tangent
    @ccall libflint.ca_sin_cos_tangent(C_NULL::Ptr{Nothing}, r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :direct || form == :sine_cosine
    @ccall libflint.ca_sin_cos_direct(C_NULL::Ptr{Nothing}, r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
    @ccall libflint.ca_tan(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :exponential
    @ccall libflint.ca_tan_exponential(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :direct || form == :tangent
    @ccall libflint.ca_tan_direct(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :sine_cosine
    @ccall libflint.ca_tan_sine_cosine(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
    @ccall libflint.ca_atan(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :logarithm
    @ccall libflint.ca_atan_logarithm(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :direct || form == :arctangent
    @ccall libflint.ca_atan_direct(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
    @ccall libflint.ca_asin(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :logarithm
    @ccall libflint.ca_asin_logarithm(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :direct
    @ccall libflint.ca_asin_direct(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
    @ccall libflint.ca_acos(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :logarithm
    @ccall libflint.ca_acos_logarithm(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  elseif form == :direct
    @ccall libflint.ca_acos_direct(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_gamma(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_erf(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_erfi(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_erfc(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_rewrite_complex_normal_form(r::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, deep::Cint, C::Ref{CalciumField})::Nothing
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
  ok = Bool(@ccall libflint.ca_get_fmpq(res::Ref{QQFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint)
  !ok && error("unable to convert to a rational number")
  return res
end

function ZZRingElem(a::CalciumFieldElem)
  C = a.parent
  res = ZZRingElem()
  ok = Bool(@ccall libflint.ca_get_fmpz(res::Ref{ZZRingElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint)
  !ok && error("unable to convert to an integer")
  return res
end

function QQBarFieldElem(a::CalciumFieldElem)
  C = a.parent
  res = QQBarFieldElem()
  ok = Bool(@ccall libflint.ca_get_qqbar(res::Ref{QQBarFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Cint)
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
    @ccall libflint.ca_get_acb_accurate_parts(z::Ref{AcbFieldElem}, a::Ref{CalciumFieldElem}, prec::Int, C::Ref{CalciumField})::Nothing
  else
    @ccall libflint.ca_get_acb(z::Ref{AcbFieldElem}, a::Ref{CalciumFieldElem}, prec::Int, C::Ref{CalciumField})::Nothing
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
  x = real(z)
  xx = Float64(x)
  y = imag(z)
  yy = Float64(y)
  return ComplexF64(xx, yy)
end

function (::Type{Float64})(a::CalciumFieldElem)
  isreal(a) || throw(InexactError(:Float64, Float64, a))
  x = ArbField(53, cached = false)(a)
  return Float64(x)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::CalciumFieldElem)
  C = z.parent
  @ccall libflint.ca_zero(z::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  return z
end

function one!(z::CalciumFieldElem)
  C = z.parent
  @ccall libflint.ca_one(z::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  return z
end

function neg!(z::CalciumFieldElem, a::CalciumFieldElem)
  C = z.parent
  @ccall libflint.ca_neg(z::Ref{CalciumFieldElem}, a::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  return z
end

function mul!(z::CalciumFieldElem, x::CalciumFieldElem, y::CalciumFieldElem)
  if z.parent != x.parent || x.parent != y.parent
    error("different parents in in-place operation")
  end
  C = z.parent
  @ccall libflint.ca_mul(z::Ref{CalciumFieldElem}, x::Ref{CalciumFieldElem}, y::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
  check_special(z)
  return z
end

function add!(z::CalciumFieldElem, x::CalciumFieldElem, y::CalciumFieldElem)
  if z.parent != x.parent || x.parent != y.parent
    error("different parents in in-place operation")
  end
  C = z.parent
  @ccall libflint.ca_add(z::Ref{CalciumFieldElem}, x::Ref{CalciumFieldElem}, y::Ref{CalciumFieldElem}, C::Ref{CalciumField})::Nothing
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
  @ccall libflint.ca_transfer(r::Ref{CalciumFieldElem}, C::Ref{CalciumField}, v::Ref{CalciumFieldElem}, D::Ref{CalciumField})::Nothing
  check_special(r)
  return r
end

function (C::CalciumField)(v::Int)
  z = CalciumFieldElem(C)
  @ccall libflint.ca_set_si(z::Ref{CalciumFieldElem}, v::Int, C::Ref{CalciumField})::Nothing
  return z
end

function (C::CalciumField)(v::ZZRingElem)
  z = CalciumFieldElem(C)
  @ccall libflint.ca_set_fmpz(z::Ref{CalciumFieldElem}, v::Ref{ZZRingElem}, C::Ref{CalciumField})::Nothing
  return z
end

function (C::CalciumField)(v::QQFieldElem)
  z = CalciumFieldElem(C)
  @ccall libflint.ca_set_fmpq(z::Ref{CalciumFieldElem}, v::Ref{QQFieldElem}, C::Ref{CalciumField})::Nothing
  return z
end

function (C::CalciumField)(v::QQBarFieldElem)
  z = CalciumFieldElem(C)
  @ccall libflint.ca_set_qqbar(z::Ref{CalciumFieldElem}, v::Ref{QQBarFieldElem}, C::Ref{CalciumField})::Nothing
  return z
end

function (C::CalciumField)(v::RationalUnion)
  return C(flintify(v))
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

