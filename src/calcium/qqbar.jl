###############################################################################
#
#   qqbar.jl : Calcium algebraic numbers in minimal polynomial representation
#
###############################################################################

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent(a::QQBarFieldElem) = QQBarField()

parent_type(::Type{QQBarFieldElem}) = QQBarField

elem_type(::Type{QQBarField}) = QQBarFieldElem

base_ring_type(::Type{QQBarField}) = Union{}

is_domain_type(::Type{QQBarFieldElem}) = true

characteristic(::QQBarField) = 0

###############################################################################
#
#   Hashing
#
###############################################################################

# todo: want a C function for this
function Base.hash(a::QQBarFieldElem, h::UInt)
  R, x = polynomial_ring(ZZ, "x")
  return xor(hash(minpoly(R, a)), h)
end

###############################################################################
#
#   Constructors
#
###############################################################################

function QQBarFieldElem(a::Int)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_set_si(z::Ref{QQBarFieldElem}, a::Int)::Nothing
  return z
end

function QQBarFieldElem(a::Complex)
  r = QQBarFieldElem(real(a))
  s = QQBarFieldElem(imag(a))
  z = QQBarFieldElem()
  @ccall libflint.qqbar_set_re_im(z::Ref{QQBarFieldElem}, r::Ref{QQBarFieldElem}, s::Ref{QQBarFieldElem})::Nothing
  return z
end

function QQBarFieldElem(a::ZZRingElem)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_set_fmpz(z::Ref{QQBarFieldElem}, a::Ref{ZZRingElem})::Nothing
  return z
end

function QQBarFieldElem(a::QQFieldElem)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_set_fmpq(z::Ref{QQBarFieldElem}, a::Ref{QQFieldElem})::Nothing
  return z
end

QQBarFieldElem(a::Rational) = QQBarFieldElem(QQFieldElem(a))

QQBarFieldElem(a::Integer) = QQBarFieldElem(ZZRingElem(a))

function deepcopy_internal(a::QQBarFieldElem, dict::IdDict)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_set(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::QQBarFieldElem; context = nothing)
  R, _ = polynomial_ring(ZZ, :x; cached=false)
  f = minpoly(R, a)
  fl = context isa IOContext && get(context, :compact, false)
  # use 6 digits normally and 3 in matrices and julia Array
  prec = fl ? 3 : 6
  return Expr(:list, Expr(:sequence, Expr(:text, "a$(degree(f)): "), Expr(:text, _native_string(a, prec))))
end

function AbstractAlgebra.show_via_expressify(io::IO, mi::MIME, obj::QQBarFieldElem; context = nothing)
   AbstractAlgebra.PrettyPrinting.show_obj(io, mi, AbstractAlgebra.PrettyPrinting.canonicalize(expressify(obj, context = io)))
end

@enable_all_show_via_expressify QQBarFieldElem

function _native_string(x::QQBarFieldElem, prec = 6)
  cstr = @ccall libflint.qqbar_get_str_nd(x::Ref{QQBarFieldElem}, Int(prec)::Int)::Ptr{UInt8}
  number = unsafe_string(cstr)
  @ccall libflint.flint_free(cstr::Ptr{UInt8})::Nothing

  number = number[1:first(findfirst(" (", number)::UnitRange)-1]
  number = replace(number, "I" => "im")
  return number
end

function native_string(x::QQBarFieldElem)
  return sprint(show, x)
end

function show(io::IO, F::QQBarField)
  # deliberately no @show_name or @show_special here as this is a singleton type
  io = pretty(io)
  if is_terse(io)
    print(io, LowercaseOff(), "QQBar")
  else
    print(io, "Algebraic closure of ", Lowercase(), QQ)
  end
end


###############################################################################
#
#   Basic manipulation
#
###############################################################################


zero(a::QQBarField) = a(0)

one(a::QQBarField) = a(1)

zero(::Type{QQBarFieldElem}) = QQBarFieldElem(0)

one(::Type{QQBarFieldElem}) = QQBarFieldElem(1)

@doc raw"""
    degree(x::QQBarFieldElem)

Return the degree of the minimal polynomial of `x`.
"""
function degree(x::QQBarFieldElem)
  return @ccall libflint.qqbar_degree(x::Ref{QQBarFieldElem})::Int
end

@doc raw"""
    iszero(x::QQBarFieldElem)

Return whether `x` is the number 0.
"""
function iszero(x::QQBarFieldElem)
  return Bool(@ccall libflint.qqbar_is_zero(x::Ref{QQBarFieldElem})::Cint)
end

@doc raw"""
    isone(x::QQBarFieldElem)

Return whether `x` is the number 1.
"""
function isone(x::QQBarFieldElem)
  return Bool(@ccall libflint.qqbar_is_one(x::Ref{QQBarFieldElem})::Cint)
end

@doc raw"""
    isinteger(x::QQBarFieldElem)

Return whether `x` is an integer.
"""
function isinteger(x::QQBarFieldElem)
  return Bool(@ccall libflint.qqbar_is_integer(x::Ref{QQBarFieldElem})::Cint)
end

@doc raw"""
    is_rational(x::QQBarFieldElem)

Return whether `x` is a rational number.
"""
function is_rational(x::QQBarFieldElem)
  return Bool(@ccall libflint.qqbar_is_rational(x::Ref{QQBarFieldElem})::Cint)
end

@doc raw"""
    isreal(x::QQBarFieldElem)

Return whether `x` is a real number.
"""
function isreal(x::QQBarFieldElem)
  return Bool(@ccall libflint.qqbar_is_real(x::Ref{QQBarFieldElem})::Cint)
end

@doc raw"""
    is_algebraic_integer(x::QQBarFieldElem)

Return whether `x` is an algebraic integer.
"""
function is_algebraic_integer(x::QQBarFieldElem)
  return Bool(@ccall libflint.qqbar_is_algebraic_integer(x::Ref{QQBarFieldElem})::Cint)
end

@doc raw"""
    minpoly(R::ZZPolyRing, x::QQBarFieldElem)

Return the minimal polynomial of `x` as an element of the polynomial ring `R`.
"""
function minpoly(R::ZZPolyRing, x::QQBarFieldElem)
  z = R()
  @ccall libflint.fmpz_poly_set(z::Ref{ZZPolyRingElem}, x::Ref{QQBarFieldElem})::Nothing
  return z
end

@doc raw"""
    minpoly(R::QQPolyRing, x::QQBarFieldElem)

Return the minimal polynomial of `x` as an element of the polynomial ring `R`.
"""
function minpoly(R::QQPolyRing, x::QQBarFieldElem)
  z = R()
  @ccall libflint.fmpq_poly_set_fmpz_poly(z::Ref{QQPolyRingElem}, x::Ref{QQBarFieldElem})::Nothing
  return z
end

@doc raw"""
    denominator(x::QQBarFieldElem)

Return the denominator of `x`, defined as the leading coefficient of the
minimal polynomial of `x`. The result is returned as an `ZZRingElem`.
"""
function denominator(x::QQBarFieldElem)
  d = degree(x)
  q = ZZRingElem()
  @ccall libflint.fmpz_poly_get_coeff_fmpz(q::Ref{ZZRingElem}, x::Ref{QQBarFieldElem}, d::Int)::Nothing
  return q
end

@doc raw"""
    numerator(x::QQBarFieldElem)

Return the numerator of `x`, defined as `x` multiplied by its denominator.
The result is an algebraic integer.
"""
function numerator(x::QQBarFieldElem)
  return x * denominator(x)
end

@doc raw"""
    height(x::QQBarFieldElem)

Return the height of the algebraic number `x`. The result is an `ZZRingElem` integer.
"""
function height(x::QQBarFieldElem)
  z = ZZRingElem()
  @ccall libflint.qqbar_height(z::Ref{ZZRingElem}, x::Ref{QQBarFieldElem})::Nothing
  return z
end

@doc raw"""
    height_bits(x::QQBarFieldElem)

Return the height of the algebraic number `x` measured in bits.
The result is a Julia integer.
"""
function height_bits(x::QQBarFieldElem)
  return @ccall libflint.qqbar_height_bits(x::Ref{QQBarFieldElem})::Int
end


###############################################################################
#
#   Random generation
#
###############################################################################

@doc raw"""
    rand(R::QQBarField; degree::Int, bits::Int, randtype::Symbol=:null)

Return a random algebraic number with degree up to `degree`
and coefficients up to `bits` in size. By default, both real and
complex numbers are generated. Set the optional `randtype` to `:real` or
`:nonreal` to generate a specific type of number. Note that
nonreal numbers require `degree` at least 2.
"""
function rand(R::QQBarField; degree::Int, bits::Int,
    randtype::Symbol=:null)
  state = _flint_rand_states[Threads.threadid()]
  x = R()

  degree <= 0 && error("degree must be positive")
  bits <= 0 && error("bits must be positive")

  if randtype == :null
    @ccall libflint.qqbar_randtest(x::Ref{QQBarFieldElem}, state::Ref{rand_ctx}, degree::Int, bits::Int)::Nothing
  elseif randtype == :real
    @ccall libflint.qqbar_randtest_real(x::Ref{QQBarFieldElem}, state::Ref{rand_ctx}, degree::Int, bits::Int)::Nothing
  elseif randtype == :nonreal
    degree < 2 && error("nonreal requires degree >= 2")
    @ccall libflint.qqbar_randtest_nonreal(x::Ref{QQBarFieldElem}, state::Ref{rand_ctx}, degree::Int, bits::Int)::Nothing
  else
    error("randtype not defined")
  end

  return x
end

###############################################################################
#
#   Conformance test element generation
#
###############################################################################

function ConformanceTests.generate_element(R::QQBarField)
  return rand(R, degree=5, bits=5)
end

###############################################################################
#
#   Unary operators
#
###############################################################################

-(a::QQBarFieldElem) = neg!(QQBarFieldElem(), a)

###############################################################################
#
#   Binary operators
#
###############################################################################

+(a::QQBarFieldElem, b::QQBarFieldElem) = add!(QQBarFieldElem(), a, b)

-(a::QQBarFieldElem, b::QQBarFieldElem) = sub!(QQBarFieldElem(), a, b)

*(a::QQBarFieldElem, b::QQBarFieldElem) = mul!(QQBarFieldElem(), a, b)

function ^(a::QQBarFieldElem, b::QQBarFieldElem)
  ok, z = pow!(QQBarFieldElem(), a, b)
  !ok && throw(DomainError((a, b)))
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

for T in (QQFieldElem, ZZRingElem, Int, UInt, Integer, Rational)
  @eval begin
    +(a::QQBarFieldElem, b::$T) = add!(QQBarFieldElem(), a, b)
    +(a::$T, b::QQBarFieldElem) = add!(QQBarFieldElem(), a, b)

    -(a::QQBarFieldElem, b::$T) = sub!(QQBarFieldElem(), a, b)
    -(a::$T, b::QQBarFieldElem) = sub!(QQBarFieldElem(), a, b)

    *(a::QQBarFieldElem, b::$T) = mul!(QQBarFieldElem(), a, b)
    *(a::$T, b::QQBarFieldElem) = mul!(QQBarFieldElem(), a, b)

    function ^(a::QQBarFieldElem, b::$T)
      is_zero(a) && b < 0 && throw(DivideError())
      return pow!(QQBarFieldElem(), a, b)
    end
    ^(a::$T, b::QQBarFieldElem) = QQBarFieldElem(a) ^ b
  end
end

###############################################################################
#
#   Exact division
#
###############################################################################

function inv(a::QQBarFieldElem)
  iszero(a) && throw(DivideError())
  z = QQBarFieldElem()
  @ccall libflint.qqbar_inv(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

function divexact(a::QQBarFieldElem, b::QQBarFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  @ccall libflint.qqbar_div(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, b::Ref{QQBarFieldElem})::Nothing
  return z
end

function divexact(a::QQBarFieldElem, b::QQFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  @ccall libflint.qqbar_div_fmpq(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, b::Ref{QQFieldElem})::Nothing
  return z
end

function divexact(a::QQBarFieldElem, b::ZZRingElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  @ccall libflint.qqbar_div_fmpz(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, b::Ref{ZZRingElem})::Nothing
  return z
end

function divexact(a::QQBarFieldElem, b::Int; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  @ccall libflint.qqbar_div_si(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, b::Int)::Nothing
  return z
end

function divexact(a::QQFieldElem, b::QQBarFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  @ccall libflint.qqbar_fmpq_div(z::Ref{QQBarFieldElem}, a::Ref{QQFieldElem}, b::Ref{QQBarFieldElem})::Nothing
  return z
end

function divexact(a::ZZRingElem, b::QQBarFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  @ccall libflint.qqbar_fmpz_div(z::Ref{QQBarFieldElem}, a::Ref{ZZRingElem}, b::Ref{QQBarFieldElem})::Nothing
  return z
end

function divexact(a::Int, b::QQBarFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  @ccall libflint.qqbar_si_div(z::Ref{QQBarFieldElem}, a::Int, b::Ref{QQBarFieldElem})::Nothing
  return z
end

//(a::QQBarFieldElem, b::QQBarFieldElem) = divexact(a, b)
//(a::QQBarFieldElem, b::QQFieldElem) = divexact(a, b)
//(a::QQBarFieldElem, b::ZZRingElem) = divexact(a, b)
//(a::QQBarFieldElem, b::Int) = divexact(a, b)
//(a::QQFieldElem, b::QQBarFieldElem) = divexact(a, b)
//(a::ZZRingElem, b::QQBarFieldElem) = divexact(a, b)
//(a::Int, b::QQBarFieldElem) = divexact(a, b)


function <<(a::QQBarFieldElem, b::Int)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_mul_2exp_si(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, b::Int)::Nothing
  return z
end

function >>(a::QQBarFieldElem, b::Int)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_mul_2exp_si(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, (-b)::Int)::Nothing
  return z
end

###############################################################################
#
#   Polynomial evaluation
#
###############################################################################

function evaluate(x::QQPolyRingElem, y::QQBarFieldElem)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_evaluate_fmpq_poly(z::Ref{QQBarFieldElem}, x::Ref{QQPolyRingElem}, y::Ref{QQBarFieldElem})::Nothing
  return z
end

function evaluate(x::ZZPolyRingElem, y::QQBarFieldElem)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_evaluate_fmpz_poly(z::Ref{QQBarFieldElem}, x::Ref{ZZPolyRingElem}, y::Ref{QQBarFieldElem})::Nothing
  return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::QQBarFieldElem, b::QQBarFieldElem)
  return Bool(@ccall libflint.qqbar_equal(a::Ref{QQBarFieldElem}, b::Ref{QQBarFieldElem})::Cint)
end

function cmp(a::QQBarFieldElem, b::QQBarFieldElem)
  !isreal(a) && throw(DomainError(a, "comparing nonreal numbers"))
  !isreal(b) && throw(DomainError(b, "comparing nonreal numbers"))
  return @ccall libflint.qqbar_cmp_re(a::Ref{QQBarFieldElem}, b::Ref{QQBarFieldElem})::Cint
end

isless(a::QQBarFieldElem, b::QQBarFieldElem) = cmp(a, b) < 0
isless(a::QQBarFieldElem, b::ZZRingElem) = isless(a, QQBarFieldElem(b))
isless(a::QQBarFieldElem, b::QQFieldElem) = isless(a, QQBarFieldElem(b))
isless(a::QQBarFieldElem, b::Int) = isless(a, QQBarFieldElem(b))
isless(a::QQFieldElem, b::QQBarFieldElem) = isless(QQBarFieldElem(a), b)
isless(a::ZZRingElem, b::QQBarFieldElem) = isless(QQBarFieldElem(a), b)
isless(a::Int, b::QQBarFieldElem) = isless(QQBarFieldElem(a), b)

is_positive(a::QQBarFieldElem) = a > 0
is_negative(a::QQBarFieldElem) = a < 0

# todo: export the cmp functions?
cmp_real(a::QQBarFieldElem, b::QQBarFieldElem) = @ccall libflint.qqbar_cmp_re(a::Ref{QQBarFieldElem}, b::Ref{QQBarFieldElem})::Cint
cmp_imag(a::QQBarFieldElem, b::QQBarFieldElem) = @ccall libflint.qqbar_cmp_im(a::Ref{QQBarFieldElem}, b::Ref{QQBarFieldElem})::Cint
cmpabs(a::QQBarFieldElem, b::QQBarFieldElem) = @ccall libflint.qqbar_cmpabs(a::Ref{QQBarFieldElem}, b::Ref{QQBarFieldElem})::Cint
cmpabs_real(a::QQBarFieldElem, b::QQBarFieldElem) = @ccall libflint.qqbar_cmpabs_re(a::Ref{QQBarFieldElem}, b::Ref{QQBarFieldElem})::Cint
cmpabs_imag(a::QQBarFieldElem, b::QQBarFieldElem) = @ccall libflint.qqbar_cmpabs_im(a::Ref{QQBarFieldElem}, b::Ref{QQBarFieldElem})::Cint
cmp_root_order(a::QQBarFieldElem, b::QQBarFieldElem) = @ccall libflint.qqbar_cmp_root_order(a::Ref{QQBarFieldElem}, b::Ref{QQBarFieldElem})::Cint

@doc raw"""
    is_equal_real(a::QQBarFieldElem, b::QQBarFieldElem)

Compares the real parts of `a` and `b`.
"""
is_equal_real(a::QQBarFieldElem, b::QQBarFieldElem) = cmp_real(a, b) == 0

@doc raw"""
    is_equal_imag(a::QQBarFieldElem, b::QQBarFieldElem)

Compares the imaginary parts of `a` and `b`.
"""
is_equal_imag(a::QQBarFieldElem, b::QQBarFieldElem) = cmp_imag(a, b) == 0

@doc raw"""
    is_equal_abs(a::QQBarFieldElem, b::QQBarFieldElem)

Compares the absolute values of `a` and `b`.
"""
is_equal_abs(a::QQBarFieldElem, b::QQBarFieldElem) = cmpabs(a, b) == 0

@doc raw"""
    is_equal_abs_real(a::QQBarFieldElem, b::QQBarFieldElem)

Compares the absolute values of the real parts of `a` and `b`.
"""
is_equal_abs_real(a::QQBarFieldElem, b::QQBarFieldElem) = cmpabs_real(a, b) == 0

@doc raw"""
    is_equal_abs_imag(a::QQBarFieldElem, b::QQBarFieldElem)

Compares the absolute values of the imaginary parts of `a` and `b`.
"""
is_equal_abs_imag(a::QQBarFieldElem, b::QQBarFieldElem) = cmpabs_imag(a, b) == 0


@doc raw"""
    is_less_real(a::QQBarFieldElem, b::QQBarFieldElem)

Compares the real parts of `a` and `b`.
"""
is_less_real(a::QQBarFieldElem, b::QQBarFieldElem) = cmp_real(a, b) < 0

@doc raw"""
    is_less_imag(a::QQBarFieldElem, b::QQBarFieldElem)

Compares the imaginary parts of `a` and `b`.
"""
is_less_imag(a::QQBarFieldElem, b::QQBarFieldElem) = cmp_imag(a, b) < 0

@doc raw"""
    is_less_abs(a::QQBarFieldElem, b::QQBarFieldElem)

Compares the absolute values of `a` and `b`.
"""
is_less_abs(a::QQBarFieldElem, b::QQBarFieldElem) = cmpabs(a, b) < 0


@doc raw"""
    is_less_abs_real(a::QQBarFieldElem, b::QQBarFieldElem)

Compares the absolute values of the real parts of `a` and `b`.
"""
is_less_abs_real(a::QQBarFieldElem, b::QQBarFieldElem) = cmpabs_real(a, b) < 0

@doc raw"""
    is_less_abs_imag(a::QQBarFieldElem, b::QQBarFieldElem)

Compares the absolute values of the imaginary parts of `a` and `b`.
"""
is_less_abs_imag(a::QQBarFieldElem, b::QQBarFieldElem) = cmpabs_imag(a, b) < 0

@doc raw"""
    is_less_root_order(a::QQBarFieldElem, b::QQBarFieldElem)

Compares the `a` and `b` in root sort order.
"""
is_less_root_order(a::QQBarFieldElem, b::QQBarFieldElem) = cmp_root_order(a, b) < 0

# todo: wrap qqbar_equal_fmpq_poly_val

###############################################################################
#
#   Complex parts
#
###############################################################################

@doc raw"""
    real(a::QQBarFieldElem)

Return the real part of `a`.
"""
function real(a::QQBarFieldElem)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_re(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

@doc raw"""
    imag(a::QQBarFieldElem)

Return the imaginary part of `a`.
"""
function imag(a::QQBarFieldElem)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_im(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

@doc raw"""
    abs(a::QQBarFieldElem)

Return the absolute value of `a`.
"""
function abs(a::QQBarFieldElem)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_abs(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

@doc raw"""
    conj(a::QQBarFieldElem)

Return the complex conjugate of `a`.
"""
function conj(a::QQBarFieldElem)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_conj(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

@doc raw"""
    abs2(a::QQBarFieldElem)

Return the squared absolute value of `a`.
"""
function abs2(a::QQBarFieldElem)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_abs2(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

@doc raw"""
    sign(a::QQBarFieldElem)

Return the complex sign of `a`, defined as zero if `a` is zero
and as $a / |a|$ otherwise.
"""
function sign(a::QQBarFieldElem)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_sgn(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

@doc raw"""
    csgn(a::QQBarFieldElem)

Return the extension of the real sign function taking the value 1
strictly in the right half plane, -1 strictly in the left half plane,
and the sign of the imaginary part when on the imaginary axis.
Equivalently, $\operatorname{csgn}(x) = x / \sqrt{x^2}$ except that the value is 0
at zero.
"""
function csgn(a::QQBarFieldElem)
  return QQBarFieldElem(@ccall libflint.qqbar_csgn(a::Ref{QQBarFieldElem})::Cint)
end

@doc raw"""
    sign_real(a::QQBarFieldElem)

Return the sign of the real part of `a`.
"""
function sign_real(a::QQBarFieldElem)
  return QQBarFieldElem(@ccall libflint.qqbar_sgn_re(a::Ref{QQBarFieldElem})::Cint)
end

@doc raw"""
    sign_imag(a::QQBarFieldElem)

Return the sign of the imaginary part of `a`.
"""
function sign_imag(a::QQBarFieldElem)
  return QQBarFieldElem(@ccall libflint.qqbar_sgn_im(a::Ref{QQBarFieldElem})::Cint)
end

function floor(a::QQBarFieldElem)
  return QQBarFieldElem(floor(ZZRingElem, a))
end

function floor(::Type{ZZRingElem}, a::QQBarFieldElem)
  z = ZZRingElem()
  @ccall libflint.qqbar_floor(z::Ref{ZZRingElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

function ceil(a::QQBarFieldElem)
  return QQBarFieldElem(ceil(ZZRingElem, a))
end

function ceil(::Type{ZZRingElem}, a::QQBarFieldElem)
  z = ZZRingElem()
  @ccall libflint.qqbar_ceil(z::Ref{ZZRingElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

###############################################################################
#
#  Round
#
###############################################################################

# rounding
function round(::Type{ZZRingElem}, a::QQBarFieldElem, ::RoundingMode{:Nearest})
  if is_zero(a)
    return zero(ZZ)
  end

  ca = floor(ZZRingElem, a)
  if a < ca + QQ(1//2)
    return ca
  elseif a > ca + QQ(1//2)
    return ca + 1
  else
    return is_even(ca) ? ca : ca + 1
  end
end

function round(a::QQBarFieldElem, ::RoundingMode{:Nearest})
  return parent(a)(round(ZZRingElem, a, RoundNearest))
end

round(x::QQBarFieldElem, ::RoundingMode{:Up}) = ceil(x)
round(::Type{ZZRingElem}, x::QQBarFieldElem, ::RoundingMode{:Up})= ceil(ZZRingElem, x)

round(x::QQBarFieldElem, ::RoundingMode{:Down}) = floor(x)
round(::Type{ZZRingElem}, x::QQBarFieldElem, ::RoundingMode{:Down}) = floor(ZZRingElem, x)

round(x::QQBarFieldElem, ::RoundingMode{:NearestTiesAway}) = sign(x) * floor(abs(x) + 1//2)
function round(::Type{ZZRingElem}, x::QQBarFieldElem, ::RoundingMode{:NearestTiesAway})
  tmp = floor(ZZRingElem, abs(x) + 1//2)
  return is_positive(x) ? tmp : -tmp
end

# default
round(a::QQBarFieldElem) = round(a, RoundNearestTiesAway)
round(::Type{ZZRingElem}, a::QQBarFieldElem) = round(ZZRingElem, a, RoundNearestTiesAway)

###############################################################################
#
#   Roots
#
###############################################################################

@doc raw"""
    sqrt(a::QQBarFieldElem; check::Bool=true)

Return the principal square root of `a`.
"""
function sqrt(a::QQBarFieldElem; check::Bool=true)
  z = QQBarFieldElem()
  @ccall libflint.qqbar_sqrt(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

# TODO: also add cbrt and friends???

@doc raw"""
    root(a::QQBarFieldElem, n::Int)

Return the principal `n`-th root of `a`. Requires positive `n`.
"""
function root(a::QQBarFieldElem, n::Int)
  n <= 0 && throw(DomainError(n))
  z = QQBarFieldElem()
  @ccall libflint.qqbar_root_ui(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, n::UInt)::Nothing
  return z
end

function qqbar_vec(n::Int)
  return @ccall libflint._qqbar_vec_init(n::Int)::Ptr{qqbar_struct}
end

function array(R::QQBarField, v::Ptr{qqbar_struct}, n::Int)
  r = Vector{QQBarFieldElem}(undef, n)
  for i=1:n
    r[i] = R()
    @ccall libflint.qqbar_set(r[i]::Ref{QQBarFieldElem}, (v + (i-1)*sizeof(qqbar_struct))::Ptr{qqbar_struct})::Nothing
  end
  return r
end

function qqbar_vec_clear(v::Ptr{qqbar_struct}, n::Int)
  @ccall libflint._qqbar_vec_clear(v::Ptr{qqbar_struct}, n::Int)::Nothing
end

@doc raw"""
    roots(R::QQBarField, f::ZZPolyRingElem)

Return all the roots of the polynomial `f` in the field of algebraic
numbers `R`. The output array is sorted in the default sort order for
algebraic numbers. Roots of multiplicity higher than one are repeated
according to their multiplicity.
"""
function roots(R::QQBarField, f::ZZPolyRingElem)
  deg = degree(f)
  if deg <= 0
    return QQBarFieldElem[]
  end
  roots = qqbar_vec(deg)
  @ccall libflint.qqbar_roots_fmpz_poly(roots::Ptr{qqbar_struct}, f::Ref{ZZPolyRingElem}, 0::Int)::Nothing
  res = array(R, roots, deg)
  qqbar_vec_clear(roots, deg)
  return res
end

@doc raw"""
    roots(R::QQBarField, f::QQPolyRingElem)

Return all the roots of the polynomial `f` in the field of algebraic
numbers `R`. The output array is sorted in the default sort order for
algebraic numbers. Roots of multiplicity higher than one are repeated
according to their multiplicity.
"""
function roots(R::QQBarField, f::QQPolyRingElem)
  deg = degree(f)
  if deg <= 0
    return QQBarFieldElem[]
  end
  roots = qqbar_vec(deg)
  @ccall libflint.qqbar_roots_fmpq_poly(roots::Ptr{qqbar_struct}, f::Ref{QQPolyRingElem}, 0::Int)::Nothing
  res = array(R, roots, deg)
  qqbar_vec_clear(roots, deg)
  return res
end

@doc raw"""
    conjugates(a::QQBarFieldElem)

Return all the roots of the polynomial `f` in the field of algebraic
numbers `R`. The output array is sorted in the default sort order for
algebraic numbers.
"""
function conjugates(a::QQBarFieldElem)
  deg = degree(a)
  if deg == 1
    return [a]
  end
  conjugates = qqbar_vec(deg)
  @ccall libflint.qqbar_conjugates(conjugates::Ptr{qqbar_struct}, a::Ref{QQBarFieldElem})::Nothing
  res = array(parent(a), conjugates, deg)
  qqbar_vec_clear(conjugates, deg)
  return res
end

# Return the eigenvalues with repetition according to the algebraic multiplicity
function _eigvals_internal(R::QQBarField, A::ZZMatrix)
  n = nrows(A)
  !is_square(A) && throw(DomainError(A, "a square matrix is required"))
  if n == 0
    return QQBarFieldElem[]
  end
  roots = qqbar_vec(n)
  @ccall libflint.qqbar_eigenvalues_fmpz_mat(roots::Ptr{qqbar_struct}, A::Ref{ZZMatrix}, 0::Int)::Nothing
  res = array(R, roots, n)
  qqbar_vec_clear(roots, n)
  return res
end

# Return the eigenvalues with repetition according to the algebraic multiplicity
function _eigvals_internal(R::QQBarField, A::QQMatrix)
  n = nrows(A)
  !is_square(A) && throw(DomainError(A, "a square matrix is required"))
  if n == 0
    return QQBarFieldElem[]
  end
  roots = qqbar_vec(n)
  @ccall libflint.qqbar_eigenvalues_fmpq_mat(roots::Ptr{qqbar_struct}, A::Ref{QQMatrix}, 0::Int)::Nothing
  res = array(R, roots, n)
  qqbar_vec_clear(roots, n)
  return res
end

@doc raw"""
    eigenvalues(R::QQBarField, A::ZZMatrix)
    eigenvalues(R::QQBarField, A::QQMatrix)

Return the eigenvalues `A` in the field of algebraic numbers `R`.
The output array is sorted in the default sort order for
algebraic numbers.
"""
function eigenvalues(R::QQBarField, A::Union{ZZMatrix, QQMatrix})
  return unique(_eigvals_internal(R, A))
end

@doc raw"""
    eigenvalues_with_multiplicities(R::QQBarField, A::ZZMatrix)
    eigenvalues_with_multiplicities(R::QQBarField, A::QQMatrix)

Return the eigenvalues `A` in the field of algebraic numbers `R` together with
their algebraic multiplicities as a vector of tuples.
The output array is sorted in the default sort order for algebraic numbers.
"""
function eigenvalues_with_multiplicities(R::QQBarField, A::Union{ZZMatrix, QQMatrix})
  eig = _eigvals_internal(R, A)
  res = Vector{Tuple{QQBarFieldElem, Int}}()
  k = 1
  n = length(eig)
  for i in 1:n
    if i < n && isequal(eig[i], eig[i + 1])
      k = k + 1
      if i == n - 1
        push!(res, (eig[i], k))
        break
      end
    else
      push!(res, (eig[i], k))
      k = 1
    end
  end

  return res
end

###############################################################################
#
#   Roots of unity and trigonometric functions
#
###############################################################################

@doc raw"""
    root_of_unity(C::QQBarField, n::Int, k::Int = 1)

Return the root of unity $e^{2 \pi i k / n}$ as an element of the field
of algebraic numbers `C`.
"""
function root_of_unity(C::QQBarField, n::Int, k::Int = 1)
  n <= 0 && throw(DomainError(n))
  z = QQBarFieldElem()
  @ccall libflint.qqbar_root_of_unity(z::Ref{QQBarFieldElem}, k::Int, n::UInt)::Nothing
  return z
end

@doc raw"""
    is_root_of_unity(a::QQBarFieldElem)

Return whether the given algebraic number is a root of unity.
"""
function is_root_of_unity(a::QQBarFieldElem)
  return Bool(@ccall libflint.qqbar_is_root_of_unity(C_NULL::Ptr{Int}, C_NULL::Ptr{Int}, a::Ref{QQBarFieldElem})::Cint)
end

@doc raw"""
    root_of_unity_as_args(a::QQBarFieldElem)

Return a pair of integers `(q, p)` such that the given `a` equals
$e^{2 \pi i p / q}$. The denominator `q` will be minimal, with
$0 \le p < q$. Throws if `a` is not a root of unity.
"""
function root_of_unity_as_args(a::QQBarFieldElem)
  p = Vector{Int}(undef, 1)
  q = Vector{Int}(undef, 1)
  if !Bool(@ccall libflint.qqbar_is_root_of_unity(p::Ptr{Int}, q::Ptr{Int}, a::Ref{QQBarFieldElem})::Cint)
    throw(DomainError(a, "value is not a root of unity"))
  end
  return (q[1], p[1])
end

@doc raw"""
    exp_pi_i(a::QQBarFieldElem)

Return $e^{\pi i a}$ as an algebraic number.
Throws if this value is transcendental.
"""
function exp_pi_i(a::QQBarFieldElem)
  r = QQFieldElem(a)
  p = Int(numerator(r))
  q = Int(denominator(r))
  z = QQBarFieldElem()
  @ccall libflint.qqbar_exp_pi_i(z::Ref{QQBarFieldElem}, p::Int, q::Int)::Nothing
  return z
end

@doc raw"""
    sinpi(a::QQBarFieldElem)

Return $\sin(\pi a)$ as an algebraic number.
Throws if this value is transcendental.

# Examples

```jldoctest
julia> QQBar = algebraic_closure(QQ);

julia> x = sinpi(QQBar(1//3))
{a2: 0.866025}
```
"""
function sinpi(a::QQBarFieldElem)
  r = QQFieldElem(a)
  p = Int(numerator(r))
  q = Int(denominator(r))
  z = QQBarFieldElem()
  @ccall libflint.qqbar_sin_pi(z::Ref{QQBarFieldElem}, p::Int, q::Int)::Nothing
  return z
end

@doc raw"""
    cospi(a::QQBarFieldElem)

Return $\cos(\pi a)$ as an algebraic number.
Throws if this value is transcendental.

# Examples

```jldoctest
julia> QQBar = algebraic_closure(QQ);

julia> x = cospi(QQBar(1//6))
{a2: 0.866025}
```
"""
function cospi(a::QQBarFieldElem)
  r = QQFieldElem(a)
  p = Int(numerator(r))
  q = Int(denominator(r))
  z = QQBarFieldElem()
  @ccall libflint.qqbar_cos_pi(z::Ref{QQBarFieldElem}, p::Int, q::Int)::Nothing
  return z
end

@doc raw"""
    sincospi(a::QQBarFieldElem)

Return $\sin(\pi a)$ and $\cos(\pi a)$ as a pair of algebraic numbers.
Throws if either value is transcendental.

# Examples

```jldoctest
julia> QQBar = algebraic_closure(QQ);

julia> s, c = sincospi(QQBar(1//3))
({a2: 0.866025}, {a1: 0.500000})
```
"""
function sincospi(a::QQBarFieldElem)
  r = QQFieldElem(a)
  p = Int(numerator(r))
  q = Int(denominator(r))
  s = QQBarFieldElem()
  @ccall libflint.qqbar_sin_pi(s::Ref{QQBarFieldElem}, p::Int, q::Int)::Nothing
  c = QQBarFieldElem()
  @ccall libflint.qqbar_cos_pi(c::Ref{QQBarFieldElem}, p::Int, q::Int)::Nothing
  return s, c
end


@doc raw"""
    tanpi(a::QQBarFieldElem)

Return $\tan(\pi a)$ as an algebraic number.
Throws if this value is transcendental or undefined.
"""
function tanpi(a::QQBarFieldElem)
  r = QQFieldElem(a)
  p = Int(numerator(r))
  q = Int(denominator(r))
  z = QQBarFieldElem()
  if !Bool(@ccall libflint.qqbar_tan_pi(z::Ref{QQBarFieldElem}, p::Int, q::Int)::Cint)
    throw(DomainError(a, "function value is not algebraic"))
  end
  return z
end

@doc raw"""
    atanpi(a::QQBarFieldElem)

Return $\operatorname{atan}(a) / \pi$ as an algebraic number.
Throws if this value is transcendental or undefined.
"""
function atanpi(a::QQBarFieldElem)
  p = Vector{Int}(undef, 1)
  q = Vector{Int}(undef, 1)
  if !Bool(@ccall libflint.qqbar_atan_pi(p::Ptr{Int}, q::Ptr{Int}, a::Ref{QQBarFieldElem})::Cint)
    throw(DomainError(a, "function value is not algebraic"))
  end
  return QQBarFieldElem(p[1]) // q[1]
end

@doc raw"""
    asinpi(a::QQBarFieldElem)

Return $\operatorname{asin}(a) / \pi$ as an algebraic number.
Throws if this value is transcendental.
"""
function asinpi(a::QQBarFieldElem)
  p = Vector{Int}(undef, 1)
  q = Vector{Int}(undef, 1)
  if !Bool(@ccall libflint.qqbar_asin_pi(p::Ptr{Int}, q::Ptr{Int}, a::Ref{QQBarFieldElem})::Cint)
    throw(DomainError(a, "function value is not algebraic"))
  end
  return QQBarFieldElem(p[1]) // q[1]
end

@doc raw"""
    acospi(a::QQBarFieldElem)

Return $\operatorname{acos}(a) / \pi$ as an algebraic number.
Throws if this value is transcendental.
"""
function acospi(a::QQBarFieldElem)
  p = Vector{Int}(undef, 1)
  q = Vector{Int}(undef, 1)
  if !Bool(@ccall libflint.qqbar_acos_pi(p::Ptr{Int}, q::Ptr{Int}, a::Ref{QQBarFieldElem})::Cint)
    throw(DomainError(a, "function value is not algebraic"))
  end
  return QQBarFieldElem(p[1]) // q[1]
end

@doc raw"""
    log_pi_i(a::QQBarFieldElem)

Return $\log(a) / (\pi i)$ as an algebraic number.
Throws if this value is transcendental or undefined.
"""
function log_pi_i(a::QQBarFieldElem)
  p = Vector{Int}(undef, 1)
  q = Vector{Int}(undef, 1)
  if !Bool(@ccall libflint.qqbar_log_pi_i(p::Ptr{Int}, q::Ptr{Int}, a::Ref{QQBarFieldElem})::Cint)
    throw(DomainError(a, "function value is not algebraic"))
  end
  return QQBarFieldElem(p[1]) // q[1]
end



###############################################################################
#
#   Guessing
#
###############################################################################

@doc raw"""
    guess(R::QQBarField, x::AcbFieldElem, maxdeg::Int, maxbits::Int=0)
    guess(R::QQBarField, x::ArbFieldElem, maxdeg::Int, maxbits::Int=0)
    guess(R::QQBarField, x::ComplexFieldElem, maxdeg::Int, maxbits::Int=0)
    guess(R::QQBarField, x::RealFieldElem, maxdeg::Int, maxbits::Int=0)

Try to reconstruct an algebraic number from a given numerical enclosure `x`.
The algorithm looks for candidates up to degree `maxdeg` and with
coefficients up to size `maxbits` (which defaults to the precision of `x`
if not given). Throws if no suitable algebraic number can be found.

Guessing typically requires high precision to succeed, and it does not make
much sense to call this function with input precision smaller than
$O(maxdeg \cdot maxbits)$.
If this function succeeds, then the output is guaranteed to be contained in
the enclosure `x`, but failure does not prove that such an algebraic
number with the specified parameters does not exist.

This function does a single iteration with the target parameters. For best
performance, one should invoke this function repeatedly with successively
larger parameters when the size of the intended solution is unknown or
may be much smaller than a worst-case bound.
"""
function guess end

function guess(R::QQBarField, x::T, maxdeg::Int, maxbits::Int=0) where {T <: Union{AcbFieldElem, ComplexFieldElem}}
  prec = precision(parent(x))
  if maxbits <= 0
    maxbits = prec
  end
  res = QQBarFieldElem()
  found = Bool(@ccall libflint.qqbar_guess(res::Ref{QQBarFieldElem}, x::Ref{T}, maxdeg::Int, maxbits::Int, 0::Int, prec::Int)::Cint)
  if !found
    error("No suitable algebraic number found")
  end
  return res
end

function guess(R::QQBarField, x::ArbFieldElem, maxdeg::Int, maxbits::Int=0)
  CC = AcbField(precision(parent(x)))
  return guess(R, CC(x), maxdeg, maxbits)
end

function guess(R::QQBarField, x::RealFieldElem, maxdeg::Int, maxbits::Int=0)
  CC = complex_field()
  return guess(R, CC(x), maxdeg, maxbits)
end

###############################################################################
#
#   Conversions
#
###############################################################################

@doc raw"""
    (R::ArbField)(a::QQBarFieldElem)

Convert `a` to a real ball with the precision of the parent field `R`.
Throws if `a` is not a real number.
"""
function (R::ArbField)(a::QQBarFieldElem)
  prec = precision(R)
  z = R()
  @ccall libflint.qqbar_get_arb(z::Ref{ArbFieldElem}, a::Ref{QQBarFieldElem}, prec::Int)::Nothing
  !isfinite(z) && throw(DomainError(a, "nonreal algebraic number"))
  return z
end

@doc raw"""
    (R::AcbField)(a::QQBarFieldElem)

Convert `a` to a complex ball with the precision of the parent field `R`.
"""
function (R::AcbField)(a::QQBarFieldElem)
  prec = precision(R)
  z = R()
  @ccall libflint.qqbar_get_acb(z::Ref{AcbFieldElem}, a::Ref{QQBarFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    (R::RealField)(a::QQBarFieldElem)

Convert `a` to a real ball with the precision of the parent field `R`.
Throws if `a` is not a real number.
"""
function (R::RealField)(a::QQBarFieldElem)
  prec = precision(Balls)
  z = R()
  @ccall libflint.qqbar_get_arb(z::Ref{RealFieldElem}, a::Ref{QQBarFieldElem}, prec::Int)::Nothing
  !isfinite(z) && throw(DomainError(a, "nonreal algebraic number"))
  return z
end

@doc raw"""
    (R::ComplexField)(a::QQBarFieldElem)

Convert `a` to a complex ball with the precision of the parent field `R`.
"""
function (R::ComplexField)(a::QQBarFieldElem)
  prec = precision(Balls)
  z = R()
  @ccall libflint.qqbar_get_acb(z::Ref{ComplexFieldElem}, a::Ref{QQBarFieldElem}, prec::Int)::Nothing
  return z
end

@doc raw"""
    QQFieldElem(a::QQBarFieldElem)

Convert `a` to a rational number of type `QQFieldElem`.
Throws if `a` is not a rational number.
"""
function QQFieldElem(a::QQBarFieldElem)
  !is_rational(a) && throw(DomainError(a, "nonrational algebraic number"))
  z = QQFieldElem()
  @ccall libflint.qqbar_get_fmpq(z::Ref{QQFieldElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

@doc raw"""
    ZZRingElem(a::QQBarFieldElem)

Convert `a` to an integer of type `ZZRingElem`.
Throws if `a` is not an integer.
"""
function ZZRingElem(a::QQBarFieldElem)
  !isinteger(a) && throw(DomainError(a, "noninteger algebraic number"))
  z = ZZRingElem()
  @ccall libflint.qqbar_get_fmpz(z::Ref{ZZRingElem}, a::Ref{QQBarFieldElem})::Nothing
  return z
end

function (::Type{ComplexF64})(a::QQBarFieldElem)
  z = AcbField(53, cached = false)(a)
  x = real(z)
  xx = Float64(x)
  y = imag(z)
  yy = Float64(y)
  return ComplexF64(xx, yy)
end

function (::Type{Float64})(a::QQBarFieldElem)
  isreal(a) || throw(InexactError(:Float64, Float64, a))
  x = ArbField(53, cached = false)(a)
  return Float64(x)
end

function (::Type{T})(a::QQBarFieldElem) where {T <: Integer}
  return T(ZZRingElem(a))
end

function (::Type{T})(a::QQBarFieldElem) where {T <: Rational}
  return T(QQFieldElem(a))
end

convert(::Type{QQBarFieldElem}, a::RationalUnion) = QQBarFieldElem(a)
convert(::Type{T}, a::QQBarFieldElem) where {T <: RationalUnion} = T(a)

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::QQBarFieldElemOrPtr)
  @ccall libflint.qqbar_zero(z::Ref{QQBarFieldElemOrPtr})::Nothing
  return z
end

function one!(z::QQBarFieldElemOrPtr)
  @ccall libflint.qqbar_one(z::Ref{QQBarFieldElemOrPtr})::Nothing
  return z
end

function neg!(z::QQBarFieldElemOrPtr, a::QQBarFieldElemOrPtr)
  @ccall libflint.qqbar_neg(z::Ref{QQBarFieldElemOrPtr}, a::Ref{QQBarFieldElemOrPtr})::Nothing
  return z
end

#

function add!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::QQBarFieldElemOrPtr)
  @ccall libflint.qqbar_add(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Ref{QQBarFieldElemOrPtr})::Nothing
  return z
end

function add!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::QQFieldElemOrPtr)
  @ccall libflint.qqbar_add_fmpq(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Ref{QQFieldElem})::Nothing
  return z
end

function add!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.qqbar_add_fmpz(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Ref{ZZRingElem})::Nothing
  return z
end

function add!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::Int)
  @ccall libflint.qqbar_add_si(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Int)::Nothing
  return z
end

function add!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::UInt)
  @ccall libflint.qqbar_add_ui(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::UInt)::Nothing
  return z
end

add!(c::QQBarFieldElemOrPtr, a::QQBarFieldElemOrPtr, b::Union{Integer, Rational}) = add!(c, a, flintify(b))
add!(c::QQBarFieldElemOrPtr, a::Union{QQFieldElemOrPtr, ZZRingElemOrPtr, Integer, Rational}, b::QQBarFieldElemOrPtr) = add!(c, b, a)

#

function sub!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::QQBarFieldElemOrPtr)
  @ccall libflint.qqbar_sub(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Ref{QQBarFieldElemOrPtr})::Nothing
  return z
end

function sub!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::QQFieldElemOrPtr)
  @ccall libflint.qqbar_sub_fmpq(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Ref{QQFieldElem})::Nothing
  return z
end

function sub!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.qqbar_sub_fmpz(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Ref{ZZRingElem})::Nothing
  return z
end

function sub!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::Int)
  @ccall libflint.qqbar_sub_si(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Int)::Nothing
  return z
end

function sub!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::UInt)
  @ccall libflint.qqbar_sub_ui(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::UInt)::Nothing
  return z
end

function sub!(z::QQBarFieldElemOrPtr, x::QQFieldElemOrPtr, y::QQBarFieldElemOrPtr)
  @ccall libflint.qqbar_fmpq_sub(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQFieldElem}, y::Ref{QQBarFieldElemOrPtr})::Nothing
  return z
end

function sub!(z::QQBarFieldElemOrPtr, x::ZZRingElemOrPtr, y::QQBarFieldElemOrPtr)
  @ccall libflint.qqbar_fmpz_sub(z::Ref{QQBarFieldElemOrPtr}, x::Ref{ZZRingElem}, y::Ref{QQBarFieldElemOrPtr})::Nothing
  return z
end

function sub!(z::QQBarFieldElemOrPtr, x::Int, y::QQBarFieldElemOrPtr)
  @ccall libflint.qqbar_si_sub(z::Ref{QQBarFieldElemOrPtr}, x::Int, y::Ref{QQBarFieldElemOrPtr})::Nothing
  return z
end

function sub!(z::QQBarFieldElemOrPtr, x::UInt, y::QQBarFieldElemOrPtr)
  @ccall libflint.qqbar_ui_sub(z::Ref{QQBarFieldElemOrPtr}, x::UInt, y::Ref{QQBarFieldElemOrPtr})::Nothing
  return z
end

sub!(c::QQBarFieldElemOrPtr, a::QQBarFieldElemOrPtr, b::Union{Integer, Rational}) = sub!(c, a, flintify(b))
sub!(c::QQBarFieldElemOrPtr, a::Union{Integer, Rational}, b::QQBarFieldElemOrPtr) = sub!(c, flintify(a), b)

#

function mul!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::QQBarFieldElemOrPtr)
  @ccall libflint.qqbar_mul(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Ref{QQBarFieldElemOrPtr})::Nothing
  return z
end

function mul!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::QQFieldElemOrPtr)
  @ccall libflint.qqbar_mul_fmpq(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Ref{QQFieldElem})::Nothing
  return z
end

function mul!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::ZZRingElemOrPtr)
  @ccall libflint.qqbar_mul_fmpz(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Ref{ZZRingElem})::Nothing
  return z
end

function mul!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::Int)
  @ccall libflint.qqbar_mul_si(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::Int)::Nothing
  return z
end

function mul!(z::QQBarFieldElemOrPtr, x::QQBarFieldElemOrPtr, y::UInt)
  @ccall libflint.qqbar_mul_ui(z::Ref{QQBarFieldElemOrPtr}, x::Ref{QQBarFieldElemOrPtr}, y::UInt)::Nothing
  return z
end

mul!(c::QQBarFieldElemOrPtr, a::QQBarFieldElemOrPtr, b::Union{Integer, Rational}) = mul!(c, a, flintify(b))
mul!(c::QQBarFieldElemOrPtr, a::Union{QQFieldElemOrPtr, ZZRingElemOrPtr, Integer, Rational}, b::QQBarFieldElemOrPtr) = mul!(c, b, a)

#


function pow!(z::QQBarFieldElemOrPtr, a::QQBarFieldElemOrPtr, b::QQBarFieldElemOrPtr)
  ok = Bool(@ccall libflint.qqbar_pow(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, b::Ref{QQBarFieldElem})::Cint)
  return ok, z
end

function pow!(z::QQBarFieldElemOrPtr, a::QQBarFieldElemOrPtr, b::ZZRingElemOrPtr)
  @ccall libflint.qqbar_pow_fmpz(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, b::Ref{ZZRingElem})::Nothing
  return z
end

function pow!(z::QQBarFieldElemOrPtr, a::QQBarFieldElemOrPtr, b::QQFieldElemOrPtr)
  @ccall libflint.qqbar_pow_fmpq(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, b::Ref{QQFieldElem})::Nothing
  return z
end

function pow!(z::QQBarFieldElemOrPtr, a::QQBarFieldElemOrPtr, b::Int)
  @ccall libflint.qqbar_pow_si(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, b::Int)::Nothing
  return z
end

function pow!(z::QQBarFieldElemOrPtr, a::QQBarFieldElemOrPtr, b::UInt)
  @ccall libflint.qqbar_pow_ui(z::Ref{QQBarFieldElem}, a::Ref{QQBarFieldElem}, b::UInt)::Nothing
  return z
end

pow!(z::QQBarFieldElemOrPtr, a::QQBarFieldElemOrPtr, b::Union{Integer, Rational}) = pow!(z, a, flintify(b))


###############################################################################
#
#   Parent object call overloads
#
###############################################################################

(a::QQBarField)() = QQBarFieldElem()

(a::QQBarField)(b::Any) = QQBarFieldElem(b)

(a::QQBarField)(b::QQBarFieldElem) = b

###############################################################################
#
#   constructor
#
###############################################################################

"""
    algebraic_closure(::QQField)

Return a field representing the algebraic closure of the field of
rational numbers.

# Examples

```jldoctest
julia> K = algebraic_closure(QQ)
Algebraic closure of rational field

julia> sqrt(K(2))
{a2: 1.41421}
```
"""
algebraic_closure(::QQField) = QQBarField()
