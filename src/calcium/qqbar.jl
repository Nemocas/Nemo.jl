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

parent(a::QQBarFieldElem) = CalciumQQBar

parent_type(::Type{QQBarFieldElem}) = QQBarField

elem_type(::Type{QQBarField}) = QQBarFieldElem

base_ring_type(::Type{QQBarField}) = typeof(Union{})

base_ring(::QQBarField) = Union{}

is_domain_type(::Type{QQBarFieldElem}) = true

check_parent(a::QQBarFieldElem, b::QQBarFieldElem, throw::Bool = true) = true

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
  ccall((:qqbar_set_si, libflint), Nothing, (Ref{QQBarFieldElem}, Int, ), z, a)
  return z
end

function QQBarFieldElem(a::Complex)
  r = QQBarFieldElem(real(a))
  s = QQBarFieldElem(imag(a))
  z = QQBarFieldElem()
  ccall((:qqbar_set_re_im, libflint),
        Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, r, s)
  return z
end

function QQBarFieldElem(a::ZZRingElem)
  z = QQBarFieldElem()
  ccall((:qqbar_set_fmpz, libflint),
        Nothing, (Ref{QQBarFieldElem}, Ref{ZZRingElem}, ), z, a)
  return z
end

function QQBarFieldElem(a::QQFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_set_fmpq, libflint),
        Nothing, (Ref{QQBarFieldElem}, Ref{QQFieldElem}, ), z, a)
  return z
end

QQBarFieldElem(a::Rational) = QQBarFieldElem(QQFieldElem(a))

QQBarFieldElem(a::Integer) = QQBarFieldElem(ZZRingElem(a))

function deepcopy_internal(a::QQBarFieldElem, dict::IdDict)
  z = QQBarFieldElem()
  ccall((:qqbar_set, libflint), Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a)
  return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::QQBarFieldElem) = a

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

# todo
# function expressify(a::QQBarFieldElem; context = nothing)
# end

#=
function qqbar_vec(n::Int)
return ccall((:_qqbar_vec_init, libflint), Ptr{qqbar_struct}, (Int,), n)
end

function array(R::QQBarField, v::Ptr{qqbar_struct}, n::Int)
r = Vector{QQBarFieldElem}(undef, n)
for i=1:n
r[i] = R()
ccall((:qqbar_set, libflint), Nothing, (Ref{QQBarFieldElem}, Ptr{qqbar_struct}),
r[i], v + (i-1)*sizeof(qqbar_struct))
end
return r
end

function qqbar_vec_clear(v::Ptr{qqbar_struct}, n::Int)
ccall((:_qqbar_vec_clear, libflint),
Nothing, (Ptr{qqbar_struct}, Int), v, n)
end

function roots(R::QQBarField, f::ZZPolyRingElem)
deg = degree(f)
if deg <= 0
return Array{QQBarFieldElem}(undef, 0)
end
roots = qqbar_vec(deg)
ccall((:qqbar_roots_fmpz_poly, libflint),
Nothing, (Ptr{qqbar_struct}, Ref{ZZPolyRingElem}, Int), roots, f, 0)
res = array(R, roots, deg)
qqbar_vec_clear(roots, deg)
return res
end
=#

function native_string(x::QQBarFieldElem)
  cstr = ccall((:qqbar_get_str_nd, libflint),
               Ptr{UInt8}, (Ref{QQBarFieldElem}, Int), x, Int(6))
  number = unsafe_string(cstr)
  ccall((:flint_free, libflint), Nothing, (Ptr{UInt8},), cstr)

  number = number[1:first(findfirst(" (", number)::UnitRange)-1]
  number = replace(number, "I" => "im")

  R, Rx = polynomial_ring(ZZ, "x")
  polynomial = string(minpoly(R, x))
  polynomial = replace(polynomial, "*" => "")
  res = string("Root ", number, " of ", polynomial)

  return res
end

function show(io::IO, F::QQBarField)
  # deliberately no @show_name or @show_special here as this is a singleton type
  if is_terse(io)
    io = pretty(io)
    print(io, LowercaseOff(), "QQBar")
  else
    print(io, "Field of algebraic numbers")
  end
end

function show(io::IO, x::QQBarFieldElem)
  print(io, native_string(x))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

is_unit(x::QQBarFieldElem) = !is_zero(x)

zero(a::QQBarField) = a(0)

one(a::QQBarField) = a(1)

zero(::Type{QQBarFieldElem}) = CalciumQQBar(0)

one(::Type{QQBarFieldElem}) = CalciumQQBar(1)

@doc raw"""
    degree(x::QQBarFieldElem)

Return the degree of the minimal polynomial of `x`.
"""
function degree(x::QQBarFieldElem)
  return ccall((:qqbar_degree, libflint), Int, (Ref{QQBarFieldElem}, ), x)
end

@doc raw"""
    iszero(x::QQBarFieldElem)

Return whether `x` is the number 0.
"""
function iszero(x::QQBarFieldElem)
  return Bool(ccall((:qqbar_is_zero, libflint), Cint, (Ref{QQBarFieldElem},), x))
end

@doc raw"""
    isone(x::QQBarFieldElem)

Return whether `x` is the number 1.
"""
function isone(x::QQBarFieldElem)
  return Bool(ccall((:qqbar_is_one, libflint), Cint, (Ref{QQBarFieldElem},), x))
end

@doc raw"""
    isinteger(x::QQBarFieldElem)

Return whether `x` is an integer.
"""
function isinteger(x::QQBarFieldElem)
  return Bool(ccall((:qqbar_is_integer, libflint), Cint, (Ref{QQBarFieldElem},), x))
end

@doc raw"""
    is_rational(x::QQBarFieldElem)

Return whether `x` is a rational number.
"""
function is_rational(x::QQBarFieldElem)
  return Bool(ccall((:qqbar_is_rational, libflint), Cint, (Ref{QQBarFieldElem},), x))
end

@doc raw"""
    isreal(x::QQBarFieldElem)

Return whether `x` is a real number.
"""
function isreal(x::QQBarFieldElem)
  return Bool(ccall((:qqbar_is_real, libflint), Cint, (Ref{QQBarFieldElem},), x))
end

@doc raw"""
    is_algebraic_integer(x::QQBarFieldElem)

Return whether `x` is an algebraic integer.
"""
function is_algebraic_integer(x::QQBarFieldElem)
  return Bool(ccall((:qqbar_is_algebraic_integer, libflint),
                    Cint, (Ref{QQBarFieldElem},), x))
end

@doc raw"""
    minpoly(R::ZZPolyRing, x::QQBarFieldElem)

Return the minimal polynomial of `x` as an element of the polynomial ring `R`.
"""
function minpoly(R::ZZPolyRing, x::QQBarFieldElem)
  z = R()
  ccall((:fmpz_poly_set, libflint),
        Nothing, (Ref{ZZPolyRingElem}, Ref{QQBarFieldElem}, ), z, x)
  return z
end

@doc raw"""
    minpoly(R::ZZPolyRing, x::QQBarFieldElem)

Return the minimal polynomial of `x` as an element of the polynomial ring `R`.
"""
function minpoly(R::QQPolyRing, x::QQBarFieldElem)
  z = R()
  ccall((:fmpq_poly_set_fmpz_poly, libflint),
        Nothing, (Ref{QQPolyRingElem}, Ref{QQBarFieldElem}, ), z, x)
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
  ccall((:fmpz_poly_get_coeff_fmpz, libflint),
        Nothing, (Ref{ZZRingElem}, Ref{QQBarFieldElem}, Int), q, x, d)
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
  ccall((:qqbar_height, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQBarFieldElem}, ), z, x)
  return z
end

@doc raw"""
    height_bits(x::QQBarFieldElem)

Return the height of the algebraic number `x` measured in bits.
The result is a Julia integer.
"""
function height_bits(x::QQBarFieldElem)
  return ccall((:qqbar_height_bits, libflint), Int, (Ref{QQBarFieldElem}, ), x)
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
    ccall((:qqbar_randtest, libflint), Nothing,
          (Ref{QQBarFieldElem}, Ref{rand_ctx}, Int, Int), x, state, degree, bits)
  elseif randtype == :real
    ccall((:qqbar_randtest_real, libflint), Nothing,
          (Ref{QQBarFieldElem}, Ref{rand_ctx}, Int, Int), x, state, degree, bits)
  elseif randtype == :nonreal
    degree < 2 && error("nonreal requires degree >= 2")
    ccall((:qqbar_randtest_nonreal, libflint), Nothing,
          (Ref{QQBarFieldElem}, Ref{rand_ctx}, Int, Int), x, state, degree, bits)
  else
    error("randtype not defined")
  end

  return x
end

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(a::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_neg, libflint), Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a)
  return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::QQBarFieldElem, b::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_add, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a, b)
  return z
end

function +(a::QQBarFieldElem, b::QQFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_add_fmpq, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQFieldElem}), z, a, b)
  return z
end

function +(a::QQBarFieldElem, b::ZZRingElem)
  z = QQBarFieldElem()
  ccall((:qqbar_add_fmpz, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{ZZRingElem}), z, a, b)
  return z
end

function +(a::QQBarFieldElem, b::Int)
  z = QQBarFieldElem()
  ccall((:qqbar_add_si, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Int), z, a, b)
  return z
end

+(a::QQFieldElem, b::QQBarFieldElem) = b + a
+(a::ZZRingElem, b::QQBarFieldElem) = b + a
+(a::Int, b::QQBarFieldElem) = b + a

function -(a::QQBarFieldElem, b::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_sub, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a, b)
  return z
end

function -(a::QQBarFieldElem, b::QQFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_sub_fmpq, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQFieldElem}), z, a, b)
  return z
end

function -(a::QQBarFieldElem, b::ZZRingElem)
  z = QQBarFieldElem()
  ccall((:qqbar_sub_fmpz, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{ZZRingElem}), z, a, b)
  return z
end

function -(a::QQBarFieldElem, b::Int)
  z = QQBarFieldElem()
  ccall((:qqbar_sub_si, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Int), z, a, b)
  return z
end

function -(a::QQFieldElem, b::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_fmpq_sub, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQFieldElem}, Ref{QQBarFieldElem}), z, a, b)
  return z
end

function -(a::ZZRingElem, b::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_fmpz_sub, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{ZZRingElem}, Ref{QQBarFieldElem}), z, a, b)
  return z
end

function -(a::Int, b::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_si_sub, libflint), Nothing,
        (Ref{QQBarFieldElem}, Int, Ref{QQBarFieldElem}), z, a, b)
  return z
end

function *(a::QQBarFieldElem, b::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_mul, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a, b)
  return z
end

function *(a::QQBarFieldElem, b::QQFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_mul_fmpq, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQFieldElem}), z, a, b)
  return z
end

function *(a::QQBarFieldElem, b::ZZRingElem)
  z = QQBarFieldElem()
  ccall((:qqbar_mul_fmpz, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{ZZRingElem}), z, a, b)
  return z
end

function *(a::QQBarFieldElem, b::Int)
  z = QQBarFieldElem()
  ccall((:qqbar_mul_si, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Int), z, a, b)
  return z
end

*(a::QQFieldElem, b::QQBarFieldElem) = b * a
*(a::ZZRingElem, b::QQBarFieldElem) = b * a
*(a::Int, b::QQBarFieldElem) = b * a

function ^(a::QQBarFieldElem, b::QQBarFieldElem)
  z = QQBarFieldElem()
  ok = Bool(ccall((:qqbar_pow, libflint), Cint,
                  (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a, b))
  !ok && throw(DomainError((a, b)))
  return z
end

# todo: want qqbar_pow_fmpz, qqbar_pow_fmpq, qqbar_pow_si
^(a::QQBarFieldElem, b::ZZRingElem) = a ^ QQBarFieldElem(b)
^(a::QQBarFieldElem, b::QQFieldElem) = a ^ QQBarFieldElem(b)
^(a::QQBarFieldElem, b::Int) = a ^ QQBarFieldElem(b)
^(a::ZZRingElem, b::QQBarFieldElem) = QQBarFieldElem(a) ^ b
^(a::QQFieldElem, b::QQBarFieldElem) = QQBarFieldElem(a) ^ b
^(a::Int, b::QQBarFieldElem) = QQBarFieldElem(a) ^ b

###############################################################################
#
#   Exact division
#
###############################################################################

function inv(a::QQBarFieldElem)
  iszero(a) && throw(DivideError())
  z = QQBarFieldElem()
  ccall((:qqbar_inv, libflint), Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a)
  return z
end

function divexact(a::QQBarFieldElem, b::QQBarFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  ccall((:qqbar_div, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a, b)
  return z
end

function divexact(a::QQBarFieldElem, b::QQFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  ccall((:qqbar_div_fmpq, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQFieldElem}), z, a, b)
  return z
end

function divexact(a::QQBarFieldElem, b::ZZRingElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  ccall((:qqbar_div_fmpz, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{ZZRingElem}), z, a, b)
  return z
end

function divexact(a::QQBarFieldElem, b::Int; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  ccall((:qqbar_div_si, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Int), z, a, b)
  return z
end

function divexact(a::QQFieldElem, b::QQBarFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  ccall((:qqbar_fmpq_div, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQFieldElem}, Ref{QQBarFieldElem}), z, a, b)
  return z
end

function divexact(a::ZZRingElem, b::QQBarFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  ccall((:qqbar_fmpz_div, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{ZZRingElem}, Ref{QQBarFieldElem}), z, a, b)
  return z
end

function divexact(a::Int, b::QQBarFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  z = QQBarFieldElem()
  ccall((:qqbar_si_div, libflint), Nothing,
        (Ref{QQBarFieldElem}, Int, Ref{QQBarFieldElem}), z, a, b)
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
  ccall((:qqbar_mul_2exp_si, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Int), z, a, b)
  return z
end

function >>(a::QQBarFieldElem, b::Int)
  z = QQBarFieldElem()
  ccall((:qqbar_mul_2exp_si, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Int), z, a, -b)
  return z
end

###############################################################################
#
#   Polynomial evaluation
#
###############################################################################

function evaluate(x::QQPolyRingElem, y::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_evaluate_fmpq_poly, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQPolyRingElem}, Ref{QQBarFieldElem}), z, x, y)
  return z
end

function evaluate(x::ZZPolyRingElem, y::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_evaluate_fmpz_poly, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{ZZPolyRingElem}, Ref{QQBarFieldElem}), z, x, y)
  return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::QQBarFieldElem, b::QQBarFieldElem)
  return Bool(ccall((:qqbar_equal, libflint), Cint,
                    (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), a, b))
end

function cmp(a::QQBarFieldElem, b::QQBarFieldElem)
  !isreal(a) && throw(DomainError(a, "comparing nonreal numbers"))
  !isreal(b) && throw(DomainError(b, "comparing nonreal numbers"))
  return ccall((:qqbar_cmp_re, libflint), Cint,
               (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), a, b)
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
cmp_real(a::QQBarFieldElem, b::QQBarFieldElem) = ccall((:qqbar_cmp_re, libflint),
                                                       Cint, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), a, b)
cmp_imag(a::QQBarFieldElem, b::QQBarFieldElem) = ccall((:qqbar_cmp_im, libflint),
                                                       Cint, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), a, b)
cmpabs(a::QQBarFieldElem, b::QQBarFieldElem) = ccall((:qqbar_cmpabs, libflint),
                                                     Cint, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), a, b)
cmpabs_real(a::QQBarFieldElem, b::QQBarFieldElem) = ccall((:qqbar_cmpabs_re, libflint),
                                                          Cint, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), a, b)
cmpabs_imag(a::QQBarFieldElem, b::QQBarFieldElem) = ccall((:qqbar_cmpabs_im, libflint),
                                                          Cint, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), a, b)
cmp_root_order(a::QQBarFieldElem, b::QQBarFieldElem) = ccall((:qqbar_cmp_root_order, libflint),
                                                             Cint, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), a, b)

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
  ccall((:qqbar_re, libflint), Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a)
  return z
end

@doc raw"""
    imag(a::QQBarFieldElem)

Return the imaginary part of `a`.
"""
function imag(a::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_im, libflint), Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a)
  return z
end

@doc raw"""
    abs(a::QQBarFieldElem)

Return the absolute value of `a`.
"""
function abs(a::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_abs, libflint), Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a)
  return z
end

@doc raw"""
    conj(a::QQBarFieldElem)

Return the complex conjugate of `a`.
"""
function conj(a::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_conj, libflint), Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a)
  return z
end

@doc raw"""
    abs2(a::QQBarFieldElem)

Return the squared absolute value of `a`.
"""
function abs2(a::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_abs2, libflint), Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a)
  return z
end

@doc raw"""
    sign(a::QQBarFieldElem)

Return the complex sign of `a`, defined as zero if `a` is zero
and as $a / |a|$ otherwise.
"""
function sign(a::QQBarFieldElem)
  z = QQBarFieldElem()
  ccall((:qqbar_sgn, libflint), Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a)
  return z
end

@doc raw"""
    csgn(a::QQBarFieldElem)

Return the extension of the real sign function taking the value 1
strictly in the right half plane, -1 strictly in the left half plane,
and the sign of the imaginary part when on the imaginary axis.
Equivalently, $\operatorname{csgn}(x) = x / \sqrt{x^2}$ except that the value is 0
at zero. The value is returned as a Julia integer.
"""
function csgn(a::QQBarFieldElem)
  return QQBarFieldElem(Int(ccall((:qqbar_csgn, libflint), Cint, (Ref{QQBarFieldElem}, ), a)))
end

@doc raw"""
    sign_real(a::QQBarFieldElem)

Return the sign of the real part of `a` as a Julia integer.
"""
function sign_real(a::QQBarFieldElem)
  return QQBarFieldElem(Int(ccall((:qqbar_sgn_re, libflint),
                                  Cint, (Ref{QQBarFieldElem}, ), a)))
end

@doc raw"""
    sign_imag(a::QQBarFieldElem)

Return the sign of the imaginary part of `a` as a Julia integer.
"""
function sign_imag(a::QQBarFieldElem)
  return QQBarFieldElem(Int(ccall((:qqbar_sgn_im, libflint),
                                  Cint, (Ref{QQBarFieldElem}, ), a)))
end

function floor(a::QQBarFieldElem)
  return QQBarFieldElem(floor(ZZRingElem, a))
end

function floor(::Type{ZZRingElem}, a::QQBarFieldElem)
  z = ZZRingElem()
  ccall((:qqbar_floor, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQBarFieldElem}, ), z, a)
  return z
end

function ceil(a::QQBarFieldElem)
  return QQBarFieldElem(ceil(ZZRingElem, a))
end

function ceil(::Type{ZZRingElem}, a::QQBarFieldElem)
  z = ZZRingElem()
  ccall((:qqbar_ceil, libflint), Nothing, (Ref{ZZRingElem}, Ref{QQBarFieldElem}, ), z, a)
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
  ccall((:qqbar_sqrt, libflint),
        Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, a)
  return z
end

@doc raw"""
    root(a::QQBarFieldElem, n::Int)

Return the principal `n`-th root of `a`. Requires positive `n`.
"""
function root(a::QQBarFieldElem, n::Int)
  n <= 0 && throw(DomainError(n))
  z = QQBarFieldElem()
  ccall((:qqbar_root_ui, libflint),
        Nothing, (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, UInt), z, a, n)
  return z
end

function qqbar_vec(n::Int)
  return ccall((:_qqbar_vec_init, libflint), Ptr{qqbar_struct}, (Int,), n)
end

function array(R::QQBarField, v::Ptr{qqbar_struct}, n::Int)
  r = Vector{QQBarFieldElem}(undef, n)
  for i=1:n
    r[i] = R()
    ccall((:qqbar_set, libflint), Nothing, (Ref{QQBarFieldElem}, Ptr{qqbar_struct}),
          r[i], v + (i-1)*sizeof(qqbar_struct))
  end
  return r
end

function qqbar_vec_clear(v::Ptr{qqbar_struct}, n::Int)
  ccall((:_qqbar_vec_clear, libflint),
        Nothing, (Ptr{qqbar_struct}, Int), v, n)
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
    return Array{QQBarFieldElem}(undef, 0)
  end
  roots = qqbar_vec(deg)
  ccall((:qqbar_roots_fmpz_poly, libflint),
        Nothing, (Ptr{qqbar_struct}, Ref{ZZPolyRingElem}, Int), roots, f, 0)
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
    return Array{QQBarFieldElem}(undef, 0)
  end
  roots = qqbar_vec(deg)
  ccall((:qqbar_roots_fmpq_poly, libflint),
        Nothing, (Ptr{qqbar_struct}, Ref{QQPolyRingElem}, Int), roots, f, 0)
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
  ccall((:qqbar_conjugates, libflint),
        Nothing, (Ptr{qqbar_struct}, Ref{QQBarFieldElem}), conjugates, a)
  res = array(parent(a), conjugates, deg)
  qqbar_vec_clear(conjugates, deg)
  return res
end

# Return the eigenvalues with repetition according to the algebraic multiplicity
function _eigvals_internal(R::QQBarField, A::ZZMatrix)
  n = nrows(A)
  !is_square(A) && throw(DomainError(A, "a square matrix is required"))
  if n == 0
    return Array{QQBarFieldElem}(undef, 0)
  end
  roots = qqbar_vec(n)
  ccall((:qqbar_eigenvalues_fmpz_mat, libflint),
        Nothing, (Ptr{qqbar_struct}, Ref{ZZMatrix}, Int), roots, A, 0)
  res = array(R, roots, n)
  qqbar_vec_clear(roots, n)
  return res
end

# Return the eigenvalues with repetition according to the algebraic multiplicity
function _eigvals_internal(R::QQBarField, A::QQMatrix)
  n = nrows(A)
  !is_square(A) && throw(DomainError(A, "a square matrix is required"))
  if n == 0
    return Array{QQBarFieldElem}(undef, 0)
  end
  roots = qqbar_vec(n)
  ccall((:qqbar_eigenvalues_fmpq_mat, libflint),
        Nothing, (Ptr{qqbar_struct}, Ref{QQMatrix}, Int), roots, A, 0)
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
    root_of_unity(C::QQBarField, n::Int)

Return the root of unity $e^{2 \pi i / n}$ as an element of the field
of algebraic numbers `C`.
"""
function root_of_unity(C::QQBarField, n::Int)
  n <= 0 && throw(DomainError(n))
  z = QQBarFieldElem()
  ccall((:qqbar_root_of_unity, libflint),
        Nothing, (Ref{QQBarFieldElem}, Int, UInt), z, 1, n)
  return z
end

@doc raw"""
    root_of_unity(C::QQBarField, n::Int, k::Int)

Return the root of unity $e^{2 \pi i k / n}$ as an element of the field
of algebraic numbers `C`.
"""
function root_of_unity(C::QQBarField, n::Int, k::Int)
  n <= 0 && throw(DomainError(n))
  z = QQBarFieldElem()
  ccall((:qqbar_root_of_unity, libflint),
        Nothing, (Ref{QQBarFieldElem}, Int, UInt), z, k, n)
  return z
end

@doc raw"""
    is_root_of_unity(a::QQBarFieldElem)

Return whether the given algebraic number is a root of unity.
"""
function is_root_of_unity(a::QQBarFieldElem)
  return Bool(ccall((:qqbar_is_root_of_unity, libflint),
                    Cint, (Ptr{Int}, Ptr{Int}, Ref{QQBarFieldElem}), C_NULL, C_NULL, a))
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
  if !Bool(ccall((:qqbar_is_root_of_unity, libflint),
                 Cint, (Ptr{Int}, Ptr{Int}, Ref{QQBarFieldElem}), p, q, a))
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
  ccall((:qqbar_exp_pi_i, libflint),
        Nothing, (Ref{QQBarFieldElem}, Int, Int), z, p, q)
  return z
end

@doc raw"""
    sinpi(a::QQBarFieldElem)

Return $\sin(\pi a)$ as an algebraic number.
Throws if this value is transcendental.

# Examples

```jldoctest
julia> x = sinpi(QQBar(1)//3)
Root 0.866025 of 4x^2 - 3

julia> sinpi(x)
ERROR: DomainError with Root 0.866025 of 4x^2 - 3:
nonrational algebraic number
```
"""
function sinpi(a::QQBarFieldElem)
  r = QQFieldElem(a)
  p = Int(numerator(r))
  q = Int(denominator(r))
  z = QQBarFieldElem()
  ccall((:qqbar_sin_pi, libflint), Nothing, (Ref{QQBarFieldElem}, Int, Int), z, p, q)
  return z
end

@doc raw"""
    cospi(a::QQBarFieldElem)

Return $\cos(\pi a)$ as an algebraic number.
Throws if this value is transcendental.

# Examples

```jldoctest
julia> x = cospi(QQBar(1)//6)
Root 0.866025 of 4x^2 - 3

julia> cospi(x)
ERROR: DomainError with Root 0.866025 of 4x^2 - 3:
nonrational algebraic number
```
"""
function cospi(a::QQBarFieldElem)
  r = QQFieldElem(a)
  p = Int(numerator(r))
  q = Int(denominator(r))
  z = QQBarFieldElem()
  ccall((:qqbar_cos_pi, libflint), Nothing, (Ref{QQBarFieldElem}, Int, Int), z, p, q)
  return z
end

@doc raw"""
    sincospi(a::QQBarFieldElem)

Return $\sin(\pi a)$ and $\cos(\pi a)$ as a pair of algebraic numbers.
Throws if either value is transcendental.

# Examples

```jldoctest
julia> s, c = sincospi(QQBar(1)//3)
(Root 0.866025 of 4x^2 - 3, Root 0.500000 of 2x - 1)

julia> sincospi(s)
ERROR: DomainError with Root 0.866025 of 4x^2 - 3:
nonrational algebraic number
```
"""
function sincospi(a::QQBarFieldElem)
  r = QQFieldElem(a)
  p = Int(numerator(r))
  q = Int(denominator(r))
  s = QQBarFieldElem()
  ccall((:qqbar_sin_pi, libflint), Nothing, (Ref{QQBarFieldElem}, Int, Int), s, p, q)
  c = QQBarFieldElem()
  ccall((:qqbar_cos_pi, libflint), Nothing, (Ref{QQBarFieldElem}, Int, Int), c, p, q)
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
  if !Bool(ccall((:qqbar_tan_pi, libflint),
                 Cint, (Ref{QQBarFieldElem}, Int, Int), z, p, q))
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
  if !Bool(ccall((:qqbar_atan_pi, libflint),
                 Cint, (Ptr{Int}, Ptr{Int}, Ref{QQBarFieldElem}), p, q, a))
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
  if !Bool(ccall((:qqbar_asin_pi, libflint),
                 Cint, (Ptr{Int}, Ptr{Int}, Ref{QQBarFieldElem}), p, q, a))
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
  if !Bool(ccall((:qqbar_acos_pi, libflint),
                 Cint, (Ptr{Int}, Ptr{Int}, Ref{QQBarFieldElem}), p, q, a))
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
  if !Bool(ccall((:qqbar_log_pi_i, libflint),
                 Cint, (Ptr{Int}, Ptr{Int}, Ref{QQBarFieldElem}), p, q, a))
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
  found = Bool(ccall((:qqbar_guess, libflint),
                     Cint, (Ref{QQBarFieldElem}, Ref{T}, Int, Int, Int, Int),
                     res, x, maxdeg, maxbits, 0, prec))
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
  CC = ComplexField()
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
  ccall((:qqbar_get_arb, libflint),
        Nothing, (Ref{ArbFieldElem}, Ref{QQBarFieldElem}, Int), z, a, prec)
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
  ccall((:qqbar_get_acb, libflint),
        Nothing, (Ref{AcbFieldElem}, Ref{QQBarFieldElem}, Int), z, a, prec)
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
  ccall((:qqbar_get_arb, libflint),
        Nothing, (Ref{RealFieldElem}, Ref{QQBarFieldElem}, Int), z, a, prec)
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
  ccall((:qqbar_get_acb, libflint),
        Nothing, (Ref{ComplexFieldElem}, Ref{QQBarFieldElem}, Int), z, a, prec)
  return z
end

# todo: provide qqbar_get_fmpq, qqbar_get_fmpz in C
@doc raw"""
    QQFieldElem(a::QQBarFieldElem)

Convert `a` to a rational number of type `QQFieldElem`.
Throws if `a` is not a rational number.
"""
function QQFieldElem(a::QQBarFieldElem)
  !is_rational(a) && throw(DomainError(a, "nonrational algebraic number"))
  p = ZZRingElem()
  q = ZZRingElem()
  ccall((:fmpz_poly_get_coeff_fmpz, libflint),
        Nothing, (Ref{ZZRingElem}, Ref{QQBarFieldElem}, Int), p, a, 0)
  ccall((:fmpz_poly_get_coeff_fmpz, libflint),
        Nothing, (Ref{ZZRingElem}, Ref{QQBarFieldElem}, Int), q, a, 1)
  ccall((:fmpz_neg, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}), p, p)
  return p // q
end

@doc raw"""
    ZZRingElem(a::QQBarFieldElem)

Convert `a` to an integer of type `ZZRingElem`.
Throws if `a` is not an integer.
"""
function ZZRingElem(a::QQBarFieldElem)
  !isinteger(a) && throw(DomainError(a, "noninteger algebraic number"))
  z = ZZRingElem()
  ccall((:fmpz_poly_get_coeff_fmpz, libflint),
        Nothing, (Ref{ZZRingElem}, Ref{QQBarFieldElem}, Int), z, a, 0)
  ccall((:fmpz_neg, libflint), Nothing, (Ref{ZZRingElem}, Ref{ZZRingElem}), z, z)
  return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::QQBarFieldElem)
  ccall((:qqbar_zero, libflint), Nothing, (Ref{QQBarFieldElem},), z)
  return z
end

function mul!(z::QQBarFieldElem, x::QQBarFieldElem, y::QQBarFieldElem)
  ccall((:qqbar_mul, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, x, y)
  return z
end

function addeq!(z::QQBarFieldElem, x::QQBarFieldElem)
  ccall((:qqbar_add, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, z, x)
  return z
end

function add!(z::QQBarFieldElem, x::QQBarFieldElem, y::QQBarFieldElem)
  ccall((:qqbar_add, libflint), Nothing,
        (Ref{QQBarFieldElem}, Ref{QQBarFieldElem}, Ref{QQBarFieldElem}), z, x, y)
  return z
end

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
Field of algebraic numbers

julia> sqrt(K(2))
Root 1.41421 of x^2 - 2
```
"""
algebraic_closure(::QQField) = QQBar
