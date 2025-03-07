###############################################################################
#
#   nf_elem.jl : Antic number fields
#
###############################################################################

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{AbsSimpleNumFieldElem}) = AbsSimpleNumField

@doc raw"""
    parent(a::AbsSimpleNumFieldElem)

Return the parent of the given number field element.
"""
parent(a::AbsSimpleNumFieldElem) = a.parent

elem_type(::Type{AbsSimpleNumField}) = AbsSimpleNumFieldElem

base_ring_type(::Type{AbsSimpleNumField}) = typeof(Union{})

base_ring(a::AbsSimpleNumField) = Union{}

is_domain_type(::Type{AbsSimpleNumFieldElem}) = true

@doc raw"""
    var(a::AbsSimpleNumField)

Returns the identifier (as a symbol, not a string), that is used for printing
the generator of the given number field.
"""
var(a::AbsSimpleNumField) = a.S

characteristic(::AbsSimpleNumField) = 0

defining_polynomial(K::AbsSimpleNumField) = K.pol

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function hash(a::AbsSimpleNumFieldElem, h::UInt)
  b = 0xc2a44fbe466a1827%UInt
  d = degree(parent(a))
  GC.@preserve a begin
    aptr = reinterpret(Ptr{Int}, pointer_from_objref(a))
    if d < 2
      den = unsafe_load(aptr, 2)
      b = _hash_integer(den, b)
      num = unsafe_load(aptr, 1)
      b = bitrotate(xor(b, xor(_hash_integer(num, h), h)), -1)
    elseif d == 2
      den = unsafe_load(aptr, 4)
      b = _hash_integer(den, b)
      num0 = unsafe_load(aptr, 1)
      b = bitrotate(xor(b, xor(_hash_integer(num0, h), h)), -1)
      num1 = unsafe_load(aptr, 2)
      b = bitrotate(xor(b, xor(_hash_integer(num1, h), h)), -1)
    else
      b = _hash_integer(a.elem_den, b)
      for i in 1:a.elem_length
        num = unsafe_load(Ptr{Int}(a.elem_coeffs), i)
        b = bitrotate(xor(b, xor(_hash_integer(num, h), h)), -1)
      end
      for i in a.elem_length+1:d
        b = bitrotate(xor(b, xor(_hash_integer(0, h), h)), -1)
      end
    end
  end
  return b
end

@doc raw"""
    coeff(x::AbsSimpleNumFieldElem, n::Int)

Return the $n$-th coefficient of the polynomial representation of the given
number field element. Coefficients are numbered from $0$, starting with the
constant coefficient.
"""
function coeff(x::AbsSimpleNumFieldElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  z = QQFieldElem()
  @ccall libflint.nf_elem_get_coeff_fmpq(z::Ref{QQFieldElem}, x::Ref{AbsSimpleNumFieldElem}, n::Int, parent(x)::Ref{AbsSimpleNumField})::Nothing
  return z
end

function num_coeff!(z::ZZRingElem, x::AbsSimpleNumFieldElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  @ccall libflint.nf_elem_get_coeff_fmpz(z::Ref{ZZRingElem}, x::Ref{AbsSimpleNumFieldElem}, n::Int, parent(x)::Ref{AbsSimpleNumField})::Nothing
  return z
end

@doc raw"""
    gen(a::AbsSimpleNumField)

Return the generator of the given number field, i.e., a symbolic root of the
defining polynomial.
"""
function gen(a::AbsSimpleNumField)
  r = AbsSimpleNumFieldElem(a)
  @ccall libflint.nf_elem_gen(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumField})::Nothing
  return r
end

one(a::AbsSimpleNumField) = one!(AbsSimpleNumFieldElem(a))

zero(a::AbsSimpleNumField) = zero!(AbsSimpleNumFieldElem(a))

@doc raw"""
    is_gen(a::AbsSimpleNumFieldElem)

Return `true` if the given number field element is the generator of the
number field, otherwise return `false`.
"""
function is_gen(a::AbsSimpleNumFieldElem)
  return @ccall libflint.nf_elem_is_gen(a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Bool
end

function isone(a::AbsSimpleNumFieldElem)
  return @ccall libflint.nf_elem_is_one(a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Bool
end

function iszero(a::AbsSimpleNumFieldElem)
  return @ccall libflint.nf_elem_is_zero(a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Bool
end

@doc raw"""
    isinteger(a::AbsSimpleNumFieldElem)

Return `true` if the given number field element is an integer, i.e., in ZZ, otherwise
return `false`.
"""
function isinteger(a::AbsSimpleNumFieldElem)
  b = @ccall libflint.nf_elem_is_integer(a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Cint
  return Bool(b)
end

@doc raw"""
    is_rational(a::AbsSimpleNumFieldElem)

Return `true` if the given number field element is a rational number, i.e., in QQ,
otherwise `false`.
"""
function is_rational(a::AbsSimpleNumFieldElem)
  b = @ccall libflint.nf_elem_is_rational(a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Cint
  return Bool(b)
end

@doc raw"""
    denominator(a::AbsSimpleNumFieldElem)

Return the denominator of the polynomial representation of the given number
field element.
"""
function denominator(a::AbsSimpleNumFieldElem)
  z = ZZRingElem()
  @ccall libflint.nf_elem_get_den(z::Ref{ZZRingElem}, a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return z
end

function elem_from_mat_row(a::AbsSimpleNumField, b::ZZMatrix, i::Int, d::ZZRingElem)
  _checkbounds(nrows(b), i) || throw(BoundsError())
  ncols(b) == degree(a) || error("Wrong number of columns")
  z = a()
  @ccall libflint.nf_elem_set_fmpz_mat_row(z::Ref{AbsSimpleNumFieldElem}, b::Ref{ZZMatrix}, (i - 1)::Int, d::Ref{ZZRingElem}, a::Ref{AbsSimpleNumField})::Nothing
  return z
end

function elem_to_mat_row!(a::ZZMatrix, i::Int, d::ZZRingElem, b::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_get_fmpz_mat_row(a::Ref{ZZMatrix}, (i - 1)::Int, d::Ref{ZZRingElem}, b::Ref{AbsSimpleNumFieldElem}, b.parent::Ref{AbsSimpleNumField})::Nothing
  nothing
end

@doc raw"""
    degree(a::AbsSimpleNumField)

Return the degree of the given number field, i.e. the degree of its
defining polynomial.
"""
degree(a::AbsSimpleNumField) = a.pol_length-1

function deepcopy_internal(d::AbsSimpleNumFieldElem, dict::IdDict)
  z = AbsSimpleNumFieldElem(parent(d), d)
  return z
end

Base.copy(d::AbsSimpleNumFieldElem) = deepcopy(d)

function is_cyclo_type(K::AbsSimpleNumField)
  return has_attribute(K, :cyclo)
end

function is_maxreal_type(K::AbsSimpleNumField)
  return get_attribute(K, :maxreal)::Bool
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function Base.show(io::IO, ::MIME"text/plain", a::AbsSimpleNumField)
  @show_name(io, a)
  @show_special(io, MIME"text/plain"(), a)
  println(io, "Number field with defining polynomial ", defining_polynomial(a))
  io = pretty(io)
  print(io, Indent(), "over ", Lowercase(), QQ, Dedent())
end

function Base.show(io::IO, a::AbsSimpleNumField)
  @show_name(io, a)
  @show_special(io, a)
  if is_terse(io)
    print(io, "Number field")
  else
    io = pretty(io)
    print(io, "Number field of degree $(degree(a))")
    print(terse(io), " over ", Lowercase(), QQ)
  end
end

function expressify(a::AbsSimpleNumFieldElem; context = nothing)
  return expressify(parent(parent(a).pol)(a), var(parent(a)), context = context)
end

function Base.show(io::IO, a::AbsSimpleNumFieldElem)
  print(io, AbstractAlgebra.obj_to_string(a, context = io))
end

canonical_unit(x::AbsSimpleNumFieldElem) = x

###############################################################################
#
#   Unary operators
#
###############################################################################

-(a::AbsSimpleNumFieldElem) = neg!(a.parent(), a)

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)
  parent(a) == parent(b) || return force_op(+, a, b)::AbsSimpleNumFieldElem
  check_parent(a, b)
  r = a.parent()
  return add!(r, a, b)
end

function -(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)
  parent(a) == parent(b) || return force_op(-, a, b)::AbsSimpleNumFieldElem
  check_parent(a, b)
  r = a.parent()
  return sub!(r, a, b)
end

function *(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)
  parent(a) == parent(b) || return force_op(*, a, b)::AbsSimpleNumFieldElem
  check_parent(a, b)
  r = a.parent()
  return mul!(r, a, b)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function +(a::AbsSimpleNumFieldElem, b::Int)
  r = a.parent()
  @ccall libflint.nf_elem_add_si(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Int, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function +(a::AbsSimpleNumFieldElem, b::ZZRingElem)
  r = a.parent()
  @ccall libflint.nf_elem_add_fmpz(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{ZZRingElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function +(a::AbsSimpleNumFieldElem, b::QQFieldElem)
  r = a.parent()
  @ccall libflint.nf_elem_add_fmpq(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{QQFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function -(a::AbsSimpleNumFieldElem, b::Int)
  r = a.parent()
  @ccall libflint.nf_elem_sub_si(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Int, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function -(a::AbsSimpleNumFieldElem, b::ZZRingElem)
  r = a.parent()
  @ccall libflint.nf_elem_sub_fmpz(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{ZZRingElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function -(a::AbsSimpleNumFieldElem, b::QQFieldElem)
  r = a.parent()
  @ccall libflint.nf_elem_sub_fmpq(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{QQFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function -(a::Int, b::AbsSimpleNumFieldElem)
  r = b.parent()
  @ccall libflint.nf_elem_si_sub(r::Ref{AbsSimpleNumFieldElem}, a::Int, b::Ref{AbsSimpleNumFieldElem}, b.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function -(a::ZZRingElem, b::AbsSimpleNumFieldElem)
  r = b.parent()
  @ccall libflint.nf_elem_fmpz_sub(r::Ref{AbsSimpleNumFieldElem}, a::Ref{ZZRingElem}, b::Ref{AbsSimpleNumFieldElem}, b.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function -(a::QQFieldElem, b::AbsSimpleNumFieldElem)
  r = b.parent()
  @ccall libflint.nf_elem_fmpq_sub(r::Ref{AbsSimpleNumFieldElem}, a::Ref{QQFieldElem}, b::Ref{AbsSimpleNumFieldElem}, b.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

+(a::AbsSimpleNumFieldElem, b::Integer) = a + flintify(b)

-(a::AbsSimpleNumFieldElem, b::Integer) = a - flintify(b)

-(a::Integer, b::AbsSimpleNumFieldElem) = flintify(a) - b

+(a::Integer, b::AbsSimpleNumFieldElem) = b + a

+(a::QQFieldElem, b::AbsSimpleNumFieldElem) = b + a

+(a::Rational, b::AbsSimpleNumFieldElem) = QQFieldElem(a) + b

+(a::AbsSimpleNumFieldElem, b::Rational) = b + a

-(a::Rational, b::AbsSimpleNumFieldElem) = QQFieldElem(a) - b

-(a::AbsSimpleNumFieldElem, b::Rational) = a - QQFieldElem(b)

function *(a::AbsSimpleNumFieldElem, b::Int)
  r = a.parent()
  @ccall libflint.nf_elem_scalar_mul_si(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Int, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function *(a::AbsSimpleNumFieldElem, b::ZZRingElem)
  r = a.parent()
  @ccall libflint.nf_elem_scalar_mul_fmpz(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{ZZRingElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function *(a::AbsSimpleNumFieldElem, b::QQFieldElem)
  r = a.parent()
  @ccall libflint.nf_elem_scalar_mul_fmpq(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{QQFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function *(a::Rational, b::AbsSimpleNumFieldElem)
  return QQFieldElem(a) * b
end

*(a::AbsSimpleNumFieldElem, b::Rational) = b * a

*(a::AbsSimpleNumFieldElem, b::Integer) = a * flintify(b)

*(a::Integer, b::AbsSimpleNumFieldElem) = b * a

*(a::ZZRingElem, b::AbsSimpleNumFieldElem) = b * a

*(a::QQFieldElem, b::AbsSimpleNumFieldElem) = b * a

//(a::AbsSimpleNumFieldElem, b::Int) = divexact(a, b)

//(a::AbsSimpleNumFieldElem, b::ZZRingElem) = divexact(a, b)

//(a::AbsSimpleNumFieldElem, b::Integer) = a//flintify(b)

//(a::AbsSimpleNumFieldElem, b::QQFieldElem) = divexact(a, b)

//(a::Integer, b::AbsSimpleNumFieldElem) = divexact(a, b)

//(a::ZZRingElem, b::AbsSimpleNumFieldElem) = divexact(a, b)

//(a::QQFieldElem, b::AbsSimpleNumFieldElem) = divexact(a, b)

//(a::Rational, b::AbsSimpleNumFieldElem) = divexact(QQFieldElem(a), b)

//(a::AbsSimpleNumFieldElem, b::Rational) = divexact(a, QQFieldElem(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::AbsSimpleNumFieldElem, n::Int)
  z = pow!(parent(a)(), a, abs(n))
  if n < 0
    z = inv!(z)
  end
  return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)
  parent(a) == parent(b) || return force_op(==, a, b)::Bool
  check_parent(a, b)
  return @ccall libflint.nf_elem_equal(a::Ref{AbsSimpleNumFieldElem}, b::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Bool
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::AbsSimpleNumFieldElem, b::ZZRingElem)
  b = @ccall libflint.nf_elem_equal_fmpz(a::Ref{AbsSimpleNumFieldElem}, b::Ref{ZZRingElem}, a.parent::Ref{AbsSimpleNumField})::Cint
  return Bool(b)
end

function ==(a::AbsSimpleNumFieldElem, b::QQFieldElem)
  b = @ccall libflint.nf_elem_equal_fmpq(a::Ref{AbsSimpleNumFieldElem}, b::Ref{QQFieldElem}, a.parent::Ref{AbsSimpleNumField})::Cint
  return Bool(b)
end

function ==(a::AbsSimpleNumFieldElem, b::Int)
  b = @ccall libflint.nf_elem_equal_si(a::Ref{AbsSimpleNumFieldElem}, b::Int, a.parent::Ref{AbsSimpleNumField})::Cint
  return Bool(b)
end

function ==(a::AbsSimpleNumFieldElem, b::UInt)
  b = @ccall libflint.nf_elem_equal_ui(a::Ref{AbsSimpleNumFieldElem}, b::UInt, a.parent::Ref{AbsSimpleNumField})::Cint
  return Bool(b)
end

==(a::AbsSimpleNumFieldElem, b::Integer) = a == flintify(b)

==(a::AbsSimpleNumFieldElem, b::Rational) = a == QQFieldElem(b)

==(a::ZZRingElem, b::AbsSimpleNumFieldElem) = b == a

==(a::QQFieldElem, b::AbsSimpleNumFieldElem) = b == a

==(a::Int, b::AbsSimpleNumFieldElem) = b == a

==(a::UInt, b::AbsSimpleNumFieldElem) = b == a

==(a::Integer, b::AbsSimpleNumFieldElem) = b == a

==(a::Rational, b::AbsSimpleNumFieldElem) = b == a

###############################################################################
#
#   Inversion
#
###############################################################################

@doc raw"""
    inv(a::AbsSimpleNumFieldElem)

Return $a^{-1}$. Requires $a \neq 0$.
"""
function inv(a::AbsSimpleNumFieldElem)
  iszero(a) && throw(DivideError())
  r = a.parent()
  @ccall libflint.nf_elem_inv(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  parent(a) == parent(b) || return force_op(divexact, a, b)::AbsSimpleNumFieldElem
  check_parent(a, b)
  r = a.parent()
  @ccall libflint.nf_elem_div(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(a::AbsSimpleNumFieldElem, b::Int; check::Bool=true)
  b == 0 && throw(DivideError())
  r = a.parent()
  @ccall libflint.nf_elem_scalar_div_si(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Int, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

function divexact(a::AbsSimpleNumFieldElem, b::ZZRingElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  r = a.parent()
  @ccall libflint.nf_elem_scalar_div_fmpz(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{ZZRingElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

divexact(a::AbsSimpleNumFieldElem, b::Integer; check::Bool=true) = divexact(a, flintify(b); check=check)

function divexact(a::AbsSimpleNumFieldElem, b::QQFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  r = a.parent()
  @ccall libflint.nf_elem_scalar_div_fmpq(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{QQFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return r
end

divexact(a::Integer, b::AbsSimpleNumFieldElem; check::Bool=true) = inv(b)*a

divexact(a::ZZRingElem, b::AbsSimpleNumFieldElem; check::Bool=true) = inv(b)*a

divexact(a::QQFieldElem, b::AbsSimpleNumFieldElem; check::Bool=true) = inv(b)*a

###############################################################################
#
#   Ad hoc division
#
###############################################################################

#to make the MPoly module happy, divrem needs it...
function Base.div(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)
  return a // b
end

function rem(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)
  return parent(a)(0)
end

###############################################################################
#
#   Removal and valuation
#
###############################################################################

@doc raw"""
    divides(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)

Returns a pair consisting of a flag which is set to `true` if $b$ divides
$a$ and `false` otherwise, and a number field element $h$ such that $a = bh$
if such exists. If not, the value of $h$ is undetermined.
"""
function divides(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)
  if iszero(a)
    return true, zero(parent(a))
  end
  if iszero(b)
    return false, zero(parent(a))
  end
  return true, divexact(a, b)
end

###############################################################################
#
#   Norm and trace
#
###############################################################################

@doc raw"""
    norm(a::AbsSimpleNumFieldElem)

Return the absolute norm of $a$. The result will be a rational number.
"""
function norm(a::AbsSimpleNumFieldElem)
  z = QQFieldElem()
  @ccall libflint.nf_elem_norm(z::Ref{QQFieldElem}, a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return z
end

@doc raw"""
    tr(a::AbsSimpleNumFieldElem)

Return the absolute trace of $a$. The result will be a rational number.
"""
function tr(a::AbsSimpleNumFieldElem)
  z = QQFieldElem()
  @ccall libflint.nf_elem_trace(z::Ref{QQFieldElem}, a::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return z
end

@doc raw"""
    representation_matrix(a::AbsSimpleNumFieldElem)

Return a matrix with rational entries representing multiplication with $a$
with respect to the power basis of the generator of the parent of $a$.
The matrix is of type QQMatrix.
"""
function representation_matrix(a::AbsSimpleNumFieldElem)
  K = parent(a)
  z = QQMatrix(degree(K), degree(K))
  @ccall libflint.nf_elem_rep_mat(z::Ref{QQMatrix}, a::Ref{AbsSimpleNumFieldElem}, K::Ref{AbsSimpleNumField})::Nothing
  return z
end

@doc raw"""
    representation_matrix_q(a::AbsSimpleNumFieldElem)

Return a matrix  representing multiplication with $a$ with respect to the
power basis of the generator of the parent of $a$.
The matrix is returned as a tuple (ZZMatrix, ZZRingElem), consisting of the
a primitive integer matrix and a denominator.
"""
function representation_matrix_q(a::AbsSimpleNumFieldElem)
  K = parent(a)
  z = ZZMatrix(degree(K), degree(K))
  d = ZZRingElem()
  @ccall libflint.nf_elem_rep_mat_fmpz_mat_den(z::Ref{ZZMatrix}, d::Ref{ZZRingElem}, a::Ref{AbsSimpleNumFieldElem}, K::Ref{AbsSimpleNumField})::Nothing
  return z, d
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_zero(a::Ref{AbsSimpleNumFieldElem}, parent(a)::Ref{AbsSimpleNumField})::Nothing
  return a
end

function one!(a::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_one(a::Ref{AbsSimpleNumFieldElem}, parent(a)::Ref{AbsSimpleNumField})::Nothing
  return a
end

function neg!(z::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_neg(z::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, parent(a)::Ref{AbsSimpleNumField})::Nothing
  return z
end

function mul!(z::AbsSimpleNumFieldElem, x::AbsSimpleNumFieldElem, y::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_mul(z::Ref{AbsSimpleNumFieldElem}, x::Ref{AbsSimpleNumFieldElem}, y::Ref{AbsSimpleNumFieldElem}, parent(x)::Ref{AbsSimpleNumField})::Nothing
  return z
end

@doc raw"""
    mul_red!(z::AbsSimpleNumFieldElem, x::AbsSimpleNumFieldElem, y::AbsSimpleNumFieldElem, red::Bool)

Multiply $x$ by $y$ and set the existing number field element $z$ to the
result. Reduction modulo the defining polynomial is only performed if `red` is
set to `true`. Note that $x$ and $y$ must be reduced. This function is provided
for performance reasons as it saves allocating a new object for the result and
eliminates associated garbage collection.
"""
function mul_red!(z::AbsSimpleNumFieldElem, x::AbsSimpleNumFieldElem, y::AbsSimpleNumFieldElem, red::Bool)
  @ccall libflint.nf_elem_mul_red(z::Ref{AbsSimpleNumFieldElem}, x::Ref{AbsSimpleNumFieldElem}, y::Ref{AbsSimpleNumFieldElem}, parent(x)::Ref{AbsSimpleNumField}, red::Cint)::Nothing
  return z
end

function add!(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem, c::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_add(a::Ref{AbsSimpleNumFieldElem}, b::Ref{AbsSimpleNumFieldElem}, c::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return a
end

@doc raw"""
    reduce!(x::AbsSimpleNumFieldElem)

Reduce the given number field element by the defining polynomial, in-place.
This only needs to be done after accumulating values computed by `mul_red!`
where reduction has not been performed. All standard Nemo number field
functions automatically reduce their outputs.
"""
function reduce!(x::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_reduce(x::Ref{AbsSimpleNumFieldElem}, parent(x)::Ref{AbsSimpleNumField})::Nothing
  return x
end

#

function pow!(z::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, n::Integer)
  @ccall libflint.nf_elem_pow(z::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, n::UInt, a.parent::Ref{AbsSimpleNumField})::Nothing
  return z
end

###############################################################################
#
#   Ad hoc unsafe functions
#
###############################################################################

function add!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::QQFieldElem)
  @ccall libflint.nf_elem_add_fmpq(c::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{QQFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return c
end

function add!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::ZZRingElem)
  @ccall libflint.nf_elem_add_fmpz(c::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{ZZRingElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return c
end

function add!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Int)
  @ccall libflint.nf_elem_add_si(c::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Int, a.parent::Ref{AbsSimpleNumField})::Nothing
  return c
end

add!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Integer) = add!(c, a, flintify(b))

function sub!(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem, c::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_sub(a::Ref{AbsSimpleNumFieldElem}, b::Ref{AbsSimpleNumFieldElem}, c::Ref{AbsSimpleNumFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return a
end

function sub!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::QQFieldElem)
  @ccall libflint.nf_elem_sub_fmpq(c::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{QQFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return c
end

function sub!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::ZZRingElem)
  @ccall libflint.nf_elem_sub_fmpz(c::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{ZZRingElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return c
end

function sub!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Int)
  @ccall libflint.nf_elem_sub_si(c::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Int, a.parent::Ref{AbsSimpleNumField})::Nothing
  return c
end

sub!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Integer) = sub!(c, a, flintify(b))

function sub!(c::AbsSimpleNumFieldElem, a::QQFieldElem, b::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_fmpq_sub(c::Ref{AbsSimpleNumFieldElem}, a::Ref{QQFieldElem}, b::Ref{AbsSimpleNumFieldElem}, parent(b)::Ref{AbsSimpleNumField})::Nothing
  return c
end

function sub!(c::AbsSimpleNumFieldElem, a::ZZRingElem, b::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_fmpz_sub(c::Ref{AbsSimpleNumFieldElem}, a::Ref{ZZRingElem}, b::Ref{AbsSimpleNumFieldElem}, parent(b)::Ref{AbsSimpleNumField})::Nothing
  return c
end

function sub!(c::AbsSimpleNumFieldElem, a::Int, b::AbsSimpleNumFieldElem)
  @ccall libflint.nf_elem_si_sub(c::Ref{AbsSimpleNumFieldElem}, a::Int, b::Ref{AbsSimpleNumFieldElem}, b.parent::Ref{AbsSimpleNumField})::Nothing
  return c
end

sub!(c::AbsSimpleNumFieldElem, a::Integer, b::AbsSimpleNumFieldElem) = sub!(c, flintify(a), b)

function mul!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::QQFieldElem)
  @ccall libflint.nf_elem_scalar_mul_fmpq(c::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{QQFieldElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return c
end

function mul!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::ZZRingElem)
  @ccall libflint.nf_elem_scalar_mul_fmpz(c::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{ZZRingElem}, a.parent::Ref{AbsSimpleNumField})::Nothing
  return c
end

function mul!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Int)
  @ccall libflint.nf_elem_scalar_mul_si(c::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Int, a.parent::Ref{AbsSimpleNumField})::Nothing
  return c
end

mul!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Integer) = mul!(c, a, flintify(b))

function divexact!(z::AbsSimpleNumFieldElem, x::AbsSimpleNumFieldElem, y::Int)
  @ccall libflint.nf_elem_scalar_div_si(r::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Int, parent(x)::Ref{AbsSimpleNumField})::Nothing
  return z
end

function divexact!(z::AbsSimpleNumFieldElem, x::AbsSimpleNumFieldElem, y::ZZRingElem)
  @ccall libflint.nf_elem_scalar_div_fmpz(z::Ref{AbsSimpleNumFieldElem}, x::Ref{AbsSimpleNumFieldElem}, y::Ref{ZZRingElem}, parent(x)::Ref{AbsSimpleNumField})::Nothing
  return z
end

function divexact!(z::AbsSimpleNumFieldElem, x::AbsSimpleNumFieldElem, y::QQFieldElem)
  @ccall libflint.nf_elem_scalar_div_fmpq(z::Ref{AbsSimpleNumFieldElem}, x::Ref{AbsSimpleNumFieldElem}, y::Ref{QQFieldElem}, parent(x)::Ref{AbsSimpleNumField})::Nothing
  return z
end

divexact!(z::AbsSimpleNumFieldElem, x::AbsSimpleNumFieldElem, y::RationalUnion) = divexact!(z, x, flintify(y))

###############################################################################
#
#   Speedups for polynomials over number fields
#
###############################################################################

function sqr_classical(a::Generic.Poly{AbsSimpleNumFieldElem})
  lena = length(a)

  t = base_ring(a)()

  lenz = 2*lena - 1
  d = Vector{AbsSimpleNumFieldElem}(undef, lenz)

  for i = 1:lena - 1
    d[2i - 1] = base_ring(a)()
    d[2i] = base_ring(a)()
    d[2i - 1] = mul_red!(d[2i - 1], coeff(a, i - 1), coeff(a, i - 1), false)
  end
  d[2*lena - 1] = base_ring(a)()
  d[2*lena - 1] = mul_red!(d[2*lena - 1], coeff(a, lena - 1), coeff(a, lena - 1), false)

  for i = 1:lena
    for j = i + 1:lena
      t = mul_red!(t, coeff(a, i - 1), coeff(a, j - 1), false)
      d[i + j - 1] = add!(d[i + j - 1], t)
      d[i + j - 1] = add!(d[i + j - 1], t)
    end
  end

  for i = 1:lenz
    d[i] = reduce!(d[i])
  end

  z = parent(a)(d)

  z = set_length!(z, normalise(z, lenz))

  return z
end

function mul_classical(a::Generic.Poly{AbsSimpleNumFieldElem}, b::Generic.Poly{AbsSimpleNumFieldElem})
  check_parent(a, b)
  lena = length(a)
  lenb = length(b)

  if lena == 0 || lenb == 0
    return parent(a)()
  end

  if a == b
    return sqr_classical(a)
  end

  t = base_ring(a)()

  lenz = lena + lenb - 1
  d = Vector{AbsSimpleNumFieldElem}(undef, lenz)

  for i = 1:lena
    d[i] = base_ring(a)()
    d[i] = mul_red!(d[i], coeff(a, i - 1), coeff(b, 0), false)
  end

  for i = 2:lenb
    d[lena + i - 1] = base_ring(a)()
    d[lena + i - 1] = mul_red!(d[lena + i - 1], a.coeffs[lena], coeff(b, i - 1), false)
  end

  for i = 1:lena - 1
    for j = 2:lenb
      t = mul_red!(t, coeff(a, i - 1), b.coeffs[j], false)
      d[i + j - 1] = add!(d[i + j - 1], t)
    end
  end

  for i = 1:lenz
    d[i] = reduce!(d[i])
  end

  z = parent(a)(d)

  z = set_length!(z, normalise(z, lenz))

  return z
end

function use_karamul(a::Generic.Poly{AbsSimpleNumFieldElem}, b::Generic.Poly{AbsSimpleNumFieldElem})
  deg = degree(base_ring(a))
  if deg > 25
    return true
  end
  bits = 0
  for i = 1:length(a)
    cbits = 0
    for j = 0:deg
      c = coeff(coeff(a, i - 1), j)
      cbits += nbits(numerator(c))
      cbits += nbits(denominator(c))
    end
    bits += div(cbits, deg + 1)
  end
  for i = 1:length(b)
    cbits = 0
    for j = 0:deg
      c = coeff(coeff(b, i - 1), j)
      cbits += nbits(numerator(c))
      cbits += nbits(denominator(c))
    end
    bits += div(cbits, deg + 1)
  end
  minlen = min(length(a), length(b))
  return minlen*div(bits, 2*(length(a) + length(b))) > 100
end

function *(a::Generic.Poly{AbsSimpleNumFieldElem}, b::Generic.Poly{AbsSimpleNumFieldElem})
  check_parent(a, b)
  # karatsuba recurses on this, so check lengths are > 1
  if length(a) > 1 && length(b) > 1 && use_karamul(a, b)
    return mul_karatsuba(a, b)
  end
  lena = length(a)
  lenb = length(b)
  if min(lena, lenb) < 20
    return mul_classical(a, b)
  end
  lenr = lena + lenb - 1
  r = parent(a)()
  if lena == 0 || lenb == 0
    return r
  end
  pol = base_ring(a).pol
  K = base_ring(a)
  R = parent(pol)
  T = elem_type(R)
  S = Generic.PolyRing{T}(R, :y)
  f = S()
  fit!(f, lena)
  for i = 1:lena
    f = setcoeff!(f, i - 1, R(coeff(a, i - 1)))
  end
  f = set_length!(f, lena)
  if a !== b
    g = S()
    fit!(g, lenb)
    for i = 1:lenb
      g = setcoeff!(g, i - 1, R(coeff(b, i - 1)))
    end
    g = set_length!(g, lenb)
  else
    g = f
  end
  p = f*g
  fit!(r, lenr)
  for i = 1:lenr
    r.coeffs[i] = K(p.coeffs[i])
  end
  r = set_length!(r, normalise(r, lenr))
  return r
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{AbsSimpleNumFieldElem}, ::Type{T}) where {T <: Integer} = AbsSimpleNumFieldElem

promote_rule(::Type{AbsSimpleNumFieldElem}, ::Type{ZZRingElem}) = AbsSimpleNumFieldElem

promote_rule(::Type{AbsSimpleNumFieldElem}, ::Type{QQFieldElem}) = AbsSimpleNumFieldElem

promote_rule(::Type{AbsSimpleNumFieldElem}, ::Type{QQPolyRingElem}) = AbsSimpleNumFieldElem

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

@doc raw"""
    (a::AbsSimpleNumField)()

Return an empty (0) element.
"""
function (a::AbsSimpleNumField)()
  z = AbsSimpleNumFieldElem(a)
  @ccall libflint.nf_elem_set_si(z::Ref{AbsSimpleNumFieldElem}, 0::Int, a::Ref{AbsSimpleNumField})::Nothing
  return z
end

@doc raw"""
    (a::AbsSimpleNumField)(c::Int)

Return $c$ as an element in $a$.
"""
function (a::AbsSimpleNumField)(c::Int)
  z = AbsSimpleNumFieldElem(a)
  @ccall libflint.nf_elem_set_si(z::Ref{AbsSimpleNumFieldElem}, c::Int, a::Ref{AbsSimpleNumField})::Nothing
  return z
end

(a::AbsSimpleNumField)(c::Integer) = a(flintify(c))

function (a::AbsSimpleNumField)(c::ZZRingElem)
  z = AbsSimpleNumFieldElem(a)
  @ccall libflint.nf_elem_set_fmpz(z::Ref{AbsSimpleNumFieldElem}, c::Ref{ZZRingElem}, a::Ref{AbsSimpleNumField})::Nothing
  return z
end

function (a::AbsSimpleNumField)(c::QQFieldElem)
  z = AbsSimpleNumFieldElem(a)
  @ccall libflint.nf_elem_set_fmpq(z::Ref{AbsSimpleNumFieldElem}, c::Ref{QQFieldElem}, a::Ref{AbsSimpleNumField})::Nothing
  return z
end

(a::AbsSimpleNumField)(c::Rational) = a(QQFieldElem(c))

function (a::AbsSimpleNumField)(b::AbsSimpleNumFieldElem)
  parent(b) == a && return b
  force_coerce(a, b)
end

function (a::AbsSimpleNumField)(pol::QQPolyRingElem)
  pol = parent(a.pol)(pol) # check pol has correct parent
  z = AbsSimpleNumFieldElem(a)
  if length(pol) >= length(a.pol)
    pol = mod(pol, a.pol)
  end
  @ccall libflint.nf_elem_set_fmpq_poly(z::Ref{AbsSimpleNumFieldElem}, pol::Ref{QQPolyRingElem}, a::Ref{AbsSimpleNumField})::Nothing
  return z
end

function (a::QQPolyRing)(b::AbsSimpleNumFieldElem)
  parent(parent(b).pol) != a && error("Cannot coerce from number field to polynomial ring")
  r = a()
  @ccall libflint.nf_elem_get_fmpq_poly(r::Ref{QQPolyRingElem}, b::Ref{AbsSimpleNumFieldElem}, parent(b)::Ref{AbsSimpleNumField})::Nothing
  return r
end

(::QQField)(a::AbsSimpleNumFieldElem) = (is_rational(a) && return coeff(a, 0)) || error("not a rational")

(::ZZRing)(a::AbsSimpleNumFieldElem) = (is_integer(a) && return numerator(coeff(a, 0))) || error("not an integer")

###############################################################################
#
#   Random generation
#
###############################################################################

RandomExtensions.maketype(K::AbsSimpleNumField, _) = elem_type(K)

function rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{AbsSimpleNumFieldElem, AbsSimpleNumField,
                                                           <:AbstractUnitRange{Int}}})
  K, r = sp[][1:end]
  R = parent(K.pol)
  n = degree(K.pol)
  return K(rand(rng, R, (n-1):(n-1), r))
end

rand(rng::AbstractRNG, K::AbsSimpleNumField, r::AbstractUnitRange{Int}) = rand(rng, make(K, r))

rand(K::AbsSimpleNumField, r) = rand(Random.default_rng(), K, r)

###############################################################################
#
#   Conformance test element generation
#
###############################################################################

function ConformanceTests.generate_element(K::AbsSimpleNumField)
  return rand(K, -10:10)
end

###############################################################################
#
#   AbsSimpleNumField constructor
#
###############################################################################

@doc raw"""
    number_field(f::QQPolyRingElem, s::VarName;
                cached::Bool = true, check::Bool = true)

Return a tuple $R, x$ consisting of the parent object $R$ and generator $x$
of the number field $\mathbb{Q}[x]/(f)$ where $f$ is the supplied polynomial.
The supplied string `s` specifies how the generator of the number field
should be printed. If `s` is not specified, it defaults to `_a`.

# Examples

```jldoctest
julia> R, x = polynomial_ring(QQ, "x");

julia> K, a = number_field(x^3 + 3x + 1, "a")
(Number field of degree 3 over QQ, a)

julia> K
Number field with defining polynomial x^3 + 3*x + 1
  over rational field
```
"""
function number_field(f::QQPolyRingElem, s::VarName = "_a"; cached::Bool = true, check::Bool = true)
  parent_obj = AbsSimpleNumField(f, Symbol(s), cached, check)

  return parent_obj, gen(parent_obj)
end

@doc raw"""
    cyclotomic_field(n::Int, s::VarName = "z_$n", t = "_\$"; cached::Bool = true)

Return a tuple $R, x$ consisting of the parent object $R$ and generator $x$
of the $n$-th cyclotomic field, $\mathbb{Q}(\zeta_n)$. The supplied string
`s` specifies how the generator of the number field should be printed. If
provided, the string `t` specifies how the generator of the polynomial ring
from which the number field is constructed, should be printed. If it is not
supplied, a default dollar sign will be used to represent the variable.
"""
function cyclotomic_field(n::Int, s::VarName = "z_$n", t = "_\$"; cached::Bool = true)
  n > 0 || throw(ArgumentError("conductor must be positive, not $n"))
  Zx, x = polynomial_ring(ZZ, gensym(); cached = false)
  Qx, = polynomial_ring(QQ, t; cached = cached)
  f = cyclotomic(n, x)
  C, g = number_field(Qx(f), Symbol(s); cached = cached, check = false)
  set_attribute!(C, :show => show_cyclo, :cyclo => n)
  return C, g
end

function show_cyclo(io::IO, a::AbsSimpleNumField)
  @assert is_cyclo_type(a)
  print(io, "Cyclotomic field of order $(get_attribute(a, :cyclo))")
end

function show_cyclo(io::IO, ::MIME"text/plain", a::AbsSimpleNumField)
  # TODO: change to print something with "cyclotomic" in it
  @assert is_cyclo_type(a)
  print(io, "Number field with defining polynomial ", defining_polynomial(a))
  println(io)
  io = pretty(io)
  print(io, Indent(), "over ", Lowercase(), QQ, Dedent())
end


@doc raw"""
    cyclotomic_real_subfield(n::Int, s::VarName = "(z_$n + 1/z_$n)", t = "\$"; cached = true)

Return a tuple $R, x$ consisting of the parent object $R$ and generator $x$
of the totally real subfield of the $n$-th cyclotomic field,
$\mathbb{Q}(\zeta_n)$. The supplied string `s` specifies how the generator of
the number field should be printed. If provided, the string `t` specifies how
the generator of the polynomial ring from which the number field is
constructed, should be printed. If it is not supplied, a default dollar sign
will be used to represent the variable.
"""
function cyclotomic_real_subfield(n::Int, s::VarName = "(z_$n + 1/z_$n)", t = "\$"; cached = true)
  Zx, x = polynomial_ring(ZZ, gensym(); cached = false)
  Qx, = polynomial_ring(QQ, t; cached = cached)
  f = cos_minpoly(n, x)
  R, a =  number_field(Qx(f), Symbol(s); cached = cached, check = false)
  set_attribute!(R, :show => show_maxreal, :maxreal => n)
  return R, a
end

function show_maxreal(io::IO, ::MIME"text/plain", a::AbsSimpleNumField)
  # TODO: adjust once show_cyclo is adjusted
  print(io, "Number field with defining polynomial ", defining_polynomial(a))
  println(io)
  io = pretty(io)
  print(io, Indent(), "over ", Lowercase(), QQ, Dedent())
end

function show_maxreal(io::IO, a::AbsSimpleNumField)
  print(io, "Maximal real subfield of cyclotomic field of order $(get_attribute(a, :maxreal))")
end

################################################################################
#
#  Residue field is number field
#
################################################################################

function residue_field(R::QQPolyRing, f::QQPolyRingElem; cached::Bool = true)
  K, a = number_field(f, :$, cached = cached)
  f = Generic.EuclideanRingResidueMap(R, K)
  return K, f
end

function preimage(f::Generic.EuclideanRingResidueMap{QQPolyRing, AbsSimpleNumField}, x)
  parent(x) !== codomain(f) && error("Not an element of the codomain")
  return domain(f)(x)
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(Qx::QQPolyRing, a::AbsSimpleNumFieldElem)
  f = charpoly(Qx, representation_matrix(a))
  return f
end

function charpoly(a::AbsSimpleNumFieldElem)
  f = charpoly(parent(parent(a).pol), a)
  return f
end

function charpoly(a::AbsSimpleNumFieldElem, ::QQField)
  return charpoly(a)
end

function charpoly(Zx::ZZPolyRing, a::AbsSimpleNumFieldElem)
  f = charpoly(a)
  if !isone(denominator(f))
    error("Element is not integral")
  end
  return Zx(f)
end

function charpoly(a::AbsSimpleNumFieldElem, Z::ZZRing)
  return charpoly(polynomial_ring(Z, cached=false)[1], a)
end

################################################################################
#
#  Minimal polynomial
#
################################################################################

@doc raw"""
    minpoly(a::AbsSimpleNumFieldElem) -> QQPolyRingElem

The minimal polynomial of $a$.
"""
function minpoly(Qx::QQPolyRing, a::AbsSimpleNumFieldElem)
  f = minpoly(Qx, representation_matrix(a))
  return f
end

function minpoly(a::AbsSimpleNumFieldElem)
  f = minpoly(parent(parent(a).pol), a)
  return f
end

function minpoly(a::AbsSimpleNumFieldElem, ::QQField)
  return minpoly(a)
end

function minpoly(a::AbsSimpleNumFieldElem, ZZ::ZZRing)
  return minpoly(polynomial_ring(ZZ, cached=false)[1], a)
end

function minpoly(Zx::ZZPolyRing, a::AbsSimpleNumFieldElem)
  f = minpoly(a)
  if !isone(denominator(f))
    error("Element is not integral")
  end
  return Zx(f)
end

################################################################################
#
#  Miscellaneous
#
################################################################################

function degree(a::AbsSimpleNumFieldElem)
  return degree(minpoly(a))
end


function Base.:(^)(a::AbsSimpleNumFieldElem, e::UInt)
  b = parent(a)()
  @ccall libflint.nf_elem_pow(b::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, e::UInt, parent(a)::Ref{AbsSimpleNumField})::Nothing
  return b
end

function basis(K::AbsSimpleNumField)
  n = degree(K)
  g = gen(K)
  d = Vector{typeof(g)}(undef, n)
  b = K(1)
  for i = 1:n-1
    d[i] = b
    b *= g
  end
  d[n] = b
  return d
end

base_field(::AbsSimpleNumField) = QQ

###############################################################################
#
#   mod
#
###############################################################################

function mod_sym!(a::AbsSimpleNumFieldElem, b::ZZRingElem)
  @ccall libflint.nf_elem_smod_fmpz(a::Ref{AbsSimpleNumFieldElem}, a::Ref{AbsSimpleNumFieldElem}, b::Ref{ZZRingElem}, parent(a)::Ref{AbsSimpleNumField})::Nothing
  return a
end

mod_sym(a::AbsSimpleNumFieldElem, b::ZZRingElem) = mod_sym!(deepcopy(a), b)

function mod_sym!(A::MatElem{AbsSimpleNumFieldElem}, m::ZZRingElem)
  for i = 1:nrows(A)
    for j = 1:ncols(A)
      mod_sym!(A[i, j], m)
    end
  end
  return A
end
