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
  ccall((:nf_elem_get_coeff_fmpq, libflint), Nothing,
        (Ref{QQFieldElem}, Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}), z, x, n, parent(x))
  return z
end

function num_coeff!(z::ZZRingElem, x::AbsSimpleNumFieldElem, n::Int)
  n < 0 && throw(DomainError(n, "Index must be non-negative"))
  ccall((:nf_elem_get_coeff_fmpz, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}), z, x, n, parent(x))
  return z
end

@doc raw"""
    gen(a::AbsSimpleNumField)

Return the generator of the given number field, i.e., a symbolic root of the
defining polynomial.
"""
function gen(a::AbsSimpleNumField)
  r = AbsSimpleNumFieldElem(a)
  ccall((:nf_elem_gen, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), r, a)
  return r
end

function one(a::AbsSimpleNumField)
  r = AbsSimpleNumFieldElem(a)
  ccall((:nf_elem_one, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), r, a)
  return r
end

function zero(a::AbsSimpleNumField)
  r = AbsSimpleNumFieldElem(a)
  ccall((:nf_elem_zero, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), r, a)
  return r
end

@doc raw"""
    is_gen(a::AbsSimpleNumFieldElem)

Return `true` if the given number field element is the generator of the
number field, otherwise return `false`.
"""
function is_gen(a::AbsSimpleNumFieldElem)
  return ccall((:nf_elem_is_gen, libflint), Bool,
               (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), a, a.parent)
end

function isone(a::AbsSimpleNumFieldElem)
  return ccall((:nf_elem_is_one, libflint), Bool,
               (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), a, a.parent)
end

function iszero(a::AbsSimpleNumFieldElem)
  return ccall((:nf_elem_is_zero, libflint), Bool,
               (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), a, a.parent)
end

@doc raw"""
    is_unit(a::AbsSimpleNumFieldElem)

Return `true` if the given number field element is invertible, i.e. nonzero,
otherwise return `false`. Note, this does not take the maximal order into account.
"""
is_unit(a::AbsSimpleNumFieldElem) = !iszero(a)

@doc raw"""
    isinteger(a::AbsSimpleNumFieldElem)

Return `true` if the given number field element is an integer, i.e., in ZZ, otherwise
return `false`.
"""
function isinteger(a::AbsSimpleNumFieldElem)
  b = ccall((:nf_elem_is_integer, libflint), Cint,
            (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), a, a.parent)
  return Bool(b)
end

@doc raw"""
    is_rational(a::AbsSimpleNumFieldElem)

Return `true` if the given number field element is a rational number, i.e., in QQ,
otherwise `false`.
"""
function is_rational(a::AbsSimpleNumFieldElem)
  b = ccall((:nf_elem_is_rational, libflint), Cint,
            (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), a, a.parent)
  return Bool(b)
end

@doc raw"""
    denominator(a::AbsSimpleNumFieldElem)

Return the denominator of the polynomial representation of the given number
field element.
"""
function denominator(a::AbsSimpleNumFieldElem)
  z = ZZRingElem()
  ccall((:nf_elem_get_den, libflint), Nothing,
        (Ref{ZZRingElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        z, a, a.parent)
  return z
end

function elem_from_mat_row(a::AbsSimpleNumField, b::ZZMatrix, i::Int, d::ZZRingElem)
  _checkbounds(nrows(b), i) || throw(BoundsError())
  ncols(b) == degree(a) || error("Wrong number of columns")
  z = a()
  ccall((:nf_elem_set_fmpz_mat_row, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{ZZMatrix}, Int, Ref{ZZRingElem}, Ref{AbsSimpleNumField}),
        z, b, i - 1, d, a)
  return z
end

function elem_to_mat_row!(a::ZZMatrix, i::Int, d::ZZRingElem, b::AbsSimpleNumFieldElem)
  ccall((:nf_elem_get_fmpz_mat_row, libflint), Nothing,
        (Ref{ZZMatrix}, Int, Ref{ZZRingElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        a, i - 1, d, b, b.parent)
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

function -(a::AbsSimpleNumFieldElem)
  r = a.parent()
  ccall((:nf_elem_neg, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        r, a, a.parent)
  return r
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)
  parent(a) == parent(b) || return force_op(+, a, b)::AbsSimpleNumFieldElem
  check_parent(a, b)
  r = a.parent()
  ccall((:nf_elem_add, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function -(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)
  parent(a) == parent(b) || return force_op(-, a, b)::AbsSimpleNumFieldElem
  check_parent(a, b)
  r = a.parent()
  ccall((:nf_elem_sub, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function *(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)
  parent(a) == parent(b) || return force_op(*, a, b)::AbsSimpleNumFieldElem
  check_parent(a, b)
  r = a.parent()
  ccall((:nf_elem_mul, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function +(a::AbsSimpleNumFieldElem, b::Int)
  r = a.parent()
  ccall((:nf_elem_add_si, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function +(a::AbsSimpleNumFieldElem, b::ZZRingElem)
  r = a.parent()
  ccall((:nf_elem_add_fmpz, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function +(a::AbsSimpleNumFieldElem, b::QQFieldElem)
  r = a.parent()
  ccall((:nf_elem_add_fmpq, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{QQFieldElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function -(a::AbsSimpleNumFieldElem, b::Int)
  r = a.parent()
  ccall((:nf_elem_sub_si, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function -(a::AbsSimpleNumFieldElem, b::ZZRingElem)
  r = a.parent()
  ccall((:nf_elem_sub_fmpz, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function -(a::AbsSimpleNumFieldElem, b::QQFieldElem)
  r = a.parent()
  ccall((:nf_elem_sub_fmpq, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{QQFieldElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function -(a::Int, b::AbsSimpleNumFieldElem)
  r = b.parent()
  ccall((:nf_elem_si_sub, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        r, a, b, b.parent)
  return r
end

function -(a::ZZRingElem, b::AbsSimpleNumFieldElem)
  r = b.parent()
  ccall((:nf_elem_fmpz_sub, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        r, a, b, b.parent)
  return r
end

function -(a::QQFieldElem, b::AbsSimpleNumFieldElem)
  r = b.parent()
  ccall((:nf_elem_fmpq_sub, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{QQFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        r, a, b, b.parent)
  return r
end

+(a::AbsSimpleNumFieldElem, b::Integer) = a + ZZRingElem(b)

-(a::AbsSimpleNumFieldElem, b::Integer) = a - ZZRingElem(b)

-(a::Integer, b::AbsSimpleNumFieldElem) = ZZRingElem(a) - b

+(a::Integer, b::AbsSimpleNumFieldElem) = b + a

+(a::QQFieldElem, b::AbsSimpleNumFieldElem) = b + a

+(a::Rational, b::AbsSimpleNumFieldElem) = QQFieldElem(a) + b

+(a::AbsSimpleNumFieldElem, b::Rational) = b + a

-(a::Rational, b::AbsSimpleNumFieldElem) = QQFieldElem(a) - b

-(a::AbsSimpleNumFieldElem, b::Rational) = a - QQFieldElem(b)

function *(a::AbsSimpleNumFieldElem, b::Int)
  r = a.parent()
  ccall((:nf_elem_scalar_mul_si, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function *(a::AbsSimpleNumFieldElem, b::ZZRingElem)
  r = a.parent()
  ccall((:nf_elem_scalar_mul_fmpz, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function *(a::AbsSimpleNumFieldElem, b::QQFieldElem)
  r = a.parent()
  ccall((:nf_elem_scalar_mul_fmpq, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{QQFieldElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function *(a::Rational, b::AbsSimpleNumFieldElem)
  return QQFieldElem(a) * b
end

*(a::AbsSimpleNumFieldElem, b::Rational) = b * a

*(a::AbsSimpleNumFieldElem, b::Integer) = a * ZZRingElem(b)

*(a::Integer, b::AbsSimpleNumFieldElem) = b * a

*(a::ZZRingElem, b::AbsSimpleNumFieldElem) = b * a

*(a::QQFieldElem, b::AbsSimpleNumFieldElem) = b * a

//(a::AbsSimpleNumFieldElem, b::Int) = divexact(a, b)

//(a::AbsSimpleNumFieldElem, b::ZZRingElem) = divexact(a, b)

//(a::AbsSimpleNumFieldElem, b::Integer) = a//ZZRingElem(b)

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
  r = a.parent()
  ccall((:nf_elem_pow, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}),
        r, a, abs(n), a.parent)
  if n < 0
    r = inv(r)
  end
  return r
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem)
  parent(a) == parent(b) || return force_op(==, a, b)::Bool
  check_parent(a, b)
  return ccall((:nf_elem_equal, libflint), Bool,
               (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), a, b, a.parent)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::AbsSimpleNumFieldElem, b::ZZRingElem)
  b = ccall((:nf_elem_equal_fmpz, libflint), Cint,
            (Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumField}),
            a, b, a.parent)
  return Bool(b)
end

function ==(a::AbsSimpleNumFieldElem, b::QQFieldElem)
  b = ccall((:nf_elem_equal_fmpq, libflint), Cint,
            (Ref{AbsSimpleNumFieldElem}, Ref{QQFieldElem}, Ref{AbsSimpleNumField}),
            a, b, a.parent)
  return Bool(b)
end

function ==(a::AbsSimpleNumFieldElem, b::Int)
  b = ccall((:nf_elem_equal_si, libflint), Cint,
            (Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}),
            a, b, a.parent)
  return Bool(b)
end

function ==(a::AbsSimpleNumFieldElem, b::UInt)
  b = ccall((:nf_elem_equal_ui, libflint), Cint,
            (Ref{AbsSimpleNumFieldElem}, UInt, Ref{AbsSimpleNumField}),
            a, b, a.parent)
  return Bool(b)
end

==(a::AbsSimpleNumFieldElem, b::Integer) = a == ZZRingElem(b)

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
  ccall((:nf_elem_inv, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        r, a, a.parent)
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
  ccall((:nf_elem_div, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
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
  ccall((:nf_elem_scalar_div_si, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

function divexact(a::AbsSimpleNumFieldElem, b::ZZRingElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  r = a.parent()
  ccall((:nf_elem_scalar_div_fmpz, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
  return r
end

divexact(a::AbsSimpleNumFieldElem, b::Integer; check::Bool=true) = divexact(a, ZZRingElem(b); check=check)

function divexact(a::AbsSimpleNumFieldElem, b::QQFieldElem; check::Bool=true)
  iszero(b) && throw(DivideError())
  r = a.parent()
  ccall((:nf_elem_scalar_div_fmpq, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{QQFieldElem}, Ref{AbsSimpleNumField}),
        r, a, b, a.parent)
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
  ccall((:nf_elem_norm, libflint), Nothing,
        (Ref{QQFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        z, a, a.parent)
  return z
end

@doc raw"""
    tr(a::AbsSimpleNumFieldElem)

Return the absolute trace of $a$. The result will be a rational number.
"""
function tr(a::AbsSimpleNumFieldElem)
  z = QQFieldElem()
  ccall((:nf_elem_trace, libflint), Nothing,
        (Ref{QQFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        z, a, a.parent)
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
  ccall((:nf_elem_rep_mat, libflint), Nothing,
        (Ref{QQMatrix}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), z, a, K)
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
  ccall((:nf_elem_rep_mat_fmpz_mat_den, libflint), Nothing,
        (Ref{ZZMatrix}, Ref{ZZRingElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        z, d, a, K)
  return z, d
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::AbsSimpleNumFieldElem)
  ccall((:nf_elem_zero, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), a, parent(a))
  return a
end

function mul!(z::AbsSimpleNumFieldElem, x::AbsSimpleNumFieldElem, y::AbsSimpleNumFieldElem)
  ccall((:nf_elem_mul, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        z, x, y, parent(x))
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
  ccall((:nf_elem_mul_red, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}, Cint),
        z, x, y, parent(x), red)
  return z
end

function add!(a::AbsSimpleNumFieldElem, b::AbsSimpleNumFieldElem, c::AbsSimpleNumFieldElem)
  ccall((:nf_elem_add, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        a, b, c, a.parent)
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
  ccall((:nf_elem_reduce, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), x, parent(x))
  return x
end

###############################################################################
#
#   Ad hoc unsafe functions
#
###############################################################################

function add!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::QQFieldElem)
  ccall((:nf_elem_add_fmpq, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{QQFieldElem}, Ref{AbsSimpleNumField}),
        c, a, b, a.parent)
  return c
end

function add!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::ZZRingElem)
  ccall((:nf_elem_add_fmpz, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumField}),
        c, a, b, a.parent)
  return c
end

function add!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Int)
  ccall((:nf_elem_add_si, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}),
        c, a, b, a.parent)
  return c
end

add!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Integer) = add!(c, a, ZZRingElem(b))

function sub!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::QQFieldElem)
  ccall((:nf_elem_sub_fmpq, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{QQFieldElem}, Ref{AbsSimpleNumField}),
        c, a, b, a.parent)
  return c
end

function sub!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::ZZRingElem)
  ccall((:nf_elem_sub_fmpz, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumField}),
        c, a, b, a.parent)
  return c
end

function sub!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Int)
  ccall((:nf_elem_sub_si, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}),
        c, a, b, a.parent)
  return c
end

sub!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Integer) = sub!(c, a, ZZRingElem(b))

function sub!(c::AbsSimpleNumFieldElem, a::QQFieldElem, b::AbsSimpleNumFieldElem)
  ccall((:nf_elem_fmpq_sub, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{QQFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        c, a, b, parent(b))
  return c
end

function sub!(c::AbsSimpleNumFieldElem, a::ZZRingElem, b::AbsSimpleNumFieldElem)
  ccall((:nf_elem_fmpz_sub, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        c, a, b, parent(b))
  return c
end

function sub!(c::AbsSimpleNumFieldElem, a::Int, b::AbsSimpleNumFieldElem)
  ccall((:nf_elem_si_sub, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}),
        c, a, b, b.parent)
  return c
end

sub!(c::AbsSimpleNumFieldElem, a::Integer, b::AbsSimpleNumFieldElem) = sub!(c, ZZRingElem(a), b)

function mul!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::QQFieldElem)
  ccall((:nf_elem_scalar_mul_fmpq, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{QQFieldElem}, Ref{AbsSimpleNumField}),
        c, a, b, a.parent)
  return c
end

function mul!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::ZZRingElem)
  ccall((:nf_elem_scalar_mul_fmpz, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumField}),
        c, a, b, a.parent)
  return c
end

function mul!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Int)
  ccall((:nf_elem_scalar_mul_si, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}),
        c, a, b, a.parent)
  return c
end

mul!(c::AbsSimpleNumFieldElem, a::AbsSimpleNumFieldElem, b::Integer) = mul!(c, a, ZZRingElem(b))

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
  ccall((:nf_elem_set_si, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}), z, 0, a)
  return z
end

@doc raw"""
    (a::AbsSimpleNumField)(c::Int)

Return $c$ as an element in $a$.
"""
function (a::AbsSimpleNumField)(c::Int)
  z = AbsSimpleNumFieldElem(a)
  ccall((:nf_elem_set_si, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Int, Ref{AbsSimpleNumField}), z, c, a)
  return z
end

(a::AbsSimpleNumField)(c::Integer) = a(ZZRingElem(c))

function (a::AbsSimpleNumField)(c::ZZRingElem)
  z = AbsSimpleNumFieldElem(a)
  ccall((:nf_elem_set_fmpz, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumField}), z, c, a)
  return z
end

function (a::AbsSimpleNumField)(c::QQFieldElem)
  z = AbsSimpleNumFieldElem(a)
  ccall((:nf_elem_set_fmpq, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{QQFieldElem}, Ref{AbsSimpleNumField}), z, c, a)
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
  ccall((:nf_elem_set_fmpq_poly, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{QQPolyRingElem}, Ref{AbsSimpleNumField}), z, pol, a)
  return z
end

function (a::QQPolyRing)(b::AbsSimpleNumFieldElem)
  parent(parent(b).pol) != a && error("Cannot coerce from number field to polynomial ring")
  r = a()
  ccall((:nf_elem_get_fmpq_poly, libflint), Nothing,
        (Ref{QQPolyRingElem}, Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumField}), r, b, parent(b))
  return r
end

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

rand(K::AbsSimpleNumField, r) = rand(Random.GLOBAL_RNG, K, r)

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
  ccall((:nf_elem_pow, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, UInt, Ref{AbsSimpleNumField}),
        b, a, e, parent(a))
  return b
end

function basis(K::AbsSimpleNumField)
  n = degree(K)
  g = gen(K)
  d = Array{typeof(g)}(undef, n)
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
  ccall((:nf_elem_smod_fmpz, libflint), Nothing,
        (Ref{AbsSimpleNumFieldElem}, Ref{AbsSimpleNumFieldElem}, Ref{ZZRingElem}, Ref{AbsSimpleNumField}),
        a, a, b, parent(a))
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
