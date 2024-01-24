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

parent_type(::Type{AnticNumberFieldElem}) = AnticNumberField

@doc raw"""
    parent(a::AnticNumberFieldElem)

Return the parent of the given number field element.
"""
parent(a::AnticNumberFieldElem) = a.parent

elem_type(::Type{AnticNumberField}) = AnticNumberFieldElem

@doc raw"""
    base_ring(a::AnticNumberField)

Returns `Union{}` since a number field doesn't depend on any ring.
"""
base_ring(a::AnticNumberField) = Union{}

@doc raw"""
    base_ring(a::AnticNumberFieldElem)

Returns `Union{}` since a number field doesn't depend on any ring.
"""
base_ring(a::AnticNumberFieldElem) = Union{}

is_domain_type(::Type{AnticNumberFieldElem}) = true

@doc raw"""
    var(a::AnticNumberField)

Returns the identifier (as a symbol, not a string), that is used for printing
the generator of the given number field.
"""
var(a::AnticNumberField) = a.S

function check_parent(a::AnticNumberFieldElem, b::AnticNumberFieldElem)
   a.parent != b.parent && error("Incompatible number field elements")
end

characteristic(::AnticNumberField) = 0

defining_polynomial(K::AnticNumberField) = K.pol

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function hash(a::AnticNumberFieldElem, h::UInt)
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
    coeff(x::AnticNumberFieldElem, n::Int)

Return the $n$-th coefficient of the polynomial representation of the given
number field element. Coefficients are numbered from $0$, starting with the
constant coefficient.
"""
function coeff(x::AnticNumberFieldElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = QQFieldElem()
   ccall((:nf_elem_get_coeff_fmpq, libantic), Nothing,
     (Ref{QQFieldElem}, Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}), z, x, n, parent(x))
   return z
end

function num_coeff!(z::ZZRingElem, x::AnticNumberFieldElem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   ccall((:nf_elem_get_coeff_fmpz, libantic), Nothing,
     (Ref{ZZRingElem}, Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}), z, x, n, parent(x))
   return z
end

@doc raw"""
    gen(a::AnticNumberField)

Return the generator of the given number field, i.e., a symbolic root of the
defining polynomial.
"""
function gen(a::AnticNumberField)
   r = AnticNumberFieldElem(a)
   ccall((:nf_elem_gen, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), r, a)
   return r
end

function one(a::AnticNumberField)
   r = AnticNumberFieldElem(a)
   ccall((:nf_elem_one, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), r, a)
   return r
end

function zero(a::AnticNumberField)
   r = AnticNumberFieldElem(a)
   ccall((:nf_elem_zero, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), r, a)
   return r
end

@doc raw"""
    is_gen(a::AnticNumberFieldElem)

Return `true` if the given number field element is the generator of the
number field, otherwise return `false`.
"""
function is_gen(a::AnticNumberFieldElem)
   return ccall((:nf_elem_is_gen, libantic), Bool,
                (Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), a, a.parent)
end

function isone(a::AnticNumberFieldElem)
   return ccall((:nf_elem_is_one, libantic), Bool,
                (Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), a, a.parent)
end

function iszero(a::AnticNumberFieldElem)
   return ccall((:nf_elem_is_zero, libantic), Bool,
                (Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), a, a.parent)
end

@doc raw"""
    is_unit(a::AnticNumberFieldElem)

Return `true` if the given number field element is invertible, i.e. nonzero,
otherwise return `false`. Note, this does not take the maximal order into account.
"""
is_unit(a::AnticNumberFieldElem) = !iszero(a)

@doc raw"""
    isinteger(a::AnticNumberFieldElem)

Return `true` if the given number field element is an integer, i.e., in ZZ, otherwise
return `false`.
"""
function isinteger(a::AnticNumberFieldElem)
   b = ccall((:nf_elem_is_integer, libantic), Cint,
             (Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), a, a.parent)
   return Bool(b)
end

@doc raw"""
    is_rational(a::AnticNumberFieldElem)

Return `true` if the given number field element is a rational number, i.e., in QQ,
otherwise `false`.
"""
function is_rational(a::AnticNumberFieldElem)
   b = ccall((:nf_elem_is_rational, libantic), Cint,
             (Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), a, a.parent)
   return Bool(b)
end

@doc raw"""
    denominator(a::AnticNumberFieldElem)

Return the denominator of the polynomial representation of the given number
field element.
"""
function denominator(a::AnticNumberFieldElem)
   z = ZZRingElem()
   ccall((:nf_elem_get_den, libantic), Nothing,
         (Ref{ZZRingElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         z, a, a.parent)
   return z
end

function elem_from_mat_row(a::AnticNumberField, b::ZZMatrix, i::Int, d::ZZRingElem)
   Generic._checkbounds(nrows(b), i) || throw(BoundsError())
   ncols(b) == degree(a) || error("Wrong number of columns")
   z = a()
   ccall((:nf_elem_set_fmpz_mat_row, libantic), Nothing,
        (Ref{AnticNumberFieldElem}, Ref{ZZMatrix}, Int, Ref{ZZRingElem}, Ref{AnticNumberField}),
        z, b, i - 1, d, a)
   return z
end

function elem_to_mat_row!(a::ZZMatrix, i::Int, d::ZZRingElem, b::AnticNumberFieldElem)
   ccall((:nf_elem_get_fmpz_mat_row, libantic), Nothing,
         (Ref{ZZMatrix}, Int, Ref{ZZRingElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         a, i - 1, d, b, b.parent)
   nothing
 end

@doc raw"""
    degree(a::AnticNumberField)

Return the degree of the given number field, i.e. the degree of its
defining polynomial.
"""
degree(a::AnticNumberField) = a.pol_length-1

function deepcopy_internal(d::AnticNumberFieldElem, dict::IdDict)
   z = AnticNumberFieldElem(parent(d), d)
   return z
end

function is_cyclo_type(K::AnticNumberField)
  return has_attribute(K, :cyclo)
end

function is_maxreal_type(K::AnticNumberField)
  return get_attribute(K, :maxreal)::Bool
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function Base.show(io::IO, ::MIME"text/plain", a::AnticNumberField)
   print(io, "Number field with defining polynomial ", defining_polynomial(a))
   println(io)
   io = AbstractAlgebra.pretty(io)
   print(io, AbstractAlgebra.Indent(), "over ", AbstractAlgebra.Lowercase(), QQ)
   print(io, Dedent())
   #print(IOContext(io, :supercompact => true))
end

function Base.show(io::IO, a::AnticNumberField)
  @show_name(io, a)
  @show_special(io, a)
  if get(io, :supercompact, false)
    # no nested printing
    print(io, "Number field")
  else
    # nested printing allowed, preferably supercompact
    print(io, "Number field of degree $(degree(a))")
    print(IOContext(io, :supercompact => true), " over ", Nemo.QQ)
  end
end

function expressify(a::AnticNumberFieldElem; context = nothing)
   return expressify(parent(parent(a).pol)(a), var(parent(a)), context = context)
end

function Base.show(io::IO, a::AnticNumberFieldElem)
   print(io, AbstractAlgebra.obj_to_string(a, context = io))
end

canonical_unit(x::AnticNumberFieldElem) = x

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(a::AnticNumberFieldElem)
   r = a.parent()
   ccall((:nf_elem_neg, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         r, a, a.parent)
   return r
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::AnticNumberFieldElem, b::AnticNumberFieldElem)
   parent(a) == parent(b) || return force_op(+, a, b)::AnticNumberFieldElem
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_add, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::AnticNumberFieldElem, b::AnticNumberFieldElem)
   parent(a) == parent(b) || return force_op(-, a, b)::AnticNumberFieldElem
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_sub, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::AnticNumberFieldElem, b::AnticNumberFieldElem)
   parent(a) == parent(b) || return force_op(*, a, b)::AnticNumberFieldElem
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_mul, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function +(a::AnticNumberFieldElem, b::Int)
   r = a.parent()
   ccall((:nf_elem_add_si, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function +(a::AnticNumberFieldElem, b::ZZRingElem)
   r = a.parent()
   ccall((:nf_elem_add_fmpz, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function +(a::AnticNumberFieldElem, b::QQFieldElem)
   r = a.parent()
   ccall((:nf_elem_add_fmpq, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::AnticNumberFieldElem, b::Int)
   r = a.parent()
   ccall((:nf_elem_sub_si, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::AnticNumberFieldElem, b::ZZRingElem)
   r = a.parent()
   ccall((:nf_elem_sub_fmpz, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::AnticNumberFieldElem, b::QQFieldElem)
   r = a.parent()
   ccall((:nf_elem_sub_fmpq, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::Int, b::AnticNumberFieldElem)
   r = b.parent()
   ccall((:nf_elem_si_sub, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         r, a, b, b.parent)
   return r
end

function -(a::ZZRingElem, b::AnticNumberFieldElem)
   r = b.parent()
   ccall((:nf_elem_fmpz_sub, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{ZZRingElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         r, a, b, b.parent)
   return r
end

function -(a::QQFieldElem, b::AnticNumberFieldElem)
   r = b.parent()
   ccall((:nf_elem_fmpq_sub, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{QQFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         r, a, b, b.parent)
   return r
end

+(a::AnticNumberFieldElem, b::Integer) = a + ZZRingElem(b)

-(a::AnticNumberFieldElem, b::Integer) = a - ZZRingElem(b)

-(a::Integer, b::AnticNumberFieldElem) = ZZRingElem(a) - b

+(a::Integer, b::AnticNumberFieldElem) = b + a

+(a::QQFieldElem, b::AnticNumberFieldElem) = b + a

+(a::Rational, b::AnticNumberFieldElem) = QQFieldElem(a) + b

+(a::AnticNumberFieldElem, b::Rational) = b + a

-(a::Rational, b::AnticNumberFieldElem) = QQFieldElem(a) - b

-(a::AnticNumberFieldElem, b::Rational) = a - QQFieldElem(b)

function *(a::AnticNumberFieldElem, b::Int)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_si, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::AnticNumberFieldElem, b::ZZRingElem)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_fmpz, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::AnticNumberFieldElem, b::QQFieldElem)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_fmpq, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::Rational, b::AnticNumberFieldElem)
  return QQFieldElem(a) * b
end

*(a::AnticNumberFieldElem, b::Rational) = b * a

*(a::AnticNumberFieldElem, b::Integer) = a * ZZRingElem(b)

*(a::Integer, b::AnticNumberFieldElem) = b * a

*(a::ZZRingElem, b::AnticNumberFieldElem) = b * a

*(a::QQFieldElem, b::AnticNumberFieldElem) = b * a

//(a::AnticNumberFieldElem, b::Int) = divexact(a, b)

//(a::AnticNumberFieldElem, b::ZZRingElem) = divexact(a, b)

//(a::AnticNumberFieldElem, b::Integer) = a//ZZRingElem(b)

//(a::AnticNumberFieldElem, b::QQFieldElem) = divexact(a, b)

//(a::Integer, b::AnticNumberFieldElem) = divexact(a, b)

//(a::ZZRingElem, b::AnticNumberFieldElem) = divexact(a, b)

//(a::QQFieldElem, b::AnticNumberFieldElem) = divexact(a, b)

//(a::Rational, b::AnticNumberFieldElem) = divexact(QQFieldElem(a), b)

//(a::AnticNumberFieldElem, b::Rational) = divexact(a, QQFieldElem(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::AnticNumberFieldElem, n::Int)
   r = a.parent()
   ccall((:nf_elem_pow, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}),
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

function ==(a::AnticNumberFieldElem, b::AnticNumberFieldElem)
   parent(a) == parent(b) || return force_op(==, a, b)::Bool
   check_parent(a, b)
   return ccall((:nf_elem_equal, libantic), Bool,
           (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), a, b, a.parent)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::AnticNumberFieldElem, b::ZZRingElem)
   b = ccall((:nf_elem_equal_fmpz, libantic), Cint,
             (Ref{AnticNumberFieldElem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

function ==(a::AnticNumberFieldElem, b::QQFieldElem)
   b = ccall((:nf_elem_equal_fmpq, libantic), Cint,
             (Ref{AnticNumberFieldElem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

function ==(a::AnticNumberFieldElem, b::Int)
   b = ccall((:nf_elem_equal_si, libantic), Cint,
             (Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

function ==(a::AnticNumberFieldElem, b::UInt)
   b = ccall((:nf_elem_equal_ui, libantic), Cint,
             (Ref{AnticNumberFieldElem}, UInt, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

==(a::AnticNumberFieldElem, b::Integer) = a == ZZRingElem(b)

==(a::AnticNumberFieldElem, b::Rational) = a == QQFieldElem(b)

==(a::ZZRingElem, b::AnticNumberFieldElem) = b == a

==(a::QQFieldElem, b::AnticNumberFieldElem) = b == a

==(a::Int, b::AnticNumberFieldElem) = b == a

==(a::UInt, b::AnticNumberFieldElem) = b == a

==(a::Integer, b::AnticNumberFieldElem) = b == a

==(a::Rational, b::AnticNumberFieldElem) = b == a

###############################################################################
#
#   Inversion
#
###############################################################################

@doc raw"""
    inv(a::AnticNumberFieldElem)

Return $a^{-1}$. Requires $a \neq 0$.
"""
function inv(a::AnticNumberFieldElem)
   iszero(a) && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_inv, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         r, a, a.parent)
   return r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::AnticNumberFieldElem, b::AnticNumberFieldElem; check::Bool=true)
   iszero(b) && throw(DivideError())
   parent(a) == parent(b) || return force_op(divexact, a, b)::AnticNumberFieldElem
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_div, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(a::AnticNumberFieldElem, b::Int; check::Bool=true)
   b == 0 && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_si, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function divexact(a::AnticNumberFieldElem, b::ZZRingElem; check::Bool=true)
   iszero(b) && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_fmpz, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

divexact(a::AnticNumberFieldElem, b::Integer; check::Bool=true) = divexact(a, ZZRingElem(b); check=check)

function divexact(a::AnticNumberFieldElem, b::QQFieldElem; check::Bool=true)
   iszero(b) && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_fmpq, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

divexact(a::Integer, b::AnticNumberFieldElem; check::Bool=true) = inv(b)*a

divexact(a::ZZRingElem, b::AnticNumberFieldElem; check::Bool=true) = inv(b)*a

divexact(a::QQFieldElem, b::AnticNumberFieldElem; check::Bool=true) = inv(b)*a

###############################################################################
#
#   Removal and valuation
#
###############################################################################

@doc raw"""
    divides(a::AnticNumberFieldElem, b::AnticNumberFieldElem)

Returns a pair consisting of a flag which is set to `true` if $b$ divides
$a$ and `false` otherwise, and a number field element $h$ such that $a = bh$
if such exists. If not, the value of $h$ is undetermined.
"""
function divides(a::AnticNumberFieldElem, b::AnticNumberFieldElem)
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
    norm(a::AnticNumberFieldElem)

Return the absolute norm of $a$. The result will be a rational number.
"""
function norm(a::AnticNumberFieldElem)
   z = QQFieldElem()
   ccall((:nf_elem_norm, libantic), Nothing,
         (Ref{QQFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         z, a, a.parent)
   return z
end

@doc raw"""
    tr(a::AnticNumberFieldElem)

Return the absolute trace of $a$. The result will be a rational number.
"""
function tr(a::AnticNumberFieldElem)
   z = QQFieldElem()
   ccall((:nf_elem_trace, libantic), Nothing,
         (Ref{QQFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         z, a, a.parent)
   return z
end

@doc raw"""
    representation_matrix(a::AnticNumberFieldElem)

Return a matrix with rational entries representing multiplication with $a$
with respect to the power basis of the generator of the parent of $a$.
The matrix is of type QQMatrix.
"""
function representation_matrix(a::AnticNumberFieldElem)
  K = parent(a)
  z = QQMatrix(degree(K), degree(K))
  ccall((:nf_elem_rep_mat, libantic), Nothing,
        (Ref{QQMatrix}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), z, a, K)
  return z
end

@doc raw"""
    representation_matrix_q(a::AnticNumberFieldElem)

Return a matrix  representing multiplication with $a$ with respect to the
power basis of the generator of the parent of $a$.
The matrix is returned as a tuple (ZZMatrix, ZZRingElem), consisting of the
a primitive integer matrix and a denominator.
"""
function representation_matrix_q(a::AnticNumberFieldElem)
  K = parent(a)
  z = ZZMatrix(degree(K), degree(K))
  d = ZZRingElem()
  ccall((:nf_elem_rep_mat_fmpz_mat_den, libantic), Nothing,
        (Ref{ZZMatrix}, Ref{ZZRingElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
        z, d, a, K)
  return z, d
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::AnticNumberFieldElem)
   ccall((:nf_elem_zero, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), a, parent(a))
   return a
end

function mul!(z::AnticNumberFieldElem, x::AnticNumberFieldElem, y::AnticNumberFieldElem)
   ccall((:nf_elem_mul, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
                                                  z, x, y, parent(x))
   return z
end

@doc raw"""
    mul_red!(z::AnticNumberFieldElem, x::AnticNumberFieldElem, y::AnticNumberFieldElem, red::Bool)

Multiply $x$ by $y$ and set the existing number field element $z$ to the
result. Reduction modulo the defining polynomial is only performed if `red` is
set to `true`. Note that $x$ and $y$ must be reduced. This function is provided
for performance reasons as it saves allocating a new object for the result and
eliminates associated garbage collection.
"""
function mul_red!(z::AnticNumberFieldElem, x::AnticNumberFieldElem, y::AnticNumberFieldElem, red::Bool)
   ccall((:nf_elem_mul_red, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}, Cint),
                                                z, x, y, parent(x), red)
   return z
end

function addeq!(z::AnticNumberFieldElem, x::AnticNumberFieldElem)
   ccall((:nf_elem_add, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
                                                  z, z, x, parent(x))
   return z
end

function add!(a::AnticNumberFieldElem, b::AnticNumberFieldElem, c::AnticNumberFieldElem)
   ccall((:nf_elem_add, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         a, b, c, a.parent)
  return a
end

@doc raw"""
    reduce!(x::AnticNumberFieldElem)

Reduce the given number field element by the defining polynomial, in-place.
This only needs to be done after accumulating values computed by `mul_red!`
where reduction has not been performed. All standard Nemo number field
functions automatically reduce their outputs.
"""
function reduce!(x::AnticNumberFieldElem)
   ccall((:nf_elem_reduce, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), x, parent(x))
   return x
end

###############################################################################
#
#   Ad hoc unsafe functions
#
###############################################################################

function add!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::QQFieldElem)
   ccall((:nf_elem_add_fmpq, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function add!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::ZZRingElem)
   ccall((:nf_elem_add_fmpz, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function add!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::Int)
   ccall((:nf_elem_add_si, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

add!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::Integer) = add!(c, a, ZZRingElem(b))

function sub!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::QQFieldElem)
   ccall((:nf_elem_sub_fmpq, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::ZZRingElem)
   ccall((:nf_elem_sub_fmpz, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::Int)
   ccall((:nf_elem_sub_si, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

sub!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::Integer) = sub!(c, a, ZZRingElem(b))

function sub!(c::AnticNumberFieldElem, a::QQFieldElem, b::AnticNumberFieldElem)
   ccall((:nf_elem_fmpq_sub, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{QQFieldElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::AnticNumberFieldElem, a::ZZRingElem, b::AnticNumberFieldElem)
   ccall((:nf_elem_fmpz_sub, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{ZZRingElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::AnticNumberFieldElem, a::Int, b::AnticNumberFieldElem)
   ccall((:nf_elem_si_sub, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}),
         c, a, b, b.parent)
   return c
end

sub!(c::AnticNumberFieldElem, a::Integer, b::AnticNumberFieldElem) = sub!(c, ZZRingElem(a), b)

function mul!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::QQFieldElem)
   ccall((:nf_elem_scalar_mul_fmpq, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function mul!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::ZZRingElem)
   ccall((:nf_elem_scalar_mul_fmpz, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function mul!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::Int)
   ccall((:nf_elem_scalar_mul_si, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

mul!(c::AnticNumberFieldElem, a::AnticNumberFieldElem, b::Integer) = mul!(c, a, ZZRingElem(b))

###############################################################################
#
#   Speedups for polynomials over number fields
#
###############################################################################

function sqr_classical(a::Generic.Poly{AnticNumberFieldElem})
   lena = length(a)

   t = base_ring(a)()

   lenz = 2*lena - 1
   d = Vector{AnticNumberFieldElem}(undef, lenz)

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
         d[i + j - 1] = addeq!(d[i + j - 1], t)
         d[i + j - 1] = addeq!(d[i + j - 1], t)
      end
   end

   for i = 1:lenz
      d[i] = reduce!(d[i])
   end

   z = parent(a)(d)

   z = set_length!(z, normalise(z, lenz))

   return z
end

function mul_classical(a::Generic.Poly{AnticNumberFieldElem}, b::Generic.Poly{AnticNumberFieldElem})
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
   d = Vector{AnticNumberFieldElem}(undef, lenz)

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
         d[i + j - 1] = addeq!(d[i + j - 1], t)
      end
   end

   for i = 1:lenz
      d[i] = reduce!(d[i])
   end

   z = parent(a)(d)

   z = set_length!(z, normalise(z, lenz))

   return z
end

function use_karamul(a::Generic.Poly{AnticNumberFieldElem}, b::Generic.Poly{AnticNumberFieldElem})
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

function *(a::Generic.Poly{AnticNumberFieldElem}, b::Generic.Poly{AnticNumberFieldElem})
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

promote_rule(::Type{AnticNumberFieldElem}, ::Type{T}) where {T <: Integer} = AnticNumberFieldElem

promote_rule(::Type{AnticNumberFieldElem}, ::Type{ZZRingElem}) = AnticNumberFieldElem

promote_rule(::Type{AnticNumberFieldElem}, ::Type{QQFieldElem}) = AnticNumberFieldElem

promote_rule(::Type{AnticNumberFieldElem}, ::Type{QQPolyRingElem}) = AnticNumberFieldElem

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

@doc raw"""
    (a::AnticNumberField)()

Return an empty (0) element.
"""
function (a::AnticNumberField)()
   z = AnticNumberFieldElem(a)
   ccall((:nf_elem_set_si, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}), z, 0, a)
   return z
end

@doc raw"""
    (a::AnticNumberField)(c::Int)

Return $c$ as an element in $a$.
"""
function (a::AnticNumberField)(c::Int)
   z = AnticNumberFieldElem(a)
   ccall((:nf_elem_set_si, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Int, Ref{AnticNumberField}), z, c, a)
   return z
end

(a::AnticNumberField)(c::Integer) = a(ZZRingElem(c))

function (a::AnticNumberField)(c::ZZRingElem)
   z = AnticNumberFieldElem(a)
   ccall((:nf_elem_set_fmpz, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{ZZRingElem}, Ref{AnticNumberField}), z, c, a)
   return z
end

function (a::AnticNumberField)(c::QQFieldElem)
   z = AnticNumberFieldElem(a)
   ccall((:nf_elem_set_fmpq, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{QQFieldElem}, Ref{AnticNumberField}), z, c, a)
   return z
end

(a::AnticNumberField)(c::Rational) = a(QQFieldElem(c))

function (a::AnticNumberField)(b::AnticNumberFieldElem)
   parent(b) == a && return b
   force_coerce(a, b)
end

function (a::AnticNumberField)(pol::QQPolyRingElem)
   pol = parent(a.pol)(pol) # check pol has correct parent
   z = AnticNumberFieldElem(a)
   if length(pol) >= length(a.pol)
      pol = mod(pol, a.pol)
   end
   ccall((:nf_elem_set_fmpq_poly, libantic), Nothing,
         (Ref{AnticNumberFieldElem}, Ref{QQPolyRingElem}, Ref{AnticNumberField}), z, pol, a)
   return z
end

function (a::QQPolyRing)(b::AnticNumberFieldElem)
   parent(parent(b).pol) != a && error("Cannot coerce from number field to polynomial ring")
   r = a()
   ccall((:nf_elem_get_fmpq_poly, libantic), Nothing,
         (Ref{QQPolyRingElem}, Ref{AnticNumberFieldElem}, Ref{AnticNumberField}), r, b, parent(b))
   return r
end

###############################################################################
#
#   Random generation
#
###############################################################################

RandomExtensions.maketype(K::AnticNumberField, _) = elem_type(K)

function rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{AnticNumberFieldElem, AnticNumberField,
                                                           <:AbstractUnitRange{Int}}})
   K, r = sp[][1:end]
   R = parent(K.pol)
   n = degree(K.pol)
   return K(rand(rng, R, (n-1):(n-1), r))
end

rand(rng::AbstractRNG, K::AnticNumberField, r::AbstractUnitRange{Int}) = rand(rng, make(K, r))

rand(K::AnticNumberField, r) = rand(Random.GLOBAL_RNG, K, r)

###############################################################################
#
#   AnticNumberField constructor
#
###############################################################################

@doc raw"""
    number_field(f::QQPolyRingElem, s::VarName;
                cached::Bool = true, check::Bool = true)

Return a tuple $R, x$ consisting of the parent object $R$ and generator $x$
of the number field $\mathbb{Q}[x]/(f)$ where $f$ is the supplied polynomial.
The supplied string `s` specifies how the generator of the number field
should be printed. If `s` is not specified, it defaults to `_a`.
"""
function number_field(f::QQPolyRingElem, s::VarName = "_a"; cached::Bool = true, check::Bool = true)
   parent_obj = AnticNumberField(f, Symbol(s), cached, check)

   return parent_obj, gen(parent_obj)
end

@doc raw"""
    cyclotomic_field(n::Int, s::VarName = "z_$n", t = "_\$"; cached = true)

Return a tuple $R, x$ consisting of the parent object $R$ and generator $x$
of the $n$-th cyclotomic field, $\mathbb{Q}(\zeta_n)$. The supplied string
`s` specifies how the generator of the number field should be printed. If
provided, the string `t` specifies how the generator of the polynomial ring
from which the number field is constructed, should be printed. If it is not
supplied, a default dollar sign will be used to represent the variable.
"""
function cyclotomic_field(n::Int, s::VarName = "z_$n", t = "_\$"; cached = true)
   n > 0 || throw(ArgumentError("conductor must be positive, not $n"))
   Zx, x = polynomial_ring(FlintZZ, gensym(); cached = false)
   Qx, = polynomial_ring(FlintQQ, t; cached = cached)
   f = cyclotomic(n, x)
   C, g = number_field(Qx(f), Symbol(s); cached = cached, check = false)
   set_attribute!(C, :show => show_cyclo, :cyclo => n)
   return C, g
end

function show_cyclo(io::IO, a::AnticNumberField)
  @assert is_cyclo_type(a)
  print(io, "Cyclotomic field of order $(get_attribute(a, :cyclo))")
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
   Zx, x = polynomial_ring(FlintZZ, gensym(); cached = false)
   Qx, = polynomial_ring(FlintQQ, t; cached = cached)
   f = cos_minpoly(n, x)
   R, a =  number_field(Qx(f), Symbol(s); cached = cached, check = false)
   set_attribute!(R, :show => show_maxreal, :maxreal => n)
   return R, a
end

function show_maxreal(io::IO, a::AnticNumberField)
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

function preimage(f::Generic.EuclideanRingResidueMap{QQPolyRing, AnticNumberField}, x)
  parent(x) !== codomain(f) && error("Not an element of the codomain")
  return domain(f)(x)
end
