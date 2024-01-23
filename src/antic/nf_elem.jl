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

parent_type(::Type{nf_elem}) = AnticNumberField

@doc raw"""
    parent(a::nf_elem)

Return the parent of the given number field element.
"""
parent(a::nf_elem) = a.parent

elem_type(::Type{AnticNumberField}) = nf_elem

@doc raw"""
    base_ring(a::AnticNumberField)

Returns `Union{}` since a number field doesn't depend on any ring.
"""
base_ring(a::AnticNumberField) = Union{}

@doc raw"""
    base_ring(a::nf_elem)

Returns `Union{}` since a number field doesn't depend on any ring.
"""
base_ring(a::nf_elem) = Union{}

is_domain_type(::Type{nf_elem}) = true

@doc raw"""
    var(a::AnticNumberField)

Returns the identifier (as a symbol, not a string), that is used for printing
the generator of the given number field.
"""
var(a::AnticNumberField) = a.S

function check_parent(a::nf_elem, b::nf_elem)
   a.parent != b.parent && error("Incompatible number field elements")
end

characteristic(::AnticNumberField) = 0

defining_polynomial(K::AnticNumberField) = K.pol

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function hash(a::nf_elem, h::UInt)
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
    coeff(x::nf_elem, n::Int)

Return the $n$-th coefficient of the polynomial representation of the given
number field element. Coefficients are numbered from $0$, starting with the
constant coefficient.
"""
function coeff(x::nf_elem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   z = QQFieldElem()
   ccall((:nf_elem_get_coeff_fmpq, libantic), Nothing,
     (Ref{QQFieldElem}, Ref{nf_elem}, Int, Ref{AnticNumberField}), z, x, n, parent(x))
   return z
end

function num_coeff!(z::ZZRingElem, x::nf_elem, n::Int)
   n < 0 && throw(DomainError(n, "Index must be non-negative"))
   ccall((:nf_elem_get_coeff_fmpz, libantic), Nothing,
     (Ref{ZZRingElem}, Ref{nf_elem}, Int, Ref{AnticNumberField}), z, x, n, parent(x))
   return z
end

@doc raw"""
    gen(a::AnticNumberField)

Return the generator of the given number field, i.e., a symbolic root of the
defining polynomial.
"""
function gen(a::AnticNumberField)
   r = nf_elem(a)
   ccall((:nf_elem_gen, libantic), Nothing,
         (Ref{nf_elem}, Ref{AnticNumberField}), r, a)
   return r
end

function one(a::AnticNumberField)
   r = nf_elem(a)
   ccall((:nf_elem_one, libantic), Nothing,
         (Ref{nf_elem}, Ref{AnticNumberField}), r, a)
   return r
end

function zero(a::AnticNumberField)
   r = nf_elem(a)
   ccall((:nf_elem_zero, libantic), Nothing,
         (Ref{nf_elem}, Ref{AnticNumberField}), r, a)
   return r
end

@doc raw"""
    is_gen(a::nf_elem)

Return `true` if the given number field element is the generator of the
number field, otherwise return `false`.
"""
function is_gen(a::nf_elem)
   return ccall((:nf_elem_is_gen, libantic), Bool,
                (Ref{nf_elem}, Ref{AnticNumberField}), a, a.parent)
end

function isone(a::nf_elem)
   return ccall((:nf_elem_is_one, libantic), Bool,
                (Ref{nf_elem}, Ref{AnticNumberField}), a, a.parent)
end

function iszero(a::nf_elem)
   return ccall((:nf_elem_is_zero, libantic), Bool,
                (Ref{nf_elem}, Ref{AnticNumberField}), a, a.parent)
end

@doc raw"""
    is_unit(a::nf_elem)

Return `true` if the given number field element is invertible, i.e. nonzero,
otherwise return `false`. Note, this does not take the maximal order into account.
"""
is_unit(a::nf_elem) = !iszero(a)

@doc raw"""
    isinteger(a::nf_elem)

Return `true` if the given number field element is an integer, i.e., in ZZ, otherwise
return `false`.
"""
function isinteger(a::nf_elem)
   b = ccall((:nf_elem_is_integer, libantic), Cint,
             (Ref{nf_elem}, Ref{AnticNumberField}), a, a.parent)
   return Bool(b)
end

@doc raw"""
    is_rational(a::nf_elem)

Return `true` if the given number field element is a rational number, i.e., in QQ,
otherwise `false`.
"""
function is_rational(a::nf_elem)
   b = ccall((:nf_elem_is_rational, libantic), Cint,
             (Ref{nf_elem}, Ref{AnticNumberField}), a, a.parent)
   return Bool(b)
end

@doc raw"""
    denominator(a::nf_elem)

Return the denominator of the polynomial representation of the given number
field element.
"""
function denominator(a::nf_elem)
   z = ZZRingElem()
   ccall((:nf_elem_get_den, libantic), Nothing,
         (Ref{ZZRingElem}, Ref{nf_elem}, Ref{AnticNumberField}),
         z, a, a.parent)
   return z
end

function elem_from_mat_row(a::AnticNumberField, b::ZZMatrix, i::Int, d::ZZRingElem)
   Generic._checkbounds(nrows(b), i) || throw(BoundsError())
   ncols(b) == degree(a) || error("Wrong number of columns")
   z = a()
   ccall((:nf_elem_set_fmpz_mat_row, libantic), Nothing,
        (Ref{nf_elem}, Ref{ZZMatrix}, Int, Ref{ZZRingElem}, Ref{AnticNumberField}),
        z, b, i - 1, d, a)
   return z
end

function elem_to_mat_row!(a::ZZMatrix, i::Int, d::ZZRingElem, b::nf_elem)
   ccall((:nf_elem_get_fmpz_mat_row, libantic), Nothing,
         (Ref{ZZMatrix}, Int, Ref{ZZRingElem}, Ref{nf_elem}, Ref{AnticNumberField}),
         a, i - 1, d, b, b.parent)
   nothing
 end

@doc raw"""
    degree(a::AnticNumberField)

Return the degree of the given number field, i.e. the degree of its
defining polynomial.
"""
degree(a::AnticNumberField) = a.pol_length-1

function deepcopy_internal(d::nf_elem, dict::IdDict)
   z = nf_elem(parent(d), d)
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

function expressify(a::nf_elem; context = nothing)
   return expressify(parent(parent(a).pol)(a), var(parent(a)), context = context)
end

function Base.show(io::IO, a::nf_elem)
   print(io, AbstractAlgebra.obj_to_string(a, context = io))
end

canonical_unit(x::nf_elem) = x

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(a::nf_elem)
   r = a.parent()
   ccall((:nf_elem_neg, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, a.parent)
   return r
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::nf_elem, b::nf_elem)
   parent(a) == parent(b) || return force_op(+, a, b)::nf_elem
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_add, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::nf_elem, b::nf_elem)
   parent(a) == parent(b) || return force_op(-, a, b)::nf_elem
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_sub, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::nf_elem, b::nf_elem)
   parent(a) == parent(b) || return force_op(*, a, b)::nf_elem
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_mul, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function +(a::nf_elem, b::Int)
   r = a.parent()
   ccall((:nf_elem_add_si, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function +(a::nf_elem, b::ZZRingElem)
   r = a.parent()
   ccall((:nf_elem_add_fmpz, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function +(a::nf_elem, b::QQFieldElem)
   r = a.parent()
   ccall((:nf_elem_add_fmpq, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::nf_elem, b::Int)
   r = a.parent()
   ccall((:nf_elem_sub_si, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::nf_elem, b::ZZRingElem)
   r = a.parent()
   ccall((:nf_elem_sub_fmpz, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::nf_elem, b::QQFieldElem)
   r = a.parent()
   ccall((:nf_elem_sub_fmpq, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::Int, b::nf_elem)
   r = b.parent()
   ccall((:nf_elem_si_sub, libantic), Nothing,
         (Ref{nf_elem}, Int, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, b.parent)
   return r
end

function -(a::ZZRingElem, b::nf_elem)
   r = b.parent()
   ccall((:nf_elem_fmpz_sub, libantic), Nothing,
         (Ref{nf_elem}, Ref{ZZRingElem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, b.parent)
   return r
end

function -(a::QQFieldElem, b::nf_elem)
   r = b.parent()
   ccall((:nf_elem_fmpq_sub, libantic), Nothing,
         (Ref{nf_elem}, Ref{QQFieldElem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, b.parent)
   return r
end

+(a::nf_elem, b::Integer) = a + ZZRingElem(b)

-(a::nf_elem, b::Integer) = a - ZZRingElem(b)

-(a::Integer, b::nf_elem) = ZZRingElem(a) - b

+(a::Integer, b::nf_elem) = b + a

+(a::QQFieldElem, b::nf_elem) = b + a

+(a::Rational, b::nf_elem) = QQFieldElem(a) + b

+(a::nf_elem, b::Rational) = b + a

-(a::Rational, b::nf_elem) = QQFieldElem(a) - b

-(a::nf_elem, b::Rational) = a - QQFieldElem(b)

function *(a::nf_elem, b::Int)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_si, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::nf_elem, b::ZZRingElem)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_fmpz, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::nf_elem, b::QQFieldElem)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_fmpq, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::Rational, b::nf_elem)
  return QQFieldElem(a) * b
end

*(a::nf_elem, b::Rational) = b * a

*(a::nf_elem, b::Integer) = a * ZZRingElem(b)

*(a::Integer, b::nf_elem) = b * a

*(a::ZZRingElem, b::nf_elem) = b * a

*(a::QQFieldElem, b::nf_elem) = b * a

//(a::nf_elem, b::Int) = divexact(a, b)

//(a::nf_elem, b::ZZRingElem) = divexact(a, b)

//(a::nf_elem, b::Integer) = a//ZZRingElem(b)

//(a::nf_elem, b::QQFieldElem) = divexact(a, b)

//(a::Integer, b::nf_elem) = divexact(a, b)

//(a::ZZRingElem, b::nf_elem) = divexact(a, b)

//(a::QQFieldElem, b::nf_elem) = divexact(a, b)

//(a::Rational, b::nf_elem) = divexact(QQFieldElem(a), b)

//(a::nf_elem, b::Rational) = divexact(a, QQFieldElem(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::nf_elem, n::Int)
   r = a.parent()
   ccall((:nf_elem_pow, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
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

function ==(a::nf_elem, b::nf_elem)
   parent(a) == parent(b) || return force_op(==, a, b)::Bool
   check_parent(a, b)
   return ccall((:nf_elem_equal, libantic), Bool,
           (Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}), a, b, a.parent)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::nf_elem, b::ZZRingElem)
   b = ccall((:nf_elem_equal_fmpz, libantic), Cint,
             (Ref{nf_elem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

function ==(a::nf_elem, b::QQFieldElem)
   b = ccall((:nf_elem_equal_fmpq, libantic), Cint,
             (Ref{nf_elem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

function ==(a::nf_elem, b::Int)
   b = ccall((:nf_elem_equal_si, libantic), Cint,
             (Ref{nf_elem}, Int, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

function ==(a::nf_elem, b::UInt)
   b = ccall((:nf_elem_equal_ui, libantic), Cint,
             (Ref{nf_elem}, UInt, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

==(a::nf_elem, b::Integer) = a == ZZRingElem(b)

==(a::nf_elem, b::Rational) = a == QQFieldElem(b)

==(a::ZZRingElem, b::nf_elem) = b == a

==(a::QQFieldElem, b::nf_elem) = b == a

==(a::Int, b::nf_elem) = b == a

==(a::UInt, b::nf_elem) = b == a

==(a::Integer, b::nf_elem) = b == a

==(a::Rational, b::nf_elem) = b == a

###############################################################################
#
#   Inversion
#
###############################################################################

@doc raw"""
    inv(a::nf_elem)

Return $a^{-1}$. Requires $a \neq 0$.
"""
function inv(a::nf_elem)
   iszero(a) && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_inv, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, a.parent)
   return r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::nf_elem, b::nf_elem; check::Bool=true)
   iszero(b) && throw(DivideError())
   parent(a) == parent(b) || return force_op(divexact, a, b)::nf_elem
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_div, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(a::nf_elem, b::Int; check::Bool=true)
   b == 0 && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_si, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function divexact(a::nf_elem, b::ZZRingElem; check::Bool=true)
   iszero(b) && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_fmpz, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

divexact(a::nf_elem, b::Integer; check::Bool=true) = divexact(a, ZZRingElem(b); check=check)

function divexact(a::nf_elem, b::QQFieldElem; check::Bool=true)
   iszero(b) && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_fmpq, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

divexact(a::Integer, b::nf_elem; check::Bool=true) = inv(b)*a

divexact(a::ZZRingElem, b::nf_elem; check::Bool=true) = inv(b)*a

divexact(a::QQFieldElem, b::nf_elem; check::Bool=true) = inv(b)*a

###############################################################################
#
#   Removal and valuation
#
###############################################################################

@doc raw"""
    divides(a::nf_elem, b::nf_elem)

Returns a pair consisting of a flag which is set to `true` if $b$ divides
$a$ and `false` otherwise, and a number field element $h$ such that $a = bh$
if such exists. If not, the value of $h$ is undetermined.
"""
function divides(a::nf_elem, b::nf_elem)
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
    norm(a::nf_elem)

Return the absolute norm of $a$. The result will be a rational number.
"""
function norm(a::nf_elem)
   z = QQFieldElem()
   ccall((:nf_elem_norm, libantic), Nothing,
         (Ref{QQFieldElem}, Ref{nf_elem}, Ref{AnticNumberField}),
         z, a, a.parent)
   return z
end

@doc raw"""
    tr(a::nf_elem)

Return the absolute trace of $a$. The result will be a rational number.
"""
function tr(a::nf_elem)
   z = QQFieldElem()
   ccall((:nf_elem_trace, libantic), Nothing,
         (Ref{QQFieldElem}, Ref{nf_elem}, Ref{AnticNumberField}),
         z, a, a.parent)
   return z
end

@doc raw"""
    representation_matrix(a::nf_elem)

Return a matrix with rational entries representing multiplication with $a$
with respect to the power basis of the generator of the parent of $a$.
The matrix is of type QQMatrix.
"""
function representation_matrix(a::nf_elem)
  K = parent(a)
  z = QQMatrix(degree(K), degree(K))
  ccall((:nf_elem_rep_mat, libantic), Nothing,
        (Ref{QQMatrix}, Ref{nf_elem}, Ref{AnticNumberField}), z, a, K)
  return z
end

@doc raw"""
    representation_matrix_q(a::nf_elem)

Return a matrix  representing multiplication with $a$ with respect to the
power basis of the generator of the parent of $a$.
The matrix is returned as a tuple (ZZMatrix, ZZRingElem), consisting of the
a primitive integer matrix and a denominator.
"""
function representation_matrix_q(a::nf_elem)
  K = parent(a)
  z = ZZMatrix(degree(K), degree(K))
  d = ZZRingElem()
  ccall((:nf_elem_rep_mat_fmpz_mat_den, libantic), Nothing,
        (Ref{ZZMatrix}, Ref{ZZRingElem}, Ref{nf_elem}, Ref{AnticNumberField}),
        z, d, a, K)
  return z, d
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::nf_elem)
   ccall((:nf_elem_zero, libantic), Nothing,
         (Ref{nf_elem}, Ref{AnticNumberField}), a, parent(a))
   return a
end

function mul!(z::nf_elem, x::nf_elem, y::nf_elem)
   ccall((:nf_elem_mul, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
                                                  z, x, y, parent(x))
   return z
end

@doc raw"""
    mul_red!(z::nf_elem, x::nf_elem, y::nf_elem, red::Bool)

Multiply $x$ by $y$ and set the existing number field element $z$ to the
result. Reduction modulo the defining polynomial is only performed if `red` is
set to `true`. Note that $x$ and $y$ must be reduced. This function is provided
for performance reasons as it saves allocating a new object for the result and
eliminates associated garbage collection.
"""
function mul_red!(z::nf_elem, x::nf_elem, y::nf_elem, red::Bool)
   ccall((:nf_elem_mul_red, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}, Cint),
                                                z, x, y, parent(x), red)
   return z
end

function addeq!(z::nf_elem, x::nf_elem)
   ccall((:nf_elem_add, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
                                                  z, z, x, parent(x))
   return z
end

function add!(a::nf_elem, b::nf_elem, c::nf_elem)
   ccall((:nf_elem_add, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         a, b, c, a.parent)
  return a
end

@doc raw"""
    reduce!(x::nf_elem)

Reduce the given number field element by the defining polynomial, in-place.
This only needs to be done after accumulating values computed by `mul_red!`
where reduction has not been performed. All standard Nemo number field
functions automatically reduce their outputs.
"""
function reduce!(x::nf_elem)
   ccall((:nf_elem_reduce, libantic), Nothing,
         (Ref{nf_elem}, Ref{AnticNumberField}), x, parent(x))
   return x
end

###############################################################################
#
#   Ad hoc unsafe functions
#
###############################################################################

function add!(c::nf_elem, a::nf_elem, b::QQFieldElem)
   ccall((:nf_elem_add_fmpq, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function add!(c::nf_elem, a::nf_elem, b::ZZRingElem)
   ccall((:nf_elem_add_fmpz, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function add!(c::nf_elem, a::nf_elem, b::Int)
   ccall((:nf_elem_add_si, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

add!(c::nf_elem, a::nf_elem, b::Integer) = add!(c, a, ZZRingElem(b))

function sub!(c::nf_elem, a::nf_elem, b::QQFieldElem)
   ccall((:nf_elem_sub_fmpq, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::nf_elem, a::nf_elem, b::ZZRingElem)
   ccall((:nf_elem_sub_fmpz, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::nf_elem, a::nf_elem, b::Int)
   ccall((:nf_elem_sub_si, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

sub!(c::nf_elem, a::nf_elem, b::Integer) = sub!(c, a, ZZRingElem(b))

function sub!(c::nf_elem, a::QQFieldElem, b::nf_elem)
   ccall((:nf_elem_fmpq_sub, libantic), Nothing,
         (Ref{nf_elem}, Ref{QQFieldElem}, Ref{nf_elem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::nf_elem, a::ZZRingElem, b::nf_elem)
   ccall((:nf_elem_fmpz_sub, libantic), Nothing,
         (Ref{nf_elem}, Ref{ZZRingElem}, Ref{nf_elem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::nf_elem, a::Int, b::nf_elem)
   ccall((:nf_elem_si_sub, libantic), Nothing,
         (Ref{nf_elem}, Int, Ref{nf_elem}, Ref{AnticNumberField}),
         c, a, b, b.parent)
   return c
end

sub!(c::nf_elem, a::Integer, b::nf_elem) = sub!(c, ZZRingElem(a), b)

function mul!(c::nf_elem, a::nf_elem, b::QQFieldElem)
   ccall((:nf_elem_scalar_mul_fmpq, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{QQFieldElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function mul!(c::nf_elem, a::nf_elem, b::ZZRingElem)
   ccall((:nf_elem_scalar_mul_fmpz, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{ZZRingElem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function mul!(c::nf_elem, a::nf_elem, b::Int)
   ccall((:nf_elem_scalar_mul_si, libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

mul!(c::nf_elem, a::nf_elem, b::Integer) = mul!(c, a, ZZRingElem(b))

###############################################################################
#
#   Speedups for polynomials over number fields
#
###############################################################################

function sqr_classical(a::Generic.Poly{nf_elem})
   lena = length(a)

   t = base_ring(a)()

   lenz = 2*lena - 1
   d = Vector{nf_elem}(undef, lenz)

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

function mul_classical(a::Generic.Poly{nf_elem}, b::Generic.Poly{nf_elem})
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
   d = Vector{nf_elem}(undef, lenz)

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

function use_karamul(a::Generic.Poly{nf_elem}, b::Generic.Poly{nf_elem})
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

function *(a::Generic.Poly{nf_elem}, b::Generic.Poly{nf_elem})
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

promote_rule(::Type{nf_elem}, ::Type{T}) where {T <: Integer} = nf_elem

promote_rule(::Type{nf_elem}, ::Type{ZZRingElem}) = nf_elem

promote_rule(::Type{nf_elem}, ::Type{QQFieldElem}) = nf_elem

promote_rule(::Type{nf_elem}, ::Type{QQPolyRingElem}) = nf_elem

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
   z = nf_elem(a)
   ccall((:nf_elem_set_si, libantic), Nothing,
         (Ref{nf_elem}, Int, Ref{AnticNumberField}), z, 0, a)
   return z
end

@doc raw"""
    (a::AnticNumberField)(c::Int)

Return $c$ as an element in $a$.
"""
function (a::AnticNumberField)(c::Int)
   z = nf_elem(a)
   ccall((:nf_elem_set_si, libantic), Nothing,
         (Ref{nf_elem}, Int, Ref{AnticNumberField}), z, c, a)
   return z
end

(a::AnticNumberField)(c::Integer) = a(ZZRingElem(c))

function (a::AnticNumberField)(c::ZZRingElem)
   z = nf_elem(a)
   ccall((:nf_elem_set_fmpz, libantic), Nothing,
         (Ref{nf_elem}, Ref{ZZRingElem}, Ref{AnticNumberField}), z, c, a)
   return z
end

function (a::AnticNumberField)(c::QQFieldElem)
   z = nf_elem(a)
   ccall((:nf_elem_set_fmpq, libantic), Nothing,
         (Ref{nf_elem}, Ref{QQFieldElem}, Ref{AnticNumberField}), z, c, a)
   return z
end

(a::AnticNumberField)(c::Rational) = a(QQFieldElem(c))

function (a::AnticNumberField)(b::nf_elem)
   parent(b) == a && return b
   force_coerce(a, b)
end

function (a::AnticNumberField)(pol::QQPolyRingElem)
   pol = parent(a.pol)(pol) # check pol has correct parent
   z = nf_elem(a)
   if length(pol) >= length(a.pol)
      pol = mod(pol, a.pol)
   end
   ccall((:nf_elem_set_fmpq_poly, libantic), Nothing,
         (Ref{nf_elem}, Ref{QQPolyRingElem}, Ref{AnticNumberField}), z, pol, a)
   return z
end

function (a::QQPolyRing)(b::nf_elem)
   parent(parent(b).pol) != a && error("Cannot coerce from number field to polynomial ring")
   r = a()
   ccall((:nf_elem_get_fmpq_poly, libantic), Nothing,
         (Ref{QQPolyRingElem}, Ref{nf_elem}, Ref{AnticNumberField}), r, b, parent(b))
   return r
end

###############################################################################
#
#   Random generation
#
###############################################################################

RandomExtensions.maketype(K::AnticNumberField, _) = elem_type(K)

function rand(rng::AbstractRNG, sp::SamplerTrivial{<:Make2{nf_elem, AnticNumberField,
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
