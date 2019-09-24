###############################################################################
#
#   nf_elem.jl : Antic number fields
#
###############################################################################

export AnticNumberField, nf_elem, norm, representation_matrix,
       representation_matrix_q, tr, CyclotomicField, MaximalRealSubfield,
       add!, sub!, mul!, signature, sqr_classical, isrational, isinteger

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{nf_elem}) = AnticNumberField

@doc Markdown.doc"""
    parent(a::nf_elem)
> Return the parent of the given number field element.
"""
parent(a::nf_elem) = a.parent

elem_type(::Type{AnticNumberField}) = nf_elem

@doc Markdown.doc"""
    base_ring(a::AnticNumberField)
> Returns `Union{}` since a number field doesn't depend on any ring.
"""
base_ring(a::AnticNumberField) = Union{}

@doc Markdown.doc"""
    base_ring(a::nf_elem)
> Returns `Union{}` since a number field doesn't depend on any ring.
"""
base_ring(a::nf_elem) = Union{}

isdomain_type(::Type{nf_elem}) = true

@doc Markdown.doc"""
    var(a::AnticNumberField)
> Returns the identifier (as a symbol, not a string), that is used for printing
> the generator of the given number field.
"""
var(a::AnticNumberField) = a.S

function check_parent(a::nf_elem, b::nf_elem)
   a.parent != b.parent && error("Incompatible number field elements")
end

show_minus_one(::Type{nf_elem}) = false

###############################################################################
#
#   Basic manipulation
#
###############################################################################

const hash_seed = UInt==UInt64 ? 0xc2a44fbe466a1827 : 0xc2a44fb

function hash(a::nf_elem, h::UInt)
   global hash_seed
   b = hash_seed
   b = _hash_integer(a.elem_den, b)
   d = degree(parent(a))
   if d < 3
       x = fmpz()
       for i in 1:d
             b = xor(b, xor(hash(num_coeff!(x, a, i-1), h), h))
             b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
       end
   else
       GC.@preserve a for i in 1:a.elem_length
             b = xor(b, xor(_hash_integer(unsafe_load(Ptr{Int}(a.elem_coeffs), i), h), h))
             b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
       end
       for i in a.elem_length+1:d
             b = xor(b, xor(_hash_integer(0, h), h))
             b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
       end
   end

   return b
end

@doc Markdown.doc"""
    coeff(x::nf_elem, n::Int)
> Return the $n$-th coefficient of the polynomial representation of the given
> number field element. Coefficients are numbered from $0$, starting with the
> constant coefficient.
"""
function coeff(x::nf_elem, n::Int)
   n < 0 && throw(DomainError("Index must be non-negative: $n"))
   z = fmpq()
   ccall((:nf_elem_get_coeff_fmpq, :libantic), Nothing,
     (Ref{fmpq}, Ref{nf_elem}, Int, Ref{AnticNumberField}), z, x, n, parent(x))
   return z
end

function num_coeff!(z::fmpz, x::nf_elem, n::Int)
   n < 0 && throw(DomainError("Index must be non-negative: $n"))
   ccall((:nf_elem_get_coeff_fmpz, :libantic), Nothing,
     (Ref{fmpz}, Ref{nf_elem}, Int, Ref{AnticNumberField}), z, x, n, parent(x))
   return z
end

@doc Markdown.doc"""
    gen(a::AnticNumberField)
> Return the generator of the given number field.
"""
function gen(a::AnticNumberField)
   r = nf_elem(a)
   ccall((:nf_elem_gen, :libantic), Nothing,
         (Ref{nf_elem}, Ref{AnticNumberField}), r, a)
   return r
end

@doc Markdown.doc"""
    one(a::AnticNumberField)
> Return the multiplicative identity, i.e. one, in the given number field.
"""
function one(a::AnticNumberField)
   r = nf_elem(a)
   ccall((:nf_elem_one, :libantic), Nothing,
         (Ref{nf_elem}, Ref{AnticNumberField}), r, a)
   return r
end

@doc Markdown.doc"""
    zero(a::AnticNumberField)
> Return the multiplicative identity, i.e. one, in the given number field.
"""
function zero(a::AnticNumberField)
   r = nf_elem(a)
   ccall((:nf_elem_zero, :libantic), Nothing,
         (Ref{nf_elem}, Ref{AnticNumberField}), r, a)
   return r
end

@doc Markdown.doc"""
    isgen(a::nf_elem)
> Return `true` if the given number field element is the generator of the
> number field, otherwise return `false`.
"""
function isgen(a::nf_elem)
   return ccall((:nf_elem_is_gen, :libantic), Bool,
                (Ref{nf_elem}, Ref{AnticNumberField}), a, a.parent)
end

@doc Markdown.doc"""
    isone(a::nf_elem)
> Return `true` if the given number field element is the multiplicative
> identity of the number field, i.e. one, otherwise return `false`.
"""
function isone(a::nf_elem)
   return ccall((:nf_elem_is_one, :libantic), Bool,
                (Ref{nf_elem}, Ref{AnticNumberField}), a, a.parent)
end

@doc Markdown.doc"""
    iszero(a::nf_elem)
> Return `true` if the given number field element is the additive
> identity of the number field, i.e. zero, otherwise return `false`.
"""
function iszero(a::nf_elem)
   return ccall((:nf_elem_is_zero, :libantic), Bool,
                (Ref{nf_elem}, Ref{AnticNumberField}), a, a.parent)
end

@doc Markdown.doc"""
    isunit(a::nf_elem)
> Return `true` if the given number field element is invertible, i.e. nonzero,
> otherwise return `false`.
"""
isunit(a::nf_elem) = !iszero(a)

@doc Markdown.doc"""
    isinteger(a::nf_elem)
> Return `true` if the given number field element is an integer, otherwise
> return `false`.
"""
function isinteger(a::nf_elem)
   b = ccall((:nf_elem_is_integer, :libantic), Cint,
             (Ref{nf_elem}, Ref{AnticNumberField}), a, a.parent)
   return Bool(b)
end

@doc Markdown.doc"""
    isrational(a::nf_elem)
> Return `true` if the given number field element is a rational number,
> otherwise `false`.
"""
function isrational(a::nf_elem)
   b = ccall((:nf_elem_is_rational, :libantic), Cint,
             (Ref{nf_elem}, Ref{AnticNumberField}), a, a.parent)
   return Bool(b)
end

@doc Markdown.doc"""
    denominator(a::nf_elem)
> Return the denominator of the polynomial representation of the given number
> field element.
"""
function denominator(a::nf_elem)
   z = fmpz()
   ccall((:nf_elem_get_den, :libantic), Nothing,
         (Ref{fmpz}, Ref{nf_elem}, Ref{AnticNumberField}),
         z, a, a.parent)
   return z
end

function elem_from_mat_row(a::AnticNumberField, b::fmpz_mat, i::Int, d::fmpz)
   Generic._checkbounds(nrows(b), i) || throw(BoundsError())
   ncols(b) == degree(a) || error("Wrong number of columns")
   z = a()
   ccall((:nf_elem_set_fmpz_mat_row, :libantic), Nothing,
        (Ref{nf_elem}, Ref{fmpz_mat}, Int, Ref{fmpz}, Ref{AnticNumberField}),
        z, b, i - 1, d, a)
   return z
end

function elem_to_mat_row!(a::fmpz_mat, i::Int, d::fmpz, b::nf_elem)
   ccall((:nf_elem_get_fmpz_mat_row, :libantic), Nothing,
         (Ref{fmpz_mat}, Int, Ref{fmpz}, Ref{nf_elem}, Ref{AnticNumberField}),
         a, i - 1, d, b, b.parent)
   nothing
 end

@doc Markdown.doc"""
    degree(a::AnticNumberField)
> Return the degree of the given number field, i.e. the degree of its
> defining polynomial.
"""
degree(a::AnticNumberField) = a.pol_length-1

@doc Markdown.doc"""
    signature(a::AnticNumberField)
> Return the signature of the given number field, i.e. a tuple $r, s$
> consisting of $r$, the number of real embeddings and $s$, half the number of
> complex embeddings.
"""
signature(a::AnticNumberField) = signature(a.pol)

function deepcopy_internal(d::nf_elem, dict::IdDict)
   z = nf_elem(parent(d), d)
   return z
end

###############################################################################
#
#   Deal with "Special"
#
###############################################################################
function set_special(K::AnticNumberField, P::Pair{Symbol, <: Any}...)
  d = _get_Special_of_nf(K, false)
  if d === nothing
    d = Dict{Symbol, Any}()
    _set_Special_of_nf(K, d)
  end
  for (S, V) in P
    d[S] = V
  end
end

function get_special(K::AnticNumberField, S::Symbol)
  d = _get_Special_of_nf(K, false)
  if d === nothing
    return nothing
  end
  if haskey(d, S)
    return d[S]
  else
    return nothing
  end
end

function iscyclo_type(K::AnticNumberField)
  return !(get_special(K, :cyclo) === nothing) ::Bool
end

function ismaxreal_type(K::AnticNumberField)
  return get_special(K, :maxreal)::Bool
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::AnticNumberField)
   s = get_special(a, :show)
   if !(s === nothing)
     return s(io, a)
   end
   print(io, "Number field over Rational Field")
   print(io, " with defining polynomial ", a.pol)
end

function show(io::IO, x::nf_elem)
   cstr = ccall((:nf_elem_get_str_pretty, :libantic), Ptr{UInt8},
                (Ref{nf_elem}, Ptr{UInt8}, Ref{AnticNumberField}),
                 x, string(var(parent(x))), parent(x))
   s = unsafe_string(cstr)
   ccall((:flint_free, :libflint), Nothing, (Ptr{UInt8},), cstr)

   s = replace(s, "/" => "//")

   print(io, s)

end

needs_parentheses(::Nemo.nf_elem) = true

displayed_with_minus_in_front(::nf_elem) = false

canonical_unit(x::nf_elem) = x

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(a::nf_elem)
   r = a.parent()
   ccall((:nf_elem_neg, :libantic), Nothing,
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
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_add, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::nf_elem, b::nf_elem)
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_sub, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::nf_elem, b::nf_elem)
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_mul, :libantic), Nothing,
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
   ccall((:nf_elem_add_si, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function +(a::nf_elem, b::fmpz)
   r = a.parent()
   ccall((:nf_elem_add_fmpz, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpz}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function +(a::nf_elem, b::fmpq)
   r = a.parent()
   ccall((:nf_elem_add_fmpq, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpq}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::nf_elem, b::Int)
   r = a.parent()
   ccall((:nf_elem_sub_si, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::nf_elem, b::fmpz)
   r = a.parent()
   ccall((:nf_elem_sub_fmpz, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpz}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::nf_elem, b::fmpq)
   r = a.parent()
   ccall((:nf_elem_sub_fmpq, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpq}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function -(a::Int, b::nf_elem)
   r = b.parent()
   ccall((:nf_elem_si_sub, :libantic), Nothing,
         (Ref{nf_elem}, Int, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, b.parent)
   return r
end

function -(a::fmpz, b::nf_elem)
   r = b.parent()
   ccall((:nf_elem_fmpz_sub, :libantic), Nothing,
         (Ref{nf_elem}, Ref{fmpz}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, b.parent)
   return r
end

function -(a::fmpq, b::nf_elem)
   r = b.parent()
   ccall((:nf_elem_fmpq_sub, :libantic), Nothing,
         (Ref{nf_elem}, Ref{fmpq}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, b.parent)
   return r
end

+(a::nf_elem, b::Integer) = a + fmpz(b)

-(a::nf_elem, b::Integer) = a - fmpz(b)

-(a::Integer, b::nf_elem) = fmpz(a) - b

+(a::Integer, b::nf_elem) = b + a

+(a::fmpq, b::nf_elem) = b + a

+(a::Rational, b::nf_elem) = fmpq(a) + b

+(a::nf_elem, b::Rational) = b + a

-(a::Rational, b::nf_elem) = fmpq(a) - b

-(a::nf_elem, b::Rational) = a - fmpq(b)

function *(a::nf_elem, b::Int)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_si, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::nf_elem, b::fmpz)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_fmpz, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpz}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::nf_elem, b::fmpq)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_fmpq, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpq}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function *(a::Rational, b::nf_elem)
  return fmpq(a) * b
end

*(a::nf_elem, b::Rational) = b * a

*(a::nf_elem, b::Integer) = a * fmpz(b)

*(a::Integer, b::nf_elem) = b * a

*(a::fmpz, b::nf_elem) = b * a

*(a::fmpq, b::nf_elem) = b * a

//(a::nf_elem, b::Int) = divexact(a, b)

//(a::nf_elem, b::fmpz) = divexact(a, b)

//(a::nf_elem, b::Integer) = a//fmpz(b)

//(a::nf_elem, b::fmpq) = divexact(a, b)

//(a::Integer, b::nf_elem) = divexact(a, b)

//(a::fmpz, b::nf_elem) = divexact(a, b)

//(a::fmpq, b::nf_elem) = divexact(a, b)

//(a::Rational, b::nf_elem) = divexact(fmpq(a), b)

//(a::nf_elem, b::Rational) = divexact(a, fmpq(b))

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::nf_elem, n::Int)
   r = a.parent()
   ccall((:nf_elem_pow, :libantic), Nothing,
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
   check_parent(a, b)
   return ccall((:nf_elem_equal, :libantic), Bool,
           (Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}), a, b, a.parent)
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::nf_elem, b::fmpz)
   b = ccall((:nf_elem_equal_fmpz, :libantic), Cint,
             (Ref{nf_elem}, Ref{fmpz}, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

function ==(a::nf_elem, b::fmpq)
   b = ccall((:nf_elem_equal_fmpq, :libantic), Cint,
             (Ref{nf_elem}, Ref{fmpq}, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

function ==(a::nf_elem, b::Int)
   b = ccall((:nf_elem_equal_si, :libantic), Cint,
             (Ref{nf_elem}, Int, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

function ==(a::nf_elem, b::UInt)
   b = ccall((:nf_elem_equal_ui, :libantic), Cint,
             (Ref{nf_elem}, UInt, Ref{AnticNumberField}),
              a, b, a.parent)
   return Bool(b)
end

==(a::nf_elem, b::Integer) = a == fmpz(b)

==(a::nf_elem, b::Rational) = a == fmpq(b)

==(a::fmpz, b::nf_elem) = b == a

==(a::fmpq, b::nf_elem) = b == a

==(a::Int, b::nf_elem) = b == a

==(a::UInt, b::nf_elem) = b == a

==(a::Integer, b::nf_elem) = b == a

==(a::Rational, b::nf_elem) = b == a

###############################################################################
#
#   Inversion
#
###############################################################################

@doc Markdown.doc"""
    inv(a::nf_elem)
> Return $a^{-1}$. Requires $a \neq 0$.
"""
function inv(a::nf_elem)
   iszero(a) && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_inv, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, a.parent)
   return r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::nf_elem, b::nf_elem)
   iszero(b) && throw(DivideError())
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_div, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(a::nf_elem, b::Int)
   b == 0 && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_si, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

function divexact(a::nf_elem, b::fmpz)
   iszero(b) && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_fmpz, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpz}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

divexact(a::nf_elem, b::Integer) = divexact(a, fmpz(b))

function divexact(a::nf_elem, b::fmpq)
   iszero(b) && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_fmpq, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpq}, Ref{AnticNumberField}),
         r, a, b, a.parent)
   return r
end

divexact(a::Integer, b::nf_elem) = inv(b)*a

divexact(a::fmpz, b::nf_elem) = inv(b)*a

divexact(a::fmpq, b::nf_elem) = inv(b)*a

###############################################################################
#
#   Removal and valuation
#
###############################################################################

@doc Markdown.doc"""
    divides(a::nf_elem, b::nf_elem)
> Returns a pair consisting of a flag which is set to `true` if $b$ divides
> $a$ and `false` otherwise, and a number field element $h$ such that $a = bh$
> if such exists. If not, the value of $h$ is undetermined.
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

@doc Markdown.doc"""
    norm(a::nf_elem)
> Return the absolute norm of $a$. The result will be a rational number.
"""
function norm(a::nf_elem)
   z = fmpq()
   ccall((:nf_elem_norm, :libantic), Nothing,
         (Ref{fmpq}, Ref{nf_elem}, Ref{AnticNumberField}),
         z, a, a.parent)
   return z
end

@doc Markdown.doc"""
    tr(a::nf_elem)
> Return the absolute trace of $a$. The result will be a rational number.
"""
function tr(a::nf_elem)
   z = fmpq()
   ccall((:nf_elem_trace, :libantic), Nothing,
         (Ref{fmpq}, Ref{nf_elem}, Ref{AnticNumberField}),
         z, a, a.parent)
   return z
end

@doc Markdown.doc"""
    representation_matrix(a::nf_elem)
> Return a matrix with rational entries representing multiplication with $a$
> with respect to the power basis of the generator of the parent of $a$.
> The matrix is of type fmpq_mat.
"""
function representation_matrix(a::nf_elem)
  K = parent(a)
  z = fmpq_mat(degree(K), degree(K))
  z.base_ring = FlintQQ
  ccall((:nf_elem_rep_mat, :libantic), Nothing,
        (Ref{fmpq_mat}, Ref{nf_elem}, Ref{AnticNumberField}), z, a, K)
  return z
end

@doc Markdown.doc"""
    representation_matrix_q(a::nf_elem)
> Return a matrix  representing multiplication with $a$ with respect to the
> power basis of the generator of the parent of $a$.
> The matrix is returned as a tuple (fmpz_mat, fmpz), consisting of the
> a primitive integer matrix and a denominator.
"""
function representation_matrix_q(a::nf_elem)
  K = parent(a)
  z = fmpz_mat(degree(K), degree(K))
  z.base_ring = FlintZZ
  d = fmpz()
  ccall((:nf_elem_rep_mat_fmpz_mat_den, :libantic), Nothing,
        (Ref{fmpz_mat}, Ref{fmpz}, Ref{nf_elem}, Ref{AnticNumberField}),
        z, d, a, K)
  return z, d
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::nf_elem)
   ccall((:nf_elem_zero, :libantic), Nothing,
         (Ref{nf_elem}, Ref{AnticNumberField}), a, parent(a))
   return a
end

function mul!(z::nf_elem, x::nf_elem, y::nf_elem)
   ccall((:nf_elem_mul, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
                                                  z, x, y, parent(x))
   return z
end

@doc Markdown.doc"""
    mul_red!(z::nf_elem, x::nf_elem, y::nf_elem, red::Bool)
> Multiply $a$ by $b$ and set the existing number field element $c$ to the
> result. Reduction modulo the defining polynomial is only performed if `red` is
> set to `true`. Note that $a$ and $b$ must be reduced. This function is provided
> for performance reasons as it saves allocating a new object for the result and
> eliminates associated garbage collection.
"""
function mul_red!(z::nf_elem, x::nf_elem, y::nf_elem, red::Bool)
   ccall((:nf_elem_mul_red, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}, Cint),
                                                z, x, y, parent(x), red)
   return z
end

function addeq!(z::nf_elem, x::nf_elem)
   ccall((:nf_elem_add, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
                                                  z, z, x, parent(x))
   return z
end

function add!(a::nf_elem, b::nf_elem, c::nf_elem)
   ccall((:nf_elem_add, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{nf_elem}, Ref{AnticNumberField}),
         a, b, c, a.parent)
  return a
end

@doc Markdown.doc"""
    reduce!(x::nf_elem)
> Reduce the given number field element by the defining polynomial, in-place.
> This only needs to be done after accumulating values computed by `mul_red!`
> where reduction has not been performed. All standard Nemo number field
> functions automatically reduce their outputs.
"""
function reduce!(x::nf_elem)
   ccall((:nf_elem_reduce, :libantic), Nothing,
         (Ref{nf_elem}, Ref{AnticNumberField}), x, parent(x))
   return x
end

###############################################################################
#
#   Ad hoc unsafe functions
#
###############################################################################

function add!(c::nf_elem, a::nf_elem, b::fmpq)
   ccall((:nf_elem_add_fmpq, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpq}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function add!(c::nf_elem, a::nf_elem, b::fmpz)
   ccall((:nf_elem_add_fmpz, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpz}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function add!(c::nf_elem, a::nf_elem, b::Int)
   ccall((:nf_elem_add_si, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

add!(c::nf_elem, a::nf_elem, b::Integer) = add!(c, a, fmpz(b))

function sub!(c::nf_elem, a::nf_elem, b::fmpq)
   ccall((:nf_elem_sub_fmpq, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpq}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::nf_elem, a::nf_elem, b::fmpz)
   ccall((:nf_elem_sub_fmpz, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpz}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::nf_elem, a::nf_elem, b::Int)
   ccall((:nf_elem_sub_si, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

sub!(c::nf_elem, a::nf_elem, b::Integer) = sub!(c, a, fmpz(b))

function sub!(c::nf_elem, a::fmpq, b::nf_elem)
   ccall((:nf_elem_fmpq_sub, :libantic), Nothing,
         (Ref{nf_elem}, Ref{fmpq}, Ref{nf_elem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::nf_elem, a::fmpz, b::nf_elem)
   ccall((:nf_elem_fmpz_sub, :libantic), Nothing,
         (Ref{nf_elem}, Ref{fmpz}, Ref{nf_elem}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function sub!(c::nf_elem, a::Int, b::nf_elem)
   ccall((:nf_elem_si_sub, :libantic), Nothing,
         (Ref{nf_elem}, Int, Ref{nf_elem}, Ref{AnticNumberField}),
         c, a, b, b.parent)
   return c
end

sub!(c::nf_elem, a::Integer, b::nf_elem) = sub!(c, fmpz(a), b)

function mul!(c::nf_elem, a::nf_elem, b::fmpq)
   ccall((:nf_elem_scalar_mul_fmpq, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpq}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function mul!(c::nf_elem, a::nf_elem, b::fmpz)
   ccall((:nf_elem_scalar_mul_fmpz, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Ref{fmpz}, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

function mul!(c::nf_elem, a::nf_elem, b::Int)
   ccall((:nf_elem_scalar_mul_si, :libantic), Nothing,
         (Ref{nf_elem}, Ref{nf_elem}, Int, Ref{AnticNumberField}),
         c, a, b, a.parent)
   return c
end

mul!(c::nf_elem, a::nf_elem, b::Integer) = mul!(c, a, fmpz(b))

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

   set_length!(z, normalise(z, lenz))

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

   set_length!(z, normalise(z, lenz))

   return z
end

function *(a::Generic.Poly{nf_elem}, b::Generic.Poly{nf_elem})
   check_parent(a, b)
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
   set_length!(f, lena)
   if a !== b
      g = S()
      fit!(g, lenb)
      for i = 1:lenb
         g = setcoeff!(g, i - 1, R(coeff(b, i - 1)))
      end
      set_length!(g, lenb)
   else
      g = f
   end
   p = f*g
   fit!(r, lenr)
   for i = 1:lenr
      r.coeffs[i] = K(p.coeffs[i])
   end
   set_length!(r, normalise(r, lenr))
   return r
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{nf_elem}, ::Type{T}) where {T <: Integer} = nf_elem

promote_rule(::Type{nf_elem}, ::Type{fmpz}) = nf_elem

promote_rule(::Type{nf_elem}, ::Type{fmpq}) = nf_elem

promote_rule(::Type{nf_elem}, ::Type{fmpq_poly}) = nf_elem

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

@doc Markdown.doc"""
    (a::AnticNumberField)()

> Return an empty (0) element.
"""
function (a::AnticNumberField)()
   z = nf_elem(a)
   ccall((:nf_elem_set_si, :libantic), Nothing,
         (Ref{nf_elem}, Int, Ref{AnticNumberField}), z, 0, a)
   return z
end

@doc Markdown.doc"""
    (a::AnticNumberField)(c::Int)

> Return $c$ as an element in $a$.
"""
function (a::AnticNumberField)(c::Int)
   z = nf_elem(a)
   ccall((:nf_elem_set_si, :libantic), Nothing,
         (Ref{nf_elem}, Int, Ref{AnticNumberField}), z, c, a)
   return z
end

(a::AnticNumberField)(c::Integer) = a(fmpz(c))

function (a::AnticNumberField)(c::fmpz)
   z = nf_elem(a)
   ccall((:nf_elem_set_fmpz, :libantic), Nothing,
         (Ref{nf_elem}, Ref{fmpz}, Ref{AnticNumberField}), z, c, a)
   return z
end

function (a::AnticNumberField)(c::fmpq)
   z = nf_elem(a)
   ccall((:nf_elem_set_fmpq, :libantic), Nothing,
         (Ref{nf_elem}, Ref{fmpq}, Ref{AnticNumberField}), z, c, a)
   return z
end

(a::AnticNumberField)(c::Rational) = a(fmpq(c))

function (a::AnticNumberField)(b::nf_elem)
   parent(b) != a && error("Cannot coerce number field element")
   return b
end

function (a::AnticNumberField)(pol::fmpq_poly)
   pol = parent(a.pol)(pol) # check pol has correct parent
   z = nf_elem(a)
   if length(pol) >= length(a.pol)
      pol = mod(pol, a.pol)
   end
   ccall((:nf_elem_set_fmpq_poly, :libantic), Nothing,
         (Ref{nf_elem}, Ref{fmpq_poly}, Ref{AnticNumberField}), z, pol, a)
   return z
end

function (a::FmpqPolyRing)(b::nf_elem)
   parent(parent(b).pol) != a && error("Cannot coerce from number field to polynomial ring")
   r = a()
   ccall((:nf_elem_get_fmpq_poly, :libantic), Nothing,
         (Ref{fmpq_poly}, Ref{nf_elem}, Ref{AnticNumberField}), r, b, parent(b))
   return r
end

###############################################################################
#
#   Random generation
#
###############################################################################

function rand(rng::AbstractRNG, K::AnticNumberField, r::UnitRange{Int})
   R = parent(K.pol)
   n = degree(K.pol)
   return K(rand(rng, R, (n-1):(n-1), r))
end

###############################################################################
#
#   AnticNumberField constructor
#
###############################################################################

@doc Markdown.doc"""
    NumberField(f::fmpq_poly, s::AbstractString; cached::Bool = true, check::Bool = true)
> Return a tuple $R, x$ consisting of the parent object $R$ and generator $x$
> of the number field $\mathbb{Q}/(f)$ where $f$ is the supplied polynomial.
> The supplied string `s` specifies how the generator of the number field
> should be printed.
"""
function NumberField(f::fmpq_poly, s::AbstractString; cached::Bool = true, check::Bool = true)
   S = Symbol(s)
   parent_obj = AnticNumberField(f, S, cached, check)

   return parent_obj, gen(parent_obj)
end

@doc Markdown.doc"""
    CyclotomicField(n::Int, s::AbstractString, t = "\$"; cached = true)
> Return a tuple $R, x$ consisting of the parent object $R$ and generator $x$
> of the $n$-th cyclotomic field, $\mathbb{Q}(\zeta_n)$. The supplied string
> `s` specifies how the generator of the number field should be printed. If
> provided, the string `t` specifies how the generator of the polynomial ring
> from which the number field is constructed, should be printed. If it is not
> supplied, a default dollar sign will be used to represent the variable.
"""
function CyclotomicField(n::Int, s::AbstractString = "z_$n", t = "_\$"; cached = true)
   Zx, x = PolynomialRing(FlintZZ, string(gensym()); cached = cached)
   Qx, = PolynomialRing(FlintQQ, t; cached = cached)
   f = cyclotomic(n, x)
   C, g = NumberField(Qx(f), s; cached = cached, check = false)
   set_special(C, :show => show_cyclo, :cyclo => n)
   return C, g
end

function show_cyclo(io::IO, a::AnticNumberField)
  @assert iscyclo_type(a)
  print(io, "Cyclotomic field of order $(get_special(a, :cyclo))")
end


@doc Markdown.doc"""
    MaximalRealSubfield(n::Int, s::AbstractString, t = "\$"; cached = true)
> Return a tuple $R, x$ consisting of the parent object $R$ and generator $x$
> of the totally real subfield of the $n$-th cyclotomic field,
> $\mathbb{Q}(\zeta_n)$. The supplied string `s` specifies how the generator of
> the number field should be printed. If provided, the string `t` specifies how
> the generator of the polynomial ring from which the number field is
> constructed, should be printed. If it is not supplied, a default dollar sign
> will be used to represent the variable.
"""
function MaximalRealSubfield(n::Int, s::AbstractString = "(z_$n + 1/z_$n)", t = "\$"; cached = true)
   Zx, x = PolynomialRing(FlintZZ, string(gensym()); cached = cached)
   Qx, = PolynomialRing(FlintQQ, t; cached = cached)
   f = cos_minpoly(n, x)
   R, a =  NumberField(Qx(f), s; cached = cached, check = false)
   set_special(R, :show => show_maxreal, :maxreal => n)
   return R, a
end

function show_maxreal(io::IO, a::AnticNumberField)
  print(io, "Maximal real subfield of cyclotomic field of order $(get_special(a, :maxreal))")
end
