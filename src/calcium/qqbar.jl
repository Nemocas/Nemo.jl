###############################################################################
#
#   qqbar.jl : Calcium algebraic numbers in minimal polynomial representation
#
###############################################################################

export qqbar, CalciumQQBar, CalciumQQBarField, isalgebraic_integer, rand, abs2,
       csgn, sign_real, sign_imag, fmpq, fmpz, exp_pi_i, atanpi, asinpi, acospi,
       conjugates, eigenvalues, guess, root_of_unity_as_args, isroot_of_unity,
       log_pi_i, rand

export isequal_real, isequal_imag, isequal_abs, isequal_abs_real,
       isequal_abs_imag, isequal_root_order, isless_real, isless_imag,
       isless_abs, isless_abs_real, isless_abs_imag, isless_root_order

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

parent(a::qqbar) = CalciumQQBar

parent_type(::Type{qqbar}) = CalciumQQBarField

elem_type(::Type{CalciumQQBarField}) = qqbar

base_ring(a::CalciumQQBarField) = CalciumQQBar

base_ring(a::qqbar) = CalciumQQBar

isdomain_type(::Type{qqbar}) = true

check_parent(a::qqbar, b::qqbar, throw::Bool = true) = true

###############################################################################
#
#   Hashing
#
###############################################################################

# todo: want a C function for this
function Base.hash(a::qqbar, h::UInt)
   R, x = PolynomialRing(FlintZZ, "x")
   return xor(hash(minpoly(R, a)), h)
end

###############################################################################
#
#   Constructors
#
###############################################################################

function qqbar(a::Int)
   z = qqbar()
   ccall((:qqbar_set_si, libcalcium), Nothing, (Ref{qqbar}, Int, ), z, a)
  return z
end

function qqbar(a::Complex{Int})
   r = qqbar(real(a))
   s = qqbar(imag(a))
   z = qqbar()
   ccall((:qqbar_set_re_im, libcalcium),
        Nothing, (Ref{qqbar}, Ref{qqbar}, Ref{qqbar}), z, r, s)
  return z
end

function qqbar(a::fmpz)
   z = qqbar()
   ccall((:qqbar_set_fmpz, libcalcium),
        Nothing, (Ref{qqbar}, Ref{fmpz}, ), z, a)
   return z
end

function qqbar(a::fmpq)
   z = qqbar()
   ccall((:qqbar_set_fmpq, libcalcium),
        Nothing, (Ref{qqbar}, Ref{fmpq}, ), z, a)
   return z
end

function deepcopy_internal(a::qqbar, dict::IdDict)
   z = qqbar()
   ccall((:qqbar_set, libcalcium), Nothing, (Ref{qqbar}, Ref{qqbar}), z, a)
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::qqbar) = a

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

# todo
# function expressify(a::qqbar; context = nothing)::Any
# end

#=
function qqbar_vec(n::Int)
   return ccall((:_qqbar_vec_init, libcalcium), Ptr{qqbar_struct}, (Int,), n)
end

function array(R::CalciumQQBarField, v::Ptr{qqbar_struct}, n::Int)
   r = Vector{qqbar}(undef, n)
   for i=1:n
       r[i] = R()
       ccall((:qqbar_set, libcalcium), Nothing, (Ref{qqbar}, Ptr{qqbar_struct}),
           r[i], v + (i-1)*sizeof(qqbar_struct))
   end
   return r
end

function qqbar_vec_clear(v::Ptr{qqbar_struct}, n::Int)
   ccall((:_qqbar_vec_clear, libcalcium),
        Nothing, (Ptr{qqbar_struct}, Int), v, n)
end

function roots(f::fmpz_poly, R::CalciumQQBarField)
   deg = degree(f)
   if deg <= 0
      return Array{qqbar}(undef, 0)
   end
   roots = qqbar_vec(deg)
   ccall((:qqbar_roots_fmpz_poly, libcalcium),
        Nothing, (Ptr{qqbar_struct}, Ref{fmpz_poly}, Int), roots, f, 0)
   res = array(R, roots, deg)
   qqbar_vec_clear(roots, deg)
   return res
end
=#

function native_string(x::qqbar)
   cstr = ccall((:qqbar_get_str_nd, libcalcium),
        Ptr{UInt8}, (Ref{qqbar}, Int), x, Int(6))
   number = unsafe_string(cstr)
   ccall((:flint_free, libflint), Nothing, (Ptr{UInt8},), cstr)

   number = number[1:first(findfirst(" (", number))-1]
   number = replace(number, "I" => "im")

   R, Rx = PolynomialRing(ZZ, "x")
   polynomial = string(minpoly(R, x))
   polynomial = replace(polynomial, "*" => "")
   res = string("Root ", number, " of ", polynomial)

   return res
end

function show(io::IO, F::CalciumQQBarField)
  print(io, "Field of Algebraic Numbers in minimal polynomial representation")
end

function show(io::IO, x::qqbar)
   print(io, native_string(x))
end

needs_parentheses(x::qqbar) = true

###############################################################################
#
#   Basic manipulation
#
###############################################################################

zero(a::CalciumQQBarField) = a(0)

one(a::CalciumQQBarField) = a(1)

@doc Markdown.doc"""
    degree(x::qqbar)

Return the degree of the minimal polynomial of `x`.
"""
function degree(x::qqbar)
   return ccall((:qqbar_degree, libcalcium), Int, (Ref{qqbar}, ), x)
end

@doc Markdown.doc"""
    iszero(x::qqbar)

Return whether `x` is the number 0.
"""
function iszero(x::qqbar)
   return Bool(ccall((:qqbar_is_zero, libcalcium), Cint, (Ref{qqbar},), x))
end

@doc Markdown.doc"""
    isone(x::qqbar)

Return whether `x` is the number 1.
"""
function isone(x::qqbar)
   return Bool(ccall((:qqbar_is_one, libcalcium), Cint, (Ref{qqbar},), x))
end

@doc Markdown.doc"""
    isinteger(x::qqbar)

Return whether `x` is an integer.
"""
function isinteger(x::qqbar)
   return Bool(ccall((:qqbar_is_integer, libcalcium), Cint, (Ref{qqbar},), x))
end

@doc Markdown.doc"""
    isrational(x::qqbar)

Return whether `x` is a rational number.
"""
function isrational(x::qqbar)
   return Bool(ccall((:qqbar_is_rational, libcalcium), Cint, (Ref{qqbar},), x))
end

@doc Markdown.doc"""
    isreal(x::qqbar)

Return whether `x` is a real number.
"""
function isreal(x::qqbar)
   return Bool(ccall((:qqbar_is_real, libcalcium), Cint, (Ref{qqbar},), x))
end

@doc Markdown.doc"""
    isalgebraic_integer(x::qqbar)

Return whether `x` is an algebraic integer.
"""
function isalgebraic_integer(x::qqbar)
   return Bool(ccall((:qqbar_is_algebraic_integer, libcalcium),
        Cint, (Ref{qqbar},), x))
end

@doc Markdown.doc"""
    minpoly(R::FmpzPolyRing, x::qqbar)

Return the minimal polynomial of `x` as an element of the polynomial ring `R`.
"""
function minpoly(R::FmpzPolyRing, x::qqbar)
   z = R()
   ccall((:fmpz_poly_set, libflint),
        Nothing, (Ref{fmpz_poly}, Ref{qqbar}, ), z, x)
   return z
end

@doc Markdown.doc"""
    minpoly(R::FmpzPolyRing, x::qqbar)

Return the minimal polynomial of `x` as an element of the polynomial ring `R`.
"""
function minpoly(R::FmpqPolyRing, x::qqbar)
   z = R()
   ccall((:fmpq_poly_set_fmpz_poly, libflint),
        Nothing, (Ref{fmpq_poly}, Ref{qqbar}, ), z, x)
   return z
end

@doc Markdown.doc"""
    denominator(x::qqbar)

Return the denominator of `x`, defined as the leading coefficient of the
minimal polynomial of `x`. The result is returned as an `fmpz`.
"""
function denominator(x::qqbar)
   d = degree(x)
   q = fmpz()
   ccall((:fmpz_poly_get_coeff_fmpz, libflint),
        Nothing, (Ref{fmpz}, Ref{qqbar}, Int), q, x, d)
   return q
end

@doc Markdown.doc"""
    numerator(x::qqbar)

Return the numerator of `x`, defined as `x` multiplied by its denominator.
The result is an algebraic integer.
"""
function numerator(x::qqbar)
   return x * denominator(x)
end

@doc Markdown.doc"""
    height(x::qqbar)

Return the height of the algebraic number `x`. The result is an `fmpz` integer.
"""
function height(x::qqbar)
   z = fmpz()
   ccall((:qqbar_height, libcalcium), Nothing, (Ref{fmpz}, Ref{qqbar}, ), z, x)
   return z
end

@doc Markdown.doc"""
    height_bits(x::qqbar)

Return the height of the algebraic number `x` measured in bits.
The result is a Julia integer.
"""
function height_bits(x::qqbar)
   return ccall((:qqbar_height_bits, libcalcium), Int, (Ref{qqbar}, ), x)
end


###############################################################################
#
#   Random generation
#
###############################################################################

@doc Markdown.doc"""
    rand(R::CalciumQQBarField; degree::Int, bits::Int, randtype::Symbol=:null)

Return a random algebraic number with degree up to `degree`
and coefficients up to `bits` in size. By default, both real and
complex numbers are generated. Set the optional `randtype` to `:real` or
`:nonreal` to generate a specific type of number. Note that
nonreal numbers require `degree` at least 2.
"""
function rand(R::CalciumQQBarField; degree::Int, bits::Int,
                                            randtype::Symbol=:null)
   state = _flint_rand_states[Threads.threadid()]
   x = R()

   degree <= 0 && error("degree must be positive")
   bits <= 0 && error("bits must be positive")

   if randtype == :null
      ccall((:qqbar_randtest, libcalcium), Nothing,
          (Ref{qqbar}, Ptr{Cvoid}, Int, Int), x, state.ptr, degree, bits)
   elseif randtype == :real
      ccall((:qqbar_randtest_real, libcalcium), Nothing,
          (Ref{qqbar}, Ptr{Cvoid}, Int, Int), x, state.ptr, degree, bits)
   elseif randtype == :nonreal
      degree < 2 && error("nonreal requires degree >= 2")
      ccall((:qqbar_randtest_nonreal, libcalcium), Nothing,
          (Ref{qqbar}, Ptr{Cvoid}, Int, Int), x, state.ptr, degree, bits)
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

function -(a::qqbar)
   z = qqbar()
   ccall((:qqbar_neg, libcalcium), Nothing, (Ref{qqbar}, Ref{qqbar}), z, a)
   return z
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::qqbar, b::qqbar)
   z = qqbar()
   ccall((:qqbar_add, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{qqbar}), z, a, b)
   return z
end

function +(a::qqbar, b::fmpq)
   z = qqbar()
   ccall((:qqbar_add_fmpq, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{fmpq}), z, a, b)
   return z
end

function +(a::qqbar, b::fmpz)
   z = qqbar()
   ccall((:qqbar_add_fmpz, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{fmpz}), z, a, b)
   return z
end

function +(a::qqbar, b::Int)
   z = qqbar()
   ccall((:qqbar_add_si, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Int), z, a, b)
   return z
end

+(a::fmpq, b::qqbar) = b + a
+(a::fmpz, b::qqbar) = b + a
+(a::Int, b::qqbar) = b + a

function -(a::qqbar, b::qqbar)
   z = qqbar()
   ccall((:qqbar_sub, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{qqbar}), z, a, b)
   return z
end

function -(a::qqbar, b::fmpq)
   z = qqbar()
   ccall((:qqbar_sub_fmpq, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{fmpq}), z, a, b)
   return z
end

function -(a::qqbar, b::fmpz)
   z = qqbar()
   ccall((:qqbar_sub_fmpz, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{fmpz}), z, a, b)
   return z
end

function -(a::qqbar, b::Int)
   z = qqbar()
   ccall((:qqbar_sub_si, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Int), z, a, b)
   return z
end

function -(a::fmpq, b::qqbar)
   z = qqbar()
   ccall((:qqbar_fmpq_sub, libcalcium), Nothing,
         (Ref{qqbar}, Ref{fmpq}, Ref{qqbar}), z, a, b)
   return z
end

function -(a::fmpz, b::qqbar)
   z = qqbar()
   ccall((:qqbar_fmpz_sub, libcalcium), Nothing,
         (Ref{qqbar}, Ref{fmpz}, Ref{qqbar}), z, a, b)
   return z
end

function -(a::Int, b::qqbar)
   z = qqbar()
   ccall((:qqbar_si_sub, libcalcium), Nothing,
         (Ref{qqbar}, Int, Ref{qqbar}), z, a, b)
   return z
end

function *(a::qqbar, b::qqbar)
   z = qqbar()
   ccall((:qqbar_mul, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{qqbar}), z, a, b)
   return z
end

function *(a::qqbar, b::fmpq)
   z = qqbar()
   ccall((:qqbar_mul_fmpq, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{fmpq}), z, a, b)
   return z
end

function *(a::qqbar, b::fmpz)
   z = qqbar()
   ccall((:qqbar_mul_fmpz, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{fmpz}), z, a, b)
   return z
end

function *(a::qqbar, b::Int)
   z = qqbar()
   ccall((:qqbar_mul_si, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Int), z, a, b)
   return z
end

*(a::fmpq, b::qqbar) = b * a
*(a::fmpz, b::qqbar) = b * a
*(a::Int, b::qqbar) = b * a

function ^(a::qqbar, b::qqbar)
   z = qqbar()
   ok = Bool(ccall((:qqbar_pow, libcalcium), Cint,
         (Ref{qqbar}, Ref{qqbar}, Ref{qqbar}), z, a, b))
   !ok && throw(DomainError((a, b)))
   return z
end

# todo: want qqbar_pow_fmpz, qqbar_pow_fmpq, qqbar_pow_si
^(a::qqbar, b::fmpz) = a ^ qqbar(b)
^(a::qqbar, b::fmpq) = a ^ qqbar(b)
^(a::qqbar, b::Int) = a ^ qqbar(b)
^(a::fmpz, b::qqbar) = qqbar(a) ^ b
^(a::fmpq, b::qqbar) = qqbar(a) ^ b
^(a::Int, b::qqbar) = qqbar(a) ^ b

###############################################################################
#
#   Exact division
#
###############################################################################

function inv(a::qqbar)
   iszero(a) && throw(DivideError())
   z = qqbar()
   ccall((:qqbar_inv, libcalcium), Nothing, (Ref{qqbar}, Ref{qqbar}), z, a)
   return z
end

function divexact(a::qqbar, b::qqbar)
   iszero(b) && throw(DivideError())
   z = qqbar()
   ccall((:qqbar_div, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{qqbar}), z, a, b)
   return z
end

function divexact(a::qqbar, b::fmpq)
   iszero(b) && throw(DivideError())
   z = qqbar()
   ccall((:qqbar_div_fmpq, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{fmpq}), z, a, b)
   return z
end

function divexact(a::qqbar, b::fmpz)
   iszero(b) && throw(DivideError())
   z = qqbar()
   ccall((:qqbar_div_fmpz, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Ref{fmpz}), z, a, b)
   return z
end

function divexact(a::qqbar, b::Int)
   iszero(b) && throw(DivideError())
   z = qqbar()
   ccall((:qqbar_div_si, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Int), z, a, b)
   return z
end

//(a::qqbar, b::qqbar) = divexact(a, b)
//(a::qqbar, b::fmpq) = divexact(a, b)
//(a::qqbar, b::fmpz) = divexact(a, b)
//(a::qqbar, b::Int) = divexact(a, b)
//(a::fmpq, b::qqbar) = divexact(a, b)
//(a::fmpz, b::qqbar) = divexact(a, b)
//(a::Int, b::qqbar) = divexact(a, b)


function <<(a::qqbar, b::Int)
   z = qqbar()
   ccall((:qqbar_mul_2exp_si, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Int), z, a, b)
   return z
end

function >>(a::qqbar, b::Int)
   z = qqbar()
   ccall((:qqbar_mul_2exp_si, libcalcium), Nothing,
         (Ref{qqbar}, Ref{qqbar}, Int), z, a, -b)
   return z
end

###############################################################################
#
#   Polynomial evaluation
#
###############################################################################

function evaluate(x::fmpq_poly, y::qqbar)
   z = qqbar()
   ccall((:qqbar_evaluate_fmpq_poly, libcalcium), Nothing,
                (Ref{qqbar}, Ref{fmpq_poly}, Ref{qqbar}), z, x, y)
   return z
end

function evaluate(x::fmpz_poly, y::qqbar)
   z = qqbar()
   ccall((:qqbar_evaluate_fmpz_poly, libcalcium), Nothing,
                (Ref{qqbar}, Ref{fmpz_poly}, Ref{qqbar}), z, x, y)
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::qqbar, b::qqbar)
   return Bool(ccall((:qqbar_equal, libcalcium), Cint,
                (Ref{qqbar}, Ref{qqbar}), a, b))
end

function cmp(a::qqbar, b::qqbar)
   !isreal(a) && throw(DomainError(a, "comparing nonreal numbers"))
   !isreal(b) && throw(DomainError(b, "comparing nonreal numbers"))
   return ccall((:qqbar_cmp_re, libcalcium), Cint,
                (Ref{qqbar}, Ref{qqbar}), a, b)
end

isless(a::qqbar, b::qqbar) = cmp(a, b) < 0
isless(a::qqbar, b::fmpz) = isless(a, qqbar(b))
isless(a::qqbar, b::fmpq) = isless(a, qqbar(b))
isless(a::qqbar, b::Int) = isless(a, qqbar(b))
isless(a::fmpq, b::qqbar) = isless(qqbar(a), b)
isless(a::fmpz, b::qqbar) = isless(qqbar(a), b)
isless(a::Int, b::qqbar) = isless(qqbar(a), b)

# todo: export the cmp functions?
cmp_real(a::qqbar, b::qqbar) = ccall((:qqbar_cmp_re, libcalcium),
    Cint, (Ref{qqbar}, Ref{qqbar}), a, b)
cmp_imag(a::qqbar, b::qqbar) = ccall((:qqbar_cmp_im, libcalcium),
    Cint, (Ref{qqbar}, Ref{qqbar}), a, b)
cmpabs(a::qqbar, b::qqbar) = ccall((:qqbar_cmpabs, libcalcium),
    Cint, (Ref{qqbar}, Ref{qqbar}), a, b)
cmpabs_real(a::qqbar, b::qqbar) = ccall((:qqbar_cmpabs_re, libcalcium),
    Cint, (Ref{qqbar}, Ref{qqbar}), a, b)
cmpabs_imag(a::qqbar, b::qqbar) = ccall((:qqbar_cmpabs_im, libcalcium),
    Cint, (Ref{qqbar}, Ref{qqbar}), a, b)
cmp_root_order(a::qqbar, b::qqbar) = ccall((:qqbar_cmp_root_order, libcalcium),
    Cint, (Ref{qqbar}, Ref{qqbar}), a, b)

@doc Markdown.doc"""
    isequal_real(a::qqbar, b::qqbar)

Compares the real parts of `a` and `b`.
"""
isequal_real(a::qqbar, b::qqbar) = cmp_real(a, b) == 0

@doc Markdown.doc"""
    isequal_imag(a::qqbar, b::qqbar)

Compares the imaginary parts of `a` and `b`.
"""
isequal_imag(a::qqbar, b::qqbar) = cmp_imag(a, b) == 0

@doc Markdown.doc"""
    isequal_abs(a::qqbar, b::qqbar)

Compares the absolute values of `a` and `b`.
"""
isequal_abs(a::qqbar, b::qqbar) = cmpabs(a, b) == 0

@doc Markdown.doc"""
    isequal_abs_real(a::qqbar, b::qqbar)

Compares the absolute values of the real parts of `a` and `b`.
"""
isequal_abs_real(a::qqbar, b::qqbar) = cmpabs_real(a, b) == 0

@doc Markdown.doc"""
    isequal_abs_imag(a::qqbar, b::qqbar)

Compares the absolute values of the imaginary parts of `a` and `b`.
"""
isequal_abs_imag(a::qqbar, b::qqbar) = cmpabs_imag(a, b) == 0


@doc Markdown.doc"""
    isless_real(a::qqbar, b::qqbar)

Compares the real parts of `a` and `b`.
"""
isless_real(a::qqbar, b::qqbar) = cmp_real(a, b) < 0

@doc Markdown.doc"""
    isless_imag(a::qqbar, b::qqbar)

Compares the imaginary parts of `a` and `b`.
"""
isless_imag(a::qqbar, b::qqbar) = cmp_imag(a, b) < 0

@doc Markdown.doc"""
    isless_abs(a::qqbar, b::qqbar)

Compares the absolute values of `a` and `b`.
"""
isless_abs(a::qqbar, b::qqbar) = cmpabs(a, b) < 0


@doc Markdown.doc"""
    isless_abs_real(a::qqbar, b::qqbar)

Compares the absolute values of the real parts of `a` and `b`.
"""
isless_abs_real(a::qqbar, b::qqbar) = cmpabs_real(a, b) < 0

@doc Markdown.doc"""
    isless_abs_imag(a::qqbar, b::qqbar)

Compares the absolute values of the imaginary parts of `a` and `b`.
"""
isless_abs_imag(a::qqbar, b::qqbar) = cmpabs_imag(a, b) < 0

@doc Markdown.doc"""
    isless_root_order(a::qqbar, b::qqbar)

Compares the `a` and `b` in root sort order.
"""
isless_root_order(a::qqbar, b::qqbar) = cmp_root_order(a, b) < 0

# todo: wrap qqbar_equal_fmpq_poly_val

###############################################################################
#
#   Complex parts
#
###############################################################################

@doc Markdown.doc"""
    real(a::qqbar)

Return the real part of `a`.
"""
function real(a::qqbar)
   z = qqbar()
   ccall((:qqbar_re, libcalcium), Nothing, (Ref{qqbar}, Ref{qqbar}), z, a)
   return z
end

@doc Markdown.doc"""
    imag(a::qqbar)

Return the imaginary part of `a`.
"""
function imag(a::qqbar)
   z = qqbar()
   ccall((:qqbar_im, libcalcium), Nothing, (Ref{qqbar}, Ref{qqbar}), z, a)
   return z
end

@doc Markdown.doc"""
    abs(a::qqbar)

Return the absolute value of `a`.
"""
function abs(a::qqbar)
   z = qqbar()
   ccall((:qqbar_abs, libcalcium), Nothing, (Ref{qqbar}, Ref{qqbar}), z, a)
   return z
end

@doc Markdown.doc"""
    conj(a::qqbar)

Return the complex conjugate of `a`.
"""
function conj(a::qqbar)
   z = qqbar()
   ccall((:qqbar_conj, libcalcium), Nothing, (Ref{qqbar}, Ref{qqbar}), z, a)
   return z
end

@doc Markdown.doc"""
    abs2(a::qqbar)

Return the squared absolute value of `a`.
"""
function abs2(a::qqbar)
   z = qqbar()
   ccall((:qqbar_abs2, libcalcium), Nothing, (Ref{qqbar}, Ref{qqbar}), z, a)
   return z
end

@doc Markdown.doc"""
    sign(a::qqbar)

Return the complex sign of `a`, defined as zero if `a` is zero
and as $a / |a|$ otherwise.
"""
function sign(a::qqbar)
   z = qqbar()
   ccall((:qqbar_sgn, libcalcium), Nothing, (Ref{qqbar}, Ref{qqbar}), z, a)
   return z
end

@doc Markdown.doc"""
    csgn(a::qqbar)

Return the extension of the real sign function taking the value 1
strictly in the right half plane, -1 strictly in the left half plane,
and the sign of the imaginary part when on the imaginary axis.
Equivalently, $\operatorname{csgn}(x) = x / \sqrt{x^2}$ except that the value is 0
at zero. The value is returned as a Julia integer.
"""
function csgn(a::qqbar)
   return qqbar(Int(ccall((:qqbar_csgn, libcalcium), Cint, (Ref{qqbar}, ), a)))
end

@doc Markdown.doc"""
    sign_real(a::qqbar)

Return the sign of the real part of `a` as a Julia integer.
"""
function sign_real(a::qqbar)
   return qqbar(Int(ccall((:qqbar_sgn_re, libcalcium),
        Cint, (Ref{qqbar}, ), a)))
end

@doc Markdown.doc"""
    sign_imag(a::qqbar)

Return the sign of the imaginary part of `a` as a Julia integer.
"""
function sign_imag(a::qqbar)
   return qqbar(Int(ccall((:qqbar_sgn_im, libcalcium),
        Cint, (Ref{qqbar}, ), a)))
end

@doc Markdown.doc"""
    floor(a::qqbar)

Return the floor function of `a` as an algebraic number. Use `fmpz(floor(a))`
to construct a Nemo integer instead.
"""
function floor(a::qqbar)
   z = fmpz()
   ccall((:qqbar_floor, libcalcium), Nothing, (Ref{fmpz}, Ref{qqbar}, ), z, a)
   return qqbar(z)
end

@doc Markdown.doc"""
    ceil(a::qqbar)

Return the ceiling function of `b` as an algebraic number. Use `fmpz(ceil(a))`
to construct a Nemo integer instead.
"""
function ceil(a::qqbar)
   z = fmpz()
   ccall((:qqbar_ceil, libcalcium), Nothing, (Ref{fmpz}, Ref{qqbar}, ), z, a)
   return qqbar(z)
end


###############################################################################
#
#   Roots
#
###############################################################################

@doc Markdown.doc"""
    sqrt(a::qqbar)

Return the principal square root of `a`.
"""
function sqrt(a::qqbar)
   z = qqbar()
   ccall((:qqbar_sqrt, libcalcium),
        Nothing, (Ref{qqbar}, Ref{qqbar}), z, a)
   return z
end

@doc Markdown.doc"""
    root(a::qqbar, n::Int)

Return the principal `n`-th root of `a`. Requires positive `n`.
"""
function root(a::qqbar, n::Int)
   n <= 0 && throw(DomainError(n))
   z = qqbar()
   ccall((:qqbar_root_ui, libcalcium),
        Nothing, (Ref{qqbar}, Ref{qqbar}, UInt), z, a, n)
   return z
end

function qqbar_vec(n::Int)
   return ccall((:_qqbar_vec_init, libcalcium), Ptr{qqbar_struct}, (Int,), n)
end

function array(R::CalciumQQBarField, v::Ptr{qqbar_struct}, n::Int)
   r = Vector{qqbar}(undef, n)
   for i=1:n
       r[i] = R()
       ccall((:qqbar_set, libcalcium), Nothing, (Ref{qqbar}, Ptr{qqbar_struct}),
           r[i], v + (i-1)*sizeof(qqbar_struct))
   end
   return r
end

function qqbar_vec_clear(v::Ptr{qqbar_struct}, n::Int)
   ccall((:_qqbar_vec_clear, libcalcium),
        Nothing, (Ptr{qqbar_struct}, Int), v, n)
end

@doc Markdown.doc"""
    roots(f::fmpz_poly, R::CalciumQQBarField)

Return all the roots of the polynomial `f` in the field of algebraic
numbers `R`. The output array is sorted in the default sort order for
algebraic numbers. Roots of multiplicity higher than one are repeated
according to their multiplicity.
"""
function roots(f::fmpz_poly, R::CalciumQQBarField)
   deg = degree(f)
   if deg <= 0
      return Array{qqbar}(undef, 0)
   end
   roots = qqbar_vec(deg)
   ccall((:qqbar_roots_fmpz_poly, libcalcium),
        Nothing, (Ptr{qqbar_struct}, Ref{fmpz_poly}, Int), roots, f, 0)
   res = array(R, roots, deg)
   qqbar_vec_clear(roots, deg)
   return res
end

@doc Markdown.doc"""
    roots(f::fmpq_poly, R::CalciumQQBarField)

Return all the roots of the polynomial `f` in the field of algebraic
numbers `R`. The output array is sorted in the default sort order for
algebraic numbers. Roots of multiplicity higher than one are repeated
according to their multiplicity.
"""
function roots(f::fmpq_poly, R::CalciumQQBarField)
   deg = degree(f)
   if deg <= 0
      return Array{qqbar}(undef, 0)
   end
   roots = qqbar_vec(deg)
   ccall((:qqbar_roots_fmpq_poly, libcalcium),
        Nothing, (Ptr{qqbar_struct}, Ref{fmpq_poly}, Int), roots, f, 0)
   res = array(R, roots, deg)
   qqbar_vec_clear(roots, deg)
   return res
end

@doc Markdown.doc"""
    conjugates(a::qqbar)

Return all the roots of the polynomial `f` in the field of algebraic
numbers `R`. The output array is sorted in the default sort order for
algebraic numbers.
"""
function conjugates(a::qqbar)
   deg = degree(a)
   if deg == 1
      return [a]
   end
   conjugates = qqbar_vec(deg)
   ccall((:qqbar_conjugates, libcalcium),
        Nothing, (Ptr{qqbar_struct}, Ref{qqbar}), conjugates, a)
   res = array(parent(a), conjugates, deg)
   qqbar_vec_clear(conjugates, deg)
   return res
end

@doc Markdown.doc"""
    eigenvalues(A::fmpz_mat, R::CalciumQQBarField)

Return all the eigenvalues of the matrix `A` in the field of algebraic
numbers `R`. The output array is sorted in the default sort order for
algebraic numbers. Eigenvalues of multiplicity higher than one are repeated
according to their multiplicity.
"""
function eigenvalues(A::fmpz_mat, R::CalciumQQBarField)
   n = nrows(A)
   !issquare(A) && throw(DomainError(A, "a square matrix is required"))
   if n == 0
      return Array{qqbar}(undef, 0)
   end
   roots = qqbar_vec(n)
   ccall((:qqbar_eigenvalues_fmpz_mat, libcalcium),
        Nothing, (Ptr{qqbar_struct}, Ref{fmpz_mat}, Int), roots, A, 0)
   res = array(R, roots, n)
   qqbar_vec_clear(roots, n)
   return res
end

@doc Markdown.doc"""
    eigenvalues(A::fmpq_mat, R::CalciumQQBarField)

Return all the eigenvalues of the matrix `A` in the field of algebraic
numbers `R`. The output array is sorted in the default sort order for
algebraic numbers. Eigenvalues of multiplicity higher than one are repeated
according to their multiplicity.
"""
function eigenvalues(A::fmpq_mat, R::CalciumQQBarField)
   n = nrows(A)
   !issquare(A) && throw(DomainError(A, "a square matrix is required"))
   if n == 0
      return Array{qqbar}(undef, 0)
   end
   roots = qqbar_vec(n)
   ccall((:qqbar_eigenvalues_fmpq_mat, libcalcium),
        Nothing, (Ptr{qqbar_struct}, Ref{fmpq_mat}, Int), roots, A, 0)
   res = array(R, roots, n)
   qqbar_vec_clear(roots, n)
   return res
end

###############################################################################
#
#   Roots of unity and trigonometric functions
#
###############################################################################

@doc Markdown.doc"""
    root_of_unity(C::CalciumQQBarField, n::Int)

Return the root of unity $e^{2 \pi i / n}$ as an element of the field
of algebraic numbers `C`.
"""
function root_of_unity(C::CalciumQQBarField, n::Int)
   n <= 0 && throw(DomainError(n))
   z = qqbar()
   ccall((:qqbar_root_of_unity, libcalcium),
        Nothing, (Ref{qqbar}, Int, UInt), z, 1, n)
   return z
end

@doc Markdown.doc"""
    root_of_unity(C::CalciumQQBarField, n::Int, k::Int)

Return the root of unity $e^{2 \pi i k / n}$ as an element of the field
of algebraic numbers `C`.
"""
function root_of_unity(C::CalciumQQBarField, n::Int, k::Int)
   n <= 0 && throw(DomainError(n))
   z = qqbar()
   ccall((:qqbar_root_of_unity, libcalcium),
        Nothing, (Ref{qqbar}, Int, UInt), z, k, n)
   return z
end

@doc Markdown.doc"""
    isroot_of_unity(a::qqbar)

Return whether the given algebraic number is a root of unity.
"""
function isroot_of_unity(a::qqbar)
   return Bool(ccall((:qqbar_is_root_of_unity, libcalcium),
        Cint, (Ptr{Int}, Ptr{Int}, Ref{qqbar}), C_NULL, C_NULL, a))
end

@doc Markdown.doc"""
    root_of_unity_as_args(a::qqbar)

Return a pair of integers `(q, p)` such that the given `a` equals
$e^{2 \pi i p / q}$. The denominator `q` will be minimal, with
$0 \le p < q$. Throws if `a` is not a root of unity.
"""
function root_of_unity_as_args(a::qqbar)
   p = Vector{Int}(undef, 1)
   q = Vector{Int}(undef, 1)
   if !Bool(ccall((:qqbar_is_root_of_unity, libcalcium),
        Cint, (Ptr{Int}, Ptr{Int}, Ref{qqbar}), p, q, a))
      throw(DomainError(a, "value is not a root of unity"))
   end
   return (q[1], p[1])
end

@doc Markdown.doc"""
    exp_pi_i(a::qqbar)

Return $e^{\pi i a}$ as an algebraic number.
Throws if this value is transcendental.
"""
function exp_pi_i(a::qqbar)
   r = fmpq(a)
   p = Int(numerator(r))
   q = Int(denominator(r))
   z = qqbar()
   ccall((:qqbar_exp_pi_i, libcalcium),
        Nothing, (Ref{qqbar}, Int, Int), z, p, q)
   return z
end

@doc Markdown.doc"""
    sinpi(a::qqbar)

Return $\sin(\pi a)$ as an algebraic number.
Throws if this value is transcendental.
"""
function sinpi(a::qqbar)
   r = fmpq(a)
   p = Int(numerator(r))
   q = Int(denominator(r))
   z = qqbar()
   ccall((:qqbar_sin_pi, libcalcium), Nothing, (Ref{qqbar}, Int, Int), z, p, q)
   return z
end

@doc Markdown.doc"""
    cospi(a::qqbar)

Return $\cos(\pi a)$ as an algebraic number.
Throws if this value is transcendental.
"""
function cospi(a::qqbar)
   r = fmpq(a)
   p = Int(numerator(r))
   q = Int(denominator(r))
   z = qqbar()
   ccall((:qqbar_cos_pi, libcalcium), Nothing, (Ref{qqbar}, Int, Int), z, p, q)
   return z
end

@doc Markdown.doc"""
    tanpi(a::qqbar)

Return $\tan(\pi a)$ as an algebraic number.
Throws if this value is transcendental or undefined.
"""
function tanpi(a::qqbar)
   r = fmpq(a)
   p = Int(numerator(r))
   q = Int(denominator(r))
   z = qqbar()
   if !Bool(ccall((:qqbar_tan_pi, libcalcium),
        Cint, (Ref{qqbar}, Int, Int), z, p, q))
      throw(DomainError(a, "function value is not algebraic"))
   end
   return z
end

@doc Markdown.doc"""
    atanpi(a::qqbar)

Return $\operatorname{atan}(a) / \pi$ as an algebraic number.
Throws if this value is transcendental or undefined.
"""
function atanpi(a::qqbar)
   p = Vector{Int}(undef, 1)
   q = Vector{Int}(undef, 1)
   if !Bool(ccall((:qqbar_atan_pi, libcalcium),
        Cint, (Ptr{Int}, Ptr{Int}, Ref{qqbar}), p, q, a))
      throw(DomainError(a, "function value is not algebraic"))
   end
   return qqbar(p[1]) // q[1]
end

@doc Markdown.doc"""
    asinpi(a::qqbar)

Return $\operatorname{asin}(a) / \pi$ as an algebraic number.
Throws if this value is transcendental.
"""
function asinpi(a::qqbar)
   p = Vector{Int}(undef, 1)
   q = Vector{Int}(undef, 1)
   if !Bool(ccall((:qqbar_asin_pi, libcalcium),
        Cint, (Ptr{Int}, Ptr{Int}, Ref{qqbar}), p, q, a))
      throw(DomainError(a, "function value is not algebraic"))
   end
   return qqbar(p[1]) // q[1]
end

@doc Markdown.doc"""
    acospi(a::qqbar)

Return $\operatorname{acos}(a) / \pi$ as an algebraic number.
Throws if this value is transcendental.
"""
function acospi(a::qqbar)
   p = Vector{Int}(undef, 1)
   q = Vector{Int}(undef, 1)
   if !Bool(ccall((:qqbar_acos_pi, libcalcium),
        Cint, (Ptr{Int}, Ptr{Int}, Ref{qqbar}), p, q, a))
      throw(DomainError(a, "function value is not algebraic"))
   end
   return qqbar(p[1]) // q[1]
end

@doc Markdown.doc"""
    log_pi_i(a::qqbar)

Return $\log(a) / (\pi i)$ as an algebraic number.
Throws if this value is transcendental or undefined.
"""
function log_pi_i(a::qqbar)
   p = Vector{Int}(undef, 1)
   q = Vector{Int}(undef, 1)
   if !Bool(ccall((:qqbar_log_pi_i, libcalcium),
        Cint, (Ptr{Int}, Ptr{Int}, Ref{qqbar}), p, q, a))
      throw(DomainError(a, "function value is not algebraic"))
   end
   return qqbar(p[1]) // q[1]
end



###############################################################################
#
#   Guessing
#
###############################################################################

@doc Markdown.doc"""
    guess(R::CalciumQQBarField, x::acb, maxdeg::Int, maxbits::Int=0)

Try to reconstruct an algebraic number from a given numerical enclosure `x`.
The algorithm looks for candidates up to degree `maxdeg` and with
coefficients up to size `maxbits` (which defaults to the precision of `x`
if not given). Throws if no suitable algebraic number can be found.

Guessing typically requires high precision to succeed, and it does not make
much sense to call this function with input precision smaller than
$O(maxdeg \cdot maxbits)$.
If this function succeeds, then the output is guaranteed to be contained in
the enclosure `x`, but failure does not prove that such an algebric
number with the specified parameters does not exist.

This function does a single iteration with the target parameters. For best
performance, one should invoke this function repeatedly with successively
larger parameters when the size of the intended solution is unknown or
may be much smaller than a worst-case bound.
"""
function guess(R::CalciumQQBarField, x::acb, maxdeg::Int, maxbits::Int=0)
   prec = precision(parent(x))
   if maxbits <= 0
      maxbits = prec
   end
   res = qqbar()
   found = Bool(ccall((:qqbar_guess, libcalcium),
        Cint, (Ref{qqbar}, Ref{acb}, Int, Int, Int, Int),
            res, x, maxdeg, maxbits, 0, prec))
   if !found
      error("No suitable algebraic number found")
   end
   return res
end

@doc Markdown.doc"""
    guess(R::CalciumQQBarField, x::acb, maxdeg::Int, maxbits::Int=0)

Try to reconstruct an algebraic number from a given numerical enclosure `x`.
The algorithm looks for candidates up to degree `maxdeg` and with
coefficients up to size `maxbits` (which defaults to the precision of `x`
if not given). Throws if no suitable algebraic number can be found.

Guessing typically requires high precision to succeed, and it does not make
much sense to call this function with input precision smaller than
$O(maxdeg \cdot maxbits)$.
If this function succeeds, then the output is guaranteed to be contained in
the enclosure `x`, but failure does not prove that such an algebric
number with the specified parameters does not exist.

This function does a single iteration with the target parameters. For best
performance, one should invoke this function repeatedly with successively
larger parameters when the size of the intended solution is unknown or
may be much smaller than a worst-case bound.
"""
function guess(R::CalciumQQBarField, x::arb, maxdeg::Int, maxbits::Int=0)
   CC = AcbField(precision(parent(x)))
   return guess(R, CC(x), maxdeg, maxbits)
end

###############################################################################
#
#   Conversions
#
###############################################################################

@doc Markdown.doc"""
    (R::ArbField)(a::qqbar)

Convert `a` to a real ball with the precision of the parent field `R`.
Throws if `a` is not a real number.
"""
function (R::ArbField)(a::qqbar)
   prec = precision(R)
   z = R()
   ccall((:qqbar_get_arb, libcalcium),
        Nothing, (Ref{arb}, Ref{qqbar}, Int), z, a, prec)
   !isfinite(z) && throw(DomainError(a, "nonreal algebraic number"))
   return z
end

@doc Markdown.doc"""
    (R::ArbField)(a::qqbar)

Convert `a` to a complex ball with the precision of the parent field `R`.
Throws if `a` is not a real number.
"""
function (R::AcbField)(a::qqbar)
   prec = precision(R)
   z = R()
   ccall((:qqbar_get_acb, libcalcium),
        Nothing, (Ref{acb}, Ref{qqbar}, Int), z, a, prec)
   return z
end

# todo: provide qqbar_get_fmpq, qqbar_get_fmpz in C
@doc Markdown.doc"""
    fmpq(a::qqbar)

Convert `a` to a rational number of type `fmpq`.
Throws if `a` is not a rational number.
"""
function fmpq(a::qqbar)
   !isrational(a) && throw(DomainError(a, "nonrational algebraic number"))
   p = fmpz()
   q = fmpz()
   ccall((:fmpz_poly_get_coeff_fmpz, libflint),
        Nothing, (Ref{fmpz}, Ref{qqbar}, Int), p, a, 0)
   ccall((:fmpz_poly_get_coeff_fmpz, libflint),
        Nothing, (Ref{fmpz}, Ref{qqbar}, Int), q, a, 1)
   ccall((:fmpz_neg, libflint), Nothing, (Ref{fmpz}, Ref{fmpz}), p, p)
   return p // q
end

@doc Markdown.doc"""
    fmpz(a::qqbar)

Convert `a` to an integer of type `fmpz`.
Throws if `a` is not an integer.
"""
function fmpz(a::qqbar)
   !isinteger(a) && throw(DomainError(a, "noninteger algebraic number"))
   z = fmpz()
   ccall((:fmpz_poly_get_coeff_fmpz, libflint),
        Nothing, (Ref{fmpz}, Ref{qqbar}, Int), z, a, 0)
   ccall((:fmpz_neg, libflint), Nothing, (Ref{fmpz}, Ref{fmpz}), z, z)
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::qqbar)
   ccall((:qqbar_zero, libcalcium), Nothing, (Ref{qqbar},), z)
   return z
end

function mul!(z::qqbar, x::qqbar, y::qqbar)
   ccall((:qqbar_mul, libcalcium), Nothing,
                (Ref{qqbar}, Ref{qqbar}, Ref{qqbar}), z, x, y)
   return z
end

function addeq!(z::qqbar, x::qqbar)
   ccall((:qqbar_add, libcalcium), Nothing,
                (Ref{qqbar}, Ref{qqbar}, Ref{qqbar}), z, z, x)
   return z
end

function add!(z::qqbar, x::qqbar, y::qqbar)
   ccall((:qqbar_add, libcalcium), Nothing,
                (Ref{qqbar}, Ref{qqbar}, Ref{qqbar}), z, x, y)
   return z
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

(a::CalciumQQBarField)() = qqbar()

(a::CalciumQQBarField)(b::Int) = qqbar(b)

(a::CalciumQQBarField)(b::Complex{Int}) = qqbar(b)

(a::CalciumQQBarField)(b::fmpz) = qqbar(b)

(a::CalciumQQBarField)(b::fmpq) = qqbar(b)

(a::CalciumQQBarField)(b::qqbar) = b

