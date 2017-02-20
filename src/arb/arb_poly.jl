###############################################################################
#
#   arb_poly.jl : Polynomials over arb
#
###############################################################################

export ArbPolyRing, arb_poly, derivative, integral, evaluate, evaluate2,
       compose, from_roots, evaluate_iter, evaluate_fast, evaluate,
       interpolate, interpolate_newton, interpolate_barycentric,
       interpolate_fast

###############################################################################
#
#   Basic manipulation
#
###############################################################################    
  
length(x::arb_poly) = ccall((:arb_poly_length, :libarb), Int, 
                                   (Ptr{arb_poly},), &x)

set_length!(x::arb_poly, n::Int) = ccall((:_arb_poly_set_length, :libarb), Void,
                                   (Ptr{arb_poly}, Int), &x, n)

degree(x::arb_poly) = length(x) - 1

function coeff(a::arb_poly, n::Int)
  n < 0 && throw(DomainError())
  t = parent(a).base_ring()
  ccall((:arb_poly_get_coeff_arb, :libarb), Void,
              (Ptr{arb}, Ptr{arb_poly}, Int), &t, &a, n)
  return t
end

zero(a::ArbPolyRing) = a(0)

one(a::ArbPolyRing) = a(1)

function gen(a::ArbPolyRing)
   z = arb_poly()
   ccall((:arb_poly_set_coeff_si, :libarb), Void,
        (Ptr{arb_poly}, Int, Int), &z, 1, 1)
   z.parent = a
   return z
end

# todo: write a C function for this
function isgen(a::arb_poly)
   return isequal(a, gen(parent(a)))
end

#function iszero(a::arb_poly)
#   return length(a) == 0
#end

#function isone(a::arb_poly)
#   return strongequal(a, one(parent(a)))
#end

function deepcopy_internal(a::arb_poly, dict::ObjectIdDict)
   z = arb_poly(a)
   z.parent = parent(a)
   return z
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, x::ArbPolyRing)
  print(io, "Univariate Polynomial Ring in ")
  print(io, string(var(x)))
  print(io, " over ")
  show(io, x.base_ring)
end

function show(io::IO, f::arb_poly)
  if length(f) == 0
    print(io, "0")
  else
    print(io, "[ ")
    for i in 0:degree(f)-1
      print(io, coeff(f,i))
      print(io, ", ")
    end
    print(io, coeff(f,degree(f)))
    print(io, " ]")
  end
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function isequal(x::arb_poly, y::arb_poly)
   return ccall((:arb_poly_equal, :libarb), Bool, 
                                      (Ptr{arb_poly}, Ptr{arb_poly}), &x, &y)
end

doc"""
    overlaps(x::arb_poly, y::arb_poly)
> Return `true` if the coefficient balls of $x$ overlap the coefficient balls
> of $y$, otherwise return `false`.
"""
function overlaps(x::arb_poly, y::arb_poly)
   return ccall((:arb_poly_overlaps, :libarb), Bool, 
                                      (Ptr{arb_poly}, Ptr{arb_poly}), &x, &y)
end

doc"""
    contains(x::arb_poly, y::arb_poly)
> Return `true` if the coefficient balls of $x$ contain the corresponding
> coefficient balls of $y$, otherwise return `false`.
"""
function contains(x::arb_poly, y::arb_poly)
   return ccall((:arb_poly_contains, :libarb), Bool, 
                                      (Ptr{arb_poly}, Ptr{arb_poly}), &x, &y)
end

doc"""
    contains(x::arb_poly, y::fmpz_poly)
> Return `true` if the coefficient balls of $x$ contain the corresponding
> exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::arb_poly, y::fmpz_poly)
   return ccall((:arb_poly_contains_fmpz_poly, :libarb), Bool, 
                                      (Ptr{arb_poly}, Ptr{fmpz_poly}), &x, &y)
end

doc"""
    contains(x::arb_poly, y::fmpq_poly)
> Return `true` if the coefficient balls of $x$ contain the corresponding
> exact coefficients of $y$, otherwise return `false`.
"""
function contains(x::arb_poly, y::fmpq_poly)
   return ccall((:arb_poly_contains_fmpq_poly, :libarb), Bool, 
                                      (Ptr{arb_poly}, Ptr{fmpq_poly}), &x, &y)
end

function ==(x::arb_poly, y::arb_poly)
    if length(x) != length(y)
        return false
    end
    for i = 0:degree(x)
        if !(coeff(x, i) == coeff(y, i))
            return false
        end
    end
    return true
end

function !=(x::arb_poly, y::arb_poly)
    for i = 0:max(degree(x), degree(y))
        if coeff(x, i) != coeff(y, i)
            return true
        end
    end
    return false
end

doc"""
    unique_integer(x::arb_poly)
> Return a tuple `(t, z)` where $t$ is `true` if there is a unique integer
> contained in each of the coefficients of $x$, otherwise sets $t$ to `false`.
> In the former case, $z$ is set to the integer polynomial.
"""
function unique_integer(x::arb_poly)
  z = FmpzPolyRing(var(parent(x)))()
  unique = ccall((:arb_poly_get_unique_fmpz_poly, :libarb), Int,
    (Ptr{fmpz_poly}, Ptr{arb_poly}), &z, &x)
  return (unique != 0, z)
end

###############################################################################
#
#   Shifting
#
###############################################################################

function shift_left(x::arb_poly, len::Int)
   len < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:arb_poly_shift_left, :libarb), Void,
      (Ptr{arb_poly}, Ptr{arb_poly}, Int), &z, &x, len)
   return z
end

function shift_right(x::arb_poly, len::Int)
   len < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:arb_poly_shift_right, :libarb), Void,
       (Ptr{arb_poly}, Ptr{arb_poly}, Int), &z, &x, len)
   return z
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::arb_poly)
  z = parent(x)()
  ccall((:arb_poly_neg, :libarb), Void, (Ptr{arb_poly}, Ptr{arb_poly}), &z, &x)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::arb_poly, y::arb_poly)
  z = parent(x)()
  ccall((:arb_poly_add, :libarb), Void,
              (Ptr{arb_poly}, Ptr{arb_poly}, Ptr{arb_poly}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

function *(x::arb_poly, y::arb_poly)
  z = parent(x)()
  ccall((:arb_poly_mul, :libarb), Void,
              (Ptr{arb_poly}, Ptr{arb_poly}, Ptr{arb_poly}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

function -(x::arb_poly, y::arb_poly)
  z = parent(x)()
  ccall((:arb_poly_sub, :libarb), Void,
              (Ptr{arb_poly}, Ptr{arb_poly}, Ptr{arb_poly}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

function ^(x::arb_poly, y::Int)
  y < 0 && throw(DomainError())
  z = parent(x)()
  ccall((:arb_poly_pow_ui, :libarb), Void,
              (Ptr{arb_poly}, Ptr{arb_poly}, UInt, Int),
              &z, &x, y, prec(parent(x)))
  return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

+(x::arb_poly, y::Int) = x + parent(x)(y)

+(x::arb_poly, y::Integer) = x + parent(x)(y)

+(x::arb_poly, y::fmpz) = x + parent(x)(y)

+(x::arb_poly, y::fmpq) = x + parent(x)(y)

+(x::arb_poly, y::Float64) = x + parent(x)(y)

+(x::arb_poly, y::arb) = x + parent(x)(y)

+(x::arb_poly, y::fmpz_poly) = x + parent(x)(y)

+(x::arb_poly, y::fmpq_poly) = x + parent(x)(y)

-(x::arb_poly, y::Int) = x - parent(x)(y)

-(x::arb_poly, y::Integer) = x - parent(x)(y)

-(x::arb_poly, y::fmpz) = x - parent(x)(y)

-(x::arb_poly, y::fmpq) = x - parent(x)(y)

-(x::arb_poly, y::Float64) = x - parent(x)(y)

-(x::arb_poly, y::arb) = x - parent(x)(y)

-(x::arb_poly, y::fmpz_poly) = x - parent(x)(y)

-(x::arb_poly, y::fmpq_poly) = x - parent(x)(y)

*(x::arb_poly, y::Int) = x*parent(x)(y)

*(x::arb_poly, y::Integer) = x*parent(x)(y)

*(x::arb_poly, y::fmpz) = x*parent(x)(y)

*(x::arb_poly, y::fmpq) = x*parent(x)(y)

*(x::arb_poly, y::Float64) = x*parent(x)(y)

*(x::arb_poly, y::arb) = x*parent(x)(y)

*(x::arb_poly, y::fmpz_poly) = x*parent(x)(y)

*(x::arb_poly, y::fmpq_poly) = x*parent(x)(y)

+(x::Int, y::arb_poly) = y + x

+(x::Integer, y::arb_poly) = y + x

+(x::fmpz, y::arb_poly) = y + x

+(x::fmpq, y::arb_poly) = y + x

+(x::Float64, y::arb_poly) = y + x

+(x::arb, y::arb_poly) = y + x

+(x::fmpz_poly, y::arb_poly) = y + x

+(x::fmpq_poly, y::arb_poly) = y + x

-(x::Int, y::arb_poly) = parent(y)(x) - y

-(x::Integer, y::arb_poly) = parent(y)(x) - y

-(x::fmpz, y::arb_poly) = parent(y)(x) - y

-(x::fmpq, y::arb_poly) = parent(y)(x) - y

-(x::Float64, y::arb_poly) = parent(y)(x) - y

-(x::arb, y::arb_poly) = parent(y)(x) - y

-(x::fmpz_poly, y::arb_poly) = parent(y)(x) - y

-(x::fmpq_poly, y::arb_poly) = parent(y)(x) - y

*(x::Int, y::arb_poly) = y*x

*(x::Integer, y::arb_poly) = y*x

*(x::fmpz, y::arb_poly) = y*x

*(x::fmpq, y::arb_poly) = y*x

*(x::Float64, y::arb_poly) = y*x

*(x::arb, y::arb_poly) = y*x

*(x::fmpz_poly, y::arb_poly) = y*x

*(x::fmpq_poly, y::arb_poly) = y*x

###############################################################################
#
#   Scalar division
#
###############################################################################

divexact(x::arb_poly, y::Int) = x*inv(base_ring(parent(x))(y))

divexact(x::arb_poly, y::Integer) = x*inv(base_ring(parent(x))(y))

divexact(x::arb_poly, y::fmpz) = x*inv(base_ring(parent(x))(y))

divexact(x::arb_poly, y::fmpq) = x*inv(base_ring(parent(x))(y))

divexact(x::arb_poly, y::Float64) = x*inv(base_ring(parent(x))(y))

divexact(x::arb_poly, y::arb) = x*inv(base_ring(parent(x))(y))

//(x::arb_poly, y::Int) = divexact(x, y)

//(x::arb_poly, y::Integer) = divexact(x, y)

//(x::arb_poly, y::fmpz) = divexact(x, y)

//(x::arb_poly, y::fmpq) = divexact(x, y)

//(x::arb_poly, y::Float64) = divexact(x, y)

//(x::arb_poly, y::arb) = divexact(x, y)

###############################################################################
#
#   Euclidean division
#
###############################################################################

function divrem(x::arb_poly, y::arb_poly)
   iszero(y) && throw(DivideError())
   q = parent(x)()
   r = parent(x)()
   if (ccall((:arb_poly_divrem, :libarb), Int, 
         (Ptr{arb_poly}, Ptr{arb_poly}, Ptr{arb_poly}, Ptr{arb_poly}, Int), 
               &q, &r, &x, &y, prec(parent(x))) == 1)
      return (q, r)
   else
      throw(DivideError())
   end
end

function mod(x::arb_poly, y::arb_poly)
   return divrem(x, y)[2]
end

function divexact(x::arb_poly, y::arb_poly)
   return divrem(x, y)[1]
end

###############################################################################
#
#   Truncation
#
###############################################################################

function truncate(a::arb_poly, n::Int)
   n < 0 && throw(DomainError())
   if length(a) <= n
      return a
   end
   # todo: implement set_trunc in arb
   z = deepcopy(a)
   ccall((:arb_poly_truncate, :libarb), Void,
                (Ptr{arb_poly}, Int), &z, n)
   return z
end

function mullow(x::arb_poly, y::arb_poly, n::Int)
   n < 0 && throw(DomainError())
   z = parent(x)()
   ccall((:arb_poly_mullow, :libarb), Void,
         (Ptr{arb_poly}, Ptr{arb_poly}, Ptr{arb_poly}, Int, Int),
            &z, &x, &y, n, prec(parent(x)))
   return z
end

###############################################################################
#
#   Reversal
#
###############################################################################

#function reverse(x::arb_poly, len::Int)
#   len < 0 && throw(DomainError())
#   z = parent(x)()
#   ccall((:arb_poly_reverse, :libarb), Void,
#                (Ptr{arb_poly}, Ptr{arb_poly}, Int), &z, &x, len)
#   return z
#end

###############################################################################
#
#   Evaluation
#
###############################################################################

function evaluate(x::arb_poly, y::arb)
   z = parent(y)()
   ccall((:arb_poly_evaluate, :libarb), Void, 
                (Ptr{arb}, Ptr{arb_poly}, Ptr{arb}, Int),
                &z, &x, &y, prec(parent(y)))
   return z
end

doc"""
    evaluate2(x::arb_poly, y::arb)
> Return a tuple $p, q$ consisting of the polynomial $x$ evaluated at $y$ and
> its derivative evaluated at $y$.
"""
function evaluate2(x::arb_poly, y::arb)
   z = parent(y)()
   w = parent(y)()
   ccall((:arb_poly_evaluate2, :libarb), Void, 
                (Ptr{arb}, Ptr{arb}, Ptr{arb_poly}, Ptr{arb}, Int),
                &z, &w, &x, &y, prec(parent(y)))
   return z, w
end

function evaluate(x::arb_poly, y::acb)
   z = parent(y)()
   ccall((:arb_poly_evaluate_acb, :libarb), Void, 
                (Ptr{acb}, Ptr{arb_poly}, Ptr{acb}, Int),
                &z, &x, &y, prec(parent(y)))
   return z
end

doc"""
    evaluate2(x::arb_poly, y::acb)
> Return a tuple $p, q$ consisting of the polynomial $x$ evaluated at $y$ and
> its derivative evaluated at $y$.
"""
function evaluate2(x::arb_poly, y::acb)
   z = parent(y)()
   w = parent(y)()
   ccall((:arb_poly_evaluate2_acb, :libarb), Void, 
                (Ptr{acb}, Ptr{acb}, Ptr{arb_poly}, Ptr{acb}, Int),
                &z, &w, &x, &y, prec(parent(y)))
   return z, w
end

function evaluate(x::arb_poly, y::Union{Int,Float64,fmpq})
    return evaluate(x, base_ring(parent(x))(y))
end

function evaluate(x::arb_poly, y::fmpz)
    return evaluate(x, base_ring(parent(x))(y))
end

doc"""
    evaluate2(x::arb_poly, y::Integer)
> Return a tuple $p, q$ consisting of the polynomial $x$ evaluated at $y$ and
> its derivative evaluated at $y$.
"""
function evaluate2(x::arb_poly, y::Integer)
    return evaluate2(x, base_ring(parent(x))(y))
end

doc"""
    evaluate2(x::arb_poly, y::Float64)
> Return a tuple $p, q$ consisting of the polynomial $x$ evaluated at $y$ and
> its derivative evaluated at $y$.
"""
function evaluate2(x::arb_poly, y::Float64)
    return evaluate2(x, base_ring(parent(x))(y))
end

doc"""
    evaluate2(x::arb_poly, y::fmpz)
> Return a tuple $p, q$ consisting of the polynomial $x$ evaluated at $y$ and
> its derivative evaluated at $y$.
"""
function evaluate2(x::arb_poly, y::fmpz)
    return evaluate2(x, base_ring(parent(x))(y))
end

doc"""
    evaluate2(x::arb_poly, y::fmpq)
> Return a tuple $p, q$ consisting of the polynomial $x$ evaluated at $y$ and
> its derivative evaluated at $y$.
"""
function evaluate2(x::arb_poly, y::fmpq)
    return evaluate2(x, base_ring(parent(x))(y))
end

###############################################################################
#
#   Composition
#
###############################################################################

function compose(x::arb_poly, y::arb_poly)
   z = parent(x)()
   ccall((:arb_poly_compose, :libarb), Void, 
                (Ptr{arb_poly}, Ptr{arb_poly}, Ptr{arb_poly}, Int),
                &z, &x, &y, prec(parent(x)))
   return z
end

###############################################################################
#
#   Derivative and integral
#
###############################################################################

function derivative(x::arb_poly)
   z = parent(x)()
   ccall((:arb_poly_derivative, :libarb), Void, 
                (Ptr{arb_poly}, Ptr{arb_poly}, Int), &z, &x, prec(parent(x)))
   return z
end

function integral(x::arb_poly)
   z = parent(x)()
   ccall((:arb_poly_integral, :libarb), Void, 
                (Ptr{arb_poly}, Ptr{arb_poly}, Int), &z, &x, prec(parent(x)))
   return z
end

###############################################################################
#
#   Multipoint evaluation and interpolation
#
###############################################################################

function arb_vec(n::Int)
   return ccall((:_arb_vec_init, :libarb), Ptr{arb_struct}, (Int,), n)
end

function arb_vec(b::Array{arb, 1})
   v = ccall((:_arb_vec_init, :libarb), Ptr{arb_struct}, (Int,), length(b))
   for i=1:length(b)
       ccall((:arb_set, :libarb), Void, (Ptr{arb_struct}, Ptr{arb}),
           v + (i-1)*sizeof(arb_struct), &b[i])
   end
   return v
end

function array(R::ArbField, v::Ptr{arb_struct}, n::Int)
   r = Array{arb}(n)
   for i=1:n
       r[i] = R()
       ccall((:arb_set, :libarb), Void, (Ptr{arb}, Ptr{arb_struct}),
           &r[i], v + (i-1)*sizeof(arb_struct))
   end
   return r
end

function arb_vec_clear(v::Ptr{arb_struct}, n::Int)
   ccall((:_arb_vec_clear, :libarb), Void, (Ptr{arb_struct}, Int), v, n)
end

doc"""
    from_roots(R::ArbPolyRing, b::Array{arb, 1})
> Construct a polynomial in the given polynomial ring from a list of its roots.
"""
function from_roots(R::ArbPolyRing, b::Array{arb, 1})
   z = R()
   tmp = arb_vec(b)
   ccall((:arb_poly_product_roots, :libarb), Void, 
                (Ptr{arb_poly}, Ptr{arb_struct}, Int, Int), &z, tmp, length(b), prec(R))
   arb_vec_clear(tmp, length(b))
   return z
end

function evaluate_iter(x::arb_poly, b::Array{arb, 1})
   return arb[evaluate(x, b[i]) for i=1:length(b)]
end

function evaluate_fast(x::arb_poly, b::Array{arb, 1})
   tmp = arb_vec(b)
   ccall((:arb_poly_evaluate_vec_fast, :libarb), Void, 
                (Ptr{arb_struct}, Ptr{arb_poly}, Ptr{arb_struct}, Int, Int),
            tmp, &x, tmp, length(b), prec(parent(x)))
   res = array(base_ring(parent(x)), tmp, length(b))
   arb_vec_clear(tmp, length(b))
   return res
end

function interpolate_newton(R::ArbPolyRing, xs::Array{arb, 1}, ys::Array{arb, 1})
   length(xs) != length(ys) && error()
   z = R()
   xsv = arb_vec(xs)
   ysv = arb_vec(ys)
   ccall((:arb_poly_interpolate_newton, :libarb), Void, 
                (Ptr{arb_poly}, Ptr{arb_struct}, Ptr{arb_struct}, Int, Int),
            &z, xsv, ysv, length(xs), prec(R))
   arb_vec_clear(xsv, length(xs))
   arb_vec_clear(ysv, length(ys))
   return z
end

function interpolate_barycentric(R::ArbPolyRing, xs::Array{arb, 1}, ys::Array{arb, 1})
   length(xs) != length(ys) && error()
   z = R()
   xsv = arb_vec(xs)
   ysv = arb_vec(ys)
   ccall((:arb_poly_interpolate_barycentric, :libarb), Void, 
                (Ptr{arb_poly}, Ptr{arb_struct}, Ptr{arb_struct}, Int, Int),
            &z, xsv, ysv, length(xs), prec(R))
   arb_vec_clear(xsv, length(xs))
   arb_vec_clear(ysv, length(ys))
   return z
end

function interpolate_fast(R::ArbPolyRing, xs::Array{arb, 1}, ys::Array{arb, 1})
   length(xs) != length(ys) && error()
   z = R()
   xsv = arb_vec(xs)
   ysv = arb_vec(ys)
   ccall((:arb_poly_interpolate_fast, :libarb), Void, 
                (Ptr{arb_poly}, Ptr{arb_struct}, Ptr{arb_struct}, Int, Int),
            &z, xsv, ysv, length(xs), prec(R))
   arb_vec_clear(xsv, length(xs))
   arb_vec_clear(ysv, length(ys))
   return z
end

# todo: cutoffs for fast algorithm
function interpolate(R::ArbPolyRing, xs::Array{arb, 1}, ys::Array{arb, 1})
   return interpolate_newton(R, xs, ys)
end

# todo: cutoffs for fast algorithm
function evaluate(x::arb_poly, b::Array{arb, 1})
   return evaluate_iter(x, b)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::arb_poly)
   ccall((:arb_poly_zero, :libarb), Void, 
                    (Ptr{arb_poly}, ), &z)
   nothing
end

function fit!(z::arb_poly, n::Int)
   ccall((:arb_poly_fit_length, :libarb), Void, 
                    (Ptr{arb_poly}, Int), &z, n)
   nothing
end

function setcoeff!(z::arb_poly, n::Int, x::fmpz)
   ccall((:arb_poly_set_coeff_fmpz, :libarb), Void, 
                    (Ptr{arb_poly}, Int, Ptr{fmpz}), &z, n, &x)
   nothing
end

function setcoeff!(z::arb_poly, n::Int, x::arb)
   ccall((:arb_poly_set_coeff_arb, :libarb), Void, 
                    (Ptr{arb_poly}, Int, Ptr{arb}), &z, n, &x)
   nothing
end

function mul!(z::arb_poly, x::arb_poly, y::arb_poly)
   ccall((:arb_poly_mul, :libarb), Void, 
                (Ptr{arb_poly}, Ptr{arb_poly}, Ptr{arb_poly}, Int),
                    &z, &x, &y, prec(parent(z)))
   nothing
end

function addeq!(z::arb_poly, x::arb_poly)
   ccall((:arb_poly_add, :libarb), Void, 
                (Ptr{arb_poly}, Ptr{arb_poly}, Ptr{arb_poly}, Int),
                    &z, &z, &x, prec(parent(z)))
   nothing
end

function add!(z::arb_poly, x::arb_poly, y::arb_poly)
   ccall((:arb_poly_add, :libarb), Void, 
                (Ptr{arb_poly}, Ptr{arb_poly}, Ptr{arb_poly}, Int),
                    &z, &x, &y, prec(parent(z)))
   nothing
end

###############################################################################
#
#   Promotions
#
###############################################################################

Base.promote_rule(::Type{arb_poly}, ::Type{Float64}) = arb_poly

Base.promote_rule(::Type{arb_poly}, ::Type{Int}) = arb_poly

Base.promote_rule(::Type{arb_poly}, ::Type{fmpz}) = arb_poly

Base.promote_rule(::Type{arb_poly}, ::Type{fmpq}) = arb_poly

Base.promote_rule(::Type{arb_poly}, ::Type{arb}) = arb_poly

Base.promote_rule(::Type{arb_poly}, ::Type{fmpz_poly}) = arb_poly

Base.promote_rule(::Type{arb_poly}, ::Type{fmpq_poly}) = arb_poly

################################################################################
#
#  Parent object call overloads
#
################################################################################

function (a::ArbPolyRing)()
   z = arb_poly()
   z.parent = a
   return z
end

function (a::ArbPolyRing)(b::Union{Int,fmpz,fmpq,Float64,arb})
   z = arb_poly(base_ring(a)(b), a.base_ring.prec)
   z.parent = a
   return z
end

function (a::ArbPolyRing)(b::Array{arb, 1})
   z = arb_poly(b, a.base_ring.prec)
   z.parent = a
   return z
end

function (a::ArbPolyRing)(b::fmpz_poly)
   z = arb_poly(b, a.base_ring.prec)
   z.parent = a
   return z
end

function (a::ArbPolyRing)(b::fmpq_poly)
   z = arb_poly(b, a.base_ring.prec)
   z.parent = a
   return z
end

function (a::ArbPolyRing)(b::arb_poly)
   z = arb_poly(b, a.base_ring.prec)
   z.parent = a
   return z
end

################################################################################
#
#  PolynomialRing constructor
#
################################################################################

function PolynomialRing(R::ArbField, s::AbstractString)
  S = Symbol(s)
  parent_obj = ArbPolyRing(R, S)
  return parent_obj, parent_obj(fmpz_poly([fmpz(0), fmpz(1)]))
end

