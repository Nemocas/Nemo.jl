###############################################################################
#
#   fmpz_mat.jl : Flint matrices over fmpz
#
###############################################################################

export fmpz_mat, FmpzMatSpace, getindex, getindex!, setindex!, rows, cols,
       charpoly, det, det_divisor, det_given_divisor, gram, hadamard,
       ishadamard, hnf, ishnf, hnf_with_transform, hnf_modular, lll, lll_ctx,
       lll_gram, lll_with_transform, lll_gram_with_transform, lll_with_removal,
       lll_with_removal_transform, nullspace, rank, rref, reduce_mod, similar,
       snf, snf_diagonal, issnf, solve, solve_rational, cansolve,
       cansolve_with_nullspace, solve_dixon, trace, transpose, content, hcat,
       vcat, addmul!, zero!, window, pseudo_inv, hnf_modular_eldiv,
       nullspace_right_rational

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

elem_type(::Type{FmpzMatSpace}) = fmpz_mat

parent_type(::Type{fmpz_mat}) = FmpzMatSpace

base_ring(a::FmpzMatSpace) = a.base_ring

parent(a::fmpz_mat, cached::Bool = true) =
    FmpzMatSpace(rows(a), cols(a), cached)

function check_parent(a::fmpz_mat, b::fmpz_mat)
   (rows(a) != rows(b) || cols(a) != cols(b)) && error("Incompatible matrices")
end

###############################################################################
#
#   Similar
#
###############################################################################

function similar(x::fmpz_mat)
   z = fmpz_mat(rows(x), cols(x))
   z.base_ring = x.base_ring
   return z
end

function similar(x::fmpz_mat, r::Int, c::Int)
   z = fmpz_mat(r, c)
   z.base_ring = x.base_ring
   return z
end

###############################################################################
#
#   View and sub
#
###############################################################################

function Base.view(x::fmpz_mat, r1::Int, c1::Int, r2::Int, c2::Int)
   Generic._checkbounds(x, r1, c1)
   Generic._checkbounds(x, r2, c2)
   (r1 > r2 || c1 > c2) && error("Invalid parameters")
   b = fmpz_mat()
   b.base_ring = FlintZZ
   ccall((:fmpz_mat_window_init, :libflint), Void,
         (Ref{fmpz_mat}, Ref{fmpz_mat}, Int, Int, Int, Int),
             b, x, r1 - 1, c1 - 1, r2, c2)
   finalizer(b, _fmpz_mat_window_clear_fn)
   return b
end

function Base.view(x::fmpz_mat, r::UnitRange{Int}, c::UnitRange{Int})
   return Base.view(x, r.start, c.start, r.stop, c.stop)
end

function _fmpz_mat_window_clear_fn(a::fmpz_mat)
   ccall((:fmpz_mat_window_clear, :libflint), Void, (Ref{fmpz_mat},), a)
end

function sub(x::fmpz_mat, r1::Int, c1::Int, r2::Int, c2::Int)
   return deepcopy(view(x, r1, c1, r2, c2))
end

function sub(x::fmpz_mat, r::UnitRange{Int}, c::UnitRange{Int})
   return deepcopy(view(x, r, c))
end

getindex(x::fmpz_mat, r::UnitRange{Int}, c::UnitRange{Int}) = sub(x, r, c)

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function getindex!(v::fmpz, a::fmpz_mat, r::Int, c::Int)
   z = ccall((:fmpz_mat_entry, :libflint), Ptr{fmpz},
             (Ref{fmpz_mat}, Int, Int), a, r - 1, c - 1)
   ccall((:fmpz_set, :libflint), Void, (Ref{fmpz}, Ptr{fmpz}), v, z)
end

@inline function getindex(a::fmpz_mat, r::Int, c::Int)
   @boundscheck Generic._checkbounds(a, r, c)
   v = fmpz()
   z = ccall((:fmpz_mat_entry, :libflint), Ptr{fmpz},
             (Ref{fmpz_mat}, Int, Int), a, r - 1, c - 1)
   ccall((:fmpz_set, :libflint), Void, (Ref{fmpz}, Ptr{fmpz}), v, z)
   return v
end

@inline function setindex!(a::fmpz_mat, d::fmpz, r::Int, c::Int)
   @boundscheck Generic._checkbounds(a, r, c)
   z = ccall((:fmpz_mat_entry, :libflint), Ptr{fmpz},
             (Ref{fmpz_mat}, Int, Int), a, r - 1, c - 1)
   ccall((:fmpz_set, :libflint), Void, (Ptr{fmpz}, Ref{fmpz}), z, d)
end

@inline setindex!(a::fmpz_mat, d::Integer, r::Int, c::Int) = setindex!(a, fmpz(d), r, c)

@inline function setindex!(a::fmpz_mat, d::Int, r::Int, c::Int)
   @boundscheck Generic._checkbounds(a, r, c)
   z = ccall((:fmpz_mat_entry, :libflint), Ptr{fmpz},
             (Ref{fmpz_mat}, Int, Int), a, r - 1, c - 1)
   ccall((:fmpz_set_si, :libflint), Void, (Ptr{fmpz}, Int), z, d)
end

@inline rows(a::fmpz_mat) = a.r

@inline cols(a::fmpz_mat) = a.c

zero(a::FmpzMatSpace) = a()

one(a::FmpzMatSpace) = a(1)

iszero(a::fmpz_mat) = ccall((:fmpz_mat_is_zero, :libflint), Bool,
                            (Ref{fmpz_mat},), a)

isone(a::fmpz_mat) = ccall((:fmpz_mat_is_one, :libflint), Bool,
                           (Ref{fmpz_mat},), a)

function deepcopy_internal(d::fmpz_mat, dict::ObjectIdDict)
   z = fmpz_mat(d)
   z.base_ring = d.base_ring
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::fmpz_mat) = canonical_unit(a[1, 1])

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, a::FmpzMatSpace)
   print(io, "Matrix Space of ")
   print(io, a.rows, " rows and ", a.cols, " columns over ")
   print(io, "Integer Ring")
end

function show(io::IO, a::fmpz_mat)
   r = rows(a)
   c = cols(a)
   for i = 1:r
      print(io, "[")
      for j = 1:c
         print(io, a[i, j])
         if j != c
            print(io, " ")
         end
      end
      print(io, "]")
      if i != r
         println(io, "")
      end
   end
end

show_minus_one(::Type{fmpz_mat}) = show_minus_one(fmpz)

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fmpz_mat)
   z = similar(x)
   ccall((:fmpz_mat_neg, :libflint), Void,
         (Ref{fmpz_mat}, Ref{fmpz_mat}), z, x)
   return z
end

###############################################################################
#
#   transpose
#
###############################################################################

function transpose(x::fmpz_mat)
   z = similar(x, cols(x), rows(x))
   ccall((:fmpz_mat_transpose, :libflint), Void,
         (Ref{fmpz_mat}, Ref{fmpz_mat}), z, x)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fmpz_mat, y::fmpz_mat)
   check_parent(x, y)
   z = similar(x)
   ccall((:fmpz_mat_add, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat},  Ref{fmpz_mat}),
               z, x, y)
   return z
end

function -(x::fmpz_mat, y::fmpz_mat)
   check_parent(x, y)
   z = similar(x)
   ccall((:fmpz_mat_sub, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat},  Ref{fmpz_mat}),
               z, x, y)
   return z
end

function *(x::fmpz_mat, y::fmpz_mat)
   cols(x) != rows(y) && error("Incompatible matrix dimensions")
   if rows(x) == cols(y) && rows(x) == cols(x)
      z = similar(x)
   else
      z = similar(x, rows(x), cols(y))
   end
   ccall((:fmpz_mat_mul, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat},  Ref{fmpz_mat}),
               z, x, y)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Int, y::fmpz_mat)
   z = similar(y)
   ccall((:fmpz_mat_scalar_mul_si, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Int), z, y, x)
   return z
end

function *(x::fmpz, y::fmpz_mat)
   z = similar(y)
   ccall((:fmpz_mat_scalar_mul_fmpz, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz}), z, y, x)
   return z
end

*(x::fmpz_mat, y::Int) = y*x

*(x::fmpz_mat, y::fmpz) = y*x

*(x::Integer, y::fmpz_mat) = fmpz(x)*y

*(x::fmpz_mat, y::Integer) = fmpz(y)*x

function +(x::fmpz_mat, y::Integer)
   z = deepcopy(x)
   for i = 1:min(rows(x), cols(x))
      z[i, i] += y
   end
   return z
end

function +(x::fmpz_mat, y::fmpz)
   z = deepcopy(x)
   for i = 1:min(rows(x), cols(x))
      z[i, i] = addeq!(z[i, i], y)
   end
   return z
end

+(x::Integer, y::fmpz_mat) = y + x

+(x::fmpz, y::fmpz_mat) = y + x

-(x::fmpz_mat, y::Integer) = x + (-y)

-(x::fmpz_mat, y::fmpz) = x + (-y)

function -(x::Integer, y::fmpz_mat)
   z = -y
   for i = 1:min(rows(y), cols(y))
      z[i, i] += x
   end
   return z
end

function -(x::fmpz, y::fmpz_mat)
   z = -y
   for i = 1:min(rows(y), cols(y))
      z[i, i] = addeq!(z[i, i], x)
   end
   return z
end

###############################################################################
#
#   Scaling
#
###############################################################################

doc"""
    <<(x::fmpz_mat, y::Int)
> Return $2^yx$.
"""
function <<(x::fmpz_mat, y::Int)
   y < 0 && throw(DomainError())
   z = similar(x)
   ccall((:fmpz_mat_scalar_mul_2exp, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Int),
               z, x, y)
   return z
end

doc"""
    >>(x::fmpz_mat, y::Int)
> Return $x/2^y$ where rounding is towards zero.
"""
function >>(x::fmpz_mat, y::Int)
   y < 0 && throw(DomainError())
   z = similar(x)
   ccall((:fmpz_mat_scalar_tdiv_q_2exp, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Int),
               z, x, y)
   return z
end

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::fmpz_mat, y::Int)
   y < 0 && throw(DomainError())
   rows(x) != cols(x) && error("Incompatible matrix dimensions")
   z = similar(x)
   ccall((:fmpz_mat_pow, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Int),
               z, x, y)
   return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

function ==(x::fmpz_mat, y::fmpz_mat)
   check_parent(x, y)
   ccall((:fmpz_mat_equal, :libflint), Bool,
                                       (Ref{fmpz_mat}, Ref{fmpz_mat}), x, y)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::fmpz_mat, y::Integer)
   for i = 1:min(rows(x), cols(x))
      if x[i, i] != y
         return false
      end
   end
   for i = 1:rows(x)
      for j = 1:cols(x)
         if i != j && x[i, j] != 0
            return false
         end
      end
   end
   return true
end

==(x::Integer, y::fmpz_mat) = y == x

==(x::fmpz_mat, y::fmpz) = x == parent(x)(y)

==(x::fmpz, y::fmpz_mat) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::fmpz_mat)
   z = similar(x)
   d = fmpz()
   ccall((:fmpz_mat_inv, :libflint), Void,
         (Ref{fmpz_mat}, Ref{fmpz}, Ref{fmpz_mat}), z, d, x)
   if d == 1
      return z
   end
   if d == -1
      return -z
   end
   error("Matrix not invertible")
end

###############################################################################
#
#   Pseudo inversion
#
###############################################################################

doc"""
    pseudo_inv(x::fmpz_mat)
> Return a tuple $(z, d)$ consisting of a matrix $z$ and denominator $d$ such
> that $z/d$ is the inverse of $x$.
"""
function pseudo_inv(x::fmpz_mat)
   z = similar(x)
   d = fmpz()
   ccall((:fmpz_mat_inv, :libflint), Void,
         (Ref{fmpz_mat}, Ref{fmpz}, Ref{fmpz_mat}), z, d, x)
   if !iszero(d)
      return (z, d)
   end
   error("Matrix is singular")
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpz_mat, y::fmpz_mat)
   cols(x) != cols(y) && error("Incompatible matrix dimensions")
   x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpz_mat, y::Int)
   z = similar(x)
   ccall((:fmpz_mat_scalar_divexact_si, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Int), z, x, y)
   return z
end

function divexact(x::fmpz_mat, y::fmpz)
   z = similar(x)
   ccall((:fmpz_mat_scalar_divexact_fmpz, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz}), z, x, y)
   return z
end

divexact(x::fmpz_mat, y::Integer) = divexact(x, fmpz(y))

###############################################################################
#
#   Kronecker product
#
###############################################################################

function kronecker_product(x::fmpz_mat, y::fmpz_mat)
   base_ring(x) == base_ring(y) || error("Incompatible matrices")
   z = similar(x, rows(x)*rows(y), cols(x)*cols(y))
   ccall((:fmpz_mat_kronecker_product, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz_mat}), z, x, y)
   return z
end

###############################################################################
#
#   Modular reduction
#
###############################################################################

doc"""
    reduce_mod(x::fmpz_mat, y::fmpz)
> Reduce the entries of $x$ modulo $y$ and return the result.
"""
function reduce_mod(x::fmpz_mat, y::fmpz)
   z = similar(x)
   ccall((:fmpz_mat_scalar_mod_fmpz, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz}), z, x, y)
   return z
end

doc"""
    reduce_mod(x::fmpz_mat, y::Integer)
> Reduce the entries of $x$ modulo $y$ and return the result.
"""
reduce_mod(x::fmpz_mat, y::Integer) = reduce_mod(x, fmpz(y))

###############################################################################
#
#   Characteristic polynomial
#
###############################################################################

function charpoly(R::FmpzPolyRing, x::fmpz_mat)
   rows(x) != cols(x) && error("Non-square")
   z = R()
   ccall((:fmpz_mat_charpoly, :libflint), Void,
                (Ref{fmpz_poly}, Ref{fmpz_mat}), z, x)
   return z
end

###############################################################################
#
#   Minimal polynomial
#
###############################################################################

function minpoly(R::FmpzPolyRing, x::fmpz_mat)
   rows(x) != cols(x) && error("Non-square")
   z = R()
   ccall((:fmpz_mat_minpoly, :libflint), Void,
                (Ref{fmpz_poly}, Ref{fmpz_mat}), z, x)
   return z
end

###############################################################################
#
#   Determinant
#
###############################################################################

function det(x::fmpz_mat)
   rows(x) != cols(x) && error("Non-square matrix")
   z = fmpz()
   ccall((:fmpz_mat_det, :libflint), Void,
                (Ref{fmpz}, Ref{fmpz_mat}), z, x)
   return z
end

doc"""
    det_divisor(x::fmpz_mat)
> Return some positive divisor of the determinant of $x$, if the determinant
> is nonzero, otherwise return zero.
"""
function det_divisor(x::fmpz_mat)
   rows(x) != cols(x) && error("Non-square matrix")
   z = fmpz()
   ccall((:fmpz_mat_det_divisor, :libflint), Void,
                (Ref{fmpz}, Ref{fmpz_mat}), z, x)
   return z
end

doc"""
    det_given_divisor(x::fmpz_mat, d::fmpz, proved=true)
> Return the determinant of $x$ given a positive divisor of its determinant. If
> `proved == true` (the default), the output is guaranteed to be correct,
> otherwise a heuristic algorithm is used.
"""
function det_given_divisor(x::fmpz_mat, d::fmpz, proved=true)
   rows(x) != cols(x) && error("Non-square")
   z = fmpz()
   ccall((:fmpz_mat_det_modular_given_divisor, :libflint), Void,
               (Ref{fmpz}, Ref{fmpz_mat}, Ref{fmpz}, Cint), z, x, d, proved)
   return z
end

doc"""
    det_given_divisor(x::fmpz_mat, d::Integer, proved=true)
> Return the determinant of $x$ given a positive divisor of its determinant. If
> `proved == true` (the default), the output is guaranteed to be correct,
> otherwise a heuristic algorithm is used.
"""
function det_given_divisor(x::fmpz_mat, d::Integer, proved=true)
   return det_given_divisor(x, fmpz(d), proved)
end

###############################################################################
#
#   Gram matrix
#
###############################################################################

function gram(x::fmpz_mat)
   z = similar(x, rows(x), rows(x))
   ccall((:fmpz_mat_gram, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}), z, x)
   return z
end

###############################################################################
#
#   Hadamard matrix
#
###############################################################################

doc"""
    hadamard(R::FmpzMatSpace)
> Return the Hadamard matrix for the given matrix space. The number of rows and
> columns must be equal.
"""
function hadamard(R::FmpzMatSpace)
   R.rows != R.cols && error("Unable to create Hadamard matrix")
   z = R()
   success = ccall((:fmpz_mat_hadamard, :libflint), Bool,
                   (Ref{fmpz_mat},), z)
   !success && error("Unable to create Hadamard matrix")
   return z
end

doc"""
    ishadamard(x::fmpz_mat)
> Return `true` if the given matrix is Hadamard, otherwise return `false`.
"""
function ishadamard(x::fmpz_mat)
   return ccall((:fmpz_mat_is_hadamard, :libflint), Bool,
                   (Ref{fmpz_mat},), x)
end

###############################################################################
#
#   Hermite normal form
#
###############################################################################

doc"""
    hnf(x::fmpz_mat)
> Return the Hermite Normal Form of $x$.
"""
function hnf(x::fmpz_mat)
   z = similar(x)
   ccall((:fmpz_mat_hnf, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}), z, x)
   return z
end

doc"""
    hnf_with_transform(x::fmpz_mat)
> Compute a tuple $(H, T)$ where $H$ is the Hermite normal form of $x$ and $T$
> is a transformation matrix so that $H = Tx$.
"""
function hnf_with_transform(x::fmpz_mat)
   z = similar(x)
   u = similar(x, rows(x), rows(x))
   ccall((:fmpz_mat_hnf_transform, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz_mat}), z, u, x)
   return z, u
end

doc"""
    hnf_modular(x::fmpz_mat, d::fmpz)
> Compute the Hermite normal form of $x$ given that $d$ is a multiple of the
> determinant of the nonzero rows of $x$.
"""
function hnf_modular(x::fmpz_mat, d::fmpz)
   z = similar(x)
   ccall((:fmpz_mat_hnf_modular, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz}), z, x, d)
   return z
end

doc"""
    hnf_modular_eldiv(x::fmpz_mat, d::fmpz)
> Compute the Hermite normal form of $x$ given that $d$ is a multiple of the
> largest elementary divisor of $x$. The matrix $x$ must have full rank.
"""
function hnf_modular_eldiv(x::fmpz_mat, d::fmpz)
   (rows(x) < cols(x)) &&
                error("Matrix must have at least as many rows as columns")
   z = deepcopy(x)
   ccall((:fmpz_mat_hnf_modular_eldiv, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz}), z, d)
   return z
end

doc"""
    ishnf(x::fmpz_mat)
> Return `true` if the given matrix is in Hermite Normal Form, otherwise return
> `false`.
"""
function ishnf(x::fmpz_mat)
   return ccall((:fmpz_mat_is_in_hnf, :libflint), Bool,
                   (Ref{fmpz_mat},), x)
end

###############################################################################
#
#   LLL
#
###############################################################################

mutable struct lll_ctx
   delta::Float64
   eta::Float64
   rep_type::Int
   gram_type::Int

   function lll_ctx(delta::Float64, eta::Float64,
                    rep::Symbol = :zbasis, gram::Symbol = :approx)
      rt = rep == :zbasis ? 1 : 0
      gt = gram == :approx ? 0 : 1
      return new(delta, eta, rt, gt)
   end
end


doc"""
> Compute a tuple $(L, T)$ where $L$ is the LLL reduction of $a$ and $T$ is a
> transformation matrix so that $L = Ta$. All the default parameters can be
> overridden by supplying an optional context object.
"""
function lll_with_transform(x::fmpz_mat, ctx::lll_ctx = lll_ctx(0.99, 0.51))
   z = deepcopy(x)
   u = similar(x, rows(x), rows(x))
   for i in 1:rows(u)
      u[i, i] = 1
   end
   ccall((:fmpz_lll, :libflint), Void,
         (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{lll_ctx}), z, u, ctx)
   return z, u
end

doc"""
    lll(x::fmpz_mat, ctx=lll_ctx(0.99, 0.51))
> Return the LLL reduction of the matrix $x$. By default the matrix $x$ is a
> $\mathbb{Z}$-basis and the Gram matrix is maintained throughout in
> approximate form. The LLL is performed with reduction parameters
> $\delta = 0.99$ and $\eta = 0.51$. All of these defaults can be overridden by
> specifying an optional context object.
"""
function lll(x::fmpz_mat, ctx::lll_ctx = lll_ctx(0.99, 0.51))
   z = deepcopy(x)
   if rows(z) == 0
     return z
   end
   u = similar(x, rows(x), rows(x))
   ccall((:fmpz_lll, :libflint), Void,
         (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{lll_ctx}), z, u, ctx)
   return z
end

doc"""
    lll_gram_with_transform(x::fmpz_mat, ctx=lll_ctx(0.99, 0.51, :gram))
> Given the Gram matrix $x$ of a matrix $M$, compute a tuple $(L, T)$ where
> $L$ is the gram matrix of the LLL reduction of the matrix and $T$ is a
> transformation matrix so that $L = TM$.
"""
function lll_gram_with_transform(x::fmpz_mat, ctx::lll_ctx = lll_ctx(0.99, 0.51, :gram))
   z = deepcopy(x)
   u = similar(x, rows(x), rows(x))
   for i in 1:rows(u)
      u[i, i] = 1
   end
   ccall((:fmpz_lll, :libflint), Void,
         (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{lll_ctx}), z, u, ctx)
   return z, u
end

doc"""
    lll_gram(x::fmpz_mat, ctx=lll_ctx(0.99, 0.51, :gram))
> Given the Gram matrix $x$ of a matrix, compute the Gram matrix of its LLL
> reduction.
"""
function lll_gram(x::fmpz_mat, ctx::lll_ctx = lll_ctx(0.99, 0.51, :gram))
   z = deepcopy(x)
   u = similar(x, rows(x), rows(x))
   ccall((:fmpz_lll, :libflint), Void,
         (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{lll_ctx}), z, u, ctx)
   return z
end

doc"""
    lll_with_removal_transform(x::fmpz_mat, b::fmpz, ctx=lll_ctx(0.99, 0.51))
> Compute a tuple $(r, L, T)$ where the first $r$ rows of $L$ are those
> remaining from the LLL reduction after removal of vectors with norm exceeding
> the bound $b$ and $T$ is a transformation matrix so that $L = Tx$.
"""
function lll_with_removal_transform(x::fmpz_mat, b::fmpz, ctx::lll_ctx = lll_ctx(0.99, 0.51))
   z = deepcopy(x)
   u = similar(x, rows(x), rows(x))
   for i in 1:rows(u)
      u[i, i] = 1
   end
   d = Int(ccall((:fmpz_lll_with_removal, :libflint), Cint,
    (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz}, Ref{lll_ctx}), z, u, b, ctx))
   return d, z, u
end

doc"""
    lll_with_removal(x::fmpz_mat, b::fmpz, ctx=lll_ctx(0.99, 0.51))
> Compute the LLL reduction of $x$ and throw away rows whose norm exceeds
> the given bound $b$. Return a tuple $(r, L)$ where the first $r$ rows of $L$
> are the rows remaining after removal.
"""
function lll_with_removal(x::fmpz_mat, b::fmpz, ctx::lll_ctx = lll_ctx(0.99, 0.51))
   z = deepcopy(x)
   u = similar(x, rows(x), rows(x))
   d = Int(ccall((:fmpz_lll_with_removal, :libflint), Cint,
    (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz}, Ref{lll_ctx}), z, u, b, ctx))
   return d, z
end

###############################################################################
#
#   Nullspace
#
###############################################################################

function nullspace(x::fmpz_mat)
  H, T = hnf_with_transform(transpose(x))
  for i = rows(H):-1:1
    for j = 1:cols(H)
      if !iszero(H[i, j])
        N = similar(x, cols(x), rows(H) - i)
        for k = 1:rows(N)
          for l = 1:cols(N)
            N[k, l] = T[rows(T) - l + 1, k]
          end
        end
        return N, cols(N)
      end
    end
  end
  return similar(x, cols(x), 0), 0
end

doc"""
    nullspace_right_rational(x::fmpz_mat)
> Return the right rational nullspace of $x$, i.e. a set of vectors over
> $\mathbb{Z}$ giving a $\mathbb{Q}$-basis for the nullspace of $x$
> considered as a matrix over $\mathbb{Q}$.
"""
function nullspace_right_rational(x::fmpz_mat)
   z = similar(x)
   u = similar(x, cols(x), cols(x))
   rank = ccall((:fmpz_mat_nullspace, :libflint), Cint,
                (Ref{fmpz_mat}, Ref{fmpz_mat}), u, x)
   return u, rank
end

###############################################################################
#
#   Rank
#
###############################################################################

function rank(x::fmpz_mat)
   return ccall((:fmpz_mat_rank, :libflint), Int,
                (Ref{fmpz_mat},), x)
end

###############################################################################
#
#   Reduced row echelon form
#
###############################################################################

function rref(x::fmpz_mat)
   z = similar(x)
   d = fmpz()
   r = ccall((:fmpz_mat_rref, :libflint), Int,
            (Ref{fmpz_mat}, Ref{fmpz}, Ref{fmpz_mat}), z, d, x)
   return r, z, d
end

###############################################################################
#
#   Smith normal form
#
###############################################################################

doc"""
    snf(x::fmpz_mat)
> Compute the Smith normal form of $x$.
"""
function snf(x::fmpz_mat)
   z = similar(x)
   ccall((:fmpz_mat_snf, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}), z, x)
   return z
end

doc"""
    snf_diagonal(x::fmpz_mat)
> Given a diagonal matrix $x$ compute the Smith normal form of $x$.
"""
function snf_diagonal(x::fmpz_mat)
   z = similar(x)
   ccall((:fmpz_mat_snf_diagonal, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}), z, x)
   return z
end

doc"""
    issnf(x::fmpz_mat)
> Return `true` if $x$ is in Smith normal form, otherwise return `false`.
"""
function issnf(x::fmpz_mat)
   return ccall((:fmpz_mat_is_in_snf, :libflint), Bool,
                   (Ref{fmpz_mat},), x)
end

###############################################################################
#
#   Linear solving
#
###############################################################################

doc"""
    solve(a::fmpz_mat, b::fmpz_mat) -> fmpz_mat
> Return a matrix $x$ such that $ax = b$. An exception is raised
> if this is not possible.
"""
function solve(a::fmpz_mat, b::fmpz_mat)
   rows(b) != rows(a) && error("Incompatible dimensions in solve")
   fl, z = cansolve(a, b)
   if !fl
     error("system is inconsistent")
   end
   return z
end

doc"""
    cansolve(a::fmpz_mat, b::fmpz_mat) -> Bool, fmpz_mat
> Return true and a matrix $x$ such that $ax = b$, or false and some matrix
> in case $x$ does not exist.
"""
function cansolve(a::fmpz_mat, b::fmpz_mat)
   rows(b) != rows(a) && error("Incompatible dimensions in cansolve")
   H, T = hnf_with_transform(transpose(a))
   b = deepcopy(b)
   z = similar(a, cols(b), cols(a))
   l = min(rows(a), cols(a))
   for i = 1:cols(b)
     for j = 1:l
       k = 1
       while k <= cols(H) && iszero(H[j, k])
         k += 1
       end
       if k > cols(H)
         continue
       end
       q, r = divrem(b[k, i], H[j, k])
       if !iszero(r)
         return false, b
       end
       for h = k:cols(H)
         b[h, i] -= q*H[j, h]
       end
       z[i, j] = q
     end
   end
   if !iszero(b)
     return false, b
   end
   return true, transpose(z*T)
end

doc"""
    cansolve_with_nullspace(a::fmpz_mat, b::fmpz_mat) -> Bool, fmpz_mat, fmpz_mat
> Return true, a matrix $x$ and a matrix $k$ such that $ax = b$ and the columns
> of $k$ form a basis for the nullspace of $a$. In case $x$ does not exist, false
> and two arbitrary matrices are returned.
"""
function cansolve_with_nullspace(a::fmpz_mat, b::fmpz_mat)
   rows(b) != rows(a) && error("Incompatible dimensions in cansolve_with_nullspace")
   H, T = hnf_with_transform(transpose(a))
   z = similar(a, cols(b), cols(a))
   l = min(rows(a), cols(a))
   for i=1:cols(b)
     for j=1:l
       k = 1
       while k <= cols(H) && iszero(H[j, k])
         k += 1
       end
       if k > cols(H)
         continue
       end
       q, r = divrem(b[k, i], H[j, k])
       if !iszero(r)
         return false, b, b
       end
       for h=k:cols(H)
         b[h, i] -= q*H[j, h]
       end
       z[i, k] = q
     end
   end
   if !iszero(b)
     return false, b, b
   end

   for i = rows(H):-1:1
     for j = 1:cols(H)
       if !iszero(H[i,j])
         N = similar(a, cols(a), rows(H) - i)
         for k = 1:rows(N)
           for l = 1:cols(N)
             N[k,l] = T[rows(T) - l + 1, k]
           end
         end
         return true, transpose(z*T), N
       end
     end
   end
   N =  similar(a, cols(a), 0)

   return true, (z*T), N
end

doc"""
    solve_rational(a::fmpz_mat, b::fmpz_mat)
> If it exists, return a tuple $(x, d)$ consisting of a column vector $x$ such
> that $ax = db$. The element $b$ must be a column vector with the same number
> of rows as $a$ and $a$ must be a square matrix. If these conditions are not
> met or $(x, d)$ does not exist, an exception is raised.
"""
function solve_rational(a::fmpz_mat, b::fmpz_mat)
   rows(a) != cols(a) && error("Not a square matrix in solve_rational")
   rows(b) != rows(a) && error("Incompatible dimensions in solve_rational")
   z = similar(b)
   d = fmpz()
   nonsing = ccall((:fmpz_mat_solve, :libflint), Bool,
      (Ref{fmpz_mat}, Ref{fmpz}, Ref{fmpz_mat}, Ref{fmpz_mat}), z, d, a, b)
   !nonsing && error("Singular matrix in solve_rational")
   return z, d
end

function Generic.solve_with_det(a::fmpz_mat, b::fmpz_mat)
   return solve_rational(a, b)
end

doc"""
    solve_dixon(a::fmpz_mat, b::fmpz_mat)
> Return a tuple $(x, m)$ consisting of a column vector $x$ such that $ax = b
> \pmod{m}$. The element  $b$ must be a column vector with the same number > of
> rows as $a$ and $a$ must be a square matrix. If these conditions are not met
> or $(x, d)$ does not exist, an exception is raised.
"""
function solve_dixon(a::fmpz_mat, b::fmpz_mat)
   rows(a) != cols(a) && error("Not a square matrix in solve")
   rows(b) != rows(a) && error("Incompatible dimensions in solve")
   z = similar(b)
   d = fmpz()
   nonsing = ccall((:fmpz_mat_solve_dixon, :libflint), Bool,
      (Ref{fmpz_mat}, Ref{fmpz}, Ref{fmpz_mat}, Ref{fmpz_mat}), z, d, a, b)
   !nonsing && error("Singular matrix in solve")
   return z, d
end

###############################################################################
#
#   Trace
#
###############################################################################

function trace(x::fmpz_mat)
   rows(x) != cols(x) && error("Not a square matrix in trace")
   d = fmpz()
   ccall((:fmpz_mat_trace, :libflint), Int,
                (Ref{fmpz}, Ref{fmpz_mat}), d, x)
   return d
end

###############################################################################
#
#   Content
#
###############################################################################

function content(x::fmpz_mat)
  d = fmpz()
  ccall((:fmpz_mat_content, :libflint), Void,
        (Ref{fmpz}, Ref{fmpz_mat}), d, x)
  return d
end

###############################################################################
#
#   Concatenation
#
###############################################################################

function hcat(a::fmpz_mat, b::fmpz_mat)
  rows(a) != rows(b) && error("Incompatible number of rows in hcat")
  c = similar(a, rows(a), cols(a) + cols(b))
  ccall((:fmpz_mat_concat_horizontal, :libflint), Void,
        (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz_mat}), c, a, b)
  return c
end

function vcat(a::fmpz_mat, b::fmpz_mat)
  cols(a) != cols(b) && error("Incompatible number of columns in vcat")
  c = similar(a, rows(a) + rows(b), cols(a))
  ccall((:fmpz_mat_concat_vertical, :libflint), Void,
        (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz_mat}), c, a, b)
  return c
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function mul!(z::fmpz_mat, x::fmpz_mat, y::fmpz_mat)
   ccall((:fmpz_mat_mul, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz_mat}), z, x, y)
   return z
end

function mul!(y::fmpz_mat, x::Int)
   ccall((:fmpz_mat_scalar_mul_si, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Int), y, y, x)
   return y
end

function mul!(y::fmpz_mat, x::fmpz)
   ccall((:fmpz_mat_scalar_mul_fmpz, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz}), y, y, x)
   return y
end

function addmul!(z::fmpz_mat, y::fmpz_mat, x::fmpz)
   ccall((:fmpz_mat_scalar_addmul_fmpz, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz}), z, y, x)
   return y
end

function addmul!(z::fmpz_mat, y::fmpz_mat, x::Int)
   ccall((:fmpz_mat_scalar_addmul_si, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Int), z, y, x)
   return y
end

function addeq!(z::fmpz_mat, x::fmpz_mat)
   ccall((:fmpz_mat_add, :libflint), Void,
                (Ref{fmpz_mat}, Ref{fmpz_mat}, Ref{fmpz_mat}), z, z, x)
   return z
end

function zero!(z::fmpz_mat)
   ccall((:fmpz_mat_zero, :libflint), Void,
                (Ref{fmpz_mat},), z)
   return z
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::FmpzMatSpace)()
   z = fmpz_mat(a.rows, a.cols)
   z.base_ring = FlintZZ
   return z
end

function (a::FmpzMatSpace)(arr::Array{fmpz, 2})
   _check_dim(a.rows, a.cols, arr)
   z = fmpz_mat(a.rows, a.cols, arr)
   z.base_ring = FlintZZ
   return z
end

function (a::FmpzMatSpace)(arr::Array{T, 2}) where {T <: Integer}
   _check_dim(a.rows, a.cols, arr)
   z = fmpz_mat(a.rows, a.cols, arr)
   z.base_ring = FlintZZ
   return z
end

function (a::FmpzMatSpace)(arr::Array{fmpz, 1})
   _check_dim(a.rows, a.cols, arr)
   z = fmpz_mat(a.rows, a.cols, arr)
   z.base_ring = FlintZZ
   return z
end

function (a::FmpzMatSpace)(arr::Array{T, 1}) where {T <: Integer}
   _check_dim(a.rows, a.cols, arr)
   z = fmpz_mat(a.rows, a.cols, arr)
   z.base_ring = FlintZZ
   return z
end

function (a::FmpzMatSpace)(d::fmpz)
   z = fmpz_mat(a.rows, a.cols, d)
   z.base_ring = FlintZZ
   return z
end

function (a::FmpzMatSpace)(d::Integer)
   z = fmpz_mat(a.rows, a.cols, fmpz(d))
   z.base_ring = FlintZZ
   return z
end

(a::FmpzMatSpace)(d::fmpz_mat) = d

###############################################################################
#
#   Conversions and promotions
#
###############################################################################

promote_rule(::Type{fmpz_mat}, ::Type{T}) where {T <: Integer} = fmpz_mat

promote_rule(::Type{fmpz_mat}, ::Type{fmpz}) = fmpz_mat

function (::Type{Base.Array{Int, 2}})(A::fmpz_mat)
    m, n = size(A)

    fittable = [fits(Int, A[i, j]) for i in 1:m, j in 1:n]
    if !all(fittable)
        error("When trying to convert a fmpz_mat to a Matrix{Int}, some elements were too large to fit into Int: try to convert to a matrix of BigInt.")
    end

    mat::Matrix{Int} = Int[A[i, j] for i in 1:m, j in 1:n]
    return mat
end

function (::Type{Base.Array{BigInt, 2}})(A::fmpz_mat)
    m, n = size(A)
    # No check: always ensured to fit a BigInt.
    mat::Matrix{BigInt} = BigInt[A[i, j] for i in 1:m, j in 1:n]
    return mat
end

###############################################################################
#
#   Matrix constructor
#
###############################################################################

function matrix(R::FlintIntegerRing, arr::Array{fmpz, 2})
   z = fmpz_mat(size(arr, 1), size(arr, 2), arr)
   z.base_ring = FlintZZ
   return z
end

function matrix(R::FlintIntegerRing, arr::Array{<: Integer, 2})
   z = fmpz_mat(size(arr, 1), size(arr, 2), arr)
   z.base_ring = FlintZZ
   return z
end

function matrix(R::FlintIntegerRing, r::Int, c::Int, arr::Array{fmpz, 1})
   _check_dim(r, c, arr)
   z = fmpz_mat(r, c, arr)
   z.base_ring = FlintZZ
   return z
end

function matrix(R::FlintIntegerRing, r::Int, c::Int, arr::Array{<: Integer, 1})
   _check_dim(r, c, arr)
   z = fmpz_mat(r, c, arr)
   z.base_ring = FlintZZ
   return z
end

###############################################################################
#
#  Zero matrix
#
###############################################################################

function zero_matrix(R::FlintIntegerRing, r::Int, c::Int)
   z = fmpz_mat(r, c)
   z.base_ring = FlintZZ
   return z
end

###############################################################################
#
#  Identity matrix
#
###############################################################################

function identity_matrix(R::FlintIntegerRing, n::Int)
   z = fmpz_mat(n, n)
   ccall((:fmpz_mat_one, :libflint), Void, (Ref{fmpz_mat}, ), z)
   z.base_ring = FlintZZ
   return z
end

###############################################################################
#
#   MatrixSpace constructor
#
###############################################################################

function MatrixSpace(R::FlintIntegerRing, r::Int, c::Int, cached::Bool = true)
   return FmpzMatSpace(r, c, cached)
end
