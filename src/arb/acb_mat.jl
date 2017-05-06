###############################################################################
#
#   acb_mat.jl : Arb matrices over acb
#
###############################################################################

export rows, cols, zero, one, deepcopy, -, transpose, +, *, &, ==, !=,
       strongequal, overlaps, contains, inv, divexact, charpoly, det, exp,
       lufact, lufact!, solve, solve!, solve_lu_precomp, solve_lu_precomp!,
       swap_rows, swap_rows!, bound_inf_norm, isreal

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function getindex!(z::acb, x::acb_mat, r::Int, c::Int)
  v = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
              (Ptr{acb_mat}, Int, Int), &x, r - 1, c - 1)
  ccall((:acb_set, :libarb), Void, (Ptr{acb}, Ptr{acb}), &z, v)
  return z
end

function getindex(x::acb_mat, r::Int, c::Int)
  _checkbounds(rows(x), r) || throw(BoundsError())
  _checkbounds(cols(x), c) || throw(BoundsError())

  z = base_ring(x)()
  v = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
              (Ptr{acb_mat}, Int, Int), &x, r - 1, c - 1)
  ccall((:acb_set, :libarb), Void, (Ptr{acb}, Ptr{acb}), &z, v)
  return z
end

function setindex!(x::acb_mat,
                   y::Union{Int, UInt, Float64, fmpz, fmpq, arb, BigFloat,
                            acb, AbstractString},
                   r::Int, c::Int)
  _checkbounds(rows(x), r) || throw(BoundsError())
  _checkbounds(cols(x), c) || throw(BoundsError())

  z = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
              (Ptr{acb_mat}, Int, Int), &x, r - 1, c - 1)
  _acb_set(z, y, prec(base_ring(x)))
end

function setindex!{T <: Union{Int, UInt, Float64, fmpz, fmpq, arb, BigFloat,
                              AbstractString}}(x::acb_mat, y::Tuple{T, T},
                              r::Int, c::Int)
  _checkbounds(rows(x), r) || throw(BoundsError())
  _checkbounds(cols(x), c) || throw(BoundsError())

  z = ccall((:acb_mat_entry_ptr, :libarb), Ptr{acb},
              (Ptr{acb_mat}, Int, Int), &x, r - 1, c - 1)
  _acb_set(z, y[1], y[2], prec(base_ring(x)))
end

zero(x::AcbMatSpace) = x()

function one(x::AcbMatSpace)
  z = x()
  ccall((:acb_mat_one, :libarb), Void, (Ptr{acb_mat}, ), &z)
  return z
end

rows(a::acb_mat) = a.r

cols(a::acb_mat) = a.c

function deepcopy_internal(x::acb_mat, dict::ObjectIdDict)
  z = parent(x)()
  ccall((:acb_mat_set, :libarb), Void, (Ptr{acb_mat}, Ptr{acb_mat}), &z, &x)
  return z
end

################################################################################
#
#  String I/O
#
################################################################################

function show(io::IO, a::AcbMatSpace)
   print(io, "Matrix Space of ")
   print(io, a.rows, " rows and ", a.cols, " columns over ")
   print(io, base_ring(a))
end

function show(io::IO, a::acb_mat)
   rows = a.parent.rows
   cols = a.parent.cols
   for i = 1:rows
      print(io, "[")
      for j = 1:cols
         print(io, a[i, j])
         if j != cols
            print(io, " ")
         end
      end
      print(io, "]")
      if i != rows
         println(io, "")
      end
   end
end

################################################################################
#
#  Unary operations
#
################################################################################

function -(x::acb_mat)
  z = parent(x)()
  ccall((:acb_mat_neg, :libarb), Void, (Ptr{acb_mat}, Ptr{acb_mat}), &z, &x)
  return z
end

################################################################################
#
#  Transpose
#
################################################################################

function transpose(x::acb_mat)
  z = MatrixSpace(base_ring(x), cols(x), rows(x))()
  ccall((:acb_mat_transpose, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}), &z, &x)
  return z
end

################################################################################
#
#  Binary operations
#
################################################################################

function +(x::acb_mat, y::acb_mat)
  check_parent(x, y)
  z = parent(x)()
  ccall((:acb_mat_add, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Ptr{acb_mat}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

function -(x::acb_mat, y::acb_mat)
  check_parent(x, y)
  z = parent(x)()
  ccall((:acb_mat_sub, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Ptr{acb_mat}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

function *(x::acb_mat, y::acb_mat)
  cols(x) != rows(y) && error("Matrices have wrong dimensions")
  z = MatrixSpace(base_ring(x), rows(x), cols(y))()
  ccall((:acb_mat_mul, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Ptr{acb_mat}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

################################################################################
#
#   Ad hoc binary operators
#
################################################################################

function ^(x::acb_mat, y::UInt)
  rows(x) != cols(x) && error("Matrix must be square")
  z = parent(x)()
  ccall((:acb_mat_pow_ui, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, UInt, Int),
              &z, &x, y, prec(parent(x)))
  return z
end

function *(x::acb_mat, y::Int)
  z = parent(x)()
  ccall((:acb_mat_scalar_mul_si, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Int, Int),
              &z, &x, y, prec(parent(x)))
  return z
end

*(x::Int, y::acb_mat) = y*x

function *(x::acb_mat, y::fmpz)
  z = parent(x)()
  ccall((:acb_mat_scalar_mul_fmpz, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Ptr{fmpz}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

*(x::fmpz, y::acb_mat) = y*x

function *(x::acb_mat, y::arb)
  z = parent(x)()
  ccall((:acb_mat_scalar_mul_arb, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Ptr{arb}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

*(x::arb, y::acb_mat) = y*x

function *(x::acb_mat, y::acb)
  z = parent(x)()
  ccall((:acb_mat_scalar_mul_acb, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Ptr{acb}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

*(x::acb, y::acb_mat) = y*x

+(x::Integer, y::acb_mat) = parent(y)(x) + y

+(x::acb_mat, y::Integer) = y + x

+(x::fmpz, y::acb_mat) = parent(y)(x) + y

+(x::acb_mat, y::fmpz) = y + x

-(x::Integer, y::acb_mat) = parent(y)(x) - y

-(x::acb_mat, y::Integer) = -(y - x)

-(x::fmpz, y::acb_mat) = parent(y)(x) - y

-(x::acb_mat, y::fmpz) = -(y - x)

###############################################################################
#
#   Shifting
#
###############################################################################

doc"""
    ldexp(x::acb_mat, y::Int)
> Return $2^yx$. Note that $y$ can be positive, zero or negative.
"""
function ldexp(x::acb_mat, y::Int)
  z = parent(x)()
  ccall((:acb_mat_scalar_mul_2exp_si, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Int), &z, &x, y)
  return z
end

###############################################################################
#
#   Comparisons
#
###############################################################################

doc"""
    isequal(x::acb_mat, y::acb_mat)
> Return `true` if the matrices of balls $x$ and $y$ are precisely equal,
> i.e. if all matrix entries have the same midpoints and radii.
"""
function isequal(x::acb_mat, y::acb_mat)
  r = ccall((:acb_mat_equal, :libarb), Cint,
              (Ptr{acb_mat}, Ptr{acb_mat}), &x, &y)
  return Bool(r)
end

function ==(x::acb_mat, y::acb_mat)
  check_parent(x, y)
  r = ccall((:acb_mat_eq, :libarb), Cint, (Ptr{acb_mat}, Ptr{acb_mat}), &x, &y)
  return Bool(r)
end

function !=(x::acb_mat, y::acb_mat)
  r = ccall((:acb_mat_ne, :libarb), Cint, (Ptr{acb_mat}, Ptr{acb_mat}), &x, &y)
  return Bool(r)
end

doc"""
    overlaps(x::acb_mat, y::acb_mat)
> Returns `true` if all entries of $x$ overlap with the corresponding entry of
> $y$, otherwise return `false`.
"""
function overlaps(x::acb_mat, y::acb_mat)
  r = ccall((:acb_mat_overlaps, :libarb), Cint,
              (Ptr{acb_mat}, Ptr{acb_mat}), &x, &y)
  return Bool(r)
end

doc"""
    contains(x::acb_mat, y::acb_mat)
> Returns `true` if all entries of $x$ contain the corresponding entry of
> $y$, otherwise return `false`.
"""
function contains(x::acb_mat, y::acb_mat)
  r = ccall((:acb_mat_contains, :libarb), Cint,
              (Ptr{acb_mat}, Ptr{acb_mat}), &x, &y)
  return Bool(r)
end

################################################################################
#
#  Ad hoc comparisons
#
################################################################################

doc"""
    contains(x::acb_mat, y::fmpz_mat)
> Returns `true` if all entries of $x$ contain the corresponding entry of
> $y$, otherwise return `false`.
"""
function contains(x::acb_mat, y::fmpz_mat)
  r = ccall((:acb_mat_contains_fmpz_mat, :libarb), Cint,
              (Ptr{acb_mat}, Ptr{fmpz_mat}), &x, &y)
  return Bool(r)
end

doc"""
    contains(x::acb_mat, y::fmpq_mat)
> Returns `true` if all entries of $x$ contain the corresponding entry of
> $y$, otherwise return `false`.
"""
function contains(x::acb_mat, y::fmpq_mat)
  r = ccall((:acb_mat_contains_fmpq_mat, :libarb), Cint,
              (Ptr{acb_mat}, Ptr{fmpq_mat}), &x, &y)
  return Bool(r)
end

==(x::acb_mat, y::fmpz_mat) = x == parent(x)(y)

==(x::fmpz_mat, y::acb_mat) = y == x

==(x::acb_mat, y::arb_mat) = x == parent(x)(y)

==(x::arb_mat, y::acb_mat) = y == x

################################################################################
#
#  Predicates
#
################################################################################

doc"""
    isreal(M::acb_mat)
> Returns whether every entry of $M$ has vanishing imaginary part.
"""
isreal(x::acb_mat) =
            Bool(ccall((:acb_mat_is_real, :libarb), Cint, (Ptr{acb_mat}, ), &x))

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
    inv(M::acb_mat)
> Given a $n\times n$ matrix of type `acb_mat`, return an
> $n\times n$ matrix $X$ such that $AX$ contains the 
> identity matrix. If $A$ cannot be inverted numerically an exception is raised.
"""
function inv(x::acb_mat)
  cols(x) != rows(x) && error("Matrix must be square")
  z = parent(x)()
  r = ccall((:acb_mat_inv, :libarb), Cint,
              (Ptr{acb_mat}, Ptr{acb_mat}, Int), &z, &x, prec(parent(x)))
  Bool(r) ? (return z) : error("Matrix cannot be inverted numerically")
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::acb_mat, y::acb_mat)
   cols(x) != cols(y) && error("Incompatible matrix dimensions")
   x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::acb_mat, y::Int)
  y == 0 && throw(DivideError())
  z = parent(x)()
  ccall((:acb_mat_scalar_div_si, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Int, Int),
              &z, &x, y, prec(parent(x)))
  return z
end

function divexact(x::acb_mat, y::fmpz)
  z = parent(x)()
  ccall((:acb_mat_scalar_div_fmpz, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Ptr{fmpz}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

function divexact(x::acb_mat, y::arb)
  z = parent(x)()
  ccall((:acb_mat_scalar_div_arb, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Ptr{arb}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

function divexact(x::acb_mat, y::acb)
  z = parent(x)()
  ccall((:acb_mat_scalar_div_acb, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Ptr{acb}, Int),
              &z, &x, &y, prec(parent(x)))
  return z
end

################################################################################
#
#  Characteristic polynomial
#
################################################################################

function charpoly(x::AcbPolyRing, y::acb_mat)
  base_ring(x) != base_ring(y) && error("Base rings must coincide")
  z = x()
  ccall((:acb_mat_charpoly, :libarb), Void,
              (Ptr{acb_poly}, Ptr{acb_mat}, Int), &z, &y, prec(parent(y)))
  return z
end

################################################################################
#
#  Determinant
#
################################################################################

function det(x::acb_mat)
  cols(x) != rows(x) && error("Matrix must be square")
  z = base_ring(x)()
  ccall((:acb_mat_det, :libarb), Void,
              (Ptr{acb}, Ptr{acb_mat}, Int), &z, &x, prec(parent(x)))
  return z
end

################################################################################
#
#  Exponential function
#
################################################################################

doc"""
    exp(x::acb_mat)
> Returns the exponential of the matrix $x$.
"""
function exp(x::acb_mat)
  cols(x) != rows(x) && error("Matrix must be square")
  z = parent(x)()
  ccall((:acb_mat_exp, :libarb), Void,
              (Ptr{acb_mat}, Ptr{acb_mat}, Int), &z, &x, prec(parent(x)))
  return z
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function lufact!(P::perm, x::acb_mat)
  r = ccall((:acb_mat_lu, :libarb), Cint,
              (Ptr{Int}, Ptr{acb_mat}, Ptr{acb_mat}, Int),
              P.d, &x, &x, prec(parent(x)))
  r == 0 && error("Could not find $(rows(x)) invertible pivot elements")
  inv!(P)
  return rows(x)
end

function lufact(P::perm, x::acb_mat)
  cols(x) != rows(x) && error("Matrix must be square")
  parent(P).n != rows(x) && error("Permutation does not match matrix")
  R = base_ring(x)
  L = parent(x)()
  U = deepcopy(x)
  n = cols(x)
  lufact!(P, U)
  for i = 1:n
    for j = 1:n
      if i > j
        L[i, j] = U[i, j]
        U[i, j] = R()
      elseif i == j
        L[i, j] = one(R)
      else
        L[i, j] = R()
      end
    end
  end
  return L, U
end

function solve!(z::acb_mat, x::acb_mat, y::acb_mat)
  r = ccall((:acb_mat_solve, :libarb), Cint,
              (Ptr{acb_mat}, Ptr{acb_mat}, Ptr{acb_mat}, Int),
              &z, &x, &y, prec(parent(x)))
  r == 0 && error("Matrix cannot be inverted numerically")
  nothing
end

function solve(x::acb_mat, y::acb_mat)
  cols(x) != rows(x) && error("First argument must be square")
  cols(x) != rows(y) && error("Matrix dimensions are wrong")
  z = parent(y)()
  solve!(z, x, y)
  return z
end

function solve_lu_precomp!(z::acb_mat, P::perm, LU::acb_mat, y::acb_mat)
  Q = inv(P)
  ccall((:acb_mat_solve_lu_precomp, :libarb), Void,
              (Ptr{acb_mat}, Ptr{Int}, Ptr{acb_mat}, Ptr{acb_mat}, Int),
              &z, Q.d, &LU, &y, prec(parent(LU)))
  nothing
end

function solve_lu_precomp(P::perm, LU::acb_mat, y::acb_mat)
  cols(LU) != rows(y) && error("Matrix dimensions are wrong")
  z = parent(y)()
  solve_lu_precomp!(z, P, LU, y)
  return z
end

################################################################################
#
#   Row swapping
#
################################################################################

function swap_rows(x::acb_mat, i::Int, j::Int)
  _checkbounds(rows(x), i) || throw(BoundsError())
  _checkbounds(rows(x), j) || throw(BoundsError())
  z = deepcopy(x)
  swap_rows!(z, i, j)
  return z
end

function swap_rows!(x::acb_mat, i::Int, j::Int)
  ccall((:acb_mat_swap_rows, :libarb), Void,
              (Ptr{acb_mat}, Ptr{Void}, Int, Int),
              &x, C_NULL, i - 1, j - 1)
end

################################################################################
#
#   Norm
#
################################################################################

doc"""
    bound_inf_norm(x::acb_mat)
> Returns a nonnegative element $z$ of type `acb`, such that $z$ is an upper
> bound for the infinity norm for every matrix in $x$
"""
function bound_inf_norm(x::acb_mat)
  z = arb()
  t = ccall((:arb_rad_ptr, :libarb), Ptr{mag_struct}, (Ptr{arb}, ), &z)
  ccall((:acb_mat_bound_inf_norm, :libarb), Void,
              (Ptr{mag_struct}, Ptr{acb_mat}), t, &x)
  s = ccall((:arb_mid_ptr, :libarb), Ptr{arf_struct}, (Ptr{arb}, ), &z)
  ccall((:arf_set_mag, :libarb), Void,
              (Ptr{arf_struct}, Ptr{mag_struct}), s, t)
  ccall((:mag_zero, :libarb), Void,
              (Ptr{mag_struct},), t)
  return ArbField(prec(base_ring(x)))(z)
end

################################################################################
#
#   Unsafe functions
#
################################################################################

for (s,f) in (("add!","acb_mat_add"), ("mul!","acb_mat_mul"),
              ("sub!","acb_mat_sub"))
  @eval begin
    function ($(Symbol(s)))(z::acb_mat, x::acb_mat, y::acb_mat)
      ccall(($f, :libarb), Void,
                  (Ptr{acb_mat}, Ptr{acb_mat}, Ptr{acb_mat}, Int),
                  &z, &x, &y, prec(parent(x)))
    end
  end
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (x::AcbMatSpace)()
  z = acb_mat(x.rows, x.cols)
  z.parent = x
  return z
end

function (x::AcbMatSpace)(y::fmpz_mat)
  (x.cols != cols(y) || x.rows != rows(y)) &&
      error("Dimensions are wrong")
  z = acb_mat(y, prec(x))
  z.parent = x
  return z
end

function (x::AcbMatSpace)(y::arb_mat)
  (x.cols != cols(y) || x.rows != rows(y)) &&
      error("Dimensions are wrong")
  z = acb_mat(y, prec(x))
  z.parent = x
  return z
end


function (x::AcbMatSpace){T <: Union{Int, UInt, Float64, fmpz, fmpq, BigFloat, arb, acb,
                         AbstractString}}(y::Array{T, 2})
  _check_dim(x.rows, x.cols, y)
  z = acb_mat(x.rows, x.cols, y, prec(x))
  z.parent = x
  return z
end

function (x::AcbMatSpace){T <: Union{Int, UInt, Float64, fmpz, fmpq, BigFloat, arb, acb,
                         AbstractString}}(y::Array{T, 1})
  _check_dim(x.rows, x.cols, y)
  z = acb_mat(x.rows, x.cols, y, prec(x))
  z.parent = x
  return z
end

function (x::AcbMatSpace){T <: Union{Int, UInt, Float64, fmpz, fmpq, BigFloat, arb,
                         AbstractString}}(y::Array{Tuple{T, T}, 2})
  _check_dim(x.rows, x.cols, y)
  z = acb_mat(x.rows, x.cols, y, prec(x))
  z.parent = x
  return z
end

function (x::AcbMatSpace){T <: Union{Int, UInt, Float64, fmpz, fmpq}}(y::Array{Tuple{T, T}, 1})
  _check_dim(x.rows, x.cols, y)
  z = acb_mat(x.rows, x.cols, y, prec(x))
  z.parent = x
  return z
end

function (x::AcbMatSpace){T <: Union{BigFloat, arb, AbstractString}}(y::Array{Tuple{T, T}, 1})
  _check_dim(x.rows, x.cols, y)
  z = acb_mat(x.rows, x.cols, y, prec(x))
  z.parent = x
  return z
end

function (x::AcbMatSpace)(y::Union{Int, UInt, fmpz, fmpq, Float64,
                          BigFloat, arb, acb, AbstractString})
  z = x()
  for i in 1:rows(z)
      for j = 1:cols(z)
         if i != j
            z[i, j] = zero(base_ring(x))
         else
            z[i, j] = y
         end
      end
   end
   return z
end

(x::AcbMatSpace)(y::acb_mat) = y

###############################################################################
#
#   Promotions
#
###############################################################################

Base.promote_rule{T <: Integer}(::Type{acb_mat}, ::Type{T}) = acb_mat

Base.promote_rule(::Type{acb_mat}, ::Type{fmpz}) = acb_mat

Base.promote_rule(::Type{acb_mat}, ::Type{fmpq}) = acb_mat

Base.promote_rule(::Type{acb_mat}, ::Type{arb}) = acb_mat

Base.promote_rule(::Type{acb_mat}, ::Type{acb}) = acb_mat

Base.promote_rule(::Type{acb_mat}, ::Type{fmpz_mat}) = acb_mat

Base.promote_rule(::Type{acb_mat}, ::Type{fmpq_mat}) = acb_mat

Base.promote_rule(::Type{acb_mat}, ::Type{arb_mat}) = acb_mat

###############################################################################
#
#   MatrixSpace constructor
#
###############################################################################

function MatrixSpace(R::AcbField, r::Int, c::Int; cached = true)
  (r <= 0 || c <= 0) && error("Dimensions must be positive")
  return AcbMatSpace(R, r, c, cached)
end
