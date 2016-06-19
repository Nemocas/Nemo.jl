###############################################################################
#
#   fmpq_mat.jl : Flint matrices over the rationals
#
###############################################################################

export fmpq_mat, FmpqMatSpace, gso, hilbert

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

elem_type(::FmpqMatSpace) = fmpq_mat

base_ring(a::FmpqMatSpace) = a.base_ring

parent(a::fmpq_mat) = a.parent

function check_parent(a::fmpq_mat, b::fmpq_mat)
   parent(a) != parent(b) && error("Incompatible matrices")
end

###############################################################################
#
#   Windows - handle with care!!!
#
###############################################################################

function window(x::fmpq_mat, r1::Int, c1::Int, r2::Int, c2::Int)
  _checkbounds(x.r, r1) || throw(BoundsError())
  _checkbounds(x.r, r2) || throw(BoundsError())
  _checkbounds(x.c, c1) || throw(BoundsError())
  _checkbounds(x.c, c2) || throw(BoundsError())
  (r1 > r2 || c1 > c2) && error("Invalid parameters")
  b = fmpq_mat()
  b.parent = MatrixSpace(parent(x).base_ring, r2 - r1 + 1, c2 - c1 + 1)
  ccall((:fmpq_mat_window_init, :libflint), Void,
        (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Int, Int, Int, Int),
            &b, &x, r1-1, c1-1, r2, c2)
  finalizer(b, _fmpq_mat_window_clear_fn)
  return b
end

function window(x::fmpq_mat, r::UnitRange{Int}, c::UnitRange{Int})
  return window(x, r.start, c.start, r.stop, c.stop)
end

function _fmpq_mat_window_clear_fn(a::fmpq_mat)
   ccall((:fmpq_mat_window_clear, :libflint), Void, (Ptr{fmpq_mat},), &a)
end

size(x::fmpq_mat) = tuple(x.parent.rows, x.parent.cols)

size(t::fmpq_mat, d) = d <= 2 ? size(t)[d] : 1

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function getindex!(v::fmpq, a::fmpq_mat, r::Int, c::Int)
   z = ccall((:fmpq_mat_entry, :libflint), Ptr{fmpq},
             (Ptr{fmpq_mat}, Int, Int), &a, r - 1, c - 1)
   ccall((:fmpq_set, :libflint), Void, (Ptr{fmpq}, Ptr{fmpq}), &v, z)
end

function getindex(a::fmpq_mat, r::Int, c::Int)
   _checkbounds(a.parent.rows, r) || throw(BoundsError())
   _checkbounds(a.parent.cols, c) || throw(BoundsError())
   v = fmpq()
   z = ccall((:fmpq_mat_entry, :libflint), Ptr{fmpq},
             (Ptr{fmpq_mat}, Int, Int), &a, r - 1, c - 1)
   ccall((:fmpq_set, :libflint), Void, (Ptr{fmpq}, Ptr{fmpq}), &v, z)
   return v
end

function setindex!(a::fmpq_mat, d::fmpz, r::Int, c::Int)
   z = ccall((:fmpq_mat_entry_num, :libflint), Ptr{fmpz},
             (Ptr{fmpq_mat}, Int, Int), &a, r - 1, c - 1)
   ccall((:fmpz_set, :libflint), Void, (Ptr{fmpz}, Ptr{fmpz}), z, &d)
   z = ccall((:fmpq_mat_entry_den, :libflint), Ptr{fmpz},
             (Ptr{fmpq_mat}, Int, Int), &a, r - 1, c - 1)
   ccall((:fmpz_set_si, :libflint), Void, (Ptr{fmpz}, Int), z, 1)
end

function setindex!(a::fmpq_mat, d::fmpq, r::Int, c::Int)
   z = ccall((:fmpq_mat_entry, :libflint), Ptr{fmpq},
             (Ptr{fmpq_mat}, Int, Int), &a, r - 1, c - 1)
   ccall((:fmpq_set, :libflint), Void, (Ptr{fmpq}, Ptr{fmpq}), z, &d)
end

setindex!(a::fmpq_mat, d::Integer, r::Int, c::Int) = setindex!(a, fmpq(d), r, c)

function setindex!(a::fmpq_mat, d::Int, r::Int, c::Int)
   _checkbounds(a.parent.rows, r) || throw(BoundsError())
   _checkbounds(a.parent.cols, c) || throw(BoundsError())
   z = ccall((:fmpq_mat_entry, :libflint), Ptr{fmpq},
             (Ptr{fmpq_mat}, Int, Int), &a, r - 1, c - 1)
   ccall((:fmpq_set_si, :libflint), Void, (Ptr{fmpq}, Int), z, d)
end

rows(a::fmpq_mat) = a.r

cols(a::fmpq_mat) = a.c

zero(a::FmpqMatSpace) = a()

one(a::FmpqMatSpace) = a(1)

iszero(a::fmpq_mat) = ccall((:fmpq_mat_is_zero, :libflint), Bool,
                            (Ptr{fmpq_mat},), &a)

isone(a::fmpq_mat) = ccall((:fmpq_mat_is_one, :libflint), Bool,
                           (Ptr{fmpq_mat},), &a)

function deepcopy(d::fmpq_mat)
   z = fmpq_mat(d)
   z.parent = d.parent
   return z
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

canonical_unit(a::fmpq_mat) = canonical_unit(a[1, 1])

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, a::FmpqMatSpace)
   print(io, "Matrix Space of ")
   print(io, a.rows, " rows and ", a.cols, " columns over ")
   print(io, "Rational Field")
end

function show(io::IO, a::fmpq_mat)
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

show_minus_one(::Type{fmpq_mat}) = show_minus_one(fmpq)

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::fmpq_mat)
   z = parent(x)()
   ccall((:fmpq_mat_neg, :libflint), Void,
         (Ptr{fmpq_mat}, Ptr{fmpq_mat}), &z, &x)
   return z
end

###############################################################################
#
#   transpose
#
###############################################################################

function transpose(x::fmpq_mat)
   z = MatrixSpace(FlintQQ, cols(x), rows(x))()
   ccall((:fmpq_mat_transpose, :libflint), Void,
         (Ptr{fmpq_mat}, Ptr{fmpq_mat}), &z, &x)
   return z
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::fmpq_mat, y::fmpq_mat)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpq_mat_add, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat},  Ptr{fmpq_mat}),
               &z, &x, &y)
   return z
end

function -(x::fmpq_mat, y::fmpq_mat)
   check_parent(x, y)
   z = parent(x)()
   ccall((:fmpq_mat_sub, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat},  Ptr{fmpq_mat}),
               &z, &x, &y)
   return z
end

function *(x::fmpq_mat, y::fmpq_mat)
   cols(x) != rows(y) && error("Incompatible matrix dimensions")
   if rows(x) == cols(y) && rows(x) == cols(x)
      parz = parent(x)
   else
      parz = FmpqMatSpace(rows(x), cols(y))
   end
   z = parz()
   ccall((:fmpq_mat_mul, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat},  Ptr{fmpq_mat}),
               &z, &x, &y)
   return z
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::fmpz, y::fmpq_mat)
   z = parent(y)()
   ccall((:fmpq_mat_scalar_mul_fmpz, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpz}), &z, &y, &x)
   return z
end

function *(x::fmpq, y::fmpq_mat)
   z = parent(y)()
   ccall((:fmpq_mat_scalar_mul_fmpz, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpq}), &z, &y, &num(x))
   ccall((:fmpq_mat_scalar_div_fmpz, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpq}), &z, &z, &den(x))
   return z
end

*(x::fmpq_mat, y::fmpq) = y*x

*(x::fmpq_mat, y::fmpz) = y*x

*(x::Integer, y::fmpq_mat) = fmpz(x)*y

*(x::fmpq_mat, y::Integer) = fmpz(y)*x

function +(x::fmpq_mat, y::Integer)
   z = deepcopy(x)
   for i = 1:min(rows(x), cols(x))
      z[i, i] += y
   end
   return z
end

+(x::Integer, y::fmpq_mat) = y + x

+(x::fmpz, y::fmpq_mat) = parent(y)(x) + y

+(x::fmpq_mat, y::fmpz) = x + parent(x)(y)

+(x::fmpq, y::fmpq_mat) = parent(y)(x) + y

+(x::fmpq_mat, y::fmpq) = x + parent(x)(y)

function -(x::fmpq_mat, y::Integer)
   z = deepcopy(x)
   for i = 1:min(rows(x), cols(x))
      z[i, i] -= y
   end
   return z
end

function -(x::Integer, y::fmpq_mat)
   z = -y
   for i = 1:min(rows(y), cols(y))
      z[i, i] += x
   end
   return z
end

-(x::fmpz, y::fmpq_mat) = parent(y)(x) - y

-(x::fmpq_mat, y::fmpz) = x - parent(x)(y)

-(x::fmpq, y::fmpq_mat) = parent(y)(x) - y

-(x::fmpq_mat, y::fmpq) = x - parent(x)(y)

###############################################################################
#
#   Comparisons
#
###############################################################################

function ==(x::fmpq_mat, y::fmpq_mat)
   check_parent(x, y)
   ccall((:fmpq_mat_equal, :libflint), Bool,
                                       (Ptr{fmpq_mat}, Ptr{fmpq_mat}), &x, &y)
end

###############################################################################
#
#   Ad hoc comparisons
#
###############################################################################

function ==(x::fmpq_mat, y::Integer)
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

==(x::Integer, y::fmpq_mat) = y == x

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::fmpq_mat)
   z = parent(x)()
   success = ccall((:fmpq_mat_inv, :libflint), Cint,
         (Ptr{fmpq_mat}, Ptr{fmpq_mat}), &z, &x)
   success == 0 && error("Matrix not invertible")
   return z
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::fmpq_mat, y::fmpq_mat)
   cols(x) != cols(y) && error("Incompatible matrix dimensions")
   x*inv(y)
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(x::fmpq_mat, y::fmpq)
   z = parent(x)()
   ccall((:fmpq_mat_scalar_div_fmpz, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpz}), &z, &x, &num(y))
   ccall((:fmpq_mat_scalar_mul_fmpz, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpz}), &z, &z, &den(y))
   return z
end

function divexact(x::fmpq_mat, y::fmpz)
   z = parent(x)()
   ccall((:fmpq_mat_scalar_div_fmpz, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpz}), &z, &x, &y)
   return z
end

divexact(x::fmpq_mat, y::Integer) = divexact(x, fmpz(y))

###############################################################################
#
#   Characteristic polynomial
#
###############################################################################

function charpoly(R::FmpqPolyRing, x::fmpq_mat)
   rows(x) != cols(x) && error("Non-square")
   z = R()
   ccall((:fmpq_mat_charpoly, :libflint), Void,
                (Ptr{fmpq_poly}, Ptr{fmpq_mat}), &z, &x)
   return z
end

###############################################################################
#
#   Minimal polynomial
#
###############################################################################

function minpoly(R::FmpqPolyRing, x::fmpq_mat)
   rows(x) != cols(x) && error("Non-square")
   z = R()
   ccall((:fmpq_mat_minpoly, :libflint), Void,
                (Ptr{fmpq_poly}, Ptr{fmpq_mat}), &z, &x)
   return z
end

###############################################################################
#
#   Determinant
#
###############################################################################

function det(x::fmpq_mat)
   rows(x) != cols(x) && error("Non-square matrix")
   z = fmpq()
   ccall((:fmpq_mat_det, :libflint), Void,
                (Ptr{fmpq}, Ptr{fmpq_mat}), &z, &x)
   return z
end

###############################################################################
#
#   Gram-Schmidt orthogonalisation
#
###############################################################################

doc"""
    gso(x::fmpq_mat)
> Return the Gram-Schmidt Orthogonalisation of the matrix $x$.
"""
function gso(x::fmpq_mat)
   z = parent(x)()
   ccall((:fmpq_mat_gso, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat}), &z, &x)
   return z
end

###############################################################################
#
#   Hilbert matrix
#
###############################################################################

doc"""
    hilbert(R::FmpqMatSpace)
> Return the Hilbert matrix in the given matrix space. This is the matrix with
> entries $H_{i,j} = 1/(i + j - 1)$.
"""
function hilbert(R::FmpqMatSpace)
   z = R()
   ccall((:fmpq_mat_hilbert_matrix, :libflint), Bool,
                   (Ptr{fmpq_mat},), &z)
   return z
end

###############################################################################
#
#   Rank
#
###############################################################################

function rank(x::fmpq_mat)
   z = parent(x)()
   r = ccall((:fmpq_mat_rref, :libflint), Int,
         (Ptr{fmpq_mat}, Ptr{fmpq_mat}), &z, &x)
   return r
end

###############################################################################
#
#   Reduced row echelon form
#
###############################################################################

function rref(x::fmpq_mat)
   z = parent(x)()
   r = ccall((:fmpq_mat_rref, :libflint), Int,
         (Ptr{fmpq_mat}, Ptr{fmpq_mat}), &z, &x)
   return r, z
end

###############################################################################
#
#   Linear solving
#
###############################################################################

function solve(a::fmpq_mat, b::fmpq_mat)
   rows(a) != cols(a) && error("Not a square matrix in solve")
   rows(b) != rows(a) && error("Incompatible dimensions in solve")
   z = parent(b)()
   nonsing = ccall((:fmpq_mat_solve_fraction_free, :libflint), Bool,
      (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpq_mat}), &z, &a, &b)
   !nonsing && error("Singular matrix in solve")
   return z
end

doc"""
    solve_dixon(a::fmpq_mat, b::fmpq_mat)
> Solve $ax = b$ by clearing denominators and using Dixon's algorithm. This is
> usually faster for large systems.
"""
function solve_dixon(a::fmpq_mat, b::fmpq_mat)
   rows(a) != cols(a) && error("Not a square matrix in solve")
   rows(b) != rows(a) && error("Incompatible dimensions in solve")
   z = parent(b)()
   nonsing = ccall((:fmpq_mat_solve_dixon, :libflint), Bool,
      (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpq_mat}), &z, &a, &b)
   !nonsing && error("Singular matrix in solve")
   return z
end

###############################################################################
#
#   Trace
#
###############################################################################

function trace(x::fmpq_mat)
   rows(x) != cols(x) && error("Not a square matrix in trace")
   d = fmpq()
   ccall((:fmpq_mat_trace, :libflint), Void,
                (Ptr{fmpq}, Ptr{fmpq_mat}), &d, &x)
   return d
end

###############################################################################
#
#   Concatenation
#
###############################################################################

function hcat(a::fmpq_mat, b::fmpq_mat)
  rows(a) != rows(b) && error("Incompatible number of rows in hcat")
  c = MatrixSpace(FlintQQ, rows(a), cols(a) + cols(b))()
  ccall((:fmpq_mat_concat_horizontal, :libflint), Void,
        (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpq_mat}), &c, &a, &b)
  return c
end

function vcat(a::fmpq_mat, b::fmpq_mat)
  cols(a) != cols(b) && error("Incompatible number of columns in vcat")
  c = MatrixSpace(FlintQQ, rows(a) + rows(b), cols(a))()
  ccall((:fmpq_mat_concat_vertical, :libflint), Void,
        (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpq_mat}), &c, &a, &b)
  return c
end

###############################################################################
#
#   Similarity
#
###############################################################################

function similarity!(z::fmpq_mat, r::Int, d::fmpq)
   ccall((:fmpq_mat_similarity, :libflint), Void, 
         (Ptr{fmpq_mat}, Int, Ptr{fmpq}), &z, r - 1, &d)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function mul!(z::fmpq_mat, x::fmpq_mat, y::fmpq_mat)
   ccall((:fmpq_mat_mul, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpq_mat}), &z, &x, &y)
end

function mul!(y::fmpq_mat, x::Int)
   ccall((:fmpq_mat_scalar_mul_fmpz, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpq}), &y, &y, &fmpz(x))
   return y
end

function mul!(y::fmpq_mat, x::fmpz)
   ccall((:fmpq_mat_scalar_mul_fmpz, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpz}), &y, &y, &x)
   return y
end

function addeq!(z::fmpq_mat, x::fmpq_mat)
   ccall((:fmpq_mat_add, :libflint), Void,
                (Ptr{fmpq_mat}, Ptr{fmpq_mat}, Ptr{fmpq_mat}), &z, &z, &x)
end

function zero!(z::fmpq_mat)
   ccall((:fmpq_mat_zero, :libflint), Void,
                (Ptr{fmpq_mat},), &z)
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function Base.call(a::FmpqMatSpace)
   z = fmpq_mat(a.rows, a.cols)
   z.parent = a
   return z
end

function Base.call(a::FmpqMatSpace, arr::Array{fmpq, 2})
   z = fmpq_mat(a.rows, a.cols, arr)
   z.parent = a
   return z
end

function Base.call{T <: Integer}(a::FmpqMatSpace, arr::Array{T, 2})
   z = fmpq_mat(a.rows, a.cols, arr)
   z.parent = a
   return z
end

function Base.call(a::FmpqMatSpace, arr::Array{fmpq, 1})
   z = fmpq_mat(a.rows, a.cols, arr)
   z.parent = a
   return z
end

Base.call{T <: Integer}(a::FmpqMatSpace, arr::Array{T, 1}) = a(arr'')

function Base.call(a::FmpqMatSpace, d::fmpq)
   z = fmpq_mat(a.rows, a.cols, d)
   z.parent = a
   return z
end

function Base.call(a::FmpqMatSpace, d::fmpz)
   z = fmpq_mat(a.rows, a.cols, fmpq(d))
   z.parent = a
   return z
end

function Base.call(a::FmpqMatSpace, d::Integer)
   z = fmpq_mat(a.rows, a.cols, fmpq(d))
   z.parent = a
   return z
end

Base.call(a::FmpqMatSpace, d::fmpq_mat) = d

###############################################################################
#
#   Promotions
#
###############################################################################

Base.promote_rule{T <: Integer}(::Type{fmpq_mat}, ::Type{T}) = fmpq_mat

Base.promote_rule(::Type{fmpq_mat}, ::Type{fmpq}) = fmpq_mat

Base.promote_rule(::Type{fmpq_mat}, ::Type{fmpz}) = fmpq_mat

###############################################################################
#
#   MatrixSpace constructor
#
###############################################################################

function MatrixSpace(R::FlintRationalField, r::Int, c::Int)
   return FmpqMatSpace(r, c)
end
