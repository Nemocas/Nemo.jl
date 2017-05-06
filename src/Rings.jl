###############################################################################
#
#   Rings.jl : Generic rings
#
###############################################################################

function isequal(a::RingElem, b::RingElem)
   return parent(a) == parent(b) && a == b
end

###############################################################################
#
#   Generic catchall functions
#
###############################################################################

promote_rule1{T <: RingElem, U <: RingElem}(::Type{T}, ::Type{U}) = Base.promote_rule(T, U)

function +{S <: RingElem, T <: RingElem}(x::S, y::T) 
   T1 = Base.promote_rule(S, T)
   if S == T1
      +(x, parent(x)(y))
   elseif T == T1
      +(parent(y)(x), y)
   else
      T1 = Base.promote_rule(T, S)
      if S == T1
         +(x, parent(x)(y))
      elseif T == T1
         +(parent(y)(x), y)
      else
         error("Unable to promote ", S, " and ", T, " to common type")
      end
   end
end

function +{S <: RingElem, T <: Integer}(x::S, y::T) 
   T1 = Base.promote_rule(S, T)
   if S == T1
      +(x, parent(x)(y))
   else
      error("Unable to promote ", S, " and ", T, " to common type")
   end
end

function +{S <: Integer, T <: RingElem}(x::S, y::T) 
   T1 = Base.promote_rule(T, S)
   if T == T1
      +(parent(y)(x), y)
   else
      error("Unable to promote ", S, " and ", T, " to common type")
   end
end

function -{S <: RingElem, T <: RingElem}(x::S, y::T) 
   T1 = Base.promote_rule(S, T)
   if S == T1
      -(x, parent(x)(y))
   elseif T == T1
      -(parent(y)(x), y)
   else
      T1 = Base.promote_rule(T, S)
      if S == T1
         -(x, parent(x)(y))
      elseif T == T1
         -(parent(y)(x), y)
      else
         error("Unable to promote ", S, " and ", T, " to common type")
      end
   end
end

function -{S <: RingElem, T <: Integer}(x::S, y::T) 
   T1 = Base.promote_rule(S, T)
   if S == T1
      -(x, parent(x)(y))
   else
      error("Unable to promote ", S, " and ", T, " to common type")
   end
end

function -{S <: Integer, T <: RingElem}(x::S, y::T) 
   T1 = Base.promote_rule(T, S)
   if T == T1
      -(parent(y)(x), y)
   else
      error("Unable to promote ", S, " and ", T, " to common type")
   end
end

function *{S <: RingElem, T <: RingElem}(x::S, y::T) 
   T1 = Base.promote_rule(S, T)
   if S == T1
      *(x, parent(x)(y))
   elseif T == T1
      *(parent(y)(x), y)
   else
      T1 = Base.promote_rule(T, S)
      if S == T1
         *(x, parent(x)(y))
      elseif T == T1
         *(parent(y)(x), y)
      else
         error("Unable to promote ", S, " and ", T, " to common type")
      end
   end
end

function *{S <: RingElem, T <: Integer}(x::S, y::T) 
   T1 = Base.promote_rule(S, T)
   if S == T1
      *(x, parent(x)(y))
   else
      error("Unable to promote ", S, " and ", T, " to common type")
   end
end

function *{S <: Integer, T <: RingElem}(x::S, y::T) 
   T1 = Base.promote_rule(T, S)
   if T == T1
      *(parent(y)(x), y)
   else
      error("Unable to promote ", S, " and ", T, " to common type")
   end
end

function divexact{S <: RingElem, T <: RingElem}(x::S, y::T) 
   T1 = Base.promote_rule(S, T)
   if S == T1
      divexact(x, parent(x)(y))
   elseif T == T1
      divexact(parent(y)(x), y)
   else
      T1 = Base.promote_rule(T, S)
      if S == T1
         divexact(x, parent(x)(y))
      elseif T == T1
         divexact(parent(y)(x), y)
      else
         error("Unable to promote ", S, " and ", T, " to common type")
      end
   end
end

function divexact{S <: RingElem, T <: Integer}(x::S, y::T) 
   T1 = Base.promote_rule(S, T)
   if S == T1
      divexact(x, parent(x)(y))
   else
      error("Unable to promote ", S, " and ", T, " to common type")
   end
end

function divexact{S <: Integer, T <: RingElem}(x::S, y::T) 
   T1 = Base.promote_rule(T, S)
   if T == T1
      divexact(parent(y)(x), y)
   else
      error("Unable to promote ", S, " and ", T, " to common type")
   end
end

function divides{T <: RingElem}(x::T, y::T)
   q, r = divrem(x, y)
   return r == 0, q
end

function =={S <: RingElem, T <: RingElem}(x::S, y::T) 
   T1 = Base.promote_rule(S, T)
   if S == T1
      ==(x, parent(x)(y))
   elseif T == T1
      ==(parent(y)(x), y)
   else
      T1 = Base.promote_rule(T, S)
      if S == T1
         ==(x, parent(x)(y))
      elseif T == T1
         ==(parent(y)(x), y)
      else
         error("Unable to promote ", S, " and ", T, " to common type")
      end
   end
end

function =={S <: RingElem, T <: Integer}(x::S, y::T) 
   T1 = Base.promote_rule(S, T)
   if S == T1
      ==(x, parent(x)(y))
   else
      error("Unable to promote ", S, " and ", T, " to common type")
   end
end

function =={S <: Integer, T <: RingElem}(x::S, y::T) 
   T1 = Base.promote_rule(T, S)
   if T == T1
      ==(parent(y)(x), y)
   else
      error("Unable to promote ", S, " and ", T, " to common type")
   end
end

function addmul!{T <: RingElem}(z::T, x::T, y::T, c::T)
   mul!(c, x, y)
   addeq!(z, c)
   return
end

###############################################################################
#
#   Baby-steps giant-steps powering
#
###############################################################################

function powers{T <: RingElem}(a::T, d::Int)
   d <= 0 && throw(DomainError())
   S = parent(a)
   A = Array{T}(d + 1)
   A[1] = one(S)
   if d > 1
      c = a
      A[2] = a
      for i = 2:d
         c *= a
         A[i + 1] = c
      end
   end
   return A
end

###############################################################################
#
#   Exponential function for generic rings
#
###############################################################################

function exp{T <: RingElem}(a::T)
   a != 0 && error("Exponential of nonzero element")
   return one(parent(a))
end

################################################################################
#
#   Transpose for ring elements
#
################################################################################

transpose{T <: RingElem}(x::T) = deepcopy(x)

###############################################################################
#
#   Generic and specific rings and fields
#
###############################################################################

include("flint/fmpz.jl")

include("generic/Residue.jl")

include("generic/Poly.jl")

include("flint/fmpz_poly.jl")

include("flint/nmod_poly.jl")

include("flint/fmpz_mod_poly.jl")

#include("flint/fmpz_mpoly.jl")

include("generic/MPoly.jl")

include("generic/SparsePoly.jl")

include("generic/RelSeries.jl")

include("generic/AbsSeries.jl")

include("flint/fmpz_rel_series.jl")

include("flint/fmpz_abs_series.jl")

include("flint/fmpz_mod_rel_series.jl")

include("flint/fmpz_mod_abs_series.jl")

include("generic/Matrix.jl")

include("flint/fmpz_mat.jl")

include("flint/fmpq_mat.jl")

include("flint/nmod_mat.jl")

include("Fields.jl")

include("flint/fmpq_poly.jl")

include("flint/padic.jl")

include("flint/fmpq_rel_series.jl")

include("flint/fmpq_abs_series.jl")

include("flint/fq_rel_series.jl")

include("flint/fq_abs_series.jl")

include("flint/fq_nmod_rel_series.jl")

include("flint/fq_nmod_abs_series.jl")

include("flint/fq_poly.jl")

include("flint/fq_nmod_poly.jl")

include("arb/arb_poly.jl")

include("arb/acb_poly.jl")

include("arb/arb_mat.jl")

include("arb/acb_mat.jl")

include("Factor.jl")

###############################################################################
#
#   Generic functions to be defined after all rings
#
###############################################################################

if VERSION >= v"0.5.0-dev+3171"

include("polysubst.jl")

end
