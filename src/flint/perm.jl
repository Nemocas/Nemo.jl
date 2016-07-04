###############################################################################
#
#   perm.jl : Flint permutation type
#
###############################################################################

export FlintPermGroup, perm, parity

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{perm}) = FlintPermGroup

elem_type(R::FlintPermGroup) = perm

###############################################################################
#
#   Basic manipulation
#
###############################################################################

doc"""
    parent(a::perm)
> Return the parent of the given permutation group element.
"""
parent(a::perm) = a.parent

function deepcopy(a::perm)
   R = parent(a)
   p = R()
   ccall((:_perm_set, :libflint), Void, 
         (Ref{Int}, Ref{Int}, Int), p.d, a.d, R.n)
   return p
end

function Base.hash(a::perm, h::UInt)
   b = 0x595dee0e71d271d0
   for i in 1:a.d
         b $= hash(a[i - 1], h) $ h
         b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

doc"""
    parity(a::perm)
> Return the parity of the given permutation, i.e. the parity of the number of
> transpositions that compose it. The function returns $1$ if the parity is odd
> otherwise it returns $0$.
"""
function parity(a::perm)
   R = parent(a)
   return Int(ccall((:_perm_parity, :libflint), Cint, 
                    (Ref{Int}, Int), a.d, R.n))
end

function getindex(a::perm, n::Int)
   return a.d[n] + 1
end
 
function setindex!(a::perm, d::Int, n::Int)
   a.d[n] = d - 1
end

doc"""
    eye(R::FlintPermGroup)
> Return the identity permutation for the given permutation group.
"""
eye(R::FlintPermGroup) = R()

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, R::FlintPermGroup)
   print(io, "Permutation group over ")
   print(io, R.n)
   print(io, " elements")
end

function show(io::IO, x::perm)
   print(io, "[")
   n = parent(x).n
   for i = 1:n
      print(io, x[i])
      if i != n
         print(io, ", ")
      end
   end
   print(io, "]")
end

###############################################################################
#
#   Comparison
#
###############################################################################

doc"""
    ==(a::perm, b::perm)
> Return `true` if the given permutations are equal, otherwise return `false`.
"""
function ==(a::perm, b::perm)
   R = parent(a)
   return Bool(ccall((:_perm_equal, :libflint), Cint, 
         (Ref{Int}, Ref{Int}, Int), a.d, b.d, R.n))
end

###############################################################################
#
#   Binary operators
#
###############################################################################

doc"""
> Return the composition of the two permutations, i.e. $a\circ b$. In other
> words, the permutation corresponding to applying $b$ first, then $a$, is
> returned.
"""
function *(a::perm, b::perm)
   R = parent(a)
   p = R()
   ccall((:_perm_compose, :libflint), Void, 
         (Ref{Int}, Ref{Int}, Ref{Int}, Int), p.d, a.d, b.d, R.n)
   return p
end

###############################################################################
#
#   Inversion
#
###############################################################################

doc"""
    inv(a::perm)
> Return the inverse of the given permutation, i.e. the permuation $a^{-1}$
> such that $a\circ a^{-1} = a^{-1}\circ a$ is the identity permutation.
"""
function inv(a::perm)
   R = parent(a)
   p = R()
   ccall((:_perm_inv, :libflint), Void, 
         (Ref{Int}, Ref{Int}, Int), p.d, a.d, R.n)
   return p
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function Base.call(R::FlintPermGroup)
   z = perm(R.n)
   z.parent = R
   return z
end

function Base.call(R::FlintPermGroup, a::Array{Int, 1})
   length(a) != R.n && error("Unable to coerce to permutation")
   z = perm(a)
   z.parent = R
   return z
end

function Base.call(R::FlintPermGroup, a::perm)
   parent(a) != R && error("Unable to coerce to permutation")
   return a
end

###############################################################################
#
#   FlintPermGroup constructor
#
###############################################################################

# handled by inner constructors
