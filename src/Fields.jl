################################################################################
#
#   Fields.jl : generic fields
#
################################################################################

include("flint/fmpq.jl")

include("flint/fq.jl")

include("flint/fq_nmod.jl")

include("antic/nf_elem.jl")

include("arb/arb.jl")

include("arb/acb.jl")

//(a::T, b::T) where {T <: FieldElem} = divexact(a, b)

function gcd(x::T, y::T) where {T <: FieldElem}
   check_parent(x, y)
   return iszero(x) && iszero(y) ? zero(parent(y)) : one(parent(y))
end



