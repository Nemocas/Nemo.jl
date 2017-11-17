################################################################################
#
#   Fields.jl : generic fields
#
################################################################################

include("julia/GF.jl")

include("flint/fmpq.jl")

include("flint/fq.jl")

include("flint/fq_nmod.jl")

include("antic/nf_elem.jl")

include("arb/arb.jl")

include("arb/acb.jl")

//(a::T, b::T) where {T <: FieldElem} = divexact(a, b)

//(x::T, y::Union{Integer, Rational}) where {T <: RingElem} = x//parent(x)(y)
                                          
//(x::Union{Integer, Rational}, y::T) where {T <: RingElem} = parent(y)(x)//y

function gcd(x::T, y::T) where {T <: FieldElem}
   check_parent(x, y)
   return iszero(x) && iszero(y) ? zero(parent(y)) : one(parent(y))
end

characteristic(R::Field) = 0


