################################################################################
#
#   Special functions for Fac{ZZRingElem}
#
################################################################################

Base.in(b::Integer, a::Fac{ZZRingElem}) = Base.in(ZZRingElem(b), a)

Base.getindex(a::Fac{ZZRingElem}, b::Integer) = getindex(a, ZZRingElem(b))
