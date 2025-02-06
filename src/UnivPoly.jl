###############################################################################
#
#   Specialized methods for universal polynomials
#
###############################################################################

# These methods speed up some computations with specific universal
# polynomial rings which would otherwise be handled by more generic
# code. This mainly concerns rings over the rationals.

denominator(f::UniversalPolyRingElem{QQFieldElem}) = denominator(data(f))
