
zero(a::AbstractAlgebra.Generic.MatSpace) = a()

one(a::AbstractAlgebra.Generic.MatSpace) = a(1)

################################################################################
#
#  Parent object overloading
#
################################################################################

function (a::AbstractAlgebra.Generic.MatSpace)(b::RingElement)
   M = a()  # zero
   c = base_ring(a)(b)
   for i in 1:min(nrows(a), ncols(a))
      M[i, i] = c
   end
   return M
end
