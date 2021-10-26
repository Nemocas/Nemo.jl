###############################################################################
#
# Exponent vectors
#
###############################################################################

# Return true if the exponents of the i-th exp. vector fit into UInts
function exponent_vector_fits_ui(a::FlintMPolyUnion, i::Int)
   return exponent_vector_fits(UInt, a, i)
end

# Return true if the exponents of the i-th exp. vector fit into Ints
function exponent_vector_fits_int(a::FlintMPolyUnion, i::Int)
   return exponent_vector_fits(Int, a, i)
end

# Return Julia array of Int's or UInt's corresponding to exponent vector of i-th term
function exponent_vector(::Type{T}, a::FlintMPolyUnion, i::Int) where T <: Union{Int, UInt}
   if !exponent_vector_fits(T, a, i)
      throw(DomainError(term(a, i), "exponents do not fit in $S"))
   end
   z = Vector{T}(undef, nvars(parent(a)))
   return exponent_vector!(z, a, i)
end

# Return Julia array of fmpz's corresponding to exponent vector of i-th term
function exponent_vector(::Type{fmpz}, a::FlintMPolyUnion, i::Int)
   n = nvars(parent(a))
   z = Vector{fmpz}(undef, n)
   for i in 1:n
      z[i] = fmpz()
   end
   return exponent_vector!(z, a, i)
end

function exponent_vector(a::FlintMPolyUnion, i::Int)
   return exponent_vector(Int, a, i)
end

function exponent_vector_ui(a::FlintMPolyUnion, i::Int)
   return exponent_vector(UInt, a, i)
end

function exponent_vector_fmpz(a::FlintMPolyUnion, i::Int)
   return exponent_vector(fmpz, a, i)
end

