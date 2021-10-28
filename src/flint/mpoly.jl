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
      throw(DomainError(term(a, i), "exponents do not fit in $T"))
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

# type into which the exponent vectors necessarily fit
function _exponent_vector_type(a::FlintMPolyUnion)
   return a.bits <= Sys.WORD_SIZE ? Int : fmpz
end

###############################################################################
#
# Hash
#
###############################################################################

function _hash_ui_array(a::Ptr{UInt}, n::Int, h::UInt)
   for i in 1:n
      h = hash(unsafe_load(a, i), h)
   end
   return h
end

# an array of fmpz's
function _hash_integer_array(a::Ptr{Int}, n::Int, h::UInt)
   for i in 1:n
      h = _hash_integer(unsafe_load(a, i), h)
   end
   return h
end

function _hash_mpoly_coeffs(a::fmpz_mpoly, h::UInt)
   GC.@preserve a begin
      h = _hash_integer_array(convert(Ptr{Int}, a.coeffs), a.length, h)
      return h
   end
end

function _hash_mpoly_coeffs(a::fmpq_mpoly, h::UInt)
   GC.@preserve a begin
      h = _hash_integer_array(convert(Ptr{Int}, a.coeffs), a.length, h)
      h = _hash_integer(a.content_num, h)
      h = _hash_integer(a.content_den, h)
      return h
   end
end

function _hash_mpoly_coeffs(a::gfp_mpoly, h::UInt)
   GC.@preserve a begin
      h = _hash_ui_array(convert(Ptr{UInt}, a.coeffs), a.length, h)
      return h
   end
end

function _hash_mpoly_coeffs(a::fq_nmod_mpoly, h::UInt)
   GC.@preserve a begin
      d = degree(base_ring(a))
      h = hash(d, h)
      h = _hash_ui_array(convert(Ptr{UInt}, a.coeffs), d*a.length, h)
      return h
   end
end

# fallback
function _hash_mpoly_coeffs(a::FlintMPolyUnion, h::UInt) where S
   for i in 1:length(a)
      h = hash(coeff(a, i), h)
   end
   return h
end

function _hash_mpoly_exps_via(::Type{S}, a::FlintMPolyUnion, h::UInt) where S
   n = nvars(parent(a))
   e = S[zero(S) for i in 1:n]
   h = hash(length(a), h)
   for i in 1:length(a)
      exponent_vector!(e, a, i)
      for j in 1:n
         # crutially, fmpz's hash_integer agrees with Int and UInt
         if S == fmpz
            h = hash_integer((@inbounds e[j]), h)
         else
            h = Base.hash_integer((@inbounds e[j]), h)
         end
      end
   end
   return h
end

function Base.hash(a::FlintMPolyUnion, h::UInt)
   h = _hash_mpoly_coeffs(a, h)
   h = _hash_mpoly_exps_via(_exponent_vector_type(a), a, h)
   return xor(h, 0x53dd43cd511044d1%UInt)
end

###############################################################################
#
# Expressify
#
###############################################################################

function _expressify_monomial!(prod::Expr, x, e)
   for i in 1:length(e)
      if e[i] > 1
         push!(prod.args, Expr(:call, :^, x[i], deepcopy(e[i])))
      elseif e[i] == 1
         push!(prod.args, x[i])
      end
   end
end

function _expressify_mpoly_via(::Type{S}, a::FlintMPolyUnion, x, context) where S
   sum = Expr(:call, :+)
   n = nvars(parent(a))
   e = S[zero(S) for i in 1:n]
   for i in 1:length(a)
      prod = Expr(:call, :*)
      c = coeff(a, i)
      isone(c) || push!(prod.args, expressify(c, context = context))
      exponent_vector!(e, a, i)
      _expressify_monomial!(prod, x, e)
      push!(sum.args, prod)
   end
   return sum
end

function expressify(a::FlintMPolyUnion, x = symbols(parent(a)); context = nothing)
   return _expressify_mpoly_via(_exponent_vector_type(a), a, x, context)
end

