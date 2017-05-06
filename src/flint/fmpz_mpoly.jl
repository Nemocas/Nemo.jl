###############################################################################
#
#   fmpz_mpoly.jl : Flint multivariate polynomials over fmpz
#
###############################################################################

export FmpzMPolyRing, fmpz_mpoly

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

parent_type{S, N}(::Type{fmpz_mpoly{S, N}}) = FmpzMPolyRing{S, N}

elem_type{S, N}(::FmpzMPolyRing{S, N}) = fmpz_mpoly{S, N}

vars(a::FmpzMPolyRing) = a.S

function gens{S, N}(R::FmpzMPolyRing{S, N})
   A = Array(fmpz_mpoly{S, N}, R.num_vars)
   for i = 1:R.num_vars
      z = R()
      ccall((:fmpz_mpoly_gen, :libflint), Void,
            (Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}), &z, i - 1, &R)
      A[i] = z
   end
   return A
end

function gen{S, N}(R::FmpzMPolyRing{S, N}, i::Int)
   z = R()
   ccall((:fmpz_mpoly_gen, :libflint), Void,
            (Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}), &z, i - 1, &R)
   return z
end

function isgen(a::fmpz_mpoly)
   R = parent(a)
   return Bool(ccall((:fmpz_mpoly_is_gen, :libflint), Cint,
              (Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}), &a, &a.parent))
end

function isgen(a::fmpz_mpoly, i::Int)
   R = parent(a)
   return Bool(ccall((:fmpz_mpoly_is_gen_i, :libflint), Cint,
              (Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}), &a, i - 1, &a.parent))
end

function coeff(a::fmpz_mpoly, i::Int)
   z = fmpz()
   ccall((:fmpz_mpoly_get_coeff_fmpz, :libflint), Void,
         (Ptr{fmpz}, Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}),
         &z, &a, i, &a.parent)
   return z
end

function deepcopy(a::fmpz_mpoly)
   z = parent(a)()
   ccall((:fmpz_mpoly_set, :libflint), Void,
         (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}),
         &z, &a, &a.parent)
   return z
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

num_vars(x::fmpz_mpoly) = parent(x).num_vars

function max_degrees{S, N}(a::fmpz_mpoly{S, N})
   R = a.parent
   A = Array(Int, N)
   ccall((:fmpz_mpoly_max_degrees, :libflint), Void,
         (Ptr{Int}, Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}), A, &a, &R)
   biggest = 0
   for i = 1:N
      if A[i] > biggest
         biggest = A[i]
      end
  end
  return biggest, A
end

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, x::fmpz_mpoly)
   if length(x) == 0
      print(io, "0")
   else
      cstr = ccall((:fmpz_mpoly_get_str_pretty, :libflint), Ptr{UInt8}, 
          (Ptr{fmpz_mpoly}, Ptr{Ptr{UInt8}}, Ptr{FmpzMPolyRing}), 
          &x, [bytestring(string(s)) for s in vars(parent(x))], &x.parent)
      print(io, bytestring(cstr))

      ccall((:flint_free, :libflint), Void, (Ptr{UInt8},), cstr)
   end
end

function show(io::IO, p::FmpzMPolyRing)
   const max_vars = 5 # largest number of variables to print
   n = p.num_vars
   print(io, "Multivariate Polynomial Ring in ")
   if n > max_vars
      print(io, p.num_vars)
      print(io, " variables ")
   end
   for i = 1:min(n - 1, max_vars - 1)
      print(io, string(p.S[i]), ", ")
   end
   if n > max_vars
      print(io, "..., ")
   end
   print(io, string(p.S[n]))
   print(io, " over ")
   show(io, base_ring(p))
end

###############################################################################
#
#   Basic arithmetic
#
###############################################################################

function -(a::fmpz_mpoly)
   z = parent(a)()
   ccall((:fmpz_mpoly_neg, :libflint), Void, 
       (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}),
       &z, &a, &a.parent)
   return z
end

function +{S, N}(a::fmpz_mpoly{S, N}, b::fmpz_mpoly{S, N})
   z = parent(a)()
   ccall((:fmpz_mpoly_add, :libflint), Void, 
       (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}),
       &z, &a, &b, &a.parent)
   return z
end

function -{S, N}(a::fmpz_mpoly{S, N}, b::fmpz_mpoly{S, N})
   z = parent(a)()
   ccall((:fmpz_mpoly_sub, :libflint), Void, 
       (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}),
       &z, &a, &b, &a.parent)
   return z
end

function *{S, N}(a::fmpz_mpoly{S, N}, b::fmpz_mpoly{S, N})
   z = parent(a)()
   ccall((:fmpz_mpoly_mul_johnson, :libflint), Void, 
       (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}),
       &z, &a, &b, &a.parent)
   return z
end

function mul_array{S, N}(a::fmpz_mpoly{S, N}, b::fmpz_mpoly{S, N})
   z = parent(a)()
   ccall((:fmpz_mpoly_mul_array, :libflint), Void, 
       (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}),
       &z, &a, &b, &a.parent)
   return z
end

###############################################################################
#
#   Ad hoc arithmetic
#
###############################################################################

function *(a::fmpz_mpoly, b::Int)
   r = parent(a)()
   ccall((:fmpz_mpoly_scalar_mul_si, :libflint), Void,
        (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}), 
        &r, &a, b, &a.parent)
   return r
end

function *(a::fmpz_mpoly, b::fmpz)
   r = parent(a)()
   ccall((:fmpz_mpoly_scalar_mul_fmpz, :libflint), Void,
        (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{fmpz}, Ptr{FmpzMPolyRing}), 
        &r, &a, &b, &a.parent)
   return r
end

*(a::fmpz, b::fmpz_mpoly) = b*a

*(a::Int, b::fmpz_mpoly) = b*a

function +(a::fmpz_mpoly, b::Int)
   r = parent(a)()
   ccall((:fmpz_mpoly_add_si, :libflint), Void,
        (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}), 
        &r, &a, b, &a.parent)
   return r
end

function +(a::fmpz_mpoly, b::fmpz)
   r = parent(a)()
   ccall((:fmpz_mpoly_add_fmpz, :libflint), Void,
        (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{fmpz}, Ptr{FmpzMPolyRing}), 
        &r, &a, &b, &a.parent)
   return r
end

+(a::fmpz, b::fmpz_mpoly) = b + a

+(a::Int, b::fmpz_mpoly) = b + a

function -(a::fmpz_mpoly, b::Int)
   r = parent(a)()
   ccall((:fmpz_mpoly_sub_si, :libflint), Void,
        (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}), 
        &r, &a, b, &a.parent)
   return r
end

function -(a::fmpz_mpoly, b::fmpz)
   r = parent(a)()
   ccall((:fmpz_mpoly_sub_fmpz, :libflint), Void,
        (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{fmpz}, Ptr{FmpzMPolyRing}), 
        &r, &a, &b, &a.parent)
   return r
end

-(a::fmpz, b::fmpz_mpoly) = -(b - a)

-(a::Int, b::fmpz_mpoly) = -(b - a)

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::fmpz_mpoly, b::Int)
   b < 0 && throw(DomainError())
   z = parent(a)()
   ccall((:fmpz_mpoly_pow_fps, :libflint), Void,
         (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}),
         &z, &a, b, &parent(a))
   return z
end

###############################################################################
#
#   Comparison
#
###############################################################################

function =={S, N}(a::fmpz_mpoly{S, N}, b::fmpz_mpoly{S, N})
   return Bool(ccall((:fmpz_mpoly_equal, :libflint), Cint,
               (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}),
               &a, &b, &a.parent))
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

function ==(a::fmpz_mpoly, b::Int)
   return Bool(ccall((:fmpz_mpoly_equal_si, :libflint), Cint,
               (Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}),
               &a, b, &a.parent))
end

==(a::Int, b::fmpz_mpoly) = b == a

function ==(a::fmpz_mpoly, b::fmpz)
   return Bool(ccall((:fmpz_mpoly_equal_fmpz, :libflint), Cint,
               (Ptr{fmpz_mpoly}, Ptr{fmpz}, Ptr{FmpzMPolyRing}),
               &a, &b, &a.parent))
end

==(a::fmpz, b::fmpz_mpoly) = b == a

==(a::fmpz_mpoly, b::Integer) = a == fmpz(b)

==(a::Integer, b::fmpz_mpoly) = b == a

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divides_array{S, N}(a::fmpz_mpoly{S, N}, b::fmpz_mpoly{S, N})
   z = parent(a)()
   d = ccall((:fmpz_mpoly_divides_array, :libflint), Cint, 
       (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}),
       &z, &a, &b, &a.parent)
   d == -1 && error("Polynomial too large for divides_array")
   return d == 1, z
end

function divides_monagan_pearce{S, N}(a::fmpz_mpoly{S, N}, b::fmpz_mpoly{S, N})
   z = parent(a)()
   d = Bool(ccall((:fmpz_mpoly_divides_monagan_pearce, :libflint), Cint, 
       (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}),
       &z, &a, &b, &a.parent))
   return d, z
end

###############################################################################
#
#   Division with remainder
#
###############################################################################

function divrem_monagan_pearce{S, N}(a::fmpz_mpoly{S, N}, b::fmpz_mpoly{S, N})
   q = parent(a)()
   r = parent(a)()
   ccall((:fmpz_mpoly_divrem_monagan_pearce, :libflint), Cint, 
       (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly},
        Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}),
       &q, &r, &a, &b, &a.parent)
   return q, r
end


###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(a::fmpz_mpoly, b::Int)
   r = parent(a)()
   ccall((:fmpz_mpoly_scalar_divexact_si, :libflint), Void,
        (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Int, Ptr{FmpzMPolyRing}), 
        &r, &a, b, &a.parent)
   return r
end

function divexact(a::fmpz_mpoly, b::fmpz)
   r = parent(a)()
   ccall((:fmpz_mpoly_scalar_divexact_fmpz, :libflint), Void,
        (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly}, Ptr{fmpz}, Ptr{FmpzMPolyRing}), 
        &r, &a, &b, &a.parent)
   return r
end

divexact(a::fmpz_mpoly, b::Integer) = divexact(a, fmpz(b))

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function addeq!{S, N}(a::fmpz_mpoly{S, N}, b::fmpz_mpoly{S, N})
   ccall((:fmpz_mpoly_add, :libflint), Void,
         (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly},
          Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}), &a, &a, &b, &a.parent)
   return nothing
end

function mul!{S, N}(a::fmpz_mpoly{S, N}, b::fmpz_mpoly{S, N}, c::fmpz_mpoly{S, N})
   ccall((:fmpz_mpoly_mul_johnson, :libflint), Void,
         (Ptr{fmpz_mpoly}, Ptr{fmpz_mpoly},
          Ptr{fmpz_mpoly}, Ptr{FmpzMPolyRing}), &a, &b, &c, &a.parent)
   return nothing
end

function setcoeff!(a::fmpz_mpoly, i::Int, c::Int)
   ccall((:fmpz_mpoly_set_coeff_si, :libflint), Void,
        (Ptr{fmpz_mpoly}, Int, Int, Ptr{FmpzMPolyRing}), &a, i, c, &a.parent)
end

function setcoeff!(a::fmpz_mpoly, i::Int, c::fmpz)
   ccall((:fmpz_mpoly_set_coeff_fmpz, :libflint), Void,
        (Ptr{fmpz_mpoly}, Int, Ptr{fmpz}, Ptr{FmpzMPolyRing}),
                                                          &a, i, &c, &a.parent)
end

###############################################################################
#
#   Promotion rules
#
###############################################################################

Base.promote_rule{V <: Integer}(::Type{fmpz_mpoly}, ::Type{V}) = fmpz_mpoly

Base.promote_rule(::Type{fmpz_mpoly}, ::Type{fmpz}) = fmpz_mpoly

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function Base.call{S, N}(R::FmpzMPolyRing{S, N})
   z = fmpz_mpoly{S, N}(R)
   z.parent = R
   return z
end

function Base.call{S, N}(R::FmpzMPolyRing{S, N}, b::Int)
   z = fmpz_mpoly{S, N}(R, b)
   z.parent = R
   return z
end

function Base.call{S, N}(R::FmpzMPolyRing{S, N}, b::fmpz)
   z = fmpz_mpoly{S, N}(R, b)
   z.parent = R
   return z
end

function Base.call{S, N}(R::FmpzMPolyRing{S, N}, b::Integer)
   z = fmpz_mpoly{S, N}(R, fmpz(b))
   z.parent = R
   return z
end

function Base.call{S, N}(R::FmpzMPolyRing{S, N}, a::Array{fmpz, 1}, b::Array{NTuple{N, Int}, 1})
   z = fmpz_mpoly{S, N}(R, a, b)
   z.parent = R
   return z
end

###############################################################################
#
#   PolynomialRing constructor
#
###############################################################################

function PolynomialRing(R::FlintIntegerRing, s::Array{String, 1}; cached::Bool = true, ordering::Symbol = :lex)
   U = [Symbol(x) for x in s]
   N = (ordering == :deglex || ordering == :degrevlex) ? length(U) + 1 : length(U)
   # default to 8 bit exponent fields
   parent_obj = FmpzMPolyRing{ordering, N}(U, cached)
   return tuple(parent_obj, gens(parent_obj))
end



