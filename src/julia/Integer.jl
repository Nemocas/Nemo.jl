###############################################################################
#
#   Integer.jl : Additional Nemo functionality for Julia Integer
#
###############################################################################

###############################################################################
#
#   Data type and parent object methods
#
###############################################################################

JuliaZZ = Integers{BigInt}()

zz = Integers{Int}()

parent(a::T) where T <: Integer = Integers{T}()

elem_type(::Type{Integers{T}}) where T <: Integer = T

parent_type(::Type{T}) where T <: Integer = Integers{T}

base_ring(a::Integer) = Union{}

isexact_type(::Type{T}) where T <: Integer = true

isdomain_type(::Type{T}) where T <: Integer = true

###############################################################################
#
#   Basic manipulation
#
###############################################################################

zero(::Integers{T}) where T <: Integer = T(0)

one(::Integers{T}) where T <: Integer = T(1)

if VERSION < v"0.7.0-DEV.1144"
isone(a::Integer) = a == 1
end

isunit(a::Integer) = a == 1 || a == -1

canonical_unit(a::T) where T <: Integer = a < 0 ? T(-1) : T(1)

###############################################################################
#
#   String I/O
#
###############################################################################

function show(io::IO, R::Integers)
   print(io, "Integers")
end

needs_parentheses(::Integer) = false

isnegative(a::Integer) = a < 0

show_minus_one(::Type{T}) where T <: Integer = false

###############################################################################
#
#   Modular arithmetic
#
###############################################################################

function powmod(a::T, b::Int, c::T) where T <: Integer
   b < 0 && throw(DomainError())
   # special cases
   if a == 0
      return T(0)
   elseif b == 0
      return T(1)
   else
      bit = ~((~UInt(0)) >> 1)
      while (UInt(bit) & b) == 0
         bit >>= 1
      end
      z = a
      bit >>= 1
      while bit != 0
         z = mod(z*z, c)
         if (UInt(bit) & b) != 0
            z = mod(z*a, c)
         end
         bit >>= 1
      end
      return z
   end
end

###############################################################################
#
#   Divides
#
###############################################################################

function divides(a::T, b::T) where T <: Integer
   q, r = divrem(a, b)
   return r == 0, q
end

###############################################################################
#
#   Exact division
#
###############################################################################

divexact(a::T, b::T) where T <: Integer = div(a, b)

divexact(a::BigInt, b::T) where T <: Integer = div(a, b)

###############################################################################
#
#   GCD
#
###############################################################################

function gcdinv(a::T, b::T) where T <: Integer
   g, s, t = gcdx(a, b)
   return g, s
end

###############################################################################
#
#   Square root
#
###############################################################################

function sqrt(a::T) where T <: Integer
   s = isqrt(a)
   s*s != a && error("Not a square in sqrt")
   return s 
end
 
###############################################################################
#
#   Exponential
#
###############################################################################

function exp(a::T) where T <: Integer
    a != 0 && throw(DomainError())
    return T(1)
 end
 
###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(a::Integer)
   return 0
end

function zero!(a::BigInt)
   ccall((:__gmpz_set_si, :libgmp), Void, (Ref{BigInt}, Int), a, 0)
   return a
end

function mul!(a::T, b::T, c::T) where T <: Integer
   return b*c
end

function mul!(a::BigInt, b::BigInt, c::BigInt)
   ccall((:__gmpz_mul, :libgmp), Void, (Ref{BigInt}, Ref{BigInt}, Ref{BigInt}), a, b, c)
   return a
end

function add!(a::T, b::T, c::T) where T <: Integer
   return b + c
end

function add!(a::BigInt, b::BigInt, c::BigInt)
   ccall((:__gmpz_add, :libgmp), Void, (Ref{BigInt}, Ref{BigInt}, Ref{BigInt}), a, b, c)
   return a
end

function addeq!(a::T, b::T) where T <: Integer
   return a + b
end

function addeq!(a::BigInt, b::BigInt)
   ccall((:__gmpz_add, :libgmp), Void, (Ref{BigInt}, Ref{BigInt}, Ref{BigInt}), a, a, b)
   return a
end

function addmul!(a::T, b::T, c::T, d::T) where T <: Integer
   return a + b*c
end

function addmul!(a::BigInt, b::BigInt, c::BigInt, d::BigInt)
   ccall((:__gmpz_addmul, :libgmp), Void, (Ref{BigInt}, Ref{BigInt}, Ref{BigInt}), a, b, c)
   return a
end

function addmul!(a::T, b::T, c::T) where T <: Integer # special case, no temporary required
   return a + b*c
end

function addmul!(a::BigInt, b::BigInt, c::BigInt) # special case, no temporary required
   ccall((:__gmpz_addmul, :libgmp), Void, (Ref{BigInt}, Ref{BigInt}, Ref{BigInt}), a, b, c)
   return a
end

###############################################################################
#
#   Random generation
#
###############################################################################

function rand(R::Integers, n::UnitRange{Int})
   return R(rand(n))
end

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (a::Integers{T})() where T <: Integer
   return T(0)
end

function (a::Integers{T})(b::Integer) where T <: Integer
   return T(b)
end
