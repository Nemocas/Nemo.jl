export fmpqi, FlintQQi, FlintQQiField

###############################################################################
#
#   Data type and parent methods
#
###############################################################################

elem_type(::Type{FlintQQiField}) = fmpqi

parent_type(::Type{fmpqi}) = FlintQQiField

parent(a::fmpqi) = FlintQQi

base_ring(a::FlintQQiField) = FlintZZi

base_ring(a::fmpqi) = FlintZZi

isdomain_type(::Type{fmpqi}) = true

characteristic(a::FlintQQiField) = 0

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::fmpqi; context = nothing)
   x = expressify(real(a), context=context)
   y = expressify(imag(a), context=context)
   return Expr(:call, :+, x, Expr(:call, :*, y, :im))
end

function Base.show(io::IO, a::fmpqi)
   AbstractAlgebra.show_via_expressify(io, a)
end

function Base.show(io::IO, a::FlintQQiField)
   print(io, "QQ[im]")
end

###############################################################################
#
#   Constructors
#
###############################################################################

function fmpqi()
   return fmpqi(fmpzi(), fmpz(1))
end

function fmpqi(a::fmpzi)
   return fmpqi(a, fmpz(1))
end

function fmpqi(a::fmpq)
   return fmpqi(fmpzi(numerator(a)), denominator(a))
end

function fmpqi(a::fmpq, b::fmpq)
   da = denominator(a)
   db = denominator(b)
   return reduce!(fmpqi(fmpzi(numerator(a)*db, numerator(b)*da), da*db))
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::FlintQQiField)()
   return fmpqi()
end

function (a::FlintQQiField)(b::IntegerUnion)
   return fmpqi(ZZi(b))
end

function (a::FlintQQiField)(b::Union{Rational, fmpq})
   return fmpqi(fmpq(b))
end

function (a::FlintQQiField)(b::Union{Integer, fmpz, Rational, fmpq},
                            c::Union{Integer, fmpz, Rational, fmpq})
   return fmpqi(fmpq(b), fmpq(c))
end

function (a::FlintQQiField)(b::Union{Complex{<:Integer}, fmpzi, Complex{<:Rational}})
   return fmpqi(fmpq(real(b)), fmpq(imag(b)))
end

function (a::FlintQQiField)(b::fmpqi)
   return b
end

function (a::FlintIntegerRing)(b::fmpqi)
   iszero(b.num.y) && isone(b.den) || error("cannot coerce")
   return b.num.x
end

function (a::FlintRationalField)(b::fmpqi)
   iszero(b.num.y) || error("cannot coerce")
   return b.num.x//b.den
end

function (a::FlintZZiRing)(b::fmpqi)
   isone(b.den) || error("cannot coerce")
   return b.num
end

###############################################################################
#
#   Conversions
#
###############################################################################

# see adhoc section for promotions

function Base.convert(::Type{Complex{Rational{T}}}, a::fmpqi) where T <: Integer
   return Complex{Rational{T}}(Base.convert(Rational{T}, real(a)),
                               Base.convert(Rational{T}, imag(a)))
end

function Base.convert(::Type{fmpqi}, a::Complex{T}) where T <: Union{Integer, Rational}
   return fmpqi(convert(fmpq, real(a)), convert(fmpq, imag(a)))
end

function Base.convert(::Type{fmpqi}, a::Union{Integer, fmpz, Rational, fmpq})
   return fmpqi(convert(fmpq, a), fmpq(0))
end

###############################################################################
#
#   Hashing
#
###############################################################################

function Base.hash(a::fmpqi, h::UInt)
   return hash(a.num, xor(hash(a.den, h), 0x6edeadc6d0447c19%UInt))
end

function Base.hash(a::FlintQQiField)
   return 0x4c00da8e36fcc4a8%UInt
end

###############################################################################
#
#   Random generation
#
###############################################################################

function rand_bits(a::FlintQQiField, b::Int)
   b = max(1, b)
   t = clamp(cld(rand(0:b)^2, b), 1, b)  # average b/3 for the denominator
   return reduce!(fmpqi(rand_bits(FlintZZi, clamp(b - t, 0, b)), rand_bits(ZZ, t)))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

# ???
function deepcopy_internal(a::fmpqi, d::IdDict)
   return fmpqi(deepcopy_internal(a.num, d), deepcopy_internal(a.den, d))
end

function deepcopy_internal(a::FlintQQiField, d::IdDict)
   return a
end

function real(a::fmpqi)
   return a.num.x//a.den
end

function imag(a::fmpqi)
   return a.num.y//a.den
end

function abs2(a::fmpqi)
   return abs2(a.num)//a.den^2
end

function zero(a::FlintQQiField)
   return fmpqi(zero(FlintZZi), fmpz(1))
end

function one(a::FlintQQiField)
   return fmpqi(one(FlintZZi), fmpz(1))
end

function iszero(a::fmpqi)
   return iszero(a.num)
end

function isone(a::fmpqi)
   return a.num == a.den
end

function nbits(a::fmpqi)
   return nbits(a.num) + nbits(a.den)
end

function zero!(z::fmpqi)
   zero!(z.num)
   one!(z.den)
   return z
end

function one!(z::fmpqi)
   one!(z.num)
   one!(z.den)
   return z
end

function set!(z::fmpqi, a::fmpqi)
   set!(z.num, a.num)
   set!(z.den, a.den)
   return z
end

function swap!(a::fmpqi, b::fmpqi)
   swap!(a.num, b.num)
   swap!(a.den, b.den)
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

function reduce!(z::fmpqi)
   g = fmpz()
   ccall((:fmpz_gcd3, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
         g, z.num.x, z.den, z.num.y)
   if ccall((:fmpz_sgn, libflint), Cint, (Ref{fmpz},), z.den) < 0
      neg!(g, g)
   end
   divexact!(z.num, z.num, g)
   divexact!(z.den, z.den, g)
   return z
end

function canonical_unit(a::fmpqi)
   return a
end

###############################################################################
#
#   equality
#
###############################################################################

function ==(a::fmpqi, b::fmpqi)
   return a.den == b.den && a.num == b.num
end

function ==(a::fmpqi, b::Union{Complex{<:Integer}, fmpzi, Complex{<:Rational}})
   return real(a) == real(b) && imag(a) == imag(b)
end

function ==(b::Union{Complex{<:Integer}, fmpzi, Complex{<:Rational}}, a::fmpqi)
   return a == b
end

###############################################################################
#
#   addition, subtraction, multiplication
#
###############################################################################

function addeq!(z::fmpqi, a::fmpqi)
   if z !== a
      mul!(z.num, z.num, a.den)
      addmul!(z.num, a.num, z.den)
      mul!(z.den, a.den, z.den)
      reduce!(z)
   else
      mul!(z, z, 2)
   end
   return z
end

function add!(z::fmpqi, a::fmpqi, b::fmpqi)
   if z !== b
      mul!(z.num, a.num, b.den)
      addmul!(z.num, b.num, a.den)
      mul!(z.den, a.den, b.den)
      reduce!(z)
   else
      addeq!(z, a)
   end
   return z
end

function +(a::fmpqi, b::fmpqi)
   return add!(fmpqi(), a, b)
end

function subeq!(z::fmpqi, a::fmpqi)
   if z !== a
      mul!(z.num, z.num, a.den)
      submul!(z.num, a.num, z.den)
      mul!(z.den, z.den, a.den)
      reduce!(z)
   else
      zero!(z)
   end
   return z
end

function sub!(z::fmpqi, a::fmpqi, b::fmpqi)
   if z !== b
      mul!(z.num, a.num, b.den)
      submul!(z.num, b.num, a.den)
      mul!(z.den, a.den, b.den)
      reduce!(z)
   else
      subeq!(z, a)
      neg!(z, z)
   end
   return z
end

function -(a::fmpqi, b::fmpqi)
   return sub!(fmpqi(), a, b)
end

function neg!(z::fmpqi, a::fmpqi)
   neg!(z.num, a.num)
   set!(z.den, a.den)
   return z
end

function -(a::fmpqi)
   return neg!(fmpqi(), a)
end

function mul!(z::fmpqi, a::fmpqi, b::fmpqi)
   mul!(z.num, a.num, b.num)
   mul!(z.den, a.den, b.den)
   reduce!(z)
   return z
end

function mul!(z::fmpqi, a::fmpqi, b::Union{Integer, fmpz, fmpzi})
   mul!(z.num, a.num, b)
   set!(z.den, a.den)
   reduce!(z)
   return z
end

function *(a::fmpqi, b::fmpqi)
   return mul!(fmpqi(), a, b)
end

function addmul!(z::fmpqi, a::fmpqi, b::fmpqi, t::fmpqi)
   mul!(t, a, b)
   add!(z, z, t)
   return z
end

function addmul!(z::fmpqi, a::fmpqi, b::fmpqi)
   return addmul!(z, a, b, fmpqi())
end

function submul!(z::fmpqi, a::fmpqi, b::fmpqi, t::fmpqi)
   mul!(t, a, b)
   sub!(z, z, t)
   return z
end

function submul!(z::fmpqi, a::fmpqi, b::fmpqi)
   return submul!(z, a, b, fmpqi())
end

###############################################################################
#
#   division
#
###############################################################################

function isunit(a::fmpqi)
   return !iszero(a)
end

function inv!(z::fmpqi, a::fmpqi)
   d = abs2(a.num)
   mul!(z.num.x, a.num.x, a.den)
   mul!(z.num.y, a.num.y, a.den)
   neg!(z.num.y, z.num.y)
   swap!(z.den, d)
   reduce!(z)
   return z
end

function inv(a::fmpqi)
   return inv!(fmpqi(), a)
end

function divexact!(z::fmpqi, a::fmpqi, b::fmpqi)
   return mul!(z, a, inv(b))
end

function divexact(a::fmpqi, b::fmpqi; check::Bool = true)
   return divexact!(fmpqi(), a, b)
end

###############################################################################
#
#   powering
#
###############################################################################

function pow!(z::fmpqi, a::fmpqi, b::UInt)
   pow!(z.num, a.num, b)
   pow!(z.den, a.den, b)
   reduce!(z)  # bummer: a.num and a.den are not comprime over ZZ[i]
   return z
end

function pow!(z::fmpqi, a::fmpqi, b::Int)
   if b < 0
      n = (-b)%UInt
      a = inv(a)
   else
      n = (+b)%UInt
   end
   return pow!(z, a, n)
end

function ^(a::fmpqi, b::Int)
   return pow!(fmpqi(), a, b)
end

###############################################################################
#
# adhoc
#
###############################################################################

for (A, Bs) in [
    [fmpqi, [Integer, fmpz, Complex{<:Integer}, fmpzi, Rational, fmpq, Complex{<:Rational}]],
    [fmpzi, [Rational, fmpq, Complex{<:Rational}]],
    [fmpq, [Complex{<:Integer}, Complex{<:Rational}]],
    [fmpz, [Complex{<:Rational}]]]
   for B in Bs
      # need Type{<:Integer} not Type{Integer} and we can't type <:Integer above
      TA = @eval Type{<:($(A))}
      TB = @eval Type{<:($(B))}
      @eval begin
         function Nemo.AbstractAlgebra.promote_rule(::($TA), ::($TB))
            return fmpqi
         end
         function Nemo.AbstractAlgebra.promote_rule(::($TB), ::($TA))
            return fmpqi
         end
         function +(a::($A), b::($B))
            return FlintQQi(a) + FlintQQi(b)
         end
         function +(a::($B), b::($A))
            return FlintQQi(a) + FlintQQi(b)
         end
         function -(a::($A), b::($B))
            return FlintQQi(a) - FlintQQi(b)
         end
         function -(a::($B), b::($A))
            return FlintQQi(a) - FlintQQi(b)
         end
         function *(a::($A), b::($B))
            return FlintQQi(a) * FlintQQi(b)
         end
         function *(a::($B), b::($A))
            return FlintQQi(a) * FlintQQi(b)
         end
      end
   end
end

# // overloads in AA easily lead to ambiguities
for (As, Bs) in [
      [(Integer, Rational), (fmpzi, fmpqi)],
      [(Complex{<:Integer}, Complex{<:Rational}), (fmpz, fmpq, fmpzi, fmpqi)], 
      [(fmpz, fmpq), (Complex{<:Integer}, Complex{<:Rational}, fmpzi, fmpqi)],
      [(fmpzi, fmpqi), (Integer, Rational, fmpz, fmpq, Complex{<:Integer},
                                           Complex{<:Rational}, fmpzi, fmpqi)]]
   for A in As, B in Bs
      @eval begin
         function //(a::($A), b::($B))
            return divexact(FlintQQi(a), FlintQQi(b))
         end
      end
   end
end

