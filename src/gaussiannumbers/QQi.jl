###############################################################################
#
#   Data type and parent methods
#
###############################################################################

elem_type(::Type{QQiField}) = QQiFieldElem

parent_type(::Type{QQiFieldElem}) = QQiField

parent(a::QQiFieldElem) = FlintQQi

base_ring_type(::Type{QQiField}) = ZZiRing

base_ring(a::QQiField) = FlintZZi

is_domain_type(::Type{QQiFieldElem}) = true

characteristic(a::QQiField) = 0

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function expressify(a::QQiFieldElem; context = nothing)
   x = expressify(real(a), context=context)
   y = expressify(imag(a), context=context)
   return Expr(:call, :+, x, Expr(:call, :*, y, :im))
end

function Base.show(io::IO, a::QQiFieldElem)
   AbstractAlgebra.show_via_expressify(io, a)
end

function Base.show(io::IO, a::QQiField)
   # deliberately no @show_name or @show_special here as this is a singleton type
   if get(io, :supercompact, false)
     io = pretty(io)
     print(io, LowercaseOff(), "QQ[im]")
   else
     io = pretty(io)
     print(io, LowercaseOff(), "Gaussian rational field")
   end
end

###############################################################################
#
#   Constructors
#
###############################################################################

function QQiFieldElem()
   return QQiFieldElem(ZZiRingElem(), ZZRingElem(1))
end

function QQiFieldElem(a::ZZiRingElem)
   return QQiFieldElem(a, ZZRingElem(1))
end

function QQiFieldElem(a::QQFieldElem)
   return QQiFieldElem(ZZiRingElem(numerator(a)), denominator(a))
end

function QQiFieldElem(a::QQFieldElem, b::QQFieldElem)
   da = denominator(a)
   db = denominator(b)
   return reduce!(QQiFieldElem(ZZiRingElem(numerator(a)*db, numerator(b)*da), da*db))
end

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function (a::QQiField)()
   return QQiFieldElem()
end

function (a::QQiField)(b::IntegerUnion)
   return QQiFieldElem(ZZiRingElem(b))
end

function (a::QQiField)(b::Union{Rational, QQFieldElem})
   return QQiFieldElem(QQFieldElem(b))
end

function (a::QQiField)(b::Union{Integer, ZZRingElem, Rational, QQFieldElem},
                            c::Union{Integer, ZZRingElem, Rational, QQFieldElem})
   return QQiFieldElem(QQFieldElem(b), QQFieldElem(c))
end

function (a::QQiField)(b::Union{Complex{<:Integer}, ZZiRingElem, Complex{<:Rational}})
   return QQiFieldElem(QQFieldElem(real(b)), QQFieldElem(imag(b)))
end

function (a::QQiField)(b::QQiFieldElem)
   return b
end

function (a::ZZRing)(b::QQiFieldElem)
   iszero(b.num.y) && isone(b.den) || error("cannot coerce")
   return b.num.x
end

function (a::QQField)(b::QQiFieldElem)
   iszero(b.num.y) || error("cannot coerce")
   return b.num.x//b.den
end

function (a::ZZiRing)(b::QQiFieldElem)
   isone(b.den) || error("cannot coerce")
   return b.num
end

###############################################################################
#
#   Conversions
#
###############################################################################

# see adhoc section for promotions

function Base.convert(::Type{Complex{Rational{T}}}, a::QQiFieldElem) where T <: Integer
   return Complex{Rational{T}}(Base.convert(Rational{T}, real(a)),
                               Base.convert(Rational{T}, imag(a)))
end

function Base.convert(::Type{QQiFieldElem}, a::Complex{T}) where T <: Union{Integer, Rational}
   return QQiFieldElem(convert(QQFieldElem, real(a)), convert(QQFieldElem, imag(a)))
end

function Base.convert(::Type{QQiFieldElem}, a::Union{Integer, ZZRingElem, Rational, QQFieldElem})
   return QQiFieldElem(convert(QQFieldElem, a), QQFieldElem(0))
end

###############################################################################
#
#   Hashing
#
###############################################################################

function Base.hash(a::QQiFieldElem, h::UInt)
   return hash(a.num, xor(hash(a.den, h), 0x6edeadc6d0447c19%UInt))
end

###############################################################################
#
#   Random generation
#
###############################################################################

function rand_bits(a::QQiField, b::Int)
   b = max(1, b)
   t = clamp(cld(rand(0:b)^2, b), 1, b)  # average b/3 for the denominator
   return reduce!(QQiFieldElem(rand_bits(FlintZZi, clamp(b - t, 0, b)), rand_bits(ZZ, t)))
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

# ???
function deepcopy_internal(a::QQiFieldElem, d::IdDict)
   return QQiFieldElem(deepcopy_internal(a.num, d), deepcopy_internal(a.den, d))
end

function deepcopy_internal(a::QQiField, d::IdDict)
   return a
end

function real(a::QQiFieldElem)
   return a.num.x//a.den
end

function imag(a::QQiFieldElem)
   return a.num.y//a.den
end

function abs2(a::QQiFieldElem)
   return abs2(a.num)//a.den^2
end

function zero(a::QQiField)
   return QQiFieldElem(zero(FlintZZi), ZZRingElem(1))
end

function one(a::QQiField)
   return QQiFieldElem(one(FlintZZi), ZZRingElem(1))
end

function iszero(a::QQiFieldElem)
   return iszero(a.num)
end

function isone(a::QQiFieldElem)
   return a.num == a.den
end

function nbits(a::QQiFieldElem)
   return nbits(a.num) + nbits(a.den)
end

function zero!(z::QQiFieldElem)
   zero!(z.num)
   one!(z.den)
   return z
end

function one!(z::QQiFieldElem)
   one!(z.num)
   one!(z.den)
   return z
end

function set!(z::QQiFieldElem, a::QQiFieldElem)
   set!(z.num, a.num)
   set!(z.den, a.den)
   return z
end

function swap!(a::QQiFieldElem, b::QQiFieldElem)
   swap!(a.num, b.num)
   swap!(a.den, b.den)
end

###############################################################################
#
#   Canonicalisation
#
###############################################################################

function reduce!(z::QQiFieldElem)
   g = ZZRingElem()
   ccall((:fmpz_gcd3, libflint), Nothing,
         (Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}, Ref{ZZRingElem}),
         g, z.num.x, z.den, z.num.y)
   if ccall((:fmpz_sgn, libflint), Cint, (Ref{ZZRingElem},), z.den) < 0
      neg!(g, g)
   end
   divexact!(z.num, z.num, g)
   divexact!(z.den, z.den, g)
   return z
end

function canonical_unit(a::QQiFieldElem)
   return a
end

###############################################################################
#
#   equality
#
###############################################################################

function ==(a::QQiFieldElem, b::QQiFieldElem)
   return a.den == b.den && a.num == b.num
end

function ==(a::QQiFieldElem, b::Union{Complex{<:Integer}, ZZiRingElem, Complex{<:Rational}})
   return real(a) == real(b) && imag(a) == imag(b)
end

function ==(b::Union{Complex{<:Integer}, ZZiRingElem, Complex{<:Rational}}, a::QQiFieldElem)
   return a == b
end

###############################################################################
#
#   addition, subtraction, multiplication
#
###############################################################################

function addeq!(z::QQiFieldElem, a::QQiFieldElem)
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

function add!(z::QQiFieldElem, a::QQiFieldElem, b::QQiFieldElem)
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

function +(a::QQiFieldElem, b::QQiFieldElem)
   return add!(QQiFieldElem(), a, b)
end

function subeq!(z::QQiFieldElem, a::QQiFieldElem)
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

function sub!(z::QQiFieldElem, a::QQiFieldElem, b::QQiFieldElem)
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

function -(a::QQiFieldElem, b::QQiFieldElem)
   return sub!(QQiFieldElem(), a, b)
end

function neg!(z::QQiFieldElem, a::QQiFieldElem)
   neg!(z.num, a.num)
   set!(z.den, a.den)
   return z
end

function -(a::QQiFieldElem)
   return neg!(QQiFieldElem(), a)
end

function mul!(z::QQiFieldElem, a::QQiFieldElem, b::QQiFieldElem)
   mul!(z.num, a.num, b.num)
   mul!(z.den, a.den, b.den)
   reduce!(z)
   return z
end

function mul!(z::QQiFieldElem, a::QQiFieldElem, b::Union{Integer, ZZRingElem, ZZiRingElem})
   mul!(z.num, a.num, b)
   set!(z.den, a.den)
   reduce!(z)
   return z
end

function *(a::QQiFieldElem, b::QQiFieldElem)
   return mul!(QQiFieldElem(), a, b)
end

function addmul!(z::QQiFieldElem, a::QQiFieldElem, b::QQiFieldElem, t::QQiFieldElem)
   mul!(t, a, b)
   add!(z, z, t)
   return z
end

function addmul!(z::QQiFieldElem, a::QQiFieldElem, b::QQiFieldElem)
   return addmul!(z, a, b, QQiFieldElem())
end

function submul!(z::QQiFieldElem, a::QQiFieldElem, b::QQiFieldElem, t::QQiFieldElem)
   mul!(t, a, b)
   sub!(z, z, t)
   return z
end

function submul!(z::QQiFieldElem, a::QQiFieldElem, b::QQiFieldElem)
   return submul!(z, a, b, QQiFieldElem())
end

###############################################################################
#
#   division
#
###############################################################################

function is_unit(a::QQiFieldElem)
   return !iszero(a)
end

function inv!(z::QQiFieldElem, a::QQiFieldElem)
   d = abs2(a.num)
   mul!(z.num.x, a.num.x, a.den)
   mul!(z.num.y, a.num.y, a.den)
   neg!(z.num.y, z.num.y)
   swap!(z.den, d)
   reduce!(z)
   return z
end

function inv(a::QQiFieldElem)
   return inv!(QQiFieldElem(), a)
end

function divexact!(z::QQiFieldElem, a::QQiFieldElem, b::QQiFieldElem)
   return mul!(z, a, inv(b))
end

function divexact(a::QQiFieldElem, b::QQiFieldElem; check::Bool = true)
   return divexact!(QQiFieldElem(), a, b)
end

###############################################################################
#
#   powering
#
###############################################################################

function pow!(z::QQiFieldElem, a::QQiFieldElem, b::UInt)
   pow!(z.num, a.num, b)
   pow!(z.den, a.den, b)
   reduce!(z)  # bummer: a.num and a.den are not comprime over ZZ[i]
   return z
end

function pow!(z::QQiFieldElem, a::QQiFieldElem, b::Int)
   if b < 0
      n = (-b)%UInt
      a = inv(a)
   else
      n = (+b)%UInt
   end
   return pow!(z, a, n)
end

function ^(a::QQiFieldElem, b::Int)
   return pow!(QQiFieldElem(), a, b)
end

###############################################################################
#
# adhoc
#
###############################################################################

for (A, Bs) in [
    [QQiFieldElem, [Integer, ZZRingElem, Complex{<:Integer}, ZZiRingElem, Rational, QQFieldElem, Complex{<:Rational}]],
    [ZZiRingElem, [Rational, QQFieldElem, Complex{<:Rational}]],
    [QQFieldElem, [Complex{<:Integer}, Complex{<:Rational}]],
    [ZZRingElem, [Complex{<:Rational}]]]
   for B in Bs
      # need Type{<:Integer} not Type{Integer} and we can't type <:Integer above
      TA = @eval Type{<:($(A))}
      TB = @eval Type{<:($(B))}
      @eval begin
         function Nemo.AbstractAlgebra.promote_rule(::($TA), ::($TB))
            return QQiFieldElem
         end
         function Nemo.AbstractAlgebra.promote_rule(::($TB), ::($TA))
            return QQiFieldElem
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
      [(Integer, Rational), (ZZiRingElem, QQiFieldElem)],
      [(Complex{<:Integer}, Complex{<:Rational}), (ZZRingElem, QQFieldElem, ZZiRingElem, QQiFieldElem)], 
      [(ZZRingElem, QQFieldElem), (Complex{<:Integer}, Complex{<:Rational}, ZZiRingElem, QQiFieldElem)],
      [(ZZiRingElem, QQiFieldElem), (Integer, Rational, ZZRingElem, QQFieldElem, Complex{<:Integer},
                                           Complex{<:Rational}, ZZiRingElem, QQiFieldElem)]]
   for A in As, B in Bs
      @eval begin
         function //(a::($A), b::($B))
            return divexact(FlintQQi(a), FlintQQi(b))
         end
      end
   end
end

