###############################################################################
#
#   gfp_elem.jl : Nemo gfp_elem (integers modulo small n)
#
###############################################################################

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{gfp_elem}) = GaloisField

elem_type(::Type{GaloisField}) = gfp_elem

base_ring(a::GaloisField) = Union{}

base_ring(a::gfp_elem) = Union{}

parent(a::gfp_elem) = a.parent

function check_parent(a::gfp_elem, b::gfp_elem)
   a.parent != b.parent && error("Operations on distinct Galois fields not supported")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function Base.hash(a::gfp_elem, h::UInt)
   b = 0x749c75e438001387%UInt
   return xor(xor(hash(a.data), h), b)
end

function zero(R::GaloisField)
   return gfp_elem(UInt(0), R)
end

function one(R::GaloisField)
   return gfp_elem(UInt(1), R)
end

iszero(a::gfp_elem) = a.data == 0

isone(a::gfp_elem) = a.data == 1

isunit(a::gfp_elem) = a.data != 0

modulus(R::GaloisField) = R.n

function deepcopy_internal(a::gfp_elem, dict::IdDict)
   R = parent(a)
   return gfp_elem(deepcopy(a.data), R)
end

@doc Markdown.doc"""
    order(R::GaloisField) -> fmpz
> Return the order, i.e. the number of elements in, the given Galois field.
"""
order(R::GaloisField) = fmpz(R.n)

@doc Markdown.doc"""
    characteristic(R::GaloisField) -> fmpz
> Return the characteristic of the given Galois field.
"""
characteristic(R::GaloisField) = fmpz(R.n)

###############################################################################
#
#   Canonicalisation
#
###############################################################################

function canonical_unit(x::gfp_elem)
  return x
end

###############################################################################
#
#   AbstractString I/O
#
###############################################################################

function show(io::IO, R::GaloisField)
   print(io, "Galois field with characteristic ", signed(widen(R.n)))
end

function show(io::IO, a::gfp_elem)
   print(io, signed(widen(a.data)))
end

needs_parentheses(x::gfp_elem) = false

displayed_with_minus_in_front(x::gfp_elem) = false

show_minus_one(::Type{gfp_elem}) = true

###############################################################################
#
#   Unary operations
#
###############################################################################

function -(x::gfp_elem)
   if x.data == 0
      return deepcopy(x)
   else
      R = parent(x)
      return gfp_elem(R.n - x.data, R)
   end
end

###############################################################################
#
#   Binary operations
#
###############################################################################

function +(x::gfp_elem, y::gfp_elem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data + y.data - n
   if d > x.data
      return gfp_elem(d + n, R)
   else
      return gfp_elem(d, R)
   end
end

function -(x::gfp_elem, y::gfp_elem)
   check_parent(x, y)
   R = parent(x)
   n = modulus(R)
   d = x.data - y.data
   if d > x.data
      return gfp_elem(d + n, R)
   else
      return gfp_elem(d, R)
   end
end

function *(x::gfp_elem, y::gfp_elem)
   check_parent(x, y)
   R = parent(x)
   d = ccall((:n_mulmod2_preinv, :libflint), UInt, (UInt, UInt, UInt, UInt),
             x.data, y.data, R.n, R.ninv)
   return gfp_elem(d, R)
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function *(x::Integer, y::gfp_elem)
   R = parent(y)
   return R(widen(x)*signed(widen(y.data)))
end

*(x::gfp_elem, y::Integer) = y*x

function *(x::Int, y::gfp_elem)
   R = parent(y)
   if x < 0
      d = ccall((:n_mulmod2_preinv, :libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(-x), y.data, R.n, R.ninv)
      return -gfp_elem(d, R)
   else
      d = ccall((:n_mulmod2_preinv, :libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(x), y.data, R.n, R.ninv)
      return gfp_elem(d, R)
   end
end

*(x::gfp_elem, y::Int) = y*x

function *(x::UInt, y::gfp_elem)
   R = parent(y)
   d = ccall((:n_mulmod2_preinv, :libflint), UInt, (UInt, UInt, UInt, UInt),
             UInt(x), y.data, R.n, R.ninv)
   return gfp_elem(d, R)
end

*(x::gfp_elem, y::UInt) = y*x

+(x::gfp_elem, y::Integer) = x + parent(x)(y)

+(x::Integer, y::gfp_elem) = y + x

-(x::gfp_elem, y::Integer) = x - parent(x)(y)

-(x::Integer, y::gfp_elem) = parent(y)(x) - y

*(x::fmpz, y::gfp_elem) = BigInt(x)*y

*(x::gfp_elem, y::fmpz) = y*x

+(x::gfp_elem, y::fmpz) = x + parent(x)(y)

+(x::fmpz, y::gfp_elem) = y + x

-(x::gfp_elem, y::fmpz) = x - parent(x)(y)

-(x::fmpz, y::gfp_elem) = parent(y)(x) - y

###############################################################################
#
#   Powering
#
###############################################################################

function ^(x::gfp_elem, y::Int)
   R = parent(x)
   if y < 0
      x = inv(x)
      y = -y
   end
   d = ccall((:n_powmod2_preinv, :libflint), UInt, (UInt, Int, UInt, UInt),
             UInt(x.data), y, R.n, R.ninv)
   return gfp_elem(d, R)
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(x::gfp_elem, y::gfp_elem)
   check_parent(x, y)
   return x.data == y.data
end

###############################################################################
#
#   Ad hoc comparison
#
###############################################################################

==(x::gfp_elem, y::Integer) = x == parent(x)(y)

==(x::Integer, y::gfp_elem) = parent(y)(x) == y

==(x::gfp_elem, y::fmpz) = x == parent(x)(y)

==(x::fmpz, y::gfp_elem) = parent(y)(x) == y

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(x::gfp_elem)
   R = parent(x)
   iszero(x) && throw(DivideError())
   xinv = ccall((:n_invmod, :libflint), UInt, (UInt, UInt),
            x.data, R.n)
   return gfp_elem(xinv, R)
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(x::gfp_elem, y::gfp_elem)
   check_parent(x, y)
   R = parent(x)
   yinv = ccall((:n_invmod, :libflint), UInt, (UInt, UInt),
           y.data, R.n)
   d = ccall((:n_mulmod2_preinv, :libflint), UInt, (UInt, UInt, UInt, UInt),
             x.data, yinv, R.n, R.ninv)
   return gfp_elem(d, R)
end

function divides(a::gfp_elem, b::gfp_elem)
   check_parent(a, b)
   if iszero(a)
      return true, a
   end
   if iszero(b)
      return false, a
   end
   return true, divexact(a, b)
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function zero!(z::gfp_elem)
   R = parent(z)
   return gfp_elem(UInt(0), R)
end

function mul!(z::gfp_elem, x::gfp_elem, y::gfp_elem)
   return x*y
end

function addeq!(z::gfp_elem, x::gfp_elem)
   return z + x
end

function add!(z::gfp_elem, x::gfp_elem, y::gfp_elem)
   return x + y
end

###############################################################################
#
#   Random functions
#
###############################################################################

function rand(R::GaloisField)
   n = rand(UInt(0):R.n - 1)
   return gfp_elem(n, R)
end

function rand(R::GaloisField, b::UnitRange{Int})
   n = rand(b)
   return R(n)
end

###############################################################################
#
#   Promotions
#
###############################################################################

promote_rule(::Type{gfp_elem}, ::Type{T}) where T <: Integer = gfp_elem

###############################################################################
#
#   Parent object call overload
#
###############################################################################

function (R::GaloisField)()
   return gfp_elem(UInt(0), R)
end

function (R::GaloisField)(a::Integer)
   n = R.n
   d = a%signed(widen(n))
   if d < 0
      d += n
   end
   return gfp_elem(UInt(d), R)
end

function (R::GaloisField)(a::Int)
   n = R.n
   ninv = R.ninv
   if reinterpret(Int, n) > 0 && a < 0
      a %= Int(n)
   end
   d = reinterpret(UInt, a)
   if a < 0
      d += n
   end
   if d >= n
      d = ccall((:n_mod2_preinv, :libflint), UInt, (UInt, UInt, UInt),
             d, n, ninv)
   end
   return gfp_elem(d, R)
end

function (R::GaloisField)(a::UInt)
   n = R.n
   ninv = R.ninv
   a = ccall((:n_mod2_preinv, :libflint), UInt, (UInt, UInt, UInt),
             a, n, ninv)
   return gfp_elem(a, R)
end

function (R::GaloisField)(a::fmpz)
   d = ccall((:fmpz_fdiv_ui, :libflint), UInt, (Ref{fmpz}, UInt),
             a, R.n)
   return gfp_elem(d, R)
end

function (R::GaloisField)(a::gfp_elem)
   return a
end

###############################################################################
#
#   gfp_elem constructor
#
###############################################################################

function GF(n::Int; cached::Bool=true)
   (n <= 0) && throw(DomainError("Characteristic must be positive: $n"))
   un = UInt(n)
   !is_prime(un) && throw(DomainError("Characteristic must be prime: $n"))
   return GaloisField(un, cached)
end

function GF(n::UInt; cached::Bool=true)
   un = UInt(n)
   !is_prime(un) && throw(DomainError("Characteristic must be prime: $n"))
   return GaloisField(un, cached)
end

function GF(n::fmpz; cached::Bool=true)
   return ResidueField(FlintZZ, n, cached = cached)
end

################################################################################
#
#   Characteristic & order
#
################################################################################

@doc Markdown.doc"""
    characteristic(F::Generic.ResField{fmpz}) -> fmpz
> Return the characteristic of the given Galois field.
"""
characteristic(F::Generic.ResField{fmpz}) = modulus(F)

@doc Markdown.doc"""
    order(F::Generic.ResField{fmpz})-> fmpz
> Return the order, i.e. the number of elements in, the given Galois field.
"""
order(F::Generic.ResField{fmpz}) = modulus(F)
