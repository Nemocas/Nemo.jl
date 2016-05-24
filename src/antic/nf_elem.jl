###############################################################################
#
#   nf_elem.jl : Antic number fields
#
###############################################################################

export AnticNumberField, norm, trace, CyclotomicField, MaximalRealSubfield,
       add!, sub!, mul!, signature

###############################################################################
#
#   Type and parent object methods
#
###############################################################################

parent_type(::Type{nf_elem}) = AnticNumberField

parent(a::nf_elem) = a.parent

elem_type(::AnticNumberField) = nf_elem

base_ring(a::AnticNumberField) = Union{}

var(a::AnticNumberField) = a.S

function check_parent(a::nf_elem, b::nf_elem)
   a.parent != b.parent && error("Incompatible number field elements")
end

###############################################################################
#
#   Basic manipulation
#
###############################################################################

function hash(a::nf_elem, h::UInt)
   b = 0xc2a44fbe466a1827
   d = den(a)
   b = hash(d, b)
   for i in 1:degree(parent(a)) + 1
         num_coeff!(d, a, i)
         b $= hash(d, h) $ h
         b = (b << 1) | (b >> (sizeof(Int)*8 - 1))
   end
   return b
end

function coeff(x::nf_elem, n::Int)
   n < 0 && throw(DomainError())
   z = fmpq()
   ccall((:nf_elem_get_coeff_fmpq, :libflint), Void,
     (Ptr{fmpq}, Ptr{nf_elem}, Int, Ptr{AnticNumberField}), &z, &x, n, &parent(x))
   return z
end

function num_coeff!(z::fmpz, x::nf_elem, n::Int)
   n < 0 && throw(DomainError())
   ccall((:nf_elem_get_coeff_fmpz, :libflint), Void,
     (Ptr{fmpq}, Ptr{nf_elem}, Int, Ptr{AnticNumberField}), &z, &x, n, &parent(x))
   return z
end



function gen(a::AnticNumberField)
   r = nf_elem(a)
   ccall((:nf_elem_gen, :libflint), Void,
         (Ptr{nf_elem}, Ptr{AnticNumberField}), &r, &a)
   return r
end

function one(a::AnticNumberField)
   r = nf_elem(a)
   ccall((:nf_elem_one, :libflint), Void,
         (Ptr{nf_elem}, Ptr{AnticNumberField}), &r, &a)
   return r
end

function zero(a::AnticNumberField)
   r = nf_elem(a)
   ccall((:nf_elem_zero, :libflint), Void,
         (Ptr{nf_elem}, Ptr{AnticNumberField}), &r, &a)
   return r
end

function isgen(a::nf_elem)
   return ccall((:nf_elem_is_gen, :libflint), Bool,
                (Ptr{nf_elem}, Ptr{AnticNumberField}), &a, &a.parent)
end

function isone(a::nf_elem)
   return ccall((:nf_elem_is_one, :libflint), Bool,
                (Ptr{nf_elem}, Ptr{AnticNumberField}), &a, &a.parent)
end

function iszero(a::nf_elem)
   return ccall((:nf_elem_is_zero, :libflint), Bool,
                (Ptr{nf_elem}, Ptr{AnticNumberField}), &a, &a.parent)
end

function den(a::nf_elem)
   z = fmpz()
   ccall((:nf_elem_get_den, :libflint), Void,
         (Ptr{fmpz}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &z, &a, &a.parent)
   return z
end

function elem_from_mat_row(a::AnticNumberField, b::fmpz_mat, i::Int, d::fmpz)
   _checkbounds(b.parent.rows, i) || throw(BoundsError())
   b.parent.cols == degree(a) || error("Wrong number of columns")
   z = a()
   ccall((:nf_elem_set_fmpz_mat_row, :libflint), Void,
        (Ptr{nf_elem}, Ptr{fmpz_mat}, Cint, Ptr{fmpz}, Ptr{AnticNumberField}),
        &z, &b, i - 1, &d, &a)
   return z
end

function elem_to_mat_row!(a::fmpz_mat, i::Int, d::fmpz, b::nf_elem)
   ccall((:nf_elem_get_fmpz_mat_row, :libflint), Void,
         (Ptr{fmpz_mat}, Cint, Ptr{fmpz}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &a, i - 1, &d, &b, &b.parent)
   nothing
 end

degree(a::AnticNumberField) = degree(a.pol)

signature(a::AnticNumberField) = signature(a.pol)

function deepcopy(d::nf_elem)
   z = nf_elem(parent(d), d)
   return z
end

###############################################################################
#
#   AbstractString{} I/O
#
###############################################################################

function show(io::IO, a::AnticNumberField)
   print(io, "Number field over Rational Field")
   print(io, " with defining polynomial ", a.pol)
end

function show(io::IO, x::nf_elem)
   cstr = ccall((:nf_elem_get_str_pretty, :libflint), Ptr{UInt8},
                (Ptr{nf_elem}, Ptr{UInt8}, Ptr{AnticNumberField}),
                 &x, bytestring(string(var(parent(x)))), &parent(x))

   print(io, bytestring(cstr))

   ccall((:flint_free, :libflint), Void, (Ptr{UInt8},), cstr)
end

needs_parentheses(::Nemo.nf_elem) = true

is_negative(::nf_elem) = false

###############################################################################
#
#   Unary operators
#
###############################################################################

function -(a::nf_elem)
   r = a.parent()
   ccall((:nf_elem_neg, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &r, &a, &a.parent)
   return r
end

###############################################################################
#
#   Binary operators
#
###############################################################################

function +(a::nf_elem, b::nf_elem)
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_add, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

function -(a::nf_elem, b::nf_elem)
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_sub, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

function *(a::nf_elem, b::nf_elem)
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_mul, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

###############################################################################
#
#   Ad hoc binary operators
#
###############################################################################

function +(a::nf_elem, b::Int)
   r = a.parent()
   ccall((:nf_elem_add_si, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Int, Ptr{AnticNumberField}),
         &r, &a, b, &a.parent)
   return r
end

function +(a::nf_elem, b::fmpz)
   r = a.parent()
   ccall((:nf_elem_add_fmpz, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpz}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

function +(a::nf_elem, b::fmpq)
   r = a.parent()
   ccall((:nf_elem_add_fmpq, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpq}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

function -(a::nf_elem, b::Int)
   r = a.parent()
   ccall((:nf_elem_sub_si, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Int, Ptr{AnticNumberField}),
         &r, &a, b, &a.parent)
   return r
end

function -(a::nf_elem, b::fmpz)
   r = a.parent()
   ccall((:nf_elem_sub_fmpz, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpz}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

function -(a::nf_elem, b::fmpq)
   r = a.parent()
   ccall((:nf_elem_sub_fmpq, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpq}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

function -(a::Int, b::nf_elem)
   r = b.parent()
   ccall((:nf_elem_si_sub, :libflint), Void,
         (Ptr{nf_elem}, Int, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &r, a, &b, &b.parent)
   return r
end

function -(a::fmpz, b::nf_elem)
   r = b.parent()
   ccall((:nf_elem_fmpz_sub, :libflint), Void,
         (Ptr{nf_elem}, Ptr{fmpz}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &r, &a, &b, &b.parent)
   return r
end

function -(a::fmpq, b::nf_elem)
   r = b.parent()
   ccall((:nf_elem_fmpq_sub, :libflint), Void,
         (Ptr{nf_elem}, Ptr{fmpq}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &r, &a, &b, &b.parent)
   return r
end

+(a::nf_elem, b::Integer) = a + fmpz(b)

-(a::nf_elem, b::Integer) = a - fmpz(b)

-(a::Integer, b::nf_elem) = fmpz(a) - b

+(a::Integer, b::nf_elem) = b + a

+(a::fmpq, b::nf_elem) = b + a

function *(a::nf_elem, b::Int)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_si, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Int, Ptr{AnticNumberField}),
         &r, &a, b, &a.parent)
   return r
end

function *(a::nf_elem, b::fmpz)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_fmpz, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpz}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

function *(a::nf_elem, b::fmpq)
   r = a.parent()
   ccall((:nf_elem_scalar_mul_fmpq, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpq}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

*(a::Integer, b::nf_elem) = b * a

*(a::fmpq, b::nf_elem) = b * a

//(a::nf_elem, b::Int) = divexact(a, b)

//(a::nf_elem, b::fmpz) = divexact(a, b)

//(a::nf_elem, b::Integer) = a//fmpz(b)

//(a::nf_elem, b::fmpq) = divexact(a, b)

###############################################################################
#
#   Powering
#
###############################################################################

function ^(a::nf_elem, n::Int)
   r = a.parent()
   ccall((:nf_elem_pow, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Int, Ptr{AnticNumberField}),
         &r, &a, abs(n), &a.parent)
   if n < 0
      r = inv(r)
   end
   return r
end

###############################################################################
#
#   Comparison
#
###############################################################################

function ==(a::nf_elem, b::nf_elem)
   check_parent(a, b)
   return ccall((:nf_elem_equal, :libflint), Bool,
           (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{AnticNumberField}), &a, &b, &a.parent)
end

###############################################################################
#
#   Inversion
#
###############################################################################

function inv(a::nf_elem)
   iszero(a) && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_inv, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &r, &a, &a.parent)
   return r
end

###############################################################################
#
#   Exact division
#
###############################################################################

function divexact(a::nf_elem, b::nf_elem)
   iszero(b) && throw(DivideError())
   check_parent(a, b)
   r = a.parent()
   ccall((:nf_elem_div, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

###############################################################################
#
#   Ad hoc exact division
#
###############################################################################

function divexact(a::nf_elem, b::Int)
   b == 0 && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_si, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Int, Ptr{AnticNumberField}),
         &r, &a, b, &a.parent)
   return r
end

function divexact(a::nf_elem, b::fmpz)
   b == 0 && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_fmpz, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpz}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

function divexact(a::nf_elem, b::fmpq)
   b == 0 && throw(DivideError())
   r = a.parent()
   ccall((:nf_elem_scalar_div_fmpq, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpq}, Ptr{AnticNumberField}),
         &r, &a, &b, &a.parent)
   return r
end

###############################################################################
#
#   Norm and trace
#
###############################################################################

function norm(a::nf_elem)
   z = fmpq()
   ccall((:nf_elem_norm, :libflint), Void,
         (Ptr{fmpq}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &z, &a, &a.parent)
   return z
end

function trace(a::nf_elem)
   z = fmpq()
   ccall((:nf_elem_trace, :libflint), Void,
         (Ptr{fmpq}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &z, &a, &a.parent)
   return z
end

###############################################################################
#
#   Unsafe functions
#
###############################################################################

function mul!(z::nf_elem, x::nf_elem, y::nf_elem)
   ccall((:nf_elem_mul, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{nf_elem}, Ptr{AnticNumberField}),
                                                  &z, &x, &y, &parent(x))
end

function mul_red!(z::nf_elem, x::nf_elem, y::nf_elem, red::Bool)
   ccall((:nf_elem_mul_red, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{nf_elem}, Ptr{AnticNumberField}, Cint),
                                                &z, &x, &y, &parent(x), red)
end

function addeq!(z::nf_elem, x::nf_elem)
   ccall((:nf_elem_add, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{nf_elem}, Ptr{AnticNumberField}),
                                                  &z, &z, &x, &parent(x))
end

function add!(a::nf_elem, b::nf_elem, c::nf_elem)
   ccall((:nf_elem_add, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &a, &b, &c, &a.parent)
end

function reduce!(x::nf_elem)
   ccall((:nf_elem_reduce, :libflint), Void,
         (Ptr{nf_elem}, Ptr{AnticNumberField}), &x, &parent(x))
end

###############################################################################
#
#   Ad hoc unsafe functions
#
###############################################################################

function add!(c::nf_elem, a::nf_elem, b::fmpq)
   ccall((:nf_elem_add_fmpq, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpq}, Ptr{AnticNumberField}),
         &c, &a, &b, &a.parent)
end

function add!(c::nf_elem, a::nf_elem, b::fmpz)
   ccall((:nf_elem_add_fmpz, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpz}, Ptr{AnticNumberField}),
         &c, &a, &b, &a.parent)
end

function add!(c::nf_elem, a::nf_elem, b::Int)
   ccall((:nf_elem_add_si, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Int, Ptr{AnticNumberField}),
         &c, &a, &b, &a.parent)
end

add!(c::nf_elem, a::nf_elem, b::Integer) = add!(c, a, fmpz(b))

function sub!(c::nf_elem, a::nf_elem, b::fmpq)
   ccall((:nf_elem_sub_fmpq, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpq}, Ptr{AnticNumberField}),
         &c, &a, &b, &a.parent)
end

function sub!(c::nf_elem, a::nf_elem, b::fmpz)
   ccall((:nf_elem_sub_fmpz, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpz}, Ptr{AnticNumberField}),
         &c, &a, &b, &a.parent)
end

function sub!(c::nf_elem, a::nf_elem, b::Int)
   ccall((:nf_elem_sub_si, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Int, Ptr{AnticNumberField}),
         &c, &a, &b, &a.parent)
end

sub!(c::nf_elem, a::nf_elem, b::Integer) = sub!(c, a, fmpz(b))

function sub!(c::nf_elem, a::fmpq, b::nf_elem)
   ccall((:nf_elem_fmpq_sub, :libflint), Void,
         (Ptr{nf_elem}, Ptr{fmpq}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &c, &a, &b, &a.parent)
end

function sub!(c::nf_elem, a::fmpz, b::nf_elem)
   ccall((:nf_elem_fmpz_sub, :libflint), Void,
         (Ptr{nf_elem}, Ptr{fmpz}, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &c, &a, &b, &a.parent)
end

function sub!(c::nf_elem, a::Int, b::nf_elem)
   ccall((:nf_elem_si_sub, :libflint), Void,
         (Ptr{nf_elem}, Int, Ptr{nf_elem}, Ptr{AnticNumberField}),
         &c, &a, &b, &a.parent)
end

sub!(c::nf_elem, a::Integer, b::nf_elem) = sub!(c, fmpz(a), b)

function mul!(c::nf_elem, a::nf_elem, b::fmpq)
   ccall((:nf_elem_scalar_mul_fmpq, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpq}, Ptr{AnticNumberField}),
         &c, &a, &b, &a.parent)
end

function mul!(c::nf_elem, a::nf_elem, b::fmpz)
   ccall((:nf_elem_scalar_mul_fmpz, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Ptr{fmpz}, Ptr{AnticNumberField}),
         &c, &a, &b, &a.parent)
end

function mul!(c::nf_elem, a::nf_elem, b::Int)
   ccall((:nf_elem_scalar_mul_si, :libflint), Void,
         (Ptr{nf_elem}, Ptr{nf_elem}, Int, Ptr{AnticNumberField}),
         &c, &a, b, &a.parent)
end

mul!(c::nf_elem, a::nf_elem, b::Integer) = mul!(c, a, fmpz(b))

###############################################################################
#
#   Speedups for polynomials over number fields
#
###############################################################################

function sqr_classical(a::GenPoly{nf_elem})
   lena = length(a)

   t = base_ring(a)()

   lenz = 2*lena - 1
   d = Array(nf_elem, lenz)

   for i = 1:lena - 1
      d[2i - 1] = base_ring(a)()
      d[2i] = base_ring(a)()
      mul_red!(d[2i - 1], coeff(a, i - 1), coeff(a, i - 1), false)
   end
   d[2*lena - 1] = base_ring(a)()
   mul_red!(d[2*lena - 1], coeff(a, lena - 1), coeff(a, lena - 1), false)

   for i = 1:lena
      for j = i + 1:lena
         mul_red!(t, coeff(a, i - 1), coeff(a, j - 1), false)
         addeq!(d[i + j - 1], t)
         addeq!(d[i + j - 1], t)
      end
   end

   for i = 1:lenz
      reduce!(d[i])
   end

   z = parent(a)(d)

   set_length!(z, normalise(z, lenz))

   return z
end

function mul_classical(a::GenPoly{nf_elem}, b::GenPoly{nf_elem})
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)

   if lena == 0 || lenb == 0
      return parent(a)()
   end

   if a == b
       return sqr_classical(a)
   end

   t = base_ring(a)()

   lenz = lena + lenb - 1
   d = Array(nf_elem, lenz)

   for i = 1:lena
      d[i] = base_ring(a)()
      mul_red!(d[i], coeff(a, i - 1), coeff(b, 0), false)
   end

   for i = 2:lenb
      d[lena + i - 1] = base_ring(a)()
      mul_red!(d[lena + i - 1], a.coeffs[lena], coeff(b, i - 1), false)
   end

   for i = 1:lena - 1
      for j = 2:lenb
         mul_red!(t, coeff(a, i - 1), b.coeffs[j], false)
         addeq!(d[i + j - 1], t)
      end
   end

   for i = 1:lenz
      reduce!(d[i])
   end

   z = parent(a)(d)

   set_length!(z, normalise(z, lenz))

   return z
end

function *(a::GenPoly{nf_elem}, b::GenPoly{nf_elem})
   check_parent(a, b)
   lena = length(a)
   lenb = length(b)
   if min(lena, lenb) < 20
      return mul_classical(a, b)
   end
   lenr = lena + lenb - 1
   r = parent(a)()
   if lena == 0 || lenb == 0
      return r
   end
   pol = base_ring(a).pol
   K = base_ring(a)
   R = parent(pol)
   T = elem_type(R)
   S = GenPolyRing{T}(R, :y)
   f = S()
   fit!(f, lena)
   for i = 1:lena
      setcoeff!(f, i - 1, R(coeff(a, i - 1)))
   end
   set_length!(f, lena)
   if a !== b
      g = S()
      fit!(g, lenb)
      for i = 1:lenb
         setcoeff!(g, i - 1, R(coeff(b, i - 1)))
      end
      set_length!(g, lenb)
   else
      g = f
   end
   p = f*g
   fit!(r, lenr)
   for i = 1:lenr
      r.coeffs[i] = K(p.coeffs[i])
   end
   set_length!(r, normalise(r, lenr))
   return r
end

###############################################################################
#
#   Promotions
#
###############################################################################

Base.promote_rule{T <: Integer}(::Type{nf_elem}, ::Type{T}) = nf_elem

Base.promote_rule(::Type{nf_elem}, ::Type{fmpz}) = nf_elem

Base.promote_rule(::Type{nf_elem}, ::Type{fmpq}) = nf_elem

Base.promote_rule(::Type{nf_elem}, ::Type{fmpq_poly}) = nf_elem

###############################################################################
#
#   Parent object call overloads
#
###############################################################################

function Base.call(a::AnticNumberField)
   z = nf_elem(a)
   ccall((:nf_elem_set_si, :libflint), Void,
         (Ptr{nf_elem}, Int, Ptr{AnticNumberField}), &z, 0, &a)
   return z
end

function Base.call(a::AnticNumberField, c::Int)
   z = nf_elem(a)
   ccall((:nf_elem_set_si, :libflint), Void,
         (Ptr{nf_elem}, Int, Ptr{AnticNumberField}), &z, c, &a)
   return z
end

function Base.call(a::AnticNumberField, c::fmpz)
   z = nf_elem(a)
   ccall((:nf_elem_set_fmpz, :libflint), Void,
         (Ptr{nf_elem}, Ptr{fmpz}, Ptr{AnticNumberField}), &z, &c, &a)
   return z
end

function Base.call(a::AnticNumberField, c::fmpq)
   z = nf_elem(a)
   ccall((:nf_elem_set_fmpq, :libflint), Void,
         (Ptr{nf_elem}, Ptr{fmpq}, Ptr{AnticNumberField}), &z, &c, &a)
   return z
end

function Base.call(a::AnticNumberField, b::nf_elem)
   parent(b) != a && error("Cannot coerce number field element")
   return b
end

function Base.call(a::AnticNumberField, pol::fmpq_poly)
   pol = parent(a.pol)(pol) # check pol has correct parent
   z = nf_elem(a)
   if length(pol) >= length(a.pol)
      pol = mod(pol, a.pol)
   end
   ccall((:nf_elem_set_fmpq_poly, :libflint), Void,
         (Ptr{nf_elem}, Ptr{fmpq_poly}, Ptr{AnticNumberField}), &z, &pol, &a)
   return z
end

function Base.call(a::FmpqPolyRing, b::nf_elem)
   parent(parent(b).pol) != a && error("Cannot coerce from number field to polynomial ring")
   r = a()
   ccall((:nf_elem_get_fmpq_poly, :libflint), Void,
         (Ptr{fmpq_poly}, Ptr{nf_elem}, Ptr{AnticNumberField}), &r, &b, &parent(b))
   return r
end

###############################################################################
#
#   AnticNumberField constructor
#
###############################################################################

function AnticNumberField(pol::fmpq_poly, s::AbstractString{})
   S = Symbol(s)
   parent_obj = AnticNumberField(pol, S)

   return parent_obj, gen(parent_obj)
end

function AnticCyclotomicField(n::Int, s::AbstractString{}, t = "\$")
   Zx, x = PolynomialRing(FlintZZ, string(gensym()))
   Qx, = PolynomialRing(FlintQQ, t)
   f = cyclotomic(n, x)
   return AnticNumberField(Qx(f), s)
end

function AnticMaximalRealSubfield(n::Int, s::AbstractString{}, t = "\$")
   Zx, x = PolynomialRing(FlintZZ, string(gensym()))
   Qx, = PolynomialRing(FlintQQ, t)
   f = cos_minpoly(n, x)
   return AnticNumberField(Qx(f), s)
end
