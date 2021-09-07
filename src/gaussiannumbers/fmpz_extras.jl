# stuff that should be in fmpz

function swap!(a::fmpz, b::fmpz)
  ccall((:fmpz_swap, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}),
        a, b)
end

function one!(z::fmpz)
  ccall((:fmpz_set_ui, libflint), Nothing,
        (Ref{fmpz}, UInt),
        z, 1)
  return z
end

function set!(z::fmpz, a::fmpz)
  ccall((:fmpz_set, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}),
        z, a)
  return z
end

function neg!(z::fmpz, a::fmpz)
  ccall((:fmpz_neg, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}),
        z, a)
  return z
end

function add!(z::fmpz, a::fmpz, b::Int)
  ccall((:fmpz_add_si, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, Int),
        z, a, b)
  return z
end

function add!(z::fmpz, a::fmpz, b::UInt)
  ccall((:fmpz_add_ui, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, UInt),
        z, a, b)
  return z
end

function add!(z::fmpz, a::fmpz, b::Integer)
  return add!(z, a, fmpz(b))
end

function sub!(z::fmpz, a::fmpz, b::fmpz)
  ccall((:fmpz_sub, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
        z, a, b)
  return z
end

function sub!(z::fmpz, a::fmpz, b::Int)
  ccall((:fmpz_sub_si, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, Int),
        z, a, b)
  return z
end

function sub!(z::fmpz, a::fmpz, b::UInt)
  ccall((:fmpz_sub_ui, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, UInt),
        z, a, b)
  return z
end

function sub!(z::fmpz, a::fmpz, b::Integer)
  return sub!(z, a, fmpz(b))
end

function sub!(z::fmpz, b::Integer, a::fmpz)
  sub!(z, a, b)
  return neg!(z, z)
end

function mul!(z::fmpz, a::fmpz, b::Int)
  ccall((:fmpz_mul_si, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, Int),
        z, a, b)
  return z
end

function mul!(z::fmpz, a::fmpz, b::UInt)
  ccall((:fmpz_mul_ui, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, UInt),
        z, a, b)
  return z
end

function mul!(z::fmpz, a::fmpz, b::Integer)
  return mul!(z, a, fmpz(b))
end

function submul!(z::fmpz, a::fmpz, b::fmpz)
   ccall((:fmpz_submul, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
         z, a, b)
   return z
end

function divexact!(z::fmpz, a::fmpz, b::fmpz)
  ccall((:fmpz_divexact, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
        z, a, b)
  return z
end

function pow!(z::fmpz, a::fmpz, b::Union{Int, UInt})
  ccall((:fmpz_pow_ui, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, UInt),
        z, a, UInt(b))
  return z
end

# stuff that should be in fmpq

function Base.convert(::Type{Rational{T}}, a::fmpq) where T <: Integer
  return Rational{T}(convert(T, numerator(a)), convert(T, denominator(a)))
end

function Base.convert(::Type{fmpq}, a::Rational{T}) where T <: Integer
  return convert(fmpz, numerator(a))//convert(fmpz, denominator(a))
end


