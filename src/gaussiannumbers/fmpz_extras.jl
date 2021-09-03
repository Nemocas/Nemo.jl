# stuff that should be in fmpz/fmpq

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

function pow!(z::fmpz, a::fmpz, b::Int)
  ccall((:fmpz_pow_ui, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, UInt),
        z, a, UInt(b))
  return z
end

function numerator!(z::fmpz, x::fmpq)
  ccall((:fmpq_numerator, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpq}),
        z, x)
  return z
end

function denominator!(z::fmpz, x::fmpq)
  ccall((:fmpq_denominator, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpq}),
        z, x)
  return z
end

function one!(z::fmpq)
  ccall((:fmpq_set_si, libflint), Nothing,
        (Ref{fmpq}, Int, UInt),
        z, Int(1), UInt(1))
  return z
end

function add!(z::fmpq, a::fmpq, b::Int)
  ccall((:fmpq_add_si, libflint), Nothing,
        (Ref{fmpq}, Ref{fmpq}, Int),
        z, a, b)
end

function divexact!(z::fmpz, a::fmpz, b::fmpz)
  @assert divides(a, b)[1]
  ccall((:fmpz_divexact, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
        z, a, b)
  return z
end

function sub!(z::fmpz, a::fmpz, b::fmpz)
  ccall((:fmpz_sub, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
        z, a, b)
  return z
end

function addmul!(z::fmpq, a::fmpq, b::fmpq)
   ccall((:fmpq_addmul, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpq}),
         z, a, b)
   return z
end

function submul!(z::fmpz, a::fmpz, b::fmpz)
   ccall((:fmpz_submul, libflint), Nothing,
         (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}),
         z, a, b)
   return z
end

function submul!(z::fmpq, a::fmpq, b::fmpq)
   ccall((:fmpq_submul, libflint), Nothing,
         (Ref{fmpq}, Ref{fmpq}, Ref{fmpq}),
         z, a, b)
   return z
end

function mul!(z::fmpz, a::fmpz, b::Int)
  ccall((:fmpz_mul_si, libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, Int),
        z, a, b)
  return z
end

function mul!(z::fmpq, a::fmpq, b::Int)
  ccall((:fmpq_mul_si, libflint), Nothing,
        (Ref{fmpq}, Ref{fmpq}, Int),
        z, a, b)
  return z
end
