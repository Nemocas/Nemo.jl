divexact!(z::T, x::T, y::T) where T <: Rational = divexact(x, y)

numerator!(z::T, x::Rational{T}) where T <: Integer = numerator(x)

denominator!(z::T, x::Rational{T}) where T <: Integer = denominator(x)
