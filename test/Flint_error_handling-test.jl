@testset "Flint_error_handling.showerror" begin
  e = FlintException(Nemo.FLINT_ERROR, "I am the error message")
  @test sprint(showerror, e) == "Flint Exception (General error):\nI am the error message"

  e = FlintException(Nemo.FLINT_OVERFLOW, "I am the error message")
  @test sprint(showerror, e) == "Flint Exception (Overflow):\nI am the error message"

  e = FlintException(Nemo.FLINT_IMPINV, "I am the error message")
  @test sprint(showerror, e) == "Flint Exception (Impossible inverse):\nI am the error message"

  e = FlintException(Nemo.FLINT_DOMERR, "I am the error message")
  @test sprint(showerror, e) == "Flint Exception (Domain error):\nI am the error message"

  e = FlintException(Nemo.FLINT_DIVZERO, "I am the error message")
  @test sprint(showerror, e) == "Flint Exception (Divide by zero):\nI am the error message"

  e = FlintException(Nemo.FLINT_EXPOF, "I am the error message")
  @test sprint(showerror, e) == "Flint Exception (Exponent overflow):\nI am the error message"

  e = FlintException(Nemo.FLINT_INEXACT, "I am the error message")
  @test sprint(showerror, e) == "Flint Exception (Inexact):\nI am the error message"

  e = FlintException(Nemo.FLINT_TEST_FAIL, "I am the error message")
  @test sprint(showerror, e) == "Flint Exception (Test failure):\nI am the error message"
end

@testset "Flint_error_handling.throw_error" begin
  # without interpolation
  @test_throws FlintException(Nemo.FLINT_ERROR, "I am the error message") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_ERROR::Int, "I am the error message"::Cstring
  )::Nothing
  @test_throws FlintException(Nemo.FLINT_OVERFLOW, "I am the error message") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_OVERFLOW::Int, "I am the error message"::Cstring
  )::Nothing
  @test_throws FlintException(Nemo.FLINT_IMPINV, "I am the error message") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_IMPINV::Int, "I am the error message"::Cstring
  )::Nothing
  @test_throws FlintException(Nemo.FLINT_DOMERR, "I am the error message") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_DOMERR::Int, "I am the error message"::Cstring
  )::Nothing
  @test_throws FlintException(Nemo.FLINT_DIVZERO, "I am the error message") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_DIVZERO::Int, "I am the error message"::Cstring
  )::Nothing
  @test_throws FlintException(Nemo.FLINT_EXPOF, "I am the error message") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_EXPOF::Int, "I am the error message"::Cstring
  )::Nothing
  @test_throws FlintException(Nemo.FLINT_INEXACT, "I am the error message") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_INEXACT::Int, "I am the error message"::Cstring
  )::Nothing
  @test_throws FlintException(Nemo.FLINT_TEST_FAIL, "I am the error message") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_TEST_FAIL::Int, "I am the error message"::Cstring
  )::Nothing

  # with basic interpolation
  @test_throws FlintException(Nemo.FLINT_ERROR, "Foo 42 bar") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_ERROR::Int, "Foo %d bar"::Cstring; 42::Cint
  )::Nothing
  @test_throws FlintException(Nemo.FLINT_ERROR, "Foo 42  9 bar") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_ERROR::Int, "Foo %d %2d bar"::Cstring; 42::Cint, 9::Cint
  )::Nothing
  @test_throws FlintException(Nemo.FLINT_ERROR, "Foo foobar bar") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_ERROR::Int, "Foo %s bar"::Cstring; "foobar"::Cstring
  )::Nothing

  # with flint interpolation
  @test_throws FlintException(Nemo.FLINT_ERROR, "Foo 42 bar") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_ERROR::Int, "Foo %{fmpz} bar"::Cstring; ZZ(42)::Ref{ZZRingElem}
  )::Nothing
  @test_throws FlintException(Nemo.FLINT_ERROR, "Foo 1267650600228229401496703205376 bar") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_ERROR::Int, "Foo %{fmpz} bar"::Cstring; (ZZ(2)^100)::Ref{ZZRingElem}
  )::Nothing
  @test_throws FlintException(Nemo.FLINT_ERROR, "Foo 2 / 3 bar") @ccall Nemo.libflint.flint_throw(
    Nemo.FLINT_ERROR::Int, "Foo %{fmpq} bar"::Cstring; QQ(2, 3)::Ref{QQFieldElem}
  )::Nothing
end

@testset "Flint_error_handling.real-world-example" begin
  let
    @test_throws "Flint Exception (Impossible inverse):\nCannot invert modulo 3*50" @ccall Nemo.libflint.n_invmod(3::UInt, 150::UInt)::UInt
  end

  let
    R, = Native.finite_field(42; check=false) # create a corrupt finite field to actually trigger errors
    @test_throws "Flint Exception (Impossible inverse):\nCannot invert modulo 6*7" inv(R(12))
  end

  let 
    R, = residue_ring(ZZ, 42)
    Rx, x = polynomial_ring(R, 3)
    # FLINT only supports this for prime moduli
    @test_throws "Flint Exception (Impossible inverse):\nnmod_mpoly_divides: leading coefficient is not invertible." divides(6x[1], 3x[1])
  end

  let
    @test_throws "Flint Exception (General error):\nException (fmpz_divexact). Division by zero." divexact!(ZZ(), ZZ(5), ZZ(0))
  end
end
