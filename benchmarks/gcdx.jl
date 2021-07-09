function gcdx_bigint(a::fmpz, b::fmpz)
  g, s, t = gcdx(BigInt(a), BigInt(b))
  return fmpz(g), fmpz(s), fmpz(t)
end

function gcdx_fmpz(a::fmpz, b::fmpz)
  d = ZZ()
  x = ZZ()
  y = ZZ()
  ccall((:fmpz_xgcd_canonical_bezout, :libflint), Nothing,
        (Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}, Ref{fmpz}), d, x, y, a, b)
  return d, x, y
end

function run_gcdx_bigint(x::Vector{fmpz}, y::Vector{fmpz})
  for ix in x, iy in y
    gcdx_bigint(ix, iy)
  end
end
    
function run_gcdx_fmpz(x::Vector{fmpz}, y::Vector{fmpz})
  for ix in x, iy in y
    gcdx_fmpz(ix, iy)
  end
end

function benchmark_gcdx()
  print("benchmark_gcdx ...\n")

  # small size
  range = ZZ(0):ZZ(2)^(Sys.WORD_SIZE - 2) - 1
  x = [rand(range) for _ in 1:100]
  y = [rand(range) for _ in 1:100]

  tt = @elapsed run_gcdx_bigint(x, y)
  println("Small sized integers for BigInt-solution: $tt")
  tt = @elapsed run_gcdx_fmpz(x, y)
  println("Small sized integers for fmpz-solution:   $tt")

  # mixed integers
  range = ZZ(0):ZZ(2)^Sys.WORD_SIZE
  x = [rand(range) for _ in 1:100]
  y = [rand(range) for _ in 1:100]

  tt = @elapsed run_gcdx_bigint(x, y)
  println("Mixed sized integers for BigInt-solution: $tt")
  tt = @elapsed run_gcdx_fmpz(x, y)
  println("Mixed sized integers for fmpz-solution:   $tt")

  # big integers
  range = ZZ(0):ZZ(2)^512
  x = [rand(range) for _ in 1:100]
  y = [rand(range) for _ in 1:100]

  tt = @elapsed run_gcdx_bigint(x, y)
  println("Large sized integers for BigInt-solution: $tt")
  tt = @elapsed run_gcdx_fmpz(x, y)
  println("Large sized integers for fmpz-solution:   $tt")
end
