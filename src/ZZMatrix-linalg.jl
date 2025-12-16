##################################################################
## Functions related to unimodularity verification
##################################################################

# ***SOURCE ALGORITHM***
# Refined implementation of method from
# Authors: C. Pauderis + A. Storjohann
# Title:   Detrministic Unimodularity Certification
# Where:   Proc ISSAC 2012, pp. 281--287
# See also: thesis of Colton Pauderis, 2013, Univ of Waterloo
#           Deterministic Unimodularity Certification and Applications for Integer Matrices


# add_verbosity_scope(:UnimodVerif) -- see func __init__ in src/Nemo.jl

#######################################################
# Low-level auxiliary functions -- should be elsewhere?
#######################################################

function _det(a::fpMatrix)
  a.r < 9 && return det(a).data  # inspired by FLINT source code
  #_det avoids a copy: det is computed destructively
  r = @ccall libflint._nmod_mat_det(a::Ref{fpMatrix})::UInt
  return r
end

function map_entries!(k::Nemo.fpField, a::fpMatrix, A::ZZMatrix)
  @ccall libflint.nmod_mat_set_mod(a::Ref{fpMatrix}, k.n::UInt)::Cvoid
  @ccall libflint.fmpz_mat_get_nmod_mat(a::Ref{fpMatrix}, A::Ref{ZZMatrix})::Cvoid
  a.base_ring = k  # exploiting that the internal repr is the indep of char
  return a
end

function change_base_ring!(k::Nemo.fpField, a::fpMatrix)
  @ccall libflint.nmod_mat_set_mod(a::Ref{fpMatrix}, k.n::UInt)::Cvoid
  a.base_ring = k  # exploiting that the internal repr is the indep of char
  return a
end

@doc raw"""
    is_unimodular(A::ZZMatrix)
    is_unimodular(A::ZZMatrix; algorithm=:auto)

Determine whether a square matrix `A` is unimodular, i.e. whether `abs(det(A)) == 1`.
The method used is either that of Pauderis--Storjohann or using CRT;
the choice is made based on cost estimates for the two approaches.

The kwarg `algorithm` may be specified to be `:CRT` or `:pauderis_storjohann`
to ensure that the corresponding underlying algorithm is used (after a quick
initial unimodularity check).  `:CRT` is better if the matrix is unimodular
and has an inverse containing large entries (or if it is not unimodular);
`:pauderis_storjohann` is asymptotically faster when the matrix is unimodular
and its inverse does not contain large entries, but it requires more space
for intermediate expressions.
"""
function is_unimodular(A::ZZMatrix; algorithm=:auto)
  # Call this function when no extra info about the matrix is available.
  # It does a preliminary check that det(A) is +/-1 modulo roughly 2^100.
  # If so, then delegate the complete check to is_unimodular_given_det_mod_m
  @req  is_square(A)  "Matrix must be square"
  @req (algorithm in [:auto, :CRT, :pauderis_storjohann])  "algorithm must be one of [:CRT, :pauderis_storjohann, :auto]"
  # Deal with two trivial cases
  if nrows(A) == 0
    return true
  end
  if nrows(A) == 1
    return (abs(A[1,1]) == 1);
  end
  # Compute det mod M -- may later include some more primes
  target_bitsize_modulus = 100 # magic number -- should not be too small!
  @vprintln(:UnimodVerif,1,"Quick first check: compute det by crt up to modulus with about $(target_bitsize_modulus) bits")
  p::Int = 2^20 # start point of primes for initial CRT trial -- det with modulus >= 2^22 is much slower
  M = ZZ(1)
  det_mod_m = 0  # 0 means uninitialized, o/w value is 1 or -1
  A_mod_p = identity_matrix(Nemo.Native.GF(2), nrows(A))
  while nbits(M) <= target_bitsize_modulus
    @vprint(:UnimodVerif,2,".")
    p = next_prime(p + rand(1:2^10), false) # increment by a random step to a prime
    ZZmodP = Nemo.Native.GF(p; cached = false, check = false)
    map_entries!(ZZmodP, A_mod_p, A)
    @vtime :UnimodVerif 2  det_mod_p = _det(A_mod_p)
    if det_mod_p != 1 && det_mod_p != p-1
      return false  # obviously not unimodular
    end
    if det_mod_m == 0 # first iteration: so just set the value of det_mod_m
      det_mod_m = (det_mod_p == 1) ? 1 : -1;
    elseif det_mod_m == 1
      if det_mod_p != 1
        return false  # obviously not unimodular
      end
    elseif det_mod_p == 1 # we know that here det_mod_m == -1
      return false  # obviously not unimodular
    end
    M = mul!(M, p)
  end
  return _is_unimodular_given_det_mod_m(A, det_mod_m, M; algorithm)
end #function


function _is_unimodular_given_det_mod_m(A::ZZMatrix, det_mod_m::Int, M::ZZRingElem; algorithm=:auto)
  # This UI is convenient for our sophisticated determinant algorithm.
  # We estimate cost of computing det by CRT and cost of doing Pauderis-Storjohann
  # unimodular verification then call whichever is likely to be faster
  # (but we avoid P-S if the space estimate is too large).
  # M is a modulus satisfying M > 2^30;
  # det_mod_m is det(A) mod M [!not checked!]: it is either +1 or -1.
  is_square(A) || throw(ArgumentError("Matrix must be square"))
  abs(det_mod_m) == 1 || throw(ArgumentError("det_mod_m must be +1 or -1"))
  M >  2^30 || throw(ArgumentError("modulus must be greater than 2^30"))
  (algorithm in [:auto, :CRT, :pauderis_storjohann]) || throw(ArgumentError("algorithm must be one of [:CRT, :pauderis_storjohann, :auto]"))
  # Deal with two trivial cases -- does this make sense here???
  nrows(A) == 0 && return true
  nrows(A) == 1 && return (abs(A[1,1]) == 1)
  @vprintln(:UnimodVerif,1,"is_unimodular_given_det_mod_m starting; det_mod_m=$(det_mod_m)")
  if algorithm == :pauderis_storjohann
    @vprintln(:UnimodVerif,1,"User specified Pauderis_Storjohann --> delegating")
    return _is_unimodular_Pauderis_Storjohann_Hensel(A)
  end
  n = ncols(A)
  Hrow = hadamard_bound2(A)
  Hcol = hadamard_bound2(transpose(A))
  DetBoundSq = min(Hrow, Hcol)
  Hbits = 1+div(1+nbits(DetBoundSq),2)
  @vprintln(:UnimodVerif,1,"Hadamard bound in bits $(Hbits)")
  if algorithm == :auto
    # Estimate whether better to "climb out" with CRT or use Pauderis-Storjohann
    CRT_speed_factor = 20.0  # scale factor is JUST A GUESS !!!  (how much faster CRT is compared to P-S)
    total_entry_size = sum(nbits,A)
    Estimate_CRT = ceil(((Hbits - nbits(M))*(2.5+total_entry_size/n^3))/CRT_speed_factor)  # total_entry_size/n is proportional to cost of reducing entries mod p
    EntrySize = maximum(abs, A)
    entry_size_bits = maximum(nbits, A)
    @vprintln(:UnimodVerif,1,"entry_size_bits = $(entry_size_bits)   log2(EntrySize) = $(ceil(log2(EntrySize)))")
    e = max(16, Int(2+ceil(2*log2(n)+log2(EntrySize))))  # see Condition in Fig 1, p.284 of Pauderis-Storjohann
    Estimate_PS = ceil(e*log(e)) # assumes soft-linear ZZ arith
    @vprintln(:UnimodVerif,1,"Est_CRT = $(Estimate_CRT)  and  Est_PS = $(Estimate_PS)")
    Space_estimate_PS = ZZ(n)^2 * e  # nbits of values, ignoring mem mgt overhead
    @vprintln(:UnimodVerif,1,"Space est PS: $(Space_estimate_PS)  [might be bytes]")
    if Estimate_PS < Estimate_CRT #=&& Space_estimate_PS < 2^32=#
      # Expected RAM requirement is not excessive, and
      # Pauderis-Storjohann is likely faster, so delegate to UniCertZ_hensel
      return _is_unimodular_Pauderis_Storjohann_Hensel(A)
    end
  end
  if algorithm == :CRT
    @vprintln(:UnimodVerif,1,"User specified CRT --> delegating")
  end
  @vprintln(:UnimodVerif,1,"UnimodularVerification: CRT loop")
  # Climb out with CRT:
  # in CRT loop start with 22 bit primes; if we reach threshold (empirical), jump to much larger primes.
  p = 2^21; stride = 2^10; threshold = 5000000;
  A_mod_p = zero_matrix(Nemo.Native.GF(2), nrows(A), ncols(A))
  while nbits(M) <= Hbits
    @vprint(:UnimodVerif,2,".")
    # Next lines increment p (by random step up to stride) to a prime not dividing M
    if p > threshold && p < threshold+stride
      @vprint(:UnimodVerif,2,"!!")
      p = 2^56; stride = 2^25;  # p = 2^56 is a good choice on my standard 64-bit machine
      continue
    end
    # advance to another prime which does not divide M:
    while true
      p = next_prime(p+rand(1:stride), false)
      if !is_divisible_by(M,p)
        break
      end
    end
    ZZmodP = Nemo.Native.GF(p; cached = false, check = false)
    map_entries!(ZZmodP, A_mod_p, A)
    det_mod_p = _det(A_mod_p) # type is UInt
    @vprintln(:UnimodVerif,3, "p=$(p)  det_mod_p=$(det_mod_p)")
    verified_mod_p = (det_mod_m == 1) ? (det_mod_p == 1) : (det_mod_p == p-1)
    if !verified_mod_p
      return false  # obviously not unimodular
    end
    M = mul!(M, p)
  end
  return true
end

#DoublePlusOne complex ##############################

#TODO: Hensel is slower than CRT if modulus is small(ish)

function _PSH_init(A::ZZMatrix)
  #computes 3 things:
  # modulus (X) in the paper
  # the inverse of A mod X
  # the exponent target for the lifting.

  n = nrows(A)
  EntrySize = maximum(abs, A)
  entry_size_bits = nbits(EntrySize)
  e = max(16, 2+ceil(Int, 2*log2(n)+entry_size_bits))
  ModulusLWB = ZZ(2)^e
  @vprintln(:UnimodVerif,1,"ModulusLWB is 2^$(e)")
  # Now look for prime: about 25 bits, and such that p^(2^sthg) is
  # just slightly bigger than ModulusLWB.  Bit-size of prime matters little.
  root_order = exp2(ceil(log2(e/25)))
  j = ceil(exp2(e/root_order))
  p = next_prime(Int(j))
  @vprintln(:UnimodVerif,1,"Hensel prime is p = $(p)")
  m = ZZ(p)
  ZZmodP = Nemo.Native.GF(p)
  B0 = lift(inv(map_entries(ZZmodP, A))) #produces pos. lift
  mod_sym!(B0, m)
  delta = identity_matrix(base_ring(A), n)
  A_mod_m = identity_matrix(base_ring(A), n)
  @vprintln(:UnimodVerif,1,"Starting stage 1 lifting")
  while (m < ModulusLWB)
    @vprintln(:UnimodVerif,2,"Loop 1: nbits(m) = $(nbits(m))");
    copy!(A_mod_m, A)
    mm = m^2
    mod_sym!(A_mod_m, mm)
    @vtime :UnimodVerif 2  mul!(delta, A_mod_m, B0)
    sub!(delta, delta, 1)
    divexact!(delta, m) # to make matrix product in line below cheaper
    @vtime :UnimodVerif 2 mul!(A_mod_m, B0, delta)
    mul!(A_mod_m, A_mod_m, m)
    sub!(B0, B0, A_mod_m)
    m = mm
    mod_sym!(B0, m)
  end
  @vprintln(:UnimodVerif,1,"Got stage 1 modular inverse: modulus has $(nbits(m)) bits; value is $(p)^$(Int(round(log2(m)/log2(p))))")

  # Hadamard bound brings little benefit; anyway, almost never lift all the way
###  H = min(hadamard_bound2(A), hadamard_bound2(transpose(A)))
###  println("nbits(H) = $(nbits(H))");
  # Compute max num iters in k
  k = (n-1)*(entry_size_bits+log2(n)/2) - (2*log2(n) + entry_size_bits -1)
  k = 2+nbits(ceil(Int,k/log2(m)))
  @vprintln(:UnimodVerif,1,"Stage 2: max num iters is k=$(k)")

  return m, B0, k
end


# THIS FUNCTION NOT OFFICIALLY DOCUMENTED -- not intended for public use.
# Pauderis-Storjohann unimodular verification/certification.
# We use Hensel lifting to obtain the modular inverse B0
# Seems to be faster than the CRT prototype (not incl. here)
# VERBOSITY via :UnimodVerif
function _is_unimodular_Pauderis_Storjohann_Hensel(A::ZZMatrix)
  @assert  is_square(A)
  n = nrows(A)
  m, B0, k = _PSH_init(A)

  ##  @assert is_one(lift(MatModM(B0*A)))
  # Originally: R = (I-A*B0)/m
  R = A*B0
  sub!(R, R, 1) #this is -R, but is_zero test is the same, and the loop starts by squaring
  divexact!(R, m)
  if is_zero(R)
    return true
  end

  #ZZMatrix and ZZModMatrix are the same type. The modulus is
  #always passed in as an extra argument when needed...
  #mul! for ZZModMatrix is mul, followed by reduce.
  #thus the code is not using ZZModMatrices at all...

  # Two workspace matrices to avoid repeated alloc/dealloc
  R_bar = zero_matrix(ZZ, n, n)
  M = zero_matrix(ZZ, n, n)

  @vprintln(:UnimodVerif,1,"Starting stage 2 lifting")
  for i in 0:k-1
    @vprintln(:UnimodVerif,1,"Stage 2 loop: i=$(i)")

    mul!(R_bar, R, R)
    #Next 3 lines do:  M = lift(B0_modm*MatModM(R_bar));
    mod_sym!(M, R_bar, m)
    mul!(M, B0, M)
    mod_sym!(M, m)
    # Next 3 lines do: R = (R_bar - A*M)/m; but with less memory allocation
    mul!(R, A, M)
    sub!(R, R_bar, R)
    divexact!(R, m)
    if is_zero(R)
      return true
    end
  end
  return false
end

#adaptive rational_reconstruction: if solution is unbalanced with
#denominator smaller than numerator
@doc raw"""
    _induce_rational_reconstruction(a::ZZMatrix, b::ZZRingElem; error_tolerant ::Bool = false, unbalanced::Bool = true) -> Bool, ZZMatrix, ZZRingElem

Apply rational reconstruction to all entries in the matrix `a` in the attempt
to find `D` (over ZZ) and `n` (in ZZ) such that `an - D` is divisible by `b`.
Should be used mainly if a common denominator is expected as `rational_reconstrucion`
  is called for each entry in `a` and the denominators are accumulated.
"""
function _induce_rational_reconstruction(a::ZZMatrix, b::ZZRingElem; error_tolerant ::Bool = false, unbalanced::Bool = false)
  @req !error_tolerant || !unbalanced "only one of `error_tolerant` and `unbalanced` can be used at a time"

  A = similar(a)

  T = ZZRingElem(Val(:raw))
  n = ZZRingElem(Val(:raw))
  d = ZZRingElem(Val(:raw))
  bN = ZZRingElem(Val(:raw))
  bD = ZZRingElem(Val(:raw))
  D = ZZRingElem(1)
  GC.@preserve a A begin
    for i=1:nrows(a)
      a_ptr = Nemo.mat_entry_ptr(a, i, 1)
      A_ptr = Nemo.mat_entry_ptr(A, i, 1)
      for j=1:ncols(a)
        Nemo.set!(T, a_ptr)
        Nemo.mul!(T, T, D)
        Nemo.mod!(T, T, b)
        if unbalanced
          fl = _ratrec!(n, d, T, b, bN, bD)
        else
          fl, n, d = _rational_reconstruction(T, b; error_tolerant)
        end
        fl || return fl, A, D
        if !isone(d)
          mul!(D, D, d)
          Nemo.mul!(A, A, d)
        end
        Nemo.set!(A_ptr, n)

        a_ptr += sizeof(ZZRingElem)
        A_ptr += sizeof(ZZRingElem)
      end
    end
  end
  Nemo._fmpz_clear_fn(bN)
  Nemo._fmpz_clear_fn(bD)
  Nemo._fmpz_clear_fn(n)
  Nemo._fmpz_clear_fn(d)
  Nemo._fmpz_clear_fn(T)

  return true, A, D
end

@doc raw"""
    _induce_rational_reconstruction_nosplit(a::ZZMatrix, b::ZZRingElem; error_tolerant ::Bool = false, unbalanced::Bool = true) -> Bool, QQMatrix

Apply rational reconstruction to all entries in the matrix `a` in the attempt
to find `D` (over QQ) such that `a - D` is divisible by `b`.

See also [`rational_reconstruction`](@ref) for an explanation of the parameters
  and [`induce_rational_reconstruction`](@ref) for a version returing the numerator matrix and the denominator seperately.
"""
function _induce_rational_reconstruction_nosplit(a::ZZMatrix, b::ZZRingElem; error_tolerant ::Bool = false, unbalanced::Bool = false)
  fl, n, d = _induce_rational_reconstruction( a, b; error_tolerant, unbalanced)
  D = matrix(QQ, n)*QQ(ZZ(1), d)
  return fl, D
end
 
function change_prime!(a::fpMatrix, p::UInt)
  @ccall libflint.nmod_mat_set_mod(a::Ref{fpMatrix}, p::UInt)::Nothing
end

function lift!(A::ZZMatrix, a::fpMatrix)
  @ccall libflint.fmpz_mat_set_nmod_mat(A::Ref{ZZMatrix}, a::Ref{fpMatrix})::Cvoid
end

mutable struct DixonCtx
  Ainv::fpMatrix
  p::UInt
  crt_primes::Vector{UInt}
  A_mod::Vector{fpMatrix}
  d_mod::fpMatrix
  y_mod::fpMatrix
  Ay_mod::fpMatrix
  Ay::ZZMatrix
  x::ZZMatrix
  d::ZZMatrix
  A::ZZMatrix
  function DixonCtx()
    return new()
  end
end

#copied from flint to allow the use of adaptive reconstruction,
function dixon_init(A::ZZMatrix, B::ZZMatrix; side::Symbol = :right)
  return dixon_init!(DixonCtx(), A, B; side)
end

function dixon_init!(D::DixonCtx, A::ZZMatrix, B::ZZMatrix; side::Symbol = :right)

  sB = size(B)
  if side == :right
    @assert nrows(B) == nrows(A)
  else
    sB = (sB[2], sB[1])
    @assert ncols(B) == ncols(A)
  end

  if isdefined(D, :A)
    @assert size(A) == size(D.A)
  end

  if isdefined(D, :x)
    @assert size(D.x) == sB
  end

  if side == :right
    if isdefined(D, :A)
      copy!(D.A, A)
    else
      D.A = A
    end
    ncB = ncols(B)
  else
    if isdefined(D, :A)
      transpose!(D.A, A)
    else
      D.A = transpose(A)
    end
    ncB = nrows(B)
  end

  n = nrows(D.A)

  p = next_prime(2^59, false)
  while true
    if isdefined(D, :Ainv)
      map_entries!(Native.GF(p, cached = false, check = false), D.Ainv, D.A)
    else
      D.Ainv = map_entries(Native.GF(p, cached = false, check = false), D.A)
    end
    try
      D.Ainv = inv(D.Ainv)
      break
    catch e
      if e != ErrorException || e.msg != "Matrix not invertible"
        rethrow(e)
      end
    end
    p = next_prime(p, false)
  end
  @assert p != 0
  D.p = p

  if !isdefined(D, :crt_primes)
    D.crt_primes = UInt[]
    D.A_mod = fpMatrix[]
  end

  k = Nemo.fpField(UInt(p), false)
  if !isdefined(D, :d_mod)
    D.d_mod = zero_matrix(k, n, ncB)
    D.y_mod = zero_matrix(k, n, ncB)
    D.Ay_mod = zero_matrix(k, n, ncB)
    D.Ay = zero_matrix(ZZ, n, ncB)
    D.x = zero_matrix(ZZ, n, ncB)
  end

  if ncB == 1
    return D
  end
  
  pr = ZZRingElem(1)
  xp = next_prime(p, false)
  mA = maximum(abs, A)
  maxA = mA *(p-1)*n*2

  while true
    push!(D.crt_primes, xp)
    push!(D.A_mod, map_entries(Nemo.fpField(UInt(xp), false), D.A))
    mul!(pr, pr, xp)
    if pr > maxA
      break
    end
    xp = next_prime(xp, false)
  end

  return D
end

function dixon_solve(D::DixonCtx, B::ZZMatrix; side::Symbol = :right, block::Int = 10) 
  if side == :right
    d = deepcopy(B)
    _B = B
  else
    d = transpose(B)
    _B = transpose(B) 
  end
  @assert ncols(D.A) == nrows(d)
  @assert size(D.x) == size(d)
  #we're solveing Ax=B, always. if side = :left, then the interface transposes
  zero!(D.x)
 
  mA = maximum(abs, D.A)
  mB = maximum(abs, B)
  n = nrows(D.A)
  #bound for the solution is hadamard/ cramer
  bound = max((n-1)*mA^2+mB^2, n*mA^2)^n * 2^30

  ppow = ZZRingElem(1)
  i = 1
  nexti = 2
  while ppow <= bound
    map_entries!(Nemo.fpField(D.p, false), D.d_mod, d)

    Nemo.mul!(D.y_mod, D.Ainv, D.d_mod)
    @ccall libflint.fmpz_mat_scalar_addmul_nmod_mat_fmpz(D.x::Ref{ZZMatrix}, D.y_mod::Ref{fpMatrix}, ppow::Ref{ZZRingElem})::Cvoid

    Nemo.mul!(ppow, ppow, D.p)
    
    if ppow > bound
      break
    end

    stabilised = i == nexti;
    i += 1

    if stabilised
      nexti = ceil(Int,(i*1.4)) + 1;
      #TODO: maybe col by col? to stop doing cols that are already there?
      #main use currently is 1 col anyway
      fl, num, den = _induce_rational_reconstruction(D.x, ppow; unbalanced = true)
      if fl
#        @show fl = (D.A*num == den*_B)
         sz = max(maximum(nbits, D.A) + maximum(nbits, num)
                                     + nbits(ncols(_B)) + 1,
                 maximum(nbits, _B) + nbits(den))

        if sz < nbits(ppow)
           #we know the product is correct as the p-adic precision
           #was high enough already...
#          @show "no prod neccessary"
        else
          xp = next_prime(D.p)
#          @show ceil(Int, (sz - nbits(ppow))/60)
          local Ap::fpMatrix
          local Bp::fpMatrix
          local nump::fpMatrix
          local lhs::fpMatrix
          local rhs::fpMatrix
          for i=1:ceil(Int, (sz - nbits(ppow))/60)
            k = Nemo.fpField(xp, false)
            if i == 1
              Ap = map_entries(k, D.A)
              Bp = map_entries(k, _B)
              nump = map_entries(k, num)
              lhs = Ap*nump
              rhs = Bp*k(den)
            else
              map_entries!(k, Ap, D.A)
              map_entries!(k, Bp, _B)
              map_entries!(k, nump, num)
              change_base_ring!(k, lhs)
              mul!(lhs, Ap, nump)
              change_base_ring!(k, rhs)
              mul!(rhs, Bp, k(den))
            end
            fl = lhs == rhs
            fl || break
            xp = next_prime(xp)
          end
        end

        if side == :right
          fl && return num, den
        else
          fl && return transpose(num), den
        end
      end
    end

    if ncols(_B) == 1
      n = nrows(D.A)
      GC.@preserve D d begin 
        for i=1:n
          Ay_ptr = mat_entry_ptr(D.Ay, i, 1)
          A_ptr = mat_entry_ptr(D.A, i, 1)
          d_ptr = mat_entry_ptr(d, i, 1)
          zero!(Ay_ptr)
          for j=1:n
            y_ptr = Nemo.mat_entry_ptr(D.y_mod, j, 1)
            addmul!(Ay_ptr, A_ptr, unsafe_load(y_ptr))
            A_ptr += sizeof(ZZRingElem)
          end
          sub!(d_ptr, d_ptr, Ay_ptr)
        end
      end
    else
      C = CrtCtx_Mat(block)
      for j=1:length(D.crt_primes)
        change_prime!(D.y_mod, D.crt_primes[j])
        change_prime!(D.Ay_mod, D.crt_primes[j])

        mul!(D.Ay_mod, D.A_mod[j], D.y_mod)
        push!(C, D.Ay_mod)
      end
      change_prime!(D.y_mod, D.p)
      Nemo.sub!(d, d, finish(C))
    end
    divexact!(d, d, ZZ(D.p))
  end
  fl, num, den = _induce_rational_reconstruction(D.x, ppow; unbalanced = true)
  @assert fl

  if side == :right
    return num, den
  else
    return transpose(num), den
  end
end

function dixon_solve(A::ZZMatrix, B::ZZMatrix; side::Symbol = :right)
  d = dixon_init(A, B; side)
  return dixon_solve(d, B; side)
end

#iterative CRT for matrices
#will use flint (induce_crt!) for blocks of ?? matrices and then switch
#to some tree-based asymptotically fast version.

function induce_crt!(A::ZZMatrix, p::ZZRingElem, B::fpMatrix; signed::Bool = false)
  #the second modulus is implicit in B: B.n

  @ccall libflint.fmpz_mat_CRT_ui(A::Ref{ZZMatrix}, A::Ref{ZZMatrix}, p::Ref{ZZRingElem}, B::Ref{fpMatrix}, signed::Int)::Cvoid
  mul!(p, p, B.n)
  return
end

mutable struct CrtCtx_Mat
  c::Int #how many primes before tree

  d::Vector{ZZRingElem} #the product tree so far
  M::Vector{ZZMatrix}

  pos::Int
  cc::Int

  function CrtCtx_Mat(c::Int = 1)
    return new(c, ZZRingElem[], ZZMatrix[], 0, 0)
  end
end

function Base.push!(C::CrtCtx_Mat, a::fpMatrix)
  if C.pos == 0
    push!(C.M, lift(a))
    push!(C.d, ZZ(a.n))
    C.cc = 1
    C.pos += 1
    return
  end
  pos = C.pos
  if C.cc == 0
    if pos > length(C.M)
      push!(C.M, lift(a))
      push!(C.d, ZZ(a.n))
    else
      C.M[pos] = lift(a)
      C.d[pos] = ZZ(a.n)
    end
    C.cc = 1
  else
    induce_crt!(C.M[C.pos], C.d[C.pos], a; signed = true)
    C.cc += 1
  end
  if C.cc >= C.c #full...
    while pos > 1 && C.d[pos-1] < C.d[pos] #assuming primes are in order
      g, e, f = gcdx(C.d[pos-1], C.d[pos])
      @assert isone(g)
      mul!(C.M[pos-1], f*C.d[pos])
      mul!(C.M[pos], e*C.d[pos-1])
      add!(C.M[pos-1], C.M[pos-1], C.M[pos])
      C.d[pos-1] *= C.d[pos]
      #@show maximum(nbits, C.M[pos-1])
      mod_sym!(C.M[pos-1], C.d[pos-1])
      pos -= 1
    end
    C.pos = pos + 1
    C.cc = 0
  end
end

function finish(C::CrtCtx_Mat)
  pos = C.pos
  if C.cc == 0
    pos -= 1
  end
  while pos > 1
    g, e, f = gcdx(C.d[pos-1], C.d[pos])
    @assert isone(g)
    mul!(C.M[pos-1], f*C.d[pos])
    mul!(C.M[pos], e*C.d[pos-1])
    add!(C.M[pos-1], C.M[pos-1], C.M[pos])
    C.d[pos-1] *= C.d[pos]
#    @show maximum(nbits, C.M[pos-1]), nbits(C.d[pos-1])
    mod_sym!(C.M[pos-1], C.d[pos-1])
    pos -= 1
  end
  result = C.M[1]
  # Reset data members to their original "empty" values
  C.cc = 0
  C.pos = 0
  empty!(C.d)
  empty!(C.M)
  return result
end


####################################################################
# DoublePlusOne Solver
####################################################################

#U represents a 1 x n ZZ matrix in base m
# each row is a single (symmetric) p-adic digit
# (or should be). At this point the entries can be too large, the
# carries will have to be propagated down
# in the algorithm
# - the top is never changed, we we will not look at it (again)
# - the matrix is larger than it seems: U.r is less than actually allocated
#   so it will be increased...
# Handle with care.
#_to_base below will actually convert U, inplace, to the correct ZZMatrix
function _renorm(U::ZZMatrix, m::ZZRingElem; start::Int = 1, last::Int = nrows(U), R::ZZMatrix = zero_matrix(ZZ, 1, ncols(U)))

  i = start
  t = ZZRingElem(Val(:raw))
#  R =  zero_matrix(ZZ, 1, ncols(U))
  zero!(R)
  GC.@preserve R U begin
    while true
      R_ptr = Nemo.mat_entry_ptr(R, 1, 1)
      U_ptr = Nemo.mat_entry_ptr(U, i, 1)
    
      for j=1:ncols(U)
        add!(R_ptr, R_ptr, U_ptr)
        mod_sym!(U_ptr, R_ptr, m, t)
        sub!(R_ptr, R_ptr, U_ptr)
        divexact!(R_ptr, R_ptr, m)
        R_ptr += sizeof(Int)
        U_ptr += sizeof(Int)
      end
      i += 1
      if i > nrows(U)
        if i > last || is_zero(R)
          Nemo._fmpz_clear_fn(t)
          return U
        end
        while !is_zero(R)
          if i > last
            Nemo._fmpz_clear_fn(t)
            return U
          end
          U.r += 1
          @assert U.r <= last
          U_ptr = Nemo.mat_entry_ptr(U, i, 1)
          R_ptr = Nemo.mat_entry_ptr(R, 1, 1)
     
          for j=1:ncols(U)
            mod_sym!(U_ptr, R_ptr, m, t)
            sub!(R_ptr, R_ptr, U_ptr)
            divexact!(R_ptr, R_ptr, m)
            R_ptr += sizeof(Int)
            U_ptr += sizeof(Int)
          end
          i += 1
        end
        Nemo._fmpz_clear_fn(t)
        return U
      end
    end
  end
end


function mod_sym!(a::Ptr{ZZRingElem}, b::Ptr{ZZRingElem}, c::ZZRingElem, t::ZZRingElem = ZZ(0))
  @ccall libflint.fmpz_ndiv_qr(t::Ref{ZZRingElem}, a::Ptr{ZZRingElem}, b::Ptr{ZZRingElem}, c::Ref{ZZRingElem})::Cvoid
end

#add C into A[c:end, :]
#add!(view(A, c+1:end, :), C) (but without the view)
function add_into!(A::ZZMatrix, C::ZZMatrix, c::Int)
  A.r = max(A.r, C.r+c)
  GC.@preserve A C begin
    for i=1:nrows(C)
      A_ptr = Nemo.mat_entry_ptr(A, i+c, 1)
      C_ptr = Nemo.mat_entry_ptr(C, i, 1)
      for j=1:ncols(A)
        add!(A_ptr, C_ptr)
        A_ptr += sizeof(Int)
        C_ptr += sizeof(Int)
      end
    end
  end
end

#sub!(view(A, c+1:end, :), C) (but without the view)
function sub_into!(A::ZZMatrix, C::ZZMatrix, c::Int)
  A.r = max(A.r, C.r+c)
  GC.@preserve A C begin
    for i=1:nrows(C)
      A_ptr = Nemo.mat_entry_ptr(A, i+c, 1)
      C_ptr = Nemo.mat_entry_ptr(C, i, 1)
      for j=1:ncols(A)
        sub!(A_ptr, C_ptr)
        A_ptr += sizeof(Int)
        C_ptr += sizeof(Int)
      end
    end
  end
end

#saves essentially the allocation of A.rows
#Edit: A.rows no longer exists, not sure if this is still useful
function view!(A::ZZMatrix, B::ZZMatrix, r::UnitRange{Int}, ::Colon)
  A.entries = B.entries + B.stride * sizeof(ZZRingElem) * (r.start - 1)
  A.r = length(r)
  A.c = B.c
  A.stride = B.stride
  A.view_parent = (B, )
  return A
end

# "solves" xA = U using the double + 1 method
#Pauderis and Storjohann, same as uni-cert.
# the functions returns
# - a matrix X in ZZ and a denominator d
#
#TODO: 
# - _renorm using preinv (to speed things up)
# - if |U| >> |A| use the Abbott trick: write U in base ? and solve multiple
#   systems - otherwise the X below needs to be too large and/ or the
#   _renorm has to work too much
# - a special version for odd determinant where X is a power of 2 or better
#   2^64 to use limbs (thus making modm _renorm and _to_base trivial)
#   (and ugly to implement)
#

function UniCertSolve(A::ZZMatrix, U::ZZMatrix)
  n = nrows(A);
  @assert ncols(A) == ncols(U) == n
  @assert maximum(nbits, A) >= maximum(nbits, U)

  GC_d = GC.enable(false) #within this function essentially
    #nothing can be collected, all(?) operations are julia inplace
    #and use a lot of memory (fir large examples)

  m, B0, k = Nemo._PSH_init(A)
  if GC_d
    GC.enable(true)
    GC.gc()
    GC.enable(false)
  end

  @vprintln(:UnimodVerif,1, "Solve with DoublePlusOne: Max num iters is k=$(k)")

  local pr::Int
  pr = 2^(k+2)-2
  allV = ZZMatrix[]
  for i=1:nrows(U)
    V = zero_matrix(ZZ, pr+10, ncols(U))
    V.r = 1
    push!(allV, V)
  end
#  @assert maximum(abs, U) <= m

  R = A*B0
  sub!(R, R, 1)
  divexact!(R, m)
  #R = (I-A*B0)/m
  
  tmp_V = zero_matrix(ZZ, max(pr + 10, n), ncols(U))
  RE_tmp = zero_matrix(ZZ, 1, ncols(U))
  view_V = view(tmp_V, :, :)
  view_U = view(U, :, :)
  view_tmp_V = view(tmp_V, :, :)

  for i=1:nrows(U)
    V = allV[i]
    mul!(view!(view_V, V, 1:1, :), view!(view_U, U, i:i, :), B0)
    V.r = 1
    _renorm(V, m; last = 10, R = RE_tmp)

    tV = view!(view_tmp_V, tmp_V, 1:nrows(V), :)
    mul!(tV, V, R)
    sub_into!(V, tV, 1) #sol mod m^2
    _renorm(V, m; last = 10, R = RE_tmp)
  end

  if is_zero(R)
    mu = vcat([_to_base!(t, m) for t = allV]...)
    tau = _induce_rational_reconstruction(mu, m; unbalanced = true)
    @assert tau[1]
    GC.enable(GC_d)
    return tau[2], tau[3]
  end
  ex = 1

  R_bar = zero_matrix(ZZ, n, n)
  M = zero_matrix(ZZ, n, n)

  for i in 1:k
    @vprintln(:UnimodVerif, 1,  "DoublePlusOne: loop: i=$(i)")
    #@time R_bar = R^2;
    mul!(R_bar, R, R)
    vT = view!(view_tmp_V, tmp_V, 1:n, :)
    mod_sym!(vT, R_bar, m)
    mul!(M, B0, vT)
    mod_sym!(M, m)
    mul!(R, A, M)
    sub!(R, R_bar, R)
    divexact!(R, m)

    for j=1:nrows(U)
      V =allV[j]

      vT = view!(view_tmp_V, tmp_V, 1:1, :)
      mul!(vT, view!(view_U, U, j:j, :), M)
      add_into!(V, vT, 2*ex)  #sol mod 2ex+1
      _renorm(V, m, start = 2*ex, last = pr, R = RE_tmp)
    end

    ex = 2*ex + 1

    for jj = 1:nrows(U)
      V = allV[jj]

      if nrows(V) > nrows(R) #this part should be automatic...
        n = nrows(R)
        l = nrows(V)
        e = div(l, n)
        vT = view(tmp_V, 1:l-e*n, :)
        mul!(vT, view(V, e*n+1:l, :), R)
        add_into!(V, vT, ex+e*n)
        vT = view(tmp_V, 1:n, :)
        for j=e:-1:1
          @assert j*n <= pr
          mul!(vT, view(V, (j-1)*n+1:j*n, :), R)
          add_into!(V, vT, ex+(j-1)*n)
        end
      else
        vT = view!(view_tmp_V, tmp_V, 1:nrows(V), :)
        mul!(vT, V, R)
        add_into!(V, vT, ex) #sol mod 2(2ex+1)
      end
      _renorm(V, m, start = ex-1, last = pr, R = RE_tmp)
      @assert V === allV[jj]
    end

    mex = m^(2*ex)
    mu = vcat([_to_base!(t[:, 1:1], m) for t = allV]...)
    tau = _induce_rational_reconstruction(mu, mex; unbalanced = true)
    if tau[1]
      GC.enable(true)
      mu = vcat([_to_base!(deepcopy(t), m) for t = allV]...)
      tau = _induce_rational_reconstruction(mu, mex; unbalanced = true)
      if tau[1]
        GC.enable(GC_d)
        return tau[2], tau[3]
      end
      GC.gc()
      GC.enable(false)
    end
  end
  mu = vcat([_to_base!(t, m) for t = allV]...)
  tau = _induce_rational_reconstruction(mu, mex; unbalanced = true)
  @assert tau[1]
  GC.enable(GC_d)
  return tau[2], tau[3]
end

#inplace conversion
#the rows of a are encoding a base-b row vector
#this computes asymptotically fast(?) the ZZ-version
function _to_base!(a::ZZMatrix, _b::ZZRingElem)
  nr = nrows(a)
  bb = ZZRingElem(Val(:raw))
  b = ZZRingElem(Val(:raw))
  set!(b, _b)
  while nr > 1
    mul!(bb, b, b)
    for i=1:div(nr, 2)
      add_row!(a, b, 2*i, 2*i-1)
      swap_rows!(a, i, 2*i-1)
      zero_row!(a, 2*i) #to release memory early
    end
    if is_odd(nr)
      add_row!(a, bb, nr, div(nr, 2))
      zero_row!(a, nr)
    end
    nr = div(nr, 2)
    b.d, bb.d = bb.d, b.d
  end
  Nemo._fmpz_clear_fn(bb)
  Nemo._fmpz_clear_fn(b)
  #all rows, but the 1st have been set to zero! - or have never been
  #used. So no memory is lost...
  a.r = 1
  return a
end

