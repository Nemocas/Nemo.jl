##################################################################
## Struct to make repeated multiplication by the same matrix faster
##################################################################

#  General idea of use case:
#  M_ = MatrixMultiplier(M);  # Initial overhead
#  MN_1 = mul(M_, N_1);       # Each product is faster
#  MN_2 = mul(M_, N_2);       # (faster)
#  ...
#  MN_k = mul(M_, N_k);       # (faster)

# The idea here is to create a structure which "captures" M, a matrix of integers,
# in such a way that M*N can be computed quickly where N is a suitably sized integer
# matrix.  This is useful if we need to compute several products M*N1, M*N2, etc.

# This implementation computes M mod p1,p2,p3,...
# Then computes modulo each prime (M*N) mod p_k, before recombining.
# This should work well if the matrix entries are "all of similar size".
# A tricky point is dealing with RHS whose entry sizes are (obviously) independent
# of the sizes of the entries in the matrix (e.g. potentially much larger)

# flatten matrix into a vector by concatenating the rows
function flatten_matrix(M::ZZMatrix)
  return vcat([M[i,:]  for i in 1:nrows(M)]...);
end


# This is used a heuristic to identify matrices which have "mostly small"
# entries, and "just a few" large entries. [[first attempt at a heuristic]]
# Compute ratio of bitsize of largest entry in M to bitsize
# of 80%-largest entry.
# Result is always >= 1.00
function matrix_entry_ratio80(M::ZZMatrix)
  L = nbits.(flatten_matrix(M));
  n = length(L);
  (n < 5)  &&  return last(L)/L[n-1]  # BUG if n < 2
  sort!(L);
  posn80 = ceil(Int, 0.80*n);
  ratio = last(L)/L[posn80];
  return ratio; 
end

# Use the function in Nemo.jl/src/flint/fmpz_mat.jl
# function mul_classical!(z::Nemo.ZZMatrixOrPtr, x::Nemo.ZZMatrixOrPtr, y::Nemo.ZZMatrixOrPtr)
#   @ccall Nemo.libflint.fmpz_mat_mul_classical(z::Ref{ZZMatrix}, x::Ref{ZZMatrix}, y::Ref{ZZMatrix})::Nothing
#   return z
# end


# 2025-04-16  The constructor is disappointingly slow, but
#             the mul function is (sometimes) pleasingly fast.

mutable struct MatrixMultiplier
  LHS_bitsize::Int  # if 0 then we just use classical multiplication (e.g. if matrix is small)
  RHS_bitsize::Int
  PrimeList::Vector{UInt}
  reducer::Hecke.crt_env{ZZRingElem}
  ModulusList::Vector{ZZRingElem}
  ModulusBitsList::Vector{Int}
  MatrixList::Vector{fpMatrix}
  OrigMatrix::ZZMatrix
##??  CRT::CrtCtx_Mat
  function MatrixMultiplier(M::ZZMatrix, RHS_bitsize::Int)
    # EDGE CASES (such as matrices with 0 or 1 rows/cols) delegate to mul_classical
    if nrows(M) <= 2 || ncols(M) <= 2 || matrix_entry_ratio80(M) > 5
      return new(0,0,Int[],crt_env([ZZ(2)]),ZZRingElem[],Int[],fpMatrix[],M);
    end
    LHS_bitsize = maximum(nbits, M);
    r = nrows(M);
    c = ncols(M);
    TargetBitSize = 1+ nbits(c) + LHS_bitsize + RHS_bitsize; # 1+... to allow for sign
    p = 2^50;
    modulus = ZZ(1);
    PrimeList = Int[];
    ModulusList = ZZRingElem[];
    ModulusBitsList = Int[];
    while nbits(modulus) <= TargetBitSize
      p = next_prime(p, false);  # disable "proving primality"
      modulus *= p;
      push!(PrimeList, p);
      push!(ModulusList, modulus);
      push!(ModulusBitsList, nbits(modulus));
    end
    # Now reduce M modulo each chosen prime: we do it "in parallel" entry-by-entry
    reducer = crt_env(ZZ.(PrimeList));
    MatrixList = [ zero_matrix(Native.GF(p; check=false), r,c)  for p in PrimeList ];
    tmp = [ZZ(0)  for _ in 1:length(PrimeList)];
    for i in 1:r
      for j in 1:c
        Hecke.crt_inv!(tmp, M[i,j], reducer);
        for k in 1:length(MatrixList)
          MatrixList[k][i,j] = tmp[k];
        end
      end
    end
    
    # # !!Loop below should be smarter!!  (binary modular reduction, not sequential)
    # MatrixList = fpMatrix[];
    # for p in PrimeList
    #   Fp = Native.GF(p; check=false);
    #   Mp = matrix(Fp, M);
    #   push!(MatrixList, Mp);
    # end
    return new(LHS_bitsize, RHS_bitsize, PrimeList, reducer, ModulusList, ModulusBitsList, MatrixList, M);
  end
end


# Just a first version
function mul(A::MatrixMultiplier, B::ZZMatrix)
  @req (ncols(A.OrigMatrix) == nrows(B))  "Incompatible matrix dimensions"
  if A.LHS_bitsize == 0
    prod = zero_matrix(ZZ, nrows(A.OrigMatrix), ncols(B));
    Nemo.mul_classical!(prod, A.OrigMatrix, B);
    return prod;
  end
  B_bitsize = maximum(nbits,B);
  if (B_bitsize > A.RHS_bitsize)
    return A.OrigMatrix*B  # ***LAZY*** revert to built-in multiplication if RHS is too big <-- IMPROVE THIS!
  end
  # TODO Handle trivial cases ...
  threshold = ZZ(2)^(2 + nbits(nrows(B)) + B_bitsize + A.LHS_bitsize);
  r = nrows(B);
  c = ncols(B);
  BpList = [ zero_matrix(base_ring(Ap), r,c)  for Ap in A.MatrixList ];
  for i in 1:r
    for j in 1:c
      # Line below is convoluted because crt_inv does not like negative inputs
      tmp = (B[i,j] >= 0) ? Hecke.crt_inv(B[i,j], A.reducer) : -Hecke.crt_inv(-B[i,j], A.reducer); # quicker NOT to convert the residues to Int
      for k in 1:length(BpList)
        BpList[k][i,j] = tmp[k];
      end
    end
  end
  CRT = Nemo.CrtCtx_Mat();
  for i in 1:length(A.MatrixList)
    Ap = A.MatrixList[i];
    push!(CRT, Ap*BpList[i]);
    if A.ModulusList[i] > threshold
      break;
    end
  end
  return finish(CRT);
end
# Some ideas to make mul "smarter":
# (0)  Must somehow make the constructor (much) faster!
# (1)  If the entries of B are much larger than those of M, split the "big" columns
#      of B into several columns with smaller entries (e.g. digits base 2^K some K);
#      the splitting must be undone in the product.  Initially try with K = 3*maximum(nbits,M)?
# (2)  If the entries of B are much smaller than those of M, it might be worth
#      coalescing several columns of B into one with larger entries (by regarding
#      the columns as digits base 2^K where K > nbits(B)+max_nbits(A)).  Not sure
#      this is really a good idea.
