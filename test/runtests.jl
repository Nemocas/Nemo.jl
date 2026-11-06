using Nemo

using Distributed

numprocs_str = get(ENV, "NUMPROCS", "1")

if !isempty(ARGS)
    jargs = [arg for arg in ARGS if startswith(arg, "-j")]
    if !isempty(jargs)
        numprocs_str = split(jargs[end], "-j")[end]
    end
end

const numprocs = parse(Int, numprocs_str)

if numprocs >= 2
    println("Adding worker processes")
    addprocs(numprocs)
end

@everywhere using Nemo
@everywhere using Test
@everywhere using InteractiveUtils: @which
@everywhere import Nemo.AbstractAlgebra
@everywhere const example_rings = [
                          ZZ,                                           # ZZRing
                          QQ,                                           # QQField
                          residue_ring(ZZ, 9)[1],                       # zzModRing
                          residue_ring(ZZ, ZZ(9))[1],                   # ZZModRing
                          Native.GF(5),                                 # fpField
                          Native.GF(ZZ(5)),                             # FpField
                          Native.finite_field(3, 2, "b")[1],            # fqPolyRepField
                          Native.finite_field(ZZRingElem(3), 2, "b")[1],# FqPolyRepField
                          GF(5),                                        # FqField
                          GF(ZZ(5)),                                    # FqField
                          finite_field(3, 2, "b")[1],                   # FqField
                          finite_field(ZZRingElem(3), 2, "b")[1],       # FqField
                          ArbField(),                                   # ArbField
                          AcbField(),                                   # AcbField
                          real_field(),                                 # RealField
                          complex_field(),                              # ComplexField
                        ]

# `divexact` by a zero element of `R`, or by each zero scalar in `zeros`
@everywhere function test_divexact_by_zero(R, zeros...)
  is_trivial(R) && return
  for check in (true, false), b in (zero(R), zeros...)
    @test_throws DivideError divexact(one(R), b; check)
    @test_throws DivideError divexact(zero(R), b; check)
  end
end

@everywhere include("rand.jl")
include("Nemo-test.jl")
