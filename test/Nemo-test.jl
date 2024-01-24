testlist = [
    "Fields-test.jl",
    "Rings-test.jl",
    "Generic-test.jl",
    "Benchmark-test.jl",
    "gaussiannumbers/continued_fraction-test.jl",
    "Native-test.jl",
    "Infinity-test.jl",
    "HeckeMiscLocalization-test.jl",
    "matrix-test.jl",
    "poly-test.jl",
    "ZZMatrix-linalg-test.jl"
]

pmap(include, testlist)
