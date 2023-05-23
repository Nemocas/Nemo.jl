using Nemo
using Test
using InteractiveUtils: @which

import Nemo.AbstractAlgebra
include(joinpath(pathof(AbstractAlgebra), "..", "..", "test", "Rings-conformance-tests.jl"))

include("Aqua.jl")
include("rand.jl")
include("Nemo-test.jl")
