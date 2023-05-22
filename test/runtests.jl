using Nemo
using Test
using Nemo.InteractiveUtils

import Nemo.AbstractAlgebra
include(joinpath(pathof(AbstractAlgebra), "..", "..", "test", "Rings-conformance-tests.jl"))

include("rand.jl")
include("Nemo-test.jl")
