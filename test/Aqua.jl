using Aqua

@testset "Aqua.jl" begin
    Aqua.test_all(
        Nemo;
        ambiguities=false,      # TODO: fix ambiguities
        piracy=false            # TODO: fix piracy
    )
end
