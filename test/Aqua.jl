using Aqua

@testset "Aqua.jl" begin
  Aqua.test_all(
                Nemo;
                piracies=false          # TODO: fix piracy
               )
end
