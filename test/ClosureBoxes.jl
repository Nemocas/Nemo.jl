# Captured variables that are reassigned force Julia to allocate a `Core.Box`,
# which defeats type inference. `Test.detect_closure_boxes` exists since
# Julia 1.14.
if isdefined(Test, :detect_closure_boxes)
  @testset "Closure boxes" begin
    boxes = Test.detect_closure_boxes(Nemo)
    for (m, vars) in boxes
      println("Boxed variable(s) ", join(vars, ", "), " in ", m)
    end
    @test length(boxes) == 0
  end
end
