function benchmark_bernoulli()
   print("benchmark_bernoulli ... ")

   R, x = FlintQQ["x"]
   S, t = power_series_ring(R, 1000, "t")

   u = t + O(t^1000)

   tt = @elapsed divexact((u*exp(x*u)), (exp(u)-1));
   println("$tt")
end

