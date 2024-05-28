function benchmark_nf_det()
  print("benchmark_nf_det ... ")
  QQx, x = polynomial_ring(QQ, "x")
  K, a = number_field(x^3 + 3*x + 1, "a")
  M = matrix_space(K, 80, 80)()

  for i in 1:80
    for j in 1:80
      for k in 0:2
        M[i, j] = M[i, j] + a^k * (rand(-100:100))
      end
    end
  end

  tt = @elapsed det(M)
  println("$tt")
end

