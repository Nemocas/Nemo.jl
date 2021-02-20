@testset "fq_nmod_embed.embed..." begin

    for i in 1:10

        p = rand(2:997)
        while !isprime(p)
            p = rand(2:997)
        end

        k1, x1 = FiniteField(p, 1, "x1")
        k2, x2 = FiniteField(p, 2, "x2")
        k3, x3 = FiniteField(p, 3, "x3")
        k4, x4 = FiniteField(p, 4, "x4")
        k6, x6 = FiniteField(p, 6, "x6")
        k8, x8 = FiniteField(p, 8, "x8")
        k9, x9 = FiniteField(p, 9, "x9")
        k12, x12 = FiniteField(p, 12, "x12")
        k16, x16 = FiniteField(p, 16, "x16")
        k18, x18 = FiniteField(p, 18, "x18")
        k24, x24 = FiniteField(p, 24, "x24")

        S = Set([(k4, k12),
                 (k6, k24),
                 (k8, k16),
                 (k2, k16),
                 (k3, k6),
                 (k9, k18),
                 (k6, k12),
                 (k3, k18),
                 (k4, k16),
                 (k6, k18),
                 (k2, k8),
                 (k12, k24),
                 (k3, k9),
                 (k2, k6),
                 (k8, k24),
                 (k3, k24),
                 (k2, k12),
                 (k3, k12),
                 (k2, k24),
                 (k2, k4),
                 (k4, k24),
                 (k1, k3),
                 (k1, k4),
                 (k1, k6),
                 (k1, k16),
                 (k1, k12),
                 (k1, k9),
                 (k4, k8)])

        f = Dict()
        # Compute embeddings in a random order
        while !isempty(S)
            k, K = pop!(S, rand(S))
            f[(k, K)] = embed(k, K)
        end

        # Check that the embeddings are compatible
        @test f[k6, k18](f[k3, k6](x3)) == f[k3, k18](x3)
        @test f[k8, k16](f[k2, k8](x2)) == f[k2, k16](x2)
        @test f[k12, k24](f[k6, k12](x6)) == f[k6, k24](x6)
        @test f[k9, k18](f[k3, k9](x3)) == f[k3, k18](x3)
        @test f[k6, k24](f[k3, k6](x3)) == f[k3, k24](x3)
        @test f[k6, k12](f[k2, k6](x2)) == f[k2, k12](x2)
        @test f[k6, k12](f[k3, k6](x3)) == f[k3, k12](x3)
        @test f[k12, k24](f[k3, k12](x3)) == f[k3, k24](x3)
        @test f[k6, k24](f[k2, k6](x2)) == f[k2, k24](x2)
        @test f[k12, k24](f[k2, k12](x2)) == f[k2, k24](x2)
        @test f[k12, k24](f[k4, k12](x4)) == f[k4, k24](x4)
        @test f[k4, k12](f[k2, k4](x2)) == f[k2, k12](x2)
        @test f[k4, k24](f[k2, k4](x2)) == f[k2, k24](x2)
        @test f[k8, k16](f[k4, k8](x4)) == f[k4, k16](x4)
        @test f[k8, k24](f[k4, k8](x4)) == f[k4, k24](x4)
        @test f[k4, k8](f[k2, k4](x2)) == f[k2, k8](x2)
        @test f[k4, k16](f[k2, k4](x2)) == f[k2, k16](x2)
        @test f[k4, k16](f[k1, k4](x1)) == f[k1, k16](x1)
        @test f[k3, k9](f[k1, k3](x1)) == f[k1, k9](x1)
        @test f[k6, k12](f[k1, k6](x1)) == f[k1, k12](x1)
    end
end

@testset "fq_nmod_embed.preimage_map..." begin

    for i in 1:10

        p = rand(2:997)
        while !isprime(p)
            p = rand(2:997)
        end

        a, b = rand(1:5), rand(1:5)
        ka, xa = FiniteField(p, a, "xa")
        kab, xab = FiniteField(p, a*b, "xab")

        f = preimage_map(ka, kab)
        g = embed(ka, kab)

        for j in 1:20
            x = rand(ka)
            @test f(g(x)) == x
        end
    end
end

@testset "fq_nmod_embed.transitive_closure..." begin

    S = Set(2:997)
    for i in 1:40

        p = rand(S)
        while !isprime(p)
            p = rand(S)
        end
        pop!(S, p)

        F = FiniteField(p, 4, "s")[1]
        Z = ResidueRing(ZZ, p)
        R, x = PolynomialRing(Z, "x")

        P1 = R(rand(Z, 4)) + x^4
        F1 = factor(P1)
        P2 = R(rand(Z, 4)) + x^4
        F2 = factor(P2)
        P3 = R(rand(Z, 4)) + x^4
        F3 = factor(P3)
        P4 = R(rand(Z, 4)) + x^4
        F4 = factor(P4)

        while length(F1) != 1 || F1[collect(keys(F1.fac))[1]] != 1
            P1 = R(rand(Z, 4)) + x^4
            F1 = factor(P1)
        end

        while length(F2) != 1 || F2[collect(keys(F2.fac))[1]] != 1
            P2 = R(rand(Z, 4)) + x^4
            F2 = factor(P2)
        end

        while length(F3) != 1 || F3[collect(keys(F3.fac))[1]] != 1
            P3 = R(rand(Z, 4)) + x^4
            F3 = factor(P3)
        end

        while length(F4) != 1 || F4[collect(keys(F4.fac))[1]] != 1
            P4 = R(rand(Z, 4)) + x^4
            F4 = factor(P4)
        end

        K1 = FiniteField(P1, "r1")[1]
        K2 = FiniteField(P2, "r2")[1]
        K3 = FiniteField(P3, "r3")[1]
        K4 = FiniteField(P4, "r4")[1]

        K = Set([K1, K2, K3, K4])
        while !isempty(K)
            embed(pop!(K, rand(K)), F)
        end

        @test length(F.subfields[4]) == 4

        x1 = rand(K1)
        x2 = rand(K2)
        x3 = rand(K3)
        x4 = rand(K4)

        @test F(x1) == F(K2(x1))
        @test F(x2) == F(K3(x2))
        @test K4(x3) == K4(K1(x3))
        @test K1(K2(x4)) == K1(K3(x4))

    end
end
