for T in [nmod_poly, gfp_poly, fmpz_mod_poly, gfp_fmpz_poly, fmpq_poly, fmpz_poly, fq_poly, fq_nmod_poly, fq_default_poly]
  (f::T)(a) = subst(f, a)

  function (f::T)(a::T)
     if parent(f) != parent(a)
        return subst(f, a)
     end
     return compose(f, a)
  end

  (f::T)(a::Integer) = evaluate(f, a)

  function (f::T)(a::RingElem)
     if parent(a) != base_ring(f)
        return subst(f, a)
     end
     return evaluate(f, a)
  end

end

