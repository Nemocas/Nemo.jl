# This file has been generated from FLINT's `acb_calc.h`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

const acb_calc_func_t = Ptr{Cvoid}

struct acb_calc_integrate_opt_struct <: FlintStruct
  deg_limit::slong
  eval_limit::slong
  depth_limit::slong
  use_heap::Cint
  verbose::Cint
end

const acb_calc_integrate_opt_t = Tuple{acb_calc_integrate_opt_struct}
