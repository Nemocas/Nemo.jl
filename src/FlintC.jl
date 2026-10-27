# This file has been generated from `etc/FlintC_template.jl`. DO NOT EDIT MANUALLY.
#
# To regenerate it, e.g. after updating FLINT_jll, run
#   julia etc/generate_FLINT_structs.jl
#
# Generated with FLINT_jll v301.600.1+0.

###############################################################################
#
#   FLINT C types
#
#   The module mirrors FLINT internals, so it is not covered by Nemo's SemVer
#   guarantees and may change in any release.
#
###############################################################################

module FlintC

using ..Nemo: FlintStruct

#
# FLINT configuration and global definitions
#

const ulong = UInt
const slong = Int

WORD(n) = Int(n)
UWORD(n) = UInt(n)

const FLINT_BITS = UInt == UInt64 ? 64 : 32

include("FlintC/flint.h.jl")
include("FlintC/limb_types.h.jl")
include("FlintC/nmod_types.h.jl")
include("FlintC/fmpz_types.h.jl")
include("FlintC/fmpq_types.h.jl")
include("FlintC/fmpz_mod_types.h.jl")
include("FlintC/fq_nmod_types.h.jl")
include("FlintC/fq_zech_types.h.jl")
include("FlintC/fq_types.h.jl")
include("FlintC/gr_types.h.jl")
include("FlintC/fq_default.h.jl")
include("FlintC/fq_default_mat.h.jl")
include("FlintC/fq_default_poly.h.jl")
include("FlintC/fq_default_poly_factor.h.jl")
include("FlintC/padic_types.h.jl")
include("FlintC/qadic.h.jl")
include("FlintC/n_poly_types.h.jl")
include("FlintC/mpoly_types.h.jl")
include("FlintC/fq_nmod_mpoly.h.jl")
include("FlintC/fq_nmod_mpoly_factor.h.jl")
include("FlintC/fmpz_poly.h.jl")
include("FlintC/fmpq_poly.h.jl")
include("FlintC/nf.h.jl")
include("FlintC/nf_elem.h.jl")
include("FlintC/arf_types.h.jl")
include("FlintC/arb_types.h.jl")
include("FlintC/acb_types.h.jl")
include("FlintC/acb_calc.h.jl")
include("FlintC/fexpr.h.jl")
include("FlintC/qqbar.h.jl")
include("FlintC/fmpz_mpoly.h.jl")
include("FlintC/ca_types.h.jl")

end # module FlintC
