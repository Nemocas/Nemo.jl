###############################################################################
#
#   Flint C types
#
###############################################################################

module FlintC

#
# FLINT configuration and global definitions
#

const ulong = UInt
const slong = Int

WORD(n) = Int(n)
UWORD(n) = UInt(n)

const FLINT_BITS = UInt == UInt64 ? 64 : 32


@include_c_header("flint.h")

@include_c_header("limb_types.h")

@include_c_header("nmod_types.h")

@include_c_header("fmpz_types.h")

@include_c_header("fmpq_types.h")

@include_c_header("fmpz_mod_types.h")

@include_c_header("fq_nmod_types.h")

@include_c_header("fq_zech_types.h")

@include_c_header("fq_types.h")

@include_c_header("gr_types.h")

@include_c_header("fq_default.h")

@include_c_header("fq_default_mat.h")

@include_c_header("fq_default_poly.h")

@include_c_header("fq_default_poly_factor.h")

@include_c_header("padic_types.h")

@include_c_header("n_poly_types.h")

@include_c_header("mpoly_types.h")

end # module FlintC
