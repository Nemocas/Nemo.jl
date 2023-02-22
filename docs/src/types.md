# Types in Nemo

Nemo is fully compatible with AbstractAlgebra.jl, but specialises implementations of
various commonly used rings with a highly optimised C implementation, provided by the
C libraries wrapped by Nemo.

Below, we give a list of all of the specialised types available in Nemo that implement
rings using a specialised C library. The types of elements of the respective rings and
other mathematical structures are given, and in parentheses we list the types of the
parent objects of the given rings and structures.

  - Flint
     - `ZZRingElem` (`ZZRing`)
     - `QQFieldElem` (`QQField`)
     - `zzModRingElem` (`zzModRing`)
     - `ZZModRingElem` (ZZModRing`)
     - `fqPolyRepFieldElem` (`fqPolyRepField`)
     - `fpFieldElem` (`fpField`)
     - `FpFieldElem` (`FpField`)
     - `FqPolyRepFieldElem` (`FqPolyRepField`)
     - `padic` (`FlintPadicField`)
     - `qadic` (`FlintQadicField`)
     - `ZZPolyRingElem` (`ZZPolyRing`)
     - `QQPolyRingElem` (`QQPolyRing`)
     - `zzModPolyRingElem` (`zzModPolyRing`)
     - `ZZModPolyRingElem` (`ZZModPolyRing`)
     - `FqPolyRepPolyRingElem` (`FqPolyRepPolyRing`)
     - `fqPolyRepPolyRingElem` (`fqPolyRepPolyRing`)
     - `ZZMPolyRingElem` (`ZZMPolyRing`)
     - `QQMPolyRingElem` (`QQMPolyRing`)
     - `zzModMPolyRingElem` (`zzModMPolyRing`)
     - `fqPolyRepMPolyRingElem` (fqPolyRepMPolyRing`)
     - `fpPolyRingElem` (`fpPolyRing`)
     - `FpPolyRingElem` (`FpPolyRing`)
     - `ZZRelPowerSeriesRingElem` (`ZZRelPowerSeriesRing`)
     - `ZZAbsPowerSeriesRingElem` (`ZZAbsPowerSeriesRing`)
     - `QQRelPowerSeriesRingElem` (`QQRelPowerSeriesRing`)
     - `QQAbsPowerSeriesRingElem` (`QQAbsPowerSeriesRing`)
     - `ZZModRelPowerSeriesRingElem` (`ZZModRelPowerSeriesRing`)
     - `ZZModAbsPowerSeriesRingElem` (`ZZModAbsPowerSeriesRing`)
     - `zzModRelPowerSeriesRingElem` (`zzModRelPowerSeriesRing`)
     - `zzModAbsPowerSeriesRingElem` (`zzModAbsPowerSeriesRing`)
     - `fpRelPowerSeriesRingElem` (`fpRelPowerSeriesRing`)
     - `fpAbsPowerSeriesRingElem` (`fpAbsPowerSeriesRing`)
     - `FpRelPowerSeriesRingElem` (`FpRelPowerSeriesRing`)
     - `FpAbsPowerSeriesRingElem` (`FpAbsPowerSeriesRing`)
     - `fqPolyRepRelPowerSeriesRingElem` (`fqPolyRepRelPowerSeriesRing`)
     - `fqPolyRepAbsPowerSeriesRingElem` (`fqPolyRepAbsPowerSeriesRing`)
     - `FqPolyRepRelPowerSeriesRingElem` (`FqPolyRepRelPowerSeriesRing`)
     - `FqPolyRepAbsPowerSeriesRingElem` (`FqPolyRepAbsPowerSeriesRing`)
     - `ZZMatrix` (`ZZMatrixSpace`)
     - `QQMatrix` (`QQMatrixSpace`)
     - `zzModMatrix` (`zzModMatrixSpace`)
     - `ZZModMatrix` (ZZModMatrixSpace`)
     - `fqPolyRepMatrix` (`fqPolyRepMatrixSpace`)
     - `FqPolyRepMatrix` (`FqPolyRepMatrixSpace`)
     - `fpMatrix` (`fpMatrixSpace`)
     - `perm` (`SymmetricGroup`)

  - Antic
     - `nf_elem` (`AnticNumberField`)

  - Arb
     - `arb` (`ArbField`)
     - `acb` (`AcbField`)
     - `arb_poly` (`ArbPolyRing`)
     - `acb_poly` (`AcbPolyRing`)
     - `arb_mat` (`ArbMatSpace`)
     - `acb_mat` (`AcbMatSpace`)

  - Calcium

     - `qqbar` (`CalciumQQBarField`)
     - `ca` (`CalciumField`)
