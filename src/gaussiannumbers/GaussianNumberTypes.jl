#### QQ(i) and ZZ(i) ####

mutable struct FlintZZiRing <: Nemo.Ring
end

const FlintZZi = FlintZZiRing()

struct fmpzi <: RingElem
  x::fmpz
  y::fmpz
end

mutable struct FlintQQiField <: Nemo.Field
end

const FlintQQi = FlintQQiField()

struct fmpqi <: FieldElem
  num::fmpzi
  den::fmpz
end

