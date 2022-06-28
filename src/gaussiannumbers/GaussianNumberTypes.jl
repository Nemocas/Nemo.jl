#### QQ(i) and ZZ(i) ####

struct FlintZZiRing <: Nemo.Ring
end

const FlintZZi = FlintZZiRing()

struct fmpzi <: RingElem
  x::fmpz
  y::fmpz
end

struct FlintQQiField <: Nemo.Field
end

const FlintQQi = FlintQQiField()

struct fmpqi <: FieldElem
  num::fmpzi
  den::fmpz
end

