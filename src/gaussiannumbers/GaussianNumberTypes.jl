#### QQ(i) and ZZ(i) ####

mutable struct FlintZZiRing <: Nemo.Ring
end

ZZi = FlintZZiRing()

struct fmpzi <: RingElem
  x::fmpz
  y::fmpz
end

mutable struct FlintQQiField <: Nemo.Field
end

QQi = FlintQQiField()

struct fmpqi <: FieldElem
  num::fmpzi
  den::fmpz
end

