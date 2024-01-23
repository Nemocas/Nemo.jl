#### QQ(i) and ZZ(i) ####

struct FlintZZiRing <: Nemo.Ring
end

const FlintZZi = FlintZZiRing()

struct fmpzi <: RingElem
  x::ZZRingElem
  y::ZZRingElem
end

struct QQiField <: Nemo.Field
end

const FlintQQi = QQiField()

struct QQiFieldElem <: FieldElem
  num::fmpzi
  den::ZZRingElem
end

