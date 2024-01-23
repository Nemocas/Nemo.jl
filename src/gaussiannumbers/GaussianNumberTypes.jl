#### QQ(i) and ZZ(i) ####

struct ZZiRing <: Nemo.Ring
end

const FlintZZi = ZZiRing()

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

