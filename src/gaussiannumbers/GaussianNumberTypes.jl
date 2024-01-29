#### QQ(i) and ZZ(i) ####

struct ZZiRing <: Nemo.Ring
end

const FlintZZi = ZZiRing()

struct ZZiRingElem <: RingElem
  x::ZZRingElem
  y::ZZRingElem
end

struct QQiField <: Nemo.Field
end

const FlintQQi = QQiField()

struct QQiFieldElem <: FieldElem
  num::ZZiRingElem
  den::ZZRingElem
end

