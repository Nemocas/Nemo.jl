#### QQ(i) and ZZ(i) ####

struct ZZiRing <: Ring
end

const ZZi = ZZiRing()
GaussianIntegers() = ZZi

struct ZZiRingElem <: RingElem
  x::ZZRingElem
  y::ZZRingElem
end

struct QQiField <: Field
end

const FlintQQi = QQiField()

struct QQiFieldElem <: FieldElem
  num::ZZiRingElem
  den::ZZRingElem
end

