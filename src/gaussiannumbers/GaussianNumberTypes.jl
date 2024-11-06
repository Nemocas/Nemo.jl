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

const QQi = QQiField()
GaussianRationals() = QQi

struct QQiFieldElem <: FieldElem
  num::ZZiRingElem
  den::ZZRingElem
end

