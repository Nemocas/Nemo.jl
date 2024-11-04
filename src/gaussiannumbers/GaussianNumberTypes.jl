#### QQ(i) and ZZ(i) ####

struct ZZiRing <: Ring
end

struct ZZiRingElem <: RingElem
  x::ZZRingElem
  y::ZZRingElem
end

struct QQiField <: Field
end

struct QQiFieldElem <: FieldElem
  num::ZZiRingElem
  den::ZZRingElem
end

