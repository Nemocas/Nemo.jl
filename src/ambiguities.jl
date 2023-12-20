# Fix ambiguities on julia 0.4

*(a::ResElem{ZZRingElem}, b::ZZRingElem) = parent(a)(data(a) * b)

*(a::ZZRingElem, b::ResElem{ZZRingElem}) = b*a

+(a::ResElem{ZZRingElem}, b::ZZRingElem) = parent(a)(data(a) + b)

+(a::ZZRingElem, b::ResElem{ZZRingElem}) = b + a

-(a::ResElem{ZZRingElem}, b::ZZRingElem) = parent(a)(data(a) - b)

-(a::ZZRingElem, b::ResElem{ZZRingElem}) = parent(b)(a - data(b))

function ==(a::ResElem{ZZRingElem}, b::ZZRingElem)
   z = base_ring(a)(b)
   return data(a) == mod(z, modulus(a))
end

==(a::ZZRingElem, b::ResElem{ZZRingElem}) = b == a
