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

#

*(::ZZRingElem, ::PolyElem{ZZRingElem}) = nothing

*(::PolyElem{ZZRingElem}, ::ZZRingElem) = nothing

+(::ZZRingElem, ::PolyElem{ZZRingElem}) = nothing

+(::PolyElem{ZZRingElem}, ::ZZRingElem) = nothing

-(::ZZRingElem, ::PolyElem{ZZRingElem}) = nothing

-(::PolyElem{ZZRingElem}, ::ZZRingElem) = nothing

==(::ZZRingElem, ::PolyElem{ZZRingElem}) = nothing

==(::PolyElem{ZZRingElem}, ::ZZRingElem) = nothing

divexact(::PolyElem{ZZRingElem}, ::ZZRingElem) = nothing

evaluate(::PolyElem{ZZRingElem}, ::ZZRingElem) = nothing

#

*(::ZZRingElem, ::SeriesElem{ZZRingElem}) = nothing

*(::SeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

+(::ZZRingElem, ::SeriesElem{ZZRingElem}) = nothing

+(::SeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

-(::ZZRingElem, ::SeriesElem{ZZRingElem}) = nothing

-(::SeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

==(::ZZRingElem, ::SeriesElem{ZZRingElem}) = nothing

==(::SeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

*(::ZZRingElem, ::RelSeriesElem{ZZRingElem}) = nothing

*(::RelSeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

+(::ZZRingElem, ::RelSeriesElem{ZZRingElem}) = nothing

+(::RelSeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

-(::ZZRingElem, ::RelSeriesElem{ZZRingElem}) = nothing

-(::RelSeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

==(::ZZRingElem, ::RelSeriesElem{ZZRingElem}) = nothing

==(::RelSeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

*(::ZZRingElem, ::AbsSeriesElem{ZZRingElem}) = nothing

*(::AbsSeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

+(::ZZRingElem, ::AbsSeriesElem{ZZRingElem}) = nothing

+(::AbsSeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

-(::ZZRingElem, ::AbsSeriesElem{ZZRingElem}) = nothing

-(::AbsSeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

==(::ZZRingElem, ::AbsSeriesElem{ZZRingElem}) = nothing

==(::AbsSeriesElem{ZZRingElem}, ::ZZRingElem) = nothing

*(::ZZRingElem, ::MatElem{ZZRingElem}) = nothing

*(::MatElem{ZZRingElem}, ::ZZRingElem) = nothing

+(::ZZRingElem, ::MatElem{ZZRingElem}) = nothing

+(::MatElem{ZZRingElem}, ::ZZRingElem) = nothing

-(::ZZRingElem, ::MatElem{ZZRingElem}) = nothing

-(::MatElem{ZZRingElem}, ::ZZRingElem) = nothing

==(::MatElem{ZZRingElem}, ::ZZRingElem) = nothing

divexact(::MatElem{ZZRingElem}, ::ZZRingElem) = nothing

#

setindex_t!(a::zzModMatrix, b::GenRes{ZZRingElem}, i::Int, j::Int) = setindex_!(a, data(b), i, j)

*(::FracElem{ZZRingElem}, ::ZZRingElem) = nothing

*(::ZZRingElem, ::FracElem{ZZRingElem}) = nothing

+(::FracElem{ZZRingElem}, ::ZZRingElem) = nothing

+(::ZZRingElem, ::FracElem{ZZRingElem}) = nothing

-(::FracElem{ZZRingElem}, ::ZZRingElem) = nothing

-(::ZZRingElem, ::FracElem{ZZRingElem}) = nothing

==(::FracElem{ZZRingElem}, ::ZZRingElem) = nothing

==(::ZZRingElem, ::FracElem{ZZRingElem}) = nothing

divexact(::FracElem{ZZRingElem}, ::ZZRingElem) = nothing

divexact(::ZZRingElem, ::FracElem{ZZRingElem}) = nothing

