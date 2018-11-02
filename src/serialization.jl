
#Flint serialization code

#--------------fmpz START-----------------

fmpzByteStream(x) = reinterpret(Int, [x])
 
io = IOBuffer()

struct serializeFmpz
	
	function serializeFmpz(sValue::fmpz)
		write(io, fmpzByteStream(sValue.d))
	end

end

function deserialize(t::Type{fmpz})
	seekstart(io)
	return fmpz(read(io, Int))
end


sr = serializeFmpz(fmpz(13))
deserialize(fmpz)


#--------------fmpz END-------------------

#--------------fmpq START-----------------


fmpqByteStream(x) = reinterpret(Int, [x])
 
io = IOBuffer()

struct serializeFmpq
	
	function serializeFmpq(sValue::fmpq)
		write(io, fmpzByteStream(sValue.num))
		write(io, fmpzByteStream(sValue.den))
	end

end

function deserialize(t::Type{fmpq})
	seekstart(io)
	p1 =  read(io, Int)
	p2 =  read(io, Int)
	return fmpq(p1, p2)
end


sr = serializeFmpq(fmpq(13, 13))
deserialize(fmpq)


#--------------fmpq END----------------

#--------------NmodRing START----------

NmodRingByteStream(x) = reinterpret(UInt, [x])
 
io = IOBuffer()

struct serializeNmodRing
	
	function serializeNmodRing(sValue::NmodRing)
		write(io, NmodRingByteStream(sValue.n))
		write(io, NmodRingByteStream(sValue.ninv))
	end

end

function deserialize(t::Type{NmodRing})
	seekstart(io)
	p1 =  read(io, UInt)
	p2 =  read(io, UInt)
	return NmodRing(p1, p2)
end


sr = serializeNmodRing(NmodRing(13, 13))
deserialize(NmodRing)


#--------------NmodRing END----------



function serialize(s::AbstractSerializer, fpp::fmpz_poly)
	serialize(s, fpp)
end

function deserialize(s::AbstractSerializer, fpp::Type{fmpz_poly})
	deserialize(s)
end

fmpzPoly = fmpz_poly( ::Ptr{Nothing}, ::Int, ::Int, ::FmpzPolyRing)

# Serialization
serialize_iob = IOBuffer()
serialize(serialize_iob, fmpzPoly)
seekstart(serialize_iob)
data = read(serialize_iob)

# Deserialization
deserialize_iob = IOBuffer(data)
fmpzPoly_rtrv = deserialize(deserialize_iob)

#--------------fmpz_poly END---------------



function serialize(s::AbstractSerializer, t::fmpz)
  return serialize(s, base(t, 16))
end

function deserialize(s::AbstractSerializer, t::Type{fmpz})
  return parse(fmpz, deserialize(s), 16)
end

function serialize(s::AbstractSerializer, t::fmpq)
  #serialize_type(s, fmpq)
  serialize(s, base(numerator(t), 16))
  return serialize(s, base(denominator(t), 16))
end

function deserialize(s::AbstractSerializer, t::Type{fmpq})
  n = parse(fmpz, deserialize(s), 16)
  d = parse(fmpz, deserialize(s), 16)
  return fmpq(n, d)
end



# simple I/O custom serialization
function serialise(sValue)
	
	io = open("objects.dat", "w");
	write(io, sValue);
	close(io);
	return 0
end

function deSerialise(dType)
	
	io = open("objects.dat", "r");
	dValue = read(io, dType);
	close(io);
	return dValue
end

data = "sample data";
serialise(data);
 
sleep(5);
 
deSerialise(typeof(data))

