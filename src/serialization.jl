
#FlintType serialization


###############################################################################
#
#   FmpzPolyRing / fmpz_poly
#
###############################################################################


#--------------FmpzPolyRing START--------------

function serialize(s::AbstractSerializer, fpr::FmpzPolyRing)
	serialize(s, fpr)
end

function deserialize(s::AbstractSerializer, fpr::Type{FmpzPolyRing})
	deserialize(s)
end

fmpzPolyR = FmpzPolyRing( ::FlintIntegerRing, ::Symbol)

# Serialization
serialize_iob = IOBuffer()
serialize(serialize_iob, fmpzPolyR)
seekstart(serialize_iob)
data = read(serialize_iob)

# Deserialization
deserialize_iob = IOBuffer(data)
fmpzPolyR_rtrv = deserialize(deserialize_iob)


#--------------FmpzPolyRing END---------------

#--------------fmpz_poly START----------------

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

