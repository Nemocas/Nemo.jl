#Flint type serialization


function serialize(s::AbstractSerializer, t::fmpz)
  return serialize(s, base(t, 62))
end

function deserialize(s::AbstractSerializer, t::Type{fmpz})
  return parse(fmpz, deserialize(s), 62)
end

function serialize(s::AbstractSerializer, t::fmpq)
  serialize_type(s, fmpq)
  serialize(s, base(numerator(t), 62))
  return serialize(s, base(denominator(t), 62))
end

function deserialize(s::AbstractSerializer, t::Type{fmpq})
  n = parse(fmpz, deserialize(s), 62)
  d = parse(fmpz, deserialize(s), 62)
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



