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