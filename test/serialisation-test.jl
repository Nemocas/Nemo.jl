function serialise(sValue)
	
	io = open("objects.dat", "w");
	write(io, sValue);
	close(io);
	return 0
end

function deSerialise()
	
	io = open("objects.dat", "r");
	dValue = read(io, String);
	close(io);
	return dValue
end


serialise("sample data");
 
sleep(10);
 
deSerialise()