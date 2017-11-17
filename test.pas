program test(output);
label 1476, 1178;
var i,lim : integer;
begin
	lim := 7;
   	i := 0;
1476:
   	while i < lim do 
   	begin
   	  writeln('*');
      i := i + 1
    end
    if lim < 5 then goto 1476
end.
