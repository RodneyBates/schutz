MODULE Test15_174; 

BEGIN 

	        FOR j:=SetPassed.Used TO i BY -1 DO

		  SetPassed.Elements[j]:= SetPassed.Elements[j-1];

		END;

END Test15_174 

. 
