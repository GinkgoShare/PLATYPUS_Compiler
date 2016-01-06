PLATYPUS {
	a = 1;
	INPUT(a);
	OUTPUT(a); OUTPUT();
	!<a = a + 2;
	!<OUTPUT(a); OUTPUT();
	USING(i = 0, i < a, i = i + 1)
	REPEAT {
		IF (i == 2) THEN i = 5;
		ELSE {
			OUTPUT("var i is "); OUTPUT(i); OUTPUT();
		};
	};
	IF(1==1)
	THEN
		IF(1==1)
		THEN
			IF(1==1)
			THEN
				IF(1==1)
				THEN
					IF(1==1)
					THEN
						IF(1==1)
						THEN
							IF(1==2)
							THEN OUTPUT("End of IF statement"); OUTPUT();
							ELSE{
								OUTPUT("End of ELSE statement"); OUTPUT();
							};
						ELSE{};
					ELSE{};
				ELSE{};
			ELSE{};
		ELSE{};
	ELSE{};
	a = 5;
	INPUT(b);
	b = b*b;
	OUTPUT(b); OUTPUT();
	IF(1==2)
	THEN OUTPUT("End of IF statement"); OUTPUT();
	ELSE{
		OUTPUT("End of ELSE statement"); OUTPUT();
	};
	INPUT(a,b,c%);
	OUTPUT(a,b,c%); OUTPUT();
}