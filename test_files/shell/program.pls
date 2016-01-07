PLATYPUS {
	a = 1;
	OUTPUT("Please enter value for a: ");
	INPUT(a);
	OUTPUT(a); OUTPUT();
	!<a = a + 2;
	!<OUTPUT(a); OUTPUT();
	USING(i = 0, i < a, i = i + 1)
	REPEAT {
		OUTPUT("var i is "); OUTPUT(i); OUTPUT();
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
	OUTPUT("Please enter value for d: ");
	INPUT(d);
	OUTPUT(d); OUTPUT();
	d = d*2;
	OUTPUT("after d*2 d's value is now ");OUTPUT(d); OUTPUT();
	INPUT(a,d,c%);
	OUTPUT(a,d,c%); OUTPUT();
}