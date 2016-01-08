PLATYPUS {
	a = 5;
	OUTPUT("Please enter value for a: ");
	INPUT(a);
	OUTPUT(a); OUTPUT();
	a = a + 2;
	OUTPUT(a); OUTPUT();
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
						ELSE{
							OUTPUT(a);OUTPUT();
						};
					ELSE{

					};
				ELSE{
					OUTPUT(a);OUTPUT();
				};
			ELSE{

			};
		ELSE{
			OUTPUT(a);OUTPUT();
		};
	ELSE{
	
	};
	a = 5;
	OUTPUT("Please enter value for d: ");
	INPUT(d);
	OUTPUT(d); OUTPUT();
	d = d*2;
	OUTPUT("after d*2 d's value is now ");OUTPUT(d); OUTPUT();
	OUTPUT("Please enter values for a,b,c%: ");
	INPUT(a,b,c%);
	OUTPUT("a is "); OUTPUT(a); OUTPUT();
	OUTPUT("b is "); OUTPUT(b); OUTPUT();
	OUTPUT("c% is "); OUTPUT(c%); OUTPUT();
	OUTPUT("Please input i: ");
	INPUT(i);
	i = i+5;
	OUTPUT("i after i+5 is : "); OUTPUT(i); OUTPUT();
	OUTPUT("i and a are (i,a):"); OUTPUT(i,a); OUTPUT();
	i = i / a;
	OUTPUT("i after i/a is : "); OUTPUT(i); OUTPUT();
	OUTPUT("i and a are (i,a): "); OUTPUT(i,a); OUTPUT();
}