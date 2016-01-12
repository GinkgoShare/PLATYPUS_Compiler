PLATYPUS {
	a = 5;
	OUTPUT("Please enter value for a: ");
	INPUT(a);
	OUTPUT(a); OUTPUT();
	USING(i = 0, i < a, i = i + 1)
	REPEAT {
		OUTPUT("var i is "); OUTPUT(i); OUTPUT();
	};

	OUTPUT("Please enter value for a: ");
	INPUT(a);
	OUTPUT(a); OUTPUT();
	USING(i = 0, i < a, i = i + 1)
	REPEAT {
		OUTPUT("var i is "); OUTPUT(i); OUTPUT();
		IF (i == 3)
		THEN
			USING(j = 0, j < a, j = j + 1)
			REPEAT {
				OUTPUT("var j is "); OUTPUT(j); OUTPUT();
				USING(k = 0, k < a, k = k + 1)
				REPEAT {
					OUTPUT("var k is "); OUTPUT(k); OUTPUT();
				};
				OUTPUT("Finished second nested loop."); OUTPUT();
			};
		ELSE {
			OUTPUT("ELSE clause."); OUTPUT();
		};
		OUTPUT("Finished first nested loop."); OUTPUT();
	};
}