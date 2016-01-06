PLATYPUS {
	i = 3;
	IF (i == 1)
	THEN 
		OUTPUT("First THEN clause."); OUTPUT();
		i = 0;
	ELSE {
		IF (i > 1)
		THEN
			OUTPUT("Nested THEN clause."); OUTPUT();
			IF (i == 2)
			THEN
				OUTPUT("Second Nested THEN clause."); OUTPUT();
			ELSE {
				OUTPUT("Second Nested ELSE clause."); OUTPUT();
				i = -5;
			};
		ELSE {
			OUTPUT("Nested ELSE clause."); OUTPUT();
			i = -5;
		};
	};
}