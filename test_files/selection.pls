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
	a = 5;
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
}