PLATYPUS {
	USING(i = 0, i < 3, i = i + 1)
	REPEAT {
		USING(d = 0, d < 3, d = d + 1)
		REPEAT {
			OUTPUT("d is "); OUTPUT(d); OUTPUT();
		};
		OUTPUT("i is "); OUTPUT(i); OUTPUT();
	};
}