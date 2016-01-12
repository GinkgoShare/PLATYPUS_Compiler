PLATYPUS {
	c = 5.4;
	str1% = "abcdef";
	str2% = "ABCDEF";
	IF(str1% > str2%)
	THEN c=-(5.9);
   	ELSE {
   		c=-c;
   	};
   	OUTPUT(c);OUTPUT();
}