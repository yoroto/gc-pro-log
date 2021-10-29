:- module( log_types, [byte_size/3, integer/3, decimal/3, datetime/3, spaces/2, ws/2] ).

byte_size( size(Value, Unit) ) --> decimal(Value), byte_unit(Unit).

byte_unit(byte) --> "B".
byte_unit(kbyte) --> "K".
byte_unit(mbyte) --> "M".
byte_unit(gbyte) --> "G".


datetime( time(Y, M, D, H, Min, S, MS, Z) ) --> 
    four_digits(Y), minus, two_digits(M), minus, two_digits(D),
    "T", two_digits(H), colon, two_digits(Min), colon, two_digits(S), 
    dot, three_digits(MS), plus, four_digits(Z).

four_digits( N ) --> digit(Th), digit(H), digit(T), digit(U),
    { N is Th * 1000 + H * 100 + T * 10 + U }.
three_digits( N ) --> digit(H), digit(T), digit(U),
    { N is H * 100 + T * 10 + U }.
two_digits( N ) --> digit(T), digit(U),
    { N is T * 10 + U }.

integer( NN ) --> minus, digits(N, 0), !, { NN is -N }.
integer( N ) --> plus, digits(N, 0), !.
integer( N ) --> digits(N, 0).

decimal( ND ) --> minus, decimal(D), !, { ND is -D }.
decimal( D ) --> plus, decimal(D), !.
decimal( D ) --> digits(Int, 0), dot, decimal_part(Dec),
    { D is Int + Dec }.

decimal_part( D ) --> digit(T), !, decimal_part(R), { D is (T + R)/10 }.
decimal_part( 0 ) --> [].

digits( N, PN ) --> digit(T), !, { CN is PN*10 + T }, digits(N, CN).
digits( N, N ) --> [].

minus --> "-".
plus --> "+".
colon --> ":".
dot --> ".".

digit( 0 ) --> "0".
digit( 1 ) --> "1".
digit( 2 ) --> "2".
digit( 3 ) --> "3".
digit( 4 ) --> "4".
digit( 5 ) --> "5".
digit( 6 ) --> "6".
digit( 7 ) --> "7".
digit( 8 ) --> "8".
digit( 9 ) --> "9".

spaces --> ws,!,spaces.
spaces --> [].

ws --> " "; "\t" ; [10] ; [13].