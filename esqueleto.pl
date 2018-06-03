symbol(a).
symbol(b).
symbol(c).

% Algunas regex de ejemplo

regexEj(1, a). % a
regexEj(2, or(a, b)). % a|b
regexEj(3, concat(E1, E2)) :- regexEj(1, E1), regexEj(2, E2). % a(a|b)
regexEj(4, star(E2)) :- regexEj(2, E2). % (a(a|b))*
regexEj(5, or(star(E1), E4)) :- regexEj(1, E1), regexEj(4, E4). % (a*|(a(a|b))*)
regexEj(6, star(or(a, ab))). %(a|ab)*
regexEj(7, concat(or(a, concat(a,b)), or(b, empty))). %(a|ab)(b|)
regexEj(8, concat(star(a), star(b))). %a*b*
regexEj(9, star(or(star(a), star(b)))).


% Ejercicio 1: tieneEstrella(+RegEx)

tieneEstrella(star(X)) :- true.
tieneEstrella(or(X,Y)) :- tieneEstrella(X),!.
tieneEstrella(or(X,Y)) :- tieneEstrella(Y),!. 
tieneEstrella(concat(X,Y)) :- tieneEstrella(X),!.
tieneEstrella(concat(X,Y)) :- tieneEstrella(Y),!.

testEj1_01() :- tieneEstrella(a).                 % Tiene que dar false.
testEj1_02() :- tieneEstrella(or(a,b)).           % Tiene que dar false.
testEj1_03() :- tieneEstrella(star(a)).           % Tiene que dar true.
testEj1_04() :- tieneEstrella(or(a,star(b))).     % Tiene que dar true.
testEj1_05() :- tieneEstrella(or(star(b),a)).     % Tiene que dar true.
 
% Ejercicio 2: longitudMaxima(+RegEx, -Length)

longitudMaxima(_, _) :- fail.

% Ejercicio 3: cadena(?Cadena)

cadena(_) :- fail.

% Ejercicio 4: match_inst(+Cadena, +RegEx)

match_inst(_, _) :- fail.

% Ejercicio 5: match(?Cadena, +RegEx)

match(_, _) :- fail.

% Ejercicio 6: diferencia(?Cadena, +RegEx, +RegEx)

diferencia(_, _, _) :- fail.

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +RegEx)

prefijoMaximo(_, _, _) :- fail.

% Ejercicio 8: reemplazar(+X, +R, +E, Res)

reemplazar(_, _, _, _) :- fail.
