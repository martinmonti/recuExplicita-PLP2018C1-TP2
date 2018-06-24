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

testEj1() :- not(tieneEstrella(a))
           , not(tieneEstrella(or(a,b)))
           , tieneEstrella(star(a))
           , tieneEstrella(or(a,star(b)))     
           , tieneEstrella(or(star(b),a))      
           , tieneEstrella(or(concat(star(a),b),c))
           , tieneEstrella(star(concat(a,b))).    

% Ejercicio 2: longitudMaxima(+RegEx, -Length)

longitudMaxima(X,N):- symbol(X),N is 1.
longitudMaxima(empty,0).
longitudMaxima(or(X,Y),N) :- longitudMaxima(X,LX), longitudMaxima(Y,LY),N is max(LX,LY).
longitudMaxima(concat(X,Y),N) :- longitudMaxima(X,LX), longitudMaxima(Y,LY), N is LX + LY.

testEj2() :-  longitudMaxima(empty,0)
            , longitudMaxima(a,1)                    
            , longitudMaxima(b,1)                      
            , not(longitudMaxima(z,N))
            , longitudMaxima(or(a,b),1)              
            , longitudMaxima(concat(a,b),2)         
            , longitudMaxima(concat(a,empty),1)    
            , longitudMaxima(concat(or(a,b),b),2)  
            , not(longitudMaxima(concat(or(a,star(b)),b),N)).


% Ejercicio 3: cadena(?Cadena)



%cadenas(+L,+LMAX,+R,- C).
cadenas(0,LMAX,[],[]).
cadenas(LR,-1,R,C):- symbol(X), C=[X|R].
cadenas(LR,-1,R,C):- L is LR+1,cadenas(L,LMAX,[X|R],C), symbol(X).
cadenas(LR,LMAX,R,C):- LR @=< LMAX, symbol(X), C=[X|R].
cadenas(LR,LMAX,R,C):- L is LR+1, L @< LMAX,cadenas(L,LMAX,[X|R],C), symbol(X).

cadena(C) :- ground(C),is_set(C),sort(C,ORDC),simbolos(SIMBOLOS),sort(SIMBOLOS,ORD_SIMBOLOS),ORDC=ORD_SIMBOLOS,!.
cadena(C) :- var(C),cadenas(0,-1,[],C).

simbolos(C) :- findall(X,symbol(X),C).

% Ejercicio 4: match_inst(+Cadena, +RegEx)

match_inst([],RE) :- RE = empty,!.
match_inst([CH],RE) :- symbol(RE), CH = RE, !.
match_inst(C,or(RE1,RE2)) :- match_inst(C,RE1),!.
match_inst(C,or(RE1,RE2)) :- match_inst(C,RE2),!.
match_inst(C,concat(RE1,RE2)):- prefix(PREF,C), append(PREF,SUF,C), match_inst(PREF,RE1),match_inst(SUF,RE2),!.
match_inst(C,star(RE)) :- prefix(PREF,C), append(PREF,RESTO,C), match_inst(PREF,RE), match_inst(RESTO,empty),!.
match_inst(C,star(RE)) :- prefix(PREF,C), append(PREF,RESTO,C), match_inst(PREF,RE), match_inst(RESTO,star(RE)),!.

testEj4() :- match_inst([], empty)
				,not(match_inst([a], empty))
				,match_inst([a],a)
				,not(match_inst([b],a))
				,not(match_inst([a], b))
				,match_inst([b], b)
				,not(match_inst([c], or(a, b)))
				,match_inst([b], or(a, b))
				,match_inst([a,b],concat(a,b))
				,not(match_inst([a,a,b],concat(a,b)))
				,not(match_inst([a,c],concat(a,b)))
				,match_inst([a, a, b], concat(star(a), b))
				,not(match_inst([b, a], concat(star(a), b))).

% Ejercicio 5: match(?Cadena, +RegEx)

match(C,REGEX) :- ground(C),match_inst(C,REGEX).
match(C,REGEX) :- var(C),tieneEstrella(REGEX),cadena(C),match_inst(C,REGEX).
match(C,REGEX) :- var(C),not(tieneEstrella(REGEX)),longitudMaxima(REGEX,MAXL),cadenas(0,MAXL,[],C),match_inst(C,REGEX).

% Ejercicio 6: diferencia(?Cadena, +RegEx, +RegEx)

diferencia(_, _, _) :- fail.

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +RegEx)

prefijoMaximo(_, _, _) :- fail.

% Ejercicio 8: reemplazar(+X, +R, +E, Res)

reemplazar(_, _, _, _) :- fail.
