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
cadena(C) :- ground(C),is_set(C),sort(C,ORDC),simbolos(SIMBOLOS),sort(SIMBOLOS,ORD_SIMBOLOS),ORDC=ORD_SIMBOLOS,!.
cadena(C) :- var(C),cadenas(0,-1,[],C).

%cadenas(+LR,+LMAX,+R,- C).
% Instancia C como todas las posibles cadenas de símbolos válidos, de longitud
% mínima LR y máxima LMAX. Pasar R con valor [].
cadenas(0,LMAX,[],[]).
cadenas(LR,-1,R,C):- symbol(X), C=[X|R].
cadenas(LR,-1,R,C):- L is LR+1,cadenas(L,LMAX,[X|R],C), symbol(X).
cadenas(LR,LMAX,R,C):- LR @=< LMAX, symbol(X), C=[X|R].
cadenas(LR,LMAX,R,C):- L is LR+1, L @< LMAX,cadenas(L,LMAX,[X|R],C), symbol(X).

%simbolos(-C) :- Devuelve en C una lista de los símbolos disponibles
simbolos(C) :- findall(X,symbol(X),C).

% Ejercicio 4: match_inst(+Cadena, +RegEx)

match_inst([],empty) :- !.
match_inst([CH],RE) :- symbol(RE), CH = RE, !.
match_inst(C,or(RE1,RE2)) :- match_inst(C,RE1),!.
match_inst(C,or(RE1,RE2)) :- match_inst(C,RE2),!.
match_inst(C,concat(RE1,RE2)):- prefix(PREF,C), append(PREF,SUF,C), match_inst(PREF,RE1),match_inst(SUF,RE2),!.
match_inst([],star(RE)):-!.
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
				,not(match_inst([b, a], concat(star(a), b)))
				,match_inst([],star(a))
				,match_inst([a],star(a))
				,match_inst([a,a],star(a))
				,match_inst([a,a,a],star(a))
				,match_inst([a,a,a,a],star(a))
				,not(match_inst([a,a,a,b],star(a)))
				,not(match_inst([b,a,a,a],star(a)))
				,not(match_inst([a,a,b,a],star(a)))
				,match_inst([],star(concat(a,b)))
				,match_inst([a,b],star(concat(a,b)))
				,match_inst([a,b,a,b],star(concat(a,b)))
				,not(match_inst([a,b,c],star(concat(a,b))))
				,not(match_inst([a,b,a],star(concat(a,b))))
				,not(match_inst([a,b,b,a],star(concat(a,b))))
				,not(match_inst([a,b,a,b,a],star(concat(a,b))))
				,not(match_inst([a,c,b],star(concat(a,b))))
				,not(match_inst([b,a],star(concat(a,b))))
				,not(match_inst([a,a,b,b],star(concat(a,b)))).

% Ejercicio 5: match(?Cadena, +RegEx)

match(C,REGEX) :- ground(C),match_inst(C,REGEX),!.
match(C,REGEX) :- var(C),cadenasCandidatasParaRegEx(C,REGEX),match_inst(C,REGEX).

%cadenasCandidatasParaRegEx(-C,+REGEX). Devuelve todas las cadenas que podrían, por su longitud, aplicar a REGEX.
cadenasCandidatasParaRegEx(C,REGEX) :- tieneEstrella(REGEX),cadena(C).
cadenasCandidatasParaRegEx(C,REGEX) :- not(tieneEstrella(REGEX)),longitudMaxima(REGEX,MAXL),cadenas(0,MAXL,[],C).

testEj5() :- match([], empty)
				,not(match([a], empty))
				,match([a],a)
				,not(match([b],a))
				,not(match([a], b))
				,match([b], b)
				,not(match([c], or(a, b)))
				,match([b], or(a, b))
				,match([a,b],concat(a,b))
				,not(match([a,a,b],concat(a,b)))
				,not(match([a,c],concat(a,b)))
				,match([a, a, b], concat(star(a), b))
				,not(match([b, a], concat(star(a), b))).

% Ejercicio 6: diferencia(?Cadena, +RegEx, +RegEx)

diferencia(C,R1,R2) :- ground(C),match_inst(C,R1),not(match_inst(C,R2)),!.
diferencia(C,R1,R2) :- var(C), cadenasCandidatasParaRegEx(C,R1),match_inst(C,R1),not(match_inst(C,R2)).

testEj6() :- diferencia([a],star(a),empty)
           , diferencia([a,a],star(a),empty)
			  , not(diferencia([b],star(a),empty))
			  , not(diferencia([],star(a),empty))
			  , diferencia([a],or(a,b),b)
			  , not(diferencia([b],or(a,b),b))
			  , diferencia([a],star(or(a,b)),star(b))
			  , diferencia([a,a],star(or(a,b)),star(b))
			  , diferencia([a,b],star(or(a,b)),star(b))
			  , diferencia([b,a],star(or(a,b)),star(b))
			  , diferencia([a,a,a],star(or(a,b)),star(b))
			  , diferencia([a,a,b],star(or(a,b)),star(b))
			  , not(diferencia([b],star(or(a,b)),star(b)))
			  , not(diferencia([b,b],star(or(a,b)),star(b)))
			  , not(diferencia([c],star(or(a,b)),star(b)))
			  , not(diferencia([c,a],star(or(a,b)),star(b)))
			  , not(diferencia([c,b],star(or(a,b)),star(b))) .

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +RegEx)

prefijoMaximo(MAX_PREF,CADENA,REGEX) :- 
	 findall(PREF,esPrefijoYMatchea(PREF,CADENA,REGEX),PREFIJOS) % Recolecto prefijos que matchean
	,length(PREFIJOS,CANT_PREFIJOS),CANT_PREFIJOS>0			% Hay al menos un prefijo que matchea
	,encontrarListaMayor(PREFIJOS,[],MAX_PREF).				% MAX_PREF es el más extenso de los prefijos que matchean.

% Devuelve la lista mayor de una lista de listas dada.
% Invocar con encontrarListaMayor(LISTAS,[],RESULTADO).
% LISTAS debe no ser vacía.
encontrarListaMayor([],LAUX,LAUX).
encontrarListaMayor([L|LS],LAUX,L_MAX):- length(L,NL),length(LAUX,NLAUX),NL<NLAUX,encontrarListaMayor(LS,LAUX,L_MAX),!.
encontrarListaMayor([L|LS],LAUX,L_MAX):- length(L,NL),length(LAUX,NLAUX),NL=:=NLAUX,encontrarListaMayor(LS,LAUX,L_MAX),!.
encontrarListaMayor([L|LS],LAUX,L_MAX):- length(L,NL),length(LAUX,NLAUX),NL>NLAUX,encontrarListaMayor(LS,L,L_MAX).

esPrefijoYMatchea(PREF,CADENA,REGEX):- prefix(PREF,CADENA), match_inst(PREF,REGEX).

testEj7() :-  prefijoMaximo([a,a,a],[a,a,a,b],star(a))
            , not(prefijoMaximo([],[a,a,a,b],star(a)))
            , not(prefijoMaximo([a],[a,a,a,b],star(a)))
            , not(prefijoMaximo([a,a],[a,a,a,b],star(a)))
            , not(prefijoMaximo([a,a,a,b],[a,a,a,b],star(a)))
            , prefijoMaximo([],[a,a,a,b,b,a],empty)
            , not(prefijoMaximo([a],[a,a,a,b,b,a],empty))
            , prefijoMaximo([a,a,a,b,b],[a,a,a,b,b,a],concat(star(a),star(b)))
            , not(prefijoMaximo([a,a,a],[a,a,a,b,b,a],concat(star(a),star(b))))
            , not(prefijoMaximo([a,a,a,b,b,a],[a,a,a,b,b,a],concat(star(a),star(b)))).


% Ejercicio 8: reemplazar(+X, +R, +E, Res)

reemplazar(CADENA,REGEX,REEMPLAZO,CADENA):-
 findall(S,substringQueMatchea(CADENA,REGEX,S),SS)  % Recolecto substrings que matchean
,length(SS,LSS),LSS=:=0,!.                          % Y Verifico que no haya ninguno

reemplazar(CADENA,REGEX,REEMPLAZO,RESULTADO):-
 findall(S,substringQueMatchea(CADENA,REGEX,S),SS)             % Recolecto substrings que matchean
,length(SS,LSS),LSS>0                                          % Verifico que haya al menos uno
,encontrarListaMayor(SS,[],MAX_S)                              % Busco la mayor lista reemplazable
,append([PRE,MAX_S,POST],CADENA),append([PRE,REEMPLAZO,POST],CADENA_CON_REEMPLAZO_HECHO) % La reemplazo propiamente
,reemplazar(CADENA_CON_REEMPLAZO_HECHO,REGEX,REEMPLAZO,RESULTADO),!.

substringQueMatchea(Cadena,Regex,SubStringMatch):-CS=[C1|R1],R1=[SubStringMatch|R2],R2=[C3],append(CS,Cadena),match_inst(SubStringMatch,Regex).

testEj8() :- reemplazar([a,b],a,[c],[c,b])
           , reemplazar([a,a,a,b,b],concat(star(a),b),[1],[1,1])
           .
