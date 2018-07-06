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

tieneEstrella(star(_)) :- true.
tieneEstrella(or(X,_)) :- tieneEstrella(X),!.
tieneEstrella(or(_,Y)) :- tieneEstrella(Y),!. 
tieneEstrella(concat(X,_)) :- tieneEstrella(X),!.
tieneEstrella(concat(_,Y)) :- tieneEstrella(Y),!.

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
cadena(C):- nonvar(C),append(R,[S],C),symbol(S),cadena(R),!.
cadena([]).
cadena(C) :- append(R,[S],C),cadena(R),symbol(S).

testEj3() :- cadena([])
           , cadena([a])
           , cadena([b])
           , cadena([c])
           , cadena([a,a])
           , cadena([a,b])
           , cadena([a,c])
           , cadena([b,a])
           , cadena([b,b])
           , cadena([b,a])
           , not(cadena([a,d]))
			  .

% Ejercicio 4: match_inst(+Cadena, +RegEx)

match_inst([],empty) :- !.
match_inst([CH],CH) :- symbol(CH),!.
match_inst(C,or(RE1,_)) :- match_inst(C,RE1),!.
match_inst(C,or(_,RE2)) :- match_inst(C,RE2),!.
match_inst(C,concat(RE1,RE2)):- prefix(PREF,C), append(PREF,SUF,C), match_inst(PREF,RE1),match_inst(SUF,RE2),!.
match_inst([],star(_)):-!.
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

%Precondición: TopeMaximo >= 0
cadena([],_).
cadena(C,TopeMaximo) :- cadena(R),symbol(S),length(R,LR),LR<TopeMaximo,append(R,[S],C).

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
,asegurarQueSeaLista(REEMPLAZO,REEMPLAZO_L)							% Aseguro que la cadena reemplazo sea lista para que funcione el append
,append([PRE,MAX_S,POST],CADENA),append([PRE,REEMPLAZO_L,POST],CADENA_CON_REEMPLAZO_HECHO) % La reemplazo propiamente
,reemplazar(CADENA_CON_REEMPLAZO_HECHO,REGEX,REEMPLAZO_L,RESULTADO),!.  						 % Y sigo buscando ...

substringQueMatchea(Cadena,Regex,SubStringMatch):-
  CS=[C1|R1],R1=[SubStringMatch|R2],R2=[C3]					% Recolecto todas las subcadenas de Cadena ...
  ,append(CS,Cadena),match_inst(SubStringMatch,Regex)    % ... que cumplan el patrón de la Regex ...
  ,length(SubStringMatch,L),L>0.									% ... y no sean la cadena vacía.

asegurarQueSeaLista(R,R):-is_list(R),!.
asegurarQueSeaLista(R,R_L):- not(is_list(R)),R_L = [R].

testEj8() :- reemplazar([a,b,c],empty,1,[a,b,c])
           , reemplazar([a,b,c,b,c],or(a,b),1,[1,1,c,1,c])
           , reemplazar([a,b,c,b,c],or(a,b),[1,2],[1,2,1,2,c,1,2,c])
           , reemplazar([a,b,c,b,c],concat(a,b),[1,2],[1,2,c,b,c])
           , reemplazar([a,b,c,b,c],concat(a,b),[],[c,b,c])
           , reemplazar([a,b,c,b,a,b,c],concat(a,b),[],[c,b,c])
           , reemplazar([a,b],a,c,[c,b])
           , reemplazar([a,b],a,[c],[c,b])
           , reemplazar([a,a,a,b,b],concat(star(a),b),1,[1,1])
           , reemplazar([a,a,a,b,b],concat(star(a),b),[1],[1,1])
           , reemplazar([a,a,a,b,b,a,b,c],concat(star(a),star(b)),[1],[1,1,c])
			  , reemplazar([c,a,a,a,c],star(a),[1],[c,1,c])
			  , reemplazar([c,a,a,a,c,a,a,c],star(a),[1],[c,1,c,1,c]).
