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


% Ejercicio 1: tieneEstrella(+Regex)
tieneEstrella(star(_)) :- true.
tieneEstrella(or(X,_)) :- tieneEstrella(X),!.
tieneEstrella(or(_,Y)) :- tieneEstrella(Y),!. 
tieneEstrella(concat(X,_)) :- tieneEstrella(X),!.
tieneEstrella(concat(_,Y)) :- tieneEstrella(Y),!.

% Ejercicio 2: longitudMaxima(+Regex, -Length)
longitudMaxima(X,N):- symbol(X),N is 1.
longitudMaxima(empty,0).
longitudMaxima(or(X,Y),N) :- longitudMaxima(X,LX), longitudMaxima(Y,LY),N is max(LX,LY).
longitudMaxima(concat(X,Y),N) :- longitudMaxima(X,LX), longitudMaxima(Y,LY), N is LX + LY.

% Ejercicio 3: cadena(?Cadena)
cadena([]).
cadena(C) :- append(R,[S],C),cadena(R),symbol(S).

% Ejercicio 4: match_inst(+Cadena, +Regex)
match_inst([],empty) :- !.
match_inst([CH],CH) :- symbol(CH),!.
match_inst(C,or(RE1,_)) :- match_inst(C,RE1),!.
match_inst(C,or(_,RE2)) :- match_inst(C,RE2),!.
match_inst(C,concat(RE1,RE2)):- append(PREF,SUF,C), match_inst(PREF,RE1),match_inst(SUF,RE2),!.
match_inst([],star(_)):-!.
match_inst(C,star(RE)) :- append(PREF,RESTO,C), match_inst(PREF,RE), match_inst(RESTO,empty),!.
match_inst(C,star(RE)) :- append(PREF,RESTO,C), match_inst(PREF,RE), match_inst(RESTO,star(RE)),!.


% Ejercicio 5: match(?Cadena, +Regex)
match(C,Regex) :- cadenaCandidataParaRegex(C,Regex),match_inst(C,Regex).

%cadenaCandidataParaRegex(?C,+Regex). Es verdadero sii C es cadena candidata por su longitud para Regex
cadenaCandidataParaRegex(C,Regex):-longitudMaxima(Regex,LongitudMaxima),cadena(C,LongitudMaxima).
cadenaCandidataParaRegex(C,Regex):-not(longitudMaxima(Regex,_)),cadena(C).

%cadena(?C,+LongitudMaxima). Precondición: LongitudMaxima >= 0
cadena([],_).
cadena([S|R],LongitudMaxima):- LongitudMaxima > 0,cadena(R,LongitudMaxima-1),symbol(S).

% Ejercicio 6: diferencia(?Cadena, +Regex, +Regex)
diferencia(C,R1,R2) :- cadenaCandidataParaRegex(C,R1),match_inst(C,R1),not(match_inst(C,R2)).

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +Regex)
prefijoMaximo(MaxPref,Cadena,Regex):- prefix(MaxPref,Cadena),match_inst(MaxPref,Regex)
,length(MaxPref,L),not(prefijoMayor(Cadena,Regex,L,OtroPrefijo)).

%prefijoMayor(+Cadena,+Regex,+Long,?OtroPrefijo). 
%Verdadero cuando OtroPrefijo es un prefijo de Cadena que matchea con Regex y su longitud es mayor a Long
prefijoMayor(Cadena,Regex,Long,OtroPrefijo):- prefix(OtroPrefijo,Cadena),match_inst(OtroPrefijo,Regex),length(OtroPrefijo,L),L>Long.

% Ejercicio 8: reemplazar(+X, +R, +E, Res)

reemplazar([],_,_,[]):-!.								  % Caso de Cadena vacía

% Caso en que el máximo prefijo que matchea NO es []
reemplazar(Cadena,Regex,Reemplazo,Resultado):-  
	 prefijoMaximo(PrefMaximo,Cadena,Regex), not(PrefMaximo=[])
	,asegurarQueSeaLista(Reemplazo,ReemplazoL)	% Detalle de Implementación para que funcione el append
	,append(PrefMaximo,Resto,Cadena),reemplazar(Resto,Regex,Reemplazo,RestoReemplazado)	
	,append(ReemplazoL,RestoReemplazado,Resultado),!. 	

% Caso en que No hay Máximo Prefijo que matchea, o éste existe, pero es [].
reemplazar([H|Tail],Regex,Reemplazo,Resultado):- reemplazar(Tail,Regex,Reemplazo,ResTail),Resultado=[H|ResTail].		

asegurarQueSeaLista(R,R):-is_list(R),!.
asegurarQueSeaLista(R,R_L):- not(is_list(R)),R_L = [R].

% -------------------------------- TESTS: ------------------------------------------------------------

testTodo():- testEj1(),testEj2(),testEj3(),testEj4(),testEj5(),testEj6(),testEj7(),testEj8().

testEj1() :- not(tieneEstrella(a))
           , not(tieneEstrella(or(a,b)))
           , tieneEstrella(star(a))
           , tieneEstrella(or(a,star(b)))     
           , tieneEstrella(or(star(b),a))      
           , tieneEstrella(or(concat(star(a),b),c))
           , tieneEstrella(star(concat(a,b))).    

testEj2() :-  longitudMaxima(empty,0)
            , longitudMaxima(a,1)                    
            , longitudMaxima(b,1)                      
            , not(longitudMaxima(z,N))
            , longitudMaxima(or(a,b),1)              
            , longitudMaxima(concat(a,b),2)         
            , longitudMaxima(concat(a,empty),1)    
            , longitudMaxima(concat(or(a,b),b),2)  
            , not(longitudMaxima(concat(or(a,star(b)),b),N)).

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
           , not(cadena([a,d])).

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
				,not(match_inst([a,a,b,b],star(concat(a,b))))
				,match_inst([],star(or(a,b)))
				,match_inst([a],star(or(a,b)))
				,match_inst([b],star(or(a,b)))
				,match_inst([a,a],star(or(a,b)))
				,match_inst([b,b],star(or(a,b)))
				,match_inst([a,a,b,b],star(or(a,b)))
				,match_inst([a,b,b,b],star(or(a,b)))
				,match_inst([b,a,b,a],star(or(a,b)))
				,match_inst([a,a,b,b,a],star(or(a,b)))
				,not(match_inst([c],star(or(a,b))))
				,not(match_inst([a,a,c,b,b],star(or(a,b))))
				,not(match_inst([a,c,b,b],star(or(a,b)))) .

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

testEj8() :- reemplazar([a,b,c],empty,1,[a,b,c])
			  , reemplazar([a,b,b],or(a,b),1,[1,1,1])
			  , reemplazar([a,b,c],or(a,b),1,[1,1,c])
			  , reemplazar([a,c,b],or(a,b),1,[1,c,1])
			  , reemplazar([c,c,c],or(a,b),1,[c,c,c])
			  , reemplazar([a,b,b],star(or(a,b)),1,[1])
			  , reemplazar([a,b,c],star(or(a,b)),1,[1,c])
			  , reemplazar([a,c,b],star(or(a,b)),1,[1,c,1])
			  , reemplazar([c,c,c],star(or(a,b)),1,[c,c,c])
			  , reemplazar([a,b,a,c,a,a,b,b,c],star(or(a,b)),1,[1,c,1,c])
			  , reemplazar([c,b,a,c,a,a,b,b,a],star(or(a,b)),1,[c,1,c,1])
			  , reemplazar([c,b,a,c,a,a,b,b,c],star(or(a,b)),1,[c,1,c,1,c])
			  , reemplazar([a,b,a,c,a,a,b,b,a],star(or(a,b)),1,[1,c,1])
			  , reemplazar([c,b,a,a,a,b,b,c],star(or(a,b)),1,[c,1,c])
			  , reemplazar([c,b,a,c,c,c,a,a,b,b,c],star(or(a,b)),1,[c,1,c,c,c,1,c])
			  , reemplazar([c,b,a,c,c,c,a,a,b,b],star(or(a,b)),1,[c,1,c,c,c,1])
			  , reemplazar([b,a,c,c,c,a,a,b,b,c],star(or(a,b)),1,[1,c,c,c,1,c])
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
			  , reemplazar([c,a,a,a,c,a,a,c],star(a),[1],[c,1,c,1,c])
			  , reemplazar([a,b,a,c,c,a,c,a,b,b,c],star(or(a,b)),1,[1,c,c,1,c,1,c])
			  , reemplazar([c,c,a,b,a,c,c,a,c,a,b,b,c],star(or(a,b)),1,[c,c,1,c,c,1,c,1,c])
			  , reemplazar([a,b,a,c,c,a,c,a,b,b],star(or(a,b)),1,[1,c,c,1,c,1])
			  , reemplazar([a,b,a,c,c,c,a,b,b,c],star(or(a,b)),1,[1,c,c,c,1,c])
			 .
