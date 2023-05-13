megold(T) :-
    T = napok(h(hetfo,_,_,_), h(kedd,_,_,_),h(szerda,_,_,_),h(csutortok,_,_,_),h(pentek,_,_,_)),
    %A telepi utcai családnál (akiknek a vezetekneve hosszabb mint a malna utcaiake) nem a tavaszi permetezést végezte   
    ezermester(A,T),csalad(A,berkes),nem_permetezes(A),utca(A,telepi),
    ezermester(A2,T),lehetseges_csalad(A2,[feher,karda,paszti]),utca(A2,malna),
    %  Pásztiék akik nem a Jenei utcában laknak, szerdán hívták Józsi bácsit
    ezermester(B,T),csalad(B,paszti),nem_jenei(B),nap(B,szerda),
	% A déri családnál Rózsika utcán, nem hétfőn dolgozott
	ezermester(C,T),csalad(C,deri),utca(C,rozsika),nem_hetfo(C),
	% Egyik nap a fehér család bízta meg valamivel,következő nap kerítést festett át az egyik háznál
	ezermester(D,T),csalad(D,feher),mellette(D,D2,T),
    ezermester(D2,T),munka(D2,kerites),
	%  Hétfőn mosógépet kellett javítania 
    ezermester(E,T),nap(E,hetfo),munka(E,mosogep),
	%A bukta utcában két nappal később dolgozott, mint berkeséknél, ahol tv antennát igazított
	ezermester(F,T),utca(F,bukta),munka(F2,tv),
    ezermester(F2,T),csalad(F2,berkes),mellette2(F2,F,T),
    %Egyik nap ereszcsatornát kellett tisztítania, de nem Kardáéknál
    ezermester(G,T),munka(G,eresz),nem_karda(G),
	%Töltsük fel ami kimaradt -> Nincs fixálva az eresz, se a permetezés
	ezermester(H,T),csalad(H,karda),
    ezermester(I,T),munka(I,permetezes),
    ezermester(J,T),utca(J,jenei).

ezermester(X, napok(X,_,_,_,_)).
ezermester(X, napok(_,X,_,_,_)).
ezermester(X, napok(_,_,X,_,_)).
ezermester(X, napok(_,_,_,X,_)).
ezermester(X, napok(_,_,_,_,X)).

nap(h(X,_,_,_), X).
csalad(h(_,X,_,_), X).
utca(h(_,_,X,_), X).
munka(h(_,_,_,X), X).

nem_permetezes(H) :- munka(H,eresz).
nem_permetezes(H) :- munka(H,mosogep).
nem_permetezes(H) :- munka(H,tv).
nem_permetezes(H) :- munka(H,kerites).

nem_jenei(H) :- utca(H,malna).
nem_jenei(H) :- utca(H,bukta).
nem_jenei(H) :- utca(H,rozsika).
nem_jenei(H) :- utca(H,telepi).

nem_hetfo(H) :- nap(H,kedd).
nem_hetfo(H) :- nap(H,szerda).
nem_hetfo(H) :- nap(H,csutortok).
nem_hetfo(H) :- nap(H,pentek).


nem_karda(H) :- csalad(H,berkes).
nem_karda(H) :- csalad(H,deri).
nem_karda(H) :- csalad(H,feher).
nem_karda(H) :- csalad(H,paszti).

lehetseges_csalad(T, [X|_]):-
    csalad(T, X).
lehetseges_csalad(T, [_|Y]):-
    lehetseges_csalad(T, Y).

koveti(X,Y,napok(X,Y,_,_,_)).
koveti(X,Y,napok(_,X,Y,_,_)).
koveti(X,Y,napok(_,_,X,Y,_)).
koveti(X,Y,napok(_,_,_,X,Y)).

mellette(X,Y,T) :- koveti(X,Y,T).

koveti2(X,Y,napok(X,_,Y,_,_)).
koveti2(X,Y,napok(_,X,_,Y,_)).
koveti2(X,Y,napok(_,_,X,_,Y)).

mellette2(X,Y,T) :- koveti2(X,Y,T).