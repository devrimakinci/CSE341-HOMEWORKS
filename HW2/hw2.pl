
% Adi : Devrim
% Soyadi : AKINCI
% Numarasi : 141044052

% PART-1

% All Facts

flight(edirne,edremit,5).

flight(edremit,edirne,5).
flight(edremit,erzincan,7).

flight(erzincan,edremit,7).

flight(istanbul,izmir,3).
flight(istanbul,trabzon,3).
flight(istanbul,ankara,5).

flight(trabzon,ankara,2).
flight(trabzon,istanbul,3).

flight(izmir,ankara,6).
flight(izmir,antalya,1).
flight(izmir,istanbul,3).

flight(ankara,konya,8).
flight(ankara,istanbul,5).
flight(ankara,trabzon,2).
flight(ankara,izmir,6).

flight(konya,kars,5).
flight(konya,ankara,8).
flight(konya,diyarbakir,1).

flight(kars,konya,5).
flight(kars,gaziantep,3).

flight(gaziantep,kars,3).

flight(diyarbakir,antalya,5).
flight(diyarbakir,konya,1).

flight(antalya,izmir,1).
flight(antalya,diyarbakir,5).

% Predicate

:- dynamic visited/1. % retractall ile assert kullanabilmek icin tanimlandi.

route(X,Z,C) :- retractall(visited(_)),routehelp(X,Z,C).
routehelp(X,Z,C) :- flight(X,Z,C).
routehelp(X,Z,R) :- flight(X,Y,A),not(visited(Y)),assert(visited(Y)),routehelp(Y,Z,B),not(Z ==X),R is A+B.

% PART-2

% minmum predicate verilen dizideki en kucuk elemani bulur ve onu dondurur.
minimum([Data],Data).
minimum([Data|L],Data) :- minimum(L,Res),Res >= Data,!.
minimum([_|L],R) :- minimum(L,R).

croute(X,Z,C) :- retractall(visited(_)),croutehelp(X,Z,List),minimum(List,C),!.
croutehelp(X,Z,[List]) :- flight(X,Z,List).
croutehelp(X,Z,List) :- croutehelp(X,Z,_,List).
croutehelp(X,Z,R,[]) :- flight(X,Z,R).
croutehelp(X,Z,R,[R|Listed]) :- flight(X,Y,A),not(visited(Y)),assert(visited(Y)),croutehelp(Y,Z,B,Listed),not(Z ==X),R is A+B.

%PART-3

enrol(1,a).
enrol(1,b).
enrol(2,a).
enrol(3,b).
enrol(4,c).
enrol(5,d).
enrol(6,d).
enrol(6,a).

% Added 5 attendee
enrol(7,c).
enrol(7,f).
enrol(8,a).
enrol(8,g).
enrol(9,g).
enrol(9,f).
enrol(10,b).
enrol(10,d).
enrol(11,d).
enrol(11,a).
enrol(11,f).

when(a,10).
when(b,12).
when(c,11).
when(d,16).
when(e,17).

% Added session
when(f,13).
when(g,12).

where(a,101).
where(b,104).
where(c,102).
where(d,103).
where(e,103).

% Added session
where(f,101).
where(g,102).

schedule(S,P,T) :- enrol(S,S1),when(S1,T),where(S1,P).

conflict(X,Y) :- when(X,T1),where(X,P1),when(Y,T2),where(Y,P2),P1 =:= P2,not(X ==Y),R is T1-T2,-2 < R,R < 2.

usage(P,T) :- where(S1,P),when(S1,T).

meet(X,Y) :- not(X ==Y),enrol(X,S1),enrol(Y,S2),when(S1,T1),when(S2,T2),where(S1,P1),where(S2,P2),T1 =:= T2,P1 =:= P2,!.

% PART-4

% UNION (PART-4.1)
helperUnion([],R,R). % Base case
helperUnion([X|L],S,U) :- member(X,S),helperUnion(L,S,U),!. % İlk listenin ilk elemanı diğer listede varsa diğer elemana geçme
helperUnion([X|L],S,[X|U]) :- not(member(X,S)),helperUnion(L,S,U). % İlk listenin ilk elemanı diğer listede yoksa sonuç listesine ekleme
union(L,S,U) :- helperUnion(L,S,U).

% INTERSECTION (PART-4.2)
helperIntersect([],_,[]). % Base case
helperIntersect([X|L1],L2,[X|I]) :- member(X,L2),helperIntersect(L1,L2,I),!. % Listedeki ilk eleman diğer listede varsa sonuç listesine ekleme
helperIntersect([_|L1],L2,I) :- helperIntersect(L1,L2,I). % Eğer ilk listedeki eleman diğer listede yoksa diğer elemana geçme
intersect(L1,L2,I) :- helperIntersect(L1,L2,I).

% FLATTEN(PART-4.3)
concatList([],L,L).
concatList([X|L1],L2,[X|L3]) :- concatList(L1,L2,L3).

helperFlatten([],[]) :- !. % Base case 
/* Recursive olarak listenin elemanlarını tek bir listeye çevirir. Cut operatorü, prologun başka alternatiflere bakmasını
önlemek için kullanıldı.*/
helperFlatten([Head|Tail],Flatten) :- helperFlatten(Head,List1),helperFlatten(Tail,List2),concatList(List1,List2,Flatten),!.
helperFlatten(E,[E]). % Eğer tek eleman varsa onu listeye ekler.
flatten(L,F) :- helperFlatten(L,F).
