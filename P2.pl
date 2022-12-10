% Un predicat care concatenează două liste.
%
% append(X, Y) =
% { Y, dacă X = []
% { X1 U append(X2...Xn, Y), altfel

append([], X, X).
append([H|T], L, [H|R]) :-
    append(T, L, R).

% Un predicat care determină dacă o valoare este putere a unui număr P.
% 
% is_power_of(X, P) =
% { true, dacă X = 1
% { true, dacă X =\= 0, X mod P = 0 şi is_power_of(X / P) = true
% { false, altfel

is_power_of(1, _).
is_power_of(X, Pow) :-
    X \= 0,
    mod(X, Pow) =:= 0,
    XR is X / Pow,
    is_power_of(XR, Pow).

% Scrieți un predicat care înlocuiește toate aparițiile unui element cu o listă.
%
% replace_element(L, Val, X) =
% { [], dacă L = []
% { L1 U replace_element(L2...Ln, Val, X), dacă L1 != Val
% { X1...Xn U replace_element(l2...ln, Val, X), altfel

replace_element([], _, _, []).
replace_element([H|T], Elem, L, [H|R]) :-
    H =\= Elem,
    replace_element(T, Elem, L, R).

replace_element([H|T], Elem, L, R) :-
    H =:= Elem,
    replace_element(T, Elem, L, Rp),
	append(L, Rp, R).

% replace_element([4, 1, 4], 4, [11, 11], R).
% replace_element([4, 1, 4], 4, [4, 4, 4], R).


% Pentru o listă Eterogenă, formată din numere întregi și liste de numere, 
% scrieți un predicat care înlocuiește în  fiecare sublistă valoarea primului element din sublistă 
% cu o nouă listă.
%
% ex5(L, X) =
% { [], dacă L = []
% { L1 U ex5(L2...Ln, X), dacă is_list(L1) = false
% { replace_element_with_list(L1, X) U ex5(L2...Ln, X), altfel

ex5([], _, []).
ex5([H|T], NL, [H|R]) :-
    not(is_list(H)),
    ex5(T, NL, R).

ex5([[LH|LT]|T], NL, [M|R]) :-
    is_list([LH|LT]),
    replace_element([LH|LT], LH, NL, M),
    ex5(T, NL, R).

% ex5([1,[4,1,4],3,6,[7,10,1,3,9],5,[1,1,1],7], [11,11], R).


% Scrieți un predicat care adaugă după pozițiile 1,3,7,15,..,(2^k)-1, o valoare dată Val.
%
% add_element_after(L, Val, K) =
% { [], dacă L = []
% { L1 U Val U add_element_after(L2...Ln, Val, K+1), dacă is_power_of(K+1,2) = true
% { L1 U add_element_after(L2...Ln, Val, K+1), altfel

add_element_after([], _, _, []).
add_element_after([H|T], Val, K, [H,Val|R]) :-
    KR is K + 1,
    is_power_of(KR, 2),
    add_element_after(T, Val, KR, R).

add_element_after([H|T], Val, K, [H|R]) :-
    KR is K + 1,
    not(is_power_of(KR, 2)),
    add_element_after(T, Val, KR, R).

% add_element_after([1,2,3,4,5,6,7,8,9], 10, 1, R).


% Pentru o listă Eterogenă, formată din numere întregi și liste de numere, 
% scrieți un predicat care adaugă după pozițiile 1,3,7,15,..,(2^k)-1 din sublistă,
% valoarea dinaintea listei.
%
% ex9(L) = 
% { [], dacă L = []
% { [L1], dacă L = [L1]
% { L1 U add_element_after(X1...Xn, X1, 1) U ex9(L2...Ln), dacă L1 este număr şi X = L2 este listă
% { L1 U ex9(L2...Ln), altfel

ex9([], []).
ex9([H], [H]).
ex9([H1, H2|T], [H1, M|R]) :-
    number(H1),
    is_list(H2),
    add_element_after(H2, H1, 1, M),
    ex9(T, R).
ex9([H1, H2|T], [H1|R]) :-
    number(H1),
    not(is_list(H2)),
    ex9([H2|T], R).
ex9([H1, H2|T], [H1|R]) :-
    not(number(H1)),
    ex9([H2|T], R).

% ex9([1,[2,3],7,[4,1,4],3,6,[7,5,1,3,9,8,2,7],5], R).
% ex9([[2,3],[4,5]], R).
% ex9([[1,2,3],1,[],2,3,4,5], R).


% Scrieți un predicat care șterge toate secvențele de valori consecutive.
%
% delete_consecutive(L, Deleted) =
% { [], dacă L = []
% { [], dacă L = [L1] şi Deleted = 1
% { [L1], dacă L = [L1] şi Deleted = 0
% { delete_consecutive(L2...Ln, 1), dacă H1 + 1 = H2
% { delete_consecutive(L2...Ln, 0), dacă H1 + 1 != H2 şi Deleted = 1
% { L1 U delete_consecutive(L2...Ln, Deleted), dacă H1 + 1 != H2 şi Deleted = 0

delete_consecutive([], _, []).
delete_consecutive([_], Deleted, []) :-
    Deleted =:= 1.
delete_consecutive([H], Deleted, [H]) :-
    Deleted =\= 1.
delete_consecutive([H1,H2|T], _, R) :-
    Succ is H1 + 1,
    H2 =:= Succ,
    delete_consecutive([H2|T], 1, R).
delete_consecutive([H1,H2|T], Deleted, R) :-
    Succ is H1 + 1,
    H2 =\= Succ,
    Deleted =:= 1,
    delete_consecutive([H2|T], 0, R).
delete_consecutive([H1,H2|T], Deleted, [H1|R]) :-
    Succ is H1 + 1,
    H2 =\= Succ,
    Deleted =:= 0,
    delete_consecutive([H2|T], Deleted, R).

% delete_consecutive([1,3,4,5,7,8,10,12,13,15], 0, R).
% delete_consecutive([1,2,3,4], 0, R).
% delete_consecutive([1, 3, 5, 7], 0, R).


% Pentru o listă Eterogenă, formată din numere întregi și liste de numere, 
% scrieți un predicat care șterge toate secvențele de valori consecutive din fiecare sublistă.
%
% ex13(L) = 
% { [], dacă L = []
% { L1 U ex13(L2...Ln), dacă L1 nu este listă
% { delete_consecutive(L1, 0) U ex13(L2...Ln), altfel

ex13([], []).
ex13([H|T], [H|R]) :-
    not(is_list(H)),
    ex13(T, R).
ex13([H|T], [M|R]) :-
    is_list(H),
	delete_consecutive(H, 0, M),
    ex13(T, R).

% ex13([1,[2,3,5],9,[1,2,4,3,4,5,7,9],11,[5,8,2],7], R).
% ex13([[2,3,4],[20,21,13,12,13,15],20], R).

