% Un predicat care determină valoarea maximului listei.
% 
% max_element(l1...ln) = 
% { l1, dacă L = [l1]
% { max_element(l2...ln), dacă l1 <= max_element(l2...ln) 
% { l1, altfel
max_element([H], H).
max_element([H|T], H) :-
    max_element(T, TMax),
    H > TMax.
max_element([H|T], TMax) :-
    max_element(T, TMax),
    H =< TMax.

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

% Un predicat care determină numărul de apariţii ale unei valori într-o listă.
%
% nOcc(L, X) =
% { 0, dacă L = []
% { 1 + nOcc(l2...ln, X), dacă X = l1
% { nOcc(l2...ln, X), altfel
nOcc([], _, 0).
nOcc([H|T], Elem, N) :-
    H = Elem,
    nOcc(T, Elem, KN),
    N is KN + 1.
nOcc([H|T], Elem, N) :-
    H \= Elem,
    nOcc(T, Elem, N).

% Un predicat care determină lungimea unei liste.
% 
% list_length(L) =
% { 0, dacă L = []
% { 1 + list_length(l2...ln), altfel
list_length([], 0).
list_length([_|T], N) :-
    list_length(T, KN),
    N is KN + 1.

% Un predicat care şterge toate apariţiile unei valori dintr-o listă.
% 
% delete_all(L, X) =
% { [], dacă L = []
% { delete_all(l2...ln, X), dacă l1 = X
% { l1 U delete_all(l2...ln, X), altfel
delete_all([], _, []).
delete_all([H|T], Elem, R) :-
    H =:= Elem,
    delete_all(T, Elem, RT),
    R = RT.
delete_all([H|T], Elem, R) :-
    H \= Elem,
    delete_all(T, Elem, RT),
    R = [H|RT].

% Scrieți un predicat care adaugă valoarea maximului listei 
% după fiecare poziție putere a lui 3.
%
% add_element_after_pow3(L, Elem, K) =
% { [], dacă L = []
% { l1 U Elem U add_element_after_pow3(l2...ln, Elem, K+1), dacă is_power_of(K, 3) = true
% { l1 U add_element_after_pow3(l2...ln, Elem, K+1), altfel
add_element_after_pow3([], _, _, []).
add_element_after_pow3([H|T], Elem, K, [H,Elem|R]) :-
    is_power_of(K, 3),
    KR is K + 1,
    add_element_after_pow3(T, Elem, KR, R).
add_element_after_pow3([H|T], Elem, K, [H|R]) :-
    not(is_power_of(K, 3)),
    KR is K+1,
    add_element_after_pow3(T, Elem, KR, R).

ex4([], []).
ex4(L, R) :-
    max_element(L, Max),
	add_element_after_pow3(L, Max, 1, R).

% L = [], ex4(L, R).
% L = [1], ex4(L, R).
% L = [1, 2, 3], ex4(L, R).

% Scrieți un predicat care descompune o listă într-una de forma: 
% [elemente_unice, elemente_doubled]. 
% În plus, va returna: 
% numărul_elementelor_unice și numărul_elementelor_doubled.
% 
% split(L, unice, repetate) =
% { unice = [], repetate = [], dacă L = []
% { split(l2...ln, l1 U unice, repetate), dacă nOcc(L, l1) = 1 
% { split(l2...ln, unice, l1 U repetate), altfel

split([], [], []).
split([H|T], Unique, Doubled) :-
    nOcc([H|T], H, Frv),
    Frv =:= 1,
    delete_all([H|T], H, D),
    split(D, RUnique, Doubled),
    Unique = [H|RUnique].
split([H|T], Unique, Doubled) :-
    nOcc([H|T], H, Frv),
    Frv \= 1,
    delete_all([H|T], H, D),
    split(D, Unique, RDoubled),
    Doubled = [H|RDoubled].

ex9(L, [Unique, Doubled], Nr_unique, Nr_doubled) :-
    split(L, Unique, Doubled),
    list_length(Unique, Nr_unique),
    list_length(Doubled, Nr_doubled).

% L = [], ex9(L, R, Nr_unique, Nr_doubled).
% L = [1, 2, 1, 3], ex9(L, R, Nr_unique, Nr_doubled).
% L = [1, 2, 3], ex9(L, R, Nr_unique, Nr_doubled).
% L = [1, 1, 2, 2], ex9(L, R, Nr_unique, Nr_doubled).

% Scrieți un predicat care să adauge 
% după fiecare poziție putere a lui 2 suma dintre elem_K-1 si elem_K+1.

% add_sum_after_pow2(L, K) =
% { [], dacă L = []
% { [l1, l1], dacă L = [l1] şi K = 2
% { [l1, l2, l2, l1], dacă L = [l1, l2] şi K = 2
% { l2 U add_sum_after_pow2([], K + 1), dacă L = [l1, l2], K =\= 2 şi is_power_of(K - 1, 2) = true
% { [l1, l2, l1], dacă L = [l1, l2], K =\= 2, is_power_of(K, 2) = false şi is_power_of(K - 1, 2) = true
% { [l2, l2], dacă L = [l1, l2], K =\= 2, is_power_of(K, 2) = false şi is_power_of(K - 1, 2) = false
% { [l0, l1, l1, l0 + l2] U add_sum_after_pow2(l1...ln), dacă is_power_of(K, 2) = true, K = 2
% { [l0, l1, l0 + l2] U add_sum_after_pow2(l1...ln), dacă is_power_of(K, 2) = true, K =\= 2
% { l0 U add_sum_after_pow2(l1...ln), dacă is_power_of(K, 2) = false şi is_power_of(K - 1, 2) = false
% { add_sum_after_pow2(l1...ln), altfel

add_sum_after_pow2([], _, []).
add_sum_after_pow2([H], K, [H, H]) :-
    K =:= 2,
    add_sum_after_pow2([], _, []).
add_sum_after_pow2([H1, H2], K, [H1, H2, H2, H1]) :-
    K =:= 2,
    add_sum_after_pow2([], _, []).
add_sum_after_pow2([_, H2], K, [H2|R]) :-
    K \= 2,
    KP is K - 1,
    is_power_of(KP, 2),
    KN is K + 1,
    add_sum_after_pow2([], KN, R).
add_sum_after_pow2([H1, H2], K, [H1, H2, H1]) :-
    K \= 2,
    KP is K - 1,
    not(is_power_of(KP, 2)),
    is_power_of(K, 2),
    KN is K + 1,
    add_sum_after_pow2([], KN, []).
add_sum_after_pow2([H1, H2], K, [H1, H2]) :-
    K \= 2,
    KP is K - 1,
    not(is_power_of(KP, 2)),
    not(is_power_of(K, 2)),
    KN is K + 1,
    add_sum_after_pow2([], KN, []).
add_sum_after_pow2([Prev, Current, Succ|T], K, [Prev, Current, Current, Sum|R]) :-
    is_power_of(K, 2),
    K =:= 2,
    Sum is Prev + Succ,
    KN is K + 1,
    add_sum_after_pow2([Current, Succ|T], KN, R).
add_sum_after_pow2([Prev, Current, Succ|T], K, [Prev, Current, Sum|R]) :-
    is_power_of(K, 2),
    K \= 2,
    Sum is Prev + Succ,
    KN is K + 1,
    add_sum_after_pow2([Current, Succ|T], KN, R).
add_sum_after_pow2([Prev, Current, Succ|T], K, [Prev|R]) :-
    not(is_power_of(K, 2)),
    KR is K - 1,
    not(is_power_of(KR, 2)),
    KN is K + 1,
    add_sum_after_pow2([Current, Succ|T], KN, R).
add_sum_after_pow2([_, Current, Succ|T], K, R) :-
    not(is_power_of(K, 2)),
    KR is K - 1,
    is_power_of(KR, 2),
    KN is K + 1,
    add_sum_after_pow2([Current, Succ|T], KN, R).

ex10(L, R) :-
    add_sum_after_pow2(L, 2, R).

% L = [], ex10(L, R).
% L = [1], ex10(L, R).
% L = [1, 2], ex10(L, R).
% L = [1, 2, 3, 4], ex10(L, R).
% L = [1, 2, 3, 4, 5], ex10(L, R).

