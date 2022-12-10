% Un predicat care verifică dacă 3 puncte sunt coliniare.

/*
|x1 y1 1|   |x1     y1     1|
|x2 y2 1| = |x2-x1  y2-y1  0| = (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
|x3 y3 1|   |x3-x1  y3-y1  0|
*/
verif_collinear([[X1, Y1], [X2, Y2], [X3, Y3]]) :-
    Det is (X2 - X1) * (Y3 - Y1) - (X3 - X1) * (Y2 - Y1),
    Det =:= 0.

% Un predicat care determină toate combinaţiile de k elemente ale unei liste.
%
% comb(L, N) =
% { L1, dacă N = 1
% { L1 U comb(L2...Ln, N - 1), dacă N - 1 > 0
% { comb(L2...Ln, N), altfel
comb([H|_], 1, [H]).
comb([H|T], K, [H|R]) :-
        K1 is K - 1,
        K1 > 0,
        comb(T, K1, R).
comb([_|T], K, R) :-
    comb(T, K, R).

% Un predicat care selectează (elimină) elementul X dintr-o listă.
%
% select(L, X) = 
% { T, dacă L1 = X
% { L1 U select(L2...Ln, X), altfel
select([H|T], H, T).
select([H|T], Elem, [H|R]) :-
      select(T, Elem, R).

% Un predicat care verifică dacă lista conţine elementul X.
%
% contains(L, X) =
% { false, dacă L = []
% { true, dacă L1 = X
% { contains(L2...Ln, X), altfel
contains([H|T], X) :-
    H = X;
    contains(T, X).

% Un predicat care returnează valori în intervalul [N, M].
%
% interval(N, M) =
% { N, dacă N <= M
% { bet(N + 1, M), altfel
interval(N, M, K) :- 
    N < M, 
    K = N.
interval(N, M, K) :- 
    N == M, 
    !, 
    K = N.
interval(N, M, K) :- 
    N < M, 
    N1 is N + 1,
    interval(N1, M, K).

% Un predicat care generează o pereche [X,Y] cu proprietatea că N = X + Y şi X < Y.
generatePair(N, X, Y) :-
    interval(1, N, X),
    Y is N - X,
    X < Y.

% Un predicat care descompune un număr într-o sumă de valori.
% decomposeNumber(N) =
% { X U decomposeNumber(Y), dacă există o pereche [X, Y] pentru care X + Y = N şi X < Y.
% { N, altfel
decomposeNumber(N, [N]).
decomposeNumber(N, [X|R]) :- 
    generatePair(N, X, Y),
    decomposeNumber(Y, R).

% Un predicat care verifică dacă elementele dintr-o listă sunt consecutive.
%
% isConsecutive(L) =
% { true, dacă L = [L1, L2] şi L1 + 1 = L2
% { isConsecutive(L2...Ln), dacă length(L) > 2 şi L1 + 1 = L2
% { false, altfel
isConsecutive([X, Y]) :-
    X+1 =:= Y.
isConsecutive([H1, H2|T]) :- 
    H1+1 =:= H2,
    isConsecutive([H2|T]).



% Fiind date n puncte în plan (reprezentate ca perechi de coordonate), 
% scrieți un predicat care să determine toate subseturile de puncte coliniare.
ex2(L, Triplet) :-
    comb(L, 3, Triplet),
    verif_collinear(Triplet).

mainEx2(L, RALL) :-
    findall(ROS, ex2(L, ROS), RALL).

% mainEx2([[1,1], [2,2], [3,3], [4,5], [5,6], [6,7], [0,0], [2,3]], R).

% Scrieți un predicat care sa determine toate descompunerile unui număr 
% în suma de numere consecutive. 
ex3(N, R) :- 
    decomposeNumber(N, R),
    isConsecutive(R).

mainEx3(N, RALL) :-
    findall(ROS, ex3(N, ROS), RALL).

% mainEx3(9, RALL).

% Scrieți un predicat care determină toate aranjamentele de k elementele ale unei liste.
% Eg.: [2, 3, 4] K=2 => [[2,3], [3,2], [2,4], [4,2], [3,4], [4,3]] (nu neaparat in aceasta ordine)

% ex5(L, N) =
% { L1 U ex5(M, N - 1, L2...Ln), dacă N > 1 şi M = select(L, L1)
% { X U ex5(L, X), unde X = contains(L), altfel

ex5(L, N, [H|T]) :-
    N > 1,
    select(L, H, M),
    N1 is N - 1,
    ex5(M, N1, T).
ex5(L, 1, [X]) :- 
    contains(L, X).

mainEx5(L, K, RALL):-
    findall(ROS, ex5(L, K, ROS), RALL).

% mainEx5([2, 3, 4], 2, RALL).