/* Pracownia 2, Metody Programowania.
 * 
 * Predykat prove/2 próbuje wyprowadzić rezolucyjny dowód sprzeczności 
 * podanego zbiory klauzul, jeśli nie są sprzeczne zwraca false. 
 * Predykat resolve/4 oblicza rezolwentę dwóch klauzul w zależności
 * od podanej zmiennej. 
 * Autor: Klaudia Weigel */

:- module(klaudia_weigel, [resolve/4, prove/2]).

:- op(200, fx, ~).
:- op(500, xfy, v).

zmienna(V) :- V= ~_, !.
zmienna(V) :- atom(V).

% prdykat member_of_clause(+Clause,+Var), sprawdza czy dana zmienna Var 
% należy do klauzuli.
member_of_clause(X, X) :- atom(X).
member_of_clause(~X, ~X) :-atom(X).
member_of_clause(Clause, X) :- Clause=Clause1 v Z, atom(Z), member_of_clause(Clause1, X), !.
member_of_clause(Clause, X) :- Clause=Z v Clause1, atom(Z), member_of_clause(Clause1, X), !.
member_of_clause(Clause, X) :- Clause=Clause1 v ~Z, atom(Z), member_of_clause(Clause1, X), !.
member_of_clause(Clause, X) :- Clause= ~Z v Clause1, atom(Z), member_of_clause(Clause1, X), !.
member_of_clause(Clause, X) :- Clause=X v _, atom(X), !.
member_of_clause(Clause, X) :- Clause=_ v X, atom(X), !.
member_of_clause(Clause, ~X) :- Clause= ~X v _, atom(X), !.
member_of_clause(Clause, ~X) :- Clause=_ v ~X, atom(X), !.

% predykat subtract_member(+Clause,+Var,-NewClause) tworzy nową
% klauzulę która jest wynikiem odjęcia od pierwszej zmiennej Var.
subtract_member(Clause, X, NewClause) :- 
    Clause=Clause1 v Z, 
    atom(Z),
    NewClause=NewClause2 v Z,
    subtract_member(Clause1, X, NewClause2), !.
subtract_member(Clause, X, NewClause) :- 
    Clause=Z v Clause1, 
    atom(Z),
    NewClause=Z v NewClause2,
    subtract_member(Clause1, X, NewClause2), !. 
subtract_member(Clause, X, NewClause) :- 
    Clause=Clause1 v ~Z, 
    atom(Z),
    NewClause=NewClause2 v ~Z,
    subtract_member(Clause1, X, NewClause2), !.        
subtract_member(Clause, X, NewClause) :- 
    Clause= ~Z v Clause1, 
    atom(Z),
    NewClause= ~Z v NewClause2,
    subtract_member(Clause1, X, NewClause2), !.    
subtract_member(Clause, X, NewClause) :-
    Clause=X v Y,
    NewClause=Y, !.    
subtract_member(Clause, X, NewClause) :-
    Clause=Y v X,
    NewClause=Y, !.    
subtract_member(Clause, ~X, NewClause) :-
    Clause= ~X v Y,
    NewClause=Y, !.
subtract_member(Clause, ~X, NewClause) :-
    Clause=Y v ~X,
    NewClause=Y, !.

subtract_member_(X, X, []) :- zmienna(X), !.
subtract_member_(Clause, X, NewClause) :-
    member_of_clause(Clause, X),
    subtract_member(Clause, X, NC),
    subtract_member_(NC, X, NewClause), !.
subtract_member_(Clause, X, NewClause) :-
    not(member_of_clause(Clause, X)),
    NewClause=Clause, !.


% scala ze sobą dwie klauzule bez powtórzeń zmiennych
mergeTwoClauses([], Clause, Clause) :- !.
mergeTwoClauses(Clause, [], Clause) :- !.
mergeTwoClauses(Var, Clause, Clause) :- zmienna(Var), member_of_clause(Clause, Var), !.
mergeTwoClauses(Var, Clause, Var v Clause) :- zmienna(Var), not(member_of_clause(Clause, Var)), !.
mergeTwoClauses(Var v Clause1, Clause2, Merged) :-
    member_of_clause(Clause2, Var), !,
    mergeTwoClauses(Clause1, Clause2, Merged).
mergeTwoClauses(Var v Clause1, Clause2, Var v Merged) :-
    mergeTwoClauses(Clause1, Clause2, Merged).

% Predykat resolve znajdujący resolwentę dwóch klauzul,
% sprawdza czy zmienna pozytywna należy do klauzuli 
% pierwszej, a negetawny do drugiej, jeśli tak to 
% odejmuje te zmienne z każdej z klauzul i scala nowo
% powstałe bez powtórzeń.

resolve(Var, Var, Clause, R) :-
    member_of_clause(Clause, ~Var),
    subtract_member_(Clause, ~Var, C),
    R=C, !.
resolve(Var, Var, Clause, R) :-
    member_of_clause(Clause, ~Var),
    subtract_member_(Clause, ~Var, C),
    R=C, !.
    
resolve(Var, Clause, ~Var, R) :-
    member_of_clause(Clause, Var),
    subtract_member_(Clause, Var, C),
    R=C, !.
resolve(Var,Var,~Var,[]) :-!.
resolve(Var, Clause1, Clause2, R) :-
    member_of_clause(Clause1, Var),
    member_of_clause(Clause2, ~Var),
    subtract_member_(Clause1, Var, C1),
    subtract_member_(Clause2, ~Var, C2),
    (C1==[], C2==[] -> R=[]; mergeTwoClauses(C1, C2, R), !).

% zamienia pojedynczą klauzulę na listę np. [p v ~p] -> [p, ~p]
axiomToList(X v Y, [X|Res]) :-
    zmienna(X),
    axiomToList(Y, Res), !.
axiomToList(X, [X]) :- zmienna(X), !.

% tworzy ponumerowaną listę początkowych aksjomatów,
% taką jaka wykorzystywana jest przez predykat prove,
% dodaje aksjomaty do dowodu
makeAxiomList([],[], _, []) :- !.
makeAxiomList([Clause|Clauses], [Axiom|AL], Num, [(Clause, axiom)|T]) :-
    axiomToList(Clause, R),
    list_to_ord_set(R, A),
    Axiom=[A, axiom, Num],
    Number is Num+1,
    makeAxiomList(Clauses, AL, Number, T).

makeAxiomList(Clauses, AL, Proof) :-
    makeAxiomList(Clauses, AL, 1, Proof), !.

% Używany w dowodzie sprzeczności predykat znajdujący rezolwentę
resolveClauses(Var, Clause1, Clause2, Res) :-
    member(Var, Clause1),
    member(~Var, Clause2),
    subtract(Clause1, [Var], C1),
    subtract(Clause2, [~Var], C2),
    union(C1, C2, Res),
    not(containsTautology(Res)).

containsTautology(Clause) :-
    member(Var, Clause),
    member(~Var, Clause), !.
 
% Usuwa z listy klauzul te, które zawierają parę p v ~p
removeTautologyClauses([],[]) :- !.        
removeTautologyClauses([[Clause,_,_]|T], NewClausesList) :-
    containsTautology(Clause),
    removeTautologyClauses(T, NewClausesList), !.
    
removeTautologyClauses([Clause|T], [Clause|T1]) :-
    not(containsTautology(Clause)),
    removeTautologyClauses(T, T1), !.
    
% contains(A, B) sprawdza czy zbiór B zawiera zbiór A
contains([], A) :- not(A==[]).
contains([H|Clause1], Clause2) :-
    member(H, Clause2),
    subtract(Clause2, [H], NewClause),
    contains(Clause1, NewClause), !.

isSuperset([Clause,_,_], [[Clause2,_,_]|_]) :-
    contains(Clause2, Clause).
isSuperset(Clause, [_|T]) :- isSuperset(Clause, T).

removeSupersets([], A, A) :-!.
removeSupersets([H|T], ClausesList, NClausesList) :-
    isSuperset(H, ClausesList),
    subtract(ClausesList, [H], NewClausesList),
    removeSupersets(T, NewClausesList, NClausesList), !.
removeSupersets([_|T], ClausesList, NClausesList) :-
    removeSupersets(T, ClausesList, NClausesList), !.

% Usuwa te klauzule, które są nadzbiorami Clause
removeSupersetsOf(_, [], A, A) :- !.
removeSupersetsOf(Clause, [H|T], ClausesList, NewClausesList) :-
    H=[Clause2,_,_],
    contains(Clause, Clause2),
    subtract(ClausesList, [H], NClausesList),
    removeSupersetsOf(Clause, T, NClausesList, NewClausesList), !.
removeSupersetsOf(Clause, [_|T], ClausesList, NewClausesList) :-
    removeSupersetsOf(Clause, T, ClausesList, NewClausesList), !.
    
% Sprawdza czy dwie klauzule są takie same
same([], []) :- !.
same([H|Clause1], Clause2) :-
    member(H, Clause2),
    subtract(Clause2, [H], NewClause),
    same(Clause1, NewClause), !.

% Sprawdza czy klauzula istnieje w zbiorze klauzul
memberOfClauses([[Clause1,_,_]|_], Clause) :- same(Clause1, Clause), !.
memberOfClauses([_|T], Clause) :- memberOfClauses(T, Clause).

% Dwa poniższe predykaty szukają nowej rezolwenty pewnych dwóch klauzul,
% nowa rezolwenta nie jest nadzbiorem żadnej poprzedniej klauzuli
% oraz nie występuje nigdzie wcześniej w zbiorze
resolventClause([Clause,_,N], [[Clause2,_,N2]|_], ClausesList, ResClause, Num) :-
    member(Var, Clause),
    atom(Var),
    member(~Var, Clause2),
    resolveClauses(Var, Clause, Clause2, Res),
    not(memberOfClauses(ClausesList, Res)),
    ResClause=[Res, (Var, N, N2), Num],
    not(isSuperset(ResClause, ClausesList)), !.
resolventClause([Clause,_,N], [[Clause2,_,N2]|_], ClausesList, ResClause, Num) :-
    member(~Var, Clause),
    atom(Var),
    member(Var, Clause2),
    resolveClauses(Var, Clause2, Clause, Res),
    not(memberOfClauses(ClausesList, Res)),
    ResClause=[Res, (Var, N2, N), Num],
    not(isSuperset(ResClause, ClausesList)), !.
resolventClause(Clause, [_|T], ClausesList, ResClause, Num) :-
    resolventClause(Clause, T, ClausesList, ResClause, Num).

newResolventClauseFromList([H|_], ClausesList, ResClause, Num) :-
    resolventClause(H, ClausesList, ClausesList, ResClause, Num).
newResolventClauseFromList([_|T], ClausesList, ResClause, Num) :-
    newResolventClauseFromList(T, ClausesList, ResClause, Num).

% Sprawdza czy zbiór klauzul jest sprzeczny 
contradictory(ClausesList, P) :-
    member([[Var],_,N1], ClausesList),
    atom(Var),
    member([[~Var],_,N2], ClausesList),
    P=[([], (Var, N1, N2))], !.

listToClause([H|T], H v Clause) :-
    zmienna(H),
    listToClause(T, Clause), !.
listToClause([H], H).
 
prove(Clauses, Proof) :-
    makeAxiomList(Clauses, A, ProofAcc),
    removeTautologyClauses(A, NCL),
    removeSupersets(NCL, NCL, NewClauses),
    length(A, Len),
    Num is Len+1,
    genProof(NewClauses, Proof, ProofAcc, Num).

% Główny predykat wyprowdzający dowód, w każdym kroku sprawdza,
% czy zbiór zawiera sprzeczność. Jeśli nie to tworzona jest nowa
% lista z rezolwentą, usuwane są nadzbiory.
genProof(ClausesList, Proof, ProofAcc, Num) :-
    (contradictory(ClausesList, P) ->
     append(ProofAcc, P, Proof),
     ! ;
     newResolventClauseFromList(ClausesList, ClausesList, ResClause, Num),
     append(ClausesList, [ResClause], NewClausesList),
     ResClause=[R, A, _],
     listToClause(R, Res),
     append(ProofAcc, [(Res, A)], ProofAcc1),
     removeSupersetsOf(R, NewClausesList, NewClausesList, NCL),
     NewNum is Num+1,
     genProof(NCL, Proof, ProofAcc1, NewNum)), !.
