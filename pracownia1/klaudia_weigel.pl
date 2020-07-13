/* Zadanie 1, Metody Programowania.
 * Program generuje wszystkie wartościowania spełniające dany 
 * zbiór klauzul. Głowny predykat programu to solve(+Expr, -Val), 
 * gdzie Expr to dany zbiór klauzul, a Val, generowane rozwiązanie. 
 * 
 * Autor: Klaudia Weigel */


:-module(klaudia_weigel, [solve/2]).

:- op(200, fx, ~).
:- op(500, xfy, v).

bool_not(t,f).
bool_not(f,t).
bool_and(f,f,f).
bool_and(f,t,f).  
bool_and(t,f,f).      
bool_and(t,t,t).        
bool_or(f,f,f).      
bool_or(f,t,t). 
bool_or(t,f,t).     
bool_or(t,t,t).

% generuje listę wszystkich zmiennych.

literaly([], []).
literaly([K|R], Out) :- 
    atom(K), !, Out = [K|O1], 
    literaly(R, O1).
        
literaly([~K|R], Out) :- 
    atom(K), !, Out = [K|O1], 
    literaly(R, O1).
        
literaly([A v B|R], Out) :- 
    literaly([A], R1), 
    literaly([B], R2), 
    literaly(R, O1), 
    append(R1,R2,C), 
    append(C, O1, Out). 
    
lista(K, O) :- 
    literaly(K, O1), 
    sort(O1, O).
 
% pierwsze wrtościowanie ustawia wszystkie zmienne na f. 
pierwsze_wart([], []).
pierwsze_wart([X|R], [(X,f)|S]) :- pierwsze_wart(R, S).
 
% generuje kolejne wartościowanie, na zasadzie otrzymywania kolejnych
% liczb binarnych.
nastepne_wart(A,S) :- reverse(A,R), nast(R,N), reverse(N,S).

nast([(X,f)|R],[(X,t)|R]).
nast([(X,t)|R],[(X,f)|S]) :- nast(R,S).

% truth_value(+Expr, +Variable_List, +Assign_List, -Truth_Val).

% sprawdza jaką wartość logiczną ma dane wartościowanie

wartosc_logiczna([],_,_,t).
wartosc_logiczna(N,_,_,N) :- member(N, [(X,t), (X,f)]).

wartosc_logiczna([H|T], Vars, Assigned_Vars, Val) :-
    wartosc_logiczna(H, Vars, Assigned_Vars, VH),
    wartosc_logiczna(T, Vars, Assigned_Vars, VT),
    bool_and(VH, VT, Val), !.

wartosc_logiczna(X, Vars, Assigned_Vars, Val) :-
    atom(X),
    sprawdz_wartosc(X, Vars, Assigned_Vars, Val).

wartosc_logiczna(X v Y, Vars, Assigned_Vars, Val) :-
    wartosc_logiczna(X, Vars, Assigned_Vars, VX),
    wartosc_logiczna(Y, Vars, Assigned_Vars, VY),
    bool_or(VX, VY, Val).

wartosc_logiczna(~X, Vars, Assigned_Vars, Val) :-
    wartosc_logiczna(X, Vars, Assigned_Vars, VX),
    bool_not(VX, Val).

% wartość logiczna zmiennej
sprawdz_wartosc(X,[X|_],[(X,V)|_],V).
sprawdz_wartosc(X,[_|Vars],[_|A],V) :- sprawdz_wartosc(X,Vars,A,V).

solve(Clauses, Solution) :-
    lista(Clauses, Lit),
    pierwsze_wart(Lit, Var),
    wartosc_logiczna(Clauses, Lit, Var, Val),
    solve(Clauses, Lit, Var, VarAcc, Val),
    Solution=VarAcc.
 
solve(_,_,S,S,t).
solve(Clauses, Lit, Var, VarAcc, _) :-
    nastepne_wart(Var, S),
    wartosc_logiczna(Clauses, Lit, S, Val1),
    solve(Clauses, Lit, S, VarAcc, Val1).
       