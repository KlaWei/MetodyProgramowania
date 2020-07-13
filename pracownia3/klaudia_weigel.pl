/* Metody Programowania, pracownia 3.
 * Częściowo poprawny parser języka HDML. */

:- module(klaudia_weigel, [parse/3]).

/* Z pliku while_parser.pl */
lexer(Tokens) -->
    white_space,
    (  (   "=",      !, { Token = tokAssgn }
        ;  ",",       !, { Token = tokColon }
        ;  "(",       !, { Token = tokLParen }
        ;  ")",       !, { Token = tokRParen }
        ;  "+",       !, { Token = tokPlus }
        ;  "-",       !, { Token = tokMinus }
        ;  "*",       !, { Token = tokTimes }
        ;  "=",       !, { Token = tokEq }
        ;  "<>",      !, { Token = tokNeq }
        ;  "<=",      !, { Token = tokLeq }
        ;  "<",       !, { Token = tokLt }
        ;  ">=",      !, { Token = tokGeq }
        ;  ">",       !, { Token = tokGt }
        ;  "/",       !, { Token = tokDiv }
        ;  "&",       !, { Token = tokAnd }
        ; "|",        !, { Token = tokOr }
        ; "~",        !, { Token = tokLambda }
        ; "%",        !, { Token = tokMod }
        ; "..",       !, { Token = tokDots }
        ; "#",        !, { Token = tokHash }
        ; "@",        !, { Token = tokMonkey }
        ; "^",        !, { Token = tokCat }
        ; "[",        !, { Token = tokSqBrLeft }
        ; "]",        !, { Token = tokSqBeRight }
        ; "_",        !, { Token = tokUnderscr }
        ;  digit(D),  !,
                number(D, N),
                { Token = tokNumber(N) }
        ;  letter(L), !, identifier(L, Id),
                {  member((Id, Token), [

                        (else, tokElse),
                        (if, tokIf),
                        (then, tokThen),
                        (def, tokDef),
                        (in, tokIn),
                        (let, tokLet)]),
                    !
                ;  Token = tokVar(Id)
                }
        ;  [_],
                { Token = tokUnknown }
        ),
        !,
            { Tokens = [Token | TokList] },
        lexer(TokList)
    ;  [],
            { Tokens = [] }
    ).


white_space -->
    [Char], { code_type(Char, space) }, !, white_space.
white_space -->
    [].
        
digit(D) -->
    [D], { code_type(D, digit) }.

digits([D|T]) -->
    digit(D),
    !,
    digits(T).
digits([]) -->
    [].

number(D, N) -->
    digits(Ds), { number_chars(N, [D|Ds]) }.

letter(L) -->
    [L], { code_type(L, alpha) }.

id_char(Char) -->
    { code_type(Char, alnum)}, [].
id_char(95) --> [].

alphanum([A|T]) -->
    [A], id_char(A), !, alphanum(T).
alphanum([]) -->
    [].

identifier(L, Id) -->
    alphanum(As),
        { atom_codes(Id, [L|As]) }.


program(Ast) -->
    definicja(Instr),
    (  program(Rest), !,
            { Ast = [Instr|Rest] }
    ;  [],
            { Ast = [Instr] }
    ).


definicja(def(Ide, Wzorzec, Wyrazenie)) --> 
    [tokDef],!, 
    identyfikator(Ide),
    [tokLParen], wzorzec(Wzorzec), [tokRParen], 
    [tokAssgn], wyrazenie(Wyrazenie).

wzorzec(wildcard(no)) --> 
    [tokUnderscr].
wzorzec(var(no, X)) --> 
    zmienna(X).
wzorzec(P) --> 
    [tokLParen], wzorzec(P), [tokRParen].
wzorzec(pair(no, P1, P2)) -->
    wzorzec(P1), !, [tokColon], wzorzec(P2).


wyrazenie(if(no, E1, E2, E3)) --> 
    [tokIf], !, wyrazenie(E1), 
    [tokThen], wyrazenie(E2), 
    [tokElse], wyrazenie(E3).
wyrazenie(let(no, P, E1, E2)) --> 
    [tokLet], !,  wzorzec(P), 
    [tokAssgn], wyrazenie(E1), 
    [tokIn], wyrazenie(E2).

wyrazenie(W) --> wyrazenie_first(W).
wyrazenie(W) --> comparison(W).

wyrazenie_first(W) --> 
    wyrazenie_op(T), rest_wyrazenie_first(T, W).

rest_wyrazenie_first(T, W) --> 
    third_op(Op), wyrazenie_op(T1), 
    rest_wyrazenie_first(op(no, Op, T, T1), W).
rest_wyrazenie_first(E, E) --> [].

wyrazenie_op(W) --> 
    term(T), rest_wyrazenie_op(T, W).

rest_wyrazenie_op(T, W) --> 
    add_op(Op), term(T1), 
    rest_wyrazenie_op(op(no, Op, T, T1), W).
rest_wyrazenie_op(E, E) --> [].
    
term(T) --> factor(P), rest_term(P, T).

rest_term(P, T) --> 
    mult_op(Op), factor(P1), 
    rest_term(op(no, Op, P, P1), T).
rest_term(T, T) --> [].

factor(op(no, Op, P)) --> 
    unary_op(Op), wyrazenie_proste(P).
factor(P) --> 
    wyrazenie_proste(P).
factor(P) --> [].


unary_exp(op(no, Op, E)) --> 
    unary_op(Op), wyrazenie_op(E).

wyrazenie_proste(P) --> 
    wyrazenie_atomowe(P).
wyrazenie_proste(P) --> 
    [tokLParen], wyrazenie(P), [tokRParen].
wyrazenie_proste(P) -->
    wybor_bitu(P).
wyrazenie_proste(P) --> 
    wybor_bitow(P).

wyrazenie_proste_rest(P) --> wyrazenie_atomowe(P).

wybor_bitu(bitsel(no, E1, E2)) -->
    wyrazenie_proste_rest(E1),! ,
    [tokSqBrLeft], wyrazenie(E2), 
    [tokSqBeRight].
    
wybor_bitow(bitsel(no, E1, E2, E3)) -->
    wyrazenie_proste_rest(E1), !, 
    [tokSqBrLeft], wyrazenie(E2), 
    [tokDots], wyrazenie(E3), 
    [tokSqBeRight].


wyrazenie_atomowe(call(no, Name, E)) --> 
    [tokVar(Name)], 
    [tokLParen], wyrazenie(E), [tokRParen].
wyrazenie_atomowe(var(no, X)) --> [tokVar(X)].
wyrazenie_atomowe(num(no, N)) --> [tokNumber(N)].
wyrazenie_atomowe(empty(no)) --> [tokSqBrLeft], [tokSqBeRight].
wyrazenie_atomowe(bit(no, E)) --> [tokSqBrLeft], wyrazenie(E), [tokSqBeRight].

comparison(op(no, Op, E1, E2)) --> 
    wyrazenie_op(E1), bool_op(Op), wyrazenie_op(E2).

unary_op('-') --> [tokMinus], !.
unary_op('~') --> [tokLambda], !.
unary_op('#') --> [tokHash], !.
bool_op('=') --> [tokAssgn], !.
bool_op('>') --> [tokGt].
bool_op('>=') --> [tokGeq], !.
bool_op('<=') --> [tokLeq], !.
bool_op('<') --> [tokLt], !.
bool_op('<>') --> [tokNeq], !.
third_op('@') --> [tokMonkey], !.
third_op(',') --> [tokColon], !.
add_op('+') --> [tokPlus], !.
add_op('-') --> [tokMinus], !.
add_op('^') --> [tokCat], !.
mult_op('%') --> [tokMod], !.
mult_op('&') --> [tokAnd], !.
mult_op('*') --> [tokTimes], !.
mult_op('/') --> [tokDiv], !.

zmienna(X) --> [tokVar(X)], !.
identyfikator(X) --> [tokVar(X)], !.

parse(Path, CharCodeList, Absynt) :-
    phrase(lexer(TokList), CharCodeList),
    phrase(program(Absynt), TokList).
