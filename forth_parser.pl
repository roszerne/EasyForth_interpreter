:- module(forth_parser, [forth_program/3, forth_input/3]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

forth_input(I) -->  optional(`TOP |-> `, {X=0}),sequence(number,white,L),{reverse(L,I)}.

forth_program(P) --> forth_program(P, eos).

forth_program([], End) --> End.
forth_program([S|Rest], End) --> statement(S), !, blanks, forth_program(Rest, End).

statement(statement(push(I))) --> integer(I).

statement(statement(Statement)) --> nonblanks(String), { basic_statement(String, Statement) }.

parse_arr(New,Final) --> nonblanks(X),{X == `;`,!,reverse(New,Final)}.
parse_arr(New,New2) --> nonblanks(X), {X == ` `, !}, parse_arr(New,New2).
parse_arr(New,New2) --> !,statement(X),{Tmp =[X|New]},whites,parse_arr(Tmp,New2).

parse_do(New,Final) --> nonblanks(X),{X == `loop`,!,reverse(New,Final)}.
parse_do(New,New2) --> nonblanks(X), {X == ` `, !}, parse_do(New,New2).
parse_do(New,New2) --> !,statement(X),{Tmp =[X|New]},whites,parse_do(Tmp,New2).

parse_until(New,Final) --> nonblanks(X),{X == `until`,!,reverse(New,Final)}.
parse_until(New,New2) --> nonblanks(X), {X == ` `, !}, parse_until(New,New2).
parse_until(New,New2) --> !,statement(X),{Tmp =[X|New]},whites,parse_until(Tmp,New2).

parse_else(New,Final) --> nonblanks(X),{X == `then`,fail}.
parse_else(New,Final) --> nonblanks(X),{X == `else`,!,reverse(New,Final)}.
parse_else(New,New2) --> nonblanks(X), {X == ` `, !}, parse_else(New,New2).
parse_else(New,New2) --> !,statement(X),{Tmp =[X|New]},whites,parse_else(Tmp,New2).

parse_then(New,Final) --> nonblanks(X),{X == `then`,!,reverse(New,Final)}.
parse_then(New,New2) --> nonblanks(X), {X == ` `, !}, parse_then(New,New2).
parse_then(New,New2) --> !,statement(X),{Tmp =[X|New]},whites,parse_then(Tmp,New2).

statement(statement(print_string(Str))) --> string_without("\"", RestOfLine), {RestOfLine == `.`,!},
nonblanks(T),whites,
string_without("\"", RestOfLine3),
nonblanks(End1),
{string_chars(Str,RestOfLine3)}.

statement(statement(define_const(Stala))) --> whites,nonblanks(X),{X == `constant`},whites,!,nonblanks(T),
{atom_codes(Stala,T)}.

statement(statement(define_var(Name))) --> whites,nonblanks(X),{X == `variable`},whites,!,nonblanks(T),
{atom_codes(Name,T)}.

% `: name 1 2 3;` -> define_fun(name, [statement(push(1)), statement(push(2)), statement(push(3))])
statement(statement(define_fun(Name,Arr))) --> nonblanks(S),whites,{S == `:`},!,nonblanks(Y),{atom_codes(Name,Y)},whites,parse_arr([],Arr),
nonblanks(End1).

% `if 1 else 2 then` -> statement(ifelse([statement(push(1))], [statement(push(2))]))
statement(statement(ifelse(Arr, Arr2))) --> nonblanks(X),{X == `if`},whites,parse_else([],Arr),!,
whites,parse_then([],Arr2),nonblanks(End1).

% `if 1 then` -> statement(if([statement(push(1))]))
statement(statement(if(Arr))) --> nonblanks(X), {X == `if`}, !,whites,parse_then([],Arr),nonblanks(End1).

% `do 1 2 loop` -> statement(loop([statement(push(1)), statement(push(2))]))
statement(statement(loop(Arr))) --> nonblanks(X), {X == `do`},whites,parse_do([],Arr),nonblanks(End1).

% `begin 1 2 until` -> statement(until([statement(push(1)), statement(push(2))]))
statement(statement(until(Arr))) --> nonblanks(X), {X == `begin`},whites,parse_until([],Arr),nonblanks(End1).

% the catch-them-all rule,
% everything unparsed is considered to be a call to the environment
statement(statement(call_env(Name))) --> nonblanks(N), { length(N, L), L > 0, atom_codes(Name, N) }.

arithmetic_operator(OP,POP) :- member(OP,[ `+`,`-`,`*`,`/`,`mod`]), atom_codes(POP,OP).
comparison_operator(OP,POP) :- member(OP,[`<`, `>`, `=`, `>=`, `<=`]), atom_codes(POP,OP).
builtin_operator(OP,POP) :- member(OP,[`and`, `or`, `invert`, `dup`, `drop`, `swap`, `over`, `rot`, `emit`, `key`, `i`,`cr`]), atom_codes(POP,OP).
modify_operator(OP,POP) :- member(OP,[`+!`, `-!`, `*!`, `/!`, `mod!`]),reverse(OP,OP2),OP2 = [H|T],reverse(T,T2) ,atom_codes(POP,T2).

basic_statement(Codes, Statement) :- `@` == Codes,!, Statement = deref.
basic_statement(Codes, Statement) :- `.` == Codes,!,Statement = print.
basic_statement(Codes, Statement) :- `?` == Codes,!,Statement = print_var.
basic_statement(Codes, Statement) :- `!` == Codes,!,Statement = set. 
basic_statement(Codes, Statement) :- arithmetic_operator(Codes,POP),!, Statement = arithmetic(POP).
basic_statement(Codes, Statement) :- comparison_operator(Codes,POP),!, Statement = comparison(POP).
basic_statement(Codes, Statement) :- builtin_operator(Codes,POP),!, Statement = POP.
basic_statement(Codes, Statement) :- modify_operator(Codes,POP),!,Statement = modify_var(POP).
