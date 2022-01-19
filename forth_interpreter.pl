:- module(forth_interpreter, [eval_program/3]).
:- use_module(forth_state).

/**
 * eval_program(+Program:list, +InitialState:term, -FinalState:term) is det
 * 
 * wykonuje wyrażenia z zadanej listy, startując w zadanym stanie interpretera
 * 
 * @arg Program lista wyrażeń forth
 * @arg InitialState początkowy stan intepretera
 * @arg FinalState stan interpretera po wykonaniu programu
 * 
 */         
eval_program(Program, InitialState, FinalState) :- foldl(state_change, Program, InitialState, FinalState).

state_change(Statement, State, NextState) :-
    % once jest po to, żeby uniknąć przypadkowego uruchomienia 'catch-all' (ostatnie eval_statement)
    once(eval_statement(Statement, State, NextState)).

eval_statement(statement(push(Number)), State, NextState) :-
    state_stack_push(State, Number, NextState).

eval_statement(statement(arithmetic(OP)), State, NextState) :- 
state_stack_pop(State,N2,N1,State2), 
Arit =.. [OP,N1,N2], 
New is Arit, 
state_stack_push(State2,New,NextState).

% Comparisons
eval_statement(statement(comparison(OP)), State, NextState) :- 
state_stack_pop(State,N1,N2,State2),
Arit =.. [OP,N1,N2],
call(Arit),!,
state_stack_push(State2,-1,NextState).

eval_statement(statement(comparison(OP)), State, NextState) :- 
state_stack_pop(State,N1,N2,State2),
Arit =.. [OP,N1,N2],
not(call(Arit)),
state_stack_push(State2,0,NextState).


% Logic
eval_statement(statement(and), State, NextState) :-
state_stack_pop(State,B1,B2,State2),
B1 \= 0,B2 \= 0,!, 
state_stack_push(State2,-1,NextState).

eval_statement(statement(and), State, NextState) :-
state_stack_pop(State,B1,B2,State2),
state_stack_push(State2,0,NextState).


eval_statement(statement(or), State, NextState) :-
state_stack_pop(State,B1,B2,State2),
B1 == 0,B2 == 0,!, 
state_stack_push(State2,0,NextState).

eval_statement(statement(or), State, NextState) :- 
state_stack_pop(State,B1,B2,State2),
state_stack_push(State2,-1,NextState).


eval_statement(statement(invert), State, NextState) :-
state_stack_pop(State,B1,State2),
B1 == 0,!,
state_stack_push(State2,-1,NextState).

eval_statement(statement(invert), State, NextState) :-
state_stack_pop(State,B1,State2),
not(B1 == 0),
state_stack_push(State2,0,NextState).


% Stack manipulation
eval_statement(statement(dup), State, NextState) :-
state_stack_pop(State,B1,State2),
state_stack_push(State2,B1,B1,NextState).


eval_statement(statement(drop), State, NextState) :-
state_stack_pop(State,B1,NextState).



eval_statement(statement(swap), State, NextState) :-
state_stack_pop(State,B1,B2,State2),
state_stack_push(State2,B2,B1,NextState).


eval_statement(statement(over), State, NextState) :-
state_stack_pop(State,B1,B2,State2),state_stack_push(State2,B2,B1,B2,NextState).



eval_statement(statement(rot), State, NextState) :-
state_stack_pop(State,N3,N2,N1,State2),
state_stack_push(State2,N2,State3),
state_stack_push(State3,N3,State4),
state_stack_push(State4,N1,NextState).



% Output
eval_statement(statement(print), State, NextState) :-
state_stack_pop(State,A,NextState),
write(A).


eval_statement(statement(emit), State, NextState) :-
state_stack_pop(State,A,NextState),
char_code(New,A),
write(New).


eval_statement(statement(cr), State, State) :-
    nl.

eval_statement(statement(print_string(String)), State, State) :-
    write(String).

% Input
eval_statement(statement(key), State, NextState) :-
    get_single_char(A),
    state_stack_push(State,A,NextState).
    
% Env Dependent Statements
eval_statement(statement(define_fun(Name, Fun)), State, NextState) :-
    state_env_set(State, Name, fun(Fun), NextState).

eval_statement(statement(define_var(Name)), State, NextState) :-
    state_mem_alloc(State,Mem,State2),
    state_env_set(State2, Name, Mem, NextState).
   
eval_statement(statement(define_const(Name)), State, NextState) :-
    state_stack_pop(State,A,State2),
    state_env_set(State2, Name, A, NextState).

eval_statement(statement(call_env(Name)), State, NextState) :-
    state_env_get(State,Name,Val),
    Val = fun(A),!,
    eval_program(A,State,NextState).

eval_statement(statement(call_env(Name)), State, NextState) :-
    state_env_get(State,Name,Val),
    state_stack_push(State,Val,NextState).

% Address handling
eval_statement(statement(deref), State, NextState) :-
    state_stack_pop(State,Addr,State2),
    state_mem_get(State2,Addr,Val),
    state_stack_push(State2,Val,NextState).

eval_statement(statement(set), State, NextState) :-
    state_stack_pop(State,Addr,State2),
    state_stack_pop(State2,Val,State3),
    state_mem_set(State3,Addr,Val,NextState).

eval_statement(statement(modify_var(OP)), State, NextState) :-
    eval_program([statement(swap),statement(over),statement(deref),
    statement(arithmetic(OP)),statement(swap),statement(set)],State,NextState).
	
eval_statement(statement(print_var), State, NextState) :-
    eval_program([statement(deref),statement(print)],State,NextState).
	
% Conditionals
eval_statement(statement(ifelse(Then, Else)), State, NextState) :-
    state_stack_pop(State,A,State2),
    A == 0,!,
    eval_program(Else,State2,NextState).

eval_statement(statement(ifelse(Then, Else)), State, NextState) :-
    state_stack_pop(State,A,State2),
    eval_program(Then,State2,NextState). 
       

eval_statement(statement(if(Then)), State, NextState) :-
    state_stack_pop(State,A,State2),
    not(A == 0),!,
    eval_program(Then,State2,NextState).

eval_statement(statement(if(Then)), State, NextState) :-
    state_stack_pop(State,A,NextState).   

% Loops    
eval_statement(statement(until(Body)), State, NextState) :- 
    eval_program(Body,State,State2),
    state_stack_pop(State2,Val,State3),
    Val == 0, !,
    eval_statement(statement(until(Body)), State3, NextState).

eval_statement(statement(until(Body)), State, State3) :- 
    eval_program(Body,State,State2),
    state_stack_pop(State2,Val,State3),
    not(Val == 0).
    
eval_statement(statement(loop(Body)), State, NextState) :-
    state_stack_pop(State,Val2,Val,State2),
    Val2 < Val,!,
    state_env_set(State2,i,Val2,State3),
    eval_loop(Val2, Val, Body, State3,  NextState).

eval_statement(statement(loop(Body)), State, NextState) :-
    state_stack_pop(State,Val2,Val, NextState),
    Val2 >= Val,!.

eval_statement(statement(i), State, NextState) :-
    state_env_get(State,i,Val),
    state_stack_push(State,Val,NextState).
    

% to wyrazenie wypisuje aktualny stan interpretera,
% przydatne do debugowania
eval_statement(statement(debug), State, State) :-
    state_debug(State).

% ma pomóc znalezc bledy
% poprzez wykrycie nieobsluzonych wyrazen 
eval_statement(Statement, State, State) :-
    format('WARNING: an unrecognized statement has been ignored: ~w\n', [Statement]),
    state_debug(State).

eval_loop(Value, To, Body, State, FinalState) :- 
eval_program(Body,State,State2),
Value2 is Value + 1,
To > Value2,!,
state_env_set(State2,i,Value2,State3),
eval_loop(Value2,To,Body,State3,FinalState).

eval_loop(Value, To, Body, State, FinalState) :- 
eval_program(Body,State,State2),
Value2 is Value + 1,
To =< Value2,!,
state_env_del(State2,i,FinalState).
