:- use_module(forth_state).
:- use_module(forth_interpreter).
:- use_module(forth_parser).
:- use_module(library(optparse)).

:- initialization(main, main).

parse_spec(Spec) :-
    Spec = [
        [opt(input), meta('FILE'), type(atom), shortflags([i]),  longflags(['input-file']), help('read input stack from file')]
    ].

parse_args(Args, Program, Input) :-
    parse_spec(OptSpec),
    (opt_parse(OptSpec, Args, Options, PositionalArgs) ->
        get_input(Options, Input),
        get_program(PositionalArgs, Program)
    ;
        opt_help(OptSpec, Help),
        writeln(Help),
        halt(-3)
    ).

get_input(Options, Input) :-
    member(input(InputPath), Options), 
    \+ var(InputPath), 
    !,
    read_input_from_file(InputPath, Input).

read_input_from_file(InputPath, Input) :-
    (phrase_from_file(forth_input(InputStack), InputPath) ->
        state_empty_with_stack(InputStack, Input)
    ;
        writeln("ERROR: failed to parse the input file"),
        halt(-2)
    ).
    
get_input(_, S) :- state_empty(S).

get_program([ProgramPath], Program) :- !,
    (read_program_from_file(ProgramPath, Program) ->
        true
    ;
        writeln("ERROR: failed to parse the forth program"),
        halt(-1)
    ).

read_program_from_file(ProgramPath, Program) :-
    phrase_from_file(forth_program(Program), ProgramPath).
    
get_program(_, _) :- 
    writeln("ERROR: there should be exactly single positional argument pointing at the program"),
    halt(-4).

main(Args) :-
    parse_args(Args, Program, Input),
    forth(Program, Input, Result),
    (
        state_stack_pop(Result, ExitCode, _),
        halt(ExitCode)
        ;
        halt  
    ).

forth(Program, InitialState, ResultState) :-
    eval_program(Program, InitialState, ResultState).