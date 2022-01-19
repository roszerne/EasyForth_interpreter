% To jest podstawa sładnia definicji własnego modułu
% nazwa modułu i lista esportowanych predykatów
:- module(forth_state, [state_empty/1, state_debug/1,
                        state_stack_pop/3, state_stack_pop/4, state_stack_pop/5,
                        state_stack_push/3, state_stack_push/4, state_stack_push/5,
                        state_env_get/3, state_env_set/4, state_env_del/3,
                        state_mem_get/3, state_mem_alloc/3, state_mem_set/4, state_mem_del/3]).

:- use_module(library(assoc)).

/**
 * state_empty(-State:term) is det
 * 
 * zwraca pusty zainicjalizowany stan interpretera
 * stan składa się ze stosu, środowiska i pamięci
 * - stos to pamięć lokalna używana to przekazywania argumentów
 * - środowiska zawiera definicje zmiennych, stałych i funkcji
 * - pamięć zawiera wartości zmiennych
 *
 * @arg State pusty stan interpretera
 */
state_empty(state([], Env, Mem)) :-
    env_empty(Env),
    mem_empty(Mem).

/**
 * state_debug(+State:term) is det
 * 
 * wypisuje szczegóły przekazanego stanu
 * 
 * @arg State stan interpreta
 */
state_debug(state(Stack, Env, Mem)) :-
    nl, 
    writeln('==========='),
    writeln('# Stack:'),
    format("~w <-| TOP\n", [Stack]),
    writeln('# Environment:'),
    env_debug(Env),
    writeln('# Memory:'),
    mem_debug(Mem),
    writeln('===========').

/**
 * state_stack_push(+State:term, +Item:int, -NextState:term) is det
 * 
 * dodaje liczbę na wierzch stosu w zadanym stanie
 * 
 * @arg State początkowy stan
 * @arg Item liczba do wrzucenia na stos
 * @arg NextState nowy stan z liczbą na stosie
 */
state_stack_push(state(Stack, Env, Mem), Item, state([Item|Stack], Env, Mem)). 
/**
 * state_stack_push(+State:term, +I1:int, +I2:int -NextState:term) is det
 * 
 * dodaje dwie liczby na wierzch stosu w zadanym stanie
 * 
 * @arg State początkowy stan
 * @arg I1 pierwsza liczba do wrzucenia na stos
 * @arg I2 druga liczba do wrzucenia na stos
 * @arg NextState nowy stan z zadanymi liczbami na stosie
 */
state_stack_push(state(Stack, Env, Mem), I1, I2, state([I1,I2|Stack], Env, Mem)). 
/**
 * state_stack_push(+State:term, +I1:int, +I2:int, +I3:int, -NextState:term) is det
 * 
 * dodaje trzy liczby na wierzch stosu w zadanym stanie
 * 
 * @arg State początkowy stan
 * @arg I1 pierwsza liczba do wrzucenia na stos
 * @arg I2 druga liczba do wrzucenia na stos
 * @arg I3 trzecia liczba do wrzucenia na stos
 * @arg NextState nowy stan z zadanymi liczbami na stosie
 */
state_stack_push(state(Stack, Env, Mem), I1, I2, I3, state([I1,I2,I3|Stack], Env, Mem)). 

/**
 * state_stack_pop(+State:term, -Item:int, -NextState:term) is det
 * 
 * zdejmuje liczbę ze stosu w zadanym stanie
 * 
 * @arg State początkowy stan
 * @arg Item liczba zdjęta ze stosu
 * @arg NextState nowy stan pozbawiony liczby z wierzchu stosu
 */
state_stack_pop(state([Item|Stack], Env, Mem), Item, state(Stack, Env, Mem)).
/**
 * state_stack_pop(+State:term, -I1:int, -I2:int, -NextState:term) is det
 * 
 * zdejmuje dwie liczby ze stosu w zadanym stanie
 * 
 * @arg State początkowy stan
 * @arg I1 liczba zdjęta z wierzchu stosu
 * @arg I2 liczba znajdująca się pod wierzchołkiem stosu
 * @arg NextState nowy stan pozbawiony liczb z wierzchu stosu
 */
state_stack_pop(state([I1,I2|Stack], Env, Mem), I1, I2, state(Stack, Env, Mem)).
/**
 * state_stack_pop(+State:term, -I1:int, -I2:int, -3:int. -NextState:term) is det
 * 
 * zdejmuje trzy liczby ze stosu w zadanym stanie
 * 
 * @arg State początkowy stan
 * @arg I1 liczba zdjęta z wierzchu stosu
 * @arg I2 liczba znajdująca się pod wierzchołkiem stosu
 * @arg I3 liczba znajdująca się dwa miejsca pod wierzchołkiem stosu
 * @arg NextState nowy stan pozbawiony liczb z wierzchu stosu
 */
state_stack_pop(state([I1,I2,I3|Stack], Env, Mem), I1, I2, I3, state(Stack, Env, Mem)).

/**
 * state_env_get(+State:term, +Name:atom, -Value:term) is det
 * 
 * pobiera ze środowiska wartość o zadanej nazwie
 * 
 * @arg State stan zawierający środowisko
 * @arg Name nazwa wartości do pobrania
 * @arg Value wartość przypisywana do nazwy w środowisku
 */
state_env_get(state(_, Env, _), Name, Value) :-
    env_get(Env, Name, Value).
/**
 * state_env_set(+State:term, +Name:atom, +Value:term, -NextState:term) is det
 * 
 * przypisuje w środowisku zadaną wartość do podanego klucza
 * 
 * @arg State początkowy stan zawierający środowisko
 * @arg Name nazwa klucza, do którego przypisana będzie wartość
 * @arg Value wartość, która ma być przypisana
 * @arg NextState stan zawierający zaktualizowane środowisko
 */
state_env_set(state(Stack, Env, Mem), Name, Value, state(Stack, NewEnv, Mem)) :-
    env_set(Env, Name, Value, NewEnv).
/**
 * state_env_del(+State:term, +Name:atom, -NextState:term) is det
 * 
 * usuwa ze środowiska wartość o zadanym kluczu
 * 
 * @arg State początkowy stan zawierający środowisko
 * @arg Name nazwa klucza do usunięcia
 * @arg NextState stan zawierający zaktualizowane środowisko
 */
state_env_del(state(Stack, Env, Mem), Name, state(Stack, NewEnv, Mem)) :-
    env_del(Env, Name, NewEnv).

/**
 * state_mem_get(+State:term, +Address:int, -Value:int) is det
 * 
 * pobiera z pamięci wartość o zadanym adresie
 * 
 * @arg State stan zawierający pamięć
 * @arg Address adres komórki pamięci
 * @arg Value wartość pod zadanym adresie w pamięci
 */
state_mem_get(state(_, _, Mem), Address, Value) :-
    mem_get(Mem, Address, Value).
/**
 * state_mem_alloc(+State:term, -Address:int, -NextState:term) is det
 * 
 * alokuje komórkę w pamięci i zwraca jej adres
 * 
 * @arg State stan zawierający pamięć
 * @arg Address adres zaalokowanej komórki pamięci
 * @arg NextState stan zawierający zaktualizowaną pamięć
 */
state_mem_alloc(state(Stack, Env, Mem), MemAddr, state(Stack, Env, NewMem)) :-
    mem_alloc(Mem, MemAddr, NewMem).
/**
 * state_mem_set(+State:term, +Address:int, +Value:int, -NextState:term) is det
 * 
 * przypisuje w pamięci zadaną wartość do zadanego adresu
 * 
 * @arg State początkowy stan zawierający środowisko
 * @arg Address adres komórki, do której zapisana będzie wartość
 * @arg Value wartość, która ma być zapisana
 * @arg NextState stan zawierający zaktualizowane środowisko
 */
state_mem_set(state(Stack, Env, Mem), Address, Value, state(Stack, Env, NewMem)) :-
    mem_set(Mem, Address, Value, NewMem).
/**
 * state_mem_del(+State:term, +Address:int, -NextState:term) is det
 * 
 * usuwa z pamięci komórkę o zadanym adresie
 * 
 * @arg State początkowy stan zawierający pamięc
 * @arg Address adres komórki do usunięcia
 * @arg NextState stan zawierający zaktualizowane środowisko
 */
state_mem_del(state(Stack, Env, Mem), Address, stack(Stack, Env, NewMem)) :-
    mem_del(Mem, Address, NewMem).

env_empty(Env) :- empty_assoc(Env).
env_set(Env, Name, Value, NewEnv) :- put_assoc(Name, Env, Value, NewEnv).
env_get(Env, Name, Value) :- get_assoc(Name, Env, Value).
env_del(Env, Name, NewEnv) :- del_assoc(Name, Env, Value, NewEnv).

env_debug(Env) :-
    writeln('-- key <- value'),
    forall(gen_assoc(Key, Env, Value),
        format('~w <- ~w\n',[Key, Value])
    ).

mem_empty(EmptyMemory) :- empty_assoc(Mem),EmptyMemory = memory(Mem,0).

mem_alloc(Memory, Address, UpdatedMemory) :- 
Memory = memory(Mem,Address),
put_assoc(Address, Mem, 0, Mem2), 
NewAddr = Address + 1, 
UpdatedMemory = memory(Mem2,NewAddr).

mem_set(Memory, Address, Value, UpdatedMemory) :- 
Memory = memory(Mem,OldAddr),
OldAddr == Address,!,
put_assoc(Address, Mem, Value, Mem2),
newAddr is Address + 1,
UpdatedMemory = memory(Mem2,newAddr).

mem_set(Memory, Address, Value, UpdatedMemory) :- 
Memory = memory(Mem,OldAddr),
OldAddr > Address,
Val = Value,
put_assoc(Address, Mem, Val, Mem2),
UpdatedMemory = memory(Mem2,OldAddr).

mem_get(Memory, Address, Value) :- 
Memory = memory(Mem,OldAddr),
get_assoc(Address, Mem, Val),
Value = Val.

mem_del(Memory, Address, UpdatedMemory) :- 
Memory = memory(Mem,Addr),
del_assoc(Address,Mem,Value,Mem2),
UpdatedMemory = memory(Mem2,Addr).

mem_debug(memory(Mem, FreeAddr)) :-
    writeln('-- address <- value'),
    forall(gen_assoc(Addr, Mem, Value),
        format('~w <- ~w\n',[Addr, Value])
    ),
    format('-- free address: ~w\n', [FreeAddr]).
