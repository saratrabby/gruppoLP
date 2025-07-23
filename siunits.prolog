%%%% 899988 Alari Matteo
%%%% 914295 Trabattoni Sara

% Unità base SI
si_base_unit(m).     % metro
si_base_unit(kg).    % chilogrammo
si_base_unit(s).     % secondo
si_base_unit('A').   % ampere
si_base_unit('K').   % kelvin
si_base_unit(mol).   % mole
si_base_unit(cd).    % candela


% Riconoscimento unità SI (base o derivate)
is_si_unit(U) :-
    si_base_unit(U).
is_si_unit(U) :-
    si_derived_unit(U, _).
is_si_unit(U1 * U2) :-
    is_si_unit(U1),
    is_si_unit(U2).
is_si_unit(U ** E) :-
    is_si_unit(U),
    integer(E).

% Prefissi SI (nome, simbolo, fattore)
si_prefix(chilo, k, 1e3).
si_prefix(etto, h, 1e2).
si_prefix(deca, da, 1e1).
si_prefix(deci, d, 1e-1).
si_prefix(centi, c, 1e-2).
si_prefix(milli, m, 1e-3).
si_prefix(micro, 'μ', 1e-6).
si_prefix(nano, n, 1e-9).
si_prefix(pico, p, 1e-12).

% Unità base SI
si_unit_symbol(metro, m).
si_unit_symbol(chilogrammo, kg).
si_unit_symbol(secondo, s).
si_unit_symbol(ampere, 'A').
si_unit_symbol(kelvin, 'K').
si_unit_symbol(mole, mol).
si_unit_symbol(candela, cd).

% Unità con prefisso: si_unit_symbol(NomeUnità, Simbolo)
si_unit_symbol(NomePrefisso-NomeUnita, SimboloPrefissoSimboloUnita) :-
    si_prefix(NomePrefisso, SimboloPrefisso, _),
    si_unit_symbol(NomeUnita, SimboloUnita),
    atom_concat(SimboloPrefisso, SimboloUnita, SimboloPrefissoSimboloUnita).

% Operazione inversa a quella precedente
si_unit_name(S, N) :-
    si_unit_symbol(N, S).
si_unit_name(S, Prefisso-NomeUnita) :-
    si_prefix(Prefisso, SimboloPrefisso, _),
    si_unit_symbol(NomeUnita, SimboloUnita),
    atom_concat(SimboloPrefisso, SimboloUnita, S).

% Result è < se U1 < U2, > se U1 > U2, = se U1 = U2 (in termini di grandezza)
compare_units(Result, U1, U2) :-
    unit_factor(U1, F1, Base1),
    unit_factor(U2, F2, Base2),
    Base1 = Base2, % devono essere la stessa unità base
    ( F1 < F2 -> Result = '<'
    ; F1 > F2 -> Result = '>'
    ; F1 =:= F2 -> Result = '='
    ).

% Calcola il fattore numerico associato all'unità (considerando il prefisso)
unit_factor(U, 1, U) :-
    si_unit_symbol(_, U). % unità senza prefisso
unit_factor(Prefisso-Nome, Fattore, Base) :-
    si_prefix(Prefisso, _, F),
    si_unit_symbol(Nome, Base),
    Fattore = F.
unit_factor(U, F, Base) :-
    atom(U),
    atom_chars(U, [First|_]),
    si_prefix(Prefisso, SimboloPrefisso, F),
    atom_chars(SimboloPrefisso, [First|_]),
    si_unit_symbol(Nome, Base),
    atom_concat(SimboloPrefisso, Base, U).

% Espansione canonica in unità base
si_unit_base_expansion(U, U) :-
    si_base_unit(U).
si_unit_base_expansion(U, Base) :-
    si_derived_unit(U, D),
    norm(D, Base).

% Dimensione valida
is_dimension(Dim) :-
    is_si_unit(Dim).

% Quantità valida
is_quantity(q(Value, Dim)) :-
    number(Value),
    is_dimension(Dim).

% Conversione dimensione in lista [(unità, esponente)]
dim_to_list(U ** E, [(U, E)]) :- !.
dim_to_list(U, [(U, 1)]) :-
    si_base_unit(U), !.
dim_to_list(U, List) :-
    si_derived_unit(U, D),
    dim_to_list(D, List), !.
dim_to_list(U1 * U2, List) :-
    dim_to_list(U1, L1),
    dim_to_list(U2, L2),
    append(L1, L2, List).

% Somma esponenti per unità duplicate
merge_units(Units, Merged) :-
    merge_units_(Units, [], Merged).

merge_units_([], Acc, Acc).
merge_units_([(U, E)|T], Acc, Result) :-
    ( select((U, E0), Acc, Rest) ->
        E1 is E + E0,
        merge_units_(T, [(U, E1)|Rest], Result)
    ;
        merge_units_(T, [(U, E)|Acc], Result)
    ).

% Ricostruzione lista 
list_to_dim([], 1).
list_to_dim([(U, 1)], U) :- !.
list_to_dim([(U, E)], U ** E) :- !.
list_to_dim([(U, 1)|T], U * D) :-
    list_to_dim(T, D).
list_to_dim([(U, E)|T], U ** E * D) :-
    list_to_dim(T, D).


% Somma tra quantità (solo se dimensioni compatibili)
qadd(q(V1, D1), q(V2, D2), q(V3, D1)) :-
    norm(D1, N1),
    norm(D2, N2),
    N1 = N2,
    V3 is V1 + V2.

% Sottrazione tra quantità
qsub(q(V1, D1), q(V2, D2), q(V3, D1)) :-
    norm(D1, N1),
    norm(D2, N2),
    N1 = N2,
    V3 is V1 - V2.

% Moltiplicazione tra quantità
qmul(q(V1, D1), q(V2, D2), q(V3, D3)) :-
    V3 is V1 * V2,
    Dtemp = D1 * D2,
    norm(Dtemp, D3).

% Divisione tra quantità
qdiv(q(V1, D1), q(V2, D2), q(V3, D3)) :-
    V3 is V1 / V2,
    Dtemp = D1 * (D2 ** -1),
    norm(Dtemp, D3).

% Elevamento a potenza intera
qexp(q(V, D), N, q(VR, DR)) :-
    integer(N),
    VR is V ** N,
    expand_power(D, N, Dexp),
    norm(Dexp, DR).

% Espansione di potenza su espressioni
expand_power(U ** E, N, U ** E1) :-
    E1 is E * N.
expand_power(U1 * U2, N, D1 * D2) :-
    expand_power(U1, N, D1),
    expand_power(U2, N, D2).
expand_power(U, N, U ** N) :-
    atom(U).

