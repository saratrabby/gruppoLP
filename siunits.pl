%%%% -*- Mode: Prolog -*-
%%%% 899988 Alari Matteo
%%%% 914295 Trabattoni Sara
%%%% 909567 Caronni Andrea
    
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

% Unità derivate SI: si_derived_unit(Simbolo, Espressione)
si_derived_unit('N', kg * m / s ** 2).         % Newton
si_derived_unit('J', kg * m ** 2 / s ** 2).    % Joule
si_derived_unit('Pa', kg / (m * s ** 2)).      % Pascal
si_derived_unit('W', kg * m ** 2 / s ** 3).    % Watt
si_derived_unit('Hz', 1 / s).                  % Hertz
si_derived_unit('C', s * 'A').                 % Coulomb
si_derived_unit('V', kg * m ** 2 / (s ** 3 * 'A')). % Volt
si_derived_unit('F', s ** 4 * 'A' ** 2 / (kg * m ** 2)). % Farad
si_derived_unit('omega', kg * m ** 2 / (s ** 3 * 'A' ** 2)). % Ohm
si_derived_unit('S', s ** 3 * 'A' ** 2 / (kg * m ** 2)). % Siemens
si_derived_unit('Wb', kg * m ** 2 / (s ** 2 * 'A')). % Weber
si_derived_unit('T', kg / (s ** 2 * 'A')).     % Tesla
si_derived_unit('H', kg * m ** 2 / (s ** 2 * 'A' ** 2)). % Henry
si_derived_unit(lm, cd).                     % Lumen (semplificato)
si_derived_unit(lx, cd / m ** 2).            % Lux (semplificato)
si_derived_unit('Bq', 1 / s).                  % Becquerel
si_derived_unit('Gy', m ** 2 / s ** 2).        % Gray
si_derived_unit('Sv', m ** 2 / s ** 2).        % Sievert
si_derived_unit(kat, mol / s).               % Katal
si_derived_unit('DC', 'K').

% Unità non SI ma usata per i multipli
grammo_base(kg).

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
si_unit_symbol(grammo, g).
si_unit_symbol(milligrammo, mg).
si_unit_symbol(microgrammo, 'μg').

% Unità con prefisso: si_unit_symbol(NomeUnità, Simbolo)
si_unit_symbol(NomePrefisso-NomeUnita, SimboloPrefissoSimboloUnita) :-
    si_prefix(NomePrefisso, SimboloPrefisso, _),
    si_unit_symbol(NomeUnita, SimboloUnita),
    atom_concat(SimboloPrefisso, SimboloUnita, SimboloPrefissoSimboloUnita).

% Gestione dei multipli di grammo
si_unit_symbol(Prefisso-grammo, Simbolo) :-
    si_prefix(Prefisso, SimboloPrefisso, _),
    atom_concat(SimboloPrefisso, 'g', Simbolo).

% Conversione da multiplo di grammo a kg
grammo_to_kg(ValueG, ValueKg) :-
    ValueKg is ValueG / 1000.

% Conversione da kg a multiplo di grammo
kg_to_grammo(ValueKg, ValueG) :-
    ValueG is ValueKg * 1000.

% Operazione inversa a quella precedente
si_unit_name(S, N) :-
    si_unit_symbol(N, S).
si_unit_name(S, Prefisso-NomeUnita) :-
    si_prefix(Prefisso, SimboloPrefisso, _),
    si_unit_symbol(NomeUnita, SimboloUnita),
    atom_concat(SimboloPrefisso, SimboloUnita, S).

% Genera la lista di simboli con tutti i prefissi per una unità base
all_prefixed_symbols(UnitaBase, ListaSimboli) :-
    findall(Simbolo,
        (si_prefix(_, SimboloPrefisso, _),
         atom_concat(SimboloPrefisso, UnitaBase, Simbolo)),
        ListaSimboli).

% Genera la lista di nomi Prolog con tutti i prefissi per una unità
all_prefixed_names(UnitaBase, ListaNomi) :-
    findall(Prefisso-UnitaBase,
        si_prefix(Prefisso, _, _),
        ListaNomi).

% Trova tutte le unità SI (base e derivate) presenti in una stringa (simbolo)
find_units_in_string(Str, ListaUnita) :-
    findall(Unita,
        (si_unit_symbol(Unita, Simbolo),
         sub_atom(Str, _, _, _, Simbolo)),
        ListaUnita).

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
si_unit_base_expansion(U, Exp) :-
    % Caso unità base SI
    si_base_unit(U), !,
    Exp = U.
si_unit_base_expansion(U, Exp) :-
    % Caso unità derivata SI
    si_derived_unit(U, D), !,
    norm(D, Exp).
si_unit_base_expansion(Prefisso-U, Exp) :-
    % Caso unità con prefisso (es. centi-metro)
    si_prefix(Prefisso, _, Fattore),
    si_unit_base_expansion(U, BaseExp),
    Exp = BaseExp * Fattore.
si_unit_base_expansion(U ** E, Exp) :-
    % Caso potenza
    si_unit_base_expansion(U, BaseExp),
    Exp = BaseExp ** E.
si_unit_base_expansion(U, Exp) :-
    % Caso simbolo con prefisso (es. cm, mg)
    atom(U),
    atom_chars(U, [First|_]),
    si_prefix(Prefisso, SimboloPrefisso, Fattore),
    atom_chars(SimboloPrefisso, [First|_]),
    si_unit_symbol(NomeUnita, BaseSimbolo),
    atom_concat(SimboloPrefisso, BaseSimbolo, U),
    si_unit_base_expansion(NomeUnita, BaseExp),
    Exp = BaseExp * Fattore.

% Dimensione valida
is_dimension(Dim) :-
    is_si_unit(Dim).
is_dimension(Dim1 * Dim2) :-
    is_dimension(Dim1),
    is_dimension(Dim2).
is_dimension(Dim ** E) :-
    is_dimension(Dim),
    integer(E).

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
dim_to_list(U, [(Base, E, Fattore)]) :-
    atom(U),
    decompose_prefixed_unit(U, _, Base, Fattore),
    E = 1, !.
dim_to_list(U ** E, [(Base, E, Fattore)]) :-
    atom(U),
    decompose_prefixed_unit(U, _, Base, Fattore), !.
dim_to_list(U ** E, [(U, E, 1)]) :- !.
dim_to_list(U, [(U, 1, 1)]) :-
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
merge_units_([(U, E, F)|T], Acc, Result) :-
    ( select((U, E0, F0), Acc, Rest) ->
        E1 is E + E0,
        F1 is F * F0,
        merge_units_(T, [(U, E1, F1)|Rest], Result)
    ;
        merge_units_(T, [(U, E, F)|Acc], Result)
    ).

% decompose_prefixed_unit(Simbolo, Prefisso, BaseUnita, Fattore)
decompose_prefixed_unit(Simbolo, Prefisso, BaseUnita, Fattore) :-
    atom(Simbolo),
    si_prefix(Prefisso, SimboloPrefisso, Fattore),
    si_unit_symbol(BaseUnita, BaseSimbolo),
    atom_concat(SimboloPrefisso, BaseSimbolo, Simbolo).
% check_prefixed_unit(Simbolo, BaseUnita)
check_prefixed_unit(Simbolo, BaseUnita) :-
    decompose_prefixed_unit(Simbolo, _, BaseUnita, _).

% expt_base_unit(Unita, Esponente, Base, FattoreTotale)
expt_base_unit(U, E, Base, FattoreTotale) :-
    decompose_prefixed_unit(U, _, Base, Fattore),
    FattoreTotale is Fattore ** E.
expt_base_unit(U, E, U, 1) :-
    si_base_unit(U).

% sum_base_units(ListaUnità, ListaSemplificata)
sum_base_units(List, Result) :-
    sum_base_units_(List, [], Result).
sum_base_units_([], Acc, Acc).
sum_base_units_([(U, E, F)|T], Acc, Result) :-
    ( select((U, E0, F0), Acc, Rest) ->
        E1 is E + E0,
        F1 is F * F0,
        sum_base_units_(T, [(U, E1, F1)|Rest], Result)
    ;
        sum_base_units_(T, [(U, E, F)|Acc], Result)
    ).
    
% simplify_base_units_list(Lista, ListaSemplificata)
simplify_base_units_list([], []).
simplify_base_units_list([(U, E, F)|T], R) :-
    (E =:= 0 -> simplify_base_units_list(T, R)
    ; R = [(U, E, F)|Rest], simplify_base_units_list(T, Rest)
    ).

% Ricostruzione lista 
list_to_dim([], 1).
list_to_dim([(U, E, F)], U ** E * F) :- !.
list_to_dim([(U, 1, F)|T], U * F * D) :-
    list_to_dim(T, D).
list_to_dim([(U, E, F)|T], U ** E * F * D) :-
    list_to_dim(T, D).

% Normalizza una dimensione: somma esponenti, 
% elimina esponenti nulli, associa a sinistra
norm(Dim, NewDim) :-
    dim_to_list(Dim, List),
    merge_units(List, Merged),
    exclude_zero_exponents(Merged, Cleaned),
    list_to_dim(Cleaned, NewDim).

exclude_zero_exponents([], []).
exclude_zero_exponents([(U, E, F)|T], R) :-
    (E =:= 0 -> exclude_zero_exponents(T, R)
    ; R = [(U, E, F)|Rest], exclude_zero_exponents(T, Rest)
    ).

% Elimina unità con esponente zero
exclude_zero_exponents([], []).
exclude_zero_exponents([(U, E)|T], R) :-
    (E =:= 0 -> exclude_zero_exponents(T, R)
    ; R = [(U, E)|Rest], exclude_zero_exponents(T, Rest)
    ).

extract_factor(Dim, F) :-
    dim_to_list(Dim, List),
    extract_factor_list(List, F).

extract_factor_list([], 1).
extract_factor_list([(_, _, F1)|T], F) :-
    extract_factor_list(T, FRest),
    F is F1 * FRest.

% Costruttore di quantità: normalizza la dimensione e converte il valore
make_quantity(Value, Dim, q(ValueNorm, NormDim)) :-
    norm(Dim, NormDim),
    extract_factor(Dim, F),
    extract_factor(NormDim, Fbase),
    ValueNorm is Value * F / Fbase.

% Costruttore di quantità normalizzata
q(N, D, q(NormN, ND)) :-
    norm(D, ND),
    extract_factor(D, F),
    extract_factor(ND, Fbase),
    NormN is N * F / Fbase.

% Validatore quantità
is_quantity(q(Value, Dim)) :-
    number(Value),
    is_dimension(Dim).

% Somma tra quantità (solo se dimensioni compatibili)
qadd(q(V1, D1), q(V2, D2), Q3) :-
    is_quantity(q(V1, D1)),
    is_quantity(q(V2, D2)),
    norm(D1, ND1),
    norm(D2, ND2),
    ND1 = ND2,
    extract_factor(D1, F1),
    extract_factor(D2, F2),
    extract_factor(ND1, Fbase),
    V1base is V1 * F1 / Fbase,
    V2base is V2 * F2 / Fbase,
    V3 is V1base + V2base,
    q(V3, ND1, Q3).

% Sottrazione tra quantità
qsub(Q1, Q2, Q3) :-
    is_quantity(Q1),
    is_quantity(Q2),
    Q1 = q(V1, D1),
    Q2 = q(V2, D2),
    norm(D1, ND1),
    norm(D2, ND2),
    ND1 = ND2,
    extract_factor(D1, F1),
    extract_factor(D2, F2),
    extract_factor(ND1, Fbase),
    V1base is V1 * F1 / Fbase,
    V2base is V2 * F2 / Fbase,
    V3 is V1base - V2base,
    q(V3, ND1, Q3).

% Moltiplicazione tra quantità
qmul(Q1, Q2, Q3) :-
    is_quantity(Q1),
    is_quantity(Q2),
    Q1 = q(V1, D1),
    Q2 = q(V2, D2),
    V3 is V1 * V2,
    Dtemp = D1 * D2,
    q(V3, Dtemp, Q3).

% Divisione tra quantità
qdiv(Q1, Q2, Q3) :-
    is_quantity(Q1),
    is_quantity(Q2),
    Q1 = q(V1, D1),
    Q2 = q(V2, D2),
    Q2 = q(V2, _), % per chiarezza
    (V2 =:= 0 -> throw(error('Divisione per zero', qdiv/3))
    ; V3 is V1 / V2,
      Dtemp = D1 * (D2 ** -1),
      q(V3, Dtemp, Q3)
    ).

% Elevamento a potenza intera
qexp(Q, N, Qres) :-
    is_quantity(Q),
    Q = q(V, D),
    integer(N),
    VR is V ** N,
    expand_power(D, N, Dexp),
    q(VR, Dexp, Qres).

% Espansione di potenza su espressioni
expand_power(Prefisso-U, N, Exp) :-
    % Caso unità con prefisso (es. centi-metro)
    si_prefix(Prefisso, _, Fattore),
    expand_power(U, N, BaseExp),
    FattorePot is Fattore ** N,
    Exp = BaseExp * FattorePot.
expand_power(U ** E, N, U ** E1) :-
    E1 is E * N.
expand_power(U1 * U2, N, D1 * D2) :-
    expand_power(U1, N, D1),
    expand_power(U2, N, D2).
expand_power(U, N, U ** N) :-
    atom(U).
% Espansione di potenza su simboli con prefisso (es. cm, mg)
expand_power(U, N, Exp) :-
    atom(U),
    decompose_prefixed_unit(U, Prefisso, BaseUnita, Fattore),
    expand_power(BaseUnita, N, BaseExp),
    FattorePot is Fattore ** N,
    Exp = BaseExp * FattorePot.

% true se le due dimensioni sono equivalenti dopo normalizzazione
same_dim(D1, D2) :-
    norm(D1, ND1),
    norm(D2, ND2),
    ND1 = ND2.

% Unità derivate espresse in unità base SI
si_unit_def('N', kg * m ** 2 * s ** -2).               % Newton = kg·m²/s²
si_unit_def('Pa', kg * m ** -1 * s ** -2).             % Pascal = N/m²
si_unit_def('J', 'N' * m).                             % Joule = N·m
si_unit_def('W', 'J' * s ** -1).                       % Watt = J/s
si_unit_def('C', s * 'A').                             % Coulomb = s·A
si_unit_def('V', m ** 2 * kg * s ** -3 * 'A' ** -1).   % Volt = W/A
si_unit_def('Ohm', m ** 2 * kg * s ** -3 * 'A' ** -2). % Ohm = V/A
si_unit_def('F', 'C' * 'V' ** -1).                     % Farad = C/V
si_unit_def('T', kg * s ** -2 * 'A' ** -1).            % Tesla = Wb/m²
si_unit_def('Wb', m ** 2 * kg * s ** -1 * 'A' ** -1).  % Weber = V·s
si_unit_def('S', 'A' * 'V' ** -1).                     % Siemens = A/V
si_unit_def('H', 'Wb' * 'A' ** -1).                    % Henry = Wb/A
si_unit_def('lx', cd * m ** -2).                       % Lux = cd/m²