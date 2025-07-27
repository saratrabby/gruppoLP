Progetto E4p - Prolog

I nomi delle unita' con prefisso hanno un - che separa prefisso da nome dell'unita'. Es. centi-metro, micro-metro. Questo non e' valido per i simboli es cm NON c-m.
I simboli delle unita' (base e derivate) usano i quote '' se hanno delle lettere maiuscole. es. m, 'A', 'omega' (caso particolare).
É stato utilizzato il simbolo μ che in alcuni editor di testo può essere visualizzato in maniera non corretta (es. Î¼).

I prefissi utilizzabili in questo programma sono: chilo, etto, deca, deci, centi, milli, micro, nano, pico.

si_unit_name e si_unit_symbol NON sono invertibili.

in si_unit_symbol, se viene specificata un'unita' che ha delle maiuscole nel nome (es. Becquerel ma non metro) bisogna utilizzare i '' attorno al nome (es. 'Ampere' o milli-'Ampere' e non milli-Ampere). Il primo argomento NON dev'essere una variabile.

in si_unit_name, il primo argomento dev'essere il simbolo (con o senza prefisso) di una unità si (o una variabile).

dim_to_list:Conversione dimensione in lista [(unitÃ , esponente)]
il secondo argomento sara'una lista di terms (unita esponente fattore),
dove fattore e' il valore per cui moltiplicare l'unita' senza prefisso
per arrivare alla unita' con prefisso es. cm -> 0.01. Tiene conto di esponenti
es. cm**2 ha fattore 0.0001.

per la trasformazione di una dimensione in forma canonica esistono due predicati principali: norm e norm_no_fatt.
La differenza tra i due e' che norm mantiene il fattore di tutte le unita' e norm_no_fatt non tiene conto dei fattori. es. norm(cm, X). -> X = m**1*0.01 norm_no_fatt(cm, X). -> X = m**1

Per il sorting viene creata una lista di pairs (library(pairs)) associati al loro valore chiave, che ne determina l'ordine.