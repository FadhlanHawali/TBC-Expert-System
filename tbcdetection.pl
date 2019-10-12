/* TBC detection
    start with ?- go. */

go :- 
    hypothesize(Symptomps),
    write('Sesuai dengan gejala anda mengidap penyakit : '),
    disease(Symptomps),
    nl,
    undo.

/*hipotesa yang akan dites */
hypothesize(disease_p01) :- disease_p01, !.
hypothesize(disease_p02) :- disease_p02, !.
hypothesize(disease_p03) :- disease_p03, !.
hypothesize(disease_p04) :- disease_p04, !.
hypothesize(disease_p05) :- disease_p05, !.
hypothesize(disease_p06) :- disease_p06, !.
hypothesize(disease_p07) :- disease_p07, !.
hypothesize(disease_p08) :- disease_p08, !.
hypothesize(disease_p09) :- disease_p09, !.
hypothesize(disease_unknown).             /* no diagnosis */

/* symptomp identification rules */
disease_p01 :- 
    verify(has_g001),
    verify(has_g002),
    verify(has_g003),
    verify(has_g004),
    verify(has_g005),
    verify(has_g006).

disease_p02 :-
    verify(has_g001),
    verify(has_g007),
    verify(has_g008),
    verify(has_g009).

disease_p03 :-
    verify(has_g010),
    verify(has_g011),
    verify(has_g012),
    verify(has_g013).



/*how to ask questions*/
ask(Question) :-
    write('Apakah anda memiliki gejala : '),
    symptomp(Question),
    write('? '),
    read(Response),
    nl,
    ( (Response == yes ; Response == y)
        ->
        assert(yes(Question)) ;
        assert(no(Question)),fail).

:- dynamic yes/1,no/1.

/* How to verify something */
verify(S) :-
   (yes(S) 
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(S))).

/* undo all yes/no assertions */
undo :- retract(yes(_)),fail. 
undo :- retract(no(_)),fail.
undo.

disease(disease_p01):-
    write('Abses Paru').
disease(disease_p02):-
    write('Aspergilosis Bronkopulmoner').
disease(disease_p03):-
    write('Atelektasis').
disease(disease_unknown):-
    write('tidak memiliki penyakit').

symptomp(has_g001):-
    write('batuk berdahak kadang disertai darah').
symptomp(has_g002):-
    write('batuk berdahak disertai bau busuk').
symptomp(has_g003):-
    write('nyeri dada ketika bernafas').
symptomp(has_g004):-
    write('sering merasa kelelahan').
symptomp(has_g005):-
    write('nafsu makan berkurang').
symptomp(has_g006):-
    write('berat badan turun secara perlahan').
symptomp(has_g007):-
    write('sesak nafas berat').
symptomp(has_g008):-
    write('mengi/bengek').
symptomp(has_g009):-
    write('demam').
symptomp(has_g010):-
    write('sesak nafas ringan (seperti asma)').
symptomp(has_g011):-
    write('mengalami gangguan pernafasan').
symptomp(has_g012):-
    write('nyeri dada').
symptomp(has_g013):-
    write('batuk kering').
symptomp(has_g014):-
    write('berat badan turun secara drastis').

switch(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).