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

disease_p04 :-
    verify(has_g010),
    verify(has_g014),
    verify(has_g015).
disease_p05 :-
    verify(has_g010),
    verify(has_g016),
    verify(has_g017),
    verify(has_g018),
    verify(has_g019),
    verify(has_g020).
disease_p06 :-
    verify(has_g021),
    verify(has_g022),
    verify(has_g023),
    verify(has_g024).
disease_p07 :-
    verify(has_g021),
    verify(has_g025),
    verify(has_g026),
    verify(has_g027).
disease_p08 :-
    verify(has_g028),
    verify(has_g029),
    verify(has_g030).
disease_p09 :-
    verify(has_g031),
    verify(has_g032),
    verify(has_g033),
    verify(has_g034).


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
disease(disease_p04):-
    write('Berilliosis').
disease(disease_p05):-
    write('Bissinosis').
disease(disease_p06):-
    write('Emboli Paru').
disease(disease_p07):-
    write('Pneumonia').
disease(disease_p08):-
    write('Histiotosis X').
disease(disease_p09):-
    write('Granulomatosis Pulmoner').
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
symptomp(has_g015):-
    write('badan terasa lemas').
symptomp(has_g016):-
    write('ada riwayat pemaparan debu').
symptomp(has_g017):-
    write('kondisi memburuk jika berada di tempat kerja').
symptomp(has_g018):-
    write('kondisi membaik jika jauh dari tempat kerja').
symptomp(has_g019):-
    write('dada terasa sesak').
symptomp(has_g020):-
    write('batuk pendek pendek').
symptomp(has_g021):-
    write('pernafasan cepat').
symptomp(has_g022):-
    write('denyut jantung cepat').
symptomp(has_g023):-
    write('keluar keringat (berkeringat)').
symptomp(has_g024):-
    write('pusing').
symptomp(has_g025):-
    write('batuk berdahak kadang dahak disertai nanah').
symptomp(has_g026):-
    write('kadang disertai rasa mual dan muntah').
symptomp(has_g027):-
    write('badan mengigil').
symptomp(has_g028):-
    write('volume air seni meningkat').
symptomp(has_g029):-
    write('kadang disertai nyeri tulang').
symptomp(has_g030):-
    write('sering merasa haus walaupun banyak minum').
symptomp(has_g031):-
    write('mudah merasa lelah').
symptomp(has_g032):-
    write('tidak enak badan (malaise).').
symptomp(has_g033):-
    write('terjadi pendarahan di hidung').
symptomp(has_g034):-
    write('ada lukas disekitar hidung').

switch(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).