% Functii ajutatoare

% functia de transpunere auxiliara o sa transpuna prima coloana din matrice
trans_aux([], [], []).
trans_aux([[H|T]|TM], [H|Hs], [T|Ts]) :- trans_aux(TM, Hs, Ts).

% functia de transpunere apeleaza functia anterioara recursiv pana cand fiecare
% coloana va fi transpuna si concatenata la matricea rezultata.
transpose([[]|_], []).
transpose(M, [H|T]) :- trans_aux(M, H, R), transpose(R, T).

% functiile de head si tail
head([H|_], R):- R = H.
tail([_|T], R):- R = T.

%Tema
:- use_module(tables).
:- use_module(check_predicates).

%functia transforma un vector de stringuri intr-un vector de lungimi
stringToLength([], []).
stringToLength([H|T], [R1|R]):- string_length(H, R1), stringToLength(T, R).

%maximul dintr-un vector
maxList([H], H).
maxList([H|T], R):- maxList(T, R2), (H > R2 -> R = H ; R = R2).

%functia de generare a stringului de formatare
plus5(X,Y):- Y is X + 5. make_format_str(MaxRowLen,Str) :-
maplist(plus5,MaxRowLen,Rp), aux_format(Rp,Str). aux_format([H],R) :- string_concat("~t~w~t~",H,R1),
string_concat(R1,"+~n",R),!.
aux_format([H|T],R) :- string_concat("~t~w~t~",H,R1),
string_concat(R1,"+ ",R2), aux_format(T,Rp), string_concat(R2,Rp,R).

% functie ce transforma matricea de stringuri primita in matrice de lungimi (dupa ce matricea initiala a fost transpusa)
% urmand ca apoi sa extraga maximul de pe fiecare line si apeleaza functia make_format_str
getFormatStr(L, R):- transpose(L, R1), maplist(stringToLength, R1, R2), maplist(maxList, R2, R3), make_format_str(R3, R).

% afiseaza o singura linie
showLine([], _).
showLine([H|T], X):- format(X, H), showLine(T, X).

% pentru fiecare linie se apeleaza functia anterioara
show(L):- getFormatStr(L, R), showLine(L, R).

% cauta linia care are pe prima pozitie elementul X, si o intoarce
% in caz contrar va intoarce o lista vida
selectAnEntry([], _, []).
selectAnEntry([H|T], X, R):- selectAnEntry(T, X, R2), head(H, R1), (R1 == X -> R = H; R = R2). 

% pentru fiecare element din lista de coloane ce se doreste a fi selectate este apelata
% functia anterioara si listele intoarse vor fi concatenate
selectAllEntries(_, [], []).
selectAllEntries(L, [X|XS], R):- selectAnEntry(L, X, R1), R1 == [], !, selectAllEntries(L, XS, R).
selectAllEntries(L, [X|XS], [R1|R]):- selectAnEntry(L, X, R1), selectAllEntries(L, XS, R).

% matricea primita este transpusa pentru a usura procesul de selectare iar matricea primita
% ca raspuns va fi si ea transpusa
select_op(L, C, R):- transpose(L, R1), selectAllEntries(R1, C, R2), transpose(R2, R).

% maplist/4 este echivalentul lui zipWith din Haskell, astfel apelez maplist pe cele 2 table
% exceptie facand headerele acestora care este alipit ulterior rezultatului apelului de maplist
join_op(Op, NewCols, [H1|T1], [H2|T2], R):- maplist(Op, T1, T2, R2), append([NewCols], R2, R).

% functia de filter preluata din curs
filter_op([],_,_,[]).
filter_op([H|T], Vars, Pred, R):- not((Vars = H, Pred)), filter_op(T, Vars, Pred, R), !.
filter_op([H|T], Vars, Pred, [H|R]):- filter_op(T, Vars, Pred, R).

% se face o filtrare dupa condiitiile impuse
complex_query(L, R):- eval(tfilter([_,_,AA,PP,_,_,_], ((AA + PP ) / 2 > 6), L), R1),
					eval(tfilter([_,_,AA,PP,PC,PA,POO], (((AA+PP+PC+PA+POO) / 5) > 5), just_table(R1)), R2),
					eval(tfilter([_,LN,_,_,_,_,_], sub_string(LN,_,_,_,"escu"), just_table(R2)), R).

eval(table(N), L) :- table_name(N, L).
eval(just_table(T), T).
eval(tprint(N), R) :- eval(N, R), show(R).
eval(select(C, L), R):- eval(L, L1), select_op(L1, C, R). 
eval(join(Op, C, T1, T2), R):- eval(T1, R1), eval(T2, R2), join_op(Op, C, R1, R2, R).
eval(tfilter(Vars, Pred, L), R):- eval(L, [H1|T1]), filter_op(T1, Vars, Pred, R1), append([H1], R1, R).
eval(complex_query1(T),R):- complex_query(T, R).


