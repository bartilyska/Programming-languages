insertion_sort(List,Sorted):- i_sort(List,[],Sorted). %wywolaj sorta z pustym akumulatorem
i_sort([],Akumulator,Akumulator). %jesli juz nie ma elementow to koniec
i_sort([H|T],Akumulator,Sorted):- 
    insert(H,Akumulator,NowyAkumulator), %wstaw Head do nowego akumulatora
    i_sort(T,NowyAkumulator,Sorted). %Tail dalej sortuj z nowym akumulatorem

insert(Elem,[],[Elem]). %jesli lista jest pusta to na poczatek wstaw X  
insert(Elem,[Y|T],[Y|NT]):- Elem<Y,insert(Elem,T,NT).%poki X mniejszy od Y to szukaj miejsca w nowym dalej
insert(Elem,[Y|T],[Elem,Y|T]):- Elem>=Y. %Wstaw Elem przed Y, jeśli Elem >= Y


write_list([]).
write_list([H|T]):- write(H),write(' '),write_list(T).

%zad2

czy_graficzny(Lista,Odp) :-
    write('Ciag stopniowy: '), write_list(Lista),
    can_be_drawn(Lista),!,% Wsadzi do X tak jesli jest graficzna lista
    Odp = 'Tak Graficzny'; % W przeciwnym wypadku do X wsadzi 'Nie'
    Odp = 'Nie Graficzny'.

can_be_drawn([H|T]) :- 
    list_full_of_zeros([H|T]),!; %jesli same zera to koniec i prawda (; w przeciwnym wypadku sprawdz dalej)
    insertion_sort([H|T], Sorted), % Posortuj zeby zawsze najwieksza liczba byla na poczatku
    step(Sorted).

step([H|T]) :- 
    list_length(T, Length),
    H =< Length, %dlugosc ogona listy musi byc wieksza lub rowna H (zeby dalo sie poodejmowac 1 od kazdego)
    decrement_first_n_elements(T, H, NewList),
    list_not_negative(NewList), %w nowej liscie nie moga byc ujemne elementy
    can_be_drawn(NewList).

% Dekrementacja pierwszych N elementów listy
decrement_first_n_elements(List, 0, List). % Jeśli N = 0, zwracamy listę bez zmian
decrement_first_n_elements([H|T], N, [H1|T1]) :- 
    H1 is H - 1, % Dekrementacja pierwszego elementu
    N1 is N - 1, % Zmniejszamy licznik
    decrement_first_n_elements(T, N1, T1).

% Obliczanie długości listy
list_length([], 0).
list_length([_|T], N) :- list_length(T, N1), N is N1 + 1.

%Sprawdź czy nie ma zadnych ujemnych elementów
list_not_negative([]).
list_not_negative([H|T]) :- H >= 0, list_not_negative(T).

%Sprawdz czy same zera
list_full_of_zeros([]).
list_full_of_zeros([H|T]) :- H = 0, list_full_of_zeros(T).

%zad3
list_greater_than_zeros([]).
list_greater_than_zeros([H|T]) :- H > 0, list_greater_than_zeros(T).

sum_list([],0).
sum_list([H|T],Sum):- sum_list(T,Sum1), Sum is Sum1+H.

is_connected([H|T]):- 
    list_greater_than_zeros([H|T]), 
    sum_list([H|T],Sum),
    list_length([H|T],Length),
    Sum >= 2*(Length-1), %musi spelniac ta zaleznosc i nie miec zerowych, gdy 1 wierzcholek z stopniem 0 to sie tu lapie
    can_be_drawn([H|T]).%musi być graficznym

czy_spojny(Lista,Odp) :-
    write('Ciag stopniowy: '), write_list(Lista),
    is_connected(Lista),!,% Wsadzi do X tak jesli spojna lista
    Odp = 'Tak Spojny'; %W przeciwnym wypadku do X wsadzi 'Nie'
    Odp = 'Nie Spojny'.

%zad4
to_samo(X,X).
to_samo(X,X,_).
%do testow dla tych samych strategii wystarczy w generatach dac to samo
generateP1(Board, NewBoard) :-
    moveLeft(Board, UpdatedBoard), moveRight2(UpdatedBoard,NewBoard), % 1 w lewo 2 w prawo
    write('P1: '), write(Board), write(' -> '), write(NewBoard), nl.

generateP2(Board, NewBoard) :-
    moveLeftmostPawn(Board, UpdatedBoard,0,Index),
    (
        NewBoard = UpdatedBoard; %Gracz nie musi  wykonac ruchu
        moveRight1WithoutIndex(UpdatedBoard, NewBoard,Index) %Gracz decyduje wykonuje ruch
    ),
    write('P2: '), write(Board), write(' -> '), write(NewBoard), nl.

moveLeft([], []). % Jeśli plansza jest pusta, nie ma ruchu do wykonania
moveLeft([H|T],[H|P]):- moveLeft(T,P), \+ to_samo(T,P). %kontynuuj przeszukiwanie 
moveLeft([X,Y|T],[X1,Y1|T]):- %cofnij pionek z Y na X(jedno pole)
    Y > 0,
    X =< 1, %moga byc dwa pionki na tym samym polu
    X1 is X+1,
    Y1 is Y-1.

moveRight2([], []). % Jeśli plansza jest pusta, nie ma ruchu do wykonania
moveRight2([H|T],[H|P]):- moveRight2(T,P), \+ to_samo(T,P). %kontynuuj przeszukiwanie
moveRight2([X,Y,Z|T],[X1,Y,Z1|T]):- %przesun pionek z X na Z (dwa pola)
    X > 0,
    Y =< 1, %moga byc dwa pionki na tym samym polu w trakcie ruchu tez liczone
    Z =< 1, 
    X1 is X-1,
    Z1 is Z+1.       

moveLeftmostPawn([], [],_, -1). % Jeśli lista jest pusta, nie ma ruchu do wykonania

moveLeftmostPawn([H, Next | T], [H | TUpdated], CurrentIndex, TargetIndex) :-
    (Next > 1; H =< 0),         % To sprawdza najpierw od prawej stad gwarancja ze jak znajdzie to wykona dla najbardziej na lewo
    NextIndex is CurrentIndex + 1,        % Przejdź do następnej pozycji i dodaj 1 do indeksu
    moveLeftmostPawn([Next | T], TUpdated,NextIndex, TargetIndex),
    !.% czy powinien być ! ? (chyba tak zeby dalej nie szukal bo to wymuszony ruch z lewej)

moveLeftmostPawn([H, Next| T], [H1, Next1| T], CurrentIndex, TargetIndex) :-
    H > 0,                      % Na tym polu musi być przynajmniej jeden pionek (moze zbedne ale niech bedzie)
    Next =< 1,                     %na next nie moze byc wiecej niz 1 pionek przed dodaniem (mozne zbedne ale niech bedzie)
    H1 is H - 1,              % Usuwamy jeden pionek z tego pola
    Next1 is Next + 1, % Dodajemy jeden pionek na następnym polu
    TargetIndex is CurrentIndex + 1.      % Zwracamy indeks, na którym wylądował pionek

moveRight1WithoutIndex([], [],_). % Jeśli lista jest pusta, nie ma ruchu do wykonania
moveRight1WithoutIndex([H|T],[H|P],Index):- NewIndex is Index - 1, moveRight1WithoutIndex(T,P,NewIndex), \+ to_samo(T,P,NewIndex). %kontynuuj przeszukiwanie
moveRight1WithoutIndex([X,Y|T],[X1,Y1|T],Index):- %przesun pionek z X na Y (jedno pole)
    %jesli indeks==0 to znaczy ze to jest to pole na ktore przesunelismy pionek jesli jest tam tylko jeden pionek to nie mozemy przesunac (dobrze policzone testowane)
    (Index =\= 0; Y > 1), 
    X > 0,
    Y =< 1, %moga byc dwa pionki na tym samym polu
    X1 is X-1,
    Y1 is Y+1.

% GAME
moveP1(A) :- generateP1(A, M), \+ moveP2(M).
moveP2(A) :- generateP2(A, M), \+ moveP1(M). 

czy_win(Lista,Odp) :-
    write('Gra: '), write(Lista), nl,
    moveP1(Lista),!,
    Odp = 'Pierwszy ma wygrywajaca'; 
    Odp = 'Drugi ma wygrywajaca'.
%Testy

?-insertion_sort([5,3,8,2,1,9], Posortowana), write_list(Posortowana),nl.
?-czy_graficzny([3, 2, 2, 1], Odp), write(Odp), nl.%tak
?-czy_graficzny([4, 2, 1, 1], Odp), write(Odp), nl.%nie 
?-czy_graficzny([1,1,1,1,1], Odp), write(Odp), nl.%nie 
?-czy_graficzny([1,2,2,1,2],Odp), write(Odp), nl.%tak
?-czy_graficzny([6,6,5,4,3,3,3,3,2,1,0,0,0],Odp), write(Odp), nl.%tak
?-czy_graficzny([3,3,3,0,3],Odp), write(Odp), nl.%tak
?-czy_graficzny([1,0,1],Odp), write(Odp), nl.%tak
?-czy_graficzny([8,7,5,3,2,4,6,7,8,5,3,8,8,8,8,8,8,8,8,8,8,8,8,8],Odp), write(Odp), nl.%tak
?-czy_graficzny([8,7,5,3,2,4,6,7,8,5,3,8,8,8,8,8,8,8,8,8,8,8,8,8,9],Odp), write(Odp), nl.%nie
?-czy_graficzny([-1,1],Odp), write(Odp), nl.%nie
?-czy_spojny( [1,0,1], Odp ), write(Odp), nl. % graf niespójny ODP = N
?-czy_spojny( [1,1,1], Odp ), write(Odp), nl. % ciąg nie graficzny ODP = N
?-czy_spojny( [1,1,1,1], Odp), write(Odp), nl. % graf niespójnyODP = N
?-czy_spojny( [1,2,2,1,2], Odp), write(Odp), nl. % graf spójnyODP = T
?-czy_spojny( [3,3,3,0,3], Odp ), write(Odp), nl. % graf niespójnyODP = N
% zazwyczaj drugi wygrywa bo pierwszy musi cofac w lewo 
?- czy_win([1,0,0,1],Odp),write(Odp),nl,nl.
?- czy_win([1,0,1,0],Odp),write(Odp),nl,nl.
?- czy_win([1,2,1,0],Odp),write(Odp),nl,nl. 
?- czy_win([0,1,2,1,0],Odp),write(Odp),nl,nl.
?- czy_win([4,0,0,0],Odp),write(Odp),nl,nl.%pierwszy nie ma jak cofnac
?- czy_win([1,1,1,1],Odp),write(Odp),nl,nl.
?- czy_win([1,2,1,1,2],Odp),write(Odp),nl,nl.
?- czy_win([1,0,2,2],Odp),write(Odp),nl,nl.%przypadek gdzie pierwszy wygrywa ta ponizej redukuje sie do podobnej pozycji (chyba)
%?- czy_win([1,0,1,0,1,1,1],Odp),write(Odp),nl,nl.%dluga plansza