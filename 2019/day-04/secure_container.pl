% https://adventofcode.com/2019/day/4
% Input 125730-579381

number_to_array(Number, Array) :-
  number_codes(Number, Codes),
  maplist(plus(48), Array, Codes).

is__adjacent(X, X, 1) :- !.
is__adjacent(_, _, 0).

is_increasing(X, Y, Result) :- X > Y, Result is 0, !.
is_increasing(_, _, 1).

valid_password([_], 0, _, 0) :- !.
valid_password([_], _, 0, 0) :- !.
valid_password([_], _, _, 1) :- !.
valid_password([X, Y | T], Adjacent, Increasing, Result) :-
  is__adjacent(X, Y, AdjacentResult),
  is_increasing(X, Y, IncreasingResult),
  NewAdjacent is Adjacent + AdjacentResult,
  NewIncreasing is Increasing * IncreasingResult,
  valid_password([Y | T], NewAdjacent, NewIncreasing, Result).

count_different_passwords(X, X, Result, Result) :- !.
count_different_passwords(Low, High, Acc, Result) :-
  number_to_array(Low, Array),
  valid_password(Array, 0, 1, Valid),
  NewAcc is Acc + Valid,
  NewLow is Low + 1,
  count_different_passwords(NewLow, High, NewAcc, Result).

% Part 1
% ?- count_different_passwords(125730, 579381, 0, R).

is_isolatably_adjacent(X, Y, Z, W, 1) :- X \= Y, Y = Z, Z \= W, !.
is_isolatably_adjacent(_, _, _, _, 0).

valid_password_2([_], 0, _, 0) :- !.
valid_password_2([_], _, 0, 0) :- !.
valid_password_2([_], _, _, 1) :- !.
valid_password_2([X, Y, Z, W | T], Adjacent, Increasing, Result) :-
  is_isolatably_adjacent(X, Y, Z, W, AdjacentResult),
  is_increasing(X, Y, IncreasingResult),
  NewAdjacent is Adjacent + AdjacentResult,
  NewIncreasing is Increasing * IncreasingResult,
  valid_password_2([Y, Z, W | T], NewAdjacent, NewIncreasing, Result).

count_different_passwords_2(X, X, Result, Result) :- !.
count_different_passwords_2(Low, High, Acc, Result) :-
  number_to_array(Low, Array),
  valid_password_2(Array, 0, 1, Valid),
  NewAcc is Acc + Valid,
  NewLow is Low + 1,
  count_different_passwords_2(NewLow, High, NewAcc, Result).

% Part 2
% ?- count_different_passwords_2(125730, 579381, 0, R).
