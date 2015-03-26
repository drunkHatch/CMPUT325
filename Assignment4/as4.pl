
%
% Knowledge base for question 1
%

c325(fall_2014,aperf,15,15,15,15,79,99).
c325(fall_2014,john,14,13,15,10,76,87).
c325(fall_2014,lily, 9,12,14,14,76,92).
c325(fall_2014,peter,8,13,12,9,56,58).
c325(fall_2014,ann,14,15,15,14,76,95).
c325(fall_2014,ken,11,12,13,14,54,87).
c325(fall_2014,kris,13,10,9,7,60,80).
c325(fall_2014,audrey,10,13,15,11,70,80).
c325(fall_2014,randy,14,13,11,9,67,76).
c325(fall_2014,david,15,15,11,12,66,76).
c325(fall_2014,sam,10,13,10,15,65,67).
c325(fall_2014,kim,14,13,12,11,68,78).
c325(fall_2014,perf,15,15,15,15,80,100).
c325(winter_2014,aperf,15,15,15,15,80,99).

setup(fall_2014,as1,15,0.1).
setup(fall_2014,as2,15,0.1).
setup(fall_2014,as3,15,0.1).
setup(fall_2014,as4,15,0.1).
setup(fall_2014,midterm,80,0.25).
setup(fall_2014,final,100,0.35).

assignments([as1, as2, as3, as4, midterm, final]).

%
% Question 1 A
%

query1(Semester, Name, Total) :-
    c325(Semester, Name, As1, As2, As3, As4, Midterm, Final),
    assignments(Assignments),
    length(Assignments, AssignmentCount),
    repeat(Semester, AssignmentCount, S), 
    zip3(S, Assignments, [As1, As2, As3, As4, Midterm, Final], CalcList),
    map(CalcList, calc, Results),
    reduce(Results, add, Total).

calc([Semester, Assignment, Mark], Result) :-
    calc(Semester, Assignment, Mark, Result).

calc(Semester, Assignment, Mark, Result) :-
    setup(Semester, Assignment, Total, Wieght),
    Result is Mark / Total * Wieght * 100.
    

%
% Helpers
%

% self explanatory
add(A, B, R) :-
    R is A + B.

% simple generator that creates the first parameter repeated multiple times in a list
repeat(_, 0, []).
repeat(A, Count, [A|C]) :-
    NewCount is Count - 1,
    Count > 0,
    repeat(A, NewCount, C),
    !.

% takes 2 lists and pairs them up element by element into a new list, lists must be the same length
zip3([], [], [], []).
zip3([A|B], [C|D], [E|F], [[A, C, E]|G]):-
    zip3(B, D, F, G).

% simple map function that operates over all elements in a list to create a new list
map([], _, []).
map([A|B], Pred, [C|D]) :-
    call(Pred, A, C),
    map(B, Pred, D).

% simple helper to reduce with a predicate
reduce([A], _, A).
reduce([A|B], Pred, Result) :-
    reduce(B, Pred, R),
    call(Pred, R, A, Result).
