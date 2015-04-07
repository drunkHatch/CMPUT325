%
% Jesse Tucker
% 1255723
% CMPUT 325 Assignment 4
%

%
% Knowledge base for question 1
%

:- use_module(library(clpfd)).
:- dynamic c325/8.
:- dynamic setup/4.

insert_data :-
    assert(c325(fall_2014,aperf,15,15,15,15,79,99)),
    assert(c325(fall_2014,john,14,13,15,10,76,87)),
    assert(c325(fall_2014,lily, 9,12,14,14,76,92)),
    assert(c325(fall_2014,peter,8,13,12,9,56,58)),
    assert(c325(fall_2014,ann,14,15,15,14,76,95)),
    assert(c325(fall_2014,ken,11,12,13,14,54,87)),
    assert(c325(fall_2014,kris,13,10,9,7,60,80)),
    assert(c325(fall_2014,audrey,10,13,15,11,70,80)),
    assert(c325(fall_2014,randy,14,13,11,9,67,76)),
    assert(c325(fall_2014,david,15,15,11,12,66,76)),
    assert(c325(fall_2014,sam,10,13,10,15,65,67)),
    assert(c325(fall_2014,kim,14,13,12,11,68,78)),
    assert(c325(fall_2014,perf,15,15,15,15,80,100)),
    assert(c325(winter_2014,aperf,15,15,15,15,80,99)),
    assert(setup(fall_2014,as1,15,0.1)),
    assert(setup(fall_2014,as2,15,0.1)),
    assert(setup(fall_2014,as3,15,0.1)),
    assert(setup(fall_2014,as4,15,0.1)),
    assert(setup(fall_2014,midterm,80,0.25)),
    assert(setup(fall_2014,final,100,0.35)).

assignments([as1, as2, as3, as4, midterm, final]).

%
% Question 1 A
%

query1(Semester, Name, Total) :-
    c325(Semester, Name, As1, As2, As3, As4, Midterm, Final),
    assignments(Assignments),
    length(Assignments, AssignmentCount),
    repeat(Semester, AssignmentCount, S), 
    zip(S, Assignments, [As1, As2, As3, As4, Midterm, Final], CalcList),
    map(CalcList, calc, Results),
    reduce(Results, add, Total).

calc([Semester, Assignment, Mark], Result) :-
    calc(Semester, Assignment, Mark, Result).

calc(Semester, Assignment, Mark, Result) :-
    setup(Semester, Assignment, Total, Wieght),
    Result is Mark / Total * Wieght * 100.

%
% Question 1 B
%

query2(Semester, Improved) :-
    findall(X, c325(Semester, X, _, _, _, _, _, _), Students),
    length(Students, NumStudents),
    repeat(Semester, NumStudents, S),
    zip(S, Students, In),
    filter(In, isImproved, Filtered),
    map(Filtered, getName, Improved),
    !.

getName([_, Name], Name).

isImproved([Semester, Name]) :-
    isImproved(Semester, Name).
isImproved(Semester, Name) :-
    c325(Semester, Name, _, _, _, _, MidtermMark, FinalMark),
    setup(Semester, midterm, MidtermTotal, _),
    setup(Semester, final, FinalTotal, _),
    (MidtermMark / MidtermTotal) < (FinalMark / FinalTotal).

%
% Question 1 C
%

query3(Semester, Name, Type, NewMark) :-
    c325(Semester, Name, As1, As2, As3, As4, Midterm, Final),
    assignments(L),
    zip(L, [As1, As2, As3, As4, Midterm, Final], AssignmentMap),
    updateMark(AssignmentMap, 
	       Type, 
	       NewMark, 
	       [NewAs1, NewAs2, NewAs3, NewAs4, NewMidterm, NewFinal]),
    retract(c325(Semester, Name, As1, As2, As3, As4, Midterm, Final)),
    assert(c325(Semester, Name, NewAs1, NewAs2, NewAs3, NewAs4, NewMidterm, NewFinal)).

query3(Semester, Name, _, _) :-
    \+ c325(Semester, Name, _, _, _, _, _, _),
    write('record not found').

updateMark([[Type, _]|Rest], Type, NewMark, [NewMark|Rest]).
updateMark([[_, Mark]|Rest], Type, NewMark, [Mark|C]) :-
    updateMark(Rest, Type, NewMark, C).

%
% Question 2
%

sessions([a, b, c, d, e, f, g, h, i, j, k]).

schedule(TimeList, RoomList) :-
    findall(I, room(I), Rooms),
    times(Times),
    length(Rooms, NumRoomsP),
    length(Times, NumTimesP),

    NumTimes is NumTimesP - 1,
    NumRooms is NumRoomsP - 1,
    
    sessions(Sessions),
    length(Sessions, L),
    length(TimeListActual, L),
    length(RoomListActual, L),

    domain(TimeListActual, 0, NumTimes),
    domain(RoomListActual, 0, NumRooms),
    
    notSameTimeCst(TimeListActual),
    beforeCst(TimeListActual),
    atCst(TimeListActual, RoomListActual),
    exclusiveCst(TimeListActual, RoomListActual),

    append(TimeListActual, RoomListActual, W),
    labeling([], W),
    
    map(TimeListActual, convertTimesToTAFormat, TimeList),
    map(RoomListActual, convertRoomsToTAFormat, RoomList),

    myPrint(TimeList, RoomList, Sessions).

convertRoomsToTAFormat(RoomNum, Res) :-
    % rooms are off by 10
    Res is RoomNum + 10.

convertTimesToTAFormat(TimeNum, Res) :-
    % times are off by 1
    Res is TimeNum + 1.

myPrint([], [], []).
myPrint([T|L],[W|R],[List|Rest]):-
    write('session '), write(List), write(' at time '),
    write(T), write(' in room '),
    write(W), write('\n'),
    myPrint(L,R,Rest).

times([firstDayAm, firstDayPm, secondDayAm, secondDayPm]).

% Not at same time constraint

notSameTimeCst(TimeList) :-
    findall(J, notAtSameTime(J), ConflictList),
    length(ConflictList, NumConflicts),
    repeat(TimeList, NumConflicts, TimeListRepeat),
    zip(ConflictList, TimeListRepeat, TimesInput),
    map(TimesInput, getTimes, ScheduledTimes),
    filter(ScheduledTimes, areUnique, Results),
    length(Results, L),
    length(ScheduledTimes, L).

getTimes([Sessions, TimeList], Times) :-
    getTimes(Sessions, TimeList, Times).
getTimes(Sessions, TimeList, Times) :-
    sessions(AllSessions),
    length(Sessions, NumSessions),
    repeat(AllSessions, NumSessions, AllSessionRepeat),
    zip(Sessions, AllSessionRepeat, IndexInput),
    map(IndexInput, getIndex, Indices),
    repeat(TimeList, NumSessions, TimeListRepeat),
    zip(Indices, TimeListRepeat, GetAtInput),
    map(GetAtInput, getAt, Times).

% before constraint

beforeCst(TimeList) :-
    findall([K1, K2], before(K1, K2), Before),
    length(Before, NumBefore),
    repeat(TimeList, NumBefore, TimeListRepeat),
    zip(Before, TimeListRepeat, GetTimesInput),
    map(GetTimesInput, getTimes, AllTimePairs),
    all(AllTimePairs, isLess).

% at constraint

atCst(TimeList, RoomList) :-
    findall([L1, L2, L3], at(L1, L2, L3), AtRaw),
    map(AtRaw, convertAtSymToNum, At),
    atCst(TimeList, RoomList, At).

atCst(_, _, []).
atCst(TimeList, RoomList, [[Session, Time, Room]|Rest]) :-
    atCstTime(TimeList, Session, Time),
    atCstRoom(RoomList, Session, Room),
    atCst(TimeList, RoomList, Rest).

atCstTime(TimeList, Session, Time) :-
    nonvar(Time),
    sessions(Sessions),
    getIndex(Session, Sessions, Index),
    getAt(Index, TimeList, T),
    T #= Time.
atCstTime(_, _, Time) :-
    var(Time).

atCstRoom(RoomList, Session, Room) :-
    sessions(Sessions),
    getIndex(Session, Sessions, Index),
    getAt(Index, RoomList, R),
    R #= Room.
atCstRoom(_, _, Room) :-
    var(Room).
    
convertAtSymToNum([Session, TimeSym, RoomSym], [Session, Time, Room]) :-
    roomSymToIndex(RoomSym, Room),
    timeSymToIndex(TimeSym, Time).

roomSymToIndex(RoomSym, RoomNum) :-
    nonvar(RoomSym),
    findall(R, room(R), Rooms),
    getIndex(RoomSym, Rooms, RoomNum).
roomSymToIndex(R, R) :-
    var(R).

timeSymToIndex(TimeSym, TimeNum) :-
    nonvar(TimeSym),
    times(Times),
    getIndex(TimeSym, Times, TimeNum).
timeSymToIndex(T, T) :-
    var(T).

% Exclusive Constraint
% Basically we cant double book a room

exclusiveCst(TimeList, RoomList) :-
    zip(TimeList, RoomList, Schedule),
    areUniquePairs(Schedule).

% various helpers for Question 2

isLess([A, B]) :-
    isLess(A, B).
isLess(A, B) :-
    A #< B.

getAt([Index, List], Res) :-
    getAt(Index, List, Res).
getAt(0, [A|_], A).
getAt(Index, [_|B], Res) :-
    I is Index - 1,
    I >= 0,
    getAt(I, B, Res).

getIndex([Element, List], Result) :-
    nonvar(Element),
    getIndex(Element, List, Result).
getIndex(Element, [Element|_], 0).
getIndex(Element, [_|B], Num) :-
    getIndex(Element, B, N),
    Num is N + 1.

notMemberPair(_, []).
notMemberPair([A, B], [[C, D]|Rest]) :-
    (A #\= C #\/ B #\= D),
    notMemberPair([A, B], Rest).

notMember(A, [B]) :-
    A #\= B.
notMember(A, [B|C]) :-
    A #\= B,
    notMember(A, C).

areUnique([]).
areUnique([_]).
areUnique([A|B]) :-
    notMember(A, B),
    areUnique(B).

areUniquePairs([]).
areUniquePairs([_]).
areUniquePairs([A|B]) :-
     notMemberPair(A, B),
     areUniquePairs(B).

%
% Question 3
%

% Those 3 marks for performance aren't worth much. Just brute force it.
% A smarter way of doing it would be to partition the list into positive and negative numbers
% choose values from the one list or the other based upon whether the sum is positive or negative
% each choice is a branch (choose any of the remaining) and include some logic to prevent duplicate
% calculations. When a list is exhausted and is selected from fail that branch and backtrack. That'sam
% a modest chunk of work for three marks...
% instead set the domain to zero or one and if the value is one pick that number, zero does not pick
% that number. Try all combinations by this mechanism. 2^N calcs performed to get all answers or determine
% there is no answer. Not good but easy to write.

p1(R):-cputime1,!,nl,t1(R),statistics(runtime,[_,0]).
p2(R):-cputime2,!,nl,t2(R),statistics(runtime,[_,0]).
p3(R):-cputime3,!,nl,t3(R),statistics(runtime,[_,0]).
p4(R):-cputime4,!,nl,t4(R),statistics(runtime,[_,0]).
p5(R):-cputime5,!,nl,t5(R),statistics(runtime,[_,0]).
p6(R):-cputime6,!,nl,t6(R),statistics(runtime,[_,0]).
p7(R):-cputime7,!,nl,t7(R),statistics(runtime,[_,0]).
p8(R):-cputime8,!,nl,t8(R),statistics(runtime,[_,0]).
p9(R):-cputime9,!,nl,t9(R),statistics(runtime,[_,0]).
 
t1(R) :- subsetSum([],R).
t2(R) :- subsetSum([-3,1,2,3],R).
t3(R) :- subsetSum([-1,2,3,5],R).
t4(R) :- subsetSum([-1,2,2,3,-1,-4,-4,8,-5,-2],R).
t5(R):-subsetSum([-3,1,2,-2,2,2,3,1,14,-15,20,-21,1,5,4,-1,-9,4,-4,9],R).
t6(R):-subsetSum([-3,1,2,-2,2,2,3,1,14,-15,20,-21,1,5,4,-1,-9,4,-4,9,-10],R).
t7(R):-subsetSum([-3,1,2,-2,2,2,3,1,14,-15,20,-21,1,5,4,-1,-9,4,-4,9,-10,9],R).
t8(R):-subsetSum([-3,1,2,1,2,2,-3,14,-15,20,-21,1,5,4,-1,-9,4,-4,9,-9,9,-15,8],R).
t9(R):-subsetSum([3,1,2,1,2,2,14,20,-520,1,5,4,6,9,4,4,9,9,8,9,23,43,56,12,34,32,45,54,90],R).

cputime1:- t1(W),!,statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime1 :- statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime2:-t2(W),!,statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime2 :- statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime3 :-t3(W),!,statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime3 :- statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime4 :-t4(W),!,statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime4 :- statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime5 :-t5(W),!,statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime5 :- statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime6 :-t6(W),!,statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime6 :- statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime7 :-t7(W),!,statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime7 :- statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime8 :-t8(W),!,statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime8 :- statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime9 :-t9(W),!,statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').
cputime9 :- statistics(runtime,[_,X]),T is X/1000,write('run time: '),write(T), write(' sec.').

subsetSum(S, Res) :-
     findall(B, subsetSum_h(S, B), AllRes),
     rmDup(AllRes, ResNoDup),
     member(Res, ResNoDup).

subsetSum_h(S, Res) :-
     length(S, L),
     length(Selector, L),
     domain(Selector, 0, 1),
     zip(S, Selector, MapInput),
     map(MapInput, subsetSumSelector, FullSet),
     filter(FullSet, notZero, Set),
     length(Set, NumSet),
     \+ NumSet is 0,
     reduce(Set, add, 0),
     labeling([], Selector),
     insertSort(Set, sortPred, Res).

subsetSumSelector([Num, S], Num) :-
     S #= 1.
subsetSumSelector([_, S], S) :-
     S #= 0.

sortPred(A, B) :-
     A > B.

notZero(Val) :-
     \+ Val is 0.

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
zip([], [], []).
zip([A|B], [C|D], [[A, C]|E]):-
    zip(B, D, E).

zip([], [], [], []).
zip([A|B], [C|D], [E|F], [[A, C, E]|G]):-
    zip(B, D, F, G).

% simple map function that operates over all elements in a list to create a new list
map([], _, []).
map([A|B], Pred, [C|D]) :-
    call(Pred, A, C),
    map(B, Pred, D).

filter([], _, []).
filter([A|B], Pred, [A|C]) :-
    call(Pred, A),
    filter(B, Pred, C).
filter([A|B], Pred, Out) :-
    \+ call(Pred, A),
    filter(B, Pred, Out).

count(A, Pred, Res) :-
    filter(A, Pred, B),
    length(B, Res).

% simple helper to reduce with a predicate
reduce([A], _, A).
reduce([A|B], Pred, Result) :-
    reduce(B, Pred, R),
    call(Pred, R, A, Result).

rmDup([], []). % cannot remove duplicates from an empty list
rmDup([A|B], [A|C]) :- % keep A if it is not a member of B
    \+ member(A, B),
    !,
    rmDup(B, C).
rmDup([A|B], S2) :- % do not keep A if it is a member of B
    member(A, B),
    !,
    rmDup(B, S2).

all([], _).
all([A|B], Pred) :-
    call(Pred, A),
    all(B, Pred).

% checks if a value is a list
isList([_|_]).
isList([]).

% sorting function from the eClass Forums
% Adapted to support predicates
% used the insertion sort for simplicity as data is not expected to be large
insertSort(List, Pred, Sorted) :-
    i_sort(List, Pred, [], Sorted).

i_sort([], _, Acc, Acc).
i_sort([H|T], Pred, Acc, Sorted) :-
    insert(H, Pred, Acc, NAcc),
    i_sort(T, Pred, NAcc, Sorted).

insert(X, Pred, [Y|T],[Y|NT]) :- 
    call(Pred, X, Y),
    insert(X, Pred, T, NT).
insert(X, Pred, [Y|T], [X,Y|T]) :-
    \+ call(Pred, X, Y).
insert(X, _, [], [X]).
