
%
% Jesse Tucker
% 1255723
% March 22nd
% CMPUT 325
% Assignment #3
%

%
% Question1
%

setDifference([], _, []). % Empty array is empty irrespective of other set.
setDifference([A|B], S2, [A|C]) :- % include A if and only if it is not a member of S2
    \+ member(A, S2),
    setDifference(B, S2, C),
    !.
setDifference([_|B], S2, S3) :- % Previous rule failed, ignore A.
    setDifference(B, S2, S3),
    !.


%
% Question 2
%

swap([], []). % zero elements cannot be swapped
swap([A], [A]). % a single element cannot be swapped
swap([A, B |T1], [B, A |T2]) :- % Lists of size 2 or larger can have their elements swapped
    swap(T1, T2).

%
% Question 3A
%

rmDup([], []). % cannot remove duplicates from an empty list
rmDup([A|B], [A|C]) :- % keep A if it is not a member of B
    \+ member(A, B),
    !,
    rmDup(B, C).
rmDup([A|B], S2) :- % do not keep A if it is a member of B
    member(A, B),
    !,
    rmDup(B, S2).


%
% Question 3B
%

rmAllDup(S1, S2) :- % removes all duplicate atoms from S1 and returns the result in S2
    flatten(S1, L1), % generate a list of all elements by flattening the list
    rmDup(L1, ElementList), % and then ensuring the list is a set
    rmAllDup(S1, S2, ElementList, _),
    !.

rmAllDup([], [], [], []). % an empty list of remaining elements and remaining items results in both being empty lists
rmAllDup([], [], X, X). % if the list is empty but their are elements left to find the remaining elements are returned
rmAllDup([A|B], [A|C], ElementList, NewElementList):- % If A is in the element list then include it
    xatom(A),
    member(A,ElementList),
    rmItem(ElementList, A, L), % remove the element from the list and continue, L contains only the elements that still need to be found
    rmAllDup(B, C, L, NewElementList).
rmAllDup([A|B], S2, ElementList, NewElementList):- % skip atoms that have already been found
    xatom(A),
    rmAllDup(B, S2, ElementList, NewElementList).
rmAllDup([A|B], [C|D], ElementList, NewElementList):- % Lists need to be recursed through
    isList(A),
    rmAllDup(A, C, ElementList, L), % make sure to pass the remaining elements on to the next call of rmAllDup
    rmAllDup(B, D, L, NewElementList).

%
% Question 4
%

generate(S1, Choice, Val) :-
    flatten(S1, L),
    rmDup(L, Vals), % no point searching duplicates, get the list down to a set and search that
    generate_h(Vals, Choice, Val).
generate_h(Vals, smallest, Val) :-
    min(Vals, Val). % return the min of the set
generate_h(Vals, largest, Val) :-
    max(Vals, Val). % return the max of the set

min([A|B], A) :- % lists of length 1 must have the min as the only value
    length([A|B], L),
    L == 1.
min([A|B], A) :- % A is the minimum if it is less than the minimum of the rest of the list
    min(B, Min),
    A < Min,
    !.
min([_|B], Min) :- % if the prev rule failed the first value was not the min
    min(B, Min),
    !.

max([A|B], A) :- % A must be the max value if the list has only one element
    length([A|B], L),
    L == 1.
max([A|B], A) :- % A is the max if it is greater than the max of the rest of the list
    max(B, Max),
    A > Max,
    !.
max([_|B], Max) :- % if the prev rule failed then the front of the list is not the max
    max(B, Max),
    !.

%
% Question 5
%

countAll(S, R) :-
    flatten(S, FlatList),
    rmDup(FlatList, EleList), % create a set of all the elements in the list
    length(EleList, L),
    repeat(S, L, S2),
    zip(EleList , S2, S3), % generate a set of pairs of [element, list]. These will be mapped into [element, count].
    map(S3, S4, countInstances), % map each pair into elements and counts.
    insertSort(S4, comparePair,  R), % sort the results by their count and the element name
    !.

comparePair([_, C1], [_, C2]) :- % predicate for sorting results
    C1 < C2. % sort by counts
comparePair([E1, C], [E2, C]) :- % if counts are the same sort by element name
    E1 @> E2.

countInstances([], _, 0). % only zero elements in an empty list
countInstances([A|B], A, C1) :- % if the head and the target are the same count 1 more
    countInstances(B, A, C2),
    C1 is C2 + 1,
    !.
countInstances([_|B], A, C) :- % do not increment count if the prev rule failed
    countInstances(B, A, C),
    !.

countInstances([Element, List], [Element, R]) :- % helper for map to expand the parameter list
    flatten(List, FlatList),
    countInstances(FlatList, Element, R).

%
% Question 6
%

convert([], []). % an empty list must convert to an empty list
convert([q|B], [q|C]) :- % an unmatched quote can just be ignored and parsing can continue normally 
    \+member(q, B),
    convert(B, C), !.
convert([q|B], [q|C]) :- % matched quotes must us the inQuotes parsing rule
    member(q, B),
    convert(B, C, inQuotes), !.
convert([e|B], C) :- % empty space not in quotes can be ignored
    convert(B, C), !.
convert([_|B], [c|C]) :- % all characters other than e and q that are outside quotes are a c.
    convert(B, C), !.
convert([q|B], [q|C], inQuotes) :- % if we encounter the end quote included it and stop parsing in quotes.
    convert(B, C), !.
convert([A|B], [A|C], inQuotes) :- % a character encountered in quotes is left unchanged
    convert(B, C, inQuotes), !.

%
% Helpers
%

% borrowed from eClass
flatten([],[]).
flatten([A|L],[A|L1]) :- 
     xatom(A), flatten(L,L1).
flatten([A|L],R) :- 
     flatten(A,A1), flatten(L,L1), append(A1,L1,R).

xatom(A) :- atom(A).
xatom(A) :- number(A).

% simple helper for removing all instances of an element from a list
rmItem([], _, []).
rmItem([Item|T], Item, S2) :-
    rmItem(T, Item, S2),
    !.
rmItem([A|B], Item, [A|C]) :-
    rmItem(B, Item, C),
    !.

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

% simple map function that operates over all elements in a list to create a new list
map([], [], _).
map([A|B], [C|D], Pred) :-
    call(Pred, A, C),
    map(B, D, Pred).

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
insert(X, _, [Y|T], [X,Y|T]).
insert(X, _, [], [X]).
