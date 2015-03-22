xor(A, B) :- A, \+B;
             \+A, B.

matchOne(_, _, []).
matchOne(S1, S2, [A|B]):-
    xor(member(A, S1),
	member(A, S2)),
    matchOne(S1, S2, B).

% Question1
% Works by taking in 2 lists (S1, S2) and
% S3 will be the elements that are in one list
% or the other but not both.
% Note: I realize this is a horrible implementation, this was my learning exercice for prolog,
% basically it generates all possible subsets of the union of both lists and filters for one that
% matches the criteria, not a good solution.
setDifference(S1, S2, S3) :-
    unionOrSubsetOfUnion(S1, S2, S3), % creates a set of possible answers, of which some are correct
    matchOne(S1, S2, S3), % filters to select only the ones that have all elements in S3 be in either S1 or S2 but not both.
    !. % Cut prevents multiple values from being returned.

% helper that generates the union of its input values and all subsets
unionOrSubsetOfUnion([], [], []).

unionOrSubsetOfUnion([A|B], S2, [A|C]) :-
    \+ member(A, S2),
    unionOrSubsetOfUnion(B, S2, C).
unionOrSubsetOfUnion(S1, [A|B], [A|C]) :-
    \+ member(A, S1),
    unionOrSubsetOfUnion(S1, B, C).

unionOrSubsetOfUnion([_|B], S2, S3) :-
    unionOrSubsetOfUnion(B, S2, S3).
unionOrSubsetOfUnion(S1, [_|B], S3) :-
    unionOrSubsetOfUnion(S1, B, S3).


%
% Question 2
%

swap([], []).
swap([A], [A]).
swap([A, B |T1], [B, A |T2]) :-
    swap(T1, T2).

%
% Question 3A
%

rmDup([], []).
rmDup([A|B], [A|C]) :-
    \+ member(A, B),
    !,
    rmDup(B, C).
rmDup([A|B], S2) :-
    member(A, B),
    !,
    rmDup(B, S2).


%
% Question 3B
%

rmAllDup(S1, S2) :-
    flatten(S1, L1),
    rmDup(L1, ElementList),
    rmAllDup(S1, S2, ElementList, _),
    !.

rmAllDup([], [], [], []).
rmAllDup([], [], X, X).
rmAllDup([A|B], [A|C], ElementList, NewElementList):-
    xatom(A),
    member(A,ElementList),
    rmItem(ElementList, A, L),
    rmAllDup(B, C, L, NewElementList).
rmAllDup([A|B], S2, ElementList, NewElementList):-
    xatom(A),
    rmAllDup(B, S2, ElementList, NewElementList).
rmAllDup([A|B], [C|D], ElementList, NewElementList):-
    isList(A),
    rmAllDup(A, C, ElementList, L),
    rmAllDup(B, D, L, NewElementList).
    

%
% Helpers
%

% Finds an item in a list or any of its sub lists.
member_recurse(T, [A|B]) :-
    (\+ isList(A),
     member(T, [A|B]));
    (isList(A),
     member_recurse(T, A));
    member_recurse(T, B).
     

intersect([], _, []).
intersect([A|B], S2, [A|C]) :-
    member(A, S2),
    !,
    intersect(B, S2, C).
intersect([_|B], S2, S3) :-
    intersect(B, S2, S3).

flatten([],[]).
flatten([A|L],[A|L1]) :- 
     xatom(A), flatten(L,L1).
flatten([A|L],R) :- 
     flatten(A,A1), flatten(L,L1), append(A1,L1,R).

xatom(A) :- atom(A).
xatom(A) :- number(A).

rmItem([], _, []).
rmItem([Item|T], Item, S2) :-
    rmItem(T, Item, S2),
    !.
rmItem([A|B], Item, [A|C]) :-
    rmItem(B, Item, C),
    !.

isList([_|_]).
isList([]).
