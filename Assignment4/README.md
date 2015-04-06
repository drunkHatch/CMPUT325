Due Date: Friday April 10th, 2015, 11:55pm.

This assignment is worth 20 marks. That is, it weighs a little more than the other assignments.

In this assignment,  to solve Questions 2 and 3, you must use the clpfd library by including the code.

:- use_module(library(clpfd)).

Note that you must actually use the constraint solver for clpfd to solve Q2 and Q3.  That is, a pure Prolog solution is not acceptable. 

For convenience, you are free to use the lists library for all problems. 

Question 1 (6 marks)

Suppose we have a database of student marks for cmput 325, in the form of a relation with attribute names 
       

         c325(Semester, Name, as1, as2, as3, as4, midterm, final)

There are some facts about the setup of course components, in the form 

        setup(Semester,Type,Max,Percentage)

where Type is one of {as1,as2,as3,as4,midterm,final}, Max is the maximum marks for Type and percentage is the weight of Type in the course. 

The following data is used for testing purposes:

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

(a) Define a predicate:

              query1(+Semester, +Name, -Total)

Given a semester and a student name, Total should be bound to the total mark, in terms of percentage out of 100, of the student for that semester. 

Test case:

?- query1(fall_2014, kim, X).
X = 81.88333333333 ?;
no


(b)  Define a predicate:

        query2(+Semester, -L).

Given a semester, find all students whose final exam shows an improvement over the midterm, in the sense that the percentage obtained from the final is (strictly) better than that of the midterm. 

Test case: 
?- query2(fall_2014, X).
X = [aperf,ken,kris] ? ;

no

(c) Define a predicate:

          query3(+Semester,+Name,+Type,+NewMark)

Updates the record of Name for Semester where Type gets NewMark. If the record is not in the database, print the message "record not found".

Test cases: 
?- query3(fall_2014, kim, final, 5).
yes

?- query3(fall_2014, jim, as1, 3).
record not found
yes

Question 2 (6 marks)

The organizers of a workshop need to book a number of rooms, which can be given by facts like

     room(r1).

     room(r2).

     .......

for a 2 days workshop, which consists of 11 half-day sessions.  Let's name the sessions by a,b,..., k, and the half-days by firstDayAm, firstDayPm, secondDayAm, and secondDayPm. In scheduling the workshop, some constraints must be satisfied.  Some sessions cannot be held at the same time. This is given by facts like

     notAtSameTime([b,i,h,g]) .

meaning that no sessions in [b,i,h,g] may be held at the same time. A session may need to take place before another session, given by facts like

     before(i,j).

meaning that i should precede j. A session may need to be placed at a particular time and/or in a particular room; the information is given by, e.g.,

     at(a,firstDayPm,_).

which means that the session a must take place at firstDayPm, in any room.

Write a program, such that given a collection of facts like above,  and a number of rooms as described at the beiginning, your program generates all solutions (one at a time), and if a solution exists, otherwise the message "cannot be scheduled" should be shown.

We can use a list of variables [A,B,C,...] to represent sessions, where two pieces of information are associated with each session, time and place. The representation of a solution is simpler if we use two lists of variables, one for times and the other for rooms. Then, we write constraints that must be satisfied, w.r.t. one of these lists, or both, depending on the constraint.

Your program will be invoked by a call

?- schedule(TimeLst, RmLst).

TimeLst = [......]

RmLst = [......]

where the first element in TimeLst is the time for session a and the first element in RmLst is the room for session a, and so on.  We can sketch the solution as


 schedule(TimeLst,RmLst) :-             % TimeLst as times assigned to sessions a, b, c, ...
 TimeLst = [A,B,C,D,E,F,G,H,I,J,K],     % RmLst as rooms assigned to sessions a, b, c, ...
 length(TimeLst,Len),
 length(RmLst,Len),                     % generate RmLst
 append(TimeLst,RmLst, W),              % will label all variables in W
 findall(L, notAtSameTime(L), C1),      % collect all such L into C1
 findall([Q1,Q2],before(Q1,Q2),C2),     % collect all before constraints into C2
 findall([Session,Time,Rm],at(Session,Time,Rm), C3),  
                                        %collect all "at constraints" into C3
 ......
 domain(TimeLst, 1,4) ,  % let's denote 1 for firstDayAm, 2 for firstDayPm, and so on
 domain(RmLst, 10, ...), % let's represent room r1 by 10, r2 by 11, and so on to avoid                         % mixing up with the representation of the half-days. 
                         % You should determine what ... is 
 ......
 constr1(TimeLst,C1),          % satisfy all "notAtSameTime constraints"
 constr2(TimeLst,C2),          % satisfy all "before constraints"
 constr3(TimeLst,RmLst,C3),    % satisfy all "at constraints"
 exclusive(TimeLst,RmLst),   
            % No sessions assigned to the same time can be in the same room
 ......     % No sessions assigned to the same room can be at the same time
 

 labeling([],W).



Question 3 (8 marks): The subset sum problem

Here is how Wikipedia describes the problem:  In Computer Science, the subset sum problem is an important problem in complexity theory and cryptography. The problem is this: given a set (or multiset) of integers, is there a non-empty subset whose sum is zero? For example, given the set {-7, -3, -2, 5, 8}, the answer is yes because the subset {-3, -2, 5} sums to zero. The problem is NP-complete.

Define a predicate

    subsetSum(+L, -Result)

where L is a list of integers (which represents a multiset where an element may repeat) and, Result should be bound to a subset of L whose sum is zero, if such a query is answered positively.  Your program should provide all such subsets when user asks for alternative answers.

We will use small instances to test the correctness of your program.  The total marks for this part is 5.

The next 3 marks are reserved for performance. We will design a few instances and run your program on them.  In each call, we will set up the time limit to 30 seconds.  The full 3 marks will be awarded to the "winners", the programs that are obviously faster than the rest.  We will determine what "obviously faster" means when we actually run your programs. The idea is that there should be some obvious performance gap between this group and the rest. We may award a program with less than 3 but greater than 0 marks, if we see it appropriate. 

Later, TAs will post some instances and tell you how TAs' program performs.