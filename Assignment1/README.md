Assignment 1

This assignment is due Jan 30th, 2015, at 23:55pm. This assignment should be submitted as a single text file. The file name should be  (your ID#).l . An example of a submission file might be 1234567.l

Each question should be preceded with a comment line clearly indicating that the code for that question is starting. Minor functions can be recycled for use in later questions. Please note you must still follow the programming style and marking guideline in regards to documenting your work.

The beginning of the file you hand in should look something like this:


;QUESTION 1
;documentation
(defun filterGPA (L N)
...
)
;documentation
(defun minor_helper_function_for_question_1 (...)
...
)
...
;QUESTION 2
;(question 1 should not use any functions beyond this point)
...
etc

Assignment Marks:
This assignment is worth 15 assignment marks. Your programs must be readable and understandable as well as correct. 

Restrictions:

Only the following built-in Lisp functions and special forms may be used:

(atom x)
(null x)
(eq x y)
(equal x y)
(numberp x)
(append x y)
(car x)
(cdr x)
(cons x y) 
(if x y z)
(cond ... ) 
(let ((x y) (u v)) z)
(let* ((x y) (u v)) z)
(defun ...)
(quote x) and its short form 'x
(print ...)
(list ...)
(sort L fun) % this is needed for the last problem
(string-lessp x y)  
(string= x y)
(string< x y)
(funcall  ...)
(apply  ...) 
(function ...) 
and numeric operators and comparisons, and logic connectives such as

(+ x y)
(- x y)
(* x y)
(/ x y)
(< x y)
(> x y)
(= x y)
(<= x y)
(>= x y)
(and x y)
(or x y)
(not x)
You may also use a combination of car and cdr, such as

(cadr ...), (cdaar ...)
etc.

You may write one or more functions to solve any given problem below. In some cases it is desirable to decompose a problem into some smaller ones. However, if a problem has a straightforward solution, it's a bad idea to solve it in a complex way by decomposition.

 

 

#1 (1 mark)
Define a function

(defun form-pair (L) ...)

which forms a list of pairs from the elements in a given list, in the way that the first two elements form the first pair, in the reversed order, and the next two form the second pair in the reversed order, and so on.  If the given list is even, you will get a list of proper pairs;  if it contains an odd number of elements, the last element should be paired with itself.

Example.

(form-pair '(a1 b 2 c 3)) ==> ((1 a) (2 b) (3 c))

(form-pair '(a1 b 2 c 3 d)) ==> ((1 a) (2 b) (3 c) (d d))

#2 (1 mark)
Write the Lisp function:

(drop-pair L)

where L is a list of pairs, each of which consists of two arithmetic expressions; the function returns the list with all expressions evaluated; in addition, if the two expressions in a pair evaluate to the same value, the pair will be dropped from the resulting list.

Example.

(drop-pair '(((+ 2 4) (* 3 4)) (6 (* 2 3)))) ==> ((6 12))

 

#3 (1 mark)
Write the Lisp function:

(remove-duplicate x)

It takes x as a list of atoms and removes repeated ones in x. The order of the elements in 
the resulting list should preserve the order in the given list.

Example.

(remove-duplicate '(a b c a d b)) ==>

(a b c d)

or

(c a d b)

Note that in both the order in the given list is preserved.

 

#4 (1 mark)
 Suppose we want to define a function my-count that counts the distinct atoms in a given list (without nesting). E.g.

> (my-count '(a b a c d c))
4
There are at least two ways to do this. The first one is to remove repeated atoms by the function defined in #3, and then do the counting on the resulting list.

(defun my-count (L)
    (count0 (remove-duplicate L))
)
(defun count0 (L)
    (if (null L)
        0
        (+ 1 (count0 (cdr)))
    )
)
The main idea in the second approach is to carry out the duplicate checking on the fly. Given L, if (car L) is in (cdr L) then we can simply drop (car L) in our counting.  This is more efficient in that we go through L only once. In this question, you are asked to solve the problem based on this second approach.

#5 (2 marks)
A list without duplication may be viewed as a set.  Define a function

            (power-set  L)

where L is a list of elements without duplication;  the function returns the power set of L.

For example,

(power-set '(a b c))  ==> (() (a) (b) (c) (a b) (a c) (b c) (a b c))

To make your work a bit easier, the resulting list need not be ordered in any certain way, as long as it contains all the correct sets)

#6 (4 marks)
6a) Let L be a list of triples each of which consists a student name, a course number, and a grade.  Given a student name and such a list L, we want to find out the best grade and the course of the student, similarly the worst grade and the course of the student, depending on an input parameter. If there is more than one such course, then return any of them.  Since two pieces of information need to be returned, you should place them into a list.

Define a function

(defun findOne (Name Type L)  ......)

where Name is a student name, Type is either the word best or the word worst, and L is a list of such triples specified abve. The function returns a list consisting of a course number and a grade as specified above.

For example, suppose

L = ((john cmput201 A-) (lily cmput114 A) (ann cmput115 B) (john cmput229 C+) (lily cmput115 B-) (john cmput325 A+) (lily cmput229 A))

(e.g., In testing, one can use setq to bind L with the list.)

Then,

(findOne 'john 'worst L) ==> (cmput229 C+)

(findOne 'john 'best L) ==> (cmput325 A+)

(findOne 'lily 'best L) ==> (cmput114  A)  or (cmput229 A)

 

6b) Now, we want to find out all the courses taken by a student and the grades, and have the resulting list sorted in increasing order on course numbers.

Define a function

(defun findAll (Name L) ...)

For example, with the same list L above, we get

(findAll 'lily L) ==> ((cmput114 A) (cmput115 B-) (cmput229 A))

For the use of the sort function, see the next questoin. We need to use the built-in function string-lessp.

#7 (5 marks)
A web page A containing a link to another one B is represented by a pair (A B). 
Given a list L of such pairs, write two Lisp functions:

(reached x L)
where x is a web page, L is a list of pairs representing linkage, and the function returns a list of all web pages that can be reached from x, but excluding x itself. The order of the web pages in the resulting list is unimportant.

Example.

>(reached 'a '((a b) (b c) (b e) (p b)))
(b c e)
>(reached 'a '((e p) (b c) (a b) (c a) (c b)))
(b c)
>(reached 'a '((a a)))
nil 

The importance of a web page could be determined by how many other web pages refer to it. A web page A is said to refer to another web page B iff A contains a link to B, and A and B are not the same web page (i.e., a web page referring to itself doesn't count). Define a function

(rank S L)
where S is a list of atoms naming web pages, and L is a list of pairs representing linkage. The function returns a permutation of S such that the web pages are ordered according to the criterion above, i.e., the most referred web page is the first in the list, and so on. If two web pages are equally important in terms of references, then it doesn't matter how they are ordered.

Example. 
>(rank '(a b p) '((a b) (b c) (b a) (p b)))
(b a p)

>(rank '(a b) '((a a) (b a) (q b) (a b)))
(b a)

(a b) would be incorrect as (a a) doesn't count as a reference to a.

Hint: Count the number of references to each atom in S to get a list, say

((Cmput325 23) (UofA 128) (CSD 68)) 
Then, you can tailor the built-in function sort for your own needs, for example, by defining

   (defun mySort (L)
      (sort L 'greaterThan))

   (defun greaterThan (L1 L2)
       (> (cadr L1) (cadr L2)))
This will give you, for the above example,

((UofA 128) (CSD 68) (Cmput325 23))
from which you can get the final result

(UofA CSD Cmput325)