Assignment 2
Due Friday March 6 2015, at 11:55pm.

-----------------------------------------------------------------------

Note:

The due date here is one week later than the tentative schedule in the course outline, considering busy exam week and the break of Reading Week, thus some students may need more time to complete this assignment.

This delay will result in overlapping of this assignment with the introduction lab to Prolog.  Thus, in the labs of week March 2-6, while a TA will introduce Prolog, another TA will be available in the last 50 minutes of the lab session, to answer questions regarding this assignment.  See "Consulting Hours" on the course website for the details of TA consulting schedule for this assignment.

--------------------------------------------------------------------

Put all your programs in one file and submit it via moodle as you did for assignment one.

In this assignment we will implement an interpreter for the simple functional programming language introduced at the beginning of this semester. Let's call the language FL.

This assignment is worth 15 assignment marks.

Restrictions: Review the allowable built-in Lisp functions and special forms.

Note that during the development of your program, one way to avoid typing a test case repeatedly is to use setq; see examples.

Overview
A program in FL is a collection of function definitions. The interpreter we are to implement takes such a collection of function definitions and a function application, and returns the result of evaluating the application. The evaluation of a function application is based on the principle of "replacing equals by equals". For this purpose, you should define a Lisp function

(interp E P)

which, given a program P and an expression E, returns the result of evaluating E with respect to P.

The language of FL includes a number of primitive functions that should be implemented in your interpreter. Essentially, we are to implement a variant of a subset of Lisp without an environment--we don't have a special form "defun" in FL.

Syntax of FL
When we write it on paper, a function definition is typically written as

f(X1,...,Xn) = Exp

where f is a function, (X1,...,Xn) is a list of parameters, and Exp denotes the body of the function; i.e., it defines what the function does. 

In this assignment we will use lists to represent function definitions as well as function applications.

A function definition is an expression

(f X1 ... Xn = Exp)

where f is the name of the function, X1 ... Xn are the parameters, and Exp the definition of the function. The left hand side of = is called the header of the function and Exp the body of the function.

Note that, if a function f has no parameters, we write

(f = Exp)

In this assignment, we do not consider higher-order functions.

A program is a list of such definitions. Review some examples. A function application takes the form

(f e1 ... en)

where e1, ..., en are called actual parameters (or arguments).

To simplify this assignment, we don't use the quote function in FL. In an application, any symbol in the place of an argument is considered  a piece of data, and in a list if the first element is not a defined function, the list is considered an input list of elements. For example, consider the following call to your interpreter

(interp '(xmember a (b c d a)) '((xmember X L = ...)))

where suppose we defined a function, called xmember, for membership testing, i.e., whether X is in L. Then, the evaluation of the expression

(xmember a (b c d a))

is such that the first occurrence of a is considered input data (as if it is quoted in Lisp), so is the list (b c d a), as b is not a defined function. 

Of course, a function should not be defined more than once.  To avoid possible confusion, we will use X,Y,Z,M,L for parameters (variables). Apparently, a variable that appears in the body of the function must also appear as a parameter in the header of the function (i.e., no free variables are allowed). Practically, these are something you do not need to worry about, since we will not use invalid programs/calls to test your interpreter. For example, we do not test your program by calling an undefined function.

Primitive Functions of FL
The following primitive functions must be implemented. The meanings of these functions are the same as those in Lisp, where first and rest are identified with car and cdr, respectively.

(if x y z)
(null x)
(atom x)
(eq x y)
(first x)
(rest x)
(cons x y)
(equal x y)
(number x)   /* true if x is a number, same as lisp (numberp x)

(+ x y)
(- x y)
(* x y)
(> x y) 
(< x y) 
(= x y) 
(and x y)
(or x y)
(not x)
In addition, as in Lisp,  we use the atom NIL for the truth value false, and anything else represents true.

Interpreter
The evaluation of an application is by "replacing equals by equals". That is, for an application (f e1 ... en), find the definition of the n-ary function f, (f X1 ... Xn = Body), in the given program, and replace (f e1 ... en) by Body with all occurrences of the parameters replaced by their corresponding arguments.

Note that a function is identified by its name and arity. If you have a definition, say (f X Y = (cons X Y)), it cannot be used to evaluate an expression, say (f 5).

An evaluation step described above is called a reduction. Reduction is performed repeatedly until no further reductions are possible. An expression that is not reducible is called a normal form. So, the goal of an interpreter for FL is to reduce a given expression to a normal form using the definitions in the given program.

As said above, you should write a Lisp function interp such that given an expression E and a program P,

(interp E P)

returns the result of evaluating E with respect to P.

You should implement your interpreter based on applicative order reduction. See orders of reductions for the interpreter to be implemented here. Review more examples.

Finally, we provide a more detailed example of reduction here.

Notes: One exception to the applicative order reduction is the IF function, for which the condition should be evaluated first, and according to the result, continue either with the THEN part or the ELSE part. Another is the Boolean functions AND and OR; always evaluate the first argument, and according to the result, either stop or continue with the second argument. E.g. with (AND E1 E2), if E1 evaluates to FALSE, stop and return FALSE without evaluating E2.

 

Marking Guide
Here is how we are going to determine partial marks.

1. [5 marks]

Your interpreter works for primitive functions. We will use the call pattern

(interp Exp nil)

to check on this. Review some examples.

2. [10 marks]

Your interpreter works for user-defined functions. This is the major part of this assignment. We will use the call pattern

(interp Exp P)

where P is nonempty to test your interpreter. We will develop a variety of test cases for this purpose.

Hints: This assignment may look a bit intimidating. But it should not be the case. The main function could be coded well within a couple of pages plus some utility functions. However, you should start early, since it takes time to study the language to be implemented. You may start from this skeleton program.