;;; CMPUT 325 LEC B1
;;; Assignment 1
;;; Jesse Tucker
;;; jtucker@ualberta.ca
;;; 1255723
;;;
;;; Note : Run (test-all) to execute all of the test cases
;;;

(defun built-in-funcs ()
  "The list of built in functions that all programs have access to.
   The format of a function definition is (name arity body)"
  (list (list 'if 3 (lambda (cnd s1 s2)(if cnd s1 s2)))
	'(null 1 null)
	'(atom 1 atom)
	'(eq 2 eq)
	'(first 1 car)
	'(rest 1 cdr)
	'(cons 2 cons)
	'(equal 2 equal)
	'(number 1 numberp)
	'(+ 2 +)
	'(- 2 -)
	'(* 2 *)
	'(> 2 >)
	'(< 2 <)
	'(= 2 =)
	(list 'and 2 (lambda (a b) (and a b)))
	(list 'or 2 (lambda (a b) (or a b)))
	'(not 1 not)
	(list 'assert 2 (lambda (cnd msg) (assert cnd () msg))))
)

(defun find-func (name arity definitions)
  "Given a function and an arity find the matching function definition.
   If none are found return nil"
  (cond ((null definitions) nil)
	((and (eq name (caar definitions)) (eq arity (cadar definitions))) (car definitions))
	(T (find-func name arity (cdr definitions))))
)

(defun eval-normal-func (func-def args all-definitions stack)
  "Execute a 'normal' function. These are functions that have no special considerations
   and can have their arguments evaluated before the function call. Ie. All applicative
   order function calls."
  (let ((body (caddr func-def))
	(params (mapcar (lambda (arg) (eval-statement arg all-definitions stack)) args))
	(isUserDefined (cadddr func-def)))
    (if isUserDefined
	(apply body (cons all-definitions params))
	(apply body params)))
)

(defun eval-special-func (func-def args all-definitions stack)
  "Execute a 'special' function. These are predefined functions that cannot have all of
   their arguments evaluated in applicative order. Instead the first argument is evaluated
   and the function is evaluated with the first arg simplified and the remaining args un-evaluated.
   Upon returning the result is the evaluated. In practice this only works for 'and', 'or', and 'if'.
   Additionally this does not support a variable number of arguments for 'or' or 'and', only 2 parameters
   are supported as per the assignment spec."
  (let ((body (caddr func-def))
	(params (cons (eval-statement (car args) all-definitions stack) (cdr args))))
    (eval-statement (apply body params) all-definitions stack)) ; Note: special functions cannot be user defined
)

(defun get-param-from-symbol (symbol stack)
  "Search the stack for a symbol. If a symbol is found return the value it is mapped to.
   Otherwise return the symbol as it was provided."
  (if (null stack)
      symbol
      (if (eq (caar stack) symbol)
	  (cadar stack)
	  (get-param-from-symbol symbol (cdr stack))))
)

(defun is-special-func (func-def)
  "Returns true if this function cannot use applicative order evaluation. Basically if it is 'if', 'and' or 'or'."
  (contains (car func-def) '(if and or))
)

(defun eval-statement (statement definitions stack)
  "Meat and Potatoes of the interpreter. Evaluates any provided statement with respect
   to the provided definitions list and the provided stack of symbols. Execution is as follows:
   Atoms are returned unmodified unless they are in the stack, atoms in the stack have their mapped
   value returned.
   Lists that do not start with a function name have each of their values evaluated and the evaluated
   list is returned.
   Lists that do start with a function are evaluated with respect to the function, the additional values
   in the list are assumed to be parameters to the function."
  (if (atom statement)
       (get-param-from-symbol statement stack)
       (let* ((name (car statement))
	      (args (cdr statement))
	      (func-def (find-func name (my-count args) definitions)))
	 (cond ((null func-def) (mapcar  ; it must be a list, eval everything in it
				 (lambda (element) (eval-statement element definitions stack)) 
				 statement))
	       ((is-special-func func-def) (eval-special-func func-def args definitions stack))
	       (T (eval-normal-func func-def args definitions stack)))))
)

(defun extract-params (func)
  "Given a function definition of the form (name param1 param2 param3 ... = body)
   extract the parameters and return them. Ie. return (param1 param2 param3)"
  (if (eq (car func) '=)
      nil
      (cons (car func) (extract-params (cdr func))))
)

(defun extract-body (func)
  "Given a function definition of the form (name param1 param2 param3 ... = body)
   return body."
  (if (eq (car func) '=)
      (cadr func)
      (extract-body (cdr func)))
)

(defun create-sym-map (symbols args)
  "Given a list of symbols create a set of pairs (a mapping) of those symbols onto values.
   Ie. symbols = (x y z) args = (1 5 9) return ( (x 1) (y 5) (z 9))"
  (if (null symbols)
      nil
      (cons (list (car symbols) (car args)) (create-sym-map (cdr symbols) (cdr args))))
)

(defun create-definition (func)
  "Given a function of the form (name param1 param2 ... body) create a function definition
   with a callable lambda that corrosponds to the code in body."
  (let ((name (car func))
	(symbols (extract-params (cdr func)))
	(body (extract-body func)))
    (let ((func-call (lambda (definitions &rest args)
		       (eval-statement body definitions (create-sym-map symbols args)))))
      (list name (my-count symbols) func-call T)))
)

(defun load-definitions (program)
  "Take the user defined program and create function definitions for the program. Then
   merge that list with the built in functions to create the program definitions."
  (merge-lists (mapcar 'create-definition program) (built-in-funcs))
)

(defun interp (args program)
  "Program entry point. Accepts a program and a statement that is to be executed with respect
   to the program."
  (let ((defs (load-definitions program)))
    (eval-statement args defs '()))
)

(defun my-count (list)
  "Utility function that counts the number of elements in a list."
  (if (null list)
      0
      (+ 1 (my-count (cdr list))))
)

(defun merge-lists (list1 list2)
  "Simple utility that combines 2 lists into 1 list"
  ; append list1 to list2
  (if (null list1)
      list2
      (cons (car list1) (merge-lists (cdr list1) list2)))
)

(defun contains (val list)
  "Simple utility for checking if a list contains an element"
  (if (null list)
      nil
      (if (eq val (car list))
	  T
	  (contains val (cdr list))))
)

(defun ta-tests () 
  "Reformatted tests provided by TA on eclass forum"
  ; built-in evaluation tests
  (assert (eq (interp '(+ 10 5) nil) '15) () 'P1-error)
  (assert (eq (interp '(- 12 8) nil) '4) () 'P2-error)
  (assert (eq (interp '(* 5 9) nil) '45) () 'P3-error)
  (assert (not (interp '(> 2 3) nil)) () 'P4-error)
  (assert (interp '(< 1 131) nil) () 'P5-error)
  (assert (interp '(= 88 88) nil) () 'P6-error)
  (assert (not(interp '(and nil true) nil)) () 'P7-error)
  (assert (interp '(or true nil) nil) () 'P8-error)
  (assert (not(interp '(not true) nil)) () 'P9-error)
  (assert (interp '(number 354) nil) () 'P10-error)
  (assert (interp '(equal (3 4 1) (3 4 1)) nil) () 'P11-error)
  (assert (eq (interp '(if nil 2 3) nil) '3) () 'P12-error)
  (assert (interp '(null ()) nil) () 'P13-error)
  (assert (not(interp '(atom (3)) nil)) () 'P14-error)
  (assert (interp '(eq x x) nil) () 'P15-error)
  (assert (eq (interp '(first (8 5 16)) nil) '8) () 'P16-error)
  (assert (equal (interp '(rest (8 5 16)) nil) '(5 16)) () 'P17-error)
  (assert (equal (interp '(cons 6 3) nil) (cons 6 3)) () 'P18-error)
  (assert (eq (interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) '12) () 'P19-error)
  (assert (interp '(and (> (+ 3 2) (- 4 2)) (or (< 3 (* 2 2)) (not (= 3 2)))) nil) () 'P20-error)
  (assert (not (interp '(or (= 5 (- 4 2)) (and (not (> 2 2)) (< 3 2))) nil)) () 'P21-error)
  (assert (equal (interp '(if (not (null (first (a c e)))) (if (number (first (a c e))) (first (a c e)) (cons (a c e) d)) (rest (a c e))) nil) (cons '(a c e) 'd)) () 'P22-error)

  ; More complicated tests that include user defined functions
  (assert (eq (interp '(greater 3 5) '((greater x y = (if (> x y) x (if (< x y) y nil)))) ) '5) () 'U1-error)
  (assert (eq (interp '(square 4) '((square x = (* x x)))) '16) () 'U2-OK 'U2-error)
  (assert (eq (interp '(simpleinterest 4 2 5) '((simpleinterest x y z = (* x (* y z))))) '40) () 'U3-error)
  (assert (interp '(xor true nil) '((xor x y = (if (equal x y) nil true)))) () 'U4-error)
  (assert (eq (interp '(cadr (5 1 2 7)) '((cadr x = (first (rest x))))) '1) () 'U5-error)
  (assert (eq (interp '(last (s u p)) '((last x = (if (null (rest x)) (first x) (last (rest x)))))) 'p) () 'U6-error)
  (assert (equal (interp '(push (1 2 3) 4) '((push x y = (if (null x) (cons y nil) (cons (first x) (push (rest x) y)))))) '(1 2 3 4)) () 'U7-error)
  (assert (equal (interp '(pop (1 2 3)) '((pop x = (if (atom (rest (rest x))) (cons (first x) nil) (cons (first x)(pop (rest x))))))) '(1 2)) () 'U8-error)
  (assert (eq (interp '(power 4 2) '((power x y = (if (= y 1) x (power (* x x) (- y 1)))))) '16) () 'U9-error)
  (assert (eq (interp '(factorial 4) '((factorial x = (if (= x 1) 1 (* x (factorial (- x 1))))))) '24) () 'U10-error)
  (assert (eq (interp '(divide 24 4) '((divide x y = (div x y 0)) (div x y z = (if (> (* y z) x) (- z 1) (div x y (+ z 1)))))) '6) () 'U11-error)
  "All TA tests passed"
)

(defun eclass-tests ()
  "Tests adapted from samples provided on elcass."
  (assert (equal (interp '(rest (1 2 (3))) nil) '(2 (3))) () "eclass 1 error")
  (assert (equal (interp '(rest (p 1 2 (3))) nil) '(1 2 (3))) () "eclass 2 error")
  (assert (equal (interp '(first (rest (1 (2 3)))) nil) '(2 3)) () "eclass 3 error")
  (assert (equal (interp '(eq (< 3 4) (eq (+ 3 4) (- 2 3))) nil) nil) () "eclass 4 error")
  (assert (equal (interp '(if (> 1 0) (+ 1 2) (+ 2 3)) nil) 3) () "eclass 5 error")
  (assert (equal (interp '(if (> 1 0) (if (eq 1 2) 3 4) 5)  nil) 4) () "eclass 6 error")
  (assert (equal (interp '(cons (first (1 2 3))  (cons a nil)) nil) '(1 a)) () "eclass 7 error")
  (assert (equal (interp '(and (or true  nil) (> 3 4)) nil) nil) () "eclass 8 error")
  (assert (equal (interp '(equal (1 2 3) (1 2 3)) nil) T) () "eclass 9 error")
  "Eclass Tests Passed!"
)

(defun if-and-or-tests ()
  "Tests to ensure that the special functions, 'if', 'and' and 'or' work correctly. Unlike
   most functions these cannot execute their extra parameters unless the first parameter indicates
   that they should. These tests ensure the the result is both correct and that code that shouldn't
   exectue doesn't. Ie. if should not evaluate both code paths and the boolean operators should
   short circuit correctly."
  (assert (interp '(if (null (first nil)) T (assert nil "If-test 1.0 failed!")) nil) () "If-test 1.1 failed!")
  (assert (interp '(if (not (null (first nil))) (assert nil "If-test 2.0 failed!") T) nil) () "If-test 2.1 failed!")
  
  (assert (interp '(or T (assert nil "Or-test 1.0 failed!")) nil) () "Or-test 1.1 failed!")
  (assert (interp '(or nil (null (first nil))) nil) () "Or-test 2 failed!")
  (assert (not (interp '(or nil (not (null (first nil)))) nil)) () "Or-test 3 failed!")
  (assert (equal (interp '(or nil (first (1 2 3))) nil) 1) () "Or-test 4 failed")
  
  (assert (not (interp '(and nil (assert nil "And-test 1.0 failed!")) nil)) () "And-test 1.1 failed!")
  (assert (interp '(and T (null (first nil))) nil) () "And-test 2 failed!")
  (assert (not (interp '(and T (not (null (first nil)))) nil)) () "And-test 3 failed!")
  (assert (equal (interp '(and T (first (1 2 3))) nil) 1) () "And-test 4 failed")
  "If, And, Or all tests passed!"
)

(defun complex-prog-tests ()
  "Test a 'complex' program. In this case implement a function to remove duplicates from
   a list. Tests recursive calls, multiple function definitions and evaluation of sub-parameters in a list."
  (let ((prg '((contains v l =
		   (if (null l)
		       nil
		       (if (equal (first l) v)
			   v
			   (contains v (rest l)))))
		(remove-dups l =
		   (if (null l)
		       nil
		       (if (contains (first l) (rest l))
		           (remove-dups (rest l))
		           (cons (first l) (remove-dups (rest l)))))))))
    (assert (equal (interp '(remove-dups (1 2 3 3 2 1)) prg) '(3 2 1)) () "Complex test 1 failed!")
    (assert (equal (interp '(remove-dups ((first (1 2 3)) 1 (1 1) (1 1) nil (null (first nil)))) prg) '(1 (1 1) nil T)) () "Complex test 2 failed"))
  "Complex Program Tests Passed!"
)

(defun all-tests ()
  "Run all tests"
  (ta-tests)
  (eclass-tests)
  (if-and-or-tests)
  (complex-prog-tests)
  "All Tests Passed!"
)
