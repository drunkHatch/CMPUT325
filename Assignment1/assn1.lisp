;;; CMPUT 325 LEC B1
;;; Assignment 1
;;; Jesse Tucker
;;; jtucker@ualberta.ca
;;; 1255723
;;;
;;; Note : Run (test-all) to execute all unit tests. Please see the tests for sample
;;;        input and output


; Question 1

(defun form-pair (ls)
  "Function that takes a list and pairs each element with its previous element.
   Returns a list of pairs. See form-pair-test for an example of expected input
   and output."
  ; First fix the list such that if it has 1 element it instead has 2 elements
  ; where the second element is the same as the first
  (if (null ls)
      nil
      (let* ((fixed-list (if (eq (cdr ls) nil) 
			     (cons (car ls) ls)
			     ls))
	     (pair (list (cadr fixed-list) (car fixed-list))) ; first two elements of the list in reverse order
	     (rest (cddr fixed-list))) ; remainder of the list
	(if (null rest) 
	    (list pair) ; terminating condition
	    (cons pair (values (form-pair rest))))))) ; recursive step

(defun form-pair-test ()
  "Tests for form-pair function"
  (print "Testing form-pair")
  (assert (equal (form-pair '(1 2 3 4 5 6 7 8)) '((2 1)(4 3)(6 5)(8 7))) () "Test 1 failed!")
  (assert (equal (form-pair '(a b c d e )) '((b a)(d c)(e e))) () "Test 2 failed!")
  (assert (equal (form-pair '((1 a)(2 b)(3 c)(4 d))) '( ((2 b) (1 a)) ((4 d) (3 c)) )) () "Test 3 failed!")
  (assert (equal (form-pair ()) '()) () "Test 4 failed!")
  "All tests passed!"
)


; Question 2

(defun drop-pair (ls)
  "Takes a list of pairs and evaluates each pair. Number are evaluated to
   themselves. Everything else is evaluated by funcall. If a pair of values
   evaluate to the same number then the pair is not included in the final output.   If the pairs do not match they are included in the final list.
   See drop-pair-test for sample input and output."
  (let ((first-pair (my-eval-and-drop(car ls)))
	(next (if (not (null (cdr ls)))
		  (drop-pair (cdr ls))
		   nil)))
    (strip-nils (cons first-pair next)))
)

(defun strip-nils (ls)
  "Takes in a list and removes all nils in the list."
  (let ((first (car ls))
	(rest (if (null (cdr ls)) ; check if there are more to check
		  nil
		  (strip-nils (cdr ls)))))
    (if (null first)
	rest
	(cons first rest))))

(defun my-eval (form)
  "Simple eval helper that does not evaluate numbers and will evaluate everything else. Assumes anything to be evaluated is of the form : (op var1 var2)."
  (if (numberp form)
      form
      (funcall (car form) (my-eval (cadr form)) (my-eval (caddr form))))
)

(defun my-eval-and-drop (ls)
  "Takes a pair and evaluates each element. If each value of the pair is equal
   this function will return nil."
  ; pair is the pair after they have each been evaulated
  (let ((pair (list (my-eval (car ls)) (my-eval (cadr ls)))))
    (if (equal (car pair) (cadr pair))
	nil
	pair))
)

(defun drop-pair-test ()
  "Tests for drop-pair. includes example from assignment, an empty result and a nested
   evaluation."
  (print "Testing drop-pair...")
  (assert (equal (drop-pair '(((+ 2 4) (* 3 4)) (6 (* 2 3)))) '((6 12))) () "Test 1 failed!")
  (assert (equal (drop-pair '(((+ 2 4) (* 3 2)) (6 (* 2 3)))) nil) () "Test 2 failed!")
  (assert (equal (drop-pair '((5 5) ((+ 2 3) 6) ((/ 10.0 10.0) (* 3 (* 2 4))) )) '((5 6) (1.0 24))) () "Test 3 failed!")
  "All tests passed!"
)


; Question 3

(defun remove-duplicate (ls)
  "Removes all duplicate entries from the provided list. Items are considered duplicates if and only if
   (equal arg1 arg2) evaluates to true. Ordering is preserved with unqiue entries at the end of the list
   taking priority. See remove-duplicate-test for examples"
  ; append car ls to front if remove-duplicate(cdr ls) does not contain it
  ; also stop if the list has only 1 more element, as it cannot possibly contain duplicates
  (if (null (cdr ls))
      ls
      (let ((front (car ls))
	    (rest (remove-duplicate (cdr ls))))
	(if (my-find front rest)
	    rest
	    (cons front rest))))
)

(defun my-find (val ls)
  "Simple helper for finding a value in a list. Must use because the default find
   function was not on the list of approved functions for assignment 1."
  (cond ((null ls) nil)
	((equal val (car ls)) T)
	((null (cdr ls)) nil)
	(T (my-find val (cdr ls))))
)

(defun remove-duplicate-test ()
  "Tests for remove-duplicate."
  (print "Testing remove-duplicate...")
  (assert (equal (remove-duplicate '(1 2 3 4)) '(1 2 3 4)) () "Test 1 failed!")
  (assert (equal (remove-duplicate '(1 2 3 4 5 4 3 2 1)) '(5 4 3 2 1)) () "Test 2 failed!")
  (assert (equal (remove-duplicate '(a b c a d b)) '(c a d b)) () "Test 3 failed!")
  (assert (equal (remove-duplicate nil) nil) () "Test 4 failed!")
  (assert (equal (remove-duplicate '(nil nil nil)) '(nil)) () "Test 5 failed!")
  (assert (equal (remove-duplicate '((a b) (1 2 3) (a b) )) '((1 2 3) (a b))) () "Test 6 failed!")
  "All tests passed!"
)


; Question 4

(defun my-count (ls)
  "Returns an integer count of the unique elements in the list provided.
   See my-count-test for example input/output"
  (cond ((null ls) 0)
	((my-find (car ls) (cdr ls)) (my-count (cdr ls)))
	(T (+ 1 (my-count (cdr ls)))))
)

(defun my-count-actual (ls)
  "Count the number of elements in the provided list"
  (if (null (cdr ls))
      1
      (+ 1 (my-count-actual (cdr ls))))
)

(defun my-count-test ()
  "Tests for my count. Checks empty lists, sets, lists with duplicates and lists with nil and other lists."
  (print "Testing my-count...")
  (assert (equal (my-count '(1 2 3 4)) 4) () "Test 1 failed!")
  (assert (equal (my-count nil) 0) () "Test 2 failed!")
  (assert (equal (my-count '(nil)) 1) () "Test 3 failed!")
  (assert (equal (my-count '(a '(1 2) b a nil)) 4) () "Test 4 failed!")
  (assert (equal (my-count '(1 2 3 4 4 3 2 1 1 1)) 4) () "Test 5 failed!")
  "All tests passed!"
) 


; Question 5

(defun power-set (ls)
  "Generates a power set from the provided set. See power-set-test for sample input and output."
  (assert (is-set ls) () "power-set only accepts sets as input! Input is not a set!")
  (if (null ls)
      '( () )
      (if (null (cdr ls))
	  (list ls '() ) ; lists with only 1 element consist of a list of themselves and nil as their power set
	  ; anything longer than one element is broken into all possible sublists with 1 less element
	  ; and their power-sets are generated.
	  (let ((sub-lists (generate-sub-lists ls ls)))
	    (remove-duplicate (cons ls (power-set-helper sub-lists))))))
)

(defun power-set-helper (ls)
  "Helper funcation intended to iterate over a list of sub-lists and aggregate the power-sets
   into a single list. Basically this is just a for-each loop."
  (let ((pwr-set (power-set (car ls))))
    (if (null (cdr ls))
	pwr-set
	(merge-lists pwr-set (power-set-helper (cdr ls)))))
)

(defun merge-lists (list-one list-two)
  "Take two lists and merge them such that one list is appended to the other as it is."
  (if (null (cdr list-two))
      (cons (car list-two) list-one)
      (cons (car list-two) (merge-lists list-one (cdr list-two))))
)

(defun generate-sub-lists (ls org)
  "Takes a list of elements that are to be removed from the original list
   before being aggregated into a list. Sample input and output:
   (generate-sub-lists('(1 2 3) '(1 2 3))) => ( (2 3) (1 3) (1 2) )
   (generate-sub-lists('(1 2) '(1 2 3 4))) => ( (2 3 4) (1 3 4) )"
  (let ((new-list (my-remove (car ls) org)))
    (if (null (cdr ls))
	(list new-list)
	(cons new-list (generate-sub-lists (cdr ls) org))))
)

(defun my-remove (val ls)
  "Removes all instances of val from ls. Sample input output:
   (my-remove 1 '(1 2 3)) => (2 3)
   (my-remove 2 '(1 2 3 2 4 2 1) => (1 3 4 1)"
  (if (null ls)
      nil
      (let ((rest (remove val (cdr ls)))
	    (first (car ls)))
	   (if (equal val first)
	       rest
	       (cons first rest))))
)

; Note : needed for power-set-test
(defun is-subset (sub-set set)
  "Returns true if sub-set is a subset of set. Nil otherwise."
  (let ((is-first-in-both (my-find (car sub-set) set)))
    (if (or (not is-first-in-both) (null (cdr sub-set))) 
	is-first-in-both
	(is-subset (cdr sub-set) set)))
)

; Note : needed for power-set-test
(defun is-set (set)
  "Returns true if set is a set of unique elements."
  ; It is considered a set if it is identical to the list
  ; created when all duplicates are removed
  (equal set (remove-duplicates set))
)

; Note : needed for power-set-test
(defun is-equivelent-sets (list-one list-two)
  "Checks if two sets are equivelent. Ordering does not matter. Lists must not
   contain any duplicates!"
  (assert (is-set list-one) () "list-one is not a set! It must be a set!")
  (assert (is-set list-two) () "list-two is not a set! It must be a set!")

  ; must be equal to each other or
  ; must be same length to be equivelent and
  ; also one must be a subset of the other
  (if (equal list-one list-two)
      T
      (and (equal (my-count list-one) (my-count list-two))
       (is-subset list-one list-two)))
)

(defun power-set-test ()
  "Tests for power set. Tests power sets from size 0 to size 4."
  (print "Testing power-set...")
  (assert (is-equivelent-sets (power-set '()) '( () )) () "Test 1 Failed")
  (assert (is-equivelent-sets (power-set '(1)) '( () (1) )) () "Test 2 Failed")
  (assert (is-equivelent-sets (power-set '(1 2)) '( () (1) (2) (1 2))) () "Test 3 Failed")
  (assert (is-equivelent-sets (power-set '(1 2 3)) '( ()
						     (1) (2) (3)
						     (1 2) (1 3) (2 3)
						     (1 2 3))) () "Test 4 Failed")
  (assert (is-equivelent-sets (power-set '(1 2 3 4)) '( ()
						       (1) (2) (3) (4)
						       (1 2) (1 3) (1 4) (2 3) (2 4) (3 4)
						       (1 2 3) (1 3 4) (1 2 4) (2 3 4)
						       (1 2 3 4))) () "Test 5 Failed")
  "All tests passed!"
)


; Question 6A
(defun findOne (name type list)
  "Finds the best or worst grade for a given student. If the student is not
   in the list returns null. Otherwise returns their best/worst grade. See
   findOne-test for detailed input/output"
  ; find-best and find-worst are lambda's for evaluating grades.
  ; a grade that closely matches the criteria will return a large value.
  ; see map-to-good-grade and map-to-bad-grade for details.
  (let ((comp-func (cond ((equal type 'best) 'map-to-good-grade)
			 ((equal type 'worst) 'map-to-bad-grade)
			 (T (assert nil () "Must provide either best or worst! No other criteria allowed!")))))
    ; first filter out names that do not match then translate the grades
    ; into numbers. After that select the highest grade in the set and return
    ; it after translating the number grade back into a letter grade.
    (let* ((class-list (map-grades (find-only-name name list) comp-func))
	   (best-grade (find-best-grade class-list)))
      (if (null best-grade)
	  nil
	  (list (car best-grade) (cadr best-grade) (map-grade-to-letter (caddr best-grade))))))
)

(defun find-best-grade (list)
  "Returns the highest grade in the list"
  (if (null (cdr list))
      (car list) ; only one element, must be the best
      (let* ((best-class (find-best-grade (cdr list)))
	     (current-grade (caddar list))
	     (best-grade (caddr best-class)))
	(if (> current-grade best-grade)
	    (car list)
	    best-class)))
)

(defun find-only-name (name grades)
  "Return the list after filtering out all names
   that do not match."
  (if (null grades)
      nil
      (let ((rest (find-only-name name (cdr grades)))
	    (first (car grades)))
	(if (equal name (car first))
	    (cons first rest)
	    rest)))
)

(defun grade-map ()
  "map of letter grades onto numbers for easy comparison"
  '((A+ 12)
    (A 11)
    (A- 10)
    (B+ 9)
    (B 8)
    (B- 7)
    (C+ 6)
    (C 5)
    (C- 4)
    (D+ 3)
    (D 2)
    (D- 1)
    (F 0))
)

(defun map-grade-to-letter (num-grade)
  "map a number grade onto a letter grade"
  (let* ((val (map-grade-to-letter-helper (abs num-grade) (grade-map))))
    (assert (not (null val)) () "Failed to map number grade to letter!")
    val)
)

(defun map-grade-to-letter-helper (num-grade list)
  "Helper for mapping number grades onto letter grades. Basically a for loop"
  (if (equal (cadar list) num-grade)
      (caar list)
      (if (null (cdr list))
	  nil
	  (map-grade-to-letter-helper num-grade (cdr list))))
)

(defun map-to-good-grade (letter-grade)
  "Map a letter grade onto it's good grade equivelent. Ie. a positive number"
  (let ((val (map-to-good-grade-helper letter-grade (grade-map))))
    (assert (not (null val)) () "Failed to map letter grade!")
    val)
)

(defun map-to-good-grade-helper (letter-grade list)
  "Helper for mapping to good grades. Basically a for loop."
  (if (equal (caar list) letter-grade)
      (cadar list)
      (if (null (cdr list))
	  nil
	  (map-to-good-grade-helper letter-grade (cdr list))))
)

(defun map-to-bad-grade (letter-val)
  "Maps a letter grade onto its bad number equivelent. Basically just the negative of the good number."
  (* -1 (map-to-good-grade letter-val))
)

(defun map-grades (grades transform)
  "Transforms a list of classes such that the grades are represented by numbers instead of letters"
  (if (null grades)
      nil
      (let* ((first (car grades))
	     (rest (cdr grades))
	     (new-grade (funcall transform (caddr first)))
	     (new-val (list (car first) (cadr first) new-grade)))
	(if (null rest)
	    (list new-val)
	    (cons new-val (map-grades rest transform)))))
)

(defun findOne-test ()
  (print "Testing findOne...")
  (assert (equal (findOne 'Joe 'best '()) '()) () "Test 1 Failed")
  (assert (equal (findOne 'Joe 'best '( (Joe CMPUT325 A-) (Jill CMPUT325 B+) (Joe CMPUT201 C-))) '(Joe CMPUT325 A-)) () "Test 2 Failed")
  (assert (equal (findOne 'Joe 'worst '( (Joe CMPUT325 A-) (Jill CMPUT325 B+) (Joe CMPUT201 C-))) '(Joe CMPUT201 C-)) () "Test 3 Failed")
  (assert (equal (findOne 'John 'best '( (Joe CMPUT325 A-) (Jill CMPUT325 B+) (Joe CMPUT201 C-))) nil) () "Test 4 Failed")
  "All tests passed"
)

; Question 6B

(defun findAll (name grades)
  "Finds all classes by student 'name in the list 'grades. Also sorts by course name. 
   See findAll-test for sample input/output."
  (sort (find-only-name name grades) 'sort-criteria)
)

(defun sort-criteria (a b)
  "sort criteria for the findAll function. Sorts by name of the class"
  (string-lessp (cadr a) (cadr b))
)

(defun findAll-test ()
  "Tests for findAll. Checks empty lists, names that aren't there, 1 element results and 2 element results."
  (print "Testing findAll...")
  (let ((ls '( (joe cmput325 A-) (jim cmput272 F) (jimmy cmput301 A) (jimmy cmput250 A+)) ))
    (assert (equal (findAll 'joe '()) nil) () "Test 1 Failed")
    (assert (equal (findAll 'joe ls) '( (joe cmput325 A-) )) () "Test 2 Failed")
    (assert (equal (findAll 'jill ls) nil) () "Test 3 Failed")
    (assert (equal (findAll 'jimmy ls) '((jimmy cmput250 A+) (jimmy cmput301 A))) () "Test 4 Failed"))
  "All tests Passed"
)


; Question 7A

(defun reached (node edges)
  "Takes an nodes and a set of pairs that define the edges in a graph. Returns the list of all nodes
   that can be reached from the starting node. Will correctly handle loops in the graph. Will not
   include the starting node in the output See reached-test for sample input/output."
  (if (or (null edges) (null node))
      nil ; no more edges or nodes to check, return an empty list
      (let ((next-nodes (my-map (find-pairs node edges) 'cadr)) ; a list of all nodes reachable from 
	    (remaining-edges (strip-nils (my-map edges 'does-first-not-match node)))) ; remove the edges that this node is connected to
	; create a list of pairs that will be passed to the helper. The pairs will be (edges node). Map will then apply reached to them.
	(let* ((helper-params (form-pair (my-zip next-nodes (my-repeat remaining-edges (my-count next-nodes)))))
	       (mapped-elements (my-map helper-params 'reached-helper)))
	  (my-remove node (strip-nils (merge-lists next-nodes (flatten mapped-elements)))))))
)

(defun reached-helper (element-edges-pair)
  "Simple helper that expands a pair into the 2 parameters for the reached function"
  (reached (cadr element-edges-pair) (car element-edges-pair))
)

(defun does-first-not-match (pair target)
  "Returns true if the first element in pair matches target."
  (if (equal (car pair) target)
      nil
      pair)
)

(defun flatten (L)
  "Flattens a list of lists into a list of atoms. No lists will remain in the final list.
   Code is borrowed from eClass."
  (cond ((null L) nil)
	((atom (car L)) (cons (car L) (flatten (cdr L))))
	(T (append (flatten (car L)) (flatten (cdr L))))))

(defun my-zip (list-one list-two)
  "Merges the two lists by alternating values. First list-one then list-two. Lists must be the same size."
  (assert (equal (my-count-actual list-one) (my-count-actual list-two)) () "Lists must be the same size!")
  (if (null list-one)
      nil
      (cons (car list-one) (cons (car list-two) (my-zip (cdr list-one) (cdr list-two)))))
)

(defun my-repeat (val count)
  "Create a new list that is val repeated count times."
  (if (not (> count 0))
      nil
      (cons val (my-repeat val (- count 1))))
)

(defun my-map (list func &optional extra-param)
  "Simple map function. Needed because of the artificial limitations of assignment 1."
  (if (null list)
      nil
      (let ((val (if (null extra-param)
		     (funcall func (car list))
		     (funcall func (car list) extra-param))))
	(cons val (my-map (cdr list) func extra-param))))
)

(defun find-pairs (element-one pairs)
  "Finds all pairs where the first element in the pair matches element-one"
  (if (null pairs)
      nil
      (let ((rest (find-pairs element-one (cdr pairs))))
	(if (equal element-one (caar pairs))
	    (cons (car pairs) rest)
	    rest)))
)

(defun reached-test ()
  "Tests for the reached function"
  (print "Testing reached...")
  (assert (is-equivelent-sets (reached 'a '( (a b) (a c) (a d) (q w) (w e) (e a))) '(b c d)) () "Test 1 Failed")
  (assert (is-equivelent-sets (reached 'a '( (a b) (b c) (c d) (q w) (e a))) '(b c d)) () "Test 2 Failed")
  (assert (is-equivelent-sets (reached 'd '( (a b) (b c) (c d))) nil) () "Test 3 Failed")
  (assert (is-equivelent-sets (reached 'u '( (a b) (b c) (c d))) nil) () "Test 4 Failed")
  (assert (is-equivelent-sets (reached 'a nil) nil) () "Test 5 Failed")
  "All tests passed!"
)


; Question 7 B

(defun rank (nodes edges)
  "Sorts the provided nodes by their rank. Rank is considered to be the number of nodes that are accessible from this node.
   Edges defines the graph. See rank-test for sample input/output"
  (my-map (sort (my-map (form-pair (my-zip nodes (my-repeat (strip-matching edges) (my-count-actual nodes)))) 
			'rank-helper) 
		'rank-sorter) 
	  'car)
)

(defun rank-sorter (ranked-pair-one ranked-pair-two)
  "Small helper for comparing pairs of (node rank)."
  (> (cadr ranked-pair-one) (cadr ranked-pair-two))
)

(defun rank-helper (node-edge-pair)
  "Small helper for mapping a pair onto the reached function and returning a pair
   (node rank)"
  (let ((node (cadr node-edge-pair))
	(edges (car node-edge-pair)))
    (list node (my-count (reached node edges))))
)

(defun strip-matching (list-of-pairs)
  "Removes pairs where the first and second are equal"
  (strip-nils (my-map list-of-pairs 'do-pairs-match))
)

(defun do-pairs-match (pair)
  "checks if a pair is matching. Ie. (a a) or (1 1) return nil but (1 2) or (a b) will return themselves."
  (if (equal (car pair) (cadr pair))
      nil
      pair)
)

(defun rank-test ()
  "Tests for rank function."
  (print "testing rank...")
  (assert (equal (rank '(c b a) '((a b) (b c) (c d))) '(a b c)) () "Test 1 Failed")
  (assert (equal (rank '(a b e) '((a b) (a c) (a d) (a p) (e f) (f g) (f h) (h i) (i j) (j k) (j h))) '(e a b)) () "Test 2 Failed")
  "All tests passed!"
)

(defun test-all ()
  "This is self explanatory..."
  (form-pair-test)
  (drop-pair-test)
  (remove-duplicate-test)
  (my-count-test)
  (power-set-test)
  (findOne-test)
  (findAll-test)
  (reached-test)
  (rank-test)
)
