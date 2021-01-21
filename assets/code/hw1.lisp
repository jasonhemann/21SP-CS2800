#|

HW 1 - Part II 

This file is a continuation of your first homework assignment.

|#


#|  Objectives  |# 

;;   - Open a file in ACL2s mode.
;;   - Experiment with ACL2s, our substrate for exploring logic and computation 
;;   - Develop a familiarity and comfort with Eclipse and ACL2s
;;   - Discover similarities and differences with ACL2s and Racket *SL languages
;;   - Practice submitting ACL2 files on GradeScope

;; Sometimes, the elegant implementation is a function. Not a
;; method. Not a class. Not a framework. Just a function.
;; -- John Carmack

#| Preliminary note: 

Only create directories in workspace using ACL2s because `Eclipse`
will not recognize a project directory it did not create. Once you
create a project directory, you can add files to the directory from
you OS using the synced directory. Don't add files to the workspace
directory from your OS in any other way! Once you have added
files (say homework files you downloaded), then in eclipse, in the
project explorer, refresh the project and you will now see and will be
able to access the files you added.

|# 

#| Get Started with ACL2s |#

#|

   1. Bookmark the [ACL2
   documentation](http://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____TOP) (NB. ACL2s
   is an easy front-end and setup for ACL2). Furthermore, [download a
   copy of the
   documentation](http://www.cs.utexas.edu/users/moore/acl2/current/combined-manual/download/)
   to your development machine, and expand this archive in a
   convenient location. This will be convenient for you.

|#

#|

   2. Next, follow the `Get Set`, `Go`, and `Line action` sections of
   the [ACL2s tutorial](http://acl2s.ccs.neu.edu/acl2s/doc/#tutorial)
   
      Make sure that you only follow the instructions starting with
      'Get Set'. The 'Unpack' section is covered in Lab 1 ('Unpack
      will install ACL2s incorrectly).

      By the end of this tutorial you should feel comfortable moving
      around the Eclipse IDE and ACL2s interface.

|#

#| 

   3. In this file, below this comment, define the `fib` function as
   the tutorial referenced above demonstrates.

|# 


#| 

   4. In this same file, beneath `fib`, use ACL2s to define at least 3
   other functions in ACL2s mode. Only use ACL2s mode.

|# 

#| 

   5. Consider the following two pieces of code. Ponder whether or not
      they are the same function. You can use ACL2s to explore. Write
      a three to four sentence answer in that same file in a block
      comment beneath your the functions you just wrote.

   
|#    
	  (defun f1 (n)
		(if (and (natp n) (< 1 n))
			(+ (- 5 n) (f1 (- n 1)))
		 5))

	  (defun f2 (n)
		(if (and (natp n) (< 1 n)) 
		  (- (/ (* (1+ n) (- 10 n)) 2) 4)
		  5))

#| 

  Answer: 

  Explanation: 

|# 


#| 

   6. When you have completed this, submit it to the HW1 Dropbox on
   gradescope. We will implicitly presume that you know to follow this
   step from now on.

|# 
