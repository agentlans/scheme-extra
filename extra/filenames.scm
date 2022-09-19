(define-module (extra filenames)
  #:use-module (extra extra) 
  #:use-module (ice-9 ftw) ; for file-system-fold
  #:export (split-path join-path exists? readable? writable? executable?
	      file-type directory? file-size mkdir-p ;directory-contents
	      glob-files))

;; Note: this part is Guile-specific and unportable.
;; See https://www.gnu.org/software/guile/manual/html_node/File-System.html
;;     for the Scheme procedures.

(define (split-path path)
  "Splits a string path to its component parts."
  (map list->string
       (split-at (string->list path)
		 file-name-separator?)))
;; (split-path "/tmp/foo/bar.txt") ; => ("" "tmp" "foo" "bar.txt")

(define (join-path parts)
  "Joins the parts of a path to a complete string."
  (string-join parts file-name-separator-string))
;; (join-path '("" "tmp" "foo")) ; => "/tmp/foo"

(define (exists? path)
  "Returns #t if path to file exists."
  (access? path F_OK))

(define (readable? path)
  (access? path R_OK))

(define (writable? path)
  (access? path W_OK))

(define (executable? path)
  (access? path X_OK))

(define (file-type path)
  "Type of the file on that path.
Example: 'regular, 'directory, ..."
  (stat:type
   (stat path)))
;; (file-type "/tmp")

(define (directory? path)
  "Returns #t if path is a directory."
  (equal? (file-type path) 'directory))

(define (file-size path)
  "Size of a regular file in bytes."
  (stat:size (stat path)))
;; (file-size "filenames.scm")

(define (mkdir-p path)
  "Creates a directory and its parents if they don't already exist.
Like mkdir -p in shell."
  (let ((series (map join-path
		     (cdr (lscan (split-path path))))))
    (display series)
    (do ((temp series (cdr temp)))
      ((null? temp) #t) ; stop
      (let ((dir (car temp)))
	(unless (exists? dir)
	  ;; Try to create the path
	  (mkdir dir))))))
;; (mkdir-p "/tmp/foo/bar/baz/")

(define (directory-contents dirname)
  "Returns the files inside dirname as a list of strings.
Note: includes \".\" and \"..\""
  (let ((d (opendir dirname)))
    (do ((f (readdir d) (readdir d))
	 ;; Add file to list
	 (out '() (cons f out)))
	((eof-object? f)
	 (begin (closedir d)
		(reverse out))))))
;; (directory-contents "/") ; => ("usr" "home" ...)

;; Below are procedures for file globbing
;; ------------------------------------------------------------
(define (glob->regex str)
  "Converts a glob wildcard pattern to a regex pattern."
  (string-concatenate
   `("^"
     ,@(map (lambda (ch)
	      (cond ((char=? ch #\?) ".")
		    ((char=? ch #\*) ".*")
		    ;; Escape special characters
		    ((member ch '(#\$ #\. #\^ #\( #\) #\[ #\]))
		     (string-concatenate
		      (list "\\" (string ch))))
		    (else (string ch))))
	    (string->list str))
     "$")))
;; (glob->regex "?foo*")

(define (reg-match? lst)
  (let ((str (cadr lst))
	(obj (car lst)))
    (regexp-exec obj str)))

(define (make-globber pattern go-deep match-length case-sensitive)
  "Returns a procedure that checks the path against the given glob pattern."
  (let* ((patterns (map (lambda (x)
			  (let ((s (glob->regex x)))
			    (if case-sensitive
				(make-regexp s)
				(make-regexp s regexp/icase))))
			(split-path pattern)))
	 (n (length patterns)))
    (lambda (path)
      ;; Return #t if path matches the glob pattern
      (let* ((parts (split-path path))
	     (n-parts (length parts)))
	(cond ((and (not go-deep) (> n-parts n)) #f)
	      ((and match-length
		    (not (= n-parts n)))
	       #f)
	      (else (all? reg-match?
			  (zip patterns parts))))))))
;; Returns #t
;; ((make-globber "?foo*/*bar.txt" #f #t) "afoodie/bar.txt")

;; Only check that the path matches the prefix of the glob pattern
;; ((make-globber "bar/foo" #f #t) "bar") ;=> #t
;; ((make-globber "bar/foo" #t #t) "bar") ;=> #f

(define (do-nothing path stat result)
  result)

(define (do-nothing-error path stat errno result)
  result)

(define (file-matcher glob recursive case-sensitive)
  "Returns closure to be used in tree fold."
  ;; We must match the whole pattern
  (let ((g (make-globber glob recursive (not recursive) case-sensitive)))
    (lambda (path stat result)
      (if (g path)
	  (cons path result)
	  result))))

(define (dir-matcher glob recursive case-sensitive)
  "Returns closure that determines whether or not to descend
into directory."
  ;; We can match part of the pattern
  (let ((g (make-globber glob recursive #f case-sensitive)))
    (lambda (path stat result)
      (g path))))

(define (has-wildcard? str)
  "Returns #t if str has a wildcard character."
  (not (disjoint? char=? '(#\* #\?)
		  (string->list str))))
;; (has-wildcard? "")

(define (longest-fixed-root path)
  "Returns the longest parent directory of path
that has no wildcard characters."
  (join-path
   (take-until has-wildcard?
	       (split-path path))))

(define (glob-files glob recursive case-sensitive follow-symlinks)
  "Returns list of all file paths that match the glob pattern.
The same file won't appear more than once in the output.

If recursive is #t, this procedure will descend 
into subdirectories below the pattern.
If follow-symlinks is #t, symlinks to directories will be treated
like the directories they point to.
Otherwise, the symlink will be treated as an ordinary file."
  (file-system-fold
   (dir-matcher glob recursive case-sensitive)
   (file-matcher glob recursive case-sensitive)
   do-nothing
   do-nothing
   do-nothing
   do-nothing-error ; ignore errors
   '()
   (longest-fixed-root glob)
   (if follow-symlinks
       stat
       lstat)))
;; (glob-files "/tmp/*" #t #t)

