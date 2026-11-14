#lang roulette/example/disrupt
;; authorship.rkt — Bayesian authorship attribution of student essays,
;; done the same way as the Caesar-cipher cryptanalysis example.
;;
;;   cryptanalysis                      authorship
;;   -------------                      ----------
;;   secret key (0..25)                 secret author (one of N students)
;;   uniform prior on keys              uniform prior on students
;;   "abstract Alice" picks letters     "abstract student" picks words with
;;     with English letter freqs          that student's word frequencies
;;   observe each ciphertext letter     observe each word of the essay
;;   posterior: most likely key         posterior: most likely author
;;
;; Data: BAWE_by_student/<id>.txt, one essay per line (built by
;; build_student_files.py). For each student the FIRST essay is held out for
;; testing and the REST are used to build their frequency model (the same
;; split as first_essay_frequency/ and rest_essay_frequency/).
;;
;; Features: only function words ("the", "of", "which", ...). They are
;; frequent, topic-independent, and say more about *style* than content, so a
;; student writing about economics and then about history still looks like
;; themselves.

(require racket/string
         racket/list
         racket/file
         racket/math
         racket/match)

(provide function-words
         students-with-essays
         read-essays
         tokenize
         build-model
         attribute
         verify
         run-experiment)

;; ---------------------------------------------------------------------------
;; data
;; ---------------------------------------------------------------------------

(define data-dir "BAWE_by_student")

;; Classic stylometry function words (Mosteller & Wallace style list).
(define function-words
  '("a" "about" "after" "all" "also" "an" "and" "any" "are" "as" "at" "be"
    "been" "but" "by" "can" "could" "do" "even" "for" "from" "had" "has"
    "have" "however" "if" "in" "into" "is" "it" "its" "may" "more" "must"
    "no" "not" "of" "on" "one" "only" "or" "other" "our" "should" "so"
    "some" "such" "than" "that" "the" "their" "then" "there" "therefore"
    "these" "they" "this" "those" "thus" "to" "upon" "was" "we" "were"
    "what" "when" "where" "which" "while" "who" "will" "with" "would"))

(define function-word-set (for/hash ([w (in-list function-words)]) (values w #t)))

;; Lowercase, keep alphabetic runs only. Works on BAWE lines and on raw text
;; (e.g. an AI-generated essay pasted into a .txt file).
(define (tokenize text)
  (regexp-match* #px"[a-z]+" (string-downcase text)))

;; Count only the function words in a list of essays.
(define (function-word-counts essays)
  (for*/fold ([h (hash)])
             ([e (in-list essays)]
              [w (in-list (tokenize e))]
              #:when (hash-ref function-word-set w #f))
    (hash-update h w add1 0)))

(define (read-essays id)
  (filter (lambda (l) (non-empty-string? (string-trim l)))
          (file->lines (build-path data-dir (string-append id ".txt")))))

;; Student ids (sorted) that have at least `min-essays` essays.
(define (students-with-essays min-essays)
  (sort (for*/list ([f (in-list (directory-list data-dir))]
                    [name (in-value (path->string f))]
                    #:when (regexp-match? #rx"[.]txt$" name)
                    [id (in-value (substring name 0 (- (string-length name) 4)))]
                    #:when (>= (length (read-essays id)) min-essays))
          id)
        string<?))

;; A student's "abstract writer": probability of each function word, with
;; add-one smoothing so unseen words are unlikely but not impossible.
(define (build-model essays)
  (define counts (function-word-counts essays))
  (define total (+ (apply + (hash-values counts)) (length function-words)))
  (for/hash ([w (in-list function-words)])
    (values w (/ (+ 1.0 (hash-ref counts w 0)) total))))

;; ---------------------------------------------------------------------------
;; the probabilistic model
;; ---------------------------------------------------------------------------

;; (uniform-choice '(x y z)) is x, y, or z, each with probability 1/3.
(define (uniform-choice xs)
  (make-categorical (for/list ([x (in-list xs)]) (cons x (/ 1 (length xs))))))

;; Given a (symbolic) author and a table author -> value, return that author's
;; value. This is the "look up the key" step; Roulette tracks every branch.
(define (lookup author table)
  (let loop ([entries table])
    (match entries
      [(list (cons _ v)) v]
      [(cons (cons a v) rest) (if (equal? author a) v (loop rest))])))

;; Observe that the essay contains word `w` exactly `n` times.
;;
;; The honest version is: for each of the n occurrences,
;;   (observe! (equal? (make-categorical (author-model author)) w))
;; which multiplies author a's weight by p_a(w) each time. We do the same thing
;; with one soft observation per word type: (observe! (flip p)) multiplies the
;; weight by p, so we use p = p_a(w)^n.
;;
;; Multiplying hundreds of small probabilities underflows a double, so each
;; author's factor is divided by the best author's factor for this word. That
;; constant is the same for every author, so it cancels when the posterior is
;; normalized: the answer is unchanged, the numbers just stay near 1.
(define (observe-word-count! author models w n)
  (define factors
    (for/list ([m (in-list models)])
      (cons (car m) (expt (hash-ref (cdr m) w) n))))
  (define best (apply max (map cdr factors)))
  (define scaled (for/list ([f (in-list factors)]) (cons (car f) (/ (cdr f) best))))
  (observe! (flip (lookup author scaled))))

(define (observe-essay! author models essay #:max-tokens max-tokens)
  (define tokens (filter (lambda (w) (hash-ref function-word-set w #f))
                         (tokenize essay)))
  (define used (take tokens (min max-tokens (length tokens))))
  (for ([(w n) (in-hash (function-word-counts (list (string-join used))))])
    (observe-word-count! author models w n)))

;; ATTRIBUTION: who, among these students, wrote `essay`?
;; `models` is a list of (student-id . model). Returns a pmf over student ids.
(define (attribute essay models #:max-tokens [max-tokens 1000])
  (query
   (define author (uniform-choice (map car models)))
   (observe-essay! author models essay #:max-tokens max-tokens)
   author))

;; VERIFICATION: did `student` write `essay`, or did someone else?
;; "Someone else" is modeled by `background`, a model built from many other
;; students. `prior` is how likely we think it is, before reading, that the
;; student wrote it. Returns a pmf over #t / #f.
(define (verify essay student-model background
                #:prior [prior 1/2]
                #:max-tokens [max-tokens 1000])
  (query
   (define wrote-it (flip prior))
   (define models (list (cons #t student-model) (cons #f background)))
   (observe-essay! wrote-it models essay #:max-tokens max-tokens)
   wrote-it))

;; ---------------------------------------------------------------------------
;; experiment (milestone 1 plan)
;; ---------------------------------------------------------------------------
;;
;; For each of N students with >= 5 essays:
;;   - build their model from every essay but the first
;;   - attribution: run `attribute` on their held-out first essay; correct if
;;     they come out on top
;;   - verification: run `verify` on (a) their own held-out essay and (b) the
;;     held-out essay of the next student in the list (an impostor)

(define (pmf-argmax d)
  (car (argmax cdr (for/list ([(v p) (in-pmf d)]) (cons v p)))))

(define (run-experiment #:students [n 30] #:min-essays [min-essays 5])
  (define ids (take (students-with-essays min-essays) n))
  (define essays (for/hash ([id (in-list ids)]) (values id (read-essays id))))
  (define held-out (for/hash ([id (in-list ids)]) (values id (first (hash-ref essays id)))))
  (define models
    (for/list ([id (in-list ids)]) (cons id (build-model (rest (hash-ref essays id))))))
  (define background (build-model (append* (map rest (hash-values essays)))))

  (printf "~a students, ~a function words\n\n" n (length function-words))
  (printf "student  top guess  P(top)  P(true)  P(wrote own)  P(wrote impostor's)\n")
  (define rows
    (for/list ([id (in-list ids)]
               [impostor (in-list (append (rest ids) (list (first ids))))])
      (define d (attribute (hash-ref held-out id) models))
      (define guess (pmf-argmax d))
      (define own ((verify (hash-ref held-out id) (cdr (assoc id models)) background) #t))
      (define fake ((verify (hash-ref held-out impostor) (cdr (assoc id models)) background) #t))
      (printf "~a     ~a       ~a   ~a    ~a          ~a\n"
              id guess
              (real->decimal-string (d guess) 3)
              (real->decimal-string (d id) 3)
              (real->decimal-string own 3)
              (real->decimal-string fake 3))
      (list (equal? guess id) (> own 1/2) (<= fake 1/2))))
  (define (pct k) (real->decimal-string (* 100.0 (/ (count values (map k rows)) n)) 1))
  (printf "\nattribution accuracy (1 of ~a):      ~a%\n" n (pct first))
  (printf "verification, own essay accepted:    ~a%\n" (pct second))
  (printf "verification, impostor rejected:     ~a%\n" (pct third)))

;; ---------------------------------------------------------------------------
;; run it
;; ---------------------------------------------------------------------------

;; Check an outside essay (e.g. an AI-written one saved as a .txt file)
;; against one student's model:
;;   racket authorship.rkt claude_essay.txt 0001
(define (check-file path id #:min-essays [min-essays 5])
  (define ids (take (students-with-essays min-essays) 30))
  (define background (build-model (append* (map (lambda (i) (rest (read-essays i))) ids))))
  (define d (verify (file->string path) (build-model (rest (read-essays id))) background))
  (printf "P(student ~a wrote ~a) = ~a\n" id path (real->decimal-string (d #t) 4)))

(module+ main
  (match (current-command-line-arguments)
    [(vector) (run-experiment #:students 30)]
    [(vector path id) (check-file path id)]
    [_ (displayln "usage: racket authorship.rkt [essay.txt student-id]")]))
