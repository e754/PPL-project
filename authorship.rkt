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
;; Data: word -> count tables (lemmas), one CSV per essay or group of essays:
;;   rest_essay_frequency/<id>.csv    a student's essays except the first
;;                                    (used to build their model)
;;   first_essay_frequency/<id>.csv   that student's first essay (held out)
;;   synthetic_data_frequency/*.csv   AI-written essays (chatgpt, claude,
;;                                    deepseek, gemini; 10 each)
;; All three are lemma counts, so student and AI essays are compared like
;; for like.
;;
;; Features: only function words ("the", "of", "which", ...). They are
;; frequent, topic-independent, and say more about *style* than content, so a
;; student writing about economics and then about history still looks like
;; themselves.
;;
;; All probabilities are exact fractions. Multiplying thousands of word
;; probabilities would underflow a floating-point number to 0, but Roulette's
;; engine handles exact fractions, so every word of every essay is used.

(require racket/string
         racket/list
         racket/file
         racket/format
         racket/match)

(provide function-words
         students-with-essays
         read-freq-table
         student-model
         held-out-essay
         synthetic-essays
         build-model
         attribute
         verify
         run-experiment)

;; ---------------------------------------------------------------------------
;; data
;; ---------------------------------------------------------------------------

(define by-student-dir "BAWE_by_student")
(define train-dir      "rest_essay_frequency")
(define test-dir       "first_essay_frequency")
(define synthetic-dir  "synthetic_data_frequency")

;; The BAWE tables and the synthetic tables were lemmatized slightly
;; differently (BAWE: their -> they, our -> we, his -> he, keeps "its";
;; synthetic: keeps their/our/his, its -> it). Map both to the same form.
(define canonical
  (hash "an" "a"
        "its" "it"
        "their" "they" "them" "they" "theirs" "they"
        "our" "we" "ours" "we"
        "his" "he" "him" "he"
        "her" "she" "hers" "she"))

(define (canonicalize w) (hash-ref canonical w w))

;; Classic stylometry function words, as lemmas ("be" covers is/are/was/...,
;; "have" covers has/had, "they" covers their/them).
(define function-words
  '("a" "about" "after" "all" "also" "and" "any" "as" "at" "be" "but" "by"
    "can" "could" "do" "even" "for" "from" "have" "however" "if" "in" "into"
    "it" "may" "more" "must" "no" "not" "of" "on" "one" "only" "or" "other"
    "should" "so" "some" "such" "than" "that" "the" "then" "there"
    "therefore" "these" "they" "this" "those" "thus" "to" "upon" "we" "what"
    "when" "where" "which" "while" "who" "will" "with" "would"))

(define function-word-set (for/hash ([w (in-list function-words)]) (values w #t)))

;; Read a "word,frequency" CSV into a hash of function word -> count.
(define (read-freq-table path)
  (for*/fold ([h (hash)])
             ([line (in-list (rest (file->lines path)))]
              [m (in-value (regexp-match #px"^(.*),([0-9]+)\\s*$" line))]
              #:when m
              [w (in-value (canonicalize (string-downcase (string-trim (cadr m) "\""))))]
              #:when (hash-ref function-word-set w #f))
    (hash-update h w (lambda (c) (+ c (string->number (caddr m)))) 0)))

(define (csv-path dir id) (build-path dir (string-append id ".csv")))

(define (essay-count id)
  (length (filter (lambda (l) (non-empty-string? (string-trim l)))
                  (file->lines (build-path by-student-dir (string-append id ".txt"))))))

;; Student ids (sorted) that have at least `min-essays` essays.
(define (students-with-essays min-essays)
  (sort (for*/list ([f (in-list (directory-list by-student-dir))]
                    [name (in-value (path->string f))]
                    #:when (regexp-match? #rx"[.]txt$" name)
                    [id (in-value (substring name 0 (- (string-length name) 4)))]
                    #:when (file-exists? (csv-path train-dir id))
                    #:when (>= (essay-count id) min-essays))
          id)
        string<?))

(define (held-out-essay id) (read-freq-table (csv-path test-dir id)))

;; List of (name . counts) for every synthetic essay, e.g. ("claude_01" . #hash(...)).
(define (synthetic-essays)
  (for/list ([f (in-list (sort (map path->string (directory-list synthetic-dir)) string<?))]
             #:when (regexp-match? #rx"[.]csv$" f))
    (cons (substring f 0 (- (string-length f) 4))
          (read-freq-table (build-path synthetic-dir f)))))

;; An "abstract writer": exact probability of each function word, built from
;; one or more count tables, with add-one smoothing so unseen words are
;; unlikely but not impossible.
(define (build-model tables)
  (define counts
    (for*/fold ([h (hash)]) ([t (in-list tables)] [(w c) (in-hash t)])
      (hash-update h w (lambda (x) (+ x c)) 0)))
  (define total (+ (apply + (hash-values counts)) (length function-words)))
  (for/hash ([w (in-list function-words)])
    (values w (/ (+ 1 (hash-ref counts w 0)) total))))

(define (student-model id) (build-model (list (read-freq-table (csv-path train-dir id)))))

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
;; The literal version is: for each of the n occurrences,
;;   (observe! (equal? (make-categorical (author-model author)) w))
;; which multiplies author a's weight by p_a(w) each time. We do the same
;; thing with one soft observation per word type: (observe! (flip p))
;; multiplies the weight by p, so we use p = p_a(w)^n.
(define (observe-word-count! author models w n)
  (observe! (flip (lookup author (for/list ([m (in-list models)])
                                   (cons (car m) (expt (hash-ref (cdr m) w) n)))))))

;; `essay` is a count table: function word -> number of occurrences.
(define (observe-essay! author models essay)
  (for ([(w n) (in-hash essay)])
    (observe-word-count! author models w n)))

;; ATTRIBUTION: who, among these students, wrote `essay`?
;; `models` is a list of (student-id . model). Returns a pmf over student ids.
(define (attribute essay models)
  (query
   (define author (uniform-choice (map car models)))
   (observe-essay! author models essay)
   author))

;; VERIFICATION: did `student` write `essay`, or did someone else?
;; "Someone else" is modeled by `background`, a model built from many other
;; students. `prior` is how likely we think it is, before reading, that the
;; student wrote it. Returns a pmf over #t / #f.
(define (verify essay student-model background #:prior [prior 1/2])
  (query
   (define wrote-it (flip prior))
   (define models (list (cons #t student-model) (cons #f background)))
   (observe-essay! wrote-it models essay)
   wrote-it))

;; ---------------------------------------------------------------------------
;; experiment (milestone 1 plan)
;; ---------------------------------------------------------------------------
;;
;; For each of N students with >= 5 essays:
;;   - their model is built from every essay but the first
;;   - attribution: run `attribute` on their held-out first essay; correct if
;;     they come out on top
;;   - verification: run `verify` on (a) their own held-out essay, (b) the
;;     held-out essay of the next student in the list (a student impostor),
;;     and (c) one AI-written essay (an AI impostor)

(define (pmf-argmax d)
  (car (argmax cdr (for/list ([(v p) (in-pmf d)]) (cons v p)))))

(define (fmt p) (real->decimal-string p 3))

(define (run-experiment #:students [n 30] #:min-essays [min-essays 5])
  (define ids (take (students-with-essays min-essays) n))
  (define models (for/list ([id (in-list ids)]) (cons id (student-model id))))
  (define held-out (for/hash ([id (in-list ids)]) (values id (held-out-essay id))))
  (define background
    (build-model (for/list ([id (in-list ids)]) (read-freq-table (csv-path train-dir id)))))
  (define ai (synthetic-essays))

  (printf "~a students, ~a function words, ~a AI essays\n\n" n (length function-words) (length ai))
  (printf "student  top guess  P(top)  P(true)  P(own)  P(student impostor)  AI essay     P(AI)\n")
  (define rows
    (for/list ([id (in-list ids)]
               [impostor (in-list (append (rest ids) (list (first ids))))]
               [i (in-naturals)])
      (define model (cdr (assoc id models)))
      (define d (attribute (hash-ref held-out id) models))
      (define guess (pmf-argmax d))
      (define own ((verify (hash-ref held-out id) model background) #t))
      (define fake ((verify (hash-ref held-out impostor) model background) #t))
      (match-define (cons ai-name ai-essay) (list-ref ai (modulo i (length ai))))
      (define ai-p ((verify ai-essay model background) #t))
      (printf "~a     ~a       ~a   ~a    ~a   ~a                ~a  ~a\n"
              id guess (fmt (d guess)) (fmt (d id)) (fmt own) (fmt fake)
              (~a ai-name #:min-width 11) (fmt ai-p))
      (list (equal? guess id) (> own 1/2) (<= fake 1/2) (<= ai-p 1/2))))
  (define (pct k) (real->decimal-string (* 100.0 (/ (count values (map k rows)) n)) 1))
  (printf "\nattribution accuracy (1 of ~a):          ~a%\n" n (pct first))
  (printf "verification, own essay accepted:        ~a%\n" (pct second))
  (printf "verification, student impostor rejected: ~a%\n" (pct third))
  (printf "verification, AI essay rejected:         ~a%\n" (pct fourth)))

;; ---------------------------------------------------------------------------
;; run it
;; ---------------------------------------------------------------------------

;; Check one essay's frequency table against one student's model:
;;   racket authorship.rkt synthetic_data_frequency/claude_01.csv 0001
(define (check-file path id #:min-essays [min-essays 5])
  (define ids (take (students-with-essays min-essays) 30))
  (define background
    (build-model (for/list ([i (in-list ids)]) (read-freq-table (csv-path train-dir i)))))
  (define d (verify (read-freq-table path) (student-model id) background))
  (printf "P(student ~a wrote ~a) = ~a\n" id path (real->decimal-string (d #t) 4)))

(module+ main
  (match (current-command-line-arguments)
    [(vector) (run-experiment #:students 30)]
    [(vector path id) (check-file path id)]
    [_ (displayln "usage: racket authorship.rkt [essay-frequency.csv student-id]")]))
