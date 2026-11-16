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
;; Features: the 100 most frequent words across the students' training essays
;; (`top-n`). Every essay, student or AI, is compared on that same list; all
;; other words are ignored. Only training essays pick the list, so held-out
;; and AI essays don't influence which words are used.
;;
;; Every occurrence of a top-100 word is observed with
;;   (observe! (equal? (make-categorical ...) w))
;; and Roulette does all of the inference. Probabilities are exact fractions,
;; so multiplying thousands of them never underflows.

(require racket/string
         racket/list
         racket/file
         racket/format
         racket/match
         racket/sequence)

(provide students-with-essays
         top-words
         restrict
         read-freq-table
         top-n
         smoothing
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

;; Read a "word,frequency" CSV into a hash of word -> count.
(define (read-freq-table path)
  (for*/fold ([h (hash)])
             ([line (in-list (rest (file->lines path)))]
              [m (in-value (regexp-match #px"^(.*),([0-9]+)\\s*$" line))]
              #:when m
              [w (in-value (canonicalize (string-downcase (string-trim (cadr m) "\""))))])
    (hash-update h w (lambda (c) (+ c (string->number (caddr m)))) 0)))

;; How many of the most frequent words to use as features.
(define top-n (make-parameter 100))

;; The `n` most frequent words across `tables` (ties broken alphabetically).
(define (top-words tables [n (top-n)])
  (define totals
    (for*/fold ([h (hash)]) ([t (in-list tables)] [(w c) (in-hash t)])
      (hash-update h w (lambda (x) (+ x c)) 0)))
  (define ranked
    (sort (hash->list totals)
          (lambda (a b) (or (> (cdr a) (cdr b))
                            (and (= (cdr a) (cdr b)) (string<? (car a) (car b)))))))
  (map car (take ranked (min n (length ranked)))))

;; Keep only the words in `words` (a list) from a count table.
(define (restrict table words)
  (for*/hash ([w (in-list words)] [c (in-value (hash-ref table w 0))] #:when (> c 0))
    (values w c)))

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

;; How many made-up occurrences each word gets before counting (add-alpha
;; smoothing). 1 is "add-one". With only the top 100 words, every student has
;; many real occurrences of each, so add-one barely changes anything.
(define smoothing (make-parameter 1))

;; An "abstract writer": exact probability of every word, built from one or
;; more count tables, smoothed over a vocabulary of `vocab` words so unseen
;; words are unlikely but not impossible:
;;   p(w) = (count(w) + alpha) / (total + alpha * vocab)
;; Stored as (counts alpha . denominator) rather than one entry per word.
(define (build-model tables vocab)
  (define counts
    (for*/fold ([h (hash)]) ([t (in-list tables)] [(w c) (in-hash t)])
      (hash-update h w (lambda (x) (+ x c)) 0)))
  (define alpha (smoothing))
  (list* counts alpha (+ (apply + (hash-values counts)) (* alpha vocab))))

(define (word-prob model w)
  (match-define (list* counts alpha denom) model)
  (/ (+ alpha (hash-ref counts w 0)) denom))

;; ---------------------------------------------------------------------------
;; the probabilistic model
;; ---------------------------------------------------------------------------

;; Given a (symbolic) author and a table author -> value, return that author's
;; value. This is the "look up the key" step; Roulette tracks every branch.
(define (lookup author table)
  (let loop ([entries table])
    (match entries
      [(list (cons _ v)) v]
      [(cons (cons a v) rest) (if (equal? author a) v (loop rest))])))

;; The abstract student: given the (unknown) author, draw a word using that
;; author's frequencies. This is the "letter-dist" of the Caesar example.
;;
;; The draw is only ever compared with the observed word `w`, so each
;; author's categorical has two outcomes, `w` and 'other, where 'other
;; stands for "any of the other words". P(draw = w) is exactly the same as
;; with all words listed separately, so the posterior is identical. It
;; matters for speed: make-categorical builds a tree of flips (~99 for 100
;; words), and since the author is unknown Roulette builds one per author
;; for every word, so the full version ran out of memory on a 10-word test.
;; The two-outcome version is one flip per author.
(define (word-dist author models w)
  (lookup author
          (for/list ([m (in-list models)])
            (define p (word-prob (cdr m) w))
            (cons (car m) (make-categorical (list (cons w p) (cons 'other (- 1 p))))))))

;; How many words to observe per query (see `infer-author`).
(define chunk-size (make-parameter 200))

;; Every word occurrence in a count table (word -> count), as a list.
(define (essay-words essay)
  (for*/list ([(w n) (in-hash essay)] [_ (in-range n)]) w))

;; Posterior over authors after reading `essay`, starting from `prior`
;; (a list of (author . probability)). `models` maps each author to a model.
;;
;; Each word is observed exactly like a ciphertext letter:
;;   (observe! (equal? (enc (letter-dist) key) 20))     ; Caesar
;;   (observe! (equal? (word-dist author models w) w))  ; here
;; and Roulette does the inference.
;;
;; Essays are read in chunks of `chunk-size` words: observe a chunk, take the
;; posterior, and use it as the prior for the next chunk (sequential Bayesian
;; updating, the same "learning from data" as the coin example). Observing
;; a whole long essay in one query makes Roulette's exact fractions grow
;; without bound; a 12,000-word essay ran the engine out of memory. Between
;; chunks the posterior is rounded to double precision (~16 digits) and
;; turned back into an exact fraction, so each chunk starts small.
;;
;; With very large fractions the engine's results carry rounding error: on
;; long essays some authors came back with posteriors like -1e-53. Anything
;; that small is noise (at least 1e16 times below the leading author), so
;; authors whose posterior is not positive are dropped and the rest are
;; renormalized to sum to 1.
(define (infer-author prior models essay)
  (define final
    (for/fold ([prior prior]) ([chunk (in-slice (chunk-size) (essay-words essay))])
      (clear-cache!)
      (define d
        (query
         (define author (make-categorical prior))
         (for ([w (in-list chunk)])
           (observe! (equal? (word-dist author models w) w)))
         author))
      (define kept
        (for/list ([(a p) (in-pmf d)] #:when (positive? p))
          (cons a (inexact->exact (exact->inexact p)))))
      (define total (apply + (map cdr kept)))
      (for/list ([a+p (in-list kept)]) (cons (car a+p) (/ (cdr a+p) total)))))
  (pmf (for/hash ([a+p (in-list final)]) (values (car a+p) (cdr a+p)))))

;; ATTRIBUTION: who, among these students, wrote `essay`?
;; `models` is a list of (student-id . model). Starts from a uniform prior,
;; like the uniform prior on Caesar keys. Returns a pmf over student ids.
(define (attribute essay models)
  (infer-author (for/list ([m (in-list models)]) (cons (car m) (/ 1 (length models))))
                models essay))

;; VERIFICATION: did `student` write `essay`, or did someone else?
;; "Someone else" is modeled by `background`, a model built from many other
;; students. `prior` is how likely we think it is, before reading, that the
;; student wrote it. Returns a pmf over #t / #f.
(define (verify essay student-model background #:prior [prior 1/2])
  (infer-author (list (cons #t prior) (cons #f (- 1 prior)))
                (list (cons #t student-model) (cons #f background))
                essay))

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
  ;; Read every table once, pick the top words from the training essays, and
  ;; cut every table down to those words.
  (define ids (take (students-with-essays min-essays) n))
  (define raw-train (for/hash ([id (in-list ids)]) (values id (read-freq-table (csv-path train-dir id)))))
  (define words (top-words (hash-values raw-train)))
  (define vocab (length words))
  (define train (for/hash ([(id t) (in-hash raw-train)]) (values id (restrict t words))))
  (define held-out (for/hash ([id (in-list ids)]) (values id (restrict (held-out-essay id) words))))
  (define ai (for/list ([e (in-list (synthetic-essays))]) (cons (car e) (restrict (cdr e) words))))
  (define models (for/list ([id (in-list ids)]) (cons id (build-model (list (hash-ref train id)) vocab))))
  (define background (build-model (hash-values train) vocab))

  (printf "~a students, top ~a words, smoothing ~a, ~a AI essays\n\n"
          n vocab (smoothing) (length ai))
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
      (flush-output) ; show each row as soon as it's done
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
  (define raw-train (for/list ([i (in-list ids)]) (read-freq-table (csv-path train-dir i))))
  (define words (top-words raw-train))
  (define vocab (length words))
  (define train (for/list ([t (in-list raw-train)]) (restrict t words)))
  (define own (restrict (read-freq-table (csv-path train-dir id)) words))
  (define essay (restrict (read-freq-table path) words))
  (define d (verify essay (build-model (list own) vocab) (build-model train vocab)))
  (printf "P(student ~a wrote ~a) = ~a\n" id path (real->decimal-string (d #t) 4)))

;; racket authorship.rkt [--top N] [--smoothing A] [essay-frequency.csv student-id]
(module+ main
  (define usage
    "usage: racket authorship.rkt [--top N] [--smoothing A] [essay-frequency.csv student-id]")
  (let loop ([args (vector->list (current-command-line-arguments))])
    (match args
      [(list* "--top" n more) (top-n (string->number n)) (loop more)]
      ;; read "0.01" as the exact fraction 1/100, so probabilities stay exact
      [(list* "--smoothing" a more)
       (smoothing (string->number a 10 'number-or-false 'decimal-as-exact))
       (loop more)]
      [(list) (run-experiment #:students 30)]
      [(list path id) (check-file path id)]
      [_ (displayln usage)])))
