(defpackage :lost-and-found
  (:nicknames :maze)
  (:use :clamp :experimental :iter)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod))

(in-package :maze)
(syntax:use-syntax :clamp)

(defparameter rows* 4 "The number of rows.")
(defparameter cols* 16 "The number of cols.")
(defparameter prob* .7
  "The probability of a square being a moveable square when generating a random maze.")
(defparameter dirs->vects* '((n (-1 0)) (e (0 1)) (s (1 0)) (w (0 -1)))
  "An alist of all of the different directions and vectors for that direction.")

(defparameter dirs* (map #'car dirs->vects*)
  "A list containing all of the different directions.")
(defparameter agent-type* 'ai-agent)
(defparameter faultiness* .2
  "The probability of a sensor giving an incorrect result.")
(defparameter print-maze* t
  "True if the maze should be printed after every turn.")

(defparameter book-maze* '(o o o o * o o o o o * o o o * o
                           * * o o * o * * o * o * o * * *
                           * o o o * o * * o o o o o * * o
                           o o * o o o * o o o o * o o o o)
  "The maze from the book AIMA.")

(defstruct (point (:conc-name nil) (:type list) (:constructor make-point (row col)))
  row col)

(deftem (maze (:conc-name nil)) grid agent-loc)

(deftem (agent (:conc-name nil)) (turn-number 0))
(deftem (human (:include agent)))
(deftem (ai-agent (:conc-name nil) (:include agent))
  (grid (error "Agent has no grid."))
  mapping
  state
  transition
  done)
(deftem (faulty-ai-agent (:include ai-agent)))

(defmethod get ((arr array) (p list))
  (apply #'aref arr p))

(defmethod (setf get) (val (arr array) (p list))
  (= (apply #'aref arr p) val))

(defmethod get ((b maze) (p list))
  (get b!grid p))

(defmethod (setf get) (val (b maze) (p list))
  (setf (get b!grid p) val))

(defmethod get ((b maze) (p number))
  (get b!grid p))

(defmethod (setf get) (val (b maze) (p number))
  (setf (get b!grid p) val))

(def p+ (p1 p2)
  "Adds two points together."
  (map #'+ p1 p2))

(def valid (p)
  "Is this point valid?"
  (and (<= 0 p!row (dec rows*))
       (<= 0 p!col (dec cols*))))

(def next-point (p dir)
  "Return the next point that is in the direction DIR of the point P."
  (check-type dir (member n e s w))
  (p+ p (alref dirs->vects* dir)))

(def moveable (maze p)
  "Is the given point in the maze a valid square to move to?"
  (and (valid p) maze.p)) ; We are representing the spaces we can move to with T.

(def pos-moves (maze p)
  "Returns a list of all of the directions the agent can move."
  (accum a
    (each dir dirs*
      (let new (next-point p dir)
        (when (moveable maze new)
          (a dir))))))

(defmethod print-object ((maze maze) stream)
  (let *standard-output* stream
    (sp)
    (repeat cols* (pr "-"))
    (sp)
    (prn)
    (up r 0 rows*
      (pr "|")
      (up c 0 cols*
        (pr (case maze.r.c
              (t)       "o" ; the symbol t.
              (nil)     "*"
              otherwise "A")))
      (prn "|"))
    (sp)
    (repeat cols* (pr "-"))
    (sp)
    (prn)))

(defmethod initialize-instance :after ((maze maze) &key)
  (or= maze!grid (gen-rand-maze))
  (zap [check _ [isa _ 'array] (list-maze->grid _)] maze!grid)
  (= maze!agent-loc (rand-empty-square maze!grid))
  (= (get maze maze!agent-loc) (inst agent-type* :grid (copy-array maze!grid))))

(def rand-empty-square (maze)
  "Returns a random empty square in the maze."
  (rand-elt
    (accum a
      (up r 0 rows*
        (up c 0 cols*
          (when maze.r.c
            (a (make-point r c))))))))

(def gen-rand-maze ()
  "Generates a random maze."
  (list-maze->grid (n-of (* rows* cols*)
                         (if (< (rand 1.0) prob*) 'o '*))))

(def list-maze->grid (list-maze)
  "Converts a maze in list form to grid form."
  (ret result (make-array (list rows* cols*) :initial-element nil)
    (iter (generating loc in list-maze)
          (up r 0 rows*
            (up c 0 cols*
              (when (is (next loc) 'o)
                (= result.r.c t)))))))

(def truthify (x)
  "Convert X into either T or NIL."
  (if x t nil))

(defmethod get-percept ((maze maze) agent)
  "Get the percept for an agent in a maze."
  (let pos (pos-moves maze maze!agent-loc)
    (accum a
      (each dir dirs*
        (a (truthify (mem dir pos)))))))

(defmethod get-action (maze (agent ai-agent) percept)
  "For the default, make a random move."
  (++ agent!turn-number)
  (rand-elt (or (pos-moves agent!grid maze!agent-loc)
                '(dont-move))))

(def move (maze dir)
  "Move the agent in the MAZE in the direction DIR."
  (unless (is dir 'dont-move)
    (let next-loc (next-point maze!agent-loc dir)
      (shiftf maze.next-loc (get maze maze!agent-loc) t)
      (= maze!agent-loc next-loc)
      maze)))

(def agent (maze)
  "Get the agent in the maze."
  (get maze maze!agent-loc))

(def turn (maze)
  "Step forward a single time step in the maze."
  (let percept (get-percept maze maze!agent)
    (unless (finished maze!agent)
      (move maze (get-action maze maze!agent percept))
      (tell-percept maze!agent percept))
    maze))

(defmethod finished (agent)
  "Is this agent finished?"
  nil)

(def copy-array (arr)
  (make-array (array-dimensions arr)
              :displaced-to (copy-seq (make-array (array-total-size arr)
                                                  :displaced-to arr))))

(defmethod initialize-instance :after ((agent ai-agent) &key)
  "We need to set the map and the possible state."
  (let count 0
    (= agent!mapping (accum a
                       (up r 0 rows*
                         (up c 0 cols*
                           (when agent!grid.r.c
                             (a (list r c))
                             (++ count))))))
    (= agent!state (make-array (list count 1) :initial-element (/ 1.0 count)))
    (= agent!transition (build-transition-matrix agent!grid agent!mapping count))
    agent))

(def build-transition-matrix (grid mapping count)
  "Build the transition matrix for the markov chain."
  (ret result (make-array (list count count) :initial-element 0)
    (up r 0 rows*
      (up c 0 cols*
        (let p (make-point r c)
          (awhen (pos p mapping)
            (let pos (pos-moves grid p)
              (when (no pos)
                (= result.it.it 1.0))
              (each move pos
                (++ (get result.it
                         (pos (next-point p move)
                              mapping))
                    (/ 1.0 (len pos)))))))))))

(def transpose (arr)
  "Transposes an array."
  (ret result (make-array (reverse (array-dimensions arr)))
    (iter (for r from 0 below (array-dimension arr 0))
          (iter (for c from 0 below (array-dimension arr 1))
                (= result.r.c arr.c.r)))))

(defmethod get-sensor-matrix ((agent ai-agent) percept)
  "Returns the sensor model matrix for the agent with the given percept."
  (let side (len agent!mapping)
    (ret result (make-array (list side side) :initial-element 0)
      (iter (for point in agent!mapping)
            (for i from 0)
            (= result.i.i (prob-percept agent percept point))))))

(def set-equal (x y)
  "Are these two sets equivalent?"
  (and (subsetp x y)
       (subsetp y x)))

(defmethod prob-percept ((agent ai-agent) percept point)
  "Returns the probability of the agent receiving the percept at the
   given point."
  (if (set-equal (pos-moves agent!grid point)
                 (ado (map #'list percept dirs*)
                      (keep #'car it)
                      (map #'cadr it)))
      1
      0))

(def normalize (arr)
  "Normalizes a vector."
  (let sum (reduce-array #'+ arr)
    (map-array [/ _ sum] arr)))

(defmethod tell-percept ((agent ai-agent) percept)
  (let new-state (normalize (mmult (get-sensor-matrix agent percept)
                                   (mmult (transpose agent!transition)
                                          agent!state)))
    (if (iso new-state agent!state)
        (set agent!done)
        (= agent!state new-state))))

(def mmult (a b)
  "Multiply two matricies."
  (assert (is (array-dimension a 1) (array-dimension b 0)))
  (ret result (make-array (list (array-dimension a 0)
                                (array-dimension b 1))
                          :initial-element 0)
    (up r 0 (array-dimension a 0)
      (up c 0 (array-dimension b 1)
        (up k 0 (array-dimension a 1)
          (++ result.r.c (* a.r.k b.k.c)))))))

(defmethod finished ((agent ai-agent))
  (if agent!done
      (let arr (linearlize agent!state)
        (ado (reduce #'max arr)
             (pos it arr)
             (elt agent!mapping it)))
      (awhen (pos [> _ 0.9] (linearlize agent!state))
        (elt agent!mapping it))))

(def map-array (fn arr)
  "Maps FN over the array ARR."
  (make-array (array-dimensions arr)
              :displaced-to (mapv fn (make-array (array-total-size arr)
                                                 :displaced-to arr))))

(def reduce-array (fn arr)
  (reduce fn (linearlize arr)))

(def linearlize (arr)
  (make-array (array-total-size arr) :displaced-to arr))

(defmethod show-result ((agent ai-agent))
  (let loc (finished agent)
    (prf "I think I am currently on row ~A col ~A." loc!row loc!col)
    agent!turn-number))

(def play ()
  (let maze (make-maze)
    (until (finished maze!agent)
      (when print-maze*
        (print maze))
      (turn maze))
    (show-result maze!agent)))

(defmethod get-percept ((maze maze) (agent faulty-ai-agent))
  "Get the percept for an agent in a maze."
  (let pos (pos-moves maze maze!agent-loc)
    (accum a
      (each dir dirs*
        (let result (truthify (mem dir pos))
          (when (< (rand 1.0) faultiness*)
            (= result (not result)))
          (a result))))))

(defmethod prob-percept ((agent faulty-ai-agent) percept point)
  "Returns the probability of the agent receiving the percept at the
   given point."
  (ado (pos-moves agent!grid point)
       (map (fn (dir) (truthify (mem dir it))) dirs*)
       (map #'list it percept)
       (count [apply ~is _] it)
       (* (expt (- 1 faultiness*) (- 4 it))
          (expt faultiness* it))))

(def simulate (n agents)
  (let print-maze* nil
    (ret results nil
      (each agent-type* agents
        (let count 0
             (repeat n
               (++ count (play)))
             (push (list agent-type* (float (/ count n)))
                   results))))))

(def test-faultiness (n vals)
  (with (print-maze* nil agent-type* 'faulty-ai-agent)
    (ret results nil
      (each faultiness* vals
        (let count 0
             (repeat n
               (++ count (play)))
             (push (list faultiness* (float (/ count n)))
                   results))))))
