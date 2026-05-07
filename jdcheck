#! /usr/bin/env janet

(comment import ./args :prefix "")
(defn a/parse-args
  [args]
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  (array/remove the-args 0)
  #
  (when (or (not head) (= head "-h") (= head "--help"))
    (break {:show-help true}))
  #
  (when (or (not head) (= head "-v") (= head "--version"))
    (break {:show-version true}))
  #
  (def default-opts @{:rest the-args})
  #
  (def opts
    (if-not (and (string/has-prefix? "{" head)
                 (string/has-suffix? "}" head))
      @{:rest (array head ;the-args)}
      (let [parsed
            (try (parse (string "@" head))
              ([e] (eprint e)
                   (errorf "failed to parse options: %n" head)))]
        (assertf (and parsed (table? parsed))
                 "expected table but found: %s" (type parsed))
        (merge default-opts parsed))))
  #
  opts)


(comment import ./cm-zipper :prefix "")
(comment import ./helpers :prefix "")
# based on code by corasaurus-hex

# `slice` doesn't necessarily preserve the input type

# XXX: differs from clojure's behavior
#      e.g. (butlast [:a]) would yield nil(?!) in clojure
(defn h/butlast
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 0 -2)
      (array/slice indexed 0 -2))))

(comment

  (h/butlast @[:a :b :c])
  # =>
  @[:a :b]

  (h/butlast [:a])
  # =>
  []

  )

(defn h/rest
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 1 -1)
      (array/slice indexed 1 -1))))

(comment

  (h/rest [:a :b :c])
  # =>
  [:b :c]

  (h/rest @[:a])
  # =>
  @[]

  )

# XXX: can pass in array - will get back tuple
(defn h/tuple-push
  [tup x & xs]
  (if tup
    [;tup x ;xs]
    [x ;xs]))

(comment

  (h/tuple-push [:a :b] :c)
  # =>
  [:a :b :c]

  (h/tuple-push nil :a)
  # =>
  [:a]

  (h/tuple-push @[] :a)
  # =>
  [:a]

  )

(defn h/to-entries
  [val]
  (if (dictionary? val)
    (pairs val)
    val))

(comment

  (sort (h/to-entries {:a 1 :b 2}))
  # =>
  @[[:a 1] [:b 2]]

  (h/to-entries {})
  # =>
  @[]

  (h/to-entries @{:a 1})
  # =>
  @[[:a 1]]

  # XXX: leaving non-dictionaries alone and passing through...
  #      is this desirable over erroring?
  (h/to-entries [:a :b :c])
  # =>
  [:a :b :c]

  )

# XXX: when xs is empty, "all" becomes nil
(defn h/first-rest-maybe-all
  [xs]
  (if (or (nil? xs) (empty? xs))
    [nil nil nil]
    [(first xs) (h/rest xs) xs]))

(comment

  (h/first-rest-maybe-all [:a :b])
  # =>
  [:a [:b] [:a :b]]

  (h/first-rest-maybe-all @[:a])
  # =>
  [:a @[] @[:a]]

  (h/first-rest-maybe-all [])
  # =>
  [nil nil nil]

  # XXX: is this what we want?
  (h/first-rest-maybe-all nil)
  # =>
  [nil nil nil]

  )



########################################################################

(defn c/zipper
  ``
  Returns a new zipper consisting of two elements:

  * `a-root` - the passed in root node.
  * `state` - table of info about node's z-location in the tree with keys:
    * `:ls` - left siblings
    * `:pnodes` - path of nodes from root to current z-location
    * `:pstate` - parent node's state
    * `:rs` - right siblings
    * `:changed?` - indicates whether "editing" has occured

  `state` has a prototype table with four functions:

  * :branch? - fn that tests if a node is a branch (has children)
  * :children - fn that returns the child nodes for the given branch.
  * :make-node - fn that takes a node + children and returns a new branch
    node with the same.
  * :make-state - fn for creating a new state
  ``
  [a-root branch?-fn children-fn make-node-fn]
  #
  (defn make-state_
    [&opt ls_ rs_ pnodes_ pstate_ changed?_]
    (table/setproto @{:ls ls_
                      :pnodes pnodes_
                      :pstate pstate_
                      :rs rs_
                      :changed? changed?_}
                    @{:branch? branch?-fn
                      :children children-fn
                      :make-node make-node-fn
                      :make-state make-state_}))
  #
  [a-root (make-state_)])

(comment

  # XXX

  )

(defn c/indexed-zip
  ``
  Returns a zipper for nested indexed data structures (tuples
  or arrays), given a root data structure.
  ``
  [indexed]
  (c/zipper indexed
          indexed?
          h/to-entries
          (fn [_p xs] xs)))

(comment

  (def a-node
    [:x [:y :z]])

  (def [the-node the-state]
    (c/indexed-zip a-node))

  the-node
  # =>
  a-node

  # merge is used to "remove" the prototype table of `st`
  (merge {} the-state)
  # =>
  @{}

  )

(defn c/node
  "Returns the node at `zloc`."
  [zloc]
  (get zloc 0))

(comment

  (c/node (c/indexed-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn c/state
  "Returns the state for `zloc`."
  [zloc]
  (get zloc 1))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (-> (c/indexed-zip [:a [:b [:x :y]]])
             c/state))
  # =>
  @{}

  )

(defn c/branch?
  ``
  Returns true if the node at `zloc` is a branch.
  Returns false otherwise.
  ``
  [zloc]
  (((c/state zloc) :branch?) (c/node zloc)))

(comment

  (c/branch? (c/indexed-zip [:a :b [:x :y]]))
  # =>
  true

  )

(defn c/children
  ``
  Returns children for a branch node at `zloc`.
  Otherwise throws an error.
  ``
  [zloc]
  (if (c/branch? zloc)
    (((c/state zloc) :children) (c/node zloc))
    (error "Called `children` on a non-branch zloc")))

(comment

  (c/children (c/indexed-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn c/make-state
  ``
  Convenience function for calling the :make-state function for `zloc`.
  ``
  [zloc &opt ls rs pnodes pstate changed?]
  (((c/state zloc) :make-state) ls rs pnodes pstate changed?))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (c/make-state (c/indexed-zip [:a :b [:x :y]])))
  # =>
  @{}

  )

(defn c/down
  ``
  Moves down the tree, returning the leftmost child z-location of
  `zloc`, or nil if there are no children.
  ``
  [zloc]
  (when (c/branch? zloc)
    (let [[z-node st] zloc
          [k rest-kids kids]
          (h/first-rest-maybe-all (c/children zloc))]
      (when kids
        [k
         (c/make-state zloc
                     []
                     rest-kids
                     (if (not (empty? st))
                       (h/tuple-push (get st :pnodes) z-node)
                       [z-node])
                     st
                     (get st :changed?))]))))

(comment

  (c/node (c/down (c/indexed-zip [:a :b [:x :y]])))
  # =>
  :a

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/branch?)
  # =>
  false

  (try
    (-> (c/indexed-zip [:a])
        c/down
        c/children)
    ([e] e))
  # =>
  "Called `children` on a non-branch zloc"

  (deep=
    #
    (merge {}
           (-> [:a [:b [:x :y]]]
               c/indexed-zip
               c/down
               c/state))
    #
    '@{:ls ()
       :pnodes ((:a (:b (:x :y))))
       :pstate @{}
       :rs ((:b (:x :y)))})
  # =>
  true

  )

(defn c/right
  ``
  Returns the z-location of the right sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st
        [r rest-rs rs_] (h/first-rest-maybe-all rs)]
    (when (and (not (empty? st)) rs_)
      [r
       (c/make-state zloc
                   (h/tuple-push ls z-node)
                   rest-rs
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (c/indexed-zip [:a :b])
      c/down
      c/right
      c/node)
  # =>
  :b

  (-> (c/indexed-zip [:a])
      c/down
      c/right)
  # =>
  nil

  )

(defn c/make-node
  ``
  Returns a branch node, given `zloc`, `a-node` and `kids`.
  ``
  [zloc a-node kids]
  (((c/state zloc) :make-node) a-node kids))

(comment

  (c/make-node (c/indexed-zip [:a :b [:x :y]])
             [:a :b] [:x :y])
  # =>
  [:x :y]

  )

(defn c/up
  ``
  Moves up the tree, returning the parent z-location of `zloc`,
  or nil if at the root z-location.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs
         :changed? changed?} st]
    (when pnodes
      (let [pnode (last pnodes)]
        (if changed?
          [(c/make-node zloc pnode [;ls z-node ;rs])
           (c/make-state zloc
                       (get pstate :ls)
                       (get pstate :rs)
                       (get pstate :pnodes)
                       (get pstate :pstate)
                       true)]
          [pnode pstate])))))

(comment

  (def m-zip
    (c/indexed-zip [:a :b [:x :y]]))

  (deep=
    (-> m-zip
        c/down
        c/up)
    m-zip)
  # =>
  true

  (deep=
    (-> m-zip
        c/down
        c/right
        c/right
        c/down
        c/up
        c/up)
    m-zip)
  # =>
  true

  )

# XXX: used by `root` and `df-next`
(defn c/end?
  "Returns true if `zloc` represents the end of a depth-first walk."
  [zloc]
  (= :end (c/state zloc)))

(defn c/root
  ``
  Moves all the way up the tree for `zloc` and returns the node at
  the root z-location.
  ``
  [zloc]
  (if (c/end? zloc)
    (c/node zloc)
    (if-let [p (c/up zloc)]
      (c/root p)
      (c/node zloc))))

(comment

  (def a-zip
    (c/indexed-zip [:a :b [:x :y]]))

  (c/node a-zip)
  # =>
  (-> a-zip
      c/down
      c/right
      c/right
      c/down
      c/root)

  )

(defn c/df-next
  ``
  Moves to the next z-location, depth-first.  When the end is
  reached, returns a special z-location detectable via `end?`.
  Does not move if already at the end.
  ``
  [zloc]
  #
  (defn recur
    [a-loc]
    (if (c/up a-loc)
      (or (c/right (c/up a-loc))
          (recur (c/up a-loc)))
      [(c/node a-loc) :end]))
  #
  (if (c/end? zloc)
    zloc
    (or (and (c/branch? zloc) (c/down zloc))
        (c/right zloc)
        (recur zloc))))

(comment

  (def a-zip
    (c/indexed-zip [:a :b [:x]]))

  (c/node (c/df-next a-zip))
  # =>
  :a

  (-> a-zip
      c/df-next
      c/df-next
      c/node)
  # =>
  :b

  (-> a-zip
      c/df-next
      c/df-next
      c/df-next
      c/df-next
      c/df-next
      c/end?)
  # =>
  true

  )

(defn c/replace
  "Replaces existing node at `zloc` with `a-node`, without moving."
  [zloc a-node]
  (let [[_ st] zloc]
    [a-node
     (c/make-state zloc
                 (get st :ls)
                 (get st :rs)
                 (get st :pnodes)
                 (get st :pstate)
                 true)]))

(comment

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      (c/replace :w)
      c/root)
  # =>
  [:w :b [:x :y]]

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/right
      c/right
      c/down
      (c/replace :w)
      c/root)
  # =>
  [:a :b [:w :y]]

  )

(defn c/edit
  ``
  Replaces the node at `zloc` with the value of `(f node args)`,
  where `node` is the node associated with `zloc`.
  ``
  [zloc f & args]
  (c/replace zloc
           (apply f (c/node zloc) args)))

(comment

  (-> (c/indexed-zip [1 2 [8 9]])
      c/down
      (c/edit inc)
      c/root)
  # =>
  [2 2 [8 9]]

  (-> (c/indexed-zip [1 2 [8 9]])
      c/down
      (c/edit inc)
      c/right
      (c/edit inc)
      c/right
      c/down
      (c/edit dec)
      c/right
      (c/edit dec)
      c/root)
  # =>
  [2 3 [7 8]]

  )

(defn c/insert-child
  ``
  Inserts `child` as the leftmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (c/replace zloc
           (c/make-node zloc
                      (c/node zloc)
                      [child ;(c/children zloc)])))

(comment

  (-> (c/indexed-zip [:a :b [:x :y]])
      (c/insert-child :c)
      c/root)
  # =>
  [:c :a :b [:x :y]]

  )

(defn c/append-child
  ``
  Appends `child` as the rightmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (c/replace zloc
           (c/make-node zloc
                      (c/node zloc)
                      [;(c/children zloc) child])))

(comment

  (-> (c/indexed-zip [:a :b [:x :y]])
      (c/append-child :c)
      c/root)
  # =>
  [:a :b [:x :y] :c]

  )

(defn c/rightmost
  ``
  Returns the z-location of the rightmost sibling of the node at
  `zloc`, or the current node's z-location if there are none to the
  right.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? rs)
             (not (empty? rs)))
      [(last rs)
       (c/make-state zloc
                   (h/tuple-push ls z-node ;(h/butlast rs))
                   []
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/rightmost
      c/node)
  # =>
  [:x :y]

  )

(defn c/remove
  ``
  Removes the node at `zloc`, returning the z-location that would have
  preceded it in a depth-first walk.  Throws an error if called at the
  root z-location.
  ``
  [zloc]
  (let [[_z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs} st]
    #
    (defn recur
      [a-zloc]
      (if-let [child (and (c/branch? a-zloc) (c/down a-zloc))]
        (recur (c/rightmost child))
        a-zloc))
    #
    (if (not (empty? st))
      (if (pos? (length ls))
        (recur [(last ls)
                (c/make-state zloc
                            (h/butlast ls)
                            rs
                            pnodes
                            pstate
                            true)])
        [(c/make-node zloc (last pnodes) rs)
         (c/make-state zloc
                     (get pstate :ls)
                     (get pstate :rs)
                     (get pstate :pnodes)
                     (get pstate :pstate)
                     true)])
      (error "Called `remove` at root"))))

(comment

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/right
      c/remove
      c/node)
  # =>
  :a

  (try
    (c/remove (c/indexed-zip [:a :b [:x :y]]))
    ([e] e))
  # =>
  "Called `remove` at root"

  )

(defn c/left
  ``
  Returns the z-location of the left sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (when (and (not (empty? st))
               (indexed? ls)
               (not (empty? ls)))
      [(last ls)
       (c/make-state zloc
                   (h/butlast ls)
                   [z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (c/indexed-zip [:a :b :c])
      c/down
      c/right
      c/right
      c/left
      c/node)
  # =>
  :b

  (-> (c/indexed-zip [:a])
      c/down
      c/left)
  # =>
  nil

  )

(defn c/df-prev
  ``
  Moves to the previous z-location, depth-first.
  If already at the root, returns nil.
  ``
  [zloc]
  #
  (defn recur
    [a-zloc]
    (if-let [child (and (c/branch? a-zloc)
                        (c/down a-zloc))]
      (recur (c/rightmost child))
      a-zloc))
  #
  (if-let [left-loc (c/left zloc)]
    (recur left-loc)
    (c/up zloc)))

(comment

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/right
      c/df-prev
      c/node)
  # =>
  :a

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/right
      c/right
      c/down
      c/df-prev
      c/node)
  # =>
  [:x :y]

  )

(defn c/insert-right
  ``
  Inserts `a-node` as the right sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (c/make-state zloc
                   ls
                   [a-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-right` at root"))))

(comment

  (def a-zip
    (c/indexed-zip [:a :b [:x :y]]))

  (-> a-zip
      c/down
      (c/insert-right :z)
      c/root)
  # =>
  [:a :z :b [:x :y]]

  (try
    (c/insert-right a-zip :e)
    ([e] e))
  # =>
  "Called `insert-right` at root"

  )

(defn c/insert-left
  ``
  Inserts `a-node` as the left sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (c/make-state zloc
                   (h/tuple-push ls a-node)
                   rs
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-left` at root"))))

(comment

  (def a-zip
    (c/indexed-zip [:a :b [:x :y]]))

  (-> a-zip
      c/down
      (c/insert-left :z)
      c/root)
  # =>
  [:z :a :b [:x :y]]

  (try
    (c/insert-left a-zip :e)
    ([e] e))
  # =>
  "Called `insert-left` at root"

  )

(defn c/rights
  "Returns siblings to the right of `zloc`."
  [zloc]
  (when-let [st (c/state zloc)]
    (get st :rs)))

(comment

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/rights)
  # =>
  [:b [:x :y]]

  (-> (c/indexed-zip [:a :b])
      c/down
      c/right
      c/rights)
  # =>
  []

  )

(defn c/lefts
  "Returns siblings to the left of `zloc`."
  [zloc]
  (if-let [st (c/state zloc)
           ls (get st :ls)]
    ls
    []))

(comment

  (-> (c/indexed-zip [:a :b])
      c/down
      c/lefts)
  # =>
  []

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/right
      c/right
      c/lefts)
  # =>
  [:a :b]

  )

(defn c/leftmost
  ``
  Returns the z-location of the leftmost sibling of the node at `zloc`,
  or the current node's z-location if there are no siblings to the left.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? ls)
             (not (empty? ls)))
      [(first ls)
       (c/make-state zloc
                   []
                   [;(h/rest ls) z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/leftmost
      c/node)
  # =>
  :a

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/rightmost
      c/leftmost
      c/node)
  # =>
  :a

  )

(defn c/path
  "Returns the path of nodes that lead to `zloc` from the root node."
  [zloc]
  (when-let [st (c/state zloc)]
    (get st :pnodes)))

(comment

  (c/path (c/indexed-zip [:a :b [:x :y]]))
  # =>
  nil

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/path)
  # =>
  [[:a :b [:x :y]]]

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/right
      c/right
      c/down
      c/path)
  # =>
  [[:a :b [:x :y]] [:x :y]]

  )

(defn c/right-until
  ``
  Try to move right from `zloc`, calling `pred` for each
  right sibling.  If the `pred` call has a truthy result,
  return the corresponding right sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [right-sib (c/right zloc)]
    (if (pred right-sib)
      right-sib
      (c/right-until right-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      c/indexed-zip
      c/down
      c/right
      c/down
      (c/right-until |(match (c/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true))
      c/node)
  # =>
  [:symbol "+"]

  )

(defn c/right-from-until
  ``
  Call `pred` on zloc, and if it fails, successively on
  each right sibling until a truthy result.

  Return the zloc corresponding to the one which `pred`
  returns a truthy result for, if any.  Otherwise, return
  nil.
  ``
  [zloc pred]
  (defn helper
    [a-zloc]
    (when-let [right-sib (c/right a-zloc)]
      (if (pred right-sib)
        right-sib
        (helper right-sib))))
  #
  (if (pred zloc)
    zloc
    (helper zloc)))

(comment

  (-> [:code
       [:bracket-tuple
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      c/indexed-zip
      c/down
      c/right
      c/down
      c/right
      (c/right-from-until |(match (c/node $)
                           [:number]
                           true
                           #
                           false))
      c/node)
  # =>
  [:number "1"]

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      c/indexed-zip
      c/down
      c/right
      c/down
      c/right
      (c/right-from-until |(match (c/node $)
                           [:number]
                           true
                           #
                           false))
      c/node)
  # =>
  [:number "1"]

  )

(defn c/left-until
  ``
  Try to move left from `zloc`, calling `pred` for each
  left sibling.  If the `pred` call has a truthy result,
  return the corresponding left sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [left-sib (c/left zloc)]
    (if (pred left-sib)
      left-sib
      (c/left-until left-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      c/indexed-zip
      c/down
      c/right
      c/down
      c/rightmost
      (c/left-until |(match (c/node $)
                     [:comment]
                     false
                     #
                     [:whitespace]
                     false
                     #
                     true))
      c/node)
  # =>
  [:number "1"]

  )

(defn c/search-from
  ``
  Successively call `pred` on z-locations starting at `zloc`
  in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (if (pred zloc)
    zloc
    (when-let [next-zloc (c/df-next zloc)]
      (when (c/end? next-zloc)
        (break nil))
      (c/search-from next-zloc pred))))

(comment

  (-> (c/indexed-zip [:a :b :c])
      c/down
      (c/search-from |(match (c/node $)
                      :b
                      true))
      c/node)
  # =>
  :b

  (-> (c/indexed-zip [:a :b :c])
      c/down
      (c/search-from |(match (c/node $)
                      :d
                      true)))
  # =>
  nil

  (-> (c/indexed-zip [:a :b :c])
      c/down
      (c/search-from |(match (c/node $)
                      :a
                      true))
      c/node)
  # =>
  :a

  )

(defn c/search-after
  ``
  Successively call `pred` on z-locations starting after
  `zloc` in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when (c/end? zloc)
    (break nil))
  (when-let [next-zloc (c/df-next zloc)]
    (if (pred next-zloc)
      next-zloc
      (c/search-after next-zloc pred))))

(comment

  (-> (c/indexed-zip [:b :a :b])
      c/down
      (c/search-after |(match (c/node $)
                       :b
                       true))
      c/left
      c/node)
  # =>
  :a

  (-> (c/indexed-zip [:b :a :b])
      c/down
      (c/search-after |(match (c/node $)
                       :d
                       true)))
  # =>
  nil

  (-> (c/indexed-zip [:a [:b :c [2 [3 :smile] 5]]])
      (c/search-after |(match (c/node $)
                       [_ :smile]
                       true))
      c/down
      c/node)
  # =>
  3

  )

(defn c/unwrap
  ``
  If the node at `zloc` is a branch node, "unwrap" its children in
  place.  If `zloc`'s node is not a branch node, do nothing.

  Throws an error if `zloc` corresponds to a top-most container.
  ``
  [zloc]
  (unless (c/branch? zloc)
    (break zloc))
  #
  (when (empty? (c/state zloc))
    (error "Called `unwrap` at root"))
  #
  (def kids (c/children zloc))
  (var i (dec (length kids)))
  (var curr-zloc zloc)
  (while (<= 0 i) # right to left
    (set curr-zloc
         (c/insert-right curr-zloc (get kids i)))
    (-- i))
  # try to end up at a sensible spot
  (set curr-zloc
       (c/remove curr-zloc))
  (if-let [ret-zloc (c/right curr-zloc)]
    ret-zloc
    curr-zloc))

(comment

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/right
      c/right
      c/unwrap
      c/root)
  # =>
  [:a :b :x :y]

  (-> (c/indexed-zip [:a :b [:x :y]])
      c/down
      c/unwrap
      c/root)
  # =>
  [:a :b [:x :y]]

  (-> (c/indexed-zip [[:a]])
      c/down
      c/unwrap
      c/root)
  # =>
  [:a]

  (-> (c/indexed-zip [[:a :b] [:x :y]])
      c/down
      c/down
      c/remove
      c/unwrap
      c/root)
  # =>
  [:b [:x :y]]

  (try
    (-> (c/indexed-zip [:a :b [:x :y]])
        c/unwrap)
    ([e] e))
  # =>
  "Called `unwrap` at root"

  )

(defn c/eq?
  ``
  Compare two zlocs, `a-zloc` and `b-zloc`, for equality.
  ``
  [a-zloc b-zloc]
  (and (= (length (c/lefts a-zloc)) (length (c/lefts b-zloc)))
       (= (c/path a-zloc) (c/path b-zloc))))

(comment

  (def iz (c/indexed-zip [:a :b :c :b]))

  (c/eq? (-> iz c/down c/right)
       (-> iz c/down c/right c/right c/right))
  # =>
  false

  (c/eq? (-> iz c/down c/right)
       (-> iz c/down c/right c/right c/right c/left c/left))
  # =>
  true

  )

(defn c/wrap
  ``
  Replace nodes from `start-zloc` through `end-zloc` with a single
  node of the same type as `wrap-node` containing the nodes from
  `start-zloc` through `end-zloc`.

  If `end-zloc` is not specified, just wrap `start-zloc`.

  The caller is responsible for ensuring the value of `end-zloc`
  is somewhere to the right of `start-zloc`.  Throws an error if
  an inappropriate value is specified for `end-zloc`.
  ``
  [start-zloc wrap-node &opt end-zloc]
  (default end-zloc start-zloc)
  #
  # 1. collect all nodes to wrap
  #
  (def kids @[])
  (var cur-zloc start-zloc)
  (while (and cur-zloc
              (not (c/eq? cur-zloc end-zloc))) # left to right
    (array/push kids (c/node cur-zloc))
    (set cur-zloc (c/right cur-zloc)))
  (when (nil? cur-zloc)
    (error "Called `wrap` with invalid value for `end-zloc`."))
  # also collect the last node
  (array/push kids (c/node end-zloc))
  #
  # 2. replace locations that will be removed with non-container nodes
  #
  (def dummy-node
    (c/make-node start-zloc wrap-node (tuple)))
  (set cur-zloc start-zloc)
  # trying to do this together in step 1 is not straight-forward
  # because the desired exiting condition for the while loop depends
  # on cur-zloc becoming end-zloc -- if `replace` were to be used
  # there, the termination condition never gets fulfilled properly.
  (repeat (dec (length kids)) # left to right again
    (set cur-zloc
         (-> (c/replace cur-zloc dummy-node)
             c/right)))
  (set cur-zloc
       (c/replace cur-zloc dummy-node))
  #
  # 3. remove all relevant locations
  #
  (def new-node
    (c/make-node start-zloc wrap-node (tuple ;kids)))
  (repeat (dec (length kids)) # right to left
    (set cur-zloc
         (c/remove cur-zloc)))
  # 4. put the new container node into place
  (c/replace cur-zloc new-node))

(comment

  (def start-zloc
    (-> (c/indexed-zip [:a [:b] :c :x])
        c/down
        c/right))

  (c/node start-zloc)
  # =>
  [:b]

  (-> (c/wrap start-zloc [])
      c/root)
  # =>
  [:a [[:b]] :c :x]

  (def end-zloc
    (c/right start-zloc))

  (c/node end-zloc)
  # =>
  :c

  (-> (c/wrap start-zloc [] end-zloc)
      c/root)
  # =>
  [:a [[:b] :c] :x]

  (try
    (-> (c/wrap end-zloc [] start-zloc)
        c/root)
    ([e] e))
  # =>
  "Called `wrap` with invalid value for `end-zloc`."

  )

########################################################################

(defn c/has-children?
  ``
  Returns true if `node` can have children.
  Returns false if `node` cannot have children.
  ``
  [a-node]
  (when-let [[head] a-node]
    (truthy? (get {:document true
                   :blank false
                   :blockquote true
                   :codeblock true
                   :heading true
                   :html true
                   :linkdef true
                   :list true
                   :list-item true
                   :paragraph true
                   :t-break false}
                  head))))

(comment

  (c/has-children?
    [:paragraph @{:open? false
                  :inlines? true}
     @["hello"]])
  # =>
  true

  (c/has-children? [:blank])
  # =>
  false

  )

(defn c/zip
  ``
  Returns a zipper location (zloc or z-location) for a tree
  representing a CommonMark document.
  ``
  [a-tree]
  (defn branch?_
    [a-node]
    (truthy? (and (indexed? a-node)
                  (not (empty? a-node))
                  (c/has-children? a-node))))
  #
  (defn children_
    [a-node]
    (if (branch?_ a-node)
      (get a-node 2)
      (error "Called `children` on a non-branch node")))
  #
  (defn make-node_
    [a-node kids]
    [(first a-node) (get a-node 1) ;kids])
  #
  (c/zipper a-tree branch?_ children_ make-node_))

(defn c/attrs
  ``
  Return the attributes table for the node of a z-location.
  ``
  [zloc]
  (get (c/node zloc) 1))

(defn c/zip-down
  ``
  Convenience function that returns a zipper which has
  already had `down` called on it.
  ``
  [a-tree]
  (-> (c/zip a-tree)
      c/down))


(comment import ./empathy :prefix "")
(defn e/path-join
  [& parts]
  (def sep
    (if-let [sep (dyn :path-fs-sep)]
      sep
      (if (let [osw (os/which)]
            (or (= :windows osw) (= :mingw osw)))
        `\`
        "/")))
  #
  (string/join parts sep))

(comment

  (let [sep (dyn :path-fs-sep)]
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep "/")
      (e/path-join "/tmp" "test.txt")))
  # =>
  "/tmp/test.txt"

  (let [sep (dyn :path-fs-sep)]
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep "/")
      (e/path-join "/tmp" "foo" "test.txt")))
  # =>
  "/tmp/foo/test.txt"

  (let [sep (dyn :path-fs-sep)]
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep `\`)
      (e/path-join "C:" "windows" "system32")))
  # =>
  `C:\windows\system32`

  )

(defn e/make-itemizer
  [& paths]
  (def todo-paths (reverse paths)) # pop used to process from end
  (def seen? @{})
  #
  (coro
    (while (def p (array/pop todo-paths))
      (def [ok? value] (protect (os/realpath p)))
      (when (and ok? (not (get seen? value)))
        (put seen? value true)
        (yield p)
        (when (= :directory (os/stat p :mode))
          (each subp (reverse (os/dir p))
            (array/push todo-paths (e/path-join p subp))))))))

(comment

  (def it (e/make-itemizer (dyn :syspath) "/etc/fonts"))

  (each p it (pp p))

  )

(comment

  (do
    (var res nil)
    (each p (e/make-itemizer "bundle")
      (when (string/has-suffix? "info.jdn" p)
        (set res true)
        (break)))
    res)
  # =>
  true

  )

(defn e/itemize
  [& paths]
  (def it (e/make-itemizer ;paths))
  #
  (seq [p :in it] p))

(comment

  (e/itemize (e/path-join (os/getenv "HOME") ".config"))

  )

(comment

  (do
    (var res nil)
    (each p (e/itemize ".")
      (when (string/has-suffix? "empathy.janet" p)
        (set res true)
        (break)))
    res)
  # =>
  true

  )


(comment import ./find :prefix "")
(comment import ./jipper :prefix "")
(comment import ./helpers :prefix "")
# based on code by corasaurus-hex

# `slice` doesn't necessarily preserve the input type

# XXX: differs from clojure's behavior
#      e.g. (butlast [:a]) would yield nil(?!) in clojure
(defn j/h/butlast
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 0 -2)
      (array/slice indexed 0 -2))))

(comment

  (j/h/butlast @[:a :b :c])
  # =>
  @[:a :b]

  (j/h/butlast [:a])
  # =>
  []

  )

(defn j/h/rest
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 1 -1)
      (array/slice indexed 1 -1))))

(comment

  (j/h/rest [:a :b :c])
  # =>
  [:b :c]

  (j/h/rest @[:a])
  # =>
  @[]

  )

# XXX: can pass in array - will get back tuple
(defn j/h/tuple-push
  [tup x & xs]
  (if tup
    [;tup x ;xs]
    [x ;xs]))

(comment

  (j/h/tuple-push [:a :b] :c)
  # =>
  [:a :b :c]

  (j/h/tuple-push nil :a)
  # =>
  [:a]

  (j/h/tuple-push @[] :a)
  # =>
  [:a]

  )

(defn j/h/to-entries
  [val]
  (if (dictionary? val)
    (pairs val)
    val))

(comment

  (sort (j/h/to-entries {:a 1 :b 2}))
  # =>
  @[[:a 1] [:b 2]]

  (j/h/to-entries {})
  # =>
  @[]

  (j/h/to-entries @{:a 1})
  # =>
  @[[:a 1]]

  # XXX: leaving non-dictionaries alone and passing through...
  #      is this desirable over erroring?
  (j/h/to-entries [:a :b :c])
  # =>
  [:a :b :c]

  )

# XXX: when xs is empty, "all" becomes nil
(defn j/h/first-rest-maybe-all
  [xs]
  (if (or (nil? xs) (empty? xs))
    [nil nil nil]
    [(first xs) (j/h/rest xs) xs]))

(comment

  (j/h/first-rest-maybe-all [:a :b])
  # =>
  [:a [:b] [:a :b]]

  (j/h/first-rest-maybe-all @[:a])
  # =>
  [:a @[] @[:a]]

  (j/h/first-rest-maybe-all [])
  # =>
  [nil nil nil]

  # XXX: is this what we want?
  (j/h/first-rest-maybe-all nil)
  # =>
  [nil nil nil]

  )


(comment import ./location :prefix "")
# bl - begin line
# bc - begin column
# bp - begin position
# el - end line
# ec - end column
# ep - end position
(defn j/l/make-attrs
  [& items]
  (zipcoll [:bl :bc :bp :el :ec :ep]
           items))

(defn j/l/atom-node
  [node-type peg-form]
  ~(cmt (capture (sequence (line) (column) (position)
                           ,peg-form
                           (line) (column) (position)))
        ,|[node-type (j/l/make-attrs ;(slice $& 0 -2)) (last $&)]))

(defn j/l/reader-macro-node
  [node-type sigil]
  ~(cmt (capture (sequence (line) (column) (position)
                           ,sigil
                           (any :non-form)
                           :form
                           (line) (column) (position)))
        ,|[node-type (j/l/make-attrs ;(slice $& 0 3) ;(slice $& -5 -2))
           ;(slice $& 3 -5)]))

(defn j/l/collection-node
  [node-type open-delim close-delim]
  ~(cmt
     (capture
       (sequence
         (line) (column) (position)
         ,open-delim
         (any :input)
         (choice ,close-delim
                 (error
                   (replace (sequence (line) (column) (position))
                            ,|(string/format
                                (string "line: %p column: %p position: %p "
                                        "missing %p for %p")
                                $0 $1 $2 close-delim node-type))))
         (line) (column) (position)))
     ,|[node-type (j/l/make-attrs ;(slice $& 0 3) ;(slice $& -5 -2))
        ;(slice $& 3 -5)]))

(def j/l/loc-grammar
  ~@{:main (sequence (line) (column) (position)
                     (some :input)
                     (line) (column) (position))
     #
     :input (choice :non-form
                    :form)
     #
     :non-form (choice :whitespace
                       :comment)
     #
     :whitespace ,(j/l/atom-node :whitespace
                             '(choice (some (set " \0\f\t\v"))
                                      (choice "\r\n"
                                              "\r"
                                              "\n")))
     # :whitespace
     # (cmt (capture (sequence (line) (column)
     #                         (choice (some (set " \0\f\t\v"))
     #                                 (choice "\r\n"
     #                                         "\r"
     #                                         "\n"))
     #                         (line) (column)))
     #      ,|[:whitespace (make-attrs ;(slice $& 0 -2)) (last $&)])
     #
     :comment ,(j/l/atom-node :comment
                          '(sequence "#"
                                     (any (if-not (set "\r\n") 1))))
     #
     :form (choice # reader macros
                   :fn
                   :quasiquote
                   :quote
                   :splice
                   :unquote
                   # collections
                   :array
                   :bracket-array
                   :tuple
                   :bracket-tuple
                   :table
                   :struct
                   # atoms
                   :number
                   :constant
                   :buffer
                   :string
                   :long-buffer
                   :long-string
                   :keyword
                   :symbol)
     #
     :fn ,(j/l/reader-macro-node :fn "|")
     # :fn (cmt (capture (sequence (line) (column)
     #                             "|"
     #                             (any :non-form)
     #                             :form
     #                             (line) (column)))
     #          ,|[:fn (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
     #             ;(slice $& 2 -4)])
     #
     :quasiquote ,(j/l/reader-macro-node :quasiquote "~")
     #
     :quote ,(j/l/reader-macro-node :quote "'")
     #
     :splice ,(j/l/reader-macro-node :splice ";")
     #
     :unquote ,(j/l/reader-macro-node :unquote ",")
     #
     :array ,(j/l/collection-node :array "@(" ")")
     # :array
     # (cmt
     #   (capture
     #     (sequence
     #       (line) (column)
     #       "@("
     #       (any :input)
     #       (choice ")"
     #               (error
     #                 (replace (sequence (line) (column))
     #                          ,|(string/format
     #                              "line: %p column: %p missing %p for %p"
     #                              $0 $1 ")" :array))))
     #       (line) (column)))
     #   ,|[:array (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
     #      ;(slice $& 2 -4)])
     #
     :tuple ,(j/l/collection-node :tuple "(" ")")
     #
     :bracket-array ,(j/l/collection-node :bracket-array "@[" "]")
     #
     :bracket-tuple ,(j/l/collection-node :bracket-tuple "[" "]")
     #
     :table ,(j/l/collection-node :table "@{" "}")
     #
     :struct ,(j/l/collection-node :struct "{" "}")
     #
     :number ,(j/l/atom-node :number
                         ~(drop (sequence (cmt (capture (some :num-char))
                                               ,scan-number)
                                          (opt (sequence ":" (range "AZ" "az"))))))
     #
     :num-char (choice (range "09" "AZ" "az")
                       (set "&+-._"))
     #
     :constant ,(j/l/atom-node :constant
                           '(sequence (choice "false" "nil" "true")
                                      (not :name-char)))
     #
     :name-char (choice (range "09" "AZ" "az" "\x80\xFF")
                        (set "!$%&*+-./:<?=>@^_"))
     #
     :buffer ,(j/l/atom-node :buffer
                         '(sequence `@"`
                                    (any (choice :escape
                                                 (if-not "\"" 1)))
                                    `"`))
     #
     :escape (sequence "\\"
                       (choice (set `"'0?\abefnrtvz`)
                               (sequence "x" (2 :h))
                               (sequence "u" (4 :h))
                               (sequence "U" (6 :h))
                               (error (constant "bad escape"))))
     #
     :string ,(j/l/atom-node :string
                         '(sequence `"`
                                    (any (choice :escape
                                                 (if-not "\"" 1)))
                                    `"`))
     #
     :long-string ,(j/l/atom-node :long-string
                              :long-bytes)
     #
     :long-bytes {:main (drop (sequence :open
                                        (any (if-not :close 1))
                                        :close))
                  :open (capture :delim :n)
                  :delim (some "`")
                  :close (cmt (sequence (not (look -1 "`"))
                                        (backref :n)
                                        (capture (backmatch :n)))
                              ,=)}
     #
     :long-buffer ,(j/l/atom-node :long-buffer
                              '(sequence "@" :long-bytes))
     #
     :keyword ,(j/l/atom-node :keyword
                          '(sequence ":"
                                     (any :name-char)))
     #
     :symbol ,(j/l/atom-node :symbol
                         '(some :name-char))
     })

(comment

  (get (peg/match j/l/loc-grammar " ") 3)
  # =>
  [:whitespace @{:bl 1 :el 1 :bc 1 :bp 0 :ec 2 :ep 1} " "]

  (get (peg/match j/l/loc-grammar "true?") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :bp 0 :ec 6 :ep 5} "true?"]

  (get (peg/match j/l/loc-grammar "nil?") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :bp 0 :ec 5 :ep 4} "nil?"]

  (get (peg/match j/l/loc-grammar "false?") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :bp 0 :ec 7 :ep 6} "false?"]

  (get (peg/match j/l/loc-grammar "# hi there") 3)
  # =>
  [:comment @{:bl 1 :el 1 :bc 1 :bp 0 :ec 11 :ep 10} "# hi there"]

  (get (peg/match j/l/loc-grammar "1_000_000") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :bp 0 :ec 10 :ep 9} "1_000_000"]

  (get (peg/match j/l/loc-grammar "8.3") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :bp 0 :ec 4 :ep 3} "8.3"]

  (get (peg/match j/l/loc-grammar "1e2") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 3 :bp 0 :ec 4} "1e2"]

  (get (peg/match j/l/loc-grammar "0xfe") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5} "0xfe"]

  (get (peg/match j/l/loc-grammar "2r01") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5} "2r01"]

  (get (peg/match j/l/loc-grammar "3r101&01") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 8 :bp 0 :ec 9} "3r101&01"]

  (get (peg/match j/l/loc-grammar "2:u") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 3 :bp 0 :ec 4} "2:u"]

  (get (peg/match j/l/loc-grammar "-8:s") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5} "-8:s"]

  (get (peg/match j/l/loc-grammar "1e2:n") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 5 :bp 0 :ec 6} "1e2:n"]

  (get (peg/match j/l/loc-grammar "printf") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :ep 6 :bp 0 :ec 7} "printf"]

  (get (peg/match j/l/loc-grammar ":smile") 3)
  # =>
  [:keyword @{:bl 1 :el 1 :bc 1 :ep 6 :bp 0 :ec 7} ":smile"]

  (get (peg/match j/l/loc-grammar `"fun"`) 3)
  # =>
  [:string @{:bl 1 :el 1 :bc 1 :ep 5 :bp 0 :ec 6} "\"fun\""]

  (get (peg/match j/l/loc-grammar "``long-fun``") 3)
  # =>
  [:long-string @{:bl 1 :el 1 :bc 1 :ep 12 :bp 0 :ec 13} "``long-fun``"]

  (get (peg/match j/l/loc-grammar "@``long-buffer-fun``") 3)
  # =>
  [:long-buffer
   @{:bl 1 :el 1 :bc 1 :bp 0 :ec 21 :ep 20}
   "@``long-buffer-fun``"]

  (get (peg/match j/l/loc-grammar `@"a buffer"`) 3)
  # =>
  [:buffer @{:bl 1 :el 1 :bc 1 :ep 11 :bp 0 :ec 12} "@\"a buffer\""]

  (get (peg/match j/l/loc-grammar "@[8]") 3)
  # =>
  [:bracket-array @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5}
   [:number @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} "8"]]

  (get (peg/match j/l/loc-grammar "@{:a 1}") 3)
  # =>
  [:table @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
   [:keyword @{:bl 1 :el 1 :bc 3 :ep 4 :bp 2 :ec 5} ":a"]
   [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
   [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "1"]]

  (get (peg/match j/l/loc-grammar "~x") 3)
  # =>
  [:quasiquote @{:bl 1 :el 1 :bc 1 :ep 2 :bp 0 :ec 3}
   [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "x"]]

  (get (peg/match j/l/loc-grammar "' '[:a :b]") 3)
  # =>
  [:quote @{:bl 1 :el 1 :bc 1 :ep 10 :bp 0 :ec 11}
   [:whitespace @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} " "]
   [:quote @{:bl 1 :el 1 :bc 3 :ep 10 :bp 2 :ec 11}
    [:bracket-tuple @{:bl 1 :el 1 :bc 4 :ep 10 :bp 3 :ec 11}
     [:keyword @{:bl 1 :el 1 :bc 5 :ep 6 :bp 4 :ec 7} ":a"]
     [:whitespace @{:bl 1 :el 1 :bc 7 :ep 7 :bp 6 :ec 8} " "]
     [:keyword @{:bl 1 :el 1 :bc 8 :ep 9 :bp 7 :ec 10} ":b"]]]]

  )

(def j/l/loc-top-level-ast
  (put (table ;(kvs j/l/loc-grammar))
       :main ~(sequence (line) (column) (position)
                        :input
                        (line) (column) (position))))

(defn j/l/par
  [src &opt start single]
  (default start 0)
  (if single
    (if-let [[bl bc bp tree el ec ep]
             (peg/match j/l/loc-top-level-ast src start)]
      @[:code (j/l/make-attrs bl bc bp el ec ep) tree]
      @[:code])
    (if-let [captures (peg/match j/l/loc-grammar src start)]
      (let [[bl bc bp] (slice captures 0 3)
            [el ec ep] (slice captures -4)
            trees (array/slice captures 3 -4)]
        (array/insert trees 0
                      :code (j/l/make-attrs bl bc bp el ec ep)))
      @[:code])))

# XXX: backward compatibility
(def j/l/ast j/l/par)

(comment

  (j/l/par "(+ 1 1)")
  # =>
  @[:code @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
    [:tuple @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
     [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "+"]
     [:whitespace @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} " "]
     [:number @{:bl 1 :el 1 :bc 4 :ep 4 :bp 3 :ec 5} "1"]
     [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
     [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "1"]]]

  )

(defn j/l/gen*
  [an-ast buf]
  (case (first an-ast)
    :code
    (each elt (drop 2 an-ast)
      (j/l/gen* elt buf))
    #
    :buffer
    (buffer/push-string buf (in an-ast 2))
    :comment
    (buffer/push-string buf (in an-ast 2))
    :constant
    (buffer/push-string buf (in an-ast 2))
    :keyword
    (buffer/push-string buf (in an-ast 2))
    :long-buffer
    (buffer/push-string buf (in an-ast 2))
    :long-string
    (buffer/push-string buf (in an-ast 2))
    :number
    (buffer/push-string buf (in an-ast 2))
    :string
    (buffer/push-string buf (in an-ast 2))
    :symbol
    (buffer/push-string buf (in an-ast 2))
    :whitespace
    (buffer/push-string buf (in an-ast 2))
    #
    :array
    (do
      (buffer/push-string buf "@(")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf ")"))
    :bracket-array
    (do
      (buffer/push-string buf "@[")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "]"))
    :bracket-tuple
    (do
      (buffer/push-string buf "[")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "]"))
    :tuple
    (do
      (buffer/push-string buf "(")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf ")"))
    :struct
    (do
      (buffer/push-string buf "{")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "}"))
    :table
    (do
      (buffer/push-string buf "@{")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf))
      (buffer/push-string buf "}"))
    #
    :fn
    (do
      (buffer/push-string buf "|")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :quasiquote
    (do
      (buffer/push-string buf "~")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :quote
    (do
      (buffer/push-string buf "'")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :splice
    (do
      (buffer/push-string buf ";")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    :unquote
    (do
      (buffer/push-string buf ",")
      (each elt (drop 2 an-ast)
        (j/l/gen* elt buf)))
    ))

(defn j/l/gen
  [an-ast]
  (let [buf @""]
    (j/l/gen* an-ast buf)
    # XXX: leave as buffer?
    (string buf)))

# XXX: backward compatibility
(def j/l/code j/l/gen)

(comment

  (j/l/gen [:code])
  # =>
  ""

  (j/l/gen [:whitespace @{:bc 1 :bl 1 :bp 0
                      :ec 2 :el 1 :ep 1} " "])
  # =>
  " "

  (j/l/gen [:buffer @{:bc 1 :bl 1 :bp 0
                  :ec 12 :el 1 :ep 11} "@\"a buffer\""])
  # =>
  `@"a buffer"`

  (j/l/gen @[:code @{:bc 1 :bl 1 :bp 0
                 :ec 8 :el 1 :ep 7}
         [:tuple @{:bc 1 :bl 1 :bp 0
                   :ec 8 :el 1 :ep 7}
                 [:symbol @{:bc 2 :bl 1 :bp 1
                            :ec 3 :el 1 :ep 2} "+"]
                 [:whitespace @{:bc 3 :bl 1 :bp 2
                                :ec 4 :el 1 :ep 3} " "]
                 [:number @{:bc 4 :bl 1 :bp 3
                            :ec 5 :el 1 :ep 4} "1"]
                 [:whitespace @{:bc 5 :bl 1 :bp 4
                                :ec 6 :el 1 :ep 5} " "]
                 [:number @{:bc 6 :bl 1 :bp 5
                            :ec 7 :el 1 :ep 6} "1"]]])
  # =>
  "(+ 1 1)"

  )

(comment

  (def src "{:x  :y \n :z  [:a  :b    :c]}")

  (j/l/gen (j/l/par src))
  # =>
  src

  )

(comment

  (comment

    (let [src (slurp (string (os/getenv "HOME")
                             "/src/janet/src/boot/boot.janet"))]
      (= (string src)
         (j/l/gen (j/l/par src))))

    )

  )


(def j/version "2026-03-19_04-53-23")

# exports
(def j/par j/l/par)
(def j/gen j/l/gen)

########################################################################

(defn j/zipper
  ``
  Returns a new zipper consisting of two elements:

  * `a-root` - the passed in root node.
  * `state` - table of info about node's z-location in the tree with keys:
    * `:ls` - left siblings
    * `:pnodes` - path of nodes from root to current z-location
    * `:pstate` - parent node's state
    * `:rs` - right siblings
    * `:changed?` - indicates whether "editing" has occured

  `state` has a prototype table with four functions:

  * :branch? - fn that tests if a node is a branch (has children)
  * :children - fn that returns the child nodes for the given branch.
  * :make-node - fn that takes a node + children and returns a new branch
    node with the same.
  * :make-state - fn for creating a new state
  ``
  [a-root branch?-fn children-fn make-node-fn]
  #
  (defn make-state_
    [&opt ls_ rs_ pnodes_ pstate_ changed?_]
    (table/setproto @{:ls ls_
                      :pnodes pnodes_
                      :pstate pstate_
                      :rs rs_
                      :changed? changed?_}
                    @{:branch? branch?-fn
                      :children children-fn
                      :make-node make-node-fn
                      :make-state make-state_}))
  #
  [a-root (make-state_)])

(comment

  # XXX

  )

(defn j/indexed-zip
  ``
  Returns a zipper for nested indexed data structures (tuples
  or arrays), given a root data structure.
  ``
  [indexed]
  (j/zipper indexed
          indexed?
          j/h/to-entries
          (fn [_p xs] xs)))

(comment

  (def a-node
    [:x [:y :z]])

  (def [the-node the-state]
    (j/indexed-zip a-node))

  the-node
  # =>
  a-node

  # merge is used to "remove" the prototype table of `st`
  (merge {} the-state)
  # =>
  @{}

  )

(defn j/node
  "Returns the node at `zloc`."
  [zloc]
  (get zloc 0))

(comment

  (j/node (j/indexed-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn j/state
  "Returns the state for `zloc`."
  [zloc]
  (get zloc 1))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (-> (j/indexed-zip [:a [:b [:x :y]]])
             j/state))
  # =>
  @{}

  )

(defn j/branch?
  ``
  Returns true if the node at `zloc` is a branch.
  Returns false otherwise.
  ``
  [zloc]
  (((j/state zloc) :branch?) (j/node zloc)))

(comment

  (j/branch? (j/indexed-zip [:a :b [:x :y]]))
  # =>
  true

  )

(defn j/children
  ``
  Returns children for a branch node at `zloc`.
  Otherwise throws an error.
  ``
  [zloc]
  (if (j/branch? zloc)
    (((j/state zloc) :children) (j/node zloc))
    (error "Called `children` on a non-branch zloc")))

(comment

  (j/children (j/indexed-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn j/make-state
  ``
  Convenience function for calling the :make-state function for `zloc`.
  ``
  [zloc &opt ls rs pnodes pstate changed?]
  (((j/state zloc) :make-state) ls rs pnodes pstate changed?))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (j/make-state (j/indexed-zip [:a :b [:x :y]])))
  # =>
  @{}

  )

(defn j/down
  ``
  Moves down the tree, returning the leftmost child z-location of
  `zloc`, or nil if there are no children.
  ``
  [zloc]
  (when (j/branch? zloc)
    (let [[z-node st] zloc
          [k rest-kids kids]
          (j/h/first-rest-maybe-all (j/children zloc))]
      (when kids
        [k
         (j/make-state zloc
                     []
                     rest-kids
                     (if (not (empty? st))
                       (j/h/tuple-push (get st :pnodes) z-node)
                       [z-node])
                     st
                     (get st :changed?))]))))

(comment

  (j/node (j/down (j/indexed-zip [:a :b [:x :y]])))
  # =>
  :a

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/branch?)
  # =>
  false

  (try
    (-> (j/indexed-zip [:a])
        j/down
        j/children)
    ([e] e))
  # =>
  "Called `children` on a non-branch zloc"

  (deep=
    #
    (merge {}
           (-> [:a [:b [:x :y]]]
               j/indexed-zip
               j/down
               j/state))
    #
    '@{:ls ()
       :pnodes ((:a (:b (:x :y))))
       :pstate @{}
       :rs ((:b (:x :y)))})
  # =>
  true

  )

(defn j/right
  ``
  Returns the z-location of the right sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st
        [r rest-rs rs_] (j/h/first-rest-maybe-all rs)]
    (when (and (not (empty? st)) rs_)
      [r
       (j/make-state zloc
                   (j/h/tuple-push ls z-node)
                   rest-rs
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (j/indexed-zip [:a :b])
      j/down
      j/right
      j/node)
  # =>
  :b

  (-> (j/indexed-zip [:a])
      j/down
      j/right)
  # =>
  nil

  )

(defn j/make-node
  ``
  Returns a branch node, given `zloc`, `a-node` and `kids`.
  ``
  [zloc a-node kids]
  (((j/state zloc) :make-node) a-node kids))

(comment

  (j/make-node (j/indexed-zip [:a :b [:x :y]])
             [:a :b] [:x :y])
  # =>
  [:x :y]

  )

(defn j/up
  ``
  Moves up the tree, returning the parent z-location of `zloc`,
  or nil if at the root z-location.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs
         :changed? changed?} st]
    (when pnodes
      (let [pnode (last pnodes)]
        (if changed?
          [(j/make-node zloc pnode [;ls z-node ;rs])
           (j/make-state zloc
                       (get pstate :ls)
                       (get pstate :rs)
                       (get pstate :pnodes)
                       (get pstate :pstate)
                       true)]
          [pnode pstate])))))

(comment

  (def m-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (deep=
    (-> m-zip
        j/down
        j/up)
    m-zip)
  # =>
  true

  (deep=
    (-> m-zip
        j/down
        j/right
        j/right
        j/down
        j/up
        j/up)
    m-zip)
  # =>
  true

  )

# XXX: used by `root` and `df-next`
(defn j/end?
  "Returns true if `zloc` represents the end of a depth-first walk."
  [zloc]
  (= :end (j/state zloc)))

(defn j/root
  ``
  Moves all the way up the tree for `zloc` and returns the node at
  the root z-location.
  ``
  [zloc]
  (if (j/end? zloc)
    (j/node zloc)
    (if-let [p (j/up zloc)]
      (j/root p)
      (j/node zloc))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (j/node a-zip)
  # =>
  (-> a-zip
      j/down
      j/right
      j/right
      j/down
      j/root)

  )

(defn j/df-next
  ``
  Moves to the next z-location, depth-first.  When the end is
  reached, returns a special z-location detectable via `end?`.
  Does not move if already at the end.
  ``
  [zloc]
  #
  (defn recur
    [a-loc]
    (if (j/up a-loc)
      (or (j/right (j/up a-loc))
          (recur (j/up a-loc)))
      [(j/node a-loc) :end]))
  #
  (if (j/end? zloc)
    zloc
    (or (and (j/branch? zloc) (j/down zloc))
        (j/right zloc)
        (recur zloc))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x]]))

  (j/node (j/df-next a-zip))
  # =>
  :a

  (-> a-zip
      j/df-next
      j/df-next
      j/node)
  # =>
  :b

  (-> a-zip
      j/df-next
      j/df-next
      j/df-next
      j/df-next
      j/df-next
      j/end?)
  # =>
  true

  )

(defn j/replace
  "Replaces existing node at `zloc` with `a-node`, without moving."
  [zloc a-node]
  (let [[_ st] zloc]
    [a-node
     (j/make-state zloc
                 (get st :ls)
                 (get st :rs)
                 (get st :pnodes)
                 (get st :pstate)
                 true)]))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      (j/replace :w)
      j/root)
  # =>
  [:w :b [:x :y]]

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      (j/replace :w)
      j/root)
  # =>
  [:a :b [:w :y]]

  )

(defn j/edit
  ``
  Replaces the node at `zloc` with the value of `(f node args)`,
  where `node` is the node associated with `zloc`.
  ``
  [zloc f & args]
  (j/replace zloc
           (apply f (j/node zloc) args)))

(comment

  (-> (j/indexed-zip [1 2 [8 9]])
      j/down
      (j/edit inc)
      j/root)
  # =>
  [2 2 [8 9]]

  (-> (j/indexed-zip [1 2 [8 9]])
      j/down
      (j/edit inc)
      j/right
      (j/edit inc)
      j/right
      j/down
      (j/edit dec)
      j/right
      (j/edit dec)
      j/root)
  # =>
  [2 3 [7 8]]

  )

(defn j/insert-child
  ``
  Inserts `child` as the leftmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (j/replace zloc
           (j/make-node zloc
                      (j/node zloc)
                      [child ;(j/children zloc)])))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      (j/insert-child :c)
      j/root)
  # =>
  [:c :a :b [:x :y]]

  )

(defn j/append-child
  ``
  Appends `child` as the rightmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (j/replace zloc
           (j/make-node zloc
                      (j/node zloc)
                      [;(j/children zloc) child])))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      (j/append-child :c)
      j/root)
  # =>
  [:a :b [:x :y] :c]

  )

(defn j/rightmost
  ``
  Returns the z-location of the rightmost sibling of the node at
  `zloc`, or the current node's z-location if there are none to the
  right.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? rs)
             (not (empty? rs)))
      [(last rs)
       (j/make-state zloc
                   (j/h/tuple-push ls z-node ;(j/h/butlast rs))
                   []
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/rightmost
      j/node)
  # =>
  [:x :y]

  )

(defn j/remove
  ``
  Removes the node at `zloc`, returning the z-location that would have
  preceded it in a depth-first walk.  Throws an error if called at the
  root z-location.
  ``
  [zloc]
  (let [[_z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs} st]
    #
    (defn recur
      [a-zloc]
      (if-let [child (and (j/branch? a-zloc) (j/down a-zloc))]
        (recur (j/rightmost child))
        a-zloc))
    #
    (if (not (empty? st))
      (if (pos? (length ls))
        (recur [(last ls)
                (j/make-state zloc
                            (j/h/butlast ls)
                            rs
                            pnodes
                            pstate
                            true)])
        [(j/make-node zloc (last pnodes) rs)
         (j/make-state zloc
                     (get pstate :ls)
                     (get pstate :rs)
                     (get pstate :pnodes)
                     (get pstate :pstate)
                     true)])
      (error "Called `remove` at root"))))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/remove
      j/node)
  # =>
  :a

  (try
    (j/remove (j/indexed-zip [:a :b [:x :y]]))
    ([e] e))
  # =>
  "Called `remove` at root"

  )

(defn j/left
  ``
  Returns the z-location of the left sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (when (and (not (empty? st))
               (indexed? ls)
               (not (empty? ls)))
      [(last ls)
       (j/make-state zloc
                   (j/h/butlast ls)
                   [z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (j/indexed-zip [:a :b :c])
      j/down
      j/right
      j/right
      j/left
      j/node)
  # =>
  :b

  (-> (j/indexed-zip [:a])
      j/down
      j/left)
  # =>
  nil

  )

(defn j/df-prev
  ``
  Moves to the previous z-location, depth-first.
  If already at the root, returns nil.
  ``
  [zloc]
  #
  (defn recur
    [a-zloc]
    (if-let [child (and (j/branch? a-zloc)
                        (j/down a-zloc))]
      (recur (j/rightmost child))
      a-zloc))
  #
  (if-let [left-loc (j/left zloc)]
    (recur left-loc)
    (j/up zloc)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/df-prev
      j/node)
  # =>
  :a

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      j/df-prev
      j/node)
  # =>
  [:x :y]

  )

(defn j/insert-right
  ``
  Inserts `a-node` as the right sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (j/make-state zloc
                   ls
                   [a-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-right` at root"))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (-> a-zip
      j/down
      (j/insert-right :z)
      j/root)
  # =>
  [:a :z :b [:x :y]]

  (try
    (j/insert-right a-zip :e)
    ([e] e))
  # =>
  "Called `insert-right` at root"

  )

(defn j/insert-left
  ``
  Inserts `a-node` as the left sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (j/make-state zloc
                   (j/h/tuple-push ls a-node)
                   rs
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-left` at root"))))

(comment

  (def a-zip
    (j/indexed-zip [:a :b [:x :y]]))

  (-> a-zip
      j/down
      (j/insert-left :z)
      j/root)
  # =>
  [:z :a :b [:x :y]]

  (try
    (j/insert-left a-zip :e)
    ([e] e))
  # =>
  "Called `insert-left` at root"

  )

(defn j/rights
  "Returns siblings to the right of `zloc`."
  [zloc]
  (when-let [st (j/state zloc)]
    (get st :rs)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/rights)
  # =>
  [:b [:x :y]]

  (-> (j/indexed-zip [:a :b])
      j/down
      j/right
      j/rights)
  # =>
  []

  )

(defn j/lefts
  "Returns siblings to the left of `zloc`."
  [zloc]
  (if-let [st (j/state zloc)
           ls (get st :ls)]
    ls
    []))

(comment

  (-> (j/indexed-zip [:a :b])
      j/down
      j/lefts)
  # =>
  []

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/lefts)
  # =>
  [:a :b]

  )

(defn j/leftmost
  ``
  Returns the z-location of the leftmost sibling of the node at `zloc`,
  or the current node's z-location if there are no siblings to the left.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? ls)
             (not (empty? ls)))
      [(first ls)
       (j/make-state zloc
                   []
                   [;(j/h/rest ls) z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/leftmost
      j/node)
  # =>
  :a

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/rightmost
      j/leftmost
      j/node)
  # =>
  :a

  )

(defn j/path
  "Returns the path of nodes that lead to `zloc` from the root node."
  [zloc]
  (when-let [st (j/state zloc)]
    (get st :pnodes)))

(comment

  (j/path (j/indexed-zip [:a :b [:x :y]]))
  # =>
  nil

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/path)
  # =>
  [[:a :b [:x :y]]]

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/down
      j/path)
  # =>
  [[:a :b [:x :y]] [:x :y]]

  )

(defn j/right-until
  ``
  Try to move right from `zloc`, calling `pred` for each
  right sibling.  If the `pred` call has a truthy result,
  return the corresponding right sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [right-sib (j/right zloc)]
    (if (pred right-sib)
      right-sib
      (j/right-until right-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      j/indexed-zip
      j/down
      j/right
      j/down
      (j/right-until |(match (j/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true))
      j/node)
  # =>
  [:symbol "+"]

  )

(defn j/left-until
  ``
  Try to move left from `zloc`, calling `pred` for each
  left sibling.  If the `pred` call has a truthy result,
  return the corresponding left sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [left-sib (j/left zloc)]
    (if (pred left-sib)
      left-sib
      (j/left-until left-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      j/indexed-zip
      j/down
      j/right
      j/down
      j/rightmost
      (j/left-until |(match (j/node $)
                     [:comment]
                     false
                     #
                     [:whitespace]
                     false
                     #
                     true))
      j/node)
  # =>
  [:number "1"]

  )

(defn j/search-from
  ``
  Successively call `pred` on z-locations starting at `zloc`
  in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (if (pred zloc)
    zloc
    (when-let [next-zloc (j/df-next zloc)]
      (when (j/end? next-zloc)
        (break nil))
      (j/search-from next-zloc pred))))

(comment

  (-> (j/indexed-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :b
                      true))
      j/node)
  # =>
  :b

  (-> (j/indexed-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :d
                      true)))
  # =>
  nil

  (-> (j/indexed-zip [:a :b :c])
      j/down
      (j/search-from |(match (j/node $)
                      :a
                      true))
      j/node)
  # =>
  :a

  )

(defn j/search-after
  ``
  Successively call `pred` on z-locations starting after
  `zloc` in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when (j/end? zloc)
    (break nil))
  (when-let [next-zloc (j/df-next zloc)]
    (if (pred next-zloc)
      next-zloc
      (j/search-after next-zloc pred))))

(comment

  (-> (j/indexed-zip [:b :a :b])
      j/down
      (j/search-after |(match (j/node $)
                       :b
                       true))
      j/left
      j/node)
  # =>
  :a

  (-> (j/indexed-zip [:b :a :b])
      j/down
      (j/search-after |(match (j/node $)
                       :d
                       true)))
  # =>
  nil

  (-> (j/indexed-zip [:a [:b :c [2 [3 :smile] 5]]])
      (j/search-after |(match (j/node $)
                       [_ :smile]
                       true))
      j/down
      j/node)
  # =>
  3

  )

(defn j/unwrap
  ``
  If the node at `zloc` is a branch node, "unwrap" its children in
  place.  If `zloc`'s node is not a branch node, do nothing.

  Throws an error if `zloc` corresponds to a top-most container.
  ``
  [zloc]
  (unless (j/branch? zloc)
    (break zloc))
  #
  (when (empty? (j/state zloc))
    (error "Called `unwrap` at root"))
  #
  (def kids (j/children zloc))
  (var i (dec (length kids)))
  (var curr-zloc zloc)
  (while (<= 0 i) # right to left
    (set curr-zloc
         (j/insert-right curr-zloc (get kids i)))
    (-- i))
  # try to end up at a sensible spot
  (set curr-zloc
       (j/remove curr-zloc))
  (if-let [ret-zloc (j/right curr-zloc)]
    ret-zloc
    curr-zloc))

(comment

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/right
      j/right
      j/unwrap
      j/root)
  # =>
  [:a :b :x :y]

  (-> (j/indexed-zip [:a :b [:x :y]])
      j/down
      j/unwrap
      j/root)
  # =>
  [:a :b [:x :y]]

  (-> (j/indexed-zip [[:a]])
      j/down
      j/unwrap
      j/root)
  # =>
  [:a]

  (-> (j/indexed-zip [[:a :b] [:x :y]])
      j/down
      j/down
      j/remove
      j/unwrap
      j/root)
  # =>
  [:b [:x :y]]

  (try
    (-> (j/indexed-zip [:a :b [:x :y]])
        j/unwrap)
    ([e] e))
  # =>
  "Called `unwrap` at root"

  )

(defn j/eq?
  ``
  Compare two zlocs, `a-zloc` and `b-zloc`, for equality.
  ``
  [a-zloc b-zloc]
  (and (= (length (j/lefts a-zloc)) (length (j/lefts b-zloc)))
       (= (j/path a-zloc) (j/path b-zloc))))

(comment

  (def iz (j/indexed-zip [:a :b :c :b]))

  (j/eq? (-> iz j/down j/right)
       (-> iz j/down j/right j/right j/right))
  # =>
  false

  (j/eq? (-> iz j/down j/right)
       (-> iz j/down j/right j/right j/right j/left j/left))
  # =>
  true

  )

(defn j/wrap
  ``
  Replace nodes from `start-zloc` through `end-zloc` with a single
  node of the same type as `wrap-node` containing the nodes from
  `start-zloc` through `end-zloc`.

  If `end-zloc` is not specified, just wrap `start-zloc`.

  The caller is responsible for ensuring the value of `end-zloc`
  is somewhere to the right of `start-zloc`.  Throws an error if
  an inappropriate value is specified for `end-zloc`.
  ``
  [start-zloc wrap-node &opt end-zloc]
  (default end-zloc start-zloc)
  #
  # 1. collect all nodes to wrap
  #
  (def kids @[])
  (var cur-zloc start-zloc)
  (while (and cur-zloc
              (not (j/eq? cur-zloc end-zloc))) # left to right
    (array/push kids (j/node cur-zloc))
    (set cur-zloc (j/right cur-zloc)))
  (when (nil? cur-zloc)
    (error "Called `wrap` with invalid value for `end-zloc`."))
  # also collect the last node
  (array/push kids (j/node end-zloc))
  #
  # 2. replace locations that will be removed with non-container nodes
  #
  (def dummy-node
    (j/make-node start-zloc wrap-node (tuple)))
  (set cur-zloc start-zloc)
  # trying to do this together in step 1 is not straight-forward
  # because the desired exiting condition for the while loop depends
  # on cur-zloc becoming end-zloc -- if `replace` were to be used
  # there, the termination condition never gets fulfilled properly.
  (repeat (dec (length kids)) # left to right again
    (set cur-zloc
         (-> (j/replace cur-zloc dummy-node)
             j/right)))
  (set cur-zloc
       (j/replace cur-zloc dummy-node))
  #
  # 3. remove all relevant locations
  #
  (def new-node
    (j/make-node start-zloc wrap-node (tuple ;kids)))
  (repeat (dec (length kids)) # right to left
    (set cur-zloc
         (j/remove cur-zloc)))
  # 4. put the new container node into place
  (j/replace cur-zloc new-node))

(comment

  (def start-zloc
    (-> (j/indexed-zip [:a [:b] :c :x])
        j/down
        j/right))

  (j/node start-zloc)
  # =>
  [:b]

  (-> (j/wrap start-zloc [])
      j/root)
  # =>
  [:a [[:b]] :c :x]

  (def end-zloc
    (j/right start-zloc))

  (j/node end-zloc)
  # =>
  :c

  (-> (j/wrap start-zloc [] end-zloc)
      j/root)
  # =>
  [:a [[:b] :c] :x]

  (try
    (-> (j/wrap end-zloc [] start-zloc)
        j/root)
    ([e] e))
  # =>
  "Called `wrap` with invalid value for `end-zloc`."

  )

########################################################################

(defn j/has-children?
  ``
  Returns true if `a-node` can have children.
  Returns false if `a-node` cannot have children.
  ``
  [a-node]
  (when-let [[head] a-node]
    (truthy? (get {:code true
                   :fn true
                   :quasiquote true
                   :quote true
                   :splice true
                   :unquote true
                   :array true
                   :tuple true
                   :bracket-array true
                   :bracket-tuple true
                   :table true
                   :struct true}
                  head))))

(comment

  (j/has-children?
    [:tuple @{}
     [:symbol @{} "+"] [:whitespace @{} " "]
     [:number @{} "1"] [:whitespace @{} " "]
     [:number @{} "2"]])
  # =>
  true

  (j/has-children? [:number @{} "8"])
  # =>
  false

  )

(defn j/zip
  ``
  Returns a zipper location (zloc or z-location) for a tree
  representing Janet code.
  ``
  [a-tree]
  (defn branch?_
    [a-node]
    (truthy? (and (indexed? a-node)
                  (not (empty? a-node))
                  (j/has-children? a-node))))
  #
  (defn children_
    [a-node]
    (if (branch?_ a-node)
      (slice a-node 2)
      (error "Called `children` on a non-branch node")))
  #
  (defn make-node_
    [a-node kids]
    [(first a-node) (get a-node 1) ;kids])
  #
  (j/zipper a-tree branch?_ children_ make-node_))

(comment

  (def root-node
    @[:code @{} [:number @{} "8"]])

  (def [the-node the-state]
    (j/zip root-node))

  the-node
  # =>
  root-node

  # merge is used to "remove" the prototype table of `st`
  (merge {} the-state)
  # =>
  @{}

  )

(defn j/attrs
  ``
  Return the attributes table for the node of a z-location.  The
  attributes table contains at least bounds of the node by 1-based line
  and column numbers along with 0-based positions.
  ``
  [zloc]
  (get (j/node zloc) 1))

(comment

  (-> (j/par "(+ 1 3)")
      j/zip
      j/down
      j/attrs)
  # =>
  @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}

  )

(defn j/zip-down
  ``
  Convenience function that returns a zipper which has
  already had `down` called on it.
  ``
  [a-tree]
  (-> (j/zip a-tree)
      j/down))

(comment

  (-> (j/par "(+ 1 3)")
      j/zip-down
      j/node)
  # =>
  [:tuple @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
   [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "+"]
   [:whitespace @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} " "]
   [:number @{:bl 1 :el 1 :bc 4 :ep 4 :bp 3 :ec 5} "1"]
   [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
   [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "3"]]

  (-> (j/par "(/ 1 8)")
      j/zip-down
      j/root)
  # =>
  @[:code @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
    [:tuple @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
     [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "/"]
     [:whitespace @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} " "]
     [:number @{:bl 1 :el 1 :bc 4 :ep 4 :bp 3 :ec 5} "1"]
     [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
     [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "8"]]]

  )

# wsc == whitespace, comment
(defn j/right-skip-wsc
  ``
  Try to move right from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one right move succeeds, return the z-location
  for the last successful right move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/right-until zloc
               |(match (j/node $)
                  [:whitespace]
                  false
                  #
                  [:comment]
                  false
                  #
                  true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-wsc
      j/node)
  # =>
  [:symbol @{:bl 2 :el 2 :bc 1 :ep 13 :bp 12 :ec 2} "+"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/right-skip-wsc)
  # =>
  nil

  )

(defn j/left-skip-wsc
  ``
  Try to move left from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one left move succeeds, return the z-location
  for the last successful left move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/left-until zloc
              |(match (j/node $)
                 [:whitespace]
                 false
                 #
                 [:comment]
                 false
                 #
                 true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-wsc
      j/right-skip-wsc
      j/left-skip-wsc
      j/node)
  # =>
  [:symbol @{:bl 2 :el 2 :bc 1 :ep 13 :bp 12 :ec 2} "+"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/left-skip-wsc)
  # =>
  nil

  )

# ws == whitespace
(defn j/right-skip-ws
  ``
  Try to move right from `zloc`, skipping over whitespace
  nodes.

  When at least one right move succeeds, return the z-location
  for the last successful right move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/right-until zloc
               |(match (j/node $)
                  [:whitespace]
                  false
                  #
                  true)))

(comment

  (-> (j/par (string "( # hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right-skip-ws
      j/node)
  # =>
  [:comment @{:bl 1 :el 1 :bc 3 :ep 12 :bp 2 :ec 13} "# hi there"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/right-skip-ws)
  # =>
  nil

  )

(defn j/left-skip-ws
  ``
  Try to move left from `zloc`, skipping over whitespace
  nodes.

  When at least one left move succeeds, return the z-location
  for the last successful left move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (j/left-until zloc
              |(match (j/node $)
                 [:whitespace]
                 false
                 #
                 true)))

(comment

  (-> (j/par (string "(# hi there\n"
                   "+ 1 2)"))
      j/zip-down
      j/down
      j/right
      j/right
      j/left-skip-ws
      j/node)
  # =>
  [:comment @{:bl 1 :el 1 :bc 2 :ep 11 :bp 1 :ec 12} "# hi there"]

  (-> (j/par "(:a)")
      j/zip-down
      j/down
      j/left-skip-ws)
  # =>
  nil

  )



# XXX: doesn't handle atypical code that uses ordinary tuples for
#      parameter lists
(defn f/find-caller-docstring
  [zloc]
  (var cur-zloc zloc)
  (var ret nil)
  (set cur-zloc (j/right-until cur-zloc |(match (j/node $)
                                           [:bracket-tuple]
                                           $)))
  (when cur-zloc
    (def params-node (j/node cur-zloc))
    (while (def left-zloc (j/left cur-zloc))
      (set cur-zloc left-zloc)
      (when-let [[node-type {:bl bl :bc bc} node-value]
                 (j/node left-zloc)]
        (when (get {:string 1 :long-string 1} node-type)
          (set ret @{:bl bl :bc bc :text node-value
                     :params-str (j/gen params-node)})
          (break))))
    #
    ret))

# XXX: only called when parsed parent has length = 3
(defn f/find-defdyn-docstring
  [zloc]
  (var cur-zloc zloc)
  (var ret nil)
  # start at the rightmost end
  (set cur-zloc (j/rightmost cur-zloc))
  (when cur-zloc
    (while cur-zloc
      (when-let [[node-type {:bl bl :bc bc} node-value]
                 (j/node cur-zloc)]
        (when (get {:string 1 :long-string 1} node-type)
          (set ret @{:bl bl :bc bc :text node-value})
          (break)))
      #
      (set cur-zloc (j/left cur-zloc)))
    #
    ret))

# XXX: only called when parsed parent has length >= 4
(defn f/find-special-docstring
  [zloc]
  (var cur-zloc zloc)
  (var ret nil)
  # start at the rightmost end
  (set cur-zloc (j/rightmost cur-zloc))
  (when cur-zloc
    # cannot be the rightmost node
    (while (def left-zloc (j/left cur-zloc))
      (when-let [[node-type {:bl bl :bc bc} node-value]
                 (j/node left-zloc)]
        (when (get {:string 1 :long-string 1} node-type)
          (set ret @{:bl bl :bc bc :text node-value})
          (break)))
      #
      (set cur-zloc left-zloc))
    #
    ret))

# XXX: if the keys were strings then other code would need to
#      change...
(def f/func-definers
  {'defn 1 'defn- 1})

(def f/macro-definers
  {'defmacro 1 'defmacro- 1})

(def f/call-definers
  (merge f/func-definers f/macro-definers))

(def f/special-definers
  {'def 1 'def- 1
   'var 1 'var- 1})

(defn f/find-doc-of
  [src opts]
  (def {:pattern name} opts)
  #
  (def tree (j/par src))
  (var cur-zloc (j/zip-down tree))
  (def results @[])
  #
  (while (def next-zloc
           (j/search-from cur-zloc
                          |(match (j/node $)
                             [:symbol _ sym]
                             (when (string/has-suffix? name sym)
                               $))))
    (def parent-zloc (j/up next-zloc))
    (when (= :tuple (get (j/node parent-zloc) 0))
      (def node (j/node parent-zloc))
      (def raw-code-str (j/gen node))
      (def parsed
        (try
          (parse raw-code-str)
          ([_e]
            (eprintf "failed to parse: %s" raw-code-str))))
      (when parsed
        (def found-name (string (get parsed 1)))
        (when (string/has-suffix? name found-name)
          (def head (first parsed))
          (cond
            (and (get f/special-definers head)
                 (<= 4 (length parsed))) # metadata possible
            (when-let [ds-tbl (f/find-special-docstring (j/down parent-zloc))]
              (array/push results
                          (merge ds-tbl {:def-type (string head)
                                         :name found-name})))
            #
            (get f/call-definers head)
            (when-let [ds-tbl (f/find-caller-docstring (j/down parent-zloc))]
              (array/push results
                          (merge ds-tbl {:def-type (string head)
                                         :name found-name})))
            #
            (and (= 'defdyn head) (= 3 (length parsed)))
            (when-let [ds-tbl (f/find-defdyn-docstring (j/down parent-zloc))]
              (array/push results
                          (merge ds-tbl {:def-type (string head)
                                         :name found-name})))
            # XXX: other cases?
            nil))))
    #
    (set cur-zloc (j/df-next next-zloc)))
  #
  results)

(comment

  (f/find-doc-of
    ``
    (defn smile
      "I am a defn docstring."
      [y]
      (pp y))

    (defn- smile
      "I am a defn- docstring."
      [z]
      (pp [:z z]))
    ``
    {:pattern "smile"})
  # =>
  @[@{:bc 3 :bl 2
      :def-type "defn"
      :name "smile"
      :text `"I am a defn docstring."`
      :params-str "[y]"}
    @{:bc 3 :bl 7
      :def-type "defn-"
      :name "smile"
      :text `"I am a defn- docstring."`
      :params-str "[z]"}]

  (f/find-doc-of
    ``
    (var smile "a docstring" {:a 2})

    (var- smile "woohoo" "hello")
    ``
    {:pattern "smile"})
  # =>
  @[@{:bc 12 :bl 1
      :def-type "var"
      :name "smile"
      :text `"a docstring"`}
    @{:bc 13 :bl 3
      :def-type "var-"
      :name "smile"
      :text `"woohoo"`}]

  (f/find-doc-of
    ``
    (defdyn *smile*)

    (defdyn *smile* "smiling docstring")
    ``
    {:pattern "*smile*"})
  # =>
  @[@{:bc 17 :bl 3
      :def-type "defdyn"
      :name "*smile*"
      :text `"smiling docstring"`}]

  (f/find-doc-of
    ```
    (defmacro as-macro
      ``Use a function or macro literal `f` as a macro. This lets
      any function be used as a macro. Inside a quasiquote, the
      idiom `(as-macro ,my-custom-macro arg1 arg2...)` can be used
      to avoid unwanted variable capture of `my-custom-macro`.``
      [f & args]
      (f ;args))
    ```
    {:pattern "as-macro"})

  # =>
  @[@{:bc 3 :bl 2
      :def-type "defmacro"
      :name "as-macro"
      :text
      (string
        "``Use a function or macro literal `f` as a macro. This lets\n"
        "  any function be used as a macro. Inside a quasiquote, the\n"
        "  idiom `(as-macro ,my-custom-macro arg1 arg2...)` can be used\n"
        "  to avoid unwanted variable capture of `my-custom-macro`.``")
      :params-str "[f & args]"}]

  (f/find-doc-of
    ``
    (def smile "a docstring" 1)

    (defn smile
      "I am a docstring."
      [y]
      (pp y))
    ``
    {:pattern "smile"})
  # =>
  @[@{:bc 12 :bl 1
      :def-type "def"
      :name "smile"
      :text `"a docstring"`}
    @{:bc 3 :bl 4
      :def-type "defn"
      :name "smile"
      :text `"I am a docstring."`
      :params-str "[y]"}]

  )

(defn f/find-docs
  [src &opt opts]
  (default opts {})
  (def {:pred pred} opts)
  #
  (def tree (j/par src))
  (var cur-zloc (j/zip-down tree))
  (def results @[])
  #
  (while (def next-zloc
           (j/search-from cur-zloc
                          |(match (j/node $)
                             [:tuple]
                             $)))
    (def node (j/node next-zloc))
    (def raw-code-str (j/gen node))
    (def parsed
      (try
        (parse raw-code-str)
        ([_e]
          (eprintf "failed to parse: %s" raw-code-str))))
    (when (and parsed
               (if-not pred true (pred parsed)))
      (when-let [head (first parsed)]
        (when (symbol? head)
          (def name (string (get parsed 1)))
          (cond
            (and (get f/special-definers head)
                 (<= 4 (length parsed))) # metadata possible
            (when-let [ds-tbl (f/find-special-docstring (j/down next-zloc))]
              (array/push results
                          (merge ds-tbl {:def-type (string head)
                                         :name name})))
            #
            (get f/call-definers head)
            (when-let [ds-tbl (f/find-caller-docstring (j/down next-zloc))]
              (array/push results
                          (merge ds-tbl {:def-type (string head)
                                         :name name})))
            #
            (and (= 'defdyn head) (= 3 (length parsed)))
            (when-let [ds-tbl (f/find-defdyn-docstring (j/down next-zloc))]
              (array/push results
                          (merge ds-tbl {:def-type (string head)
                                         :name name})))
            # XXX: other cases?
            nil))))
    #
    (set cur-zloc (j/df-next next-zloc)))
  #
  results)

(comment

  (f/find-docs
    ``
    (defn smile
      "I am a defn docstring."
      [y]
      (pp y))

    (defn- smile
      "I am a defn- docstring."
      [z]
      (pp [:z z]))
    ``)
  # =>
  @[@{:bc 3 :bl 2
      :def-type "defn"
      :name "smile"
      :text `"I am a defn docstring."`
      :params-str "[y]"}
    @{:bc 3 :bl 7
      :def-type "defn-"
      :name "smile"
      :text `"I am a defn- docstring."`
      :params-str "[z]"}]

  (f/find-docs
    ``
    (var smile "a docstring" {:a 2})

    (var- smile "woohoo" "hello")
    ``)
  # =>
  @[@{:bc 12 :bl 1
      :def-type "var"
      :name "smile"
      :text `"a docstring"`}
    @{:bc 13 :bl 3
      :def-type "var-"
      :name "smile"
      :text `"woohoo"`}]

  (f/find-docs
    ``
    (defdyn *smile*)

    (defdyn *smile* "smiling docstring")
    ``)
  # =>
  @[@{:bc 17 :bl 3
      :def-type "defdyn"
      :name "*smile*"
      :text `"smiling docstring"`}]

  (f/find-docs
    ```
    (defmacro as-macro
      ``Use a function or macro literal `f` as a macro. This lets
      any function be used as a macro. Inside a quasiquote, the
      idiom `(as-macro ,my-custom-macro arg1 arg2...)` can be used
      to avoid unwanted variable capture of `my-custom-macro`.``
      [f & args]
      (f ;args))
    ```)
  # =>
  @[@{:bc 3 :bl 2
      :def-type "defmacro"
      :name "as-macro"
      :text
      (string
        "``Use a function or macro literal `f` as a macro. This lets\n"
        "  any function be used as a macro. Inside a quasiquote, the\n"
        "  idiom `(as-macro ,my-custom-macro arg1 arg2...)` can be used\n"
        "  to avoid unwanted variable capture of `my-custom-macro`.``")
      :params-str "[f & args]"}]

  (f/find-docs
    ``
    (def smile "a docstring" 1)

    (defn smile
      "I am a docstring."
      [y]
      (pp y))
    ``)
  # =>
  @[@{:bc 12 :bl 1
      :def-type "def"
      :name "smile"
      :text `"a docstring"`}
    @{:bc 3 :bl 4
      :def-type "defn"
      :name "smile"
      :text `"I am a docstring."`
      :params-str "[y]"}]

  )


(comment import ./remarkable/remarkable :prefix "")
(comment import ./state :prefix "")
(def state/protocols @{})
(def state/priorities @{})

(def state/indents @[])
(def state/links @{})
(def state/delimiters @[])

# Default blocks list for building custom grammars
(def state/blocks @['+
              :blank
              :codeblock
              :t-break
              :html
              :linkdef
              :blockquote
              :list
              :heading
              :paragraph])

# Default inlines list for building custom grammars
(def state/inlines @['+
               :codespan
               :rawhtml
               :autolink
               :hardbreak
               :emphasis
               :link
               :text])

# Custom block grammars for building custom grammars
(def state/custom-block-grammars @{})

# Custom inline grammars for building custom grammars
(def state/custom-inline-grammars @{})

# Custom block protocols for building custom protocols
(def state/custom-block-protocols @{})

# Custom inline protocols for building custom protocols
(def state/custom-inline-protocols @{})

# Custom inline delimiters for excluding from :char
(def state/custom-inline-delimiters @"")

(var state/col-edge 0)
(var state/col-pos 0)

(defn state/reset-block-globals []
  (array/clear state/indents)
  (each k (keys state/links)
    (put state/links k nil)))

(defn state/reset-inline-globals []
  (array/clear state/delimiters))

(defn state/reset-cols []
  (set state/col-edge 0)
  (set state/col-pos 0))


(comment import ./blocks :prefix "")
(comment import ./state :prefix "")

(comment import ./util :prefix "")
(comment import ./entities :prefix "")
(def entities/entity-map
  {
   "&AElig;" "\xC3\x86"
   "&AMP;" "&"
   "&Aacute;" "\xC3\x81"
   "&Abreve;" "\xC4\x82"
   "&Acirc;" "\xC3\x82"
   "&Acy;" "\xD0\x90"
   "&Afr;" "\xF0\x9D\x94\x84"
   "&Agrave;" "\xC3\x80"
   "&Alpha;" "\xCE\x91"
   "&Amacr;" "\xC4\x80"
   "&And;" "\xE2\xA9\x93"
   "&Aogon;" "\xC4\x84"
   "&Aopf;" "\xF0\x9D\x94\xB8"
   "&ApplyFunction;" "\xE2\x81\xA1"
   "&Aring;" "\xC3\x85"
   "&Ascr;" "\xF0\x9D\x92\x9C"
   "&Assign;" "\xE2\x89\x94"
   "&Atilde;" "\xC3\x83"
   "&Auml;" "\xC3\x84"
   "&Backslash;" "\xE2\x88\x96"
   "&Barv;" "\xE2\xAB\xA7"
   "&Barwed;" "\xE2\x8C\x86"
   "&Bcy;" "\xD0\x91"
   "&Because;" "\xE2\x88\xB5"
   "&Bernoullis;" "\xE2\x84\xAC"
   "&Beta;" "\xCE\x92"
   "&Bfr;" "\xF0\x9D\x94\x85"
   "&Bopf;" "\xF0\x9D\x94\xB9"
   "&Breve;" "\xCB\x98"
   "&Bscr;" "\xE2\x84\xAC"
   "&Bumpeq;" "\xE2\x89\x8E"
   "&CHcy;" "\xD0\xA7"
   "&COPY;" "\xC2\xA9"
   "&Cacute;" "\xC4\x86"
   "&Cap;" "\xE2\x8B\x92"
   "&CapitalDifferentialD;" "\xE2\x85\x85"
   "&Cayleys;" "\xE2\x84\xAD"
   "&Ccaron;" "\xC4\x8C"
   "&Ccedil;" "\xC3\x87"
   "&Ccirc;" "\xC4\x88"
   "&Cconint;" "\xE2\x88\xB0"
   "&Cdot;" "\xC4\x8A"
   "&Cedilla;" "\xC2\xB8"
   "&CenterDot;" "\xC2\xB7"
   "&Cfr;" "\xE2\x84\xAD"
   "&Chi;" "\xCE\xA7"
   "&CircleDot;" "\xE2\x8A\x99"
   "&CircleMinus;" "\xE2\x8A\x96"
   "&CirclePlus;" "\xE2\x8A\x95"
   "&CircleTimes;" "\xE2\x8A\x97"
   "&ClockwiseContourIntegral;" "\xE2\x88\xB2"
   "&CloseCurlyDoubleQuote;" "\xE2\x80\x9D"
   "&CloseCurlyQuote;" "\xE2\x80\x99"
   "&Colon;" "\xE2\x88\xB7"
   "&Colone;" "\xE2\xA9\xB4"
   "&Congruent;" "\xE2\x89\xA1"
   "&Conint;" "\xE2\x88\xAF"
   "&ContourIntegral;" "\xE2\x88\xAE"
   "&Copf;" "\xE2\x84\x82"
   "&Coproduct;" "\xE2\x88\x90"
   "&CounterClockwiseContourIntegral;" "\xE2\x88\xB3"
   "&Cross;" "\xE2\xA8\xAF"
   "&Cscr;" "\xF0\x9D\x92\x9E"
   "&Cup;" "\xE2\x8B\x93"
   "&CupCap;" "\xE2\x89\x8D"
   "&DD;" "\xE2\x85\x85"
   "&DDotrahd;" "\xE2\xA4\x91"
   "&DJcy;" "\xD0\x82"
   "&DScy;" "\xD0\x85"
   "&DZcy;" "\xD0\x8F"
   "&Dagger;" "\xE2\x80\xA1"
   "&Darr;" "\xE2\x86\xA1"
   "&Dashv;" "\xE2\xAB\xA4"
   "&Dcaron;" "\xC4\x8E"
   "&Dcy;" "\xD0\x94"
   "&Del;" "\xE2\x88\x87"
   "&Delta;" "\xCE\x94"
   "&Dfr;" "\xF0\x9D\x94\x87"
   "&DiacriticalAcute;" "\xC2\xB4"
   "&DiacriticalDot;" "\xCB\x99"
   "&DiacriticalDoubleAcute;" "\xCB\x9D"
   "&DiacriticalGrave;" "`"
   "&DiacriticalTilde;" "\xCB\x9C"
   "&Diamond;" "\xE2\x8B\x84"
   "&DifferentialD;" "\xE2\x85\x86"
   "&Dopf;" "\xF0\x9D\x94\xBB"
   "&Dot;" "\xC2\xA8"
   "&DotDot;" "\xE2\x83\x9C"
   "&DotEqual;" "\xE2\x89\x90"
   "&DoubleContourIntegral;" "\xE2\x88\xAF"
   "&DoubleDot;" "\xC2\xA8"
   "&DoubleDownArrow;" "\xE2\x87\x93"
   "&DoubleLeftArrow;" "\xE2\x87\x90"
   "&DoubleLeftRightArrow;" "\xE2\x87\x94"
   "&DoubleLeftTee;" "\xE2\xAB\xA4"
   "&DoubleLongLeftArrow;" "\xE2\x9F\xB8"
   "&DoubleLongLeftRightArrow;" "\xE2\x9F\xBA"
   "&DoubleLongRightArrow;" "\xE2\x9F\xB9"
   "&DoubleRightArrow;" "\xE2\x87\x92"
   "&DoubleRightTee;" "\xE2\x8A\xA8"
   "&DoubleUpArrow;" "\xE2\x87\x91"
   "&DoubleUpDownArrow;" "\xE2\x87\x95"
   "&DoubleVerticalBar;" "\xE2\x88\xA5"
   "&DownArrow;" "\xE2\x86\x93"
   "&DownArrowBar;" "\xE2\xA4\x93"
   "&DownArrowUpArrow;" "\xE2\x87\xB5"
   "&DownBreve;" "\xCC\x91"
   "&DownLeftRightVector;" "\xE2\xA5\x90"
   "&DownLeftTeeVector;" "\xE2\xA5\x9E"
   "&DownLeftVector;" "\xE2\x86\xBD"
   "&DownLeftVectorBar;" "\xE2\xA5\x96"
   "&DownRightTeeVector;" "\xE2\xA5\x9F"
   "&DownRightVector;" "\xE2\x87\x81"
   "&DownRightVectorBar;" "\xE2\xA5\x97"
   "&DownTee;" "\xE2\x8A\xA4"
   "&DownTeeArrow;" "\xE2\x86\xA7"
   "&Downarrow;" "\xE2\x87\x93"
   "&Dscr;" "\xF0\x9D\x92\x9F"
   "&Dstrok;" "\xC4\x90"
   "&ENG;" "\xC5\x8A"
   "&ETH;" "\xC3\x90"
   "&Eacute;" "\xC3\x89"
   "&Ecaron;" "\xC4\x9A"
   "&Ecirc;" "\xC3\x8A"
   "&Ecy;" "\xD0\xAD"
   "&Edot;" "\xC4\x96"
   "&Efr;" "\xF0\x9D\x94\x88"
   "&Egrave;" "\xC3\x88"
   "&Element;" "\xE2\x88\x88"
   "&Emacr;" "\xC4\x92"
   "&EmptySmallSquare;" "\xE2\x97\xBB"
   "&EmptyVerySmallSquare;" "\xE2\x96\xAB"
   "&Eogon;" "\xC4\x98"
   "&Eopf;" "\xF0\x9D\x94\xBC"
   "&Epsilon;" "\xCE\x95"
   "&Equal;" "\xE2\xA9\xB5"
   "&EqualTilde;" "\xE2\x89\x82"
   "&Equilibrium;" "\xE2\x87\x8C"
   "&Escr;" "\xE2\x84\xB0"
   "&Esim;" "\xE2\xA9\xB3"
   "&Eta;" "\xCE\x97"
   "&Euml;" "\xC3\x8B"
   "&Exists;" "\xE2\x88\x83"
   "&ExponentialE;" "\xE2\x85\x87"
   "&Fcy;" "\xD0\xA4"
   "&Ffr;" "\xF0\x9D\x94\x89"
   "&FilledSmallSquare;" "\xE2\x97\xBC"
   "&FilledVerySmallSquare;" "\xE2\x96\xAA"
   "&Fopf;" "\xF0\x9D\x94\xBD"
   "&ForAll;" "\xE2\x88\x80"
   "&Fouriertrf;" "\xE2\x84\xB1"
   "&Fscr;" "\xE2\x84\xB1"
   "&GJcy;" "\xD0\x83"
   "&GT;" ">"
   "&Gamma;" "\xCE\x93"
   "&Gammad;" "\xCF\x9C"
   "&Gbreve;" "\xC4\x9E"
   "&Gcedil;" "\xC4\xA2"
   "&Gcirc;" "\xC4\x9C"
   "&Gcy;" "\xD0\x93"
   "&Gdot;" "\xC4\xA0"
   "&Gfr;" "\xF0\x9D\x94\x8A"
   "&Gg;" "\xE2\x8B\x99"
   "&Gopf;" "\xF0\x9D\x94\xBE"
   "&GreaterEqual;" "\xE2\x89\xA5"
   "&GreaterEqualLess;" "\xE2\x8B\x9B"
   "&GreaterFullEqual;" "\xE2\x89\xA7"
   "&GreaterGreater;" "\xE2\xAA\xA2"
   "&GreaterLess;" "\xE2\x89\xB7"
   "&GreaterSlantEqual;" "\xE2\xA9\xBE"
   "&GreaterTilde;" "\xE2\x89\xB3"
   "&Gscr;" "\xF0\x9D\x92\xA2"
   "&Gt;" "\xE2\x89\xAB"
   "&HARDcy;" "\xD0\xAA"
   "&Hacek;" "\xCB\x87"
   "&Hat;" "^"
   "&Hcirc;" "\xC4\xA4"
   "&Hfr;" "\xE2\x84\x8C"
   "&HilbertSpace;" "\xE2\x84\x8B"
   "&Hopf;" "\xE2\x84\x8D"
   "&HorizontalLine;" "\xE2\x94\x80"
   "&Hscr;" "\xE2\x84\x8B"
   "&Hstrok;" "\xC4\xA6"
   "&HumpDownHump;" "\xE2\x89\x8E"
   "&HumpEqual;" "\xE2\x89\x8F"
   "&IEcy;" "\xD0\x95"
   "&IJlig;" "\xC4\xB2"
   "&IOcy;" "\xD0\x81"
   "&Iacute;" "\xC3\x8D"
   "&Icirc;" "\xC3\x8E"
   "&Icy;" "\xD0\x98"
   "&Idot;" "\xC4\xB0"
   "&Ifr;" "\xE2\x84\x91"
   "&Igrave;" "\xC3\x8C"
   "&Im;" "\xE2\x84\x91"
   "&Imacr;" "\xC4\xAA"
   "&ImaginaryI;" "\xE2\x85\x88"
   "&Implies;" "\xE2\x87\x92"
   "&Int;" "\xE2\x88\xAC"
   "&Integral;" "\xE2\x88\xAB"
   "&Intersection;" "\xE2\x8B\x82"
   "&InvisibleComma;" "\xE2\x81\xA3"
   "&InvisibleTimes;" "\xE2\x81\xA2"
   "&Iogon;" "\xC4\xAE"
   "&Iopf;" "\xF0\x9D\x95\x80"
   "&Iota;" "\xCE\x99"
   "&Iscr;" "\xE2\x84\x90"
   "&Itilde;" "\xC4\xA8"
   "&Iukcy;" "\xD0\x86"
   "&Iuml;" "\xC3\x8F"
   "&Jcirc;" "\xC4\xB4"
   "&Jcy;" "\xD0\x99"
   "&Jfr;" "\xF0\x9D\x94\x8D"
   "&Jopf;" "\xF0\x9D\x95\x81"
   "&Jscr;" "\xF0\x9D\x92\xA5"
   "&Jsercy;" "\xD0\x88"
   "&Jukcy;" "\xD0\x84"
   "&KHcy;" "\xD0\xA5"
   "&KJcy;" "\xD0\x8C"
   "&Kappa;" "\xCE\x9A"
   "&Kcedil;" "\xC4\xB6"
   "&Kcy;" "\xD0\x9A"
   "&Kfr;" "\xF0\x9D\x94\x8E"
   "&Kopf;" "\xF0\x9D\x95\x82"
   "&Kscr;" "\xF0\x9D\x92\xA6"
   "&LJcy;" "\xD0\x89"
   "&LT;" "<"
   "&Lacute;" "\xC4\xB9"
   "&Lambda;" "\xCE\x9B"
   "&Lang;" "\xE2\x9F\xAA"
   "&Laplacetrf;" "\xE2\x84\x92"
   "&Larr;" "\xE2\x86\x9E"
   "&Lcaron;" "\xC4\xBD"
   "&Lcedil;" "\xC4\xBB"
   "&Lcy;" "\xD0\x9B"
   "&LeftAngleBracket;" "\xE2\x9F\xA8"
   "&LeftArrow;" "\xE2\x86\x90"
   "&LeftArrowBar;" "\xE2\x87\xA4"
   "&LeftArrowRightArrow;" "\xE2\x87\x86"
   "&LeftCeiling;" "\xE2\x8C\x88"
   "&LeftDoubleBracket;" "\xE2\x9F\xA6"
   "&LeftDownTeeVector;" "\xE2\xA5\xA1"
   "&LeftDownVector;" "\xE2\x87\x83"
   "&LeftDownVectorBar;" "\xE2\xA5\x99"
   "&LeftFloor;" "\xE2\x8C\x8A"
   "&LeftRightArrow;" "\xE2\x86\x94"
   "&LeftRightVector;" "\xE2\xA5\x8E"
   "&LeftTee;" "\xE2\x8A\xA3"
   "&LeftTeeArrow;" "\xE2\x86\xA4"
   "&LeftTeeVector;" "\xE2\xA5\x9A"
   "&LeftTriangle;" "\xE2\x8A\xB2"
   "&LeftTriangleBar;" "\xE2\xA7\x8F"
   "&LeftTriangleEqual;" "\xE2\x8A\xB4"
   "&LeftUpDownVector;" "\xE2\xA5\x91"
   "&LeftUpTeeVector;" "\xE2\xA5\xA0"
   "&LeftUpVector;" "\xE2\x86\xBF"
   "&LeftUpVectorBar;" "\xE2\xA5\x98"
   "&LeftVector;" "\xE2\x86\xBC"
   "&LeftVectorBar;" "\xE2\xA5\x92"
   "&Leftarrow;" "\xE2\x87\x90"
   "&Leftrightarrow;" "\xE2\x87\x94"
   "&LessEqualGreater;" "\xE2\x8B\x9A"
   "&LessFullEqual;" "\xE2\x89\xA6"
   "&LessGreater;" "\xE2\x89\xB6"
   "&LessLess;" "\xE2\xAA\xA1"
   "&LessSlantEqual;" "\xE2\xA9\xBD"
   "&LessTilde;" "\xE2\x89\xB2"
   "&Lfr;" "\xF0\x9D\x94\x8F"
   "&Ll;" "\xE2\x8B\x98"
   "&Lleftarrow;" "\xE2\x87\x9A"
   "&Lmidot;" "\xC4\xBF"
   "&LongLeftArrow;" "\xE2\x9F\xB5"
   "&LongLeftRightArrow;" "\xE2\x9F\xB7"
   "&LongRightArrow;" "\xE2\x9F\xB6"
   "&Longleftarrow;" "\xE2\x9F\xB8"
   "&Longleftrightarrow;" "\xE2\x9F\xBA"
   "&Longrightarrow;" "\xE2\x9F\xB9"
   "&Lopf;" "\xF0\x9D\x95\x83"
   "&LowerLeftArrow;" "\xE2\x86\x99"
   "&LowerRightArrow;" "\xE2\x86\x98"
   "&Lscr;" "\xE2\x84\x92"
   "&Lsh;" "\xE2\x86\xB0"
   "&Lstrok;" "\xC5\x81"
   "&Lt;" "\xE2\x89\xAA"
   "&Map;" "\xE2\xA4\x85"
   "&Mcy;" "\xD0\x9C"
   "&MediumSpace;" "\xE2\x81\x9F"
   "&Mellintrf;" "\xE2\x84\xB3"
   "&Mfr;" "\xF0\x9D\x94\x90"
   "&MinusPlus;" "\xE2\x88\x93"
   "&Mopf;" "\xF0\x9D\x95\x84"
   "&Mscr;" "\xE2\x84\xB3"
   "&Mu;" "\xCE\x9C"
   "&NJcy;" "\xD0\x8A"
   "&Nacute;" "\xC5\x83"
   "&Ncaron;" "\xC5\x87"
   "&Ncedil;" "\xC5\x85"
   "&Ncy;" "\xD0\x9D"
   "&NegativeMediumSpace;" "\xE2\x80\x8B"
   "&NegativeThickSpace;" "\xE2\x80\x8B"
   "&NegativeThinSpace;" "\xE2\x80\x8B"
   "&NegativeVeryThinSpace;" "\xE2\x80\x8B"
   "&NestedGreaterGreater;" "\xE2\x89\xAB"
   "&NestedLessLess;" "\xE2\x89\xAA"
   "&NewLine;" "\n"
   "&Nfr;" "\xF0\x9D\x94\x91"
   "&NoBreak;" "\xE2\x81\xA0"
   "&NonBreakingSpace;" "\xC2\xA0"
   "&Nopf;" "\xE2\x84\x95"
   "&Not;" "\xE2\xAB\xAC"
   "&NotCongruent;" "\xE2\x89\xA2"
   "&NotCupCap;" "\xE2\x89\xAD"
   "&NotDoubleVerticalBar;" "\xE2\x88\xA6"
   "&NotElement;" "\xE2\x88\x89"
   "&NotEqual;" "\xE2\x89\xA0"
   "&NotEqualTilde;" "\xE2\x89\x82\xCC\xB8"
   "&NotExists;" "\xE2\x88\x84"
   "&NotGreater;" "\xE2\x89\xAF"
   "&NotGreaterEqual;" "\xE2\x89\xB1"
   "&NotGreaterFullEqual;" "\xE2\x89\xA7\xCC\xB8"
   "&NotGreaterGreater;" "\xE2\x89\xAB\xCC\xB8"
   "&NotGreaterLess;" "\xE2\x89\xB9"
   "&NotGreaterSlantEqual;" "\xE2\xA9\xBE\xCC\xB8"
   "&NotGreaterTilde;" "\xE2\x89\xB5"
   "&NotHumpDownHump;" "\xE2\x89\x8E\xCC\xB8"
   "&NotHumpEqual;" "\xE2\x89\x8F\xCC\xB8"
   "&NotLeftTriangle;" "\xE2\x8B\xAA"
   "&NotLeftTriangleBar;" "\xE2\xA7\x8F\xCC\xB8"
   "&NotLeftTriangleEqual;" "\xE2\x8B\xAC"
   "&NotLess;" "\xE2\x89\xAE"
   "&NotLessEqual;" "\xE2\x89\xB0"
   "&NotLessGreater;" "\xE2\x89\xB8"
   "&NotLessLess;" "\xE2\x89\xAA\xCC\xB8"
   "&NotLessSlantEqual;" "\xE2\xA9\xBD\xCC\xB8"
   "&NotLessTilde;" "\xE2\x89\xB4"
   "&NotNestedGreaterGreater;" "\xE2\xAA\xA2\xCC\xB8"
   "&NotNestedLessLess;" "\xE2\xAA\xA1\xCC\xB8"
   "&NotPrecedes;" "\xE2\x8A\x80"
   "&NotPrecedesEqual;" "\xE2\xAA\xAF\xCC\xB8"
   "&NotPrecedesSlantEqual;" "\xE2\x8B\xA0"
   "&NotReverseElement;" "\xE2\x88\x8C"
   "&NotRightTriangle;" "\xE2\x8B\xAB"
   "&NotRightTriangleBar;" "\xE2\xA7\x90\xCC\xB8"
   "&NotRightTriangleEqual;" "\xE2\x8B\xAD"
   "&NotSquareSubset;" "\xE2\x8A\x8F\xCC\xB8"
   "&NotSquareSubsetEqual;" "\xE2\x8B\xA2"
   "&NotSquareSuperset;" "\xE2\x8A\x90\xCC\xB8"
   "&NotSquareSupersetEqual;" "\xE2\x8B\xA3"
   "&NotSubset;" "\xE2\x8A\x82\xE2\x83\x92"
   "&NotSubsetEqual;" "\xE2\x8A\x88"
   "&NotSucceeds;" "\xE2\x8A\x81"
   "&NotSucceedsEqual;" "\xE2\xAA\xB0\xCC\xB8"
   "&NotSucceedsSlantEqual;" "\xE2\x8B\xA1"
   "&NotSucceedsTilde;" "\xE2\x89\xBF\xCC\xB8"
   "&NotSuperset;" "\xE2\x8A\x83\xE2\x83\x92"
   "&NotSupersetEqual;" "\xE2\x8A\x89"
   "&NotTilde;" "\xE2\x89\x81"
   "&NotTildeEqual;" "\xE2\x89\x84"
   "&NotTildeFullEqual;" "\xE2\x89\x87"
   "&NotTildeTilde;" "\xE2\x89\x89"
   "&NotVerticalBar;" "\xE2\x88\xA4"
   "&Nscr;" "\xF0\x9D\x92\xA9"
   "&Ntilde;" "\xC3\x91"
   "&Nu;" "\xCE\x9D"
   "&OElig;" "\xC5\x92"
   "&Oacute;" "\xC3\x93"
   "&Ocirc;" "\xC3\x94"
   "&Ocy;" "\xD0\x9E"
   "&Odblac;" "\xC5\x90"
   "&Ofr;" "\xF0\x9D\x94\x92"
   "&Ograve;" "\xC3\x92"
   "&Omacr;" "\xC5\x8C"
   "&Omega;" "\xCE\xA9"
   "&Omicron;" "\xCE\x9F"
   "&Oopf;" "\xF0\x9D\x95\x86"
   "&OpenCurlyDoubleQuote;" "\xE2\x80\x9C"
   "&OpenCurlyQuote;" "\xE2\x80\x98"
   "&Or;" "\xE2\xA9\x94"
   "&Oscr;" "\xF0\x9D\x92\xAA"
   "&Oslash;" "\xC3\x98"
   "&Otilde;" "\xC3\x95"
   "&Otimes;" "\xE2\xA8\xB7"
   "&Ouml;" "\xC3\x96"
   "&OverBar;" "\xE2\x80\xBE"
   "&OverBrace;" "\xE2\x8F\x9E"
   "&OverBracket;" "\xE2\x8E\xB4"
   "&OverParenthesis;" "\xE2\x8F\x9C"
   "&PartialD;" "\xE2\x88\x82"
   "&Pcy;" "\xD0\x9F"
   "&Pfr;" "\xF0\x9D\x94\x93"
   "&Phi;" "\xCE\xA6"
   "&Pi;" "\xCE\xA0"
   "&PlusMinus;" "\xC2\xB1"
   "&Poincareplane;" "\xE2\x84\x8C"
   "&Popf;" "\xE2\x84\x99"
   "&Pr;" "\xE2\xAA\xBB"
   "&Precedes;" "\xE2\x89\xBA"
   "&PrecedesEqual;" "\xE2\xAA\xAF"
   "&PrecedesSlantEqual;" "\xE2\x89\xBC"
   "&PrecedesTilde;" "\xE2\x89\xBE"
   "&Prime;" "\xE2\x80\xB3"
   "&Product;" "\xE2\x88\x8F"
   "&Proportion;" "\xE2\x88\xB7"
   "&Proportional;" "\xE2\x88\x9D"
   "&Pscr;" "\xF0\x9D\x92\xAB"
   "&Psi;" "\xCE\xA8"
   "&QUOT;" "\""
   "&Qfr;" "\xF0\x9D\x94\x94"
   "&Qopf;" "\xE2\x84\x9A"
   "&Qscr;" "\xF0\x9D\x92\xAC"
   "&RBarr;" "\xE2\xA4\x90"
   "&REG;" "\xC2\xAE"
   "&Racute;" "\xC5\x94"
   "&Rang;" "\xE2\x9F\xAB"
   "&Rarr;" "\xE2\x86\xA0"
   "&Rarrtl;" "\xE2\xA4\x96"
   "&Rcaron;" "\xC5\x98"
   "&Rcedil;" "\xC5\x96"
   "&Rcy;" "\xD0\xA0"
   "&Re;" "\xE2\x84\x9C"
   "&ReverseElement;" "\xE2\x88\x8B"
   "&ReverseEquilibrium;" "\xE2\x87\x8B"
   "&ReverseUpEquilibrium;" "\xE2\xA5\xAF"
   "&Rfr;" "\xE2\x84\x9C"
   "&Rho;" "\xCE\xA1"
   "&RightAngleBracket;" "\xE2\x9F\xA9"
   "&RightArrow;" "\xE2\x86\x92"
   "&RightArrowBar;" "\xE2\x87\xA5"
   "&RightArrowLeftArrow;" "\xE2\x87\x84"
   "&RightCeiling;" "\xE2\x8C\x89"
   "&RightDoubleBracket;" "\xE2\x9F\xA7"
   "&RightDownTeeVector;" "\xE2\xA5\x9D"
   "&RightDownVector;" "\xE2\x87\x82"
   "&RightDownVectorBar;" "\xE2\xA5\x95"
   "&RightFloor;" "\xE2\x8C\x8B"
   "&RightTee;" "\xE2\x8A\xA2"
   "&RightTeeArrow;" "\xE2\x86\xA6"
   "&RightTeeVector;" "\xE2\xA5\x9B"
   "&RightTriangle;" "\xE2\x8A\xB3"
   "&RightTriangleBar;" "\xE2\xA7\x90"
   "&RightTriangleEqual;" "\xE2\x8A\xB5"
   "&RightUpDownVector;" "\xE2\xA5\x8F"
   "&RightUpTeeVector;" "\xE2\xA5\x9C"
   "&RightUpVector;" "\xE2\x86\xBE"
   "&RightUpVectorBar;" "\xE2\xA5\x94"
   "&RightVector;" "\xE2\x87\x80"
   "&RightVectorBar;" "\xE2\xA5\x93"
   "&Rightarrow;" "\xE2\x87\x92"
   "&Ropf;" "\xE2\x84\x9D"
   "&RoundImplies;" "\xE2\xA5\xB0"
   "&Rrightarrow;" "\xE2\x87\x9B"
   "&Rscr;" "\xE2\x84\x9B"
   "&Rsh;" "\xE2\x86\xB1"
   "&RuleDelayed;" "\xE2\xA7\xB4"
   "&SHCHcy;" "\xD0\xA9"
   "&SHcy;" "\xD0\xA8"
   "&SOFTcy;" "\xD0\xAC"
   "&Sacute;" "\xC5\x9A"
   "&Sc;" "\xE2\xAA\xBC"
   "&Scaron;" "\xC5\xA0"
   "&Scedil;" "\xC5\x9E"
   "&Scirc;" "\xC5\x9C"
   "&Scy;" "\xD0\xA1"
   "&Sfr;" "\xF0\x9D\x94\x96"
   "&ShortDownArrow;" "\xE2\x86\x93"
   "&ShortLeftArrow;" "\xE2\x86\x90"
   "&ShortRightArrow;" "\xE2\x86\x92"
   "&ShortUpArrow;" "\xE2\x86\x91"
   "&Sigma;" "\xCE\xA3"
   "&SmallCircle;" "\xE2\x88\x98"
   "&Sopf;" "\xF0\x9D\x95\x8A"
   "&Sqrt;" "\xE2\x88\x9A"
   "&Square;" "\xE2\x96\xA1"
   "&SquareIntersection;" "\xE2\x8A\x93"
   "&SquareSubset;" "\xE2\x8A\x8F"
   "&SquareSubsetEqual;" "\xE2\x8A\x91"
   "&SquareSuperset;" "\xE2\x8A\x90"
   "&SquareSupersetEqual;" "\xE2\x8A\x92"
   "&SquareUnion;" "\xE2\x8A\x94"
   "&Sscr;" "\xF0\x9D\x92\xAE"
   "&Star;" "\xE2\x8B\x86"
   "&Sub;" "\xE2\x8B\x90"
   "&Subset;" "\xE2\x8B\x90"
   "&SubsetEqual;" "\xE2\x8A\x86"
   "&Succeeds;" "\xE2\x89\xBB"
   "&SucceedsEqual;" "\xE2\xAA\xB0"
   "&SucceedsSlantEqual;" "\xE2\x89\xBD"
   "&SucceedsTilde;" "\xE2\x89\xBF"
   "&SuchThat;" "\xE2\x88\x8B"
   "&Sum;" "\xE2\x88\x91"
   "&Sup;" "\xE2\x8B\x91"
   "&Superset;" "\xE2\x8A\x83"
   "&SupersetEqual;" "\xE2\x8A\x87"
   "&Supset;" "\xE2\x8B\x91"
   "&THORN;" "\xC3\x9E"
   "&TRADE;" "\xE2\x84\xA2"
   "&TSHcy;" "\xD0\x8B"
   "&TScy;" "\xD0\xA6"
   "&Tab;" "\t"
   "&Tau;" "\xCE\xA4"
   "&Tcaron;" "\xC5\xA4"
   "&Tcedil;" "\xC5\xA2"
   "&Tcy;" "\xD0\xA2"
   "&Tfr;" "\xF0\x9D\x94\x97"
   "&Therefore;" "\xE2\x88\xB4"
   "&Theta;" "\xCE\x98"
   "&ThickSpace;" "\xE2\x81\x9F\xE2\x80\x8A"
   "&ThinSpace;" "\xE2\x80\x89"
   "&Tilde;" "\xE2\x88\xBC"
   "&TildeEqual;" "\xE2\x89\x83"
   "&TildeFullEqual;" "\xE2\x89\x85"
   "&TildeTilde;" "\xE2\x89\x88"
   "&Topf;" "\xF0\x9D\x95\x8B"
   "&TripleDot;" "\xE2\x83\x9B"
   "&Tscr;" "\xF0\x9D\x92\xAF"
   "&Tstrok;" "\xC5\xA6"
   "&Uacute;" "\xC3\x9A"
   "&Uarr;" "\xE2\x86\x9F"
   "&Uarrocir;" "\xE2\xA5\x89"
   "&Ubrcy;" "\xD0\x8E"
   "&Ubreve;" "\xC5\xAC"
   "&Ucirc;" "\xC3\x9B"
   "&Ucy;" "\xD0\xA3"
   "&Udblac;" "\xC5\xB0"
   "&Ufr;" "\xF0\x9D\x94\x98"
   "&Ugrave;" "\xC3\x99"
   "&Umacr;" "\xC5\xAA"
   "&UnderBar;" "_"
   "&UnderBrace;" "\xE2\x8F\x9F"
   "&UnderBracket;" "\xE2\x8E\xB5"
   "&UnderParenthesis;" "\xE2\x8F\x9D"
   "&Union;" "\xE2\x8B\x83"
   "&UnionPlus;" "\xE2\x8A\x8E"
   "&Uogon;" "\xC5\xB2"
   "&Uopf;" "\xF0\x9D\x95\x8C"
   "&UpArrow;" "\xE2\x86\x91"
   "&UpArrowBar;" "\xE2\xA4\x92"
   "&UpArrowDownArrow;" "\xE2\x87\x85"
   "&UpDownArrow;" "\xE2\x86\x95"
   "&UpEquilibrium;" "\xE2\xA5\xAE"
   "&UpTee;" "\xE2\x8A\xA5"
   "&UpTeeArrow;" "\xE2\x86\xA5"
   "&Uparrow;" "\xE2\x87\x91"
   "&Updownarrow;" "\xE2\x87\x95"
   "&UpperLeftArrow;" "\xE2\x86\x96"
   "&UpperRightArrow;" "\xE2\x86\x97"
   "&Upsi;" "\xCF\x92"
   "&Upsilon;" "\xCE\xA5"
   "&Uring;" "\xC5\xAE"
   "&Uscr;" "\xF0\x9D\x92\xB0"
   "&Utilde;" "\xC5\xA8"
   "&Uuml;" "\xC3\x9C"
   "&VDash;" "\xE2\x8A\xAB"
   "&Vbar;" "\xE2\xAB\xAB"
   "&Vcy;" "\xD0\x92"
   "&Vdash;" "\xE2\x8A\xA9"
   "&Vdashl;" "\xE2\xAB\xA6"
   "&Vee;" "\xE2\x8B\x81"
   "&Verbar;" "\xE2\x80\x96"
   "&Vert;" "\xE2\x80\x96"
   "&VerticalBar;" "\xE2\x88\xA3"
   "&VerticalLine;" "|"
   "&VerticalSeparator;" "\xE2\x9D\x98"
   "&VerticalTilde;" "\xE2\x89\x80"
   "&VeryThinSpace;" "\xE2\x80\x8A"
   "&Vfr;" "\xF0\x9D\x94\x99"
   "&Vopf;" "\xF0\x9D\x95\x8D"
   "&Vscr;" "\xF0\x9D\x92\xB1"
   "&Vvdash;" "\xE2\x8A\xAA"
   "&Wcirc;" "\xC5\xB4"
   "&Wedge;" "\xE2\x8B\x80"
   "&Wfr;" "\xF0\x9D\x94\x9A"
   "&Wopf;" "\xF0\x9D\x95\x8E"
   "&Wscr;" "\xF0\x9D\x92\xB2"
   "&Xfr;" "\xF0\x9D\x94\x9B"
   "&Xi;" "\xCE\x9E"
   "&Xopf;" "\xF0\x9D\x95\x8F"
   "&Xscr;" "\xF0\x9D\x92\xB3"
   "&YAcy;" "\xD0\xAF"
   "&YIcy;" "\xD0\x87"
   "&YUcy;" "\xD0\xAE"
   "&Yacute;" "\xC3\x9D"
   "&Ycirc;" "\xC5\xB6"
   "&Ycy;" "\xD0\xAB"
   "&Yfr;" "\xF0\x9D\x94\x9C"
   "&Yopf;" "\xF0\x9D\x95\x90"
   "&Yscr;" "\xF0\x9D\x92\xB4"
   "&Yuml;" "\xC5\xB8"
   "&ZHcy;" "\xD0\x96"
   "&Zacute;" "\xC5\xB9"
   "&Zcaron;" "\xC5\xBD"
   "&Zcy;" "\xD0\x97"
   "&Zdot;" "\xC5\xBB"
   "&ZeroWidthSpace;" "\xE2\x80\x8B"
   "&Zeta;" "\xCE\x96"
   "&Zfr;" "\xE2\x84\xA8"
   "&Zopf;" "\xE2\x84\xA4"
   "&Zscr;" "\xF0\x9D\x92\xB5"
   "&aacute;" "\xC3\xA1"
   "&abreve;" "\xC4\x83"
   "&ac;" "\xE2\x88\xBE"
   "&acE;" "\xE2\x88\xBE\xCC\xB3"
   "&acd;" "\xE2\x88\xBF"
   "&acirc;" "\xC3\xA2"
   "&acute;" "\xC2\xB4"
   "&acy;" "\xD0\xB0"
   "&aelig;" "\xC3\xA6"
   "&af;" "\xE2\x81\xA1"
   "&afr;" "\xF0\x9D\x94\x9E"
   "&agrave;" "\xC3\xA0"
   "&alefsym;" "\xE2\x84\xB5"
   "&aleph;" "\xE2\x84\xB5"
   "&alpha;" "\xCE\xB1"
   "&amacr;" "\xC4\x81"
   "&amalg;" "\xE2\xA8\xBF"
   "&amp;" "&"
   "&and;" "\xE2\x88\xA7"
   "&andand;" "\xE2\xA9\x95"
   "&andd;" "\xE2\xA9\x9C"
   "&andslope;" "\xE2\xA9\x98"
   "&andv;" "\xE2\xA9\x9A"
   "&ang;" "\xE2\x88\xA0"
   "&ange;" "\xE2\xA6\xA4"
   "&angle;" "\xE2\x88\xA0"
   "&angmsd;" "\xE2\x88\xA1"
   "&angmsdaa;" "\xE2\xA6\xA8"
   "&angmsdab;" "\xE2\xA6\xA9"
   "&angmsdac;" "\xE2\xA6\xAA"
   "&angmsdad;" "\xE2\xA6\xAB"
   "&angmsdae;" "\xE2\xA6\xAC"
   "&angmsdaf;" "\xE2\xA6\xAD"
   "&angmsdag;" "\xE2\xA6\xAE"
   "&angmsdah;" "\xE2\xA6\xAF"
   "&angrt;" "\xE2\x88\x9F"
   "&angrtvb;" "\xE2\x8A\xBE"
   "&angrtvbd;" "\xE2\xA6\x9D"
   "&angsph;" "\xE2\x88\xA2"
   "&angst;" "\xC3\x85"
   "&angzarr;" "\xE2\x8D\xBC"
   "&aogon;" "\xC4\x85"
   "&aopf;" "\xF0\x9D\x95\x92"
   "&ap;" "\xE2\x89\x88"
   "&apE;" "\xE2\xA9\xB0"
   "&apacir;" "\xE2\xA9\xAF"
   "&ape;" "\xE2\x89\x8A"
   "&apid;" "\xE2\x89\x8B"
   "&apos;" "'"
   "&approx;" "\xE2\x89\x88"
   "&approxeq;" "\xE2\x89\x8A"
   "&aring;" "\xC3\xA5"
   "&ascr;" "\xF0\x9D\x92\xB6"
   "&ast;" "*"
   "&asymp;" "\xE2\x89\x88"
   "&asympeq;" "\xE2\x89\x8D"
   "&atilde;" "\xC3\xA3"
   "&auml;" "\xC3\xA4"
   "&awconint;" "\xE2\x88\xB3"
   "&awint;" "\xE2\xA8\x91"
   "&bNot;" "\xE2\xAB\xAD"
   "&backcong;" "\xE2\x89\x8C"
   "&backepsilon;" "\xCF\xB6"
   "&backprime;" "\xE2\x80\xB5"
   "&backsim;" "\xE2\x88\xBD"
   "&backsimeq;" "\xE2\x8B\x8D"
   "&barvee;" "\xE2\x8A\xBD"
   "&barwed;" "\xE2\x8C\x85"
   "&barwedge;" "\xE2\x8C\x85"
   "&bbrk;" "\xE2\x8E\xB5"
   "&bbrktbrk;" "\xE2\x8E\xB6"
   "&bcong;" "\xE2\x89\x8C"
   "&bcy;" "\xD0\xB1"
   "&bdquo;" "\xE2\x80\x9E"
   "&becaus;" "\xE2\x88\xB5"
   "&because;" "\xE2\x88\xB5"
   "&bemptyv;" "\xE2\xA6\xB0"
   "&bepsi;" "\xCF\xB6"
   "&bernou;" "\xE2\x84\xAC"
   "&beta;" "\xCE\xB2"
   "&beth;" "\xE2\x84\xB6"
   "&between;" "\xE2\x89\xAC"
   "&bfr;" "\xF0\x9D\x94\x9F"
   "&bigcap;" "\xE2\x8B\x82"
   "&bigcirc;" "\xE2\x97\xAF"
   "&bigcup;" "\xE2\x8B\x83"
   "&bigodot;" "\xE2\xA8\x80"
   "&bigoplus;" "\xE2\xA8\x81"
   "&bigotimes;" "\xE2\xA8\x82"
   "&bigsqcup;" "\xE2\xA8\x86"
   "&bigstar;" "\xE2\x98\x85"
   "&bigtriangledown;" "\xE2\x96\xBD"
   "&bigtriangleup;" "\xE2\x96\xB3"
   "&biguplus;" "\xE2\xA8\x84"
   "&bigvee;" "\xE2\x8B\x81"
   "&bigwedge;" "\xE2\x8B\x80"
   "&bkarow;" "\xE2\xA4\x8D"
   "&blacklozenge;" "\xE2\xA7\xAB"
   "&blacksquare;" "\xE2\x96\xAA"
   "&blacktriangle;" "\xE2\x96\xB4"
   "&blacktriangledown;" "\xE2\x96\xBE"
   "&blacktriangleleft;" "\xE2\x97\x82"
   "&blacktriangleright;" "\xE2\x96\xB8"
   "&blank;" "\xE2\x90\xA3"
   "&blk12;" "\xE2\x96\x92"
   "&blk14;" "\xE2\x96\x91"
   "&blk34;" "\xE2\x96\x93"
   "&block;" "\xE2\x96\x88"
   "&bne;" "=\xE2\x83\xA5"
   "&bnequiv;" "\xE2\x89\xA1\xE2\x83\xA5"
   "&bnot;" "\xE2\x8C\x90"
   "&bopf;" "\xF0\x9D\x95\x93"
   "&bot;" "\xE2\x8A\xA5"
   "&bottom;" "\xE2\x8A\xA5"
   "&bowtie;" "\xE2\x8B\x88"
   "&boxDL;" "\xE2\x95\x97"
   "&boxDR;" "\xE2\x95\x94"
   "&boxDl;" "\xE2\x95\x96"
   "&boxDr;" "\xE2\x95\x93"
   "&boxH;" "\xE2\x95\x90"
   "&boxHD;" "\xE2\x95\xA6"
   "&boxHU;" "\xE2\x95\xA9"
   "&boxHd;" "\xE2\x95\xA4"
   "&boxHu;" "\xE2\x95\xA7"
   "&boxUL;" "\xE2\x95\x9D"
   "&boxUR;" "\xE2\x95\x9A"
   "&boxUl;" "\xE2\x95\x9C"
   "&boxUr;" "\xE2\x95\x99"
   "&boxV;" "\xE2\x95\x91"
   "&boxVH;" "\xE2\x95\xAC"
   "&boxVL;" "\xE2\x95\xA3"
   "&boxVR;" "\xE2\x95\xA0"
   "&boxVh;" "\xE2\x95\xAB"
   "&boxVl;" "\xE2\x95\xA2"
   "&boxVr;" "\xE2\x95\x9F"
   "&boxbox;" "\xE2\xA7\x89"
   "&boxdL;" "\xE2\x95\x95"
   "&boxdR;" "\xE2\x95\x92"
   "&boxdl;" "\xE2\x94\x90"
   "&boxdr;" "\xE2\x94\x8C"
   "&boxh;" "\xE2\x94\x80"
   "&boxhD;" "\xE2\x95\xA5"
   "&boxhU;" "\xE2\x95\xA8"
   "&boxhd;" "\xE2\x94\xAC"
   "&boxhu;" "\xE2\x94\xB4"
   "&boxminus;" "\xE2\x8A\x9F"
   "&boxplus;" "\xE2\x8A\x9E"
   "&boxtimes;" "\xE2\x8A\xA0"
   "&boxuL;" "\xE2\x95\x9B"
   "&boxuR;" "\xE2\x95\x98"
   "&boxul;" "\xE2\x94\x98"
   "&boxur;" "\xE2\x94\x94"
   "&boxv;" "\xE2\x94\x82"
   "&boxvH;" "\xE2\x95\xAA"
   "&boxvL;" "\xE2\x95\xA1"
   "&boxvR;" "\xE2\x95\x9E"
   "&boxvh;" "\xE2\x94\xBC"
   "&boxvl;" "\xE2\x94\xA4"
   "&boxvr;" "\xE2\x94\x9C"
   "&bprime;" "\xE2\x80\xB5"
   "&breve;" "\xCB\x98"
   "&brvbar;" "\xC2\xA6"
   "&bscr;" "\xF0\x9D\x92\xB7"
   "&bsemi;" "\xE2\x81\x8F"
   "&bsim;" "\xE2\x88\xBD"
   "&bsime;" "\xE2\x8B\x8D"
   "&bsol;" "\\"
   "&bsolb;" "\xE2\xA7\x85"
   "&bsolhsub;" "\xE2\x9F\x88"
   "&bull;" "\xE2\x80\xA2"
   "&bullet;" "\xE2\x80\xA2"
   "&bump;" "\xE2\x89\x8E"
   "&bumpE;" "\xE2\xAA\xAE"
   "&bumpe;" "\xE2\x89\x8F"
   "&bumpeq;" "\xE2\x89\x8F"
   "&cacute;" "\xC4\x87"
   "&cap;" "\xE2\x88\xA9"
   "&capand;" "\xE2\xA9\x84"
   "&capbrcup;" "\xE2\xA9\x89"
   "&capcap;" "\xE2\xA9\x8B"
   "&capcup;" "\xE2\xA9\x87"
   "&capdot;" "\xE2\xA9\x80"
   "&caps;" "\xE2\x88\xA9\xEF\xB8\x80"
   "&caret;" "\xE2\x81\x81"
   "&caron;" "\xCB\x87"
   "&ccaps;" "\xE2\xA9\x8D"
   "&ccaron;" "\xC4\x8D"
   "&ccedil;" "\xC3\xA7"
   "&ccirc;" "\xC4\x89"
   "&ccups;" "\xE2\xA9\x8C"
   "&ccupssm;" "\xE2\xA9\x90"
   "&cdot;" "\xC4\x8B"
   "&cedil;" "\xC2\xB8"
   "&cemptyv;" "\xE2\xA6\xB2"
   "&cent;" "\xC2\xA2"
   "&centerdot;" "\xC2\xB7"
   "&cfr;" "\xF0\x9D\x94\xA0"
   "&chcy;" "\xD1\x87"
   "&check;" "\xE2\x9C\x93"
   "&checkmark;" "\xE2\x9C\x93"
   "&chi;" "\xCF\x87"
   "&cir;" "\xE2\x97\x8B"
   "&cirE;" "\xE2\xA7\x83"
   "&circ;" "\xCB\x86"
   "&circeq;" "\xE2\x89\x97"
   "&circlearrowleft;" "\xE2\x86\xBA"
   "&circlearrowright;" "\xE2\x86\xBB"
   "&circledR;" "\xC2\xAE"
   "&circledS;" "\xE2\x93\x88"
   "&circledast;" "\xE2\x8A\x9B"
   "&circledcirc;" "\xE2\x8A\x9A"
   "&circleddash;" "\xE2\x8A\x9D"
   "&cire;" "\xE2\x89\x97"
   "&cirfnint;" "\xE2\xA8\x90"
   "&cirmid;" "\xE2\xAB\xAF"
   "&cirscir;" "\xE2\xA7\x82"
   "&clubs;" "\xE2\x99\xA3"
   "&clubsuit;" "\xE2\x99\xA3"
   "&colon;" ":"
   "&colone;" "\xE2\x89\x94"
   "&coloneq;" "\xE2\x89\x94"
   "&comma;" ","
   "&commat;" "@"
   "&comp;" "\xE2\x88\x81"
   "&compfn;" "\xE2\x88\x98"
   "&complement;" "\xE2\x88\x81"
   "&complexes;" "\xE2\x84\x82"
   "&cong;" "\xE2\x89\x85"
   "&congdot;" "\xE2\xA9\xAD"
   "&conint;" "\xE2\x88\xAE"
   "&copf;" "\xF0\x9D\x95\x94"
   "&coprod;" "\xE2\x88\x90"
   "&copy;" "\xC2\xA9"
   "&copysr;" "\xE2\x84\x97"
   "&crarr;" "\xE2\x86\xB5"
   "&cross;" "\xE2\x9C\x97"
   "&cscr;" "\xF0\x9D\x92\xB8"
   "&csub;" "\xE2\xAB\x8F"
   "&csube;" "\xE2\xAB\x91"
   "&csup;" "\xE2\xAB\x90"
   "&csupe;" "\xE2\xAB\x92"
   "&ctdot;" "\xE2\x8B\xAF"
   "&cudarrl;" "\xE2\xA4\xB8"
   "&cudarrr;" "\xE2\xA4\xB5"
   "&cuepr;" "\xE2\x8B\x9E"
   "&cuesc;" "\xE2\x8B\x9F"
   "&cularr;" "\xE2\x86\xB6"
   "&cularrp;" "\xE2\xA4\xBD"
   "&cup;" "\xE2\x88\xAA"
   "&cupbrcap;" "\xE2\xA9\x88"
   "&cupcap;" "\xE2\xA9\x86"
   "&cupcup;" "\xE2\xA9\x8A"
   "&cupdot;" "\xE2\x8A\x8D"
   "&cupor;" "\xE2\xA9\x85"
   "&cups;" "\xE2\x88\xAA\xEF\xB8\x80"
   "&curarr;" "\xE2\x86\xB7"
   "&curarrm;" "\xE2\xA4\xBC"
   "&curlyeqprec;" "\xE2\x8B\x9E"
   "&curlyeqsucc;" "\xE2\x8B\x9F"
   "&curlyvee;" "\xE2\x8B\x8E"
   "&curlywedge;" "\xE2\x8B\x8F"
   "&curren;" "\xC2\xA4"
   "&curvearrowleft;" "\xE2\x86\xB6"
   "&curvearrowright;" "\xE2\x86\xB7"
   "&cuvee;" "\xE2\x8B\x8E"
   "&cuwed;" "\xE2\x8B\x8F"
   "&cwconint;" "\xE2\x88\xB2"
   "&cwint;" "\xE2\x88\xB1"
   "&cylcty;" "\xE2\x8C\xAD"
   "&dArr;" "\xE2\x87\x93"
   "&dHar;" "\xE2\xA5\xA5"
   "&dagger;" "\xE2\x80\xA0"
   "&daleth;" "\xE2\x84\xB8"
   "&darr;" "\xE2\x86\x93"
   "&dash;" "\xE2\x80\x90"
   "&dashv;" "\xE2\x8A\xA3"
   "&dbkarow;" "\xE2\xA4\x8F"
   "&dblac;" "\xCB\x9D"
   "&dcaron;" "\xC4\x8F"
   "&dcy;" "\xD0\xB4"
   "&dd;" "\xE2\x85\x86"
   "&ddagger;" "\xE2\x80\xA1"
   "&ddarr;" "\xE2\x87\x8A"
   "&ddotseq;" "\xE2\xA9\xB7"
   "&deg;" "\xC2\xB0"
   "&delta;" "\xCE\xB4"
   "&demptyv;" "\xE2\xA6\xB1"
   "&dfisht;" "\xE2\xA5\xBF"
   "&dfr;" "\xF0\x9D\x94\xA1"
   "&dharl;" "\xE2\x87\x83"
   "&dharr;" "\xE2\x87\x82"
   "&diam;" "\xE2\x8B\x84"
   "&diamond;" "\xE2\x8B\x84"
   "&diamondsuit;" "\xE2\x99\xA6"
   "&diams;" "\xE2\x99\xA6"
   "&die;" "\xC2\xA8"
   "&digamma;" "\xCF\x9D"
   "&disin;" "\xE2\x8B\xB2"
   "&div;" "\xC3\xB7"
   "&divide;" "\xC3\xB7"
   "&divideontimes;" "\xE2\x8B\x87"
   "&divonx;" "\xE2\x8B\x87"
   "&djcy;" "\xD1\x92"
   "&dlcorn;" "\xE2\x8C\x9E"
   "&dlcrop;" "\xE2\x8C\x8D"
   "&dollar;" "$"
   "&dopf;" "\xF0\x9D\x95\x95"
   "&dot;" "\xCB\x99"
   "&doteq;" "\xE2\x89\x90"
   "&doteqdot;" "\xE2\x89\x91"
   "&dotminus;" "\xE2\x88\xB8"
   "&dotplus;" "\xE2\x88\x94"
   "&dotsquare;" "\xE2\x8A\xA1"
   "&doublebarwedge;" "\xE2\x8C\x86"
   "&downarrow;" "\xE2\x86\x93"
   "&downdownarrows;" "\xE2\x87\x8A"
   "&downharpoonleft;" "\xE2\x87\x83"
   "&downharpoonright;" "\xE2\x87\x82"
   "&drbkarow;" "\xE2\xA4\x90"
   "&drcorn;" "\xE2\x8C\x9F"
   "&drcrop;" "\xE2\x8C\x8C"
   "&dscr;" "\xF0\x9D\x92\xB9"
   "&dscy;" "\xD1\x95"
   "&dsol;" "\xE2\xA7\xB6"
   "&dstrok;" "\xC4\x91"
   "&dtdot;" "\xE2\x8B\xB1"
   "&dtri;" "\xE2\x96\xBF"
   "&dtrif;" "\xE2\x96\xBE"
   "&duarr;" "\xE2\x87\xB5"
   "&duhar;" "\xE2\xA5\xAF"
   "&dwangle;" "\xE2\xA6\xA6"
   "&dzcy;" "\xD1\x9F"
   "&dzigrarr;" "\xE2\x9F\xBF"
   "&eDDot;" "\xE2\xA9\xB7"
   "&eDot;" "\xE2\x89\x91"
   "&eacute;" "\xC3\xA9"
   "&easter;" "\xE2\xA9\xAE"
   "&ecaron;" "\xC4\x9B"
   "&ecir;" "\xE2\x89\x96"
   "&ecirc;" "\xC3\xAA"
   "&ecolon;" "\xE2\x89\x95"
   "&ecy;" "\xD1\x8D"
   "&edot;" "\xC4\x97"
   "&ee;" "\xE2\x85\x87"
   "&efDot;" "\xE2\x89\x92"
   "&efr;" "\xF0\x9D\x94\xA2"
   "&eg;" "\xE2\xAA\x9A"
   "&egrave;" "\xC3\xA8"
   "&egs;" "\xE2\xAA\x96"
   "&egsdot;" "\xE2\xAA\x98"
   "&el;" "\xE2\xAA\x99"
   "&elinters;" "\xE2\x8F\xA7"
   "&ell;" "\xE2\x84\x93"
   "&els;" "\xE2\xAA\x95"
   "&elsdot;" "\xE2\xAA\x97"
   "&emacr;" "\xC4\x93"
   "&empty;" "\xE2\x88\x85"
   "&emptyset;" "\xE2\x88\x85"
   "&emptyv;" "\xE2\x88\x85"
   "&emsp13;" "\xE2\x80\x84"
   "&emsp14;" "\xE2\x80\x85"
   "&emsp;" "\xE2\x80\x83"
   "&eng;" "\xC5\x8B"
   "&ensp;" "\xE2\x80\x82"
   "&eogon;" "\xC4\x99"
   "&eopf;" "\xF0\x9D\x95\x96"
   "&epar;" "\xE2\x8B\x95"
   "&eparsl;" "\xE2\xA7\xA3"
   "&eplus;" "\xE2\xA9\xB1"
   "&epsi;" "\xCE\xB5"
   "&epsilon;" "\xCE\xB5"
   "&epsiv;" "\xCF\xB5"
   "&eqcirc;" "\xE2\x89\x96"
   "&eqcolon;" "\xE2\x89\x95"
   "&eqsim;" "\xE2\x89\x82"
   "&eqslantgtr;" "\xE2\xAA\x96"
   "&eqslantless;" "\xE2\xAA\x95"
   "&equals;" "="
   "&equest;" "\xE2\x89\x9F"
   "&equiv;" "\xE2\x89\xA1"
   "&equivDD;" "\xE2\xA9\xB8"
   "&eqvparsl;" "\xE2\xA7\xA5"
   "&erDot;" "\xE2\x89\x93"
   "&erarr;" "\xE2\xA5\xB1"
   "&escr;" "\xE2\x84\xAF"
   "&esdot;" "\xE2\x89\x90"
   "&esim;" "\xE2\x89\x82"
   "&eta;" "\xCE\xB7"
   "&eth;" "\xC3\xB0"
   "&euml;" "\xC3\xAB"
   "&euro;" "\xE2\x82\xAC"
   "&excl;" "!"
   "&exist;" "\xE2\x88\x83"
   "&expectation;" "\xE2\x84\xB0"
   "&exponentiale;" "\xE2\x85\x87"
   "&fallingdotseq;" "\xE2\x89\x92"
   "&fcy;" "\xD1\x84"
   "&female;" "\xE2\x99\x80"
   "&ffilig;" "\xEF\xAC\x83"
   "&fflig;" "\xEF\xAC\x80"
   "&ffllig;" "\xEF\xAC\x84"
   "&ffr;" "\xF0\x9D\x94\xA3"
   "&filig;" "\xEF\xAC\x81"
   "&fjlig;" "fj"
   "&flat;" "\xE2\x99\xAD"
   "&fllig;" "\xEF\xAC\x82"
   "&fltns;" "\xE2\x96\xB1"
   "&fnof;" "\xC6\x92"
   "&fopf;" "\xF0\x9D\x95\x97"
   "&forall;" "\xE2\x88\x80"
   "&fork;" "\xE2\x8B\x94"
   "&forkv;" "\xE2\xAB\x99"
   "&fpartint;" "\xE2\xA8\x8D"
   "&frac12;" "\xC2\xBD"
   "&frac13;" "\xE2\x85\x93"
   "&frac14;" "\xC2\xBC"
   "&frac15;" "\xE2\x85\x95"
   "&frac16;" "\xE2\x85\x99"
   "&frac18;" "\xE2\x85\x9B"
   "&frac23;" "\xE2\x85\x94"
   "&frac25;" "\xE2\x85\x96"
   "&frac34;" "\xC2\xBE"
   "&frac35;" "\xE2\x85\x97"
   "&frac38;" "\xE2\x85\x9C"
   "&frac45;" "\xE2\x85\x98"
   "&frac56;" "\xE2\x85\x9A"
   "&frac58;" "\xE2\x85\x9D"
   "&frac78;" "\xE2\x85\x9E"
   "&frasl;" "\xE2\x81\x84"
   "&frown;" "\xE2\x8C\xA2"
   "&fscr;" "\xF0\x9D\x92\xBB"
   "&gE;" "\xE2\x89\xA7"
   "&gEl;" "\xE2\xAA\x8C"
   "&gacute;" "\xC7\xB5"
   "&gamma;" "\xCE\xB3"
   "&gammad;" "\xCF\x9D"
   "&gap;" "\xE2\xAA\x86"
   "&gbreve;" "\xC4\x9F"
   "&gcirc;" "\xC4\x9D"
   "&gcy;" "\xD0\xB3"
   "&gdot;" "\xC4\xA1"
   "&ge;" "\xE2\x89\xA5"
   "&gel;" "\xE2\x8B\x9B"
   "&geq;" "\xE2\x89\xA5"
   "&geqq;" "\xE2\x89\xA7"
   "&geqslant;" "\xE2\xA9\xBE"
   "&ges;" "\xE2\xA9\xBE"
   "&gescc;" "\xE2\xAA\xA9"
   "&gesdot;" "\xE2\xAA\x80"
   "&gesdoto;" "\xE2\xAA\x82"
   "&gesdotol;" "\xE2\xAA\x84"
   "&gesl;" "\xE2\x8B\x9B\xEF\xB8\x80"
   "&gesles;" "\xE2\xAA\x94"
   "&gfr;" "\xF0\x9D\x94\xA4"
   "&gg;" "\xE2\x89\xAB"
   "&ggg;" "\xE2\x8B\x99"
   "&gimel;" "\xE2\x84\xB7"
   "&gjcy;" "\xD1\x93"
   "&gl;" "\xE2\x89\xB7"
   "&glE;" "\xE2\xAA\x92"
   "&gla;" "\xE2\xAA\xA5"
   "&glj;" "\xE2\xAA\xA4"
   "&gnE;" "\xE2\x89\xA9"
   "&gnap;" "\xE2\xAA\x8A"
   "&gnapprox;" "\xE2\xAA\x8A"
   "&gne;" "\xE2\xAA\x88"
   "&gneq;" "\xE2\xAA\x88"
   "&gneqq;" "\xE2\x89\xA9"
   "&gnsim;" "\xE2\x8B\xA7"
   "&gopf;" "\xF0\x9D\x95\x98"
   "&grave;" "`"
   "&gscr;" "\xE2\x84\x8A"
   "&gsim;" "\xE2\x89\xB3"
   "&gsime;" "\xE2\xAA\x8E"
   "&gsiml;" "\xE2\xAA\x90"
   "&gt;" ">"
   "&gtcc;" "\xE2\xAA\xA7"
   "&gtcir;" "\xE2\xA9\xBA"
   "&gtdot;" "\xE2\x8B\x97"
   "&gtlPar;" "\xE2\xA6\x95"
   "&gtquest;" "\xE2\xA9\xBC"
   "&gtrapprox;" "\xE2\xAA\x86"
   "&gtrarr;" "\xE2\xA5\xB8"
   "&gtrdot;" "\xE2\x8B\x97"
   "&gtreqless;" "\xE2\x8B\x9B"
   "&gtreqqless;" "\xE2\xAA\x8C"
   "&gtrless;" "\xE2\x89\xB7"
   "&gtrsim;" "\xE2\x89\xB3"
   "&gvertneqq;" "\xE2\x89\xA9\xEF\xB8\x80"
   "&gvnE;" "\xE2\x89\xA9\xEF\xB8\x80"
   "&hArr;" "\xE2\x87\x94"
   "&hairsp;" "\xE2\x80\x8A"
   "&half;" "\xC2\xBD"
   "&hamilt;" "\xE2\x84\x8B"
   "&hardcy;" "\xD1\x8A"
   "&harr;" "\xE2\x86\x94"
   "&harrcir;" "\xE2\xA5\x88"
   "&harrw;" "\xE2\x86\xAD"
   "&hbar;" "\xE2\x84\x8F"
   "&hcirc;" "\xC4\xA5"
   "&hearts;" "\xE2\x99\xA5"
   "&heartsuit;" "\xE2\x99\xA5"
   "&hellip;" "\xE2\x80\xA6"
   "&hercon;" "\xE2\x8A\xB9"
   "&hfr;" "\xF0\x9D\x94\xA5"
   "&hksearow;" "\xE2\xA4\xA5"
   "&hkswarow;" "\xE2\xA4\xA6"
   "&hoarr;" "\xE2\x87\xBF"
   "&homtht;" "\xE2\x88\xBB"
   "&hookleftarrow;" "\xE2\x86\xA9"
   "&hookrightarrow;" "\xE2\x86\xAA"
   "&hopf;" "\xF0\x9D\x95\x99"
   "&horbar;" "\xE2\x80\x95"
   "&hscr;" "\xF0\x9D\x92\xBD"
   "&hslash;" "\xE2\x84\x8F"
   "&hstrok;" "\xC4\xA7"
   "&hybull;" "\xE2\x81\x83"
   "&hyphen;" "\xE2\x80\x90"
   "&iacute;" "\xC3\xAD"
   "&ic;" "\xE2\x81\xA3"
   "&icirc;" "\xC3\xAE"
   "&icy;" "\xD0\xB8"
   "&iecy;" "\xD0\xB5"
   "&iexcl;" "\xC2\xA1"
   "&iff;" "\xE2\x87\x94"
   "&ifr;" "\xF0\x9D\x94\xA6"
   "&igrave;" "\xC3\xAC"
   "&ii;" "\xE2\x85\x88"
   "&iiiint;" "\xE2\xA8\x8C"
   "&iiint;" "\xE2\x88\xAD"
   "&iinfin;" "\xE2\xA7\x9C"
   "&iiota;" "\xE2\x84\xA9"
   "&ijlig;" "\xC4\xB3"
   "&imacr;" "\xC4\xAB"
   "&image;" "\xE2\x84\x91"
   "&imagline;" "\xE2\x84\x90"
   "&imagpart;" "\xE2\x84\x91"
   "&imath;" "\xC4\xB1"
   "&imof;" "\xE2\x8A\xB7"
   "&imped;" "\xC6\xB5"
   "&in;" "\xE2\x88\x88"
   "&incare;" "\xE2\x84\x85"
   "&infin;" "\xE2\x88\x9E"
   "&infintie;" "\xE2\xA7\x9D"
   "&inodot;" "\xC4\xB1"
   "&int;" "\xE2\x88\xAB"
   "&intcal;" "\xE2\x8A\xBA"
   "&integers;" "\xE2\x84\xA4"
   "&intercal;" "\xE2\x8A\xBA"
   "&intlarhk;" "\xE2\xA8\x97"
   "&intprod;" "\xE2\xA8\xBC"
   "&iocy;" "\xD1\x91"
   "&iogon;" "\xC4\xAF"
   "&iopf;" "\xF0\x9D\x95\x9A"
   "&iota;" "\xCE\xB9"
   "&iprod;" "\xE2\xA8\xBC"
   "&iquest;" "\xC2\xBF"
   "&iscr;" "\xF0\x9D\x92\xBE"
   "&isin;" "\xE2\x88\x88"
   "&isinE;" "\xE2\x8B\xB9"
   "&isindot;" "\xE2\x8B\xB5"
   "&isins;" "\xE2\x8B\xB4"
   "&isinsv;" "\xE2\x8B\xB3"
   "&isinv;" "\xE2\x88\x88"
   "&it;" "\xE2\x81\xA2"
   "&itilde;" "\xC4\xA9"
   "&iukcy;" "\xD1\x96"
   "&iuml;" "\xC3\xAF"
   "&jcirc;" "\xC4\xB5"
   "&jcy;" "\xD0\xB9"
   "&jfr;" "\xF0\x9D\x94\xA7"
   "&jmath;" "\xC8\xB7"
   "&jopf;" "\xF0\x9D\x95\x9B"
   "&jscr;" "\xF0\x9D\x92\xBF"
   "&jsercy;" "\xD1\x98"
   "&jukcy;" "\xD1\x94"
   "&kappa;" "\xCE\xBA"
   "&kappav;" "\xCF\xB0"
   "&kcedil;" "\xC4\xB7"
   "&kcy;" "\xD0\xBA"
   "&kfr;" "\xF0\x9D\x94\xA8"
   "&kgreen;" "\xC4\xB8"
   "&khcy;" "\xD1\x85"
   "&kjcy;" "\xD1\x9C"
   "&kopf;" "\xF0\x9D\x95\x9C"
   "&kscr;" "\xF0\x9D\x93\x80"
   "&lAarr;" "\xE2\x87\x9A"
   "&lArr;" "\xE2\x87\x90"
   "&lAtail;" "\xE2\xA4\x9B"
   "&lBarr;" "\xE2\xA4\x8E"
   "&lE;" "\xE2\x89\xA6"
   "&lEg;" "\xE2\xAA\x8B"
   "&lHar;" "\xE2\xA5\xA2"
   "&lacute;" "\xC4\xBA"
   "&laemptyv;" "\xE2\xA6\xB4"
   "&lagran;" "\xE2\x84\x92"
   "&lambda;" "\xCE\xBB"
   "&lang;" "\xE2\x9F\xA8"
   "&langd;" "\xE2\xA6\x91"
   "&langle;" "\xE2\x9F\xA8"
   "&lap;" "\xE2\xAA\x85"
   "&laquo;" "\xC2\xAB"
   "&larr;" "\xE2\x86\x90"
   "&larrb;" "\xE2\x87\xA4"
   "&larrbfs;" "\xE2\xA4\x9F"
   "&larrfs;" "\xE2\xA4\x9D"
   "&larrhk;" "\xE2\x86\xA9"
   "&larrlp;" "\xE2\x86\xAB"
   "&larrpl;" "\xE2\xA4\xB9"
   "&larrsim;" "\xE2\xA5\xB3"
   "&larrtl;" "\xE2\x86\xA2"
   "&lat;" "\xE2\xAA\xAB"
   "&latail;" "\xE2\xA4\x99"
   "&late;" "\xE2\xAA\xAD"
   "&lates;" "\xE2\xAA\xAD\xEF\xB8\x80"
   "&lbarr;" "\xE2\xA4\x8C"
   "&lbbrk;" "\xE2\x9D\xB2"
   "&lbrace;" "{"
   "&lbrack;" "["
   "&lbrke;" "\xE2\xA6\x8B"
   "&lbrksld;" "\xE2\xA6\x8F"
   "&lbrkslu;" "\xE2\xA6\x8D"
   "&lcaron;" "\xC4\xBE"
   "&lcedil;" "\xC4\xBC"
   "&lceil;" "\xE2\x8C\x88"
   "&lcub;" "{"
   "&lcy;" "\xD0\xBB"
   "&ldca;" "\xE2\xA4\xB6"
   "&ldquo;" "\xE2\x80\x9C"
   "&ldquor;" "\xE2\x80\x9E"
   "&ldrdhar;" "\xE2\xA5\xA7"
   "&ldrushar;" "\xE2\xA5\x8B"
   "&ldsh;" "\xE2\x86\xB2"
   "&le;" "\xE2\x89\xA4"
   "&leftarrow;" "\xE2\x86\x90"
   "&leftarrowtail;" "\xE2\x86\xA2"
   "&leftharpoondown;" "\xE2\x86\xBD"
   "&leftharpoonup;" "\xE2\x86\xBC"
   "&leftleftarrows;" "\xE2\x87\x87"
   "&leftrightarrow;" "\xE2\x86\x94"
   "&leftrightarrows;" "\xE2\x87\x86"
   "&leftrightharpoons;" "\xE2\x87\x8B"
   "&leftrightsquigarrow;" "\xE2\x86\xAD"
   "&leftthreetimes;" "\xE2\x8B\x8B"
   "&leg;" "\xE2\x8B\x9A"
   "&leq;" "\xE2\x89\xA4"
   "&leqq;" "\xE2\x89\xA6"
   "&leqslant;" "\xE2\xA9\xBD"
   "&les;" "\xE2\xA9\xBD"
   "&lescc;" "\xE2\xAA\xA8"
   "&lesdot;" "\xE2\xA9\xBF"
   "&lesdoto;" "\xE2\xAA\x81"
   "&lesdotor;" "\xE2\xAA\x83"
   "&lesg;" "\xE2\x8B\x9A\xEF\xB8\x80"
   "&lesges;" "\xE2\xAA\x93"
   "&lessapprox;" "\xE2\xAA\x85"
   "&lessdot;" "\xE2\x8B\x96"
   "&lesseqgtr;" "\xE2\x8B\x9A"
   "&lesseqqgtr;" "\xE2\xAA\x8B"
   "&lessgtr;" "\xE2\x89\xB6"
   "&lesssim;" "\xE2\x89\xB2"
   "&lfisht;" "\xE2\xA5\xBC"
   "&lfloor;" "\xE2\x8C\x8A"
   "&lfr;" "\xF0\x9D\x94\xA9"
   "&lg;" "\xE2\x89\xB6"
   "&lgE;" "\xE2\xAA\x91"
   "&lhard;" "\xE2\x86\xBD"
   "&lharu;" "\xE2\x86\xBC"
   "&lharul;" "\xE2\xA5\xAA"
   "&lhblk;" "\xE2\x96\x84"
   "&ljcy;" "\xD1\x99"
   "&ll;" "\xE2\x89\xAA"
   "&llarr;" "\xE2\x87\x87"
   "&llcorner;" "\xE2\x8C\x9E"
   "&llhard;" "\xE2\xA5\xAB"
   "&lltri;" "\xE2\x97\xBA"
   "&lmidot;" "\xC5\x80"
   "&lmoust;" "\xE2\x8E\xB0"
   "&lmoustache;" "\xE2\x8E\xB0"
   "&lnE;" "\xE2\x89\xA8"
   "&lnap;" "\xE2\xAA\x89"
   "&lnapprox;" "\xE2\xAA\x89"
   "&lne;" "\xE2\xAA\x87"
   "&lneq;" "\xE2\xAA\x87"
   "&lneqq;" "\xE2\x89\xA8"
   "&lnsim;" "\xE2\x8B\xA6"
   "&loang;" "\xE2\x9F\xAC"
   "&loarr;" "\xE2\x87\xBD"
   "&lobrk;" "\xE2\x9F\xA6"
   "&longleftarrow;" "\xE2\x9F\xB5"
   "&longleftrightarrow;" "\xE2\x9F\xB7"
   "&longmapsto;" "\xE2\x9F\xBC"
   "&longrightarrow;" "\xE2\x9F\xB6"
   "&looparrowleft;" "\xE2\x86\xAB"
   "&looparrowright;" "\xE2\x86\xAC"
   "&lopar;" "\xE2\xA6\x85"
   "&lopf;" "\xF0\x9D\x95\x9D"
   "&loplus;" "\xE2\xA8\xAD"
   "&lotimes;" "\xE2\xA8\xB4"
   "&lowast;" "\xE2\x88\x97"
   "&lowbar;" "_"
   "&loz;" "\xE2\x97\x8A"
   "&lozenge;" "\xE2\x97\x8A"
   "&lozf;" "\xE2\xA7\xAB"
   "&lpar;" "("
   "&lparlt;" "\xE2\xA6\x93"
   "&lrarr;" "\xE2\x87\x86"
   "&lrcorner;" "\xE2\x8C\x9F"
   "&lrhar;" "\xE2\x87\x8B"
   "&lrhard;" "\xE2\xA5\xAD"
   "&lrm;" "\xE2\x80\x8E"
   "&lrtri;" "\xE2\x8A\xBF"
   "&lsaquo;" "\xE2\x80\xB9"
   "&lscr;" "\xF0\x9D\x93\x81"
   "&lsh;" "\xE2\x86\xB0"
   "&lsim;" "\xE2\x89\xB2"
   "&lsime;" "\xE2\xAA\x8D"
   "&lsimg;" "\xE2\xAA\x8F"
   "&lsqb;" "["
   "&lsquo;" "\xE2\x80\x98"
   "&lsquor;" "\xE2\x80\x9A"
   "&lstrok;" "\xC5\x82"
   "&lt;" "<"
   "&ltcc;" "\xE2\xAA\xA6"
   "&ltcir;" "\xE2\xA9\xB9"
   "&ltdot;" "\xE2\x8B\x96"
   "&lthree;" "\xE2\x8B\x8B"
   "&ltimes;" "\xE2\x8B\x89"
   "&ltlarr;" "\xE2\xA5\xB6"
   "&ltquest;" "\xE2\xA9\xBB"
   "&ltrPar;" "\xE2\xA6\x96"
   "&ltri;" "\xE2\x97\x83"
   "&ltrie;" "\xE2\x8A\xB4"
   "&ltrif;" "\xE2\x97\x82"
   "&lurdshar;" "\xE2\xA5\x8A"
   "&luruhar;" "\xE2\xA5\xA6"
   "&lvertneqq;" "\xE2\x89\xA8\xEF\xB8\x80"
   "&lvnE;" "\xE2\x89\xA8\xEF\xB8\x80"
   "&mDDot;" "\xE2\x88\xBA"
   "&macr;" "\xC2\xAF"
   "&male;" "\xE2\x99\x82"
   "&malt;" "\xE2\x9C\xA0"
   "&maltese;" "\xE2\x9C\xA0"
   "&map;" "\xE2\x86\xA6"
   "&mapsto;" "\xE2\x86\xA6"
   "&mapstodown;" "\xE2\x86\xA7"
   "&mapstoleft;" "\xE2\x86\xA4"
   "&mapstoup;" "\xE2\x86\xA5"
   "&marker;" "\xE2\x96\xAE"
   "&mcomma;" "\xE2\xA8\xA9"
   "&mcy;" "\xD0\xBC"
   "&mdash;" "\xE2\x80\x94"
   "&measuredangle;" "\xE2\x88\xA1"
   "&mfr;" "\xF0\x9D\x94\xAA"
   "&mho;" "\xE2\x84\xA7"
   "&micro;" "\xC2\xB5"
   "&mid;" "\xE2\x88\xA3"
   "&midast;" "*"
   "&midcir;" "\xE2\xAB\xB0"
   "&middot;" "\xC2\xB7"
   "&minus;" "\xE2\x88\x92"
   "&minusb;" "\xE2\x8A\x9F"
   "&minusd;" "\xE2\x88\xB8"
   "&minusdu;" "\xE2\xA8\xAA"
   "&mlcp;" "\xE2\xAB\x9B"
   "&mldr;" "\xE2\x80\xA6"
   "&mnplus;" "\xE2\x88\x93"
   "&models;" "\xE2\x8A\xA7"
   "&mopf;" "\xF0\x9D\x95\x9E"
   "&mp;" "\xE2\x88\x93"
   "&mscr;" "\xF0\x9D\x93\x82"
   "&mstpos;" "\xE2\x88\xBE"
   "&mu;" "\xCE\xBC"
   "&multimap;" "\xE2\x8A\xB8"
   "&mumap;" "\xE2\x8A\xB8"
   "&nGg;" "\xE2\x8B\x99\xCC\xB8"
   "&nGt;" "\xE2\x89\xAB\xE2\x83\x92"
   "&nGtv;" "\xE2\x89\xAB\xCC\xB8"
   "&nLeftarrow;" "\xE2\x87\x8D"
   "&nLeftrightarrow;" "\xE2\x87\x8E"
   "&nLl;" "\xE2\x8B\x98\xCC\xB8"
   "&nLt;" "\xE2\x89\xAA\xE2\x83\x92"
   "&nLtv;" "\xE2\x89\xAA\xCC\xB8"
   "&nRightarrow;" "\xE2\x87\x8F"
   "&nVDash;" "\xE2\x8A\xAF"
   "&nVdash;" "\xE2\x8A\xAE"
   "&nabla;" "\xE2\x88\x87"
   "&nacute;" "\xC5\x84"
   "&nang;" "\xE2\x88\xA0\xE2\x83\x92"
   "&nap;" "\xE2\x89\x89"
   "&napE;" "\xE2\xA9\xB0\xCC\xB8"
   "&napid;" "\xE2\x89\x8B\xCC\xB8"
   "&napos;" "\xC5\x89"
   "&napprox;" "\xE2\x89\x89"
   "&natur;" "\xE2\x99\xAE"
   "&natural;" "\xE2\x99\xAE"
   "&naturals;" "\xE2\x84\x95"
   "&nbsp;" "\xC2\xA0"
   "&nbump;" "\xE2\x89\x8E\xCC\xB8"
   "&nbumpe;" "\xE2\x89\x8F\xCC\xB8"
   "&ncap;" "\xE2\xA9\x83"
   "&ncaron;" "\xC5\x88"
   "&ncedil;" "\xC5\x86"
   "&ncong;" "\xE2\x89\x87"
   "&ncongdot;" "\xE2\xA9\xAD\xCC\xB8"
   "&ncup;" "\xE2\xA9\x82"
   "&ncy;" "\xD0\xBD"
   "&ndash;" "\xE2\x80\x93"
   "&ne;" "\xE2\x89\xA0"
   "&neArr;" "\xE2\x87\x97"
   "&nearhk;" "\xE2\xA4\xA4"
   "&nearr;" "\xE2\x86\x97"
   "&nearrow;" "\xE2\x86\x97"
   "&nedot;" "\xE2\x89\x90\xCC\xB8"
   "&nequiv;" "\xE2\x89\xA2"
   "&nesear;" "\xE2\xA4\xA8"
   "&nesim;" "\xE2\x89\x82\xCC\xB8"
   "&nexist;" "\xE2\x88\x84"
   "&nexists;" "\xE2\x88\x84"
   "&nfr;" "\xF0\x9D\x94\xAB"
   "&ngE;" "\xE2\x89\xA7\xCC\xB8"
   "&nge;" "\xE2\x89\xB1"
   "&ngeq;" "\xE2\x89\xB1"
   "&ngeqq;" "\xE2\x89\xA7\xCC\xB8"
   "&ngeqslant;" "\xE2\xA9\xBE\xCC\xB8"
   "&nges;" "\xE2\xA9\xBE\xCC\xB8"
   "&ngsim;" "\xE2\x89\xB5"
   "&ngt;" "\xE2\x89\xAF"
   "&ngtr;" "\xE2\x89\xAF"
   "&nhArr;" "\xE2\x87\x8E"
   "&nharr;" "\xE2\x86\xAE"
   "&nhpar;" "\xE2\xAB\xB2"
   "&ni;" "\xE2\x88\x8B"
   "&nis;" "\xE2\x8B\xBC"
   "&nisd;" "\xE2\x8B\xBA"
   "&niv;" "\xE2\x88\x8B"
   "&njcy;" "\xD1\x9A"
   "&nlArr;" "\xE2\x87\x8D"
   "&nlE;" "\xE2\x89\xA6\xCC\xB8"
   "&nlarr;" "\xE2\x86\x9A"
   "&nldr;" "\xE2\x80\xA5"
   "&nle;" "\xE2\x89\xB0"
   "&nleftarrow;" "\xE2\x86\x9A"
   "&nleftrightarrow;" "\xE2\x86\xAE"
   "&nleq;" "\xE2\x89\xB0"
   "&nleqq;" "\xE2\x89\xA6\xCC\xB8"
   "&nleqslant;" "\xE2\xA9\xBD\xCC\xB8"
   "&nles;" "\xE2\xA9\xBD\xCC\xB8"
   "&nless;" "\xE2\x89\xAE"
   "&nlsim;" "\xE2\x89\xB4"
   "&nlt;" "\xE2\x89\xAE"
   "&nltri;" "\xE2\x8B\xAA"
   "&nltrie;" "\xE2\x8B\xAC"
   "&nmid;" "\xE2\x88\xA4"
   "&nopf;" "\xF0\x9D\x95\x9F"
   "&not;" "\xC2\xAC"
   "&notin;" "\xE2\x88\x89"
   "&notinE;" "\xE2\x8B\xB9\xCC\xB8"
   "&notindot;" "\xE2\x8B\xB5\xCC\xB8"
   "&notinva;" "\xE2\x88\x89"
   "&notinvb;" "\xE2\x8B\xB7"
   "&notinvc;" "\xE2\x8B\xB6"
   "&notni;" "\xE2\x88\x8C"
   "&notniva;" "\xE2\x88\x8C"
   "&notnivb;" "\xE2\x8B\xBE"
   "&notnivc;" "\xE2\x8B\xBD"
   "&npar;" "\xE2\x88\xA6"
   "&nparallel;" "\xE2\x88\xA6"
   "&nparsl;" "\xE2\xAB\xBD\xE2\x83\xA5"
   "&npart;" "\xE2\x88\x82\xCC\xB8"
   "&npolint;" "\xE2\xA8\x94"
   "&npr;" "\xE2\x8A\x80"
   "&nprcue;" "\xE2\x8B\xA0"
   "&npre;" "\xE2\xAA\xAF\xCC\xB8"
   "&nprec;" "\xE2\x8A\x80"
   "&npreceq;" "\xE2\xAA\xAF\xCC\xB8"
   "&nrArr;" "\xE2\x87\x8F"
   "&nrarr;" "\xE2\x86\x9B"
   "&nrarrc;" "\xE2\xA4\xB3\xCC\xB8"
   "&nrarrw;" "\xE2\x86\x9D\xCC\xB8"
   "&nrightarrow;" "\xE2\x86\x9B"
   "&nrtri;" "\xE2\x8B\xAB"
   "&nrtrie;" "\xE2\x8B\xAD"
   "&nsc;" "\xE2\x8A\x81"
   "&nsccue;" "\xE2\x8B\xA1"
   "&nsce;" "\xE2\xAA\xB0\xCC\xB8"
   "&nscr;" "\xF0\x9D\x93\x83"
   "&nshortmid;" "\xE2\x88\xA4"
   "&nshortparallel;" "\xE2\x88\xA6"
   "&nsim;" "\xE2\x89\x81"
   "&nsime;" "\xE2\x89\x84"
   "&nsimeq;" "\xE2\x89\x84"
   "&nsmid;" "\xE2\x88\xA4"
   "&nspar;" "\xE2\x88\xA6"
   "&nsqsube;" "\xE2\x8B\xA2"
   "&nsqsupe;" "\xE2\x8B\xA3"
   "&nsub;" "\xE2\x8A\x84"
   "&nsubE;" "\xE2\xAB\x85\xCC\xB8"
   "&nsube;" "\xE2\x8A\x88"
   "&nsubset;" "\xE2\x8A\x82\xE2\x83\x92"
   "&nsubseteq;" "\xE2\x8A\x88"
   "&nsubseteqq;" "\xE2\xAB\x85\xCC\xB8"
   "&nsucc;" "\xE2\x8A\x81"
   "&nsucceq;" "\xE2\xAA\xB0\xCC\xB8"
   "&nsup;" "\xE2\x8A\x85"
   "&nsupE;" "\xE2\xAB\x86\xCC\xB8"
   "&nsupe;" "\xE2\x8A\x89"
   "&nsupset;" "\xE2\x8A\x83\xE2\x83\x92"
   "&nsupseteq;" "\xE2\x8A\x89"
   "&nsupseteqq;" "\xE2\xAB\x86\xCC\xB8"
   "&ntgl;" "\xE2\x89\xB9"
   "&ntilde;" "\xC3\xB1"
   "&ntlg;" "\xE2\x89\xB8"
   "&ntriangleleft;" "\xE2\x8B\xAA"
   "&ntrianglelefteq;" "\xE2\x8B\xAC"
   "&ntriangleright;" "\xE2\x8B\xAB"
   "&ntrianglerighteq;" "\xE2\x8B\xAD"
   "&nu;" "\xCE\xBD"
   "&num;" "#"
   "&numero;" "\xE2\x84\x96"
   "&numsp;" "\xE2\x80\x87"
   "&nvDash;" "\xE2\x8A\xAD"
   "&nvHarr;" "\xE2\xA4\x84"
   "&nvap;" "\xE2\x89\x8D\xE2\x83\x92"
   "&nvdash;" "\xE2\x8A\xAC"
   "&nvge;" "\xE2\x89\xA5\xE2\x83\x92"
   "&nvgt;" ">\xE2\x83\x92"
   "&nvinfin;" "\xE2\xA7\x9E"
   "&nvlArr;" "\xE2\xA4\x82"
   "&nvle;" "\xE2\x89\xA4\xE2\x83\x92"
   "&nvlt;" "<\xE2\x83\x92"
   "&nvltrie;" "\xE2\x8A\xB4\xE2\x83\x92"
   "&nvrArr;" "\xE2\xA4\x83"
   "&nvrtrie;" "\xE2\x8A\xB5\xE2\x83\x92"
   "&nvsim;" "\xE2\x88\xBC\xE2\x83\x92"
   "&nwArr;" "\xE2\x87\x96"
   "&nwarhk;" "\xE2\xA4\xA3"
   "&nwarr;" "\xE2\x86\x96"
   "&nwarrow;" "\xE2\x86\x96"
   "&nwnear;" "\xE2\xA4\xA7"
   "&oS;" "\xE2\x93\x88"
   "&oacute;" "\xC3\xB3"
   "&oast;" "\xE2\x8A\x9B"
   "&ocir;" "\xE2\x8A\x9A"
   "&ocirc;" "\xC3\xB4"
   "&ocy;" "\xD0\xBE"
   "&odash;" "\xE2\x8A\x9D"
   "&odblac;" "\xC5\x91"
   "&odiv;" "\xE2\xA8\xB8"
   "&odot;" "\xE2\x8A\x99"
   "&odsold;" "\xE2\xA6\xBC"
   "&oelig;" "\xC5\x93"
   "&ofcir;" "\xE2\xA6\xBF"
   "&ofr;" "\xF0\x9D\x94\xAC"
   "&ogon;" "\xCB\x9B"
   "&ograve;" "\xC3\xB2"
   "&ogt;" "\xE2\xA7\x81"
   "&ohbar;" "\xE2\xA6\xB5"
   "&ohm;" "\xCE\xA9"
   "&oint;" "\xE2\x88\xAE"
   "&olarr;" "\xE2\x86\xBA"
   "&olcir;" "\xE2\xA6\xBE"
   "&olcross;" "\xE2\xA6\xBB"
   "&oline;" "\xE2\x80\xBE"
   "&olt;" "\xE2\xA7\x80"
   "&omacr;" "\xC5\x8D"
   "&omega;" "\xCF\x89"
   "&omicron;" "\xCE\xBF"
   "&omid;" "\xE2\xA6\xB6"
   "&ominus;" "\xE2\x8A\x96"
   "&oopf;" "\xF0\x9D\x95\xA0"
   "&opar;" "\xE2\xA6\xB7"
   "&operp;" "\xE2\xA6\xB9"
   "&oplus;" "\xE2\x8A\x95"
   "&or;" "\xE2\x88\xA8"
   "&orarr;" "\xE2\x86\xBB"
   "&ord;" "\xE2\xA9\x9D"
   "&order;" "\xE2\x84\xB4"
   "&orderof;" "\xE2\x84\xB4"
   "&ordf;" "\xC2\xAA"
   "&ordm;" "\xC2\xBA"
   "&origof;" "\xE2\x8A\xB6"
   "&oror;" "\xE2\xA9\x96"
   "&orslope;" "\xE2\xA9\x97"
   "&orv;" "\xE2\xA9\x9B"
   "&oscr;" "\xE2\x84\xB4"
   "&oslash;" "\xC3\xB8"
   "&osol;" "\xE2\x8A\x98"
   "&otilde;" "\xC3\xB5"
   "&otimes;" "\xE2\x8A\x97"
   "&otimesas;" "\xE2\xA8\xB6"
   "&ouml;" "\xC3\xB6"
   "&ovbar;" "\xE2\x8C\xBD"
   "&par;" "\xE2\x88\xA5"
   "&para;" "\xC2\xB6"
   "&parallel;" "\xE2\x88\xA5"
   "&parsim;" "\xE2\xAB\xB3"
   "&parsl;" "\xE2\xAB\xBD"
   "&part;" "\xE2\x88\x82"
   "&pcy;" "\xD0\xBF"
   "&percnt;" "%"
   "&period;" "."
   "&permil;" "\xE2\x80\xB0"
   "&perp;" "\xE2\x8A\xA5"
   "&pertenk;" "\xE2\x80\xB1"
   "&pfr;" "\xF0\x9D\x94\xAD"
   "&phi;" "\xCF\x86"
   "&phiv;" "\xCF\x95"
   "&phmmat;" "\xE2\x84\xB3"
   "&phone;" "\xE2\x98\x8E"
   "&pi;" "\xCF\x80"
   "&pitchfork;" "\xE2\x8B\x94"
   "&piv;" "\xCF\x96"
   "&planck;" "\xE2\x84\x8F"
   "&planckh;" "\xE2\x84\x8E"
   "&plankv;" "\xE2\x84\x8F"
   "&plus;" "+"
   "&plusacir;" "\xE2\xA8\xA3"
   "&plusb;" "\xE2\x8A\x9E"
   "&pluscir;" "\xE2\xA8\xA2"
   "&plusdo;" "\xE2\x88\x94"
   "&plusdu;" "\xE2\xA8\xA5"
   "&pluse;" "\xE2\xA9\xB2"
   "&plusmn;" "\xC2\xB1"
   "&plussim;" "\xE2\xA8\xA6"
   "&plustwo;" "\xE2\xA8\xA7"
   "&pm;" "\xC2\xB1"
   "&pointint;" "\xE2\xA8\x95"
   "&popf;" "\xF0\x9D\x95\xA1"
   "&pound;" "\xC2\xA3"
   "&pr;" "\xE2\x89\xBA"
   "&prE;" "\xE2\xAA\xB3"
   "&prap;" "\xE2\xAA\xB7"
   "&prcue;" "\xE2\x89\xBC"
   "&pre;" "\xE2\xAA\xAF"
   "&prec;" "\xE2\x89\xBA"
   "&precapprox;" "\xE2\xAA\xB7"
   "&preccurlyeq;" "\xE2\x89\xBC"
   "&preceq;" "\xE2\xAA\xAF"
   "&precnapprox;" "\xE2\xAA\xB9"
   "&precneqq;" "\xE2\xAA\xB5"
   "&precnsim;" "\xE2\x8B\xA8"
   "&precsim;" "\xE2\x89\xBE"
   "&prime;" "\xE2\x80\xB2"
   "&primes;" "\xE2\x84\x99"
   "&prnE;" "\xE2\xAA\xB5"
   "&prnap;" "\xE2\xAA\xB9"
   "&prnsim;" "\xE2\x8B\xA8"
   "&prod;" "\xE2\x88\x8F"
   "&profalar;" "\xE2\x8C\xAE"
   "&profline;" "\xE2\x8C\x92"
   "&profsurf;" "\xE2\x8C\x93"
   "&prop;" "\xE2\x88\x9D"
   "&propto;" "\xE2\x88\x9D"
   "&prsim;" "\xE2\x89\xBE"
   "&prurel;" "\xE2\x8A\xB0"
   "&pscr;" "\xF0\x9D\x93\x85"
   "&psi;" "\xCF\x88"
   "&puncsp;" "\xE2\x80\x88"
   "&qfr;" "\xF0\x9D\x94\xAE"
   "&qint;" "\xE2\xA8\x8C"
   "&qopf;" "\xF0\x9D\x95\xA2"
   "&qprime;" "\xE2\x81\x97"
   "&qscr;" "\xF0\x9D\x93\x86"
   "&quaternions;" "\xE2\x84\x8D"
   "&quatint;" "\xE2\xA8\x96"
   "&quest;" "?"
   "&questeq;" "\xE2\x89\x9F"
   "&quot;" "\""
   "&rAarr;" "\xE2\x87\x9B"
   "&rArr;" "\xE2\x87\x92"
   "&rAtail;" "\xE2\xA4\x9C"
   "&rBarr;" "\xE2\xA4\x8F"
   "&rHar;" "\xE2\xA5\xA4"
   "&race;" "\xE2\x88\xBD\xCC\xB1"
   "&racute;" "\xC5\x95"
   "&radic;" "\xE2\x88\x9A"
   "&raemptyv;" "\xE2\xA6\xB3"
   "&rang;" "\xE2\x9F\xA9"
   "&rangd;" "\xE2\xA6\x92"
   "&range;" "\xE2\xA6\xA5"
   "&rangle;" "\xE2\x9F\xA9"
   "&raquo;" "\xC2\xBB"
   "&rarr;" "\xE2\x86\x92"
   "&rarrap;" "\xE2\xA5\xB5"
   "&rarrb;" "\xE2\x87\xA5"
   "&rarrbfs;" "\xE2\xA4\xA0"
   "&rarrc;" "\xE2\xA4\xB3"
   "&rarrfs;" "\xE2\xA4\x9E"
   "&rarrhk;" "\xE2\x86\xAA"
   "&rarrlp;" "\xE2\x86\xAC"
   "&rarrpl;" "\xE2\xA5\x85"
   "&rarrsim;" "\xE2\xA5\xB4"
   "&rarrtl;" "\xE2\x86\xA3"
   "&rarrw;" "\xE2\x86\x9D"
   "&ratail;" "\xE2\xA4\x9A"
   "&ratio;" "\xE2\x88\xB6"
   "&rationals;" "\xE2\x84\x9A"
   "&rbarr;" "\xE2\xA4\x8D"
   "&rbbrk;" "\xE2\x9D\xB3"
   "&rbrace;" "}"
   "&rbrack;" "]"
   "&rbrke;" "\xE2\xA6\x8C"
   "&rbrksld;" "\xE2\xA6\x8E"
   "&rbrkslu;" "\xE2\xA6\x90"
   "&rcaron;" "\xC5\x99"
   "&rcedil;" "\xC5\x97"
   "&rceil;" "\xE2\x8C\x89"
   "&rcub;" "}"
   "&rcy;" "\xD1\x80"
   "&rdca;" "\xE2\xA4\xB7"
   "&rdldhar;" "\xE2\xA5\xA9"
   "&rdquo;" "\xE2\x80\x9D"
   "&rdquor;" "\xE2\x80\x9D"
   "&rdsh;" "\xE2\x86\xB3"
   "&real;" "\xE2\x84\x9C"
   "&realine;" "\xE2\x84\x9B"
   "&realpart;" "\xE2\x84\x9C"
   "&reals;" "\xE2\x84\x9D"
   "&rect;" "\xE2\x96\xAD"
   "&reg;" "\xC2\xAE"
   "&rfisht;" "\xE2\xA5\xBD"
   "&rfloor;" "\xE2\x8C\x8B"
   "&rfr;" "\xF0\x9D\x94\xAF"
   "&rhard;" "\xE2\x87\x81"
   "&rharu;" "\xE2\x87\x80"
   "&rharul;" "\xE2\xA5\xAC"
   "&rho;" "\xCF\x81"
   "&rhov;" "\xCF\xB1"
   "&rightarrow;" "\xE2\x86\x92"
   "&rightarrowtail;" "\xE2\x86\xA3"
   "&rightharpoondown;" "\xE2\x87\x81"
   "&rightharpoonup;" "\xE2\x87\x80"
   "&rightleftarrows;" "\xE2\x87\x84"
   "&rightleftharpoons;" "\xE2\x87\x8C"
   "&rightrightarrows;" "\xE2\x87\x89"
   "&rightsquigarrow;" "\xE2\x86\x9D"
   "&rightthreetimes;" "\xE2\x8B\x8C"
   "&ring;" "\xCB\x9A"
   "&risingdotseq;" "\xE2\x89\x93"
   "&rlarr;" "\xE2\x87\x84"
   "&rlhar;" "\xE2\x87\x8C"
   "&rlm;" "\xE2\x80\x8F"
   "&rmoust;" "\xE2\x8E\xB1"
   "&rmoustache;" "\xE2\x8E\xB1"
   "&rnmid;" "\xE2\xAB\xAE"
   "&roang;" "\xE2\x9F\xAD"
   "&roarr;" "\xE2\x87\xBE"
   "&robrk;" "\xE2\x9F\xA7"
   "&ropar;" "\xE2\xA6\x86"
   "&ropf;" "\xF0\x9D\x95\xA3"
   "&roplus;" "\xE2\xA8\xAE"
   "&rotimes;" "\xE2\xA8\xB5"
   "&rpar;" ")"
   "&rpargt;" "\xE2\xA6\x94"
   "&rppolint;" "\xE2\xA8\x92"
   "&rrarr;" "\xE2\x87\x89"
   "&rsaquo;" "\xE2\x80\xBA"
   "&rscr;" "\xF0\x9D\x93\x87"
   "&rsh;" "\xE2\x86\xB1"
   "&rsqb;" "]"
   "&rsquo;" "\xE2\x80\x99"
   "&rsquor;" "\xE2\x80\x99"
   "&rthree;" "\xE2\x8B\x8C"
   "&rtimes;" "\xE2\x8B\x8A"
   "&rtri;" "\xE2\x96\xB9"
   "&rtrie;" "\xE2\x8A\xB5"
   "&rtrif;" "\xE2\x96\xB8"
   "&rtriltri;" "\xE2\xA7\x8E"
   "&ruluhar;" "\xE2\xA5\xA8"
   "&rx;" "\xE2\x84\x9E"
   "&sacute;" "\xC5\x9B"
   "&sbquo;" "\xE2\x80\x9A"
   "&sc;" "\xE2\x89\xBB"
   "&scE;" "\xE2\xAA\xB4"
   "&scap;" "\xE2\xAA\xB8"
   "&scaron;" "\xC5\xA1"
   "&sccue;" "\xE2\x89\xBD"
   "&sce;" "\xE2\xAA\xB0"
   "&scedil;" "\xC5\x9F"
   "&scirc;" "\xC5\x9D"
   "&scnE;" "\xE2\xAA\xB6"
   "&scnap;" "\xE2\xAA\xBA"
   "&scnsim;" "\xE2\x8B\xA9"
   "&scpolint;" "\xE2\xA8\x93"
   "&scsim;" "\xE2\x89\xBF"
   "&scy;" "\xD1\x81"
   "&sdot;" "\xE2\x8B\x85"
   "&sdotb;" "\xE2\x8A\xA1"
   "&sdote;" "\xE2\xA9\xA6"
   "&seArr;" "\xE2\x87\x98"
   "&searhk;" "\xE2\xA4\xA5"
   "&searr;" "\xE2\x86\x98"
   "&searrow;" "\xE2\x86\x98"
   "&sect;" "\xC2\xA7"
   "&semi;" ";"
   "&seswar;" "\xE2\xA4\xA9"
   "&setminus;" "\xE2\x88\x96"
   "&setmn;" "\xE2\x88\x96"
   "&sext;" "\xE2\x9C\xB6"
   "&sfr;" "\xF0\x9D\x94\xB0"
   "&sfrown;" "\xE2\x8C\xA2"
   "&sharp;" "\xE2\x99\xAF"
   "&shchcy;" "\xD1\x89"
   "&shcy;" "\xD1\x88"
   "&shortmid;" "\xE2\x88\xA3"
   "&shortparallel;" "\xE2\x88\xA5"
   "&shy;" "\xC2\xAD"
   "&sigma;" "\xCF\x83"
   "&sigmaf;" "\xCF\x82"
   "&sigmav;" "\xCF\x82"
   "&sim;" "\xE2\x88\xBC"
   "&simdot;" "\xE2\xA9\xAA"
   "&sime;" "\xE2\x89\x83"
   "&simeq;" "\xE2\x89\x83"
   "&simg;" "\xE2\xAA\x9E"
   "&simgE;" "\xE2\xAA\xA0"
   "&siml;" "\xE2\xAA\x9D"
   "&simlE;" "\xE2\xAA\x9F"
   "&simne;" "\xE2\x89\x86"
   "&simplus;" "\xE2\xA8\xA4"
   "&simrarr;" "\xE2\xA5\xB2"
   "&slarr;" "\xE2\x86\x90"
   "&smallsetminus;" "\xE2\x88\x96"
   "&smashp;" "\xE2\xA8\xB3"
   "&smeparsl;" "\xE2\xA7\xA4"
   "&smid;" "\xE2\x88\xA3"
   "&smile;" "\xE2\x8C\xA3"
   "&smt;" "\xE2\xAA\xAA"
   "&smte;" "\xE2\xAA\xAC"
   "&smtes;" "\xE2\xAA\xAC\xEF\xB8\x80"
   "&softcy;" "\xD1\x8C"
   "&sol;" "/"
   "&solb;" "\xE2\xA7\x84"
   "&solbar;" "\xE2\x8C\xBF"
   "&sopf;" "\xF0\x9D\x95\xA4"
   "&spades;" "\xE2\x99\xA0"
   "&spadesuit;" "\xE2\x99\xA0"
   "&spar;" "\xE2\x88\xA5"
   "&sqcap;" "\xE2\x8A\x93"
   "&sqcaps;" "\xE2\x8A\x93\xEF\xB8\x80"
   "&sqcup;" "\xE2\x8A\x94"
   "&sqcups;" "\xE2\x8A\x94\xEF\xB8\x80"
   "&sqsub;" "\xE2\x8A\x8F"
   "&sqsube;" "\xE2\x8A\x91"
   "&sqsubset;" "\xE2\x8A\x8F"
   "&sqsubseteq;" "\xE2\x8A\x91"
   "&sqsup;" "\xE2\x8A\x90"
   "&sqsupe;" "\xE2\x8A\x92"
   "&sqsupset;" "\xE2\x8A\x90"
   "&sqsupseteq;" "\xE2\x8A\x92"
   "&squ;" "\xE2\x96\xA1"
   "&square;" "\xE2\x96\xA1"
   "&squarf;" "\xE2\x96\xAA"
   "&squf;" "\xE2\x96\xAA"
   "&srarr;" "\xE2\x86\x92"
   "&sscr;" "\xF0\x9D\x93\x88"
   "&ssetmn;" "\xE2\x88\x96"
   "&ssmile;" "\xE2\x8C\xA3"
   "&sstarf;" "\xE2\x8B\x86"
   "&star;" "\xE2\x98\x86"
   "&starf;" "\xE2\x98\x85"
   "&straightepsilon;" "\xCF\xB5"
   "&straightphi;" "\xCF\x95"
   "&strns;" "\xC2\xAF"
   "&sub;" "\xE2\x8A\x82"
   "&subE;" "\xE2\xAB\x85"
   "&subdot;" "\xE2\xAA\xBD"
   "&sube;" "\xE2\x8A\x86"
   "&subedot;" "\xE2\xAB\x83"
   "&submult;" "\xE2\xAB\x81"
   "&subnE;" "\xE2\xAB\x8B"
   "&subne;" "\xE2\x8A\x8A"
   "&subplus;" "\xE2\xAA\xBF"
   "&subrarr;" "\xE2\xA5\xB9"
   "&subset;" "\xE2\x8A\x82"
   "&subseteq;" "\xE2\x8A\x86"
   "&subseteqq;" "\xE2\xAB\x85"
   "&subsetneq;" "\xE2\x8A\x8A"
   "&subsetneqq;" "\xE2\xAB\x8B"
   "&subsim;" "\xE2\xAB\x87"
   "&subsub;" "\xE2\xAB\x95"
   "&subsup;" "\xE2\xAB\x93"
   "&succ;" "\xE2\x89\xBB"
   "&succapprox;" "\xE2\xAA\xB8"
   "&succcurlyeq;" "\xE2\x89\xBD"
   "&succeq;" "\xE2\xAA\xB0"
   "&succnapprox;" "\xE2\xAA\xBA"
   "&succneqq;" "\xE2\xAA\xB6"
   "&succnsim;" "\xE2\x8B\xA9"
   "&succsim;" "\xE2\x89\xBF"
   "&sum;" "\xE2\x88\x91"
   "&sung;" "\xE2\x99\xAA"
   "&sup1;" "\xC2\xB9"
   "&sup2;" "\xC2\xB2"
   "&sup3;" "\xC2\xB3"
   "&sup;" "\xE2\x8A\x83"
   "&supE;" "\xE2\xAB\x86"
   "&supdot;" "\xE2\xAA\xBE"
   "&supdsub;" "\xE2\xAB\x98"
   "&supe;" "\xE2\x8A\x87"
   "&supedot;" "\xE2\xAB\x84"
   "&suphsol;" "\xE2\x9F\x89"
   "&suphsub;" "\xE2\xAB\x97"
   "&suplarr;" "\xE2\xA5\xBB"
   "&supmult;" "\xE2\xAB\x82"
   "&supnE;" "\xE2\xAB\x8C"
   "&supne;" "\xE2\x8A\x8B"
   "&supplus;" "\xE2\xAB\x80"
   "&supset;" "\xE2\x8A\x83"
   "&supseteq;" "\xE2\x8A\x87"
   "&supseteqq;" "\xE2\xAB\x86"
   "&supsetneq;" "\xE2\x8A\x8B"
   "&supsetneqq;" "\xE2\xAB\x8C"
   "&supsim;" "\xE2\xAB\x88"
   "&supsub;" "\xE2\xAB\x94"
   "&supsup;" "\xE2\xAB\x96"
   "&swArr;" "\xE2\x87\x99"
   "&swarhk;" "\xE2\xA4\xA6"
   "&swarr;" "\xE2\x86\x99"
   "&swarrow;" "\xE2\x86\x99"
   "&swnwar;" "\xE2\xA4\xAA"
   "&szlig;" "\xC3\x9F"
   "&target;" "\xE2\x8C\x96"
   "&tau;" "\xCF\x84"
   "&tbrk;" "\xE2\x8E\xB4"
   "&tcaron;" "\xC5\xA5"
   "&tcedil;" "\xC5\xA3"
   "&tcy;" "\xD1\x82"
   "&tdot;" "\xE2\x83\x9B"
   "&telrec;" "\xE2\x8C\x95"
   "&tfr;" "\xF0\x9D\x94\xB1"
   "&there4;" "\xE2\x88\xB4"
   "&therefore;" "\xE2\x88\xB4"
   "&theta;" "\xCE\xB8"
   "&thetasym;" "\xCF\x91"
   "&thetav;" "\xCF\x91"
   "&thickapprox;" "\xE2\x89\x88"
   "&thicksim;" "\xE2\x88\xBC"
   "&thinsp;" "\xE2\x80\x89"
   "&thkap;" "\xE2\x89\x88"
   "&thksim;" "\xE2\x88\xBC"
   "&thorn;" "\xC3\xBE"
   "&tilde;" "\xCB\x9C"
   "&times;" "\xC3\x97"
   "&timesb;" "\xE2\x8A\xA0"
   "&timesbar;" "\xE2\xA8\xB1"
   "&timesd;" "\xE2\xA8\xB0"
   "&tint;" "\xE2\x88\xAD"
   "&toea;" "\xE2\xA4\xA8"
   "&top;" "\xE2\x8A\xA4"
   "&topbot;" "\xE2\x8C\xB6"
   "&topcir;" "\xE2\xAB\xB1"
   "&topf;" "\xF0\x9D\x95\xA5"
   "&topfork;" "\xE2\xAB\x9A"
   "&tosa;" "\xE2\xA4\xA9"
   "&tprime;" "\xE2\x80\xB4"
   "&trade;" "\xE2\x84\xA2"
   "&triangle;" "\xE2\x96\xB5"
   "&triangledown;" "\xE2\x96\xBF"
   "&triangleleft;" "\xE2\x97\x83"
   "&trianglelefteq;" "\xE2\x8A\xB4"
   "&triangleq;" "\xE2\x89\x9C"
   "&triangleright;" "\xE2\x96\xB9"
   "&trianglerighteq;" "\xE2\x8A\xB5"
   "&tridot;" "\xE2\x97\xAC"
   "&trie;" "\xE2\x89\x9C"
   "&triminus;" "\xE2\xA8\xBA"
   "&triplus;" "\xE2\xA8\xB9"
   "&trisb;" "\xE2\xA7\x8D"
   "&tritime;" "\xE2\xA8\xBB"
   "&trpezium;" "\xE2\x8F\xA2"
   "&tscr;" "\xF0\x9D\x93\x89"
   "&tscy;" "\xD1\x86"
   "&tshcy;" "\xD1\x9B"
   "&tstrok;" "\xC5\xA7"
   "&twixt;" "\xE2\x89\xAC"
   "&twoheadleftarrow;" "\xE2\x86\x9E"
   "&twoheadrightarrow;" "\xE2\x86\xA0"
   "&uArr;" "\xE2\x87\x91"
   "&uHar;" "\xE2\xA5\xA3"
   "&uacute;" "\xC3\xBA"
   "&uarr;" "\xE2\x86\x91"
   "&ubrcy;" "\xD1\x9E"
   "&ubreve;" "\xC5\xAD"
   "&ucirc;" "\xC3\xBB"
   "&ucy;" "\xD1\x83"
   "&udarr;" "\xE2\x87\x85"
   "&udblac;" "\xC5\xB1"
   "&udhar;" "\xE2\xA5\xAE"
   "&ufisht;" "\xE2\xA5\xBE"
   "&ufr;" "\xF0\x9D\x94\xB2"
   "&ugrave;" "\xC3\xB9"
   "&uharl;" "\xE2\x86\xBF"
   "&uharr;" "\xE2\x86\xBE"
   "&uhblk;" "\xE2\x96\x80"
   "&ulcorn;" "\xE2\x8C\x9C"
   "&ulcorner;" "\xE2\x8C\x9C"
   "&ulcrop;" "\xE2\x8C\x8F"
   "&ultri;" "\xE2\x97\xB8"
   "&umacr;" "\xC5\xAB"
   "&uml;" "\xC2\xA8"
   "&uogon;" "\xC5\xB3"
   "&uopf;" "\xF0\x9D\x95\xA6"
   "&uparrow;" "\xE2\x86\x91"
   "&updownarrow;" "\xE2\x86\x95"
   "&upharpoonleft;" "\xE2\x86\xBF"
   "&upharpoonright;" "\xE2\x86\xBE"
   "&uplus;" "\xE2\x8A\x8E"
   "&upsi;" "\xCF\x85"
   "&upsih;" "\xCF\x92"
   "&upsilon;" "\xCF\x85"
   "&upuparrows;" "\xE2\x87\x88"
   "&urcorn;" "\xE2\x8C\x9D"
   "&urcorner;" "\xE2\x8C\x9D"
   "&urcrop;" "\xE2\x8C\x8E"
   "&uring;" "\xC5\xAF"
   "&urtri;" "\xE2\x97\xB9"
   "&uscr;" "\xF0\x9D\x93\x8A"
   "&utdot;" "\xE2\x8B\xB0"
   "&utilde;" "\xC5\xA9"
   "&utri;" "\xE2\x96\xB5"
   "&utrif;" "\xE2\x96\xB4"
   "&uuarr;" "\xE2\x87\x88"
   "&uuml;" "\xC3\xBC"
   "&uwangle;" "\xE2\xA6\xA7"
   "&vArr;" "\xE2\x87\x95"
   "&vBar;" "\xE2\xAB\xA8"
   "&vBarv;" "\xE2\xAB\xA9"
   "&vDash;" "\xE2\x8A\xA8"
   "&vangrt;" "\xE2\xA6\x9C"
   "&varepsilon;" "\xCF\xB5"
   "&varkappa;" "\xCF\xB0"
   "&varnothing;" "\xE2\x88\x85"
   "&varphi;" "\xCF\x95"
   "&varpi;" "\xCF\x96"
   "&varpropto;" "\xE2\x88\x9D"
   "&varr;" "\xE2\x86\x95"
   "&varrho;" "\xCF\xB1"
   "&varsigma;" "\xCF\x82"
   "&varsubsetneq;" "\xE2\x8A\x8A\xEF\xB8\x80"
   "&varsubsetneqq;" "\xE2\xAB\x8B\xEF\xB8\x80"
   "&varsupsetneq;" "\xE2\x8A\x8B\xEF\xB8\x80"
   "&varsupsetneqq;" "\xE2\xAB\x8C\xEF\xB8\x80"
   "&vartheta;" "\xCF\x91"
   "&vartriangleleft;" "\xE2\x8A\xB2"
   "&vartriangleright;" "\xE2\x8A\xB3"
   "&vcy;" "\xD0\xB2"
   "&vdash;" "\xE2\x8A\xA2"
   "&vee;" "\xE2\x88\xA8"
   "&veebar;" "\xE2\x8A\xBB"
   "&veeeq;" "\xE2\x89\x9A"
   "&vellip;" "\xE2\x8B\xAE"
   "&verbar;" "|"
   "&vert;" "|"
   "&vfr;" "\xF0\x9D\x94\xB3"
   "&vltri;" "\xE2\x8A\xB2"
   "&vnsub;" "\xE2\x8A\x82\xE2\x83\x92"
   "&vnsup;" "\xE2\x8A\x83\xE2\x83\x92"
   "&vopf;" "\xF0\x9D\x95\xA7"
   "&vprop;" "\xE2\x88\x9D"
   "&vrtri;" "\xE2\x8A\xB3"
   "&vscr;" "\xF0\x9D\x93\x8B"
   "&vsubnE;" "\xE2\xAB\x8B\xEF\xB8\x80"
   "&vsubne;" "\xE2\x8A\x8A\xEF\xB8\x80"
   "&vsupnE;" "\xE2\xAB\x8C\xEF\xB8\x80"
   "&vsupne;" "\xE2\x8A\x8B\xEF\xB8\x80"
   "&vzigzag;" "\xE2\xA6\x9A"
   "&wcirc;" "\xC5\xB5"
   "&wedbar;" "\xE2\xA9\x9F"
   "&wedge;" "\xE2\x88\xA7"
   "&wedgeq;" "\xE2\x89\x99"
   "&weierp;" "\xE2\x84\x98"
   "&wfr;" "\xF0\x9D\x94\xB4"
   "&wopf;" "\xF0\x9D\x95\xA8"
   "&wp;" "\xE2\x84\x98"
   "&wr;" "\xE2\x89\x80"
   "&wreath;" "\xE2\x89\x80"
   "&wscr;" "\xF0\x9D\x93\x8C"
   "&xcap;" "\xE2\x8B\x82"
   "&xcirc;" "\xE2\x97\xAF"
   "&xcup;" "\xE2\x8B\x83"
   "&xdtri;" "\xE2\x96\xBD"
   "&xfr;" "\xF0\x9D\x94\xB5"
   "&xhArr;" "\xE2\x9F\xBA"
   "&xharr;" "\xE2\x9F\xB7"
   "&xi;" "\xCE\xBE"
   "&xlArr;" "\xE2\x9F\xB8"
   "&xlarr;" "\xE2\x9F\xB5"
   "&xmap;" "\xE2\x9F\xBC"
   "&xnis;" "\xE2\x8B\xBB"
   "&xodot;" "\xE2\xA8\x80"
   "&xopf;" "\xF0\x9D\x95\xA9"
   "&xoplus;" "\xE2\xA8\x81"
   "&xotime;" "\xE2\xA8\x82"
   "&xrArr;" "\xE2\x9F\xB9"
   "&xrarr;" "\xE2\x9F\xB6"
   "&xscr;" "\xF0\x9D\x93\x8D"
   "&xsqcup;" "\xE2\xA8\x86"
   "&xuplus;" "\xE2\xA8\x84"
   "&xutri;" "\xE2\x96\xB3"
   "&xvee;" "\xE2\x8B\x81"
   "&xwedge;" "\xE2\x8B\x80"
   "&yacute;" "\xC3\xBD"
   "&yacy;" "\xD1\x8F"
   "&ycirc;" "\xC5\xB7"
   "&ycy;" "\xD1\x8B"
   "&yen;" "\xC2\xA5"
   "&yfr;" "\xF0\x9D\x94\xB6"
   "&yicy;" "\xD1\x97"
   "&yopf;" "\xF0\x9D\x95\xAA"
   "&yscr;" "\xF0\x9D\x93\x8E"
   "&yucy;" "\xD1\x8E"
   "&yuml;" "\xC3\xBF"
   "&zacute;" "\xC5\xBA"
   "&zcaron;" "\xC5\xBE"
   "&zcy;" "\xD0\xB7"
   "&zdot;" "\xC5\xBC"
   "&zeetrf;" "\xE2\x84\xA8"
   "&zeta;" "\xCE\xB6"
   "&zfr;" "\xF0\x9D\x94\xB7"
   "&zhcy;" "\xD0\xB6"
   "&zigrarr;" "\xE2\x87\x9D"
   "&zopf;" "\xF0\x9D\x95\xAB"
   "&zscr;" "\xF0\x9D\x93\x8F"
   "&zwj;" "\xE2\x80\x8D"
   "&zwnj;" "\xE2\x80\x8C"
})

(defn entities/valid-entity? [entity]
  (not= nil (get entities/entity-map entity)))

(defn entities/to-grapheme [entity]
  (get entities/entity-map entity))
(comment import ./folding :prefix "")
(def folding/lower
  {
   0x0041 [0x0061]
   0x0042 [0x0062]
   0x0043 [0x0063]
   0x0044 [0x0064]
   0x0045 [0x0065]
   0x0046 [0x0066]
   0x0047 [0x0067]
   0x0048 [0x0068]
   0x0049 [0x0069]
   0x004A [0x006A]
   0x004B [0x006B]
   0x004C [0x006C]
   0x004D [0x006D]
   0x004E [0x006E]
   0x004F [0x006F]
   0x0050 [0x0070]
   0x0051 [0x0071]
   0x0052 [0x0072]
   0x0053 [0x0073]
   0x0054 [0x0074]
   0x0055 [0x0075]
   0x0056 [0x0076]
   0x0057 [0x0077]
   0x0058 [0x0078]
   0x0059 [0x0079]
   0x005A [0x007A]
   0x00B5 [0x03BC]
   0x00C0 [0x00E0]
   0x00C1 [0x00E1]
   0x00C2 [0x00E2]
   0x00C3 [0x00E3]
   0x00C4 [0x00E4]
   0x00C5 [0x00E5]
   0x00C6 [0x00E6]
   0x00C7 [0x00E7]
   0x00C8 [0x00E8]
   0x00C9 [0x00E9]
   0x00CA [0x00EA]
   0x00CB [0x00EB]
   0x00CC [0x00EC]
   0x00CD [0x00ED]
   0x00CE [0x00EE]
   0x00CF [0x00EF]
   0x00D0 [0x00F0]
   0x00D1 [0x00F1]
   0x00D2 [0x00F2]
   0x00D3 [0x00F3]
   0x00D4 [0x00F4]
   0x00D5 [0x00F5]
   0x00D6 [0x00F6]
   0x00D8 [0x00F8]
   0x00D9 [0x00F9]
   0x00DA [0x00FA]
   0x00DB [0x00FB]
   0x00DC [0x00FC]
   0x00DD [0x00FD]
   0x00DE [0x00FE]
   0x00DF [0x0073 0x0073]
   0x0100 [0x0101]
   0x0102 [0x0103]
   0x0104 [0x0105]
   0x0106 [0x0107]
   0x0108 [0x0109]
   0x010A [0x010B]
   0x010C [0x010D]
   0x010E [0x010F]
   0x0110 [0x0111]
   0x0112 [0x0113]
   0x0114 [0x0115]
   0x0116 [0x0117]
   0x0118 [0x0119]
   0x011A [0x011B]
   0x011C [0x011D]
   0x011E [0x011F]
   0x0120 [0x0121]
   0x0122 [0x0123]
   0x0124 [0x0125]
   0x0126 [0x0127]
   0x0128 [0x0129]
   0x012A [0x012B]
   0x012C [0x012D]
   0x012E [0x012F]
   0x0130 [0x0069 0x0307]
   0x0132 [0x0133]
   0x0134 [0x0135]
   0x0136 [0x0137]
   0x0139 [0x013A]
   0x013B [0x013C]
   0x013D [0x013E]
   0x013F [0x0140]
   0x0141 [0x0142]
   0x0143 [0x0144]
   0x0145 [0x0146]
   0x0147 [0x0148]
   0x0149 [0x02BC 0x006E]
   0x014A [0x014B]
   0x014C [0x014D]
   0x014E [0x014F]
   0x0150 [0x0151]
   0x0152 [0x0153]
   0x0154 [0x0155]
   0x0156 [0x0157]
   0x0158 [0x0159]
   0x015A [0x015B]
   0x015C [0x015D]
   0x015E [0x015F]
   0x0160 [0x0161]
   0x0162 [0x0163]
   0x0164 [0x0165]
   0x0166 [0x0167]
   0x0168 [0x0169]
   0x016A [0x016B]
   0x016C [0x016D]
   0x016E [0x016F]
   0x0170 [0x0171]
   0x0172 [0x0173]
   0x0174 [0x0175]
   0x0176 [0x0177]
   0x0178 [0x00FF]
   0x0179 [0x017A]
   0x017B [0x017C]
   0x017D [0x017E]
   0x017F [0x0073]
   0x0181 [0x0253]
   0x0182 [0x0183]
   0x0184 [0x0185]
   0x0186 [0x0254]
   0x0187 [0x0188]
   0x0189 [0x0256]
   0x018A [0x0257]
   0x018B [0x018C]
   0x018E [0x01DD]
   0x018F [0x0259]
   0x0190 [0x025B]
   0x0191 [0x0192]
   0x0193 [0x0260]
   0x0194 [0x0263]
   0x0196 [0x0269]
   0x0197 [0x0268]
   0x0198 [0x0199]
   0x019C [0x026F]
   0x019D [0x0272]
   0x019F [0x0275]
   0x01A0 [0x01A1]
   0x01A2 [0x01A3]
   0x01A4 [0x01A5]
   0x01A6 [0x0280]
   0x01A7 [0x01A8]
   0x01A9 [0x0283]
   0x01AC [0x01AD]
   0x01AE [0x0288]
   0x01AF [0x01B0]
   0x01B1 [0x028A]
   0x01B2 [0x028B]
   0x01B3 [0x01B4]
   0x01B5 [0x01B6]
   0x01B7 [0x0292]
   0x01B8 [0x01B9]
   0x01BC [0x01BD]
   0x01C4 [0x01C6]
   0x01C5 [0x01C6]
   0x01C7 [0x01C9]
   0x01C8 [0x01C9]
   0x01CA [0x01CC]
   0x01CB [0x01CC]
   0x01CD [0x01CE]
   0x01CF [0x01D0]
   0x01D1 [0x01D2]
   0x01D3 [0x01D4]
   0x01D5 [0x01D6]
   0x01D7 [0x01D8]
   0x01D9 [0x01DA]
   0x01DB [0x01DC]
   0x01DE [0x01DF]
   0x01E0 [0x01E1]
   0x01E2 [0x01E3]
   0x01E4 [0x01E5]
   0x01E6 [0x01E7]
   0x01E8 [0x01E9]
   0x01EA [0x01EB]
   0x01EC [0x01ED]
   0x01EE [0x01EF]
   0x01F0 [0x006A 0x030C]
   0x01F1 [0x01F3]
   0x01F2 [0x01F3]
   0x01F4 [0x01F5]
   0x01F6 [0x0195]
   0x01F7 [0x01BF]
   0x01F8 [0x01F9]
   0x01FA [0x01FB]
   0x01FC [0x01FD]
   0x01FE [0x01FF]
   0x0200 [0x0201]
   0x0202 [0x0203]
   0x0204 [0x0205]
   0x0206 [0x0207]
   0x0208 [0x0209]
   0x020A [0x020B]
   0x020C [0x020D]
   0x020E [0x020F]
   0x0210 [0x0211]
   0x0212 [0x0213]
   0x0214 [0x0215]
   0x0216 [0x0217]
   0x0218 [0x0219]
   0x021A [0x021B]
   0x021C [0x021D]
   0x021E [0x021F]
   0x0220 [0x019E]
   0x0222 [0x0223]
   0x0224 [0x0225]
   0x0226 [0x0227]
   0x0228 [0x0229]
   0x022A [0x022B]
   0x022C [0x022D]
   0x022E [0x022F]
   0x0230 [0x0231]
   0x0232 [0x0233]
   0x023A [0x2C65]
   0x023B [0x023C]
   0x023D [0x019A]
   0x023E [0x2C66]
   0x0241 [0x0242]
   0x0243 [0x0180]
   0x0244 [0x0289]
   0x0245 [0x028C]
   0x0246 [0x0247]
   0x0248 [0x0249]
   0x024A [0x024B]
   0x024C [0x024D]
   0x024E [0x024F]
   0x0345 [0x03B9]
   0x0370 [0x0371]
   0x0372 [0x0373]
   0x0376 [0x0377]
   0x037F [0x03F3]
   0x0386 [0x03AC]
   0x0388 [0x03AD]
   0x0389 [0x03AE]
   0x038A [0x03AF]
   0x038C [0x03CC]
   0x038E [0x03CD]
   0x038F [0x03CE]
   0x0390 [0x03B9 0x0308 0x0301]
   0x0391 [0x03B1]
   0x0392 [0x03B2]
   0x0393 [0x03B3]
   0x0394 [0x03B4]
   0x0395 [0x03B5]
   0x0396 [0x03B6]
   0x0397 [0x03B7]
   0x0398 [0x03B8]
   0x0399 [0x03B9]
   0x039A [0x03BA]
   0x039B [0x03BB]
   0x039C [0x03BC]
   0x039D [0x03BD]
   0x039E [0x03BE]
   0x039F [0x03BF]
   0x03A0 [0x03C0]
   0x03A1 [0x03C1]
   0x03A3 [0x03C3]
   0x03A4 [0x03C4]
   0x03A5 [0x03C5]
   0x03A6 [0x03C6]
   0x03A7 [0x03C7]
   0x03A8 [0x03C8]
   0x03A9 [0x03C9]
   0x03AA [0x03CA]
   0x03AB [0x03CB]
   0x03B0 [0x03C5 0x0308 0x0301]
   0x03C2 [0x03C3]
   0x03CF [0x03D7]
   0x03D0 [0x03B2]
   0x03D1 [0x03B8]
   0x03D5 [0x03C6]
   0x03D6 [0x03C0]
   0x03D8 [0x03D9]
   0x03DA [0x03DB]
   0x03DC [0x03DD]
   0x03DE [0x03DF]
   0x03E0 [0x03E1]
   0x03E2 [0x03E3]
   0x03E4 [0x03E5]
   0x03E6 [0x03E7]
   0x03E8 [0x03E9]
   0x03EA [0x03EB]
   0x03EC [0x03ED]
   0x03EE [0x03EF]
   0x03F0 [0x03BA]
   0x03F1 [0x03C1]
   0x03F4 [0x03B8]
   0x03F5 [0x03B5]
   0x03F7 [0x03F8]
   0x03F9 [0x03F2]
   0x03FA [0x03FB]
   0x03FD [0x037B]
   0x03FE [0x037C]
   0x03FF [0x037D]
   0x0400 [0x0450]
   0x0401 [0x0451]
   0x0402 [0x0452]
   0x0403 [0x0453]
   0x0404 [0x0454]
   0x0405 [0x0455]
   0x0406 [0x0456]
   0x0407 [0x0457]
   0x0408 [0x0458]
   0x0409 [0x0459]
   0x040A [0x045A]
   0x040B [0x045B]
   0x040C [0x045C]
   0x040D [0x045D]
   0x040E [0x045E]
   0x040F [0x045F]
   0x0410 [0x0430]
   0x0411 [0x0431]
   0x0412 [0x0432]
   0x0413 [0x0433]
   0x0414 [0x0434]
   0x0415 [0x0435]
   0x0416 [0x0436]
   0x0417 [0x0437]
   0x0418 [0x0438]
   0x0419 [0x0439]
   0x041A [0x043A]
   0x041B [0x043B]
   0x041C [0x043C]
   0x041D [0x043D]
   0x041E [0x043E]
   0x041F [0x043F]
   0x0420 [0x0440]
   0x0421 [0x0441]
   0x0422 [0x0442]
   0x0423 [0x0443]
   0x0424 [0x0444]
   0x0425 [0x0445]
   0x0426 [0x0446]
   0x0427 [0x0447]
   0x0428 [0x0448]
   0x0429 [0x0449]
   0x042A [0x044A]
   0x042B [0x044B]
   0x042C [0x044C]
   0x042D [0x044D]
   0x042E [0x044E]
   0x042F [0x044F]
   0x0460 [0x0461]
   0x0462 [0x0463]
   0x0464 [0x0465]
   0x0466 [0x0467]
   0x0468 [0x0469]
   0x046A [0x046B]
   0x046C [0x046D]
   0x046E [0x046F]
   0x0470 [0x0471]
   0x0472 [0x0473]
   0x0474 [0x0475]
   0x0476 [0x0477]
   0x0478 [0x0479]
   0x047A [0x047B]
   0x047C [0x047D]
   0x047E [0x047F]
   0x0480 [0x0481]
   0x048A [0x048B]
   0x048C [0x048D]
   0x048E [0x048F]
   0x0490 [0x0491]
   0x0492 [0x0493]
   0x0494 [0x0495]
   0x0496 [0x0497]
   0x0498 [0x0499]
   0x049A [0x049B]
   0x049C [0x049D]
   0x049E [0x049F]
   0x04A0 [0x04A1]
   0x04A2 [0x04A3]
   0x04A4 [0x04A5]
   0x04A6 [0x04A7]
   0x04A8 [0x04A9]
   0x04AA [0x04AB]
   0x04AC [0x04AD]
   0x04AE [0x04AF]
   0x04B0 [0x04B1]
   0x04B2 [0x04B3]
   0x04B4 [0x04B5]
   0x04B6 [0x04B7]
   0x04B8 [0x04B9]
   0x04BA [0x04BB]
   0x04BC [0x04BD]
   0x04BE [0x04BF]
   0x04C0 [0x04CF]
   0x04C1 [0x04C2]
   0x04C3 [0x04C4]
   0x04C5 [0x04C6]
   0x04C7 [0x04C8]
   0x04C9 [0x04CA]
   0x04CB [0x04CC]
   0x04CD [0x04CE]
   0x04D0 [0x04D1]
   0x04D2 [0x04D3]
   0x04D4 [0x04D5]
   0x04D6 [0x04D7]
   0x04D8 [0x04D9]
   0x04DA [0x04DB]
   0x04DC [0x04DD]
   0x04DE [0x04DF]
   0x04E0 [0x04E1]
   0x04E2 [0x04E3]
   0x04E4 [0x04E5]
   0x04E6 [0x04E7]
   0x04E8 [0x04E9]
   0x04EA [0x04EB]
   0x04EC [0x04ED]
   0x04EE [0x04EF]
   0x04F0 [0x04F1]
   0x04F2 [0x04F3]
   0x04F4 [0x04F5]
   0x04F6 [0x04F7]
   0x04F8 [0x04F9]
   0x04FA [0x04FB]
   0x04FC [0x04FD]
   0x04FE [0x04FF]
   0x0500 [0x0501]
   0x0502 [0x0503]
   0x0504 [0x0505]
   0x0506 [0x0507]
   0x0508 [0x0509]
   0x050A [0x050B]
   0x050C [0x050D]
   0x050E [0x050F]
   0x0510 [0x0511]
   0x0512 [0x0513]
   0x0514 [0x0515]
   0x0516 [0x0517]
   0x0518 [0x0519]
   0x051A [0x051B]
   0x051C [0x051D]
   0x051E [0x051F]
   0x0520 [0x0521]
   0x0522 [0x0523]
   0x0524 [0x0525]
   0x0526 [0x0527]
   0x0528 [0x0529]
   0x052A [0x052B]
   0x052C [0x052D]
   0x052E [0x052F]
   0x0531 [0x0561]
   0x0532 [0x0562]
   0x0533 [0x0563]
   0x0534 [0x0564]
   0x0535 [0x0565]
   0x0536 [0x0566]
   0x0537 [0x0567]
   0x0538 [0x0568]
   0x0539 [0x0569]
   0x053A [0x056A]
   0x053B [0x056B]
   0x053C [0x056C]
   0x053D [0x056D]
   0x053E [0x056E]
   0x053F [0x056F]
   0x0540 [0x0570]
   0x0541 [0x0571]
   0x0542 [0x0572]
   0x0543 [0x0573]
   0x0544 [0x0574]
   0x0545 [0x0575]
   0x0546 [0x0576]
   0x0547 [0x0577]
   0x0548 [0x0578]
   0x0549 [0x0579]
   0x054A [0x057A]
   0x054B [0x057B]
   0x054C [0x057C]
   0x054D [0x057D]
   0x054E [0x057E]
   0x054F [0x057F]
   0x0550 [0x0580]
   0x0551 [0x0581]
   0x0552 [0x0582]
   0x0553 [0x0583]
   0x0554 [0x0584]
   0x0555 [0x0585]
   0x0556 [0x0586]
   0x0587 [0x0565 0x0582]
   0x10A0 [0x2D00]
   0x10A1 [0x2D01]
   0x10A2 [0x2D02]
   0x10A3 [0x2D03]
   0x10A4 [0x2D04]
   0x10A5 [0x2D05]
   0x10A6 [0x2D06]
   0x10A7 [0x2D07]
   0x10A8 [0x2D08]
   0x10A9 [0x2D09]
   0x10AA [0x2D0A]
   0x10AB [0x2D0B]
   0x10AC [0x2D0C]
   0x10AD [0x2D0D]
   0x10AE [0x2D0E]
   0x10AF [0x2D0F]
   0x10B0 [0x2D10]
   0x10B1 [0x2D11]
   0x10B2 [0x2D12]
   0x10B3 [0x2D13]
   0x10B4 [0x2D14]
   0x10B5 [0x2D15]
   0x10B6 [0x2D16]
   0x10B7 [0x2D17]
   0x10B8 [0x2D18]
   0x10B9 [0x2D19]
   0x10BA [0x2D1A]
   0x10BB [0x2D1B]
   0x10BC [0x2D1C]
   0x10BD [0x2D1D]
   0x10BE [0x2D1E]
   0x10BF [0x2D1F]
   0x10C0 [0x2D20]
   0x10C1 [0x2D21]
   0x10C2 [0x2D22]
   0x10C3 [0x2D23]
   0x10C4 [0x2D24]
   0x10C5 [0x2D25]
   0x10C7 [0x2D27]
   0x10CD [0x2D2D]
   0x13F8 [0x13F0]
   0x13F9 [0x13F1]
   0x13FA [0x13F2]
   0x13FB [0x13F3]
   0x13FC [0x13F4]
   0x13FD [0x13F5]
   0x1C80 [0x0432]
   0x1C81 [0x0434]
   0x1C82 [0x043E]
   0x1C83 [0x0441]
   0x1C84 [0x0442]
   0x1C85 [0x0442]
   0x1C86 [0x044A]
   0x1C87 [0x0463]
   0x1C88 [0xA64B]
   0x1C89 [0x1C8A]
   0x1C90 [0x10D0]
   0x1C91 [0x10D1]
   0x1C92 [0x10D2]
   0x1C93 [0x10D3]
   0x1C94 [0x10D4]
   0x1C95 [0x10D5]
   0x1C96 [0x10D6]
   0x1C97 [0x10D7]
   0x1C98 [0x10D8]
   0x1C99 [0x10D9]
   0x1C9A [0x10DA]
   0x1C9B [0x10DB]
   0x1C9C [0x10DC]
   0x1C9D [0x10DD]
   0x1C9E [0x10DE]
   0x1C9F [0x10DF]
   0x1CA0 [0x10E0]
   0x1CA1 [0x10E1]
   0x1CA2 [0x10E2]
   0x1CA3 [0x10E3]
   0x1CA4 [0x10E4]
   0x1CA5 [0x10E5]
   0x1CA6 [0x10E6]
   0x1CA7 [0x10E7]
   0x1CA8 [0x10E8]
   0x1CA9 [0x10E9]
   0x1CAA [0x10EA]
   0x1CAB [0x10EB]
   0x1CAC [0x10EC]
   0x1CAD [0x10ED]
   0x1CAE [0x10EE]
   0x1CAF [0x10EF]
   0x1CB0 [0x10F0]
   0x1CB1 [0x10F1]
   0x1CB2 [0x10F2]
   0x1CB3 [0x10F3]
   0x1CB4 [0x10F4]
   0x1CB5 [0x10F5]
   0x1CB6 [0x10F6]
   0x1CB7 [0x10F7]
   0x1CB8 [0x10F8]
   0x1CB9 [0x10F9]
   0x1CBA [0x10FA]
   0x1CBD [0x10FD]
   0x1CBE [0x10FE]
   0x1CBF [0x10FF]
   0x1E00 [0x1E01]
   0x1E02 [0x1E03]
   0x1E04 [0x1E05]
   0x1E06 [0x1E07]
   0x1E08 [0x1E09]
   0x1E0A [0x1E0B]
   0x1E0C [0x1E0D]
   0x1E0E [0x1E0F]
   0x1E10 [0x1E11]
   0x1E12 [0x1E13]
   0x1E14 [0x1E15]
   0x1E16 [0x1E17]
   0x1E18 [0x1E19]
   0x1E1A [0x1E1B]
   0x1E1C [0x1E1D]
   0x1E1E [0x1E1F]
   0x1E20 [0x1E21]
   0x1E22 [0x1E23]
   0x1E24 [0x1E25]
   0x1E26 [0x1E27]
   0x1E28 [0x1E29]
   0x1E2A [0x1E2B]
   0x1E2C [0x1E2D]
   0x1E2E [0x1E2F]
   0x1E30 [0x1E31]
   0x1E32 [0x1E33]
   0x1E34 [0x1E35]
   0x1E36 [0x1E37]
   0x1E38 [0x1E39]
   0x1E3A [0x1E3B]
   0x1E3C [0x1E3D]
   0x1E3E [0x1E3F]
   0x1E40 [0x1E41]
   0x1E42 [0x1E43]
   0x1E44 [0x1E45]
   0x1E46 [0x1E47]
   0x1E48 [0x1E49]
   0x1E4A [0x1E4B]
   0x1E4C [0x1E4D]
   0x1E4E [0x1E4F]
   0x1E50 [0x1E51]
   0x1E52 [0x1E53]
   0x1E54 [0x1E55]
   0x1E56 [0x1E57]
   0x1E58 [0x1E59]
   0x1E5A [0x1E5B]
   0x1E5C [0x1E5D]
   0x1E5E [0x1E5F]
   0x1E60 [0x1E61]
   0x1E62 [0x1E63]
   0x1E64 [0x1E65]
   0x1E66 [0x1E67]
   0x1E68 [0x1E69]
   0x1E6A [0x1E6B]
   0x1E6C [0x1E6D]
   0x1E6E [0x1E6F]
   0x1E70 [0x1E71]
   0x1E72 [0x1E73]
   0x1E74 [0x1E75]
   0x1E76 [0x1E77]
   0x1E78 [0x1E79]
   0x1E7A [0x1E7B]
   0x1E7C [0x1E7D]
   0x1E7E [0x1E7F]
   0x1E80 [0x1E81]
   0x1E82 [0x1E83]
   0x1E84 [0x1E85]
   0x1E86 [0x1E87]
   0x1E88 [0x1E89]
   0x1E8A [0x1E8B]
   0x1E8C [0x1E8D]
   0x1E8E [0x1E8F]
   0x1E90 [0x1E91]
   0x1E92 [0x1E93]
   0x1E94 [0x1E95]
   0x1E96 [0x0068 0x0331]
   0x1E97 [0x0074 0x0308]
   0x1E98 [0x0077 0x030A]
   0x1E99 [0x0079 0x030A]
   0x1E9A [0x0061 0x02BE]
   0x1E9B [0x1E61]
   0x1E9E [0x0073 0x0073]
   0x1EA0 [0x1EA1]
   0x1EA2 [0x1EA3]
   0x1EA4 [0x1EA5]
   0x1EA6 [0x1EA7]
   0x1EA8 [0x1EA9]
   0x1EAA [0x1EAB]
   0x1EAC [0x1EAD]
   0x1EAE [0x1EAF]
   0x1EB0 [0x1EB1]
   0x1EB2 [0x1EB3]
   0x1EB4 [0x1EB5]
   0x1EB6 [0x1EB7]
   0x1EB8 [0x1EB9]
   0x1EBA [0x1EBB]
   0x1EBC [0x1EBD]
   0x1EBE [0x1EBF]
   0x1EC0 [0x1EC1]
   0x1EC2 [0x1EC3]
   0x1EC4 [0x1EC5]
   0x1EC6 [0x1EC7]
   0x1EC8 [0x1EC9]
   0x1ECA [0x1ECB]
   0x1ECC [0x1ECD]
   0x1ECE [0x1ECF]
   0x1ED0 [0x1ED1]
   0x1ED2 [0x1ED3]
   0x1ED4 [0x1ED5]
   0x1ED6 [0x1ED7]
   0x1ED8 [0x1ED9]
   0x1EDA [0x1EDB]
   0x1EDC [0x1EDD]
   0x1EDE [0x1EDF]
   0x1EE0 [0x1EE1]
   0x1EE2 [0x1EE3]
   0x1EE4 [0x1EE5]
   0x1EE6 [0x1EE7]
   0x1EE8 [0x1EE9]
   0x1EEA [0x1EEB]
   0x1EEC [0x1EED]
   0x1EEE [0x1EEF]
   0x1EF0 [0x1EF1]
   0x1EF2 [0x1EF3]
   0x1EF4 [0x1EF5]
   0x1EF6 [0x1EF7]
   0x1EF8 [0x1EF9]
   0x1EFA [0x1EFB]
   0x1EFC [0x1EFD]
   0x1EFE [0x1EFF]
   0x1F08 [0x1F00]
   0x1F09 [0x1F01]
   0x1F0A [0x1F02]
   0x1F0B [0x1F03]
   0x1F0C [0x1F04]
   0x1F0D [0x1F05]
   0x1F0E [0x1F06]
   0x1F0F [0x1F07]
   0x1F18 [0x1F10]
   0x1F19 [0x1F11]
   0x1F1A [0x1F12]
   0x1F1B [0x1F13]
   0x1F1C [0x1F14]
   0x1F1D [0x1F15]
   0x1F28 [0x1F20]
   0x1F29 [0x1F21]
   0x1F2A [0x1F22]
   0x1F2B [0x1F23]
   0x1F2C [0x1F24]
   0x1F2D [0x1F25]
   0x1F2E [0x1F26]
   0x1F2F [0x1F27]
   0x1F38 [0x1F30]
   0x1F39 [0x1F31]
   0x1F3A [0x1F32]
   0x1F3B [0x1F33]
   0x1F3C [0x1F34]
   0x1F3D [0x1F35]
   0x1F3E [0x1F36]
   0x1F3F [0x1F37]
   0x1F48 [0x1F40]
   0x1F49 [0x1F41]
   0x1F4A [0x1F42]
   0x1F4B [0x1F43]
   0x1F4C [0x1F44]
   0x1F4D [0x1F45]
   0x1F50 [0x03C5 0x0313]
   0x1F52 [0x03C5 0x0313 0x0300]
   0x1F54 [0x03C5 0x0313 0x0301]
   0x1F56 [0x03C5 0x0313 0x0342]
   0x1F59 [0x1F51]
   0x1F5B [0x1F53]
   0x1F5D [0x1F55]
   0x1F5F [0x1F57]
   0x1F68 [0x1F60]
   0x1F69 [0x1F61]
   0x1F6A [0x1F62]
   0x1F6B [0x1F63]
   0x1F6C [0x1F64]
   0x1F6D [0x1F65]
   0x1F6E [0x1F66]
   0x1F6F [0x1F67]
   0x1F80 [0x1F00 0x03B9]
   0x1F81 [0x1F01 0x03B9]
   0x1F82 [0x1F02 0x03B9]
   0x1F83 [0x1F03 0x03B9]
   0x1F84 [0x1F04 0x03B9]
   0x1F85 [0x1F05 0x03B9]
   0x1F86 [0x1F06 0x03B9]
   0x1F87 [0x1F07 0x03B9]
   0x1F88 [0x1F00 0x03B9]
   0x1F89 [0x1F01 0x03B9]
   0x1F8A [0x1F02 0x03B9]
   0x1F8B [0x1F03 0x03B9]
   0x1F8C [0x1F04 0x03B9]
   0x1F8D [0x1F05 0x03B9]
   0x1F8E [0x1F06 0x03B9]
   0x1F8F [0x1F07 0x03B9]
   0x1F90 [0x1F20 0x03B9]
   0x1F91 [0x1F21 0x03B9]
   0x1F92 [0x1F22 0x03B9]
   0x1F93 [0x1F23 0x03B9]
   0x1F94 [0x1F24 0x03B9]
   0x1F95 [0x1F25 0x03B9]
   0x1F96 [0x1F26 0x03B9]
   0x1F97 [0x1F27 0x03B9]
   0x1F98 [0x1F20 0x03B9]
   0x1F99 [0x1F21 0x03B9]
   0x1F9A [0x1F22 0x03B9]
   0x1F9B [0x1F23 0x03B9]
   0x1F9C [0x1F24 0x03B9]
   0x1F9D [0x1F25 0x03B9]
   0x1F9E [0x1F26 0x03B9]
   0x1F9F [0x1F27 0x03B9]
   0x1FA0 [0x1F60 0x03B9]
   0x1FA1 [0x1F61 0x03B9]
   0x1FA2 [0x1F62 0x03B9]
   0x1FA3 [0x1F63 0x03B9]
   0x1FA4 [0x1F64 0x03B9]
   0x1FA5 [0x1F65 0x03B9]
   0x1FA6 [0x1F66 0x03B9]
   0x1FA7 [0x1F67 0x03B9]
   0x1FA8 [0x1F60 0x03B9]
   0x1FA9 [0x1F61 0x03B9]
   0x1FAA [0x1F62 0x03B9]
   0x1FAB [0x1F63 0x03B9]
   0x1FAC [0x1F64 0x03B9]
   0x1FAD [0x1F65 0x03B9]
   0x1FAE [0x1F66 0x03B9]
   0x1FAF [0x1F67 0x03B9]
   0x1FB2 [0x1F70 0x03B9]
   0x1FB3 [0x03B1 0x03B9]
   0x1FB4 [0x03AC 0x03B9]
   0x1FB6 [0x03B1 0x0342]
   0x1FB7 [0x03B1 0x0342 0x03B9]
   0x1FB8 [0x1FB0]
   0x1FB9 [0x1FB1]
   0x1FBA [0x1F70]
   0x1FBB [0x1F71]
   0x1FBC [0x03B1 0x03B9]
   0x1FBE [0x03B9]
   0x1FC2 [0x1F74 0x03B9]
   0x1FC3 [0x03B7 0x03B9]
   0x1FC4 [0x03AE 0x03B9]
   0x1FC6 [0x03B7 0x0342]
   0x1FC7 [0x03B7 0x0342 0x03B9]
   0x1FC8 [0x1F72]
   0x1FC9 [0x1F73]
   0x1FCA [0x1F74]
   0x1FCB [0x1F75]
   0x1FCC [0x03B7 0x03B9]
   0x1FD2 [0x03B9 0x0308 0x0300]
   0x1FD3 [0x03B9 0x0308 0x0301]
   0x1FD6 [0x03B9 0x0342]
   0x1FD7 [0x03B9 0x0308 0x0342]
   0x1FD8 [0x1FD0]
   0x1FD9 [0x1FD1]
   0x1FDA [0x1F76]
   0x1FDB [0x1F77]
   0x1FE2 [0x03C5 0x0308 0x0300]
   0x1FE3 [0x03C5 0x0308 0x0301]
   0x1FE4 [0x03C1 0x0313]
   0x1FE6 [0x03C5 0x0342]
   0x1FE7 [0x03C5 0x0308 0x0342]
   0x1FE8 [0x1FE0]
   0x1FE9 [0x1FE1]
   0x1FEA [0x1F7A]
   0x1FEB [0x1F7B]
   0x1FEC [0x1FE5]
   0x1FF2 [0x1F7C 0x03B9]
   0x1FF3 [0x03C9 0x03B9]
   0x1FF4 [0x03CE 0x03B9]
   0x1FF6 [0x03C9 0x0342]
   0x1FF7 [0x03C9 0x0342 0x03B9]
   0x1FF8 [0x1F78]
   0x1FF9 [0x1F79]
   0x1FFA [0x1F7C]
   0x1FFB [0x1F7D]
   0x1FFC [0x03C9 0x03B9]
   0x2126 [0x03C9]
   0x212A [0x006B]
   0x212B [0x00E5]
   0x2132 [0x214E]
   0x2160 [0x2170]
   0x2161 [0x2171]
   0x2162 [0x2172]
   0x2163 [0x2173]
   0x2164 [0x2174]
   0x2165 [0x2175]
   0x2166 [0x2176]
   0x2167 [0x2177]
   0x2168 [0x2178]
   0x2169 [0x2179]
   0x216A [0x217A]
   0x216B [0x217B]
   0x216C [0x217C]
   0x216D [0x217D]
   0x216E [0x217E]
   0x216F [0x217F]
   0x2183 [0x2184]
   0x24B6 [0x24D0]
   0x24B7 [0x24D1]
   0x24B8 [0x24D2]
   0x24B9 [0x24D3]
   0x24BA [0x24D4]
   0x24BB [0x24D5]
   0x24BC [0x24D6]
   0x24BD [0x24D7]
   0x24BE [0x24D8]
   0x24BF [0x24D9]
   0x24C0 [0x24DA]
   0x24C1 [0x24DB]
   0x24C2 [0x24DC]
   0x24C3 [0x24DD]
   0x24C4 [0x24DE]
   0x24C5 [0x24DF]
   0x24C6 [0x24E0]
   0x24C7 [0x24E1]
   0x24C8 [0x24E2]
   0x24C9 [0x24E3]
   0x24CA [0x24E4]
   0x24CB [0x24E5]
   0x24CC [0x24E6]
   0x24CD [0x24E7]
   0x24CE [0x24E8]
   0x24CF [0x24E9]
   0x2C00 [0x2C30]
   0x2C01 [0x2C31]
   0x2C02 [0x2C32]
   0x2C03 [0x2C33]
   0x2C04 [0x2C34]
   0x2C05 [0x2C35]
   0x2C06 [0x2C36]
   0x2C07 [0x2C37]
   0x2C08 [0x2C38]
   0x2C09 [0x2C39]
   0x2C0A [0x2C3A]
   0x2C0B [0x2C3B]
   0x2C0C [0x2C3C]
   0x2C0D [0x2C3D]
   0x2C0E [0x2C3E]
   0x2C0F [0x2C3F]
   0x2C10 [0x2C40]
   0x2C11 [0x2C41]
   0x2C12 [0x2C42]
   0x2C13 [0x2C43]
   0x2C14 [0x2C44]
   0x2C15 [0x2C45]
   0x2C16 [0x2C46]
   0x2C17 [0x2C47]
   0x2C18 [0x2C48]
   0x2C19 [0x2C49]
   0x2C1A [0x2C4A]
   0x2C1B [0x2C4B]
   0x2C1C [0x2C4C]
   0x2C1D [0x2C4D]
   0x2C1E [0x2C4E]
   0x2C1F [0x2C4F]
   0x2C20 [0x2C50]
   0x2C21 [0x2C51]
   0x2C22 [0x2C52]
   0x2C23 [0x2C53]
   0x2C24 [0x2C54]
   0x2C25 [0x2C55]
   0x2C26 [0x2C56]
   0x2C27 [0x2C57]
   0x2C28 [0x2C58]
   0x2C29 [0x2C59]
   0x2C2A [0x2C5A]
   0x2C2B [0x2C5B]
   0x2C2C [0x2C5C]
   0x2C2D [0x2C5D]
   0x2C2E [0x2C5E]
   0x2C2F [0x2C5F]
   0x2C60 [0x2C61]
   0x2C62 [0x026B]
   0x2C63 [0x1D7D]
   0x2C64 [0x027D]
   0x2C67 [0x2C68]
   0x2C69 [0x2C6A]
   0x2C6B [0x2C6C]
   0x2C6D [0x0251]
   0x2C6E [0x0271]
   0x2C6F [0x0250]
   0x2C70 [0x0252]
   0x2C72 [0x2C73]
   0x2C75 [0x2C76]
   0x2C7E [0x023F]
   0x2C7F [0x0240]
   0x2C80 [0x2C81]
   0x2C82 [0x2C83]
   0x2C84 [0x2C85]
   0x2C86 [0x2C87]
   0x2C88 [0x2C89]
   0x2C8A [0x2C8B]
   0x2C8C [0x2C8D]
   0x2C8E [0x2C8F]
   0x2C90 [0x2C91]
   0x2C92 [0x2C93]
   0x2C94 [0x2C95]
   0x2C96 [0x2C97]
   0x2C98 [0x2C99]
   0x2C9A [0x2C9B]
   0x2C9C [0x2C9D]
   0x2C9E [0x2C9F]
   0x2CA0 [0x2CA1]
   0x2CA2 [0x2CA3]
   0x2CA4 [0x2CA5]
   0x2CA6 [0x2CA7]
   0x2CA8 [0x2CA9]
   0x2CAA [0x2CAB]
   0x2CAC [0x2CAD]
   0x2CAE [0x2CAF]
   0x2CB0 [0x2CB1]
   0x2CB2 [0x2CB3]
   0x2CB4 [0x2CB5]
   0x2CB6 [0x2CB7]
   0x2CB8 [0x2CB9]
   0x2CBA [0x2CBB]
   0x2CBC [0x2CBD]
   0x2CBE [0x2CBF]
   0x2CC0 [0x2CC1]
   0x2CC2 [0x2CC3]
   0x2CC4 [0x2CC5]
   0x2CC6 [0x2CC7]
   0x2CC8 [0x2CC9]
   0x2CCA [0x2CCB]
   0x2CCC [0x2CCD]
   0x2CCE [0x2CCF]
   0x2CD0 [0x2CD1]
   0x2CD2 [0x2CD3]
   0x2CD4 [0x2CD5]
   0x2CD6 [0x2CD7]
   0x2CD8 [0x2CD9]
   0x2CDA [0x2CDB]
   0x2CDC [0x2CDD]
   0x2CDE [0x2CDF]
   0x2CE0 [0x2CE1]
   0x2CE2 [0x2CE3]
   0x2CEB [0x2CEC]
   0x2CED [0x2CEE]
   0x2CF2 [0x2CF3]
   0xA640 [0xA641]
   0xA642 [0xA643]
   0xA644 [0xA645]
   0xA646 [0xA647]
   0xA648 [0xA649]
   0xA64A [0xA64B]
   0xA64C [0xA64D]
   0xA64E [0xA64F]
   0xA650 [0xA651]
   0xA652 [0xA653]
   0xA654 [0xA655]
   0xA656 [0xA657]
   0xA658 [0xA659]
   0xA65A [0xA65B]
   0xA65C [0xA65D]
   0xA65E [0xA65F]
   0xA660 [0xA661]
   0xA662 [0xA663]
   0xA664 [0xA665]
   0xA666 [0xA667]
   0xA668 [0xA669]
   0xA66A [0xA66B]
   0xA66C [0xA66D]
   0xA680 [0xA681]
   0xA682 [0xA683]
   0xA684 [0xA685]
   0xA686 [0xA687]
   0xA688 [0xA689]
   0xA68A [0xA68B]
   0xA68C [0xA68D]
   0xA68E [0xA68F]
   0xA690 [0xA691]
   0xA692 [0xA693]
   0xA694 [0xA695]
   0xA696 [0xA697]
   0xA698 [0xA699]
   0xA69A [0xA69B]
   0xA722 [0xA723]
   0xA724 [0xA725]
   0xA726 [0xA727]
   0xA728 [0xA729]
   0xA72A [0xA72B]
   0xA72C [0xA72D]
   0xA72E [0xA72F]
   0xA732 [0xA733]
   0xA734 [0xA735]
   0xA736 [0xA737]
   0xA738 [0xA739]
   0xA73A [0xA73B]
   0xA73C [0xA73D]
   0xA73E [0xA73F]
   0xA740 [0xA741]
   0xA742 [0xA743]
   0xA744 [0xA745]
   0xA746 [0xA747]
   0xA748 [0xA749]
   0xA74A [0xA74B]
   0xA74C [0xA74D]
   0xA74E [0xA74F]
   0xA750 [0xA751]
   0xA752 [0xA753]
   0xA754 [0xA755]
   0xA756 [0xA757]
   0xA758 [0xA759]
   0xA75A [0xA75B]
   0xA75C [0xA75D]
   0xA75E [0xA75F]
   0xA760 [0xA761]
   0xA762 [0xA763]
   0xA764 [0xA765]
   0xA766 [0xA767]
   0xA768 [0xA769]
   0xA76A [0xA76B]
   0xA76C [0xA76D]
   0xA76E [0xA76F]
   0xA779 [0xA77A]
   0xA77B [0xA77C]
   0xA77D [0x1D79]
   0xA77E [0xA77F]
   0xA780 [0xA781]
   0xA782 [0xA783]
   0xA784 [0xA785]
   0xA786 [0xA787]
   0xA78B [0xA78C]
   0xA78D [0x0265]
   0xA790 [0xA791]
   0xA792 [0xA793]
   0xA796 [0xA797]
   0xA798 [0xA799]
   0xA79A [0xA79B]
   0xA79C [0xA79D]
   0xA79E [0xA79F]
   0xA7A0 [0xA7A1]
   0xA7A2 [0xA7A3]
   0xA7A4 [0xA7A5]
   0xA7A6 [0xA7A7]
   0xA7A8 [0xA7A9]
   0xA7AA [0x0266]
   0xA7AB [0x025C]
   0xA7AC [0x0261]
   0xA7AD [0x026C]
   0xA7AE [0x026A]
   0xA7B0 [0x029E]
   0xA7B1 [0x0287]
   0xA7B2 [0x029D]
   0xA7B3 [0xAB53]
   0xA7B4 [0xA7B5]
   0xA7B6 [0xA7B7]
   0xA7B8 [0xA7B9]
   0xA7BA [0xA7BB]
   0xA7BC [0xA7BD]
   0xA7BE [0xA7BF]
   0xA7C0 [0xA7C1]
   0xA7C2 [0xA7C3]
   0xA7C4 [0xA794]
   0xA7C5 [0x0282]
   0xA7C6 [0x1D8E]
   0xA7C7 [0xA7C8]
   0xA7C9 [0xA7CA]
   0xA7CB [0x0264]
   0xA7CC [0xA7CD]
   0xA7CE [0xA7CF]
   0xA7D0 [0xA7D1]
   0xA7D2 [0xA7D3]
   0xA7D4 [0xA7D5]
   0xA7D6 [0xA7D7]
   0xA7D8 [0xA7D9]
   0xA7DA [0xA7DB]
   0xA7DC [0x019B]
   0xA7F5 [0xA7F6]
   0xAB70 [0x13A0]
   0xAB71 [0x13A1]
   0xAB72 [0x13A2]
   0xAB73 [0x13A3]
   0xAB74 [0x13A4]
   0xAB75 [0x13A5]
   0xAB76 [0x13A6]
   0xAB77 [0x13A7]
   0xAB78 [0x13A8]
   0xAB79 [0x13A9]
   0xAB7A [0x13AA]
   0xAB7B [0x13AB]
   0xAB7C [0x13AC]
   0xAB7D [0x13AD]
   0xAB7E [0x13AE]
   0xAB7F [0x13AF]
   0xAB80 [0x13B0]
   0xAB81 [0x13B1]
   0xAB82 [0x13B2]
   0xAB83 [0x13B3]
   0xAB84 [0x13B4]
   0xAB85 [0x13B5]
   0xAB86 [0x13B6]
   0xAB87 [0x13B7]
   0xAB88 [0x13B8]
   0xAB89 [0x13B9]
   0xAB8A [0x13BA]
   0xAB8B [0x13BB]
   0xAB8C [0x13BC]
   0xAB8D [0x13BD]
   0xAB8E [0x13BE]
   0xAB8F [0x13BF]
   0xAB90 [0x13C0]
   0xAB91 [0x13C1]
   0xAB92 [0x13C2]
   0xAB93 [0x13C3]
   0xAB94 [0x13C4]
   0xAB95 [0x13C5]
   0xAB96 [0x13C6]
   0xAB97 [0x13C7]
   0xAB98 [0x13C8]
   0xAB99 [0x13C9]
   0xAB9A [0x13CA]
   0xAB9B [0x13CB]
   0xAB9C [0x13CC]
   0xAB9D [0x13CD]
   0xAB9E [0x13CE]
   0xAB9F [0x13CF]
   0xABA0 [0x13D0]
   0xABA1 [0x13D1]
   0xABA2 [0x13D2]
   0xABA3 [0x13D3]
   0xABA4 [0x13D4]
   0xABA5 [0x13D5]
   0xABA6 [0x13D6]
   0xABA7 [0x13D7]
   0xABA8 [0x13D8]
   0xABA9 [0x13D9]
   0xABAA [0x13DA]
   0xABAB [0x13DB]
   0xABAC [0x13DC]
   0xABAD [0x13DD]
   0xABAE [0x13DE]
   0xABAF [0x13DF]
   0xABB0 [0x13E0]
   0xABB1 [0x13E1]
   0xABB2 [0x13E2]
   0xABB3 [0x13E3]
   0xABB4 [0x13E4]
   0xABB5 [0x13E5]
   0xABB6 [0x13E6]
   0xABB7 [0x13E7]
   0xABB8 [0x13E8]
   0xABB9 [0x13E9]
   0xABBA [0x13EA]
   0xABBB [0x13EB]
   0xABBC [0x13EC]
   0xABBD [0x13ED]
   0xABBE [0x13EE]
   0xABBF [0x13EF]
   0xFB00 [0x0066 0x0066]
   0xFB01 [0x0066 0x0069]
   0xFB02 [0x0066 0x006C]
   0xFB03 [0x0066 0x0066 0x0069]
   0xFB04 [0x0066 0x0066 0x006C]
   0xFB05 [0x0073 0x0074]
   0xFB06 [0x0073 0x0074]
   0xFB13 [0x0574 0x0576]
   0xFB14 [0x0574 0x0565]
   0xFB15 [0x0574 0x056B]
   0xFB16 [0x057E 0x0576]
   0xFB17 [0x0574 0x056D]
   0xFF21 [0xFF41]
   0xFF22 [0xFF42]
   0xFF23 [0xFF43]
   0xFF24 [0xFF44]
   0xFF25 [0xFF45]
   0xFF26 [0xFF46]
   0xFF27 [0xFF47]
   0xFF28 [0xFF48]
   0xFF29 [0xFF49]
   0xFF2A [0xFF4A]
   0xFF2B [0xFF4B]
   0xFF2C [0xFF4C]
   0xFF2D [0xFF4D]
   0xFF2E [0xFF4E]
   0xFF2F [0xFF4F]
   0xFF30 [0xFF50]
   0xFF31 [0xFF51]
   0xFF32 [0xFF52]
   0xFF33 [0xFF53]
   0xFF34 [0xFF54]
   0xFF35 [0xFF55]
   0xFF36 [0xFF56]
   0xFF37 [0xFF57]
   0xFF38 [0xFF58]
   0xFF39 [0xFF59]
   0xFF3A [0xFF5A]
   0x10400 [0x10428]
   0x10401 [0x10429]
   0x10402 [0x1042A]
   0x10403 [0x1042B]
   0x10404 [0x1042C]
   0x10405 [0x1042D]
   0x10406 [0x1042E]
   0x10407 [0x1042F]
   0x10408 [0x10430]
   0x10409 [0x10431]
   0x1040A [0x10432]
   0x1040B [0x10433]
   0x1040C [0x10434]
   0x1040D [0x10435]
   0x1040E [0x10436]
   0x1040F [0x10437]
   0x10410 [0x10438]
   0x10411 [0x10439]
   0x10412 [0x1043A]
   0x10413 [0x1043B]
   0x10414 [0x1043C]
   0x10415 [0x1043D]
   0x10416 [0x1043E]
   0x10417 [0x1043F]
   0x10418 [0x10440]
   0x10419 [0x10441]
   0x1041A [0x10442]
   0x1041B [0x10443]
   0x1041C [0x10444]
   0x1041D [0x10445]
   0x1041E [0x10446]
   0x1041F [0x10447]
   0x10420 [0x10448]
   0x10421 [0x10449]
   0x10422 [0x1044A]
   0x10423 [0x1044B]
   0x10424 [0x1044C]
   0x10425 [0x1044D]
   0x10426 [0x1044E]
   0x10427 [0x1044F]
   0x104B0 [0x104D8]
   0x104B1 [0x104D9]
   0x104B2 [0x104DA]
   0x104B3 [0x104DB]
   0x104B4 [0x104DC]
   0x104B5 [0x104DD]
   0x104B6 [0x104DE]
   0x104B7 [0x104DF]
   0x104B8 [0x104E0]
   0x104B9 [0x104E1]
   0x104BA [0x104E2]
   0x104BB [0x104E3]
   0x104BC [0x104E4]
   0x104BD [0x104E5]
   0x104BE [0x104E6]
   0x104BF [0x104E7]
   0x104C0 [0x104E8]
   0x104C1 [0x104E9]
   0x104C2 [0x104EA]
   0x104C3 [0x104EB]
   0x104C4 [0x104EC]
   0x104C5 [0x104ED]
   0x104C6 [0x104EE]
   0x104C7 [0x104EF]
   0x104C8 [0x104F0]
   0x104C9 [0x104F1]
   0x104CA [0x104F2]
   0x104CB [0x104F3]
   0x104CC [0x104F4]
   0x104CD [0x104F5]
   0x104CE [0x104F6]
   0x104CF [0x104F7]
   0x104D0 [0x104F8]
   0x104D1 [0x104F9]
   0x104D2 [0x104FA]
   0x104D3 [0x104FB]
   0x10570 [0x10597]
   0x10571 [0x10598]
   0x10572 [0x10599]
   0x10573 [0x1059A]
   0x10574 [0x1059B]
   0x10575 [0x1059C]
   0x10576 [0x1059D]
   0x10577 [0x1059E]
   0x10578 [0x1059F]
   0x10579 [0x105A0]
   0x1057A [0x105A1]
   0x1057C [0x105A3]
   0x1057D [0x105A4]
   0x1057E [0x105A5]
   0x1057F [0x105A6]
   0x10580 [0x105A7]
   0x10581 [0x105A8]
   0x10582 [0x105A9]
   0x10583 [0x105AA]
   0x10584 [0x105AB]
   0x10585 [0x105AC]
   0x10586 [0x105AD]
   0x10587 [0x105AE]
   0x10588 [0x105AF]
   0x10589 [0x105B0]
   0x1058A [0x105B1]
   0x1058C [0x105B3]
   0x1058D [0x105B4]
   0x1058E [0x105B5]
   0x1058F [0x105B6]
   0x10590 [0x105B7]
   0x10591 [0x105B8]
   0x10592 [0x105B9]
   0x10594 [0x105BB]
   0x10595 [0x105BC]
   0x10C80 [0x10CC0]
   0x10C81 [0x10CC1]
   0x10C82 [0x10CC2]
   0x10C83 [0x10CC3]
   0x10C84 [0x10CC4]
   0x10C85 [0x10CC5]
   0x10C86 [0x10CC6]
   0x10C87 [0x10CC7]
   0x10C88 [0x10CC8]
   0x10C89 [0x10CC9]
   0x10C8A [0x10CCA]
   0x10C8B [0x10CCB]
   0x10C8C [0x10CCC]
   0x10C8D [0x10CCD]
   0x10C8E [0x10CCE]
   0x10C8F [0x10CCF]
   0x10C90 [0x10CD0]
   0x10C91 [0x10CD1]
   0x10C92 [0x10CD2]
   0x10C93 [0x10CD3]
   0x10C94 [0x10CD4]
   0x10C95 [0x10CD5]
   0x10C96 [0x10CD6]
   0x10C97 [0x10CD7]
   0x10C98 [0x10CD8]
   0x10C99 [0x10CD9]
   0x10C9A [0x10CDA]
   0x10C9B [0x10CDB]
   0x10C9C [0x10CDC]
   0x10C9D [0x10CDD]
   0x10C9E [0x10CDE]
   0x10C9F [0x10CDF]
   0x10CA0 [0x10CE0]
   0x10CA1 [0x10CE1]
   0x10CA2 [0x10CE2]
   0x10CA3 [0x10CE3]
   0x10CA4 [0x10CE4]
   0x10CA5 [0x10CE5]
   0x10CA6 [0x10CE6]
   0x10CA7 [0x10CE7]
   0x10CA8 [0x10CE8]
   0x10CA9 [0x10CE9]
   0x10CAA [0x10CEA]
   0x10CAB [0x10CEB]
   0x10CAC [0x10CEC]
   0x10CAD [0x10CED]
   0x10CAE [0x10CEE]
   0x10CAF [0x10CEF]
   0x10CB0 [0x10CF0]
   0x10CB1 [0x10CF1]
   0x10CB2 [0x10CF2]
   0x10D50 [0x10D70]
   0x10D51 [0x10D71]
   0x10D52 [0x10D72]
   0x10D53 [0x10D73]
   0x10D54 [0x10D74]
   0x10D55 [0x10D75]
   0x10D56 [0x10D76]
   0x10D57 [0x10D77]
   0x10D58 [0x10D78]
   0x10D59 [0x10D79]
   0x10D5A [0x10D7A]
   0x10D5B [0x10D7B]
   0x10D5C [0x10D7C]
   0x10D5D [0x10D7D]
   0x10D5E [0x10D7E]
   0x10D5F [0x10D7F]
   0x10D60 [0x10D80]
   0x10D61 [0x10D81]
   0x10D62 [0x10D82]
   0x10D63 [0x10D83]
   0x10D64 [0x10D84]
   0x10D65 [0x10D85]
   0x118A0 [0x118C0]
   0x118A1 [0x118C1]
   0x118A2 [0x118C2]
   0x118A3 [0x118C3]
   0x118A4 [0x118C4]
   0x118A5 [0x118C5]
   0x118A6 [0x118C6]
   0x118A7 [0x118C7]
   0x118A8 [0x118C8]
   0x118A9 [0x118C9]
   0x118AA [0x118CA]
   0x118AB [0x118CB]
   0x118AC [0x118CC]
   0x118AD [0x118CD]
   0x118AE [0x118CE]
   0x118AF [0x118CF]
   0x118B0 [0x118D0]
   0x118B1 [0x118D1]
   0x118B2 [0x118D2]
   0x118B3 [0x118D3]
   0x118B4 [0x118D4]
   0x118B5 [0x118D5]
   0x118B6 [0x118D6]
   0x118B7 [0x118D7]
   0x118B8 [0x118D8]
   0x118B9 [0x118D9]
   0x118BA [0x118DA]
   0x118BB [0x118DB]
   0x118BC [0x118DC]
   0x118BD [0x118DD]
   0x118BE [0x118DE]
   0x118BF [0x118DF]
   0x16E40 [0x16E60]
   0x16E41 [0x16E61]
   0x16E42 [0x16E62]
   0x16E43 [0x16E63]
   0x16E44 [0x16E64]
   0x16E45 [0x16E65]
   0x16E46 [0x16E66]
   0x16E47 [0x16E67]
   0x16E48 [0x16E68]
   0x16E49 [0x16E69]
   0x16E4A [0x16E6A]
   0x16E4B [0x16E6B]
   0x16E4C [0x16E6C]
   0x16E4D [0x16E6D]
   0x16E4E [0x16E6E]
   0x16E4F [0x16E6F]
   0x16E50 [0x16E70]
   0x16E51 [0x16E71]
   0x16E52 [0x16E72]
   0x16E53 [0x16E73]
   0x16E54 [0x16E74]
   0x16E55 [0x16E75]
   0x16E56 [0x16E76]
   0x16E57 [0x16E77]
   0x16E58 [0x16E78]
   0x16E59 [0x16E79]
   0x16E5A [0x16E7A]
   0x16E5B [0x16E7B]
   0x16E5C [0x16E7C]
   0x16E5D [0x16E7D]
   0x16E5E [0x16E7E]
   0x16E5F [0x16E7F]
   0x16EA0 [0x16EBB]
   0x16EA1 [0x16EBC]
   0x16EA2 [0x16EBD]
   0x16EA3 [0x16EBE]
   0x16EA4 [0x16EBF]
   0x16EA5 [0x16EC0]
   0x16EA6 [0x16EC1]
   0x16EA7 [0x16EC2]
   0x16EA8 [0x16EC3]
   0x16EA9 [0x16EC4]
   0x16EAA [0x16EC5]
   0x16EAB [0x16EC6]
   0x16EAC [0x16EC7]
   0x16EAD [0x16EC8]
   0x16EAE [0x16EC9]
   0x16EAF [0x16ECA]
   0x16EB0 [0x16ECB]
   0x16EB1 [0x16ECC]
   0x16EB2 [0x16ECD]
   0x16EB3 [0x16ECE]
   0x16EB4 [0x16ECF]
   0x16EB5 [0x16ED0]
   0x16EB6 [0x16ED1]
   0x16EB7 [0x16ED2]
   0x16EB8 [0x16ED3]
   0x1E900 [0x1E922]
   0x1E901 [0x1E923]
   0x1E902 [0x1E924]
   0x1E903 [0x1E925]
   0x1E904 [0x1E926]
   0x1E905 [0x1E927]
   0x1E906 [0x1E928]
   0x1E907 [0x1E929]
   0x1E908 [0x1E92A]
   0x1E909 [0x1E92B]
   0x1E90A [0x1E92C]
   0x1E90B [0x1E92D]
   0x1E90C [0x1E92E]
   0x1E90D [0x1E92F]
   0x1E90E [0x1E930]
   0x1E90F [0x1E931]
   0x1E910 [0x1E932]
   0x1E911 [0x1E933]
   0x1E912 [0x1E934]
   0x1E913 [0x1E935]
   0x1E914 [0x1E936]
   0x1E915 [0x1E937]
   0x1E916 [0x1E938]
   0x1E917 [0x1E939]
   0x1E918 [0x1E93A]
   0x1E919 [0x1E93B]
   0x1E91A [0x1E93C]
   0x1E91B [0x1E93D]
   0x1E91C [0x1E93E]
   0x1E91D [0x1E93F]
   0x1E91E [0x1E940]
   0x1E91F [0x1E941]
   0x1E920 [0x1E942]
   0x1E921 [0x1E943]
})


(defn folding/case-fold [s]
  (def buf @"")
  (var i 0)
  (while (< i (length s))
    (def c (get s i))
    (def grapheme
      (cond
        # 1-byte variant (0xxxxxxx)
        (< c 0x80) c
        # 2-byte variant (110xxxxx 10xxxxxx)
        (< 0xBF c 0xE0) (bor (blshift (band c 0x1F) 6)
                             (band (get s (++ i)) 0x3F))
        # 3-byte variant (1110xxxx 10xxxxxx 10xxxxxx)
        (< c 0xF0) (bor (blshift (band c 0x0F) 12)
                        (blshift (band (get s (++ i)) 0x3F) 6)
                        (band (get s (++ i)) 0x3F))
        # 4-byte variant (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
        (< c 0xF8) (bor (blshift (band c 0x07) 18)
                        (blshift (band (get s (++ i)) 0x3F) 12)
                        (blshift (band (get s (++ i)) 0x3F) 6)
                        (band (get s (++ i)) 0x3F))))
    (def lowered (get folding/lower grapheme))
    (if (nil? lowered)
      (buffer/push buf grapheme)
      (buffer/push buf ;lowered))
    (++ i))
  (string buf))
(comment import ./node :prefix "")
# Node manipulation functions
#
# This module provides functions for inspecting and manipulating AST nodes.
# These are useful when writing custom block/inline protocols and renderers.

(defn node/type-of
  ```
  Gets the type of the node
  ```
  [node]
  (first node))

(defn node/attribute
  ```
  Gets or sets the attribute `attr` of `node`

  Whether this function gets or sets depends on whether `value` is provided.
  ```
  [node attr &opt value]
  (if (nil? value)
    (get (get node 1) attr)
    (put (get node 1) attr value)))

(defn node/children-of
  ```
  Gets the children of `node`
  ```
  [node]
  (last node))

(defn node/next-child
  ```
  Gets the last child of `node` if it is a container

  Returns nil if `node` is not a container.
  ```
  [node]
  (when (node/attribute node :container?)
    (last (node/children-of node))))

(defn node/next-container
  ```
  Gets the next open child of `node`

  Returns nil if the next child is not open.
  ```
  [node]
  (def child (node/next-child node))
  (when (and child (node/attribute child :open?))
    child))

(defn node/get-fn
  ```
  Gets a function called `name` associated with the type of `node`

  If there is no function called `name` associated with the type, instead gets
  the function called `name` from the default group.
  ```
  [name node functions]
  (or (get (get functions (node/type-of node)) name)
      (get (get functions 'default) name)))

(defn node/close-children
  ```
  Closes the children of `parent`
  ```
  [parent functions]
  (var prev parent)
  (while (def node (node/next-container prev))
    (def close-fn (node/get-fn :close node functions))
    (close-fn node prev)
    (set prev node)))

(defn node/last-descendant
  ```
  Gets the last descendant of `node`
  ```
  [node]
  (var curr-n (node/next-child node))
  (while (def next-n (node/next-child curr-n))
    (set curr-n next-n))
  curr-n)

(comment import ./state :prefix "")


# Private functions

(defn- util/num-cols [c &opt extra-width]
  (default extra-width 0)
  (case c
     9 (- 4 (% (+ state/col-edge extra-width) 4))
    32 1))

# State functions

(defn util/record-padding [padding]
  (each c padding
    (+= state/col-edge (util/num-cols c))))

# Selection functions

(defn util/update-col-pos
  ```
  Updates the column position in the global state

  The column position is updated based on the width of `block`. Returns `block`.
  ```
  [block]
  (when (def width (node/attribute block :width))
    (+= state/col-pos width))
  block)

# Relationship functions

(defn util/add-to
  ```
  Adds the values in `d2` to `d1` using the same keys
  ```
  [d1 d2]
  (each k (keys d2)
    (if (nil? (get d1 k))
      (put d1 k (get d2 k))
      # TOASK why is it like this?
      (each name (keys (get d2 k))
        (put-in d1 [k name] (get-in d2 [k name])))))
  d1)

# String manipulation

(defn util/dedent
  ```
  Dedents the `line` based on `start`
  ```
  [line start]
  (def len (length line))
  (var pos start)
  (var total-width 0)
  (while (< pos len)
    (def width (util/num-cols (get line pos) total-width))
    (if (nil? width)
      (break))
    (+= total-width width)
    (++ pos))
  (+= state/col-edge total-width)
  pos)

(defn util/entity-decode
  ```
  Encodes `entity` of kind `kind`
  ```
  [kind entity]
  (case kind
    :ent
    (or (entities/to-grapheme entity) entity)
    :dec
    (let [code (scan-number (string/slice entity 2 -2))]
      (if (zero? code)
        "\uFFFD"
        (parse (string/format `"\U%06x"` code))))
    :hex
    (let [code (string/slice entity 3 -2)
          len  (length code)]
      (if (string/check-set "0" code)
        "\uFFFD"
        (parse (string `"\U` (when (< len 6) (string/repeat "0" (- 6 len))) code `"`))))
    # impossible
    (errorf "kind %j not recognised" kind)))

(defn util/normalise
  ```
  Normalises whitespace in `s`
  ```
  [s]
  (def buf @"")
  (var found-space? false)
  (each c s
    (if (or (= 9 c) (= 10 c) (= 32 c))
      (set found-space? true)
      (do
        (buffer/push buf (if found-space? " " "") c)
        (set found-space? false))))
  (folding/case-fold buf))

(defn util/uri-encode
  ```
  Converts `uri` to be URL-encoded
  ```
  [uri]
  (def hex ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"])
  (defn encode-char [ch]
    (def c (get ch 0))
    (string "%" (in hex (math/floor (/ c 16))) (in hex (% c 16)))) #TODO Is there a more efficient way to do this?
  (def encode-grammar
    ~{:main      (* (% (any (+ :escaped :unescaped))) -1)
      :escaped   (+ '"%"
                    (/ '"&"  "&amp;")
                    (/ '(+ (set "<>\"|^`[]{}\\")
                           (range "\x00\x20")
                           (range "\x7F\xFF")) ,encode-char))
      :unescaped '1})
  (first (peg/match encode-grammar uri)))

# Miscellaneous

(defn util/to-continuation
  ```
  Creates a block node of `block-type` with trimmed `line` as its content
  ```
  [block-type line pos]
  (def content (-> (string/slice line pos) string/trim))
  [block-type @{} @[content]])

(defn util/words
  ```
  Converts `xs` into a grammar rule for use in a PEG
  ```
  [& xs]
  (defn word [s]
    (tuple
      ;(reduce (fn [arr c]
                 (->> ~(set ,(string/from-bytes c (- c 32)))
                      (array/push arr)))
               @['*]
               s)))
  ['+ ;(map word xs)])

(comment import ./node :prefix "")


(util/add-to state/protocols @{:blocks @{}})

(def blocks/blocks ~@[+])

(comment import ./blocks/blank :prefix "")
(comment import ../state :prefix "")


## Grammar

(defn- blank/blank []
  (when (< (- state/col-edge state/col-pos) 4)
    [:blank]))

(def blank/grammar
  ~{:blank (cmt :nl ,blank/blank)})

(comment import ./blocks/blockquote :prefix "")
(comment import ../state :prefix "")

(comment import ../util :prefix "")

(comment import ../node :prefix "")

(comment import ../container :prefix "")
(comment import ./state :prefix "")

(comment import ./util :prefix "")

(comment import ./node :prefix "")


(defn- container/blank [node parent protocols]
  (node/next-container node))

(defn- container/equal? [node block]
  (= (node/type-of node) (node/type-of block)))

(defn- container/next-block [node line pos grammar protocols]
  (peg/match grammar line pos))

(defn- container/see-blank [node protocols]
  nil)

(defn container/make-protocol
  ```
  Creates a table of standard container block protocol functions

  Container blocks (e.g. blockquotes, lists) are block-level elements that can
  contain other blocks. They share common behavior for managing child blocks.
  This function returns base implementations that can be customized via the
  optional `overrides` parameter.

  Overrides:

  ``
  :equal?     - Custom equality check (default: type equality)
  :see-blank  - Called when blank line seen (default: close container)
  :blank      - Handle blank lines (default: pass to next container)
  :next-block - Custom next block matching (default: delegate to grammar)
  :needs-nl?  - Whether container needs newline before closing
  ``

  The base implementations:

  - `equal?` Checks if node type matches block type
  - `see-blank` Closes container and all open children
  - `blank` Continues to next open container
  - `next-block` Delegates to grammar for parsing

  Example (simple container):

  ``janet
    (util/add-to state/protocols
      {:blocks {:blockquote (container/make-protocol)}})
  ``

  Example (container with custom behavior):

  ``janet
    (util/add-to state/protocols
      {:blocks {:list (container/make-protocol
                        {:equal?     list-equal?
                         :blank      list-blank
                         :next-block list-next-block})}})
  ``
  ```
  [&opt overrides]
  (default overrides {})
  @{:equal?     (get overrides :equal? container/equal?)
    :see-blank  (get overrides :see-blank container/see-blank)
    :blank      (get overrides :blank container/blank)
    :next-block (get overrides :next-block container/next-block)
    :needs-nl?  (get overrides :needs-nl?)})


(defn- blockquote/blockquote [trailing-space]
  (++ state/col-edge)
  (set state/col-pos state/col-edge)
  (util/record-padding trailing-space)
  (unless (empty? trailing-space)
    (++ state/col-pos))
  [:blockquote @{:container? true :open? true} @[]])

(def blockquote/grammar
  ~{:blockquote (/ (* ">" '(any :space)) ,blockquote/blockquote)})

(defn- blockquote/blockquote-see-blank [a-blockquote protocols]
  (when (node/attribute a-blockquote :open?)
    (node/attribute a-blockquote :open? false)
    (node/close-children a-blockquote protocols)))

(util/add-to state/protocols
  {:blocks
    {:blockquote (container/make-protocol
                   {:see-blank blockquote/blockquote-see-blank})}})

(comment import ./blocks/codeblock :prefix "")
(comment import ../state :prefix "")

(comment import ../util :prefix "")

(comment import ../node :prefix "")


## Grammar

(defn- codeblock/codeblock-f [delim fence info]
  (def num (inc (length fence)))
  (def [lang extra]
    (if (empty? info)
      [nil]
      (if (def first-space (string/find " " info))
        [(string/slice info 0 first-space) (string/slice info (inc first-space))]
        [info nil])))
  (def indent (- state/col-edge state/col-pos))
  [:codeblock @{:open? true :kind :fenced :delim delim :num num :info lang :extra extra :indent indent} @[]])

(defn- codeblock/codeblock-i [text]
  (def indent (- state/col-edge state/col-pos))
  (when (>= indent 4)
    (def extra-cols (- indent 4))
    (def extra-space (when (> extra-cols 0) (string/repeat " " extra-cols)))
    (def line (string extra-space text))
    [:codeblock @{:open? true :kind :indented} @[line]]))

(def codeblock/grammar
  ~{:codeblock {:main     (+ :indented :fenced)
                :indented {:main (cmt (* :code :eol) ,codeblock/codeblock-i)
                           :code '(thru :eol)}
                :fenced   {:main  (/ (* :fence :eol) ,codeblock/codeblock-f)
                           :fence (+ :tilde :tick)
                           :tilde (* '"~" '(at-least 2 "~") (any :space) (% (any :char)))
                           :tick  (* '"`" '(at-least 2 "`") (any :space) (% (any (if-not "`" :char))))}}})

## Functions

(defn- codeblock/codeblock-blank [a-codeblock parent protocols]
  (when (node/attribute a-codeblock :open?)
    (array/push (node/children-of a-codeblock) "\n"))
  nil)

(defn- codeblock/codeblock-close [a-codeblock &opt parent]
  (node/attribute a-codeblock :open? false)
  (when (= :indented (node/attribute a-codeblock :kind))
    (def lines (node/children-of a-codeblock))
    (def last-index (dec (length lines)))
    (var start 0)
    (var line (get lines start))
    (while (and (not (nil? line))
                (string/check-set " \t\n" line))
      (set line (get lines (++ start))))
    (var end last-index)
    (set line (get lines end))
    (while (and (not (nil? line))
                (string/check-set " \t\n" line))
      (set line (get lines (-- end))))
    (unless (and (= 0 start) (= last-index end))
      (array/remove lines (inc end) (- last-index end))
      (array/remove lines 0 start))))

(defn- codeblock/codeblock-continue [a-codeblock block]
  (def lines (node/children-of block))
  (if (= :close (first lines))
    (codeblock/codeblock-close a-codeblock)
    (array/concat (node/children-of a-codeblock) lines)))

(defn- codeblock/codeblock-equal? [a-codeblock block]
  (and (= :codeblock (node/type-of block))
       (node/attribute a-codeblock :open?)))

(defn- codeblock/codeblock-follower [a-codeblock block]
  (when (= :paragraph (node/type-of block))
    [:paragraph {} (node/children-of a-codeblock)]))

(defn- codeblock/codeblock-lazy? [a-codeblock]
  (= :indented (node/attribute a-codeblock :kind)))

(defn- codeblock/codeblock-needs-nl? [a-codeblock]
  (= :indented (node/attribute a-codeblock :kind)))

(defn- codeblock/codeblock-next-block [a-codeblock line pos codeblock/grammar protocols]
  (if (= :indented (node/attribute a-codeblock :kind))
    (peg/match codeblock/grammar line pos)
    (do
      (def delim (node/attribute a-codeblock :delim))
      (def delim-num (node/attribute a-codeblock :num))
      (def max-indent (node/attribute a-codeblock :indent))
      (defn fence []
        (when (< (- state/col-edge state/col-pos) 4)
          [:codeblock {} [:close]]))
      (defn code [text]
        (def extra-cols (- state/col-edge state/col-pos max-indent))
        (def extra-space (when (> extra-cols 0) (string/repeat " " extra-cols)))
        (def line (string extra-space text))
        [:codeblock {} @[line]])
      (def fence-grammar
        ~{:main    (* :padding (+ :fence :code) :eol ($))
          :eol     -1
          :ws      (set " \t\n")
          :padding (drop (/ '(any (set " \t")) ,util/record-padding))
          :fence   (cmt (* (at-least ,delim-num ,delim) (any :ws) (> 0 :eol)) ,fence)
          :code    (/ '(thru :eol) ,code)})
      (peg/match fence-grammar line pos))))

(defn- codeblock/codeblock-see-blank [a-codeblock protocols]
  (node/attribute a-codeblock :open?))

(util/add-to state/protocols
  @{:blocks
    @{:codeblock  {:blank      codeblock/codeblock-blank
                   :close      codeblock/codeblock-close
                   :continue   codeblock/codeblock-continue
                   :equal?     codeblock/codeblock-equal?
                   :follower   codeblock/codeblock-follower
                   :lazy?      codeblock/codeblock-lazy?
                   :needs-nl?  codeblock/codeblock-needs-nl?
                   :next-block codeblock/codeblock-next-block
                   :see-blank  codeblock/codeblock-see-blank}}})

(comment import ./blocks/heading :prefix "")
(comment import ../state :prefix "")

(comment import ../util :prefix "")

(comment import ../node :prefix "")


## Grammar

(defn- heading/heading-atx [level &opt content]
  (def text (if (nil? content) "" (string/trim content)))
  [:heading @{:level (length level) :open? false :inlines? true :kind :atx} @[text]])

(defn- heading/heading-setext [chars]
  (def level (if (= 61 (first chars)) 1 2))
  [:heading @{:level level :open? false :inlines? true :kind :setext} @[chars]])

(def heading/grammar
  ~{:heading {:main   (+ :atx :setext)
              :atx    {:main  (/ (* :open (? (* :space :text)) :close) ,heading/heading-atx)
                       :open  '(between 1 6 "#")
                       :text  '(to :close)
                       :close (+ (* (any :space) :eol)
                                 (* (> -1 " ") (some "#") (any :space) :eol))}
              :setext (/ (* '(+ (some "=") (some "-")) (any :space) :eol) ,heading/heading-setext)}})

## Functions

(defn- heading/heading-equal? [a-heading block]
  (= :paragraph (node/type-of block)))

(defn- heading/heading-lazy? [a-heading]
  (= :setext (node/attribute a-heading :kind)))

(defn- heading/heading-follower [a-heading block]
  (when (= :paragraph (node/type-of block))
    a-heading))

(defn- heading/heading-replace [a-heading siblings]
  (case (node/attribute a-heading :kind)
    :atx
    (array/push siblings a-heading)
    :setext
    (if (= :paragraph (node/type-of (array/peek siblings)))
      (do
        (def last-p (array/pop siblings))
        (def children (get a-heading 2))
        (array/clear children)
        (array/concat children (get last-p 2))
        (array/push siblings a-heading))
      (do
        (def text (first (get a-heading 2))) # TODO Can we assume a heading only has one element?
        (if (node/attribute (array/peek siblings) :open?)
          (do
            (def parent (node/last-descendant (array/peek siblings)))
            (array/push (get parent 2) text))
          (array/push siblings [:paragraph @{:open? true :inlines? true} @[text]]))))))

(util/add-to state/protocols
  {:blocks
    {:heading {:equal?   heading/heading-equal?
               :follower heading/heading-follower
               :lazy?    heading/heading-lazy?
               :replace  heading/heading-replace}}})

(comment import ./blocks/html :prefix "")
(comment import ../state :prefix "")

(comment import ../util :prefix "")

(comment import ../node :prefix "")


## Grammar

(defn- html/html [kind & args]
  (def indent (- state/col-edge state/col-pos))
  (def [close? text] (if (one? (length args)) [nil (first args)] args))
  (def line (string (when (> indent 0) (string/repeat " " indent))
                    (if close? (string/slice text 0 -2) text)))
  [:html @{:open? (if close? false true) :kind kind} @[line]])

(def html/grammar
  ~@{:html {:main   (/ (+ :type-1 :type-2 :type-3 :type-4 :type-5 :type-6 :type-7) ,html/html)
            :attrs  (any (* :space :attr))
            :attr   {:main  (* :name :spec)
                     :name  (* (+ :w (set "_:")) (any (+ :w :d (set "_.:-"))))
                     :spec  (* (any :space) "=" (any :space) :value)
                     :value (+ (* "\"" (thru "\""))
                               (* "'" (thru "'"))
                               (some (if-not (+ :s (set "\"'=<>`")) 1)))}
            :type-1 {:main    (* (constant 1) '(* :open (? (thru :close)) (thru :eol)))
                     :open    (* "<" :keyword (? " >"))
                     :close   (* "</" :keyword ">" (constant true))
                     :keyword ,(util/words "pre" "script" "style" "textarea")}
            :type-2 (* (constant 2) '(* "<!--" (? (* (thru "-->") (constant true))) (thru :eol)))
            :type-3 (* (constant 3) '(* "<?" (? (* (thru "?>") (constant true))) (thru :eol)))
            :type-4 (* (constant 4) '(* "<!" (range "AZ") (? (* (thru ">") (constant true))) (thru :eol)))
            :type-5 (* (constant 5) '(* "<![CDATA[" (? (* (thru "]]>") (constant true))) (thru :eol)))
            :type-6 {:main    (* (constant 6) '(* "<" (? "/") :keyword (? (+ ">" "/>")) (thru :eol)))
                     :keyword (* ,(util/words "address" "article" "aside" "base"
                                         "basefont" "blockquote" "body"
                                         "caption" "center" "col" "colgroup"
                                         "dd" "details" "dialog" "dir" "div"
                                         "dl" "dt" "fieldset" "figcaption"
                                         "figure" "footer" "form" "frame"
                                         "frameset" "h1" "h2" "h3" "h4" "h5"
                                         "h6" "head" "header" "hr" "html"
                                         "iframe" "legend" "li" "link" "main"
                                         "menu" "menuitem" "nav" "noframes" "ol"
                                         "optgroup" "option" "p" "param"
                                         "section" "source" "summary" "table"
                                         "tbody" "td" "tfoot" "th" "thead"
                                         "title" "tr" "track" "ul")
                                 (> 0 (+ :eol (set " />"))))}
            :type-7 {:main  (* (constant 7) '(* (+ :open :close) (any :space) :eol))
                     :open  (* "<" :name :attrs (any :space) ">")
                     :close (* "</" :name (any :space) ">")
                     :name  (* (not ,(util/words "pre" "script" "style" "textarea")) :w (any (+ :w :d "-")))}}})

## Functions

(defn- html/html-close [an-html &opt parent protocols]
  (node/attribute an-html :open? false)
  (def lines (node/children-of an-html))
  (def last-line (last lines))
  (def last-index (dec (length last-line)))
  (var i last-index)
  (while (> i 0)
    (def c (get last-line i))
    (unless (= 10 c)
      (break))
    (-- i))
  (unless (= last-index i)
    (array/pop lines)
    (array/push lines (string/slice last-line 0 (inc i)))))


(defn- html/html-continue [an-html block]
  (def lines (node/children-of block))
  (if (= :close (first lines))
    (do
      (unless (one? (length lines))
        (array/push (node/children-of an-html) (get lines 1)))
      (html/html-close an-html))
    (array/concat (node/children-of an-html) lines)))


(defn- html/html-needs-nl? [an-html]
  (= 7 (node/attribute an-html :kind)))


(defn- html/html-next-block [an-html line pos html/grammar protocols]
  (defn close [&opt text]
    (if (nil? text)
      [:html {} [:close]]
      [:html {} [:close text]]))
  (defn code [text]
    [:html {:kind (node/attribute an-html :kind)} [text]])
  (def html-grammar
    ~{:main   (* (+ :close :code) ($))
      :eol    -1
      :space  (set " \t\n")
      :close  (/ ,(case (node/attribute an-html :kind)
                    1 ~(<- (* (thru (* "</" ,(util/words "pre" "script" "style" "") ">")) (thru :eol)))
                    2 ~(<- (* (thru "-->") (thru :eol)))
                    3 ~(<- (* (thru "?>") (thru :eol)))
                    4 ~(<- (* (thru ">") (thru :eol)))
                    5 ~(<- (* (thru "]]>") (thru :eol)))
                    6 ~(* (any :space) :eol)
                    7 ~(* (any :space) :eol)) ,close)
      :code   (/ '(thru :eol) ,code)})
  (peg/match html-grammar line pos (node/attribute an-html :kind)))

(defn- html/html-see-blank [an-html protocols]
  true)

(util/add-to state/protocols
  @{:blocks
    @{:html {:blank       html/html-close
             :close       html/html-close
             :continue    html/html-continue
             :needs-nl?   html/html-needs-nl?
             :next-block  html/html-next-block
             :see-blank   html/html-see-blank}}})

(comment import ./blocks/linkdef :prefix "")
(comment import ../state :prefix "")

(comment import ../util :prefix "")

(comment import ../node :prefix "")


## Grammar

(defn- linkdef/linkdef [content]
  [:linkdef @{:open? true} @[content]])

(def linkdef/grammar
  ~{:linkdef (/ '(* "[" (to :eol)) ,linkdef/linkdef)})

## Functions

(defn- linkdef/linkdef-close [a-def &opt parent protocols]
  (defn register-link [ref-text dest &opt title]
    (def ref (util/normalise ref-text))
    (unless (get state/links ref)
      (put state/links ref {:url (util/uri-encode dest) :title title})))
  (def link-grammar
    ~{:main (* (/ (* :label ":" :gap :dest (+ (* (> 0 (+ :space :nl)) :gap :title (any :space) :eol) (* (any :space) :eol))) ,register-link) ($))
      :eol   (+ :nl -1)
      :nl    "\n"
      :space (set " \t")
      :blank (* :nl (any :space) :nl) # check whether parens balanced
      :gap   (* (any :space) (? :nl) (any :space))
      :escaped (+ (* "\\" '(set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")) '(* "\\" 1))
      :entity  (/ (+ (* (constant :ent) '(* "&" (some (+ :w :d)) ";"))
                     (* (constant :dec) '(* "&#" (between 1 7 :d) ";"))
                     (* (constant :hex) '(* "&#" (set "Xx") (between 1 6 :h) ";")))
                  ,util/entity-decode)
      :char    (+ :escaped :entity '1)
      :label (* "[" :gap '(some (+ (* "\\" 1) (if-not (+ (set "\\[]") :blank) 1))) :gap "]")
      :dest  (+ (* "<" (% (any (if-not (set "<>\n") :char))) ">")
                (* (not "<") (% (some (if-not (+ (range "\x00\x20") "\x7F") :char)))))
      :title (+ (* `"` (% (any (if-not (+ (set "\"") :blank) :char))) `"`)
                (* "'" (% (any (if-not (+ (set "'") :blank) :char))) "'")
                (* "(" (% (any (if-not (+ (set "()") :blank) :char))) ")"))})
  (when (node/attribute a-def :open?)
    (node/attribute a-def :open? false)
    (def all-text (-> (node/children-of a-def) (string/join "\n") string/trim))
    (var i 0)
    (while (< i (length all-text)) # a linkdef 'block' can contain multiple definitions as well as a follow-on paragraph
      (if (def res (peg/match link-grammar all-text i))
        (set i (get res 1))
        (break)))
    (array/pop (node/children-of parent))
    (when (< i (length all-text))
      (def content (string/slice all-text i))
      (array/push (node/children-of parent) [:paragraph @{:indent (node/attribute a-def :indent) :open? false :inlines? true} @[content]])))
  nil)

(defn- linkdef/linkdef-lazy? [a-def]
  true)

(defn- linkdef/linkdef-equal? [a-def block]
  (or (= :paragraph (node/type-of block))
      (= :linkdef (node/type-of block))))

(defn- linkdef/linkdef-needs-nl? [a-def]
  true)

(defn- linkdef/linkdef-next-block [a-def line pos linkdef/grammar protocols]
  (def result (peg/match linkdef/grammar line pos))
  (def block (get result 0))
  (def needs-nl-fn (node/get-fn :needs-nl? block protocols))
  (if (needs-nl-fn block)
    [(util/to-continuation :paragraph line pos) (length line)]
    result))

(util/add-to state/protocols
  {:blocks
    {:linkdef {:blank       linkdef/linkdef-close
               :close       linkdef/linkdef-close
               :equal?      linkdef/linkdef-equal?
               :lazy?       linkdef/linkdef-lazy?
               :needs-nl?   linkdef/linkdef-needs-nl?
               :next-block  linkdef/linkdef-next-block}}})

(comment import ./blocks/list :prefix "")
(comment import ../state :prefix "")

(comment import ../util :prefix "")

(comment import ../node :prefix "")

(comment import ../container :prefix "")


## Grammar

(defn- list/list-item [marker-width trailing-space &opt starts-blank?]
  (def start-pos state/col-pos)
  (+= state/col-edge marker-width)
  (set state/col-pos state/col-edge)
  (util/record-padding trailing-space)
  (if (empty? trailing-space)
    (++ state/col-pos)
    (+= state/col-pos (if (or starts-blank? (> (- state/col-edge state/col-pos) 4)) 1 (- state/col-edge state/col-pos))))
  [:list-item @{:container? true :open? true :width (- state/col-pos start-pos) :starts-blank? starts-blank?} @[]])

(defn- list/list [kind marker delim trailing-space &opt starts-blank?]
  (def start (when (= :ordinal kind) (scan-number marker)))
  (def marker-width (+ (length marker) (if (nil? delim) 0 1)))
  (def item (list/list-item marker-width trailing-space starts-blank?))
  [:list @{:kind kind :marker marker :delim delim :start start :tight? true :container? true :open? true} @[item]])

(def list/grammar
  ~{:list {:main    (cmt (* :marker :after) ,list/list)
           :marker  (+ :bullet :ordinal)
           :bullet  (* (constant :bullet) '(set "-+*") (constant nil))
           :ordinal (* (constant :ordinal) '(between 1 9 :d) '(set ".)"))
           :after   (+ (* '(any :space) :eol (constant true))
                       (* '(some :space)))}})

## Functions

(defn- list/list-blank [a-list parent protocols]
  (def item (node/next-container a-list))
  (when (and (not (nil? item))
             (node/attribute item :starts-blank?)
             (zero? (length (node/children-of item))))
    (node/attribute item :open? false))
  (node/attribute a-list :has-blank? true)
  (node/next-container a-list))

(defn- list/list-equal? [a-list block]
  (and (= :list (node/type-of block))
       (= (node/attribute a-list :kind) (node/attribute block :kind))
       (or (and (= :ordinal (node/attribute a-list :kind))
                (= (node/attribute a-list :delim) (node/attribute block :delim)))
           (and (= :bullet (node/attribute a-list :kind))
                (= (node/attribute a-list :marker) (node/attribute block :marker))))))

(defn- list/list-needs-nl? [a-list]
  (or (node/attribute (node/next-container a-list) :starts-blank?)
      (and (= :ordinal (node/attribute a-list :kind))
           (not= 1 (node/attribute a-list :start)))))

# The current open block is a list so we need to handle the case where the
# line is a continuation of the list. To check this we need to descend through
# the open list items in the current list that are indented at least as much as
# the current line.
(defn- list/list-next-block [a-list line pos list/grammar protocols]
  (def next-pos (util/dedent line pos))
  (var next-b nil)
  (var parent-list nil)
  (var parent-item nil)
  (var curr-list a-list)
  (var curr-item (node/next-container a-list))
  (while curr-item
    # set start col of list item
    (def curr-width (node/attribute curr-item :width))
    # break if there's not enough padding
    (def remaining-width (- state/col-edge state/col-pos))
    (if (> curr-width remaining-width)
      (break)
      (+= state/col-pos curr-width))
    # the parent list is at least equal to the current list
    (when parent-list
      (node/attribute parent-list :has-blank? false))
    (set parent-list curr-list)
    # create continuation
    (def new-item [:list-item-continue (get curr-item 1) @[]])
    (def new-list [:list (get curr-list 1) [new-item]])
    # set next-b and parent
    (if (nil? next-b)
      (set next-b new-list)
      (array/push (node/children-of parent-item) new-list))
    (set parent-item new-item)
    # break if no more lists
    (def child (node/next-container curr-item))
    (when (or (nil? child) (not= :list (node/type-of child)))
      (break))
    # prepare for next round of loop
    (set curr-list child)
    (set curr-item (node/next-container curr-list)))
  # parse line from current position
  (def result (peg/match list/grammar line next-pos))
  # make parent list loose if a blank line has come before
  (cond
    (and (nil? parent-list)
         (node/attribute a-list :has-blank?)
         (list/list-equal? a-list (get result 0)))
    (node/attribute a-list :tight? false)
    (and parent-list
         (node/attribute parent-list :has-blank?))
    (node/attribute parent-list :tight? false))
  # return result
  (if (nil? next-b)
    result
    [next-b next-pos]))

(defn- list/list-item-equal? [an-item block]
  (= :list-item-continue (node/type-of block)))

# Use container base with list-specific overrides
(util/add-to state/protocols
  {:blocks
    {:list (container/make-protocol
             {:equal?     list/list-equal?
              :blank      list/list-blank
              :next-block list/list-next-block
              :needs-nl?  list/list-needs-nl?})
     :list-item  {:equal? list/list-item-equal?}}})

(comment import ./blocks/paragraph :prefix "")
(comment import ../state :prefix "")

(comment import ../util :prefix "")

(comment import ../node :prefix "")


## Grammar

(defn- paragraph/paragraph [text]
  [:paragraph @{:open? true :inlines? true} @[text]])

(def paragraph/grammar
  ~{:paragraph (/ :text ,paragraph/paragraph)})

## Functions

(defn- paragraph/paragraph-append [a-paragraph continuation protocols]
  (array/concat (node/children-of a-paragraph) (node/children-of continuation)))

(defn- paragraph/paragraph-blank [a-paragraph parent protocols]
  (node/attribute a-paragraph :open? false)
  nil)

(defn- paragraph/paragraph-equal? [a-paragraph block]
  (or (= :paragraph (node/type-of block))
      (and (= :heading (node/type-of block))
           (= :setext (node/attribute block :kind)))))

(defn- paragraph/paragraph-follower [a-paragraph block]
  (when (= :paragraph (node/type-of block))
    a-paragraph))

(defn- paragraph/paragraph-lazy? [a-paragraph]
  true)

(defn- paragraph/paragraph-next-block [a-paragraph line pos paragraph/grammar protocols]
  (def result (peg/match paragraph/grammar line pos))
  (def block (get result 0))
  (defn heading? []
    (and (= :thematic-break (node/type-of block))
         (= 45 (node/attribute block :char))
         (peg/match ~(* (any " ") (some "-") (any " ") -1) (first (node/children-of block)))))
  (cond
    (heading?)
    [[:heading @{:level 2 :open? false :inlines? true :kind :setext} @["-"]] (length line)]
    ((node/get-fn :needs-nl? block protocols) block)
    [(util/to-continuation :paragraph line pos) (length line)]
    # default
    result))

(util/add-to state/protocols
  {:blocks
    {:paragraph {:append     paragraph/paragraph-append
                 :blank      paragraph/paragraph-blank
                 :equal?     paragraph/paragraph-equal?
                 :follower   paragraph/paragraph-follower
                 :lazy?      paragraph/paragraph-lazy?
                 :next-block paragraph/paragraph-next-block}}})

(comment import ./blocks/t-break :prefix "")
(defn- t-break/t-break [chars]
  [:thematic-break @{:char (get chars 0)} @[chars]])

(def t-break/grammar
  ~@{:t-break (/ (* '(+ (at-least 3 (* "-" (any :space)))
                        (at-least 3 (* "_" (any :space)))
                        (at-least 3 (* "*" (any :space)))) :eol) ,t-break/t-break)})


(array/push blocks/blocks
  :blank
  :codeblock
  :t-break
  :html
  :linkdef
  :blockquote
  :list
  :heading
  :paragraph)

(def blocks/grammar
  ~@{:main  (* (/ :block ,util/update-col-pos) ($))
     :nl    "\n"
     :eol   (+ :nl -1)
     :space (set " \t")
     :char    (+ :escaped :entity (if-not :eol '1))
     :escaped (+ (* "\\" '(set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")) '(* "\\" 1))
     :entity  (/ (+ (* (constant :ent) '(* "&" (some (+ :w :d)) ";"))
                   (* (constant :dec) '(* "&#" (between 1 7 :d) ";"))
                   (* (constant :hex) '(* "&#" (set "Xx") (between 1 6 :h) ";")))
                ,util/entity-decode)
     :text (* '(some (if-not :eol 1)) (? :nl))
     :padding (drop (/ '(any :space) ,util/record-padding))
     :block (* :padding ,(tuple ;blocks/blocks))})

(util/add-to blocks/grammar blank/grammar)
(util/add-to blocks/grammar blockquote/grammar)
(util/add-to blocks/grammar codeblock/grammar)
(util/add-to blocks/grammar heading/grammar)
(util/add-to blocks/grammar html/grammar)
(util/add-to blocks/grammar list/grammar)
(util/add-to blocks/grammar linkdef/grammar)
(util/add-to blocks/grammar paragraph/grammar)
(util/add-to blocks/grammar t-break/grammar)

## Block functions

(defn- blocks/default-block-append [node block protocols]
  (unless (= :blank (node/type-of block))
    (def replace-fn (node/get-fn :replace block protocols))
    (if replace-fn
      (replace-fn block (node/children-of node))
      (if (def peer (node/next-container node))
        ((node/get-fn :continue peer protocols) peer block)
        (array/push (node/children-of node) block)))))

(defn- blocks/default-block-blank [node parent protocols]
  (node/next-container node))

(defn- blocks/default-block-close [node &opt parent]
  (node/attribute node :open? false))

(defn- blocks/default-block-continue [peer block]
  (array/concat (node/children-of peer) (node/children-of block)))

(defn- blocks/default-block-equal? [node block]
  (= (node/type-of node) (node/type-of block)))

(defn- blocks/default-block-follower [node block]
  nil)

(defn- blocks/default-block-lazy? [block]
  false)

(defn- blocks/default-block-needs-nl? [block]
  false)

(defn- blocks/default-block-next-block [node line pos blocks/grammar protocols]
  (peg/match blocks/grammar line pos))

(defn- blocks/default-block-see-blank [node protocols]
  nil)

## Block default protocol

(util/add-to state/protocols
  {:blocks
    {'default {:append      blocks/default-block-append
               :blank       blocks/default-block-blank
               :close       blocks/default-block-close
               :continue    blocks/default-block-continue
               :equal?      blocks/default-block-equal?
               :follower    blocks/default-block-follower
               :lazy?       blocks/default-block-lazy?
               :needs-nl?   blocks/default-block-needs-nl?
               :next-block  blocks/default-block-next-block
               :see-blank   blocks/default-block-see-blank}}})

(comment import ./inlines :prefix "")
(comment import ./state :prefix "")

(comment import ./util :prefix "")


(comment import ./punctuation :prefix "")
(defn punctuation/upunc? [x]
  (or
    (and (>= x 161) (<= x 169))
    (and (>= x 171) (<= x 172))
    (and (>= x 174) (<= x 177))
    (= x 180)
    (and (>= x 182) (<= x 184))
    (= x 187)
    (= x 191)
    (= x 215)
    (= x 247)
    (and (>= x 706) (<= x 709))
    (and (>= x 722) (<= x 735))
    (and (>= x 741) (<= x 747))
    (= x 749)
    (and (>= x 751) (<= x 767))
    (= x 885)
    (= x 894)
    (and (>= x 900) (<= x 901))
    (= x 903)
    (= x 1014)
    (= x 1154)
    (and (>= x 1370) (<= x 1375))
    (and (>= x 1417) (<= x 1418))
    (and (>= x 1421) (<= x 1423))
    (= x 1470)
    (= x 1472)
    (= x 1475)
    (= x 1478)
    (and (>= x 1523) (<= x 1524))
    (and (>= x 1542) (<= x 1551))
    (= x 1563)
    (and (>= x 1565) (<= x 1567))
    (and (>= x 1642) (<= x 1645))
    (= x 1748)
    (= x 1758)
    (= x 1769)
    (and (>= x 1789) (<= x 1790))
    (and (>= x 1792) (<= x 1805))
    (and (>= x 2038) (<= x 2041))
    (and (>= x 2046) (<= x 2047))
    (and (>= x 2096) (<= x 2110))
    (= x 2142)
    (= x 2184)
    (and (>= x 2404) (<= x 2405))
    (= x 2416)
    (and (>= x 2546) (<= x 2547))
    (and (>= x 2554) (<= x 2555))
    (= x 2557)
    (= x 2678)
    (and (>= x 2800) (<= x 2801))
    (= x 2928)
    (and (>= x 3059) (<= x 3066))
    (= x 3191)
    (= x 3199)
    (= x 3204)
    (= x 3407)
    (= x 3449)
    (= x 3572)
    (= x 3647)
    (= x 3663)
    (and (>= x 3674) (<= x 3675))
    (and (>= x 3841) (<= x 3863))
    (and (>= x 3866) (<= x 3871))
    (= x 3892)
    (= x 3894)
    (= x 3896)
    (and (>= x 3898) (<= x 3901))
    (= x 3973)
    (and (>= x 4030) (<= x 4037))
    (and (>= x 4039) (<= x 4044))
    (and (>= x 4046) (<= x 4058))
    (and (>= x 4170) (<= x 4175))
    (and (>= x 4254) (<= x 4255))
    (= x 4347)
    (and (>= x 4960) (<= x 4968))
    (and (>= x 5008) (<= x 5017))
    (= x 5120)
    (and (>= x 5741) (<= x 5742))
    (and (>= x 5787) (<= x 5788))
    (and (>= x 5867) (<= x 5869))
    (and (>= x 5941) (<= x 5942))
    (and (>= x 6100) (<= x 6102))
    (and (>= x 6104) (<= x 6107))
    (and (>= x 6144) (<= x 6154))
    (= x 6464)
    (and (>= x 6468) (<= x 6469))
    (and (>= x 6622) (<= x 6655))
    (and (>= x 6686) (<= x 6687))
    (and (>= x 6816) (<= x 6822))
    (and (>= x 6824) (<= x 6829))
    (and (>= x 7002) (<= x 7018))
    (and (>= x 7028) (<= x 7038))
    (and (>= x 7164) (<= x 7167))
    (and (>= x 7227) (<= x 7231))
    (and (>= x 7294) (<= x 7295))
    (and (>= x 7360) (<= x 7367))
    (= x 7379)
    (= x 8125)
    (and (>= x 8127) (<= x 8129))
    (and (>= x 8141) (<= x 8143))
    (and (>= x 8157) (<= x 8159))
    (and (>= x 8173) (<= x 8175))
    (and (>= x 8189) (<= x 8190))
    (and (>= x 8208) (<= x 8231))
    (and (>= x 8240) (<= x 8286))
    (and (>= x 8314) (<= x 8318))
    (and (>= x 8330) (<= x 8334))
    (and (>= x 8352) (<= x 8384))
    (and (>= x 8448) (<= x 8449))
    (and (>= x 8451) (<= x 8454))
    (and (>= x 8456) (<= x 8457))
    (= x 8468)
    (and (>= x 8470) (<= x 8472))
    (and (>= x 8478) (<= x 8483))
    (= x 8485)
    (= x 8487)
    (= x 8489)
    (= x 8494)
    (and (>= x 8506) (<= x 8507))
    (and (>= x 8512) (<= x 8516))
    (and (>= x 8522) (<= x 8525))
    (= x 8527)
    (and (>= x 8586) (<= x 8587))
    (and (>= x 8592) (<= x 9254))
    (and (>= x 9280) (<= x 9290))
    (and (>= x 9372) (<= x 9449))
    (and (>= x 9472) (<= x 10101))
    (and (>= x 10132) (<= x 11123))
    (and (>= x 11126) (<= x 11157))
    (and (>= x 11159) (<= x 11263))
    (and (>= x 11493) (<= x 11498))
    (and (>= x 11513) (<= x 11516))
    (and (>= x 11518) (<= x 11519))
    (= x 11632)
    (and (>= x 11776) (<= x 11822))
    (and (>= x 11824) (<= x 11869))
    (and (>= x 11904) (<= x 11929))
    (and (>= x 11931) (<= x 12019))
    (and (>= x 12032) (<= x 12245))
    (and (>= x 12272) (<= x 12283))
    (and (>= x 12289) (<= x 12292))
    (and (>= x 12296) (<= x 12320))
    (= x 12336)
    (and (>= x 12342) (<= x 12343))
    (and (>= x 12349) (<= x 12351))
    (and (>= x 12443) (<= x 12444))
    (= x 12448)
    (= x 12539)
    (and (>= x 12688) (<= x 12689))
    (and (>= x 12694) (<= x 12703))
    (and (>= x 12736) (<= x 12771))
    (and (>= x 12800) (<= x 12830))
    (and (>= x 12842) (<= x 12871))
    (= x 12880)
    (and (>= x 12896) (<= x 12927))
    (and (>= x 12938) (<= x 12976))
    (and (>= x 12992) (<= x 13311))
    (and (>= x 19904) (<= x 19967))
    (and (>= x 42128) (<= x 42182))
    (and (>= x 42238) (<= x 42239))
    (and (>= x 42509) (<= x 42511))
    (= x 42611)
    (= x 42622)
    (and (>= x 42738) (<= x 42743))
    (and (>= x 42752) (<= x 42774))
    (and (>= x 42784) (<= x 42785))
    (and (>= x 42889) (<= x 42890))
    (and (>= x 43048) (<= x 43051))
    (and (>= x 43062) (<= x 43065))
    (and (>= x 43124) (<= x 43127))
    (and (>= x 43214) (<= x 43215))
    (and (>= x 43256) (<= x 43258))
    (= x 43260)
    (and (>= x 43310) (<= x 43311))
    (= x 43359)
    (and (>= x 43457) (<= x 43469))
    (and (>= x 43486) (<= x 43487))
    (and (>= x 43612) (<= x 43615))
    (and (>= x 43639) (<= x 43641))
    (and (>= x 43742) (<= x 43743))
    (and (>= x 43760) (<= x 43761))
    (= x 43867)
    (and (>= x 43882) (<= x 43883))
    (= x 44011)
    (= x 64297)
    (and (>= x 64434) (<= x 64450))
    (and (>= x 64830) (<= x 64847))
    (= x 64975)
    (and (>= x 65020) (<= x 65023))
    (and (>= x 65040) (<= x 65049))
    (and (>= x 65072) (<= x 65106))
    (and (>= x 65108) (<= x 65126))
    (and (>= x 65128) (<= x 65131))
    (and (>= x 65281) (<= x 65295))
    (and (>= x 65306) (<= x 65312))
    (and (>= x 65339) (<= x 65344))
    (and (>= x 65371) (<= x 65381))
    (and (>= x 65504) (<= x 65510))
    (and (>= x 65512) (<= x 65518))
    (and (>= x 65532) (<= x 65533))
    (and (>= x 65792) (<= x 65794))
    (and (>= x 65847) (<= x 65855))
    (and (>= x 65913) (<= x 65929))
    (and (>= x 65932) (<= x 65934))
    (and (>= x 65936) (<= x 65948))
    (= x 65952)
    (and (>= x 66000) (<= x 66044))
    (= x 66463)
    (= x 66512)
    (= x 66927)
    (= x 67671)
    (and (>= x 67703) (<= x 67704))
    (= x 67871)
    (= x 67903)
    (and (>= x 68176) (<= x 68184))
    (= x 68223)
    (= x 68296)
    (and (>= x 68336) (<= x 68342))
    (and (>= x 68409) (<= x 68415))
    (and (>= x 68505) (<= x 68508))
    (= x 69293)
    (and (>= x 69461) (<= x 69465))
    (and (>= x 69510) (<= x 69513))
    (and (>= x 69703) (<= x 69709))
    (and (>= x 69819) (<= x 69820))
    (and (>= x 69822) (<= x 69825))
    (and (>= x 69952) (<= x 69955))
    (and (>= x 70004) (<= x 70005))
    (and (>= x 70085) (<= x 70088))
    (= x 70093)
    (= x 70107)
    (and (>= x 70109) (<= x 70111))
    (and (>= x 70200) (<= x 70205))
    (= x 70313)
    (and (>= x 70731) (<= x 70735))
    (and (>= x 70746) (<= x 70747))
    (= x 70749)
    (= x 70854)
    (and (>= x 71105) (<= x 71127))
    (and (>= x 71233) (<= x 71235))
    (and (>= x 71264) (<= x 71276))
    (= x 71353)
    (and (>= x 71484) (<= x 71487))
    (= x 71739)
    (and (>= x 72004) (<= x 72006))
    (= x 72162)
    (and (>= x 72255) (<= x 72262))
    (and (>= x 72346) (<= x 72348))
    (and (>= x 72350) (<= x 72354))
    (and (>= x 72448) (<= x 72457))
    (and (>= x 72769) (<= x 72773))
    (and (>= x 72816) (<= x 72817))
    (and (>= x 73463) (<= x 73464))
    (and (>= x 73539) (<= x 73551))
    (and (>= x 73685) (<= x 73713))
    (= x 73727)
    (and (>= x 74864) (<= x 74868))
    (and (>= x 77809) (<= x 77810))
    (and (>= x 92782) (<= x 92783))
    (= x 92917)
    (and (>= x 92983) (<= x 92991))
    (and (>= x 92996) (<= x 92997))
    (and (>= x 93847) (<= x 93850))
    (= x 94178)
    (= x 113820)
    (= x 113823)
    (and (>= x 118608) (<= x 118723))
    (and (>= x 118784) (<= x 119029))
    (and (>= x 119040) (<= x 119078))
    (and (>= x 119081) (<= x 119140))
    (and (>= x 119146) (<= x 119148))
    (and (>= x 119171) (<= x 119172))
    (and (>= x 119180) (<= x 119209))
    (and (>= x 119214) (<= x 119274))
    (and (>= x 119296) (<= x 119361))
    (= x 119365)
    (and (>= x 119552) (<= x 119638))
    (= x 120513)
    (= x 120539)
    (= x 120571)
    (= x 120597)
    (= x 120629)
    (= x 120655)
    (= x 120687)
    (= x 120713)
    (= x 120745)
    (= x 120771)
    (and (>= x 120832) (<= x 121343))
    (and (>= x 121399) (<= x 121402))
    (and (>= x 121453) (<= x 121460))
    (and (>= x 121462) (<= x 121475))
    (and (>= x 121477) (<= x 121483))
    (= x 123215)
    (= x 123647)
    (and (>= x 125278) (<= x 125279))
    (= x 126124)
    (= x 126128)
    (= x 126254)
    (and (>= x 126704) (<= x 126705))
    (and (>= x 126976) (<= x 127019))
    (and (>= x 127024) (<= x 127123))
    (and (>= x 127136) (<= x 127150))
    (and (>= x 127153) (<= x 127167))
    (and (>= x 127169) (<= x 127183))
    (and (>= x 127185) (<= x 127221))
    (and (>= x 127245) (<= x 127405))
    (and (>= x 127462) (<= x 127490))
    (and (>= x 127504) (<= x 127547))
    (and (>= x 127552) (<= x 127560))
    (and (>= x 127568) (<= x 127569))
    (and (>= x 127584) (<= x 127589))
    (and (>= x 127744) (<= x 128727))
    (and (>= x 128732) (<= x 128748))
    (and (>= x 128752) (<= x 128764))
    (and (>= x 128768) (<= x 128886))
    (and (>= x 128891) (<= x 128985))
    (and (>= x 128992) (<= x 129003))
    (= x 129008)
    (and (>= x 129024) (<= x 129035))
    (and (>= x 129040) (<= x 129095))
    (and (>= x 129104) (<= x 129113))
    (and (>= x 129120) (<= x 129159))
    (and (>= x 129168) (<= x 129197))
    (and (>= x 129200) (<= x 129201))
    (and (>= x 129280) (<= x 129619))
    (and (>= x 129632) (<= x 129645))
    (and (>= x 129648) (<= x 129660))
    (and (>= x 129664) (<= x 129672))
    (and (>= x 129680) (<= x 129725))
    (and (>= x 129727) (<= x 129733))
    (and (>= x 129742) (<= x 129755))
    (and (>= x 129760) (<= x 129768))
    (and (>= x 129776) (<= x 129784))
    (and (>= x 129792) (<= x 129938))
    (and (>= x 129940) (<= x 129994))))

# UTF-8 helpers for Unicode punctuation detection

(defn- inlines/bytes-to-codepoint [s]
  (def b0 (get s 0))
  (cond
    (< b0 0x80) b0  # 1-byte (ASCII)
    (< b0 0xE0)     # 2-byte
      (+ (blshift (band b0 0x1F) 6)
         (band (get s 1) 0x3F))
    (< b0 0xF0)     # 3-byte
      (+ (blshift (band b0 0x0F) 12)
         (blshift (band (get s 1) 0x3F) 6)
         (band (get s 2) 0x3F))
    :else           # 4-byte
      (+ (blshift (band b0 0x07) 18)
         (blshift (band (get s 1) 0x3F) 12)
         (blshift (band (get s 2) 0x3F) 6)
         (band (get s 3) 0x3F))))

(defn- inlines/check-unicode-punc [s]
  (when (punctuation/upunc? (inlines/bytes-to-codepoint s))
    s))

(util/add-to state/protocols @{:inlines @{}})

(def inlines/inlines ~@[+])

(comment import ./inlines/autolink :prefix "")
(comment import ../util :prefix "")


## Grammar

(defn- autolink/autolink [scheme uri &opt email?]
  (def url (string scheme uri))
  [:link @{:url (util/uri-encode url)} @[(if email? uri url)]])

(def autolink/grammar
  ~{:autolink {:main   (/ (* "<" (+ :mail :other) ">") ,autolink/autolink)
               :mail   (* (constant "mailto:") :email (constant true))
               :email  '(* (some (+ :w :d (set ".!#$%&'*+/=?^_`{|}~-")))
                           "@"
                           (* :alnum (at-most 61 (+ :w :d "-"))
                           (any (* "." :alnum (at-most 61 (+ :w :d "-"))))))
               :other  (* :scheme :uri)
               :scheme '(* (+ ,(util/words "mailto")
                              (* :w (some (+ :w :d (set "+.-"))))) ":")
               :uri    '(any (if-not (+ (set "<>") (range "\x00\x20") "\x7F") 1))}})

(comment import ./inlines/codespan :prefix "")
(comment import ../state :prefix "")

(comment import ../util :prefix "")


## Grammar

(defn- codespan/codespan [delim content]
  (def buf @"")
  (var only-spaces? true)
  (each c content
    (case c
      10 (buffer/push buf " ")
      32 (buffer/push buf c)
         (do
           (buffer/push buf c)
           (set only-spaces? false))))
  (def [start end]
    (if (and (= 32 (first buf))
             (= 32 (last buf))
             (not only-spaces?))
      [1 (dec (length buf))]
      [0 nil]))
  [:codespan @{} (string/slice buf start end)])

(def codespan/grammar
  ~{:codespan {:main  (+ (unref (/ (* :open '(to :close) :close) ,codespan/codespan))
                         '(some "`"))
               :open  (<- (some "`") :delim)
               :close (* (! (> -1 "`")) (backmatch :delim) (not "`"))}})


(comment import ./inlines/emphasis :prefix "")
(comment import ../state :prefix "")

(comment import ../util :prefix "")

(comment import ../node :prefix "")


## Grammar

(defn- emphasis/emphasis [start-pos run flank &opt pre-punc? post-punc?]
  (def delim (case (get run 0) 42 "*" 95 "_"))
  (def num (length run))
  (def left? (case flank :left true :left-and-right true))
  (def right? (case flank :right true :left-and-right true))
  (def end-pos (+ num start-pos))
  (array/push state/delimiters
    [:delims @{:kind :emphasis :delim delim :count num :left? left? :right? right? :pre-punc? pre-punc? :post-punc? post-punc? :start-pos start-pos :end-pos end-pos} @[]])
  (array/peek state/delimiters))

(def emphasis/grammar
  ~{:emphasis {:main   (+ (/ (* ($) :runs) ,emphasis/emphasis) :run)
               :uws    (+ :ws # include other Unicode whitespace
                          "\u00A0" "\u1680" "\u2000" "\u2001" "\u2002" "\u2003"
                          "\u2004" "\u2005" "\u2006" "\u2007" "\u2008" "\u2009"
                          "\u200A" "\u202F" "\u205F" "\u3000")
               :runs   (+ (* (! :l-wsp) :run (! :t-wsp) (constant :left-and-right))
                          (* :l-punc    :run :t-punc    (constant :left-and-right) (constant true) (constant true))
                          (*            :run (! :t-wsp) (constant :left))
                          (* :l-wsp     :run :t-punc    (constant :left) (constant nil) (constant true))
                          (* (! :l-wsp) :run            (constant :right))
                          (* :l-punc    :run :t-wsp     (constant :right) (constant true)))
               :run    '(+ (some "*") (some "_"))
               :l-wsp  (+ :l-uws :l-punc)
               :t-wsp  (+ :t-uws :t-punc)
               :l-uws  (+ (> -1 :uws) (! (> -1 1)))
               :t-uws  (+ (> 0 :uws) (> 0 -1))
               # UTF-8 aware lookbehind: verify start byte matches offset to ensure char ends here
               :l-punc (+ (* (> -1 (range "\x00\x7F")) (> -1 :punc))  # 1-byte (ASCII)
                          (* (> -2 (range "\xC0\xDF")) (> -2 :punc))  # 2-byte
                          (* (> -3 (range "\xE0\xEF")) (> -3 :punc))  # 3-byte
                          (* (> -4 (range "\xF0\xF7")) (> -4 :punc))) # 4-byte
               :t-punc (> 0 :punc)}})

## Functions

(defn- emphasis/emphasis-match? [open-i close-i delimiters]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (and (and (= (node/attribute opener :kind) (node/attribute closer :kind))
            (= (node/attribute opener :delim) (node/attribute closer :delim))
            (not (node/attribute opener :skip?))
            (not= 0 (node/attribute opener :count))
            (not= 0 (node/attribute closer :count)))
       (case (node/attribute closer :delim)
         "*"
         (do
           (and # rules 1 and 5
                (node/attribute opener :left?)
                # rules 3 and 7
                (node/attribute closer :right?)
                # rules 9 and 10
                (or (and (not (node/attribute opener :right?))
                         (not (node/attribute closer :left?)))
                    (or (not= 0 (% (+ (node/attribute opener :count) (node/attribute closer :count)) 3))
                        (and (= 0 (% (node/attribute opener :count) 3))
                             (= 0 (% (node/attribute closer :count) 3)))))))
         "_"
         (do
           (and # rules 2 and 6
                (and (node/attribute opener :left?)
                     (or (not (node/attribute opener :right?))
                         (node/attribute opener :pre-punc?)))
                # rules 4 and 8
                (and (node/attribute closer :right?)
                     (or (not (node/attribute closer :left?))
                         (node/attribute closer :post-punc?)))
                # rules 9 and 10
                (or (and (not (node/attribute opener :right?))
                         (not (node/attribute closer :left?)))
                    (or (not= 0 (% (+ (node/attribute opener :count) (node/attribute closer :count)) 3))
                        (and (= 0 (% (node/attribute opener :count) 3))
                             (= 0 (% (node/attribute closer :count) 3))))))))))

(defn- emphasis/emphasis-match-up [open-i close-i delimiters text]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (def [kind len] (if (and (>= (node/attribute opener :count) 2)
                           (>= (node/attribute closer :count) 2))
                    [:strong 2] [:emphasis 1]))
  (array/insert (node/children-of opener) 0 [:open @{:kind kind} @[]])
  (node/attribute opener :count (- (node/attribute opener :count) len))
  (node/attribute opener :end-pos (- (node/attribute opener :end-pos) len))
  (array/push (node/children-of closer) [:close @{:kind kind} @[]])
  (node/attribute closer :count (- (node/attribute closer :count) len))
  (node/attribute closer :start-pos (+ (node/attribute closer :start-pos) len)))

(util/add-to state/protocols
  {:inlines
    {:emphasis {:match?   emphasis/emphasis-match?
                :match-up emphasis/emphasis-match-up}}})

(comment import ./inlines/link :prefix "")
(comment import ../state :prefix "")

(comment import ../util :prefix "")

(comment import ../node :prefix "")


## Grammar

(defn- link/link-inline [url &opt title]
  {:url (util/uri-encode url) :title title})

(defn- link/link [start-pos delim flank &opt meta end-pos]
  (default end-pos (+ (length delim) start-pos))
  (def image? (when (= 2 (length delim)) true))
  (def left? (when (= :left flank) true))
  (def right? (when (= :right flank) true))
  (array/push state/delimiters
    [:delims @{:kind :link :delim delim :count 1 :left? left? :right? right? :image? image? :start-pos start-pos :end-pos end-pos :meta meta} @[]])
  (array/peek state/delimiters))

(def link/grammar
  ~{:link {:main  (cmt (+ :open :close) ,link/link)
           :open  (* ($) '(* (? "!") "[") (constant :left))
           :close {:main   (* ($) '(* "]" (? "[]")) (constant :right) (+ :inline (constant nil)) ($))
                   :gap    (* (any :space) (? :nl) (any :space))
                   :char   (+ :escaped :entity '1)
                   :parens (* '"(" (% (any (+ (if-not (set "()") '1) :parens))) '")")
                   :inline (/ (* "(" :gap (* :dest (? (* :gap :title))) :gap ")") ,link/link-inline)
                   :dest   (+ (* "<" (% (any (if-not (set "<>\n") :char))) ">")
                              (* (not "<") (% (any (if-not (+ (range "\x00\x20") "\x7F" ")") (+ :parens :char))))))
                   :title  (+ (* `"` (% (any (if-not (+ (set "\"") :blank) :char))) `"`)
                              (* "'" (% (any (if-not (+ (set "'") :blank) :char))) "'")
                              (* "(" (% (any (if-not (+ (set "()") :blank) :char))) ")"))}}})

## Functions

(defn- link/image-close [node delim ancestors]
  (def meta (node/attribute delim :meta))
  (merge-into (get node 1) meta)
  (array/pop ancestors))

(defn- link/link-close [node delim ancestors]
  (if (node/attribute delim :drop?)
    (array/pop (node/children-of (array/peek ancestors)))
    (merge-into (get node 1) (node/attribute delim :meta)))
  (array/pop ancestors))

(defn- link/link-match? [open-i close-i delimiters]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (and (node/attribute opener :left?)
       (not (node/attribute opener :skip?))
       (not (zero? (node/attribute opener :count)))
       (not (zero? (node/attribute closer :count)))))

(defn- link/link-meta-from-label [open-i close-i delimiters text]
  (defn extract-ref [opener closer]
    (-> text
        (string/slice (node/attribute opener :end-pos) (node/attribute closer :start-pos))
        util/normalise))
  (var result nil)
  (def closer (get delimiters close-i))
  (def label-opener (get delimiters (+ 1 close-i)))
  (def label-closer (get delimiters (+ 2 close-i)))
  (if (and label-opener
           label-closer
           (= :link (node/attribute label-opener :kind) (node/attribute label-closer :kind))
           (= (node/attribute closer :end-pos) (node/attribute label-opener :start-pos)))
    (do
      (set result (get state/links (extract-ref label-opener label-closer)))
      (when result
        (array/push (node/children-of label-opener) [:open {:kind :link} []])
        (array/push (node/children-of label-closer) [:close {:drop? true} []])
        (node/attribute label-opener :count 0)
        (node/attribute label-closer :count 0)))
    (do
      (def opener (get delimiters open-i))
      (set result (get state/links (extract-ref opener closer)))))
  result)

(defn- link/link-match-up [open-i close-i delimiters text]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (unless (or (node/attribute opener :inactive?)
              (node/attribute closer :meta))
    (node/attribute closer :meta (link/link-meta-from-label open-i close-i delimiters text)))
  (if (or (node/attribute opener :inactive?)
          (nil? (node/attribute closer :meta)))
    (do
      (node/attribute opener :skip? true)
      (node/attribute closer :skip? true))
    (do
      (def kind (if (node/attribute opener :image?) :image :link))
      (array/push (node/children-of opener) [:open @{:kind kind} @[]])
      (node/attribute opener :count 0)
      (array/push (node/children-of closer) [:close @{:kind kind :meta (node/attribute closer :meta)} @[]])
      (node/attribute closer :count 0)
      (unless (node/attribute opener :image?)
        (var i open-i)
        (while (def prev-opener (get delimiters (-- i)))
          (when (and (node/attribute prev-opener :left?)
                     (= :link (node/attribute prev-opener :kind))
                     (not (node/attribute prev-opener :image?)))
            (node/attribute prev-opener :inactive? true)))))))

(util/add-to state/protocols
  {:inlines
    {:image    {:close    link/image-close}
     :link     {:close    link/link-close
                :match?   link/link-match?
                :match-up link/link-match-up}}})

# TODO Simplify this

(util/add-to state/priorities
  {:levels [1 0]
   :kinds  {:emphasis 0
            :link     1}})

(comment import ./inlines/hardbreak :prefix "")
## Grammar

(defn- hardbreak/hardbreak []
  [:hardbreak])

(def hardbreak/grammar
  ~{:hardbreak (/ (+ "\\\n" (* (at-least 2 :space) "\n")) ,hardbreak/hardbreak)})

(comment import ./inlines/rawhtml :prefix "")
## Grammar

(defn- rawhtml/rawhtml [content]
  [:rawhtml @{} content])

(def rawhtml/grammar
  ~{:rawhtml {:main        (/ '(+ :opening-tag :closing-tag :comment :instruction :declaration :cdata) ,rawhtml/rawhtml)
              :name        (* :alpha (any (+ :alnum "-")))
              :attribute   {:main    (* :ws :name (? :spec))
                            :name    (* (+ :alpha (set "_:")) (any (+ :alnum (set "_.:-"))))
                            :spec    (* (? :ws) "=" (? :ws) (+ :unquote :single :double))
                            :unquote (some (if-not (+ :ws (set "\"'=<>`")) 1))
                            :single  (* "'" (to "'") "'")
                            :double  (* `"` (to `"`) `"`)}
              :opening-tag (* "<" :name (any :attribute) (any :ws) (? "/") ">")
              :closing-tag (* "</" :name (any :ws) ">")
              :bad-comment (* "<!--" (any "-") ">")
              :ok-comment  (* "<!--" (to "-->") "-->")
              :comment     (+ :bad-comment :ok-comment)
              :instruction (* "<?" (thru "?>"))
              :declaration (* "<!" (some (range "AZ")) (some :ws) (some (if-not ">" 1)) ">")
              :cdata       (* "<![CDATA[" (thru "]]>"))}})


(array/push inlines/inlines
  :codespan
  :rawhtml
  :autolink
  :hardbreak
  :emphasis
  :link
  :text)

(def inlines/grammar
  ~@{:main :inlines
     :alpha (range "AZ" "az")
     :alnum (+ :alpha (range "09"))
     :utf8-char (+ (range "\x00\x7F")                                                               # 1-byte ASCII
                   (* (range "\xC0\xDF") (range "\x80\xBF"))                                        # 2-byte
                   (* (range "\xE0\xEF") (range "\x80\xBF") (range "\x80\xBF"))                     # 3-byte
                   (* (range "\xF0\xF7") (range "\x80\xBF") (range "\x80\xBF") (range "\x80\xBF"))) # 4-byte
     # Unicode punctuation - cmt fails when function returns nil/false
     :upunc (cmt (capture :utf8-char) ,inlines/check-unicode-punc)
     # ASCII punctuation only - for escaping and delimiter detection
     :ascii-punc (set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
     # all punctuation (ASCII + Unicode) - for delimiter detection
     :punc (+ :ascii-punc (drop :upunc))
     :ws    (set " \n\r\v\t")
     :nl    "\n"
     :blank (* "\n" (any :space) "\n")
     :space (set " \t")
     :char    (if-not (+ (set "&<>*_[]!`\\") (* (at-least 2 :space) "\n")) '1)
     :entity  (/ (+ (* (constant :ent) '(* "&" (some (+ :w :d)) ";"))
                    (* (constant :dec) '(* "&#" (between 1 7 :d) ";"))
                    (* (constant :hex) '(* "&#" (set "Xx") (between 1 6 :h) ";")))
                 ,util/entity-decode)
     :escaped (+ (* "\\" ':ascii-punc)  # Only ASCII punctuation can be escaped
                 '(* "\\" (if-not :nl 1)))
     :inlines (any :inline)
     :inline  (+ ,(tuple ;inlines/inlines))
     :trail (* :space ':nl)
     :text  (+ (% (some (+ :escaped :entity :trail :char))) '1)})

(util/add-to inlines/grammar autolink/grammar)
(util/add-to inlines/grammar codespan/grammar)
(util/add-to inlines/grammar emphasis/grammar)
(util/add-to inlines/grammar hardbreak/grammar)
(util/add-to inlines/grammar link/grammar)
(util/add-to inlines/grammar rawhtml/grammar)

## Inline functions

(defn- inlines/default-inline-close [node delim ancestors]
  (array/pop ancestors))

## Inline default protocol

(util/add-to state/protocols
  {:inlines
    {'default {:close inlines/default-inline-close}}})

(comment import ./parser :prefix "")
(comment import ./state :prefix "")

(comment import ./util :prefix "")

(comment import ./node :prefix "")


## Blocks

(defn- parser/append-to-document [document line grammar protocols]
  (set state/col-edge 0)
  (set state/col-pos 0)
  (var block nil)
  (var pos 0)
  (var parent-node document)
  (var peer-node (node/next-container parent-node))
  (defn contains-block? [curr-b]
    (not (nil? (node/next-child curr-b))))
  # TODO: Is this too too complicated?
  (defn descend []
    (defn descend-1 []
      (def next-block-fn (node/get-fn :next-block peer-node protocols))
      (def [new-b new-pos] (next-block-fn peer-node line pos grammar protocols))
      (var curr-b new-b)
      (var next-b (node/next-container curr-b))
      (while next-b
        (def equal-fn (node/get-fn :equal? peer-node protocols))
        (if (equal-fn peer-node curr-b)
          (do
            (set parent-node peer-node)
            (set peer-node (node/next-container peer-node))
            (set curr-b next-b)
            (set next-b (node/next-container next-b)))
          (set next-b nil)))
      (set block curr-b)
      (set pos new-pos)
      (def equal-fn (node/get-fn :equal? peer-node protocols))
      (if (and (< pos (length line)) # TODO: Is this a hack or should there be a check here?
               (equal-fn peer-node curr-b))
        [true (node/next-container peer-node)]
        [false nil]))
    (var keep-descending? true)
    (while (and (< pos (length line))
                keep-descending?)
      (def [match? next-n] (descend-1))
      (if match?
        (if (nil? next-n)
          (do
            (set keep-descending? false)
            (def [next-b next-pos] (peg/match grammar line pos))
            (set block next-b)
            (set pos next-pos)
            (set parent-node peer-node))
          (do
            (set parent-node peer-node)
            (set peer-node next-n)))
        (set keep-descending? false))))

  (defn lazy-continuation []
    (when-let [lazy-fn     (node/get-fn :lazy? block protocols)
               _           (lazy-fn block)
               last-n      (node/last-descendant parent-node)
               _           (node/attribute last-n :open?)
               follower-fn (node/get-fn :follower block protocols)
               follow-b    (follower-fn block last-n)]
      (set block follow-b)
      last-n))

  (defn realise-block []
    (var curr-b (or (node/last-descendant block) block))
    (while (< pos (length line))
      (def [next-b next-pos] (peg/match grammar line pos))
      (def append-fn (node/get-fn :append curr-b protocols))
      (append-fn curr-b next-b protocols)
      (set curr-b (or (node/last-descendant next-b) next-b))
      (set pos next-pos)))

  (defn append []
    (if (= :blank (node/type-of block))
      (do
        (var prev-n parent-node)
        (var curr-n (node/next-container prev-n))
        (var stopped? false)
        (while (and curr-n (not stopped?))
          (def see-blank-fn (node/get-fn :see-blank curr-n protocols))
          (if (see-blank-fn curr-n protocols)
            (set stopped? true)
            (do
              (set prev-n curr-n)
              (set curr-n (node/next-container curr-n)))))
        (if stopped?
          ((node/get-fn :blank curr-n protocols) curr-n prev-n protocols)
          (do
            (set prev-n parent-node)
            (set curr-n (node/next-container prev-n))
            (while curr-n
              (def blank-fn (node/get-fn :blank curr-n protocols))
              (def prev-prev-n prev-n)
              (set prev-n curr-n)
              (set curr-n (blank-fn prev-n prev-prev-n protocols))))))
      (do
        (def append-fn (node/get-fn :append parent-node protocols))
        (append-fn parent-node block protocols))))

  # Match against existing nodes
  (descend)

  # Either continue direct, continue lazy or close children
  (unless (= :blank (node/type-of block))
    (when-let [peer-node (node/next-child parent-node)
               equal-fn  (node/get-fn :equal? peer-node protocols)
               _         (not (equal-fn peer-node block))]
      (if (def last-node (lazy-continuation))
        (set parent-node last-node)
        (node/close-children parent-node protocols))))

  # Realise block
  (realise-block)

  # Append to parent node
  (unless (nil? block)
    (append)))


(defn parser/parse-blocks [input grammar protocols]
  (state/reset-block-globals)
  # Normalize line endings (CRLF -> LF) per CommonMark spec
  (def normalized-input (string/replace-all "\r\n" "\n" input))
  (def block-protocols (get protocols :blocks))
  (def document [:document @{:container? true :open? true} @[]])
  (def ends (string/find-all "\n" normalized-input))
  (var start 0)
  (each end ends
    (state/reset-cols)
    (def line (string/slice normalized-input start (inc end)))
    (parser/append-to-document document line grammar block-protocols)
    (set start (inc end)))
  (when (< start (length normalized-input)) # TODO: Is this necessary?
    (state/reset-cols)
    (def line (string/slice normalized-input start))
    (parser/append-to-document document line grammar block-protocols))
  (node/close-children document block-protocols)
  document)

## Inlines

(defn- parser/append-element [node element]
  (def children (node/children-of node))
  (def last-child (array/peek children))
  (if (bytes? element)
    (if (buffer? last-child)
      (buffer/push last-child element)
      (array/push children (buffer element)))
    (array/push children element)))

(defn- parser/parse-inlines [text grammar protocols priorities]
  (state/reset-inline-globals)
  (defn priority [kind]
    (-> (get priorities :kinds) (get kind)))
  (defn match? [open-i close-i]
    (def opener (get state/delimiters open-i))
    (def closer (get state/delimiters close-i))
    (when (= (node/attribute opener :kind) (node/attribute closer :kind))
      (def match-fn (-> (get protocols (node/attribute opener :kind)) (get :match?)))
      (match-fn open-i close-i state/delimiters)))
  (defn match-up [open-i close-i]
    (def opener (get state/delimiters open-i))
    (def match-up-fn (-> (get protocols (node/attribute opener :kind)) (get :match-up)))
    (match-up-fn open-i close-i state/delimiters text))
  (defn matching-pos [closer close-i]
    (var open-i close-i)
    (while (def opener (get state/delimiters (-- open-i)))
      (when (and (= (node/attribute closer :kind)
                    (node/attribute opener :kind))
                 (zero? (node/attribute opener :count)))
        (break)))
    open-i)
  # Create inline elements
  (def elements (peg/match grammar text))
  # Match delimiters
  (each level (get priorities :levels)
    (var close-i 1)
    (while (def closer (get state/delimiters close-i))
      (var open-i close-i)
      (when (and (node/attribute closer :right?)
                 (not (zero? (node/attribute closer :count)))
                 (not (node/attribute closer :skip?))
                 (= level (priority (node/attribute closer :kind))))
        (def discards @[])
        (while (def opener (get state/delimiters (-- open-i)))
          (def opener-level (priority (node/attribute opener :kind)))
          (cond
            # If the opener level has higher priority than the current level,
            # we are either inside a pair or outside pair. If we are inside a
            # pair, this closing delimiter cannot match anything and should be
            # skipped. If we are outside a pair, we need to skip to the
            # matching delimiter of this pair before we continue searching.
            (and (> opener-level level)
                 (zero? (node/attribute opener :count))
                 (not (zero? (length (node/children-of opener)))))
            (if (= :open (node/type-of (last (node/children-of opener))))
              (do
                (node/attribute closer :skip? true)
                (++ close-i)
                (break))
              (set open-i (matching-pos opener open-i)))
            (= opener-level level)
            (if (match? open-i close-i)
              (when (match-up open-i close-i)
                (each discard discards (node/attribute discard :skip? true))
                (break))
              (array/push discards opener)))))
      (when (or (= open-i close-i) (< open-i 0))
        (++ close-i))))
  # Build tree of elements
  (def root [:root @{} @[]])
  (def ancestors @[])
  (var curr root)
  (each element elements
    (if (not= :delims (node/type-of element))
      (parser/append-element curr element)
      (do
        # If there are any leftover characters in the delimiter we need to
        # decide either to append them to the parent either before or after
        # appending the inline elements. If the first child is :open, the
        # leftover characters should be appended first. Otherwise, the
        # characters should be appended after the inline elements.
        (def leftovers
          (unless (zero? (node/attribute element :count))
            (string/slice text
                          (node/attribute element :start-pos)
                          (node/attribute element :end-pos))))
        (def prepend-leftovers?
          (and (not= 0 (length (node/children-of element)))
               (= :open (node/type-of (first (node/children-of element))))))
        (when (and leftovers prepend-leftovers?)
          (parser/append-element curr leftovers))
        (each delim (node/children-of element)
          (if (= :open (node/type-of delim))
            (do
              (array/push ancestors curr)
              (set curr [(node/attribute delim :kind) @{} @[]])
              (array/push (node/children-of (array/peek ancestors)) curr))
            (do
              (def close-fn (node/get-fn :close curr protocols))
              (set curr (close-fn curr delim ancestors)))))
        (when (and leftovers (not prepend-leftovers?))
          (parser/append-element curr leftovers)))))
  # Return root
  (node/children-of root))

(defn- parser/walk-tree [node grammar protocols priorities]
  (def children (node/children-of node))
  (if (node/attribute node :container?)
    (each child children
      (parser/walk-tree child grammar protocols priorities))
    (when (node/attribute node :inlines?)
      (def text (-> (string/join children "\n") string/trim))
      (array/clear children)
      (array/concat children (parser/parse-inlines text grammar protocols priorities)))))

(defn parser/parse-all-inlines
  ```
  Parses all inline nodes within `document`

  Returns an updated `document`.
  ```
  [document grammar protocols priorities]
  (parser/walk-tree document grammar (get protocols :inlines) priorities)
  document)

(comment ./renderers/html)

(defn r/parse-md
  ```
  Parses Markdown into an AST

  This function transforms `input` into an AST. It optionally takes a number of
  named parameters for extending the parser:

  - `:blocks`: custom blocks PEG
  - `:inlines`: custom inlines PEG
  - `:protocols`: custom block/inline protocols
  - `:priorities`: custom delimiter priorities for inlines

  There are helper utilities for extension in lib/extend.janet.
  ```
  [input &named blocks inlines protocols priorities]
  (default blocks (peg/compile blocks/grammar))
  (default inlines (peg/compile inlines/grammar))
  (default protocols state/protocols)
  (default priorities state/priorities)
  (-> (string/trimr input " \t\v")
      (parser/parse-blocks blocks protocols)
      (parser/parse-all-inlines inlines protocols priorities)))

(comment render-html
  ```
  Renders an AST node to HTML

  This functions transforms `root` into an HTML string. It can also be called
  with a struct/table `opts` with the following keys:

  - `:renderers`: custom renderer functions
  - `:start-nl?`: start with newline
  - `:inner?`: skip wrapper tags for paragraphs

  Render functions have the signature `(defn my-renderer [node renderers opts] ...)`.
  ```
  [root &opt opts]
  (html/render root opts))

(defn r/reset-state
  ```
  Resets the global state to defaults

  This function clears all custom grammars and protocols that were added via
  the extend module, resetting the state to the default configuration. This is
  primarily useful for testing when you want to ensure a clean state between
  tests.
  ```
  []
  (array/clear state/blocks)
  (array/push state/blocks ;blocks/blocks)
  (array/clear state/inlines)
  (array/push state/inlines ;inlines/inlines)
  (table/clear state/custom-block-grammars)
  (table/clear state/custom-block-protocols)
  (table/clear state/custom-inline-grammars)
  (table/clear state/custom-inline-protocols)
  (buffer/clear state/custom-inline-delimiters))

(comment import ./utils :prefix "")
(defn u/has-janet-shebang?
  [path]
  (with [f (file/open path)]
    (def first-line (file/read f :line))
    (when first-line
      # some .js files have very long first lines and can contain
      # a lot of strings...
      (and (string/find "bin/env" first-line)
           (string/find "janet" first-line)))))

(defn u/looks-like-janet?
  [path]
  (or (string/has-suffix? ".janet" path)
      (u/has-janet-shebang? path)))



(def version "2026-05-07_10-42-36")

(def usage
  `````
  Usage: jdcheck <file-or-dir>...

         jdcheck [-h|--help]|[-v|--version]

  Check Janet docstrings.

  Parameters:

    <file-or-dir>          path to file or directory

  Options:

    -h, --help             show this output
    -v, --version          show version information

  Examples:

    Check docstrings in the file `data/has-heading.janet`:

    $ jdcheck data/has-heading.janet

    Check docstrings in `data` directory:

    $ jdheck data
  `````)

########################################################################

(defn check-node
  [a-node]
  (def head (get a-node 0))
  (when (not (keyword? head))
    (break true))
  #
  (case head
    # unsupported blocks
    :blank false
    :blockquote false
    :heading false
    :html false
    :linkdef false
    :t-break false
    # unsupported inlines
    :autolink false
    :hardbreak false
    :link false
    :rawhtml false
    # supported
    :document true
    # XXX: though not fenced with tildes
    :codeblock true
    :codespan true
    :emphasis true
    :list true
    :list-item true
    :paragraph true))

(defn main
  [_ & args]
  (def opts (a/parse-args args))
  #
  (when (get opts :show-help)
    (print usage)
    (os/exit 0))
  #
  (when (get opts :show-version)
    (print version)
    (os/exit 0))
  #
  (def paths (get opts :rest))
  #
  (def src-filepaths (filter |(and (= :file (os/stat $ :mode))
                                   (u/looks-like-janet? $))
                             (e/itemize ;paths)))
  (when (empty? src-filepaths)
    (eprintf "failed to find relevant paths for: %s"
             (string/join paths " "))
    (os/exit 1))
  #
  (each sfp src-filepaths
    (def src (slurp sfp))
    (when (not (empty? src))
      (def results
        (try
          (f/find-docs src)
          ([e]
            (eprint e)
            (eprintf "find-docs failed for: %s" sfp))))
      (each r results
        (def text (get r :text))
        (when (and text (not (empty? text)))
          (def tree (r/parse-md text))
          (var t-zloc (c/zip tree))
          (while (def a-zloc (c/df-next t-zloc))
            (when (c/end? a-zloc)
              (break))
            #
            (def a-node (c/node a-zloc))
            (when (not (check-node a-node))
              (def {:bc col :bl line :name name} r)
              (eprintf "%s:%d:%d: docstring of `%s` has: %V"
                       sfp line col name (get a-node 0)))
            (set t-zloc a-zloc)))))))

