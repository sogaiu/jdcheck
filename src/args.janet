(defn parse-args
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

