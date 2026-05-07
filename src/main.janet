(import ./args :as a)
(import ./cm-zipper :as c)
(import ./empathy :as e)
(import ./find :as f)
(import ./remarkable/remarkable :as r)
(import ./utils :as u)

(def version "DEVEL")

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

