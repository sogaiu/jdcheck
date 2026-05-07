#! /usr/bin/env janet

(use ./sh-dsl)

########################################################################

(defn copy-file
  [src dst]
  (spit dst (slurp src)))

########################################################################

(prin "* running jell...") (flush)
(def jell-exit ($ janet ./bin/jell))
(assertf (zero? jell-exit)
         "jell exited: %d" jell-exit)
(print "done")

(prin "* copying jdcheck.janet to jdcheck...")
(copy-file "jdcheck.janet" "jdcheck")
(print "done")

########################################################################

(print "* running niche...")
(def niche-exit ($ janet ./bin/niche.janet))
(assertf (zero? niche-exit)
         "niche exited: %d" niche-exit)
(print "done")

########################################################################

(print "* updating README...")
(def readme-update-ext ($ janet jdcheck -h > README))
(assertf (zero? readme-update-ext)
         "updating README exited: %d" readme-update-ext)
(print "done")

