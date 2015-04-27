(set! user-initial-environment (make-top-level-environment))

(environment-define user-initial-environment
                    'generic-evaluation-environment
                    (extend-top-level-environment user-initial-environment))

(define generic-evaluation-environment
  (access generic-evaluation-environment user-initial-environment))

(load "utils" user-initial-environment)
(load "argument-matching" user-initial-environment)
(load "bipartite-matching" user-initial-environment)
(load "ghelper" user-initial-environment)
(load "syntax" user-initial-environment)
(load "rtdata" user-initial-environment)
(load "library" user-initial-environment)

(load "analyze" generic-evaluation-environment)
(load "repl" generic-evaluation-environment)

(ge generic-evaluation-environment)
