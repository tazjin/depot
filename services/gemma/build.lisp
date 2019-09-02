(require :asdf)
(require :sb-posix)

(push (format nil "~A/" (sb-posix:getcwd)) asdf:*central-registry*)
(asdf:operate 'asdf:program-op :gemma)
