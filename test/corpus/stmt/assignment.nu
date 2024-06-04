=====
assignment-001-smoke-test
=====

$var = 42

-----

(nu_script
  (assignment
    (val_variable
      (identifier))
    (val_number)))

=====
assignment-002-semicolon
=====

$var += 69;

-----

(nu_script
  (assignment
    (val_variable
      (identifier))
    (val_number)))

=====
assignment-003-assignment-to-a-pipeline
=====

$x += 1 | $in + 10
# Note that rhs of ++= is not a pipeline, but only 1
# currently nushell parses this as two statements
# $x ++= 1 and  $in + 10. Therefore you will get error
# about adding int to nothing, and $x increased by one
# EChizhov: While this creates an error in nushell this
# shouldn't be handled by treesitter as it is grammatically
# correct - TS doesn't know what $in has.

-----

(nu_script
  (assignment
    (val_variable
      (identifier))
        (val_number))
      (pipeline
        (pipe_element
          (expr_binary
            (val_variable)
            (val_number))))
    (comment)
    (comment)
    (comment)
    (comment)
    (comment)
    (comment)
    (comment))
