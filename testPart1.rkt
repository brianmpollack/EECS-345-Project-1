(load "interpreter.scm")
(define test
  (lambda (file expected)
    (if (eq? (interpret file) expected) (string-append "Passed: " file) (string-append "Failed: " file))))

(test "Tests/1.javaish" 150)
(test "Tests/2.javaish" -4)
(test "Tests/3.javaish" 10)
(test "Tests/4.javaish" 16)
(test "Tests/5.javaish" 220)
(test "Tests/6.javaish" 5)
(test "Tests/7.javaish" 6)
(test "Tests/8.javaish" 10)
(test "Tests/9.javaish" 5)
(test "Tests/10.javaish" -39)
;(test "Tests/11.javaish" 20) ;Should return error
;(test "Tests/12.javaish" 20) ;Should return error
;(test "Tests/13.javaish" 20) ;Should return error
;(test "Tests/14.javaish" 12) ;Should return error
(test "Tests/15.javaish" 'true)
(test "Tests/16.javaish" 100)
(test "Tests/17.javaish" 'false)
(test "Tests/18.javaish" 'true)
(test "Tests/19.javaish" 128)
(test "Tests/20.javaish" 12)