(load "project1.scm")
(define test
  (lambda (file expected)
    (if (eq? (interpret file) expected) (string-append "Passed: " file) (string-append "Failed: " file))))

(test "TestsPart2/1.javaish" 20)
(test "TestsPart2/2.javaish" 164)
(test "TestsPart2/3.javaish" 32)
(test "TestsPart2/4.javaish" 2)
;(test "TestsPart2/5.javaish" 20) ;Should return error
(test "TestsPart2/6.javaish" 25)
(test "TestsPart2/7.javaish" 21)
(test "TestsPart2/8.javaish" 6)
(test "TestsPart2/9.javaish" -1)
(test "TestsPart2/10.javaish" 789)
;(test "TestsPart2/11.javaish" 20) ;Should return error
;(test "TestsPart2/12.javaish" 20) ;Should return error
;(test "TestsPart2/13.javaish" 20) ;Should return error
(test "TestsPart2/14.javaish" 12)
(test "TestsPart2/15.javaish" 125)
(test "TestsPart2/16.javaish" 110)
(test "TestsPart2/17.javaish" 2000400)
(test "TestsPart2/18.javaish" 101)
;(test "TestsPart2/19.javaish" 20) ;Should return error
;(test "TestsPart2/20.javaish" 21) ;Not required to pass