test_that("hri3:iaa works for example admission problem", {
  # setup
  s.prefs <- matrix(c(1,2,3,
                      1,2,3,
                      1,2,3,
                      2,1,3,
                      2,1,3),
                    byrow = FALSE, ncol = 5, nrow = 3);
  c.prefs <- matrix(c(1,4,2,3,5,
                      5,2,3,4,1,
                      1,2,3,4,5),
                    byrow = FALSE, ncol = 3, nrow = 5);
  nSlots <- c(2,2,2)
  
  # round 1:
  # college : proposers : accepted : rejected
  # 1       : 1 2 3     : 1 2      : 3
  # 2       : 4 5       : 5 4      : 
  # 3       :           :          :
  #
  # round 2:
  # college : proposers : accepted : rejected
  # 1       :           :          : 
  # 2       : 3         :          : 3
  # 3       :           :          :
  #
  # round 3:
  # college : proposers : accepted : rejected
  # 1       :           :          : 
  # 2       :           :          :
  # 3       : 3         : 3        :
 
  # function call with short_match = TRUE
  res <- hri3(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots)
  
  # tests
  expect_equal(res$s.prefs, s.prefs)
  expect_equal(res$c.prefs, c.prefs)
  expect_equal(res$iterations, 3)
  expect_equal(res$matchings, data.frame(student=1:5, college=c(1, 1, 3, 2, 2)))
  expect_equal(res$singles, numeric(0))
  expect_equal(res$free_cap, c(0, 0, 1))
})

test_that("hri3:da works for example admission problem", {
  # setup
  s.prefs <- matrix(c(1,2,3,
                      1,2,3,
                      1,2,3,
                      2,1,3,
                      2,1,3),
                    byrow = FALSE, ncol = 5, nrow = 3);
  c.prefs <- matrix(c(1,4,2,3,5,
                      5,2,3,4,1,
                      1,2,3,4,5),
                    byrow = FALSE, ncol = 3, nrow = 5);
  nSlots <- c(2,2,2)
  
  # round 1:
  # college : proposers : accepted : rejected 
  # 1       : 1 2 3     : 1 2      : 3        
  # 2       : 4 5       : 5 4      :          
  # 3       :           :          :          
  #
  # round 2:
  # college : proposers : accepted : rejected
  # 1       :           : 1 2      : 
  # 2       : 3         : 5 3      : 4
  # 3       :           :          :
  #
  # round 3:
  # college : proposers : accepted : rejected
  # 1       : 4         : 1 4      : 2
  # 2       :           : 5 3      :
  # 3       :           :          :  
  #
  # round 4:
  # college : proposers : accepted : rejected
  # 1       :           : 1 4      :  
  # 2       : 2         : 5 2      : 3
  # 3       :           :          :  
  #
  # round 5:
  # college : proposers : accepted : rejected
  # 1       :           : 1 4      :  
  # 2       :           : 5 2      :
  # 3       : 3         : 3        : 
 
  # function call with short_match = TRUE
  res <- hri3(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots, acceptance = "deferred")
  
  # tests
  expect_equal(res$s.prefs, s.prefs)
  expect_equal(res$c.prefs, c.prefs)
  expect_equal(res$iterations, 5)
  expect_equal(res$matchings, data.frame(student=1:5, college=c(1, 2, 3, 1, 2)))
  expect_equal(res$singles, numeric(0))
  expect_equal(res$free_cap, c(0, 0, 1))
})