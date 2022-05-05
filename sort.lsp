;This is a version of bubble sort that first finds the max number in a list
;of numbers, pushes that number to a new list, then removes it from the old list
;doing this until the new list is a sorted version of the old list

;SAMPLE OUTPUT:
;Break 52 [54]> (mainSortFunction)
;"how many numbers do you want to sort?"10
;"enter numbers. Press enter after each"11
;3
;44
;12
;6
;7
;8
;23
;14
;29
;(3 6 7 8 11 12 14 23 29 44)
;Break 52 [54]>

;------------------------------------------------------------------------------
;main function that calls a helper method to get input for list of numbers to be
;sorted and then calls another helper function to sort list
(defun mainSortFunction()
  (write "how many numbers do you want to sort?")
  (setq total (read))

  ;call getnumlist to prompt user to input a list of numbers, then assigns it to
  ;numsToSort
  (setq numsToSort (getnumlist total))

  ;call function to sort numsToSort
  (sortnums numsToSort (list))
)

;function to get input for n elements and push them to a list
(defun getnumlist(n)
  (write "enter numbers. Press enter after each")
  (defparameter nums '())

  (loop for a from 1 to n
    do(
      push (read) nums
   )
  )
  nums
  )

;recursive function that returns a sorted list of numbers
(defun sortnums(nums sortedNums)
  (setq l (length nums))

  ;base case: if length of nums is 0 return sortedNums
  (cond ((= l 0) (return-from sortnums sortedNums))
  ;else get index of max value in current list
        (t (setq maxn (maxnum nums 0 0))))
  ;set nextMax to value of max value in current list
  (setq nextMax (nth maxn nums))
  ;push nextMax to sortedNums list
  (push nextMax sortedNums)
  ;call removeElement function to remove current max value from old list
  (setq numsminusn (removeElement nums maxn))
  ;recursive call with shortend old list and sortedNums list
  (sortnums numsminusn sortedNums)
)

(defun removeElement(nums n)
  ;returns new list after removing element at n index
  (setq l (- (length nums) 1))
  (setq newnums '())
  (loop for a from 0 to l
    if (/= a n)
    do(
      push (nth a nums) newnums
    )
  )

 newnums
)

(defun maxnum(nums n maxn)
  ;returns the index of the max value in a list of numbers
  (setq nplus1 (+ n 1))
  (setq l (- (length nums) 0))
  (setq current (nth n nums))
  (setq maxval (nth maxn nums))

  ; base case: return index of max value
  (cond ((= n l)  maxn)
        ;if maxval is less than current recursive call with current index as maxn
        ((< maxval current) (maxnum nums nplus1 n));
        ;else recursive call with same maxn index
        (t (maxnum nums nplus1 maxn))
  )

)
