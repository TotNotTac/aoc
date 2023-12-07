(setv correctPasswords 0
      lines (with [f (open "input.txt")]
              (.readlines f)))

(for [l lines]
  (setv l (.split l ":")
        rules (.split (get l 0) " ")
        (get rules 0) (.split (get rules 0) "-")
        password (get l 1)
        matchChar (get rules 1)
        index1 (int (get (get rules 0) 0))
        index2 (int (get (get rules 0) 1)))

  (when (^ (= (get password index1) matchChar)
           (= (get password index2) matchChar))
    (setv correctPasswords (+ 1 correctPasswords))))

(print correctPasswords)
