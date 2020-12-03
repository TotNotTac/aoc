(setv correctPasswords 0
      lines (with [f (open "input.txt")]
              (.readlines f)))

(for [l lines]
  (setv l (.split l ":")
        rules (.split (get l 0) " ")
        (get rules 0) (.split (get rules 0) "-")
        password (get l 1)
        count (.count password (get rules 1))
        lowerBound (get (get rules 0) 0)
        upperBound (get (get rules 0) 1))

  (when (and (>= count (int lowerBound))
             (<= count (int upperBound)))
    (setv correctPasswords (+ 1 correctPasswords))))

(print correctPasswords)
