
Module Lib where


enumerate = zip [0..]

enumerateMap f = map f . enumerate
