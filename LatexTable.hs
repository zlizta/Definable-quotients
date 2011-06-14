-- Thomas ANBERREE 
-- June 2011

-- Helpers to write the LaTeX files 
-- and tests
-- for the paper Definable Quotients in Type Theories


import Data.List

{- Definition of {phi}_i:N, a family of natural sequences such that

* they are pairwise distinct

* for any finite sequence of natural numbers there are sequences with that prefix.

-}

-- Version 1
phi i 0     | even i = 0 
            | odd  i = phi (i `div` 2)  0 + 1
            
phi i (j+1) | even i = phi (i `div` 2) j
            | odd  i = phi (i `div` 2) (j+1)
-- Version 2
phi' = phi
    where
    phi i j             = sequences !! i !! j
    
    sequences            = repeat 0 : (tail (startWithAtLeast 0))
    
    startWithAtLeast n   = interleave (startWith n) (startWithAtLeast (n+1))
    startWith n          = map (n:) sequences
    interleave (x:xs) ys = x : interleave ys xs
    

-- Version 3
phi'' = phi
    where
    phi = startWithAtLeast 0
    
    startWithAtLeast n         = interleave (startWith n) (startWithAtLeast (n+1))
    
    startWith        n i 0     = n
    startWith        n i (j+1) = phi i j
    
    interleave fs gs 0     = fs 0 
    interleave fs gs (i+1) = interleave gs (fs . (+1)) i 

test = and [ phi'' i j == phi' i j  && phi i j == phi' i j | i <- [0..100],j<-[0..200]]

         
-- for testing:
c j = map (flip phi j) [0..]

evens (x:_:xs) = x:evens xs
odds  (_:x:xs) = x:odds  xs

rank n = evens . (expunge n)

expunge 0 = id
expunge n = odds . (expunge (n-1)  )

r i = [phi i j  | j <- [0..5]]

t = filter (all (<5)) [r j | j <- [0..1000000]]


-- To generate a LaTeX table showing the matrix phi i j 
main = putStrLn latex
latex = "\\[\\begin{array}{|" ++ concat ["c|" | _ <-[0..nc]] ++ "}\n\\hline\n" 
    ++ concat (intersperse " & " ["\\varphi_{"++show c++"}" | c <- [0..nc]]) ++ "\\\\\n\\hline\n"
    ++ concat
    (intersperse "\\\\\n"
     [ concat (intersperse " & " 
                [show  (phi c r)
                | c <- [0..nc]])
             |  r <- [0..nr]])
    ++ "\\\\\n"
    ++ concat (intersperse " & " ["\\vdots" | c <- [0..nc]]) 
    ++ "\\\\\n\\hline\n\\end{array}\\]"
    where
    nc = 20
    nr = 5




