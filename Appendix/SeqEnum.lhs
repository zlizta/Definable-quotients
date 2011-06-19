% -- Thomas Anberree -- June 2011
%include lhs2TeX.fmt
%include polycode.fmt


%import Data.List
%import Data.List


\subsection{Version 1}
\begin{code}
phi                  :: Int -> (Int -> Int)
phi i j              = sequences !! i !! j

sequences            = [0,0..] : (tail (startWithAtLeast 0))

startWithAtLeast n   = interleave  (startWith n) 
                                   (startWithAtLeast (n+1))
startWith n          = map (n:) sequences
interleave (x:xs) ys = x : interleave ys xs
\end{code} 

\subsection{Version 2}

\begin{code}
phi                        :: Int -> (Int -> Int)
phi                        = startWithAtLeast 0

startWithAtLeast n         = interleave (startWith n) 
                                        (startWithAtLeast (n+1))

startWith        n i 0     = n
startWith        n i (j+1) = phi i j

interleave fs gs 0         = fs 0 
interleave fs gs (i+1)     = interleave gs (fs . (+1)) i 
\end{code}

\subsection{Version 3}

\begin{code}
phi                  :: Int -> (Int -> Int)
phi i 0     | even i =  0 
            | odd  i =  phi (i `div` 2)  0 + 1         
phi i (j+1) | even i =  phi (i `div` 2) j
            | odd  i =  phi (i `div` 2) (j+1)
\end{code}

   

