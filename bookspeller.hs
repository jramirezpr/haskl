-- create spelling book phrase for a single word
phrase :: [Char] -> [Char]
phrase word =(word !! 0):" is for " ++word
-- addcomma creates a list with the spelling book phrases, and adds ", " for the head of the list
addcomma :: [[Char]] -> [[Char]]
addcomma []=[]
addcomma (x:xs) 
    |(length xs) < 1 = [phrase x]
    |otherwise  = (phrase x++", "): addcomma xs 
--speller: if no word list, blank string
speller :: [[Char]] -> [Char]
speller []=""
speller words -- if one word, returns its phrase	
    |((length words) ==1) = phrase (words!!0)
    |((length words) >1) =  (foldl (++) "" (init (addcomma words)))++ "and "++ (phrase(last words))
    -- concatenate the formatted phrases, the last entry needs an and  