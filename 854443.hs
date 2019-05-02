--
-- MATHFUN
-- 854443
--
--

import Data.Char
import Data.List
import Data.List (sortBy)
import Data.Ord (comparing)

-- Types

--
-- Define Album type here
data Album = Album{title, artist, genre, own :: String, year, clean :: Int } deriving (Show, Read)

-- testData containing a list of the 50 albumToString

testData :: [Album]
testData = [(Album "Ceremony" "New Order" "Post - Punk" "yes" 1981 6),
            (Album "Substance (LP-1)" "New Order" "Dance - Rock" "yes" 1987 3),
            (Album "Substance (LP-2)" "New Order" "Dance - Rock" "yes" 1987 3),
            (Album "Substance: 1977 - 1980 (LP-1)" "Joy Division" "Post - Punk" "yes" 1988 3),
            (Album "Substance: 1977 - 1980 (LP-2)" "Joy Division" "Post - Punk" "yes" 1988 3),
            (Album "Touched By The Hand Of God" "New Order" "Dance - Rock" "yes" 1987 3),
            (Album "Broken Machine" "Nothing But Thieves" "Alternative Rock" "yes" 2017 3),
            (Album "Record Mirror EP" "Various" "Various" "yes" 1986 3),
            (Album "Temptation" "New Order" "Dance - Rock" "yes" 1982 3),
            (Album "Wish You Were Here" "Pink Floyd" "Rock" "yes" 1975 2),
            (Album "Parallel Lines" "Blondie" "New Wave" "yes" 1978 3),
            (Album "Ghostbusters: Main Theme" "Ray Parker Jr" "Pop" "yes" 1984 1),
            (Album "Hot Fuss" "The Killers" "Alternative Rock" "yes" 2004 1),
            (Album "Vienna" "Ultravox" "New Wave" "yes" 1980 0),
            (Album "What Did You Think?" "Nothing But Thieves" "Alternative Rock" "no" 2018 0),
            (Album "Nothing But Thieves" "Nothing But Thieves" "Alternative Rock" "no" 2015 0),
            (Album "Echoes, Silence, Patience & Grace" "Foo Fighters" "Rock" "no" 2007 0),
            (Album "The Best of Depeche Mode, Vol 1" "Depeche Mode" "Pop/Dance - Rock" "no" 2006 0)]
--
--
--  Your functional code goes here

-- Converts a list of albums to a string

albumsToString :: [Album] -> String
albumsToString  [] = " "
albumsToString  (x:xs) = stringAlbum x ++ "\n" ++ albumsToString xs

-- Formats each category into a column of specified width

stringAlbum :: Album -> String
stringAlbum (Album {title = t, artist = a, year = y, genre = g}) =
   columnify 41 t ++ columnify 21 a ++ columnify 6 (show y) ++ columnify 9 (show g)

-- Gets the length value of the longest piece of data in each field and fills the gap
-- between shorter values and the max length with spaces

columnify :: Int -> String -> String
columnify len value = value ++ (take (len - (length value)) (repeat ' '))

-- Gets the top 10 highest selling albums, sorted in descending order

needClean :: [Album] -> [Album]
needClean albumList = take 10 $ sortBy (flip $ comparing clean) albumList

-- Gets all albums released between and including two user-specified dates

getBetween :: Int -> Int -> [Album] -> [Album]
getBetween fst scd [] = []
getBetween fst scd (x:xs)
      | year x < fst || year x > scd = getBetween fst scd xs
      | year x >= fst && year x <= scd = [x] ++ getBetween fst scd xs

-- Gets all albums starting with a given prefix

getPrefix :: String -> [Album] -> [Album]
getPrefix pref [] = []
getPrefix pref (x:xs)
      | isPrefixOf pref (title x) == False = getPrefix pref xs
      | isPrefixOf pref (title x) == True = [x] ++ getPrefix pref xs

-- Gets the total amount of sales from a given artist

--getSales :: String -> [Album] -> Int
--getSales artist albums = sum [ sales | Album _ a _ sales <- albums, a == artist ]

-- Gets a list of albums and displays a list of artists and the number of their albums in the top 50

albumNumber :: [Album] -> String
albumNumber [] = " "
albumNumber (Album _ art _ _:xs) = columnify 20 art ++ columnify 2 (albumTotal art testData) ++
  " \n" ++ (albumNumber (filter(\(Album _ a _ _) -> art /= a) xs))

-- Gets the total number of albums in the top 50 for each artist

albumTotal :: String -> [Album] -> String
albumTotal artist db = show $ sum  [ 1 | Album _ a _ _ <- db, a == artist ]

-- Adds and album to a list of albums

addAlbum :: String -> String -> String -> String -> Int -> Int -> [Album] -> [Album]
addAlbum title artist genre own year clean db = (Album title artist year genre clean own) : db

-- Removes the last album from the List

removeLast :: [Album] -> [Album]
removeLast xs = tail (init xs)

-- Gets the index of an album and removes it

--removeAlbum :: Int -> [Album] -> [Album]
--removeAlbum n db = do
   --let x = getIndex n db
   --take x db ++ drop (x+1) db

--insertList :: Album -> [Album] -> [Album]
--insertList a db = (insertAt(getIndex(getSales(title a) db) db) a db)

-- Inserts album into a given index

--insertAt :: Int -> Album -> [Album] -> [Album]
--insertAt pos alb db = h1++[alb]++h2
   --where (h1,h2) = (splitAt pos db)
-- Gets the index in the list of an album

--getIndex :: Int -> [Album]-> Int
--getIndex _  [] = (-1)
--getIndex sal (Album _ _ _ sales:xs)
    --| sal == sales = 0
    --| otherwise = 1+ getIndex sal xs

-- Demo function to test basic functionality (without persistence - i.e.
-- testData doesn't change and nothing is saved/loaded to/from albums file).

demo :: Int -> IO ()
demo 1  = putStrLn (albumsToString testData)
demo 2  = putStrLn (albumsToString (needClean testData))
demo 3  = putStrLn (albumsToString (getBetween 1985 1988 testData))
demo 4  = putStrLn (albumsToString (getPrefix "Subst" testData))
--demo 5  = putStrLn (show (getSales "Queen" testData))
demo 6  = putStrLn (albumNumber testData)
--demo 7  = putStrLn (albumsToString (insertList (Album "Progress" "Take That" 2010 2700000) (init testData)))
--demo 8  = putStrLn (albumsToString (insertList (Album "21" "Adele" 2011 5510000) testData))


--
--
-- Your user interface (and loading/saving) code goes here
--

-- Loads the user interface

main :: IO ()
main = do
  db <- getFile
  putStrLn "============================================================================"
  putStrLn "=                             Main menu                                    ="
  putStrLn "============================================================================"
  putStrLn (albumsToString db)
  menu db

-- Reads the album data file

getFile :: IO [Album]
getFile = do
    file <- readFile "albums.txt"
    length file `seq` return (read file :: [Album])

-- Writes the changes to the album data file

exitFile :: [Album] -> IO ()
exitFile albums = do writeFile "albums.txt" (show albums)

-- Main menu, prompts the user to select an option and then calls a function based on their response

menu :: [Album] -> IO ()
menu db = do
    putStrLn "============================================================================"
    putStrLn "\nType one of the following numbers to select an option:\n"
    putStrLn "1: Display the list of albums"
    putStrLn "2: Display the top 10 best-selling albums"
    putStrLn "3: Display albums released between 2 given years"
    putStrLn "4: Display albums with a title contating a given prefix"
    --putStrLn "5: Display the total sales for a given artist"
    putStrLn "6: Display the list of artists and the number of their albums in the top 50"
    --putStrLn "7: Remove the lowest album and add a new given album"
    --putStrLn "8: Increase the sales figure for a given album"
    putStrLn "9: Exit\n"
    putStrLn "============================================================================"
    option <- getLine
    case option of
        "1" -> do putStrLn (albumsToString db) ; menu db
        "2" -> do putStrLn (albumsToString (top10 db)) ; menu db
        "3" -> betweenDates db
        "4" -> choosePrefix db
        --"5" -> getArtistSales db
        "6" -> do putStrLn (albumNumber db) ; menu db
        --"7" -> addNew db
        --"8" -> updateSales db
        "9" -> do
                exitFile db
        _ -> do putStrLn "\nEnter a number"
                menu db

-- The user enters 2 dates and a function displaying all albums released on and between those dates is called

betweenDates :: [Album] ->  IO ()
betweenDates db = do
    putStrLn "Enter the first year:"
    year1 <- toInt
    putStrLn "Enter the second year:"
    year2 <- toInt
    putStrLn (albumsToString (getBetween year1 year2 db))
    menu db

-- The user enters a prefix and a function displaying all albums containing that prefix is called

choosePrefix :: [Album] -> IO ()
choosePrefix db = do
    putStrLn "Enter the prefix:"
    pref <- getLine
    putStrLn (albumsToString (getPrefix pref db))
    menu db

-- The user enters an artist's name and a function displaying the total sales of albums by that artist is called

--getArtistSales :: [Album] -> IO ()
--getArtistSales db = do
    --putStrLn "Enter the artist's name:"
    --art <- getLine
    --putStrLn (show (getSales art db))
    --menu db

-- Removes the lowest selling album from the list and adds a new album specified by the user

--addNew :: [Album] -> IO ()
--addNew db = do
    --putStrLn "Enter album title:"
    --ttl <- getLine
    --putStrLn "Enter album artist:"
    --art <- getLine
    --putStrLn "Enter album year:"
    --year <- toInt
    --putStrLn "Enter album sales:"
    --sal <- toInt
    --let updateAlbums = (insertList (Album ttl art year sal) $ (init db))
    --putStrLn (albumsToString updateAlbums)
    --menu updateAlbums

-- The user enters a title, artist, release year and sales figure then a function removing the existing entry of that album and adding the new entry is called

--updateSales :: [Album] -> IO ()
--updateSales db = do
   --putStrLn "Enter new album title:"
   --ttl <- getLine
   --putStrLn "Enter new album artist:"
   --art <- getLine
   --putStrLn "Enter new album release year:"
   --year <- toInt
   --putStrLn "Enter new album sales amount:"
   --sal <- toInt
   --let removedAlbum = (removeAlbum sal db)
   --let updateAlbums = (insertList (Album ttl art year sal) removedAlbum)
   --putStrLn (albumsToString updateAlbums)
   --menu updateAlbums

-- Reads the user's input as an integer

toInt :: IO Int
toInt = do
   n <- getLine
   return (read n :: Int)
