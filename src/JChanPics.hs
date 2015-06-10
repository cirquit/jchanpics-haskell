{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns, ViewPatterns #-}

import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text (unpack, append, Text)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

import Control.Concurrent.ParallelIO.Global (parallel_, stopGlobalPool)
import System.IO
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getProgName)

import ChanTypes


getPageThreads :: Int -> [Page] -> [Integer]
getPageThreads n []     = error $ "Some error happend while fetching on " ++ show n ++ " page :/"
getPageThreads 1 (x:xs) = map no $ threads x
getPageThreads n (x:xs) = getPageThreads (n-1) xs

getThreadByID :: String  -- board
              -> Integer -- threadnumber
              -> IO (Thread)
getThreadByID board ident = do
    res <- simpleHttp ("http://a.4cdn.org/" ++ board ++ "/thread/" ++ show ident ++ ".json")
    case eitherDecode res :: Either String Thread of
      Left err     -> return (Thread [(Post Nothing Nothing)])
      Right thread -> return thread


threadsToUrlList :: [Thread] -> [(Integer, String)]
threadsToUrlList []               = []
threadsToUrlList ((Thread l):xs) = postsToUrl l ++ threadsToUrlList xs
  where postsToUrl :: [Post] -> [(Integer, String)]
        postsToUrl []     = []
        postsToUrl ((Post (Just url) (Just extention)):ys) = (url, (unpack extention)) : postsToUrl ys
        postsToUrl (_:xs) = postsToUrl xs

allBoards :: [String]
allBoards = ["a", "b", "c", "d", "e", "f", "g", "gif", "h", "hr", "k", "m", "o", "p", "r", "s", "t", "u", "v",
             "vg", "vr", "w", "wg", "i", "ic", "r9k", "s4s", "cm", "hm", "lgbt", "y", "3", "adv", "an", "asp",
             "biz", "cgl", "ck", "co", "diy", "fa", "fit", "gd", "hc", "int", "jp", "lit", "mlp", "mu", "n",
             "out", "po", "pol", "sci", "soc", "sp", "tg", "toy", "trv", "tv", "vp", "wsg", "x"]


downloadImageTo :: FilePath          -- directory
                -> String            -- board
                -> (Integer, String)  -- (name, extention)
                -> IO ()
downloadImageTo fp board (name, extention) = do
    res <- simpleHttp $ "http://i.4cdn.org/" ++ board ++ "/" ++ (show name) ++ extention
    B.writeFile (fp ++ "/" ++ (show name) ++ extention) res
    putStrLn $ "Saved " ++ fp ++ "/" ++ (show name) ++ extention

downloadBoard :: String   -- board to fetch from (f.e "g")
           -> FilePath -- path to folder
           -> Int         -- from page (inclusive)
           -> Int         -- to   page (inclusive)
           -> IO ()
downloadBoard board fp from to= do

    hSetBuffering stdout NoBuffering

    let validBoard = board `elem` allBoards
    res <- (eitherDecode <$> simpleHttp ("http://a.4cdn.org/" ++ board ++ "/threads.json")) :: IO (Either String [Page])

    case (res, validBoard) of
         (_, False)          -> putStrLn "Sorry, this board can't be found..."
         (Left err,_)        -> putStrLn err
         (Right pages, True) -> do

           --  let pageCount | board == "b" = [1..9]
           --                | otherwise    = [1..10]

             let threadIds  = concatMap (\x -> getPageThreads x pages) [from..to]
                 threadIds' = drop 1 threadIds -- remove the sticky
             putStrLn "Fetched current information about threads..."
             threads <- forM threadIds' (getThreadByID board)
             putStrLn "Done extracting posts from all threads..."
             let urlist = threadsToUrlList threads
             putStrLn "Done converting threads to img-urls.."
             parallel_ (map (downloadImageTo fp board) urlist) >> stopGlobalPool


intReads :: String -> [(Int, String)]
intReads = reads

validRange :: String -> String -> (Bool, Int, Int)
validRange x y = case (intReads x, intReads y) of
                     ([(x',[])], [(y', [])]) | x' <= y', x' >= 1, y' <= 10 -> (True, x', y')
                     _                                                 -> (False, 0, 0)

main :: IO()
main = do
    args <- getArgs
    case args of
      ["-board", board, "-to", to, "-range", x, y] | (True, x', y') <- validRange x y -> do
          createDirectoryIfMissing True to
          downloadBoard board to x' y'
      _            -> do
        name <- getProgName
        putStrLn "How to use:"
        putStrLn $ "./" ++ name ++ " -board <4chanboard> -to <folder> -range <from page as int> <to page as int>"
