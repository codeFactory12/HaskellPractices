{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Parser where
import Scanner
import UU.Parsing (Pair)

data Slides = Slides [Slide] -- <body>
            | EmptySlides String
    deriving Show

data Slide = Content Body
            | EmptySlide String
             deriving(Eq,Show)

data Body = Body Title [Text]   -- <div>
    deriving (Eq, Show)

data Text = TextH2 String -- <h2> ##
          | TextH3 String
          | TextH4 String
          | TextH5 String
          | TextH6 String
          | Paragraph String -- <p> {}
    deriving (Eq,Show)

data Title = Title String -- <h1> #
            |TitleError String
    deriving(Eq, Show)


evalSlides :: Slides -> String
evalSlides (Slides slides) = "<body>" ++ "\n"  ++ concatMap evalSlide slides ++ "\n" ++ "</body>"
evalSlides (EmptySlides str) = "<body>" ++ "\n" ++ "<h1>" ++ str ++ "</h1>" ++ "\n" ++ "</body>"

evalSlide :: Slide -> String
evalSlide (Content c) = evalBody c
evalSlide (EmptySlide str) = "<div>" ++ "\n" ++ str ++ "\n" ++ "</div>" ++ "\n" 

evalBody :: Body -> String
evalBody (Body title texts) = "<div>" ++ "\n" ++ evalTitle title ++ concatMap evalText texts ++ "\n" ++ "</div>" ++ "\n" 

evalTitle :: Title -> String
evalTitle (Title t) = "<h1>" ++ t ++ "</h1>" ++ "\n"
evalTitle (TitleError t) = "<!--" ++ t ++ "-->"

evalText :: Text -> String
evalText (TextH2 str) = "<h2>" ++ str ++ "</h2>" ++ "\n"
evalText (TextH3 str) = "<h3>" ++ str ++ "</h3>" ++ "\n"
evalText (TextH4 str) = "<h4>" ++ str ++ "</h4>" ++ "\n"
evalText (TextH5 str) = "<h5>" ++ str ++ "</h5>" ++ "\n"
evalText (TextH6 str) = "<h6>" ++ str ++ "</h6>" ++ "\n"
evalText (Paragraph str) = "<p>" ++ str ++ "</p>" ++ "\n"

parser :: [Token] -> Slides
parser [] = EmptySlides "No hay contenido"
parser tokens = Slides (parseSlides tokens)

parseSlides :: [Token] -> [Slide]
parseSlides [] = []
parseSlides (Token key value line col: xs) =
    case key of
        EndSlide -> if value == "---" then EmptySlide "esta vacio mano": parseSlides xs else parseSlides xs
        keyword ->  if value == "! " then Content (Body (titleParser xs ) (textParser xs)) : parseSlides xs else parseSlides xs
        _ -> parseSlides xs

titleParser :: [Token] -> Title
titleParser [] = Title ""
titleParser (Token key val l c: xs) = Title val

textParser :: [Token] -> [Text]
textParser [] = []
textParser (Token key value line col: xs) =
    case key of
        Keyword -> case value of
                "#" -> TextH2 (sigToken xs) : textParser (drop 1 xs)
                "##" -> TextH3 (sigToken xs) : textParser (drop 1 xs)
                "###" -> TextH4 (sigToken xs) : textParser (drop 1 xs)
                "####" -> TextH5 (sigToken xs) : textParser (drop 1 xs)
                "#####" -> TextH6 (sigToken xs) : textParser (drop 1 xs)
                _ -> textParser xs
        String -> Paragraph (paragraphgetText (paragraphParser xs)) : textParser (drop 1 xs)
        _ -> textParser xs

paragraphParser :: [Token] -> Text
paragraphParser [] = Paragraph ""
paragraphParser (Token k v _ _: xs) =
    if k == String then Paragraph (v ++ paragraphgetText (paragraphParser (drop 1 xs))) else Paragraph ""


paragraphgetText :: Text -> String
paragraphgetText (Paragraph s) = s

sigToken :: [Token] -> String
sigToken (Token k v l c:xs) = v

e0 = Content (Body (Title "titulo") [TextH2 "subtitulo"])

parserConStr :: Slides -> String
parserConStr (EmptySlides x) = x
parserConStr (Slides slides) = evalSlides (Slides slides)

createhtml :: Input -> String
createhtml xs = parserConStr (parser (scan xs 1 1))

main :: IO ()
main = do input <- readFile "slide.p5"
          print input
          let token = createhtml input
          writeFile "output.html" token
          putStrLn (show token)
