-- Binôme 3TCA Kenza BENLAMLIH et Benjamin DELHOUME
--import System.IO
--import System.Environment

import Prelude hiding (Left, Right)
data Instruction = Forward Int 
                  | Right Int 
                  | Left Int 
                  | Repeat Int [Instruction] 
  deriving (Show, Read)


-- Fonction permetant de transformer notre boucle repeat en suite d'instruction pour être interprété
delrepeat :: [Instruction] -> [Instruction] -> [Instruction]
delrepeat [] fin = fin
delrepeat (i:other) fin = case i of
    (Repeat xtime ins)->delrepeat other ((concat(replicate xtime (delrepeat ins [])))++fin)
    _ ->delrepeat other (i:fin)

-- Fonction qui transforme nos instruction Forward Left et Right en points

executer :: [Instruction] -> Int -> Int -> Float -> [(Int, Int)] -> [(Int, Int)]
executer [] x y angle pts = pts
executer (i:other) x y angle pts = case i of
    Right rightturn     -> executer other x y (angle - ((realToFrac rightturn)*(pi / 180))) pts
    Left leftturn      -> executer other x y (angle + ((realToFrac leftturn)*(pi / 180))) pts
    Forward straight   -> (executer other x2 y2 angle ((x2,y2):pts))
        where   x2 = x + round ((realToFrac straight)*(cos angle))
                y2 = y + round ((realToFrac straight)*(sin angle))


-- Fonction qui prends en paramètres nos deux points et qui à l'aide d'une balise sera interprété comme un trait entre les deux 
baliseSvg :: (Int, Int) -> (Int, Int) -> String
baliseSvg ptsa ptsb = ("<line x1=\""  ++ (show (fst ptsa)) ++ "\" y1=\"" ++ (show (snd ptsa)) ++ "\" x2=\"" ++ (show (fst ptsb)) ++ "\" y2=\"" ++ (show (snd ptsb)) ++ "\" stroke=\"black\" stroke-width=\"1\" />\n")

-- Fonction qui retourne l'ensemble des balises qui composent notre figure en appellant la fonction ci-dessus balise Svg
finaltrace :: [(Int, Int)] -> String -> String
finaltrace [] svg = svg
finaltrace pts svg = if (length pts) > 1
                    then finaltrace (drop 1 pts) ((baliseSvg (pts!!1) (pts!!0)) ++ svg)
                    else svg

-- Dernière Fonction qui nous donne la suite d'instruction pour notre navigateur avec les entêtes xml adéquate
webinstruction :: [Instruction] -> String
webinstruction ins = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"++ "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"500\" height=\"500\">"++(finaltrace (executer(reverse(delrepeat ins []))  150 150 0 [(150, 150)]) "")++"</svg>"

-- 150 150 0 [(50, 50)]) on prends les valeurs suivantes pour se décaler du haut de la fenêtre de notre navigateur web pour un meilleur rendu, cela est également le points de départ de notre crayon pour le ----dessin

-- Fonction main principal qui prends nos instructions logoskell dans la variable logoinstruction puis on demande retourner dans la console nos instructions en commençant par l'entête xml puis les balises qui -composent notre figurent, ils restent cependant que c'est à la personne qui lance le programme de mettre ces instructions dans un fichier et l'éxecuter dans un navigateur web
main = do
    logoinstruction <- getLine
    putStrLn (webinstruction(read logoinstruction :: [Instruction]))
{-
Ce code ne fonctionne pas c'est un test pour travailler en passant des fichier en paramètres
    [f,g] <- getArgs
    handle <- readFile f
	writeFile g (webinstruction(read logoinstruction :: [Instruction]))

	args <- getArgs
    handle <- openFile args[1] ReadMode
    contents <- hGetContents handle
	svgfinal <- openFile
-}