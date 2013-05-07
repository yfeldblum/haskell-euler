module Main where

import System.Environment

import qualified Problem001
import qualified Problem002
import qualified Problem003
import qualified Problem004
import qualified Problem005
import qualified Problem006
import qualified Problem007
import qualified Problem008
import qualified Problem009
import qualified Problem010
import qualified Problem011
import qualified Problem012
import qualified Problem013
import qualified Problem014
import qualified Problem015
import qualified Problem016
import qualified Problem017
import qualified Problem018
import qualified Problem019
import qualified Problem020
import qualified Problem021
import qualified Problem022
import qualified Problem023
import qualified Problem024
import qualified Problem025
import qualified Problem026
import qualified Problem027
import qualified Problem028
import qualified Problem029
import qualified Problem030
import qualified Problem031
import qualified Problem032
import qualified Problem033
import qualified Problem034
import qualified Problem035
import qualified Problem036
import qualified Problem037
import qualified Problem038
import qualified Problem039
import qualified Problem040
import qualified Problem041
import qualified Problem042
import qualified Problem043
import qualified Problem044
import qualified Problem045
import qualified Problem046
import qualified Problem047
import qualified Problem048
import qualified Problem049
import qualified Problem050
import qualified Problem051
import qualified Problem052
import qualified Problem053
import qualified Problem054
import qualified Problem055
import qualified Problem056
import qualified Problem057
import qualified Problem058
import qualified Problem059
import qualified Problem060
import qualified Problem061
import qualified Problem062
import qualified Problem063
import qualified Problem064
import qualified Problem065
import qualified Problem066
import qualified Problem067
import qualified Problem068
import qualified Problem069
import qualified Problem070
import qualified Problem071
import qualified Problem072
import qualified Problem073
import qualified Problem074
import qualified Problem075
import qualified Problem076
import qualified Problem077
import qualified Problem078

solution "001" = Problem001.solutionFrom
solution "002" = Problem002.solutionFrom
solution "003" = Problem003.solutionFrom
solution "004" = Problem004.solutionFrom
solution "005" = Problem005.solutionFrom
solution "006" = Problem006.solutionFrom
solution "007" = Problem007.solutionFrom
solution "008" = Problem008.solutionFrom
solution "009" = Problem009.solutionFrom
solution "010" = Problem010.solutionFrom
solution "011" = Problem011.solutionFrom
solution "012" = Problem012.solutionFrom
solution "013" = Problem013.solutionFrom
solution "014" = Problem014.solutionFrom
solution "015" = Problem015.solutionFrom
solution "016" = Problem016.solutionFrom
solution "017" = Problem017.solutionFrom
solution "018" = Problem018.solutionFrom
solution "019" = Problem019.solutionFrom
solution "020" = Problem020.solutionFrom
solution "021" = Problem021.solutionFrom
solution "022" = Problem022.solutionFrom
solution "023" = Problem023.solutionFrom
solution "024" = Problem024.solutionFrom
solution "025" = Problem025.solutionFrom
solution "026" = Problem026.solutionFrom
solution "027" = Problem027.solutionFrom
solution "028" = Problem028.solutionFrom
solution "029" = Problem029.solutionFrom
solution "030" = Problem030.solutionFrom
solution "031" = Problem031.solutionFrom
solution "032" = Problem032.solutionFrom
solution "033" = Problem033.solutionFrom
solution "034" = Problem034.solutionFrom
solution "035" = Problem035.solutionFrom
solution "036" = Problem036.solutionFrom
solution "037" = Problem037.solutionFrom
solution "038" = Problem038.solutionFrom
solution "039" = Problem039.solutionFrom
solution "040" = Problem040.solutionFrom
solution "041" = Problem041.solutionFrom
solution "042" = Problem042.solutionFrom
solution "043" = Problem043.solutionFrom
solution "044" = Problem044.solutionFrom
solution "045" = Problem045.solutionFrom
solution "046" = Problem046.solutionFrom
solution "047" = Problem047.solutionFrom
solution "048" = Problem048.solutionFrom
solution "049" = Problem049.solutionFrom
solution "050" = Problem050.solutionFrom
solution "051" = Problem051.solutionFrom
solution "052" = Problem052.solutionFrom
solution "053" = Problem053.solutionFrom
solution "054" = Problem054.solutionFrom
solution "055" = Problem055.solutionFrom
solution "056" = Problem056.solutionFrom
solution "057" = Problem057.solutionFrom
solution "058" = Problem058.solutionFrom
solution "059" = Problem059.solutionFrom
solution "060" = Problem060.solutionFrom
solution "061" = Problem061.solutionFrom
solution "062" = Problem062.solutionFrom
solution "063" = Problem063.solutionFrom
solution "064" = Problem064.solutionFrom
solution "065" = Problem065.solutionFrom
solution "066" = Problem066.solutionFrom
solution "067" = Problem067.solutionFrom
solution "068" = Problem068.solutionFrom
solution "069" = Problem069.solutionFrom
solution "070" = Problem070.solutionFrom
solution "071" = Problem071.solutionFrom
solution "072" = Problem072.solutionFrom
solution "073" = Problem073.solutionFrom
solution "074" = Problem074.solutionFrom
solution "075" = Problem075.solutionFrom
solution "076" = Problem076.solutionFrom
solution "077" = Problem077.solutionFrom
solution "078" = Problem078.solutionFrom

main = do
	(num:args) <- getArgs
	putStrLn $ show $ solution num args
