import Hastistics
import Hastistics.Distributions
import Hastistics.Types
import Hastistics.Data.CSV

simpleReport :: HSTable t => t -> HSReport
simpleReport t = select $
                 avgOf "Punkte" $ avgOf "Note" $
                 from t

genderReport :: HSTable t => t -> HSReport 
genderReport t = select $
                 valueOf "Geschlecht" $ avgOf "Note" $ avgOf "Punkte" $
                 groupBy "Geschlecht" $
                 from t

genderGroupe    :: HSTable t => t -> HSReport
genderGroupe t  = select $
                  valueOf "Geschlecht" $ valueOf "Note" $
                  groupBy "Geschlecht" $
                  byrow $
                  from t


genderCount     :: HSTable t => t -> HSReport
genderCount t   = select $ 
                  valueOf "Geschlecht" $ count $
                  groupBy "Geschlecht" $
                  from t


klassenDaten s = csvTable [toString, toString, toString, toInt, toDouble, toDouble] s


main :: IO ()
main = do   dat  <- readFile "chrg.csv"
            --print (simpleReport (klassenDaten dat))
            -rint (genderCount (genderGroupe (klassenDaten dat)))
            --print (genderReport (klassenDaten dat))
