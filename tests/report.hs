import Hastistics
import Hastistics.Distributions
import Hastistics.Types hiding ((-))
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

sampleReport    :: HSTable t => t -> HSReport
sampleReport t  = select $
                  valueOf "Name" $ valueOf "Vorname" $ valueOf "Stichprobe" $
                  cust "Wst. fuer Testat" probabilityForTestat $
                  byrow $
                  from t
                  
probabilityForTestat :: HSValue -> HSRow -> HSValue
probabilityForTestat _ r = HSDouble (1 - (hygecdf (2 - s) 10 2 7))
                         where s = fromHSIntegerToInteger(fieldValueOf "Stichprobe" r)

klassenDaten s = csvTable [toHSString, toHSString, toHSString, toHSInteger, toHSDouble, toHSDouble] s

main :: IO ()
main = do   dat  <- readFile "chrg.csv"
            --print (simpleReport (klassenDaten dat))
            print (genderCount (genderGroupe (klassenDaten dat)))
            print (sampleReport (klassenDaten dat))
            --print (genderReport (klassenDaten dat))
