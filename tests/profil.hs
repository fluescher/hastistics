import Hastistics
import Hastistics.Distributions
import Hastistics.Types hiding ((-))
import Hastistics.Data.CSV

notenFormat    = csvTable [toHSString, toHSDouble]
studentsFormat = csvTable (repeat (toHSString))


averages       :: (HSTable a) => a -> HSReport
averages   tbl = select $
                 valueOf "Matrikel-Nr" $ avgOf "Note" $
                 groupBy "Matrikel-Nr" $
                 from tbl

joined         :: (HSTable a, HSTable b) => a -> b -> HSReport
joined stu mrk = select $
                 valueOf "Matrikel-Nr" $ valueOf "Vorname" $ valueOf "Name" $ valueOf "Average of Note" $ valueOf "Profilierung" $
                 join mrk  "Matrikel-Nr" "Matrikel-Nr" $
                 byrow $
                 from stu

profile        :: (HSTable a) => a -> HSReport
profile   tbl  = select $
                 valueOf "Profilierung" $ avgOf "Average of Note" $
                 groupBy "Profilierung" $
                 from tbl


main              :: IO ()
main              = do marksData    <- readFile "noten.csv"
                       studentsData <- readFile "students.csv"
                       let marks    = notenFormat marksData
                       let students = studentsFormat studentsData
                       
                       putStrLn "Hastistics Demo. Press any key to continue"
                       getChar

                       putStrLn "Noten Tabelle"
                       print marks
                       getChar

                       putStrLn "Students Tabelle"
                       print students
                       getChar
                       
                       
                       putStrLn "Berechnen der Notenschnitte der einzelnen Studenten..."
                       print (averages marks)
                       getChar

                       putStrLn "Verknüpfen der Tabellen..."
                       print (joined students (averages marks))
                       getChar

                       putStrLn "Gruppieren nach Prifilierung..."
                       print (profile (joined students (averages marks)))
                                   


