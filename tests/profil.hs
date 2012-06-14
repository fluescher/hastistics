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
                 valueOf "Matrikel-Nr" $ valueOf "Name" $ valueOf "Average of Note" $ valueOf "Profilierung" $
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
                       putStrLn "Noten Tabelle"
                       getChar

                       print marks
                       putStrLn "Students Tabelle"
                       getChar

                       print students
                       putStrLn "Berechnen der Notenschnitte der einzelnen Studenten..."
                       getChar
                       
                       
                       print (averages marks)
                       putStrLn "Verknüpfen der Tabellen..."
                       getChar

                       print (joined students (averages marks))
                       putStrLn "Gruppieren nach Profilierung..."
                       getChar

                       print (profile (joined students (averages marks)))
                                   


