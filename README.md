hastistics
==========

Sort of LINQ for Haskell. It was developed as part of the course "Programming language concepts" 
at the University of Applied Sciences of Northwestern Switzerland.

Functionality
-------------


```haskell
import Hastistics
import Hastistics.Data.CSV

result = select $
         valueOf "Gender" $ avgOf "Mark" $
         groupBy "Gender" $
         from (csvTable [toHSString, toHSDouble] csvString)
```

If you now print this, you get something like

```
+----------------------+----------------------+
| "Gender"             | "Average of Mark"    |
+----------------------+----------------------+
| M                    | 4.4375               |
| W                    | 4.75                 |
+----------------------+----------------------+
```


Examples
--------

You find more examples in the tests directory.
