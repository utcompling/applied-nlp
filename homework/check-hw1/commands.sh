# HW1 checker

echo "Problem 1"
scala runAll.scala variables.scala 2 9

echo "Problem 2"
scala runAll.scala calculator.scala 7 plus 4
scala runAll.scala calculator.scala 7 minus 4
scala runAll.scala calculator.scala 7 times 4
scala runAll.scala calculator.scala 7 div 4

echo "Problem 3"
scala runAll.scala addRange.scala 4 9
scala runAll.scala addRange.scala 5 2

echo "Problem 4"
scala runAll.scala factorial.scala 4
scala runAll.scala factorial.scala -3
scala runAll.scala factorial.scala 0

echo "Problem 5"
scala runAll.scala countries.scala Chile Egypt Canada Japan Bolivia

echo "Problem 6"
scala runAll.scala lengthSort.scala "After the glimpse I had had of the Martians emerging from the cylinder in which they had come to the earth from their planet, a kind of fascination paralysed my actions. I remained standing knee-deep in the heather, staring at the mound that hid them. I was a battleground of fear and curiosity."

echo "Problem 7"
scala runAll.scala wotw.scala


echo "Problem 8"
scala runAll.scala rooms.scala Martin 312 Fred 325 Mack 524 Anna 235 Lana 524


echo "Problem 9"

# Test in vocabulary translation.
scala runAll.scala translate.scala test_por2eng.txt "eu vou ver a praia amanha" "a mulher e o menino foi a praia ontem" "eu vou ver a mulher"

# Test whether out of vocab was handled
scala runAll.scala translate.scala test_por2eng.txt "eu sei que voce nao sabe andar ."


