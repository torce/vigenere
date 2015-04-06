#!/bin/bash
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k p
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -b -f out -x a -l 1 -m 8
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 1 -m 8

java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k pa
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -b -f out -x a -l 2 -m 8
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 2 -m 8

java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k pas
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -b -f out -x a -l 3 -m 8
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 3 -m 8

java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k pass
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -b -f out -x a -l 4 -m 8
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 4 -m 8

java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k passw
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -b -f out -x a -l 5 -m 8
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 5 -m 8

#Brute force takes too long with a key > 5
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k passwo
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 6 -m 8

java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k passwor
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 7 -m 8

java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k password
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 8 -m 8

java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k passwordl
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 9 -m 8

java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k passwordlo
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 10 -m 8

java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k passwordlon
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 11 -m 8

java -jar target/scala-2.11/vigenere-assembly-1.0.jar -c -f file -o out -x a -k passwordlong
java -jar target/scala-2.11/vigenere-assembly-1.0.jar -bg -f out -x a -l 12 -m 8

