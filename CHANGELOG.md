# Revision history for phladiprelio-general-simple

## 0.1.0.0 -- 2023-03-12

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2023-03-12

* First version revised A. Improved the documentation with links to new documentation and video
  recordings.

## 0.1.2.0 -- 2023-03-17

* First version revised B. Updated the dependencies and documentation.

## 0.2.0.0 -- 2023-03-19

* Second version. Added new functionality related to the reverse order of sorting and printing
the results in the default modes and the test for 'smoothness' of the line while reading,
speaking, singing etc.

## 0.2.1.0 -- 2023-03-27

* Second version revised A. Added the possibility to use "-t 2" as options to perform weaker and
more permissive test for irregularities that influence the prosody.

## 0.2.2.0 -- 2023-03-29

* Second version revised B. Added the possibility to use "-t 3" as options to perform test related
to search potentially the most irregular or unstable line options. Added the possibility to specify
the own hash step with "+k" command line option. Updated the dependency boundaries.

## 0.2.3.0 -- 2023-04-04

* Second version revised C. Added the possibility to see example lines for minimum and maximum values
  for the properties in the test mode using the "-t <one of 4, 5, or 6>" command line arguments.

## 0.2.4.0 -- 2023-04-17

* Second version revised D. Added the possibility to see example lines for minimum and maximum values
for the properties in  the test mode using the "-t <one of 7, 8, or 9>" command line arguments.

## 0.3.0.0 -- 2023-05-13

* Third version. Added two new command line groups of arguments - "-p" (no minimal grammar
rules application) and "+a ... -a" (constraints). Updated documentation (especially new pdfs). 
Added new lightweight dependencies. 

## 0.4.0.0 -- 2023-05-24

* Fourth version. Added two new command line groups of arguments - "+l" (add empty line to output for 
not test option) and +b ... -b with the extended group of constraints handling. Extended the 
set of constraints with new ones and now  there are 12 types of them. Some documentation
improvements. Fixed issues with the numbering in the constraints. 
Updated the documentation and dependencies boundaries. 

## 0.4.1.0 -- 2023-05-25

* Fourth version revised A. Fixed issues with operator precedence (&&) and (||). Now should behave 
as defined in Haskell itself (as usual). 

## 0.4.2.0 -- 2023-06-01

* Fourth version revised B. Fixed issues with spaces in the +b ... -b and some other ones with 
the dependency package phonetic-languages-constraints-array. The algebraic constraint with
parentheses still does not work properly. This is a known issue and should be fixed in the further
releases. So  this is an intermediate testing release.

## 0.5.0.0 -- 2023-06-02

* Fifth version. Fixed issues with incorrect parentheses behaviour for +b ... -b command line
  arguments group. Updated the corresponding dependency boundaries. Added much more variants of
tests (to improve mostly performance characteristics). 

## 0.6.0.0 -- 2023-06-15

* Sixth version. Fixed some issues with tests output for all platforms and issues with output for
  Windows users for non-tests functionality. Added new constraint of U. Updated the documentation.
Added two new command line options - +w and +f which allows to specify the output mode for 
printing in the terminal window and printing the result to the file. Added new dependencies. 
Switched to the two-column ouput as a usual one for non-tests functionality.

## 0.6.1.0 -- 2023-06-23

* Sixth version revised A. Updated dependencies. Fixed some issues with printing the resulting 
information.

## 0.6.2.0 -- 2023-06-24

* Sixth version revised B. Switched to the newer version of dependencies so that the library can be 
successfully compiled with GHC-9.6.2 compiler.

## 0.7.0.0 -- 2023-07-30

* Seventh version. Added asynchronous concurrent calculations and async as a package dependency. Added
  the possibility to calculate for the text with maximum 9 words or their combinations (using +x
command line option). Added new draft papers with improved theory about the usage of the software.

## 0.7.0.1 -- 2023-07-30

* Seventh version revised A. Fixed some issues with documentation.

## 0.8.0.0 -- 2023-08-14

* Eighth version. Added the possibility for 2-syllable meter to print additional information about
points of incongruences of the line for +f group of command line arguments (using +f <filename for saving> <code> where code is in the range of (10..19) inclusively). 
Added the possibility to work with multiline files as the input of the program on the per line basis using +m group
command line arguments. Moved the shared with phladiprelio-ukrainian-simple code to the new package 
phladiprelio-tests to reduce code duplication. Updated the dependencies.

## 0.9.0.0 -- 2023-09-22

* Ninth version. Added new function argsProcessing to the library. Some code and documentation improvements. Updated the dependencies boundaries. Added devotion to Nathalie Kok on her Birthday.

## 0.9.0.1 -- 2023-09-22

* Ninth version revised A. Fixed issue with the inaccurate description in the calab file.

## 0.10.0.0 -- 2023-10-01

* Tenth version. Added the possibility to use the "music" mode that is intended for better lyrics
  and music composing. Fixed issue with +x command line arguments.
Updated dependencies. Some documentation improvements.

## 0.11.0.0 -- 2023-11-01

* Eleventh version. Added a new command line argument "+ul" with additional argument to compute not
  usual rhythmicity properties, but diversity property of the sounds represented by the encoded 
additional string. Added video example for the parallel project phladiprelio-ukrainian-simple
showing the new functionality usage for Ukrainian language.

## 0.11.1.0 -- 2023-11-06

* Eleventh version revised A. Added a possibility to use +dc group of command line arguments that 
allow to specify whether to add <br> html tag to each line in the two-column output and to what file
print additionally two-column output.

## 0.12.0.0 -- 2023-11-11

* Twelfth version. Added a new functionality related to the comparison of similarity of two
  lists. Use for this +l2 ... -l2 group  of command line options. This leads to changes in functions
signatures, too.

## 0.12.1.0 -- 2023-11-12

* Twelfth version revised A. Fixed issue with 'white-spaced' halflines in the output. Updated the
  dependency boundaries of halfsplit.

## 0.12.2.0 -- 2023-11-12

* Twelfth version revised B. Added possibility to specify for +l2 ... -l2 group of arguments
  additionally a power of 10 for the multiplier that affects the value and groupping. Use for this
+q with Int number as the next command line argument in the range [2..6]. The default value is 4.

## 0.13.0.0 -- 2023-11-17

* Thirteenth version. Added a possibility to change the durations of the selected syllables
using the ={set of digits} precisely after the needed syllable. For more information, see 
https://hackage.haskell.org/package/phladiprelio-ukrainian-simple-0.14.0.0/src/Main.hs
This significantly extends the general possibilities of the program, especially for the music composing.

## 0.14.0.0 -- 2023-12-01

* Fourteenth version. Fixed issue with distance between line options in several branches. Added a possibility to analyse and compare two lines from the same file using either additionally to +m also +m2 group of command line arguments, or +m3 group of arguments instead.

## 0.14.1.0 -- 2023-12-30

* Fourteenth version revised A. Some code refactoring. Added new functions to simplify understanding and testing. Changed the function to count syllables in test and other cases modes. Now it includes the "\_{set of digits}" groups as syllables. Some minor documentation improvements.

## 0.14.2.0 -- 2024-01-30

* Fourteenth version revised B. Switched to newer versions of dependencies and removed the unneeded ones. Some performance improvements. Some documentation improvements. Added bug-tracker.

## 0.15.0.0 -- 2024-03-08

* Fifteenth version. Added special comparative mode to choose the option for the line among the lines in files using '-cm' command line argument. Some code improvements. Updated the  dependency boundaries. 

 ## 0.20.0.0 -- 2024-04-09

* Twentieth version. Switched to Word8 instead of Double for durations of the PRS and syllables. Made a version window for releases with Double-related functionality. The results now are expected to be different, because it uses a significantly different scheme of computation. Some performance and memory consumption improvements.

 ## 0.20.0.1 -- 2024-04-09

* Twentieth version revised A. Fixed issue with documentation for conversion function that now uses Word8 instead of Double.

