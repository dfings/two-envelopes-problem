*> $ brew install open-cobol
*> $ cobc -I/Users/$USER/homebrew/include -L/Users/$USER/homebrew/lib -free -x -o envelopes_cbl envelopes.cbl

IDENTIFICATION DIVISION.
PROGRAM-ID. ENVELOPES.

DATA DIVISION.
   WORKING-STORAGE SECTION.
   *> Constants
   01 WS-NUM-TRIALS PIC 9(5) VALUE 10000.
   01 WS-PRIOR-LOWER-MAX PIC 9(3) VALUE 100.
   *> Variables
   01 WS-CUTOFF PIC 9(3).
   01 WS-TOTAL COMP-2.
   01 WS-TRIAL-NUM PIC 9(5).
   01 WS-EXPECTED-VALUE COMP-2.
   01 WS-LOWER-VALUE COMP-2.
   01 WS-HIGHER-VALUE COMP-2.
   01 WS-VALUE COMP-2.
   *> RNG support
   01 WS-RNG COMP-2.
   01 WS-TMP PIC 9(4).

PROCEDURE DIVISION.
   MAIN.
   *> Approximates the expected value for each integral cutoff value.
   PERFORM MULTI-TRIAL VARYING WS-CUTOFF 
       FROM 0 BY 1 UNTIL WS-CUTOFF>2*WS-PRIOR-LOWER-MAX.
   STOP RUN.

   MULTI-TRIAL.
   *> Runs many trials at a given cutoff to approximate the expected value.
   COMPUTE WS-TOTAL = 0.
   PERFORM SINGLE-TRIAL VARYING WS-TRIAL-NUM 
       FROM 1 BY 1 UNTIL WS-TRIAL-NUM > WS-NUM-TRIALS.
   COMPUTE WS-EXPECTED-VALUE = WS-TOTAL / WS-NUM-TRIALS
   DISPLAY 'cutoff='WS-CUTOFF', expected_value='WS-EXPECTED-VALUE.

   SINGLE-TRIAL.
   *> Runs a single trial where an envelope is chosen.  If the chosen envelope 
   *> has a value < cutoff, the function will switch envelopes, otherwise it 
   *> will keep the envelope it has chosen. Returns the value of the envelope 
   *> it ultimately selects.
   PERFORM RNG.
   COMPUTE WS-LOWER-VALUE = WS-RNG * WS-PRIOR-LOWER-MAX.
   COMPUTE WS-HIGHER-VALUE = 2 * WS-LOWER-VALUE
   PERFORM RNG.
   IF WS-RNG < 0.5 THEN 
     IF WS-LOWER-VALUE >= WS-CUTOFF THEN
       COMPUTE WS-VALUE = WS-LOWER-VALUE
     ELSE
       COMPUTE WS-VALUE = WS-HIGHER-VALUE
     END-IF
   ELSE
      IF WS-HIGHER-VALUE >= WS-CUTOFF THEN
       COMPUTE WS-VALUE = WS-HIGHER-VALUE
     ELSE
       COMPUTE WS-VALUE = WS-LOWER-VALUE
     END-IF  
   END-IF.
   COMPUTE WS-TOTAL= WS-TOTAL + WS-VALUE.

   RNG.
   *> FUNCTION RANDOM produces a non-uniform distribution.
   *> So ignore the first 5 digits it produces.
   COMPUTE WS-RNG = FUNCTION RANDOM.
   COMPUTE WS-TMP = WS-RNG * 10000.
   COMPUTE WS-RNG = WS-RNG * 10000 - WS-TMP.
