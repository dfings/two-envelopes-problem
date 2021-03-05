#!/usr/bin/env gst -g
"$ brew install gnu-smalltalk"

Object subclass: Envelopes [
    | numTrials priorLowerMax rand |
    Envelopes class >> new [        
        ^(super new) init
    ]
    init [
        numTrials := 10000.
        priorLowerMax := 100.
        rand := Random new.
    ]
    "
    Runs a single trial where an envelope is chosen.

    If the chosen envelope has a value < cutoff, the function will switch
    envelopes, otherwise it will keep the envelope it has chosen. Returns
    the value of the envelope it ultimately selects.
    "
    singleTrial: cutoff [
        | lowerValue higherValue |
        lowerValue := rand next * priorLowerMax.
        higherValue := 2 * lowerValue.
        ((rand next * 2) floor) == 0
            ifTrue: [lowerValue >= cutoff 
                ifTrue: [^lowerValue] 
                ifFalse: [^higherValue]
            ]
            ifFalse: [higherValue >= cutoff 
                ifTrue: [^higherValue] 
                ifFalse: [^lowerValue]
            ]      
    ]
    "Runs many trials at a given cutoff to approximate the expected value."
    multiTrial: cutoff [
        | total |
        total := 0.0e.
        1 to: numTrials do: [:i |
            total := total + (self singleTrial: cutoff) 
        ].
        ^(total / numTrials)
    ]
].

"Approximates the expected value for each integral cutoff value."
e := Envelopes new.
1 to: 200 do: [:i | 
  'cutoff=' display.
  i display. 
  ' expectedValue ' display.
   (e multiTrial: i) asFloatE displayNl. 
]
