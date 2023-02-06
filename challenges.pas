UNIT challenges;

{$mode objfpc}{$H+}

INTERFACE

USES
  compoundGates,visualGates,serializationUtil,paletteHandling;

TYPE
  P_challenge=^T_challenge;
  T_challenge=object
    challengeLevel      :byte;
    callengeCompleted   :boolean;
    board               :P_visualBoard;
    resultTemplate      :P_visualBoard;
    expectedBehavior    :P_circuitBoard;
    palette             :P_challengePalette;
    challengeTitle      :string;
    challengeDescription:string;

  end;

  T_challengeSet=object(T_serializable)
    editable:boolean;
    challenge:array of P_challenge;
  end;

IMPLEMENTATION

end.

