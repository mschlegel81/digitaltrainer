UNIT challenges;

{$mode objfpc}{$H+}

INTERFACE

USES
  compoundGates,logicalGates,visualGates,serializationUtil,paletteHandling;

TYPE
  P_challenge=^T_challenge;

  { T_challenge }

  T_challenge=object(T_serializable)
    challengeLevel      :byte;
    callengeCompleted   :boolean;
    board               :P_visualBoard;
    resultTemplate      :P_visualBoard;
    expectedBehavior    :P_compoundGate;
    tests:array of record
      inputs:T_wireValueArray;
      maxTotalSteps:longint;
    end;
    palette             :P_challengePalette;
    challengeTitle      :string;
    challengeDescription:string;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    FUNCTION resetChallenge:P_visualBoard;
    PROCEDURE testChallenge;

    //For creation purposes...
    PROCEDURE initNewChallenge(CONST expectedAsVisual:P_visualBoard; CONST challengeBoardOption:T_challengeBoardOption; CONST challengePaletteOption:T_challengePaletteOption);
    PROCEDURE setNumberOfTestCases(CONST count:longint);
    PROCEDURE generateTestCase(CONST index:longint; CONST Interfaces:T_gateInterfaces);
    PROCEDURE generateTestCases;
  end;

  { T_challengeSet }
  P_challengeSet=^T_challengeSet;
  T_challengeSet=object(T_serializable)
    editable:boolean;
    challenge:array of P_challenge;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    PROCEDURE add(CONST c:P_challenge);
  end;

CONST checkMark='✓';

IMPLEMENTATION

{ T_challengeSet }

CONSTRUCTOR T_challengeSet.create;
  begin
    editable:=true;
    setLength(challenge,0);
  end;

DESTRUCTOR T_challengeSet.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(challenge)-1 do dispose(challenge[i],destroy);
    setLength(challenge,0);
  end;

FUNCTION T_challengeSet.getSerialVersion: dword;
  begin
    result:=serialVersionOf('T_challengeSet',0);
  end;

FUNCTION T_challengeSet.loadFromStream(VAR stream: T_bufferedInputStreamWrapper
  ): boolean;
  VAR i:longint;
  begin
    if not inherited then exit(false);
    if length(challenge)>0 then exit(false);
    editable:=stream.readBoolean;

    setLength(challenge,stream.readNaturalNumber);
    for i:=0 to length(challenge)-1 do begin
      new(challenge[i],create);
      result:=result and challenge[i]^.loadFromStream(stream);
    end;
  end;

PROCEDURE T_challengeSet.saveToStream(VAR stream: T_bufferedOutputStreamWrapper
  );
  VAR i:longint;
  begin
    inherited;
    stream.writeBoolean(editable);
    stream.writeNaturalNumber(length(challenge));
    for i:=0 to length(challenge)-1 do challenge[i]^.saveToStream(stream);
  end;

PROCEDURE T_challengeSet.add(CONST c: P_challenge);
  begin
    setLength(challenge,length(challenge)+1);
    challenge[length(challenge)-1]:=c;
  end;

{ T_challenge }

CONSTRUCTOR T_challenge.create;
  begin
    challengeLevel      :=255;
    callengeCompleted   :=false;
    challengeTitle      :='';
    challengeDescription:='';

    new(palette,create);
    new(board           ,create(palette));
    new(resultTemplate  ,create(palette));
    new(expectedBehavior,create(palette));
    setLength(tests,0);
  end;

DESTRUCTOR T_challenge.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(tests)-1 do setLength(tests[i].inputs,0); setLength(tests,0);
    dispose(board,destroy);
    dispose(resultTemplate,destroy);
    dispose(expectedBehavior,destroy);
    dispose(palette,destroy);
  end;

FUNCTION T_challenge.getSerialVersion: dword;
  begin
    result:=serialVersionOf('T_challenge',0);
  end;

FUNCTION T_challenge.loadFromStream(VAR stream: T_bufferedInputStreamWrapper
  ): boolean;
  VAR i,j:longint;
      testCount: qword;
  begin
    if not inherited then exit(false);
    challengeLevel      :=stream.readByte;
    callengeCompleted   :=stream.readBoolean;
    challengeTitle      :=stream.readAnsiString;
    challengeDescription:=stream.readAnsiString;
    if not(stream.allOkay) then exit(false);

    result:=palette^.loadFromStream(stream)
        and board         ^.loadFromStream(stream,true)
        and resultTemplate^.loadFromStream(stream,false)
        and expectedBehavior^.readPrototypeFromStream(stream,-1);
    if not(result) then exit(result);

    testCount:=stream.readNaturalNumber;
    if (testCount>65536) or not(stream.allOkay) then exit(false);
    setLength(tests,testCount);
    for i:=0 to length(tests)-1 do with tests[i] do begin
      setLength(inputs,expectedBehavior^.numberOfInputs);
      for j:=0 to expectedBehavior^.numberOfInputs-1 do inputs[j]:=deserialize(stream.readNaturalNumber);
      maxTotalSteps:=stream.readNaturalNumber;
    end;

    result:=result and stream.allOkay;
  end;

PROCEDURE T_challenge.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:longint;
  begin
    inherited;
    stream.writeByte(challengeLevel);
    stream.writeBoolean(callengeCompleted);
    stream.writeAnsiString(challengeTitle);
    stream.writeAnsiString(challengeDescription);
    palette^.saveToStream(stream);
    board^.saveToStream(stream,true);
    resultTemplate^.saveToStream(stream,false);
    expectedBehavior^.writePrototypeToStream(stream,-1);
    stream.writeNaturalNumber(length(tests));
    for i:=0 to length(tests)-1 do with tests[i] do begin
      for j:=0 to expectedBehavior^.numberOfInputs-1 do stream.writeNaturalNumber(serialize(inputs[j]));
      stream.writeNaturalNumber(maxTotalSteps);
    end;
  end;

FUNCTION T_challenge.resetChallenge: P_visualBoard;
  VAR i:longint;
  begin
    dispose(board,destroy);
    board:=resultTemplate^.clone(not(palette^.allowConfiguration));
    result:=board;
  end;

PROCEDURE T_challenge.testChallenge;
  begin
    //TODO: Test the challenge;
    //Reset the board
    //Set to completed, if everything fits...
    //A visual test would be nice, but this requires closer integration with simulation timer
    //
  end;

PROCEDURE T_challenge.initNewChallenge(CONST expectedAsVisual: P_visualBoard; CONST challengeBoardOption:T_challengeBoardOption; CONST challengePaletteOption:T_challengePaletteOption);
  VAR
    Interfaces: T_gateInterfaces;
    totalInputBits:longint=0;
    i:longint;
  begin
    challengeLevel      :=0;
    callengeCompleted   :=false;
    board               :=nil;

    new(palette,create);
    expectedAsVisual^.extractChallenge(challengeBoardOption,palette,expectedBehavior,resultTemplate);
    palette^.finalizePalette(challengeBoardOption,challengePaletteOption);

    challengeTitle      :=expectedAsVisual^.getCaption;
    challengeDescription:=expectedAsVisual^.getDescription;

    setLength(tests,0);
    Interfaces:=expectedAsVisual^.getInterfaces;
    for i:=0 to length(Interfaces.inputs)-1 do totalInputBits+=Interfaces.inputs[i].wireWidth;

    i:=1;
    while (totalInputBits>0) and (i<256) do begin
      dec(totalInputBits);
      i+=i;
    end;
    if i=1 then i+=4;
    setNumberOfTestCases(i);
  end;

PROCEDURE T_challenge.setNumberOfTestCases(CONST count: longint);
  begin

  end;

PROCEDURE T_challenge.generateTestCase(CONST index: longint; CONST Interfaces: T_gateInterfaces);
  begin

  end;

PROCEDURE T_challenge.generateTestCases;
  begin

  end;

end.

