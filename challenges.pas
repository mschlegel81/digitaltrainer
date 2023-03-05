UNIT challenges;

{$mode objfpc}{$H+}

INTERFACE

USES
  compoundGates,logicalGates,visualGates,serializationUtil,paletteHandling;

TYPE
  P_challenge=^T_challenge;

  F_caseUpdatedCallback=PROCEDURE(CONST index:longint) of object;

  P_challengeTestCreationThread=^T_challengeTestCreationThread;

  { T_challengeTestCreationThread }

  T_challengeTestCreationThread=object
  private
    lastPreparedIndex:longint;
    PROCEDURE setLastPreparedIndex(CONST index:longint);
  public
    challenge:P_challenge;
    initialRun,continueEvaluation,running:boolean;

    //TODO: Thread should be part of the challenge itself;
    //      Must be stopped when number of test cases is set;
    //      There must be an external possibility for restarting
    CONSTRUCTOR create(CONST parent:P_challenge);
    DESTRUCTOR destroy;
    PROCEDURE restart;
    PROCEDURE ensureStop;
    PROPERTY getLastPreparedIndex:longint read lastPreparedIndex;
  end;

  { T_challenge }

  T_challenge=object(T_serializable)
  private
    challengeTestCreationThread:T_challengeTestCreationThread;
    PROCEDURE updateTestCaseResults(CONST callback:F_caseUpdatedCallback; CONST resume:PBoolean; CONST initCounts:boolean=false);
  public
    challengeLevel      :byte;
    callengeCompleted   :boolean;
    board               :P_visualBoard;
    resultTemplate      :P_visualBoard;
    expectedBehavior    :P_compoundGate;
    tests:array of record
      inputs:T_wireValueArray;
      actuallyActive,  //during construction only
      maxTotalSteps:longint;
      outputs:T_wireValueArray;
    end;
    Interfaces: T_gateInterfaces; //during construction only

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
    PROCEDURE generateTestCases(CONST allInputsThenScramble:boolean=false);
    PROCEDURE updateTestCaseResults;
    FUNCTION lastTestCasePrepared:longint;
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
USES sysutils;

{ T_challengeTestCreationThread }

PROCEDURE T_challengeTestCreationThread.setLastPreparedIndex(CONST index: longint);
  begin
    lastPreparedIndex:=index;
  end;

CONSTRUCTOR T_challengeTestCreationThread.create(CONST parent: P_challenge);
  begin
    challenge:=parent;
    initialRun:=true;
    continueEvaluation:=true;
    running:=false;
    lastPreparedIndex:=-1;
  end;

DESTRUCTOR T_challengeTestCreationThread.destroy;
  begin
    ensureStop;
  end;

FUNCTION challengeTestCreation(p:pointer):ptrint; Register;
  begin
    with P_challengeTestCreationThread(p)^ do begin
      challenge^.updateTestCaseResults(@setLastPreparedIndex,@continueEvaluation,initialRun);
      if not(continueEvaluation) then initialRun:=false;
      running:=false;
    end;
    result:=0;
  end;

PROCEDURE T_challengeTestCreationThread.restart;
  begin
    ensureStop;
    running:=true; continueEvaluation:=true;
    beginThread(@challengeTestCreation,@self);
  end;

PROCEDURE T_challengeTestCreationThread.ensureStop;
  begin
    while running do begin
      continueEvaluation:=false;
      sleep(1);
    end;
  end;

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
    c^.challengeTestCreationThread.ensureStop;
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
    challengeTestCreationThread.create(@self);
  end;

DESTRUCTOR T_challenge.destroy;
  VAR i:longint;
  begin
    challengeTestCreationThread.destroy;
    for i:=0 to length(tests)-1 do setLength(tests[i].inputs,0); setLength(tests,0);
    if board<>nil then dispose(board,destroy);
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
  begin
    dispose(board,destroy);
    board:=resultTemplate^.clone(not(palette^.allowConfiguration));
    result:=board;
    palette^.resetCounts;
  end;

PROCEDURE T_challenge.testChallenge;
  begin
    //TODO: Test the challenge;
    //Reset the board
    //Set to completed, if everything fits...
    //A visual test would be nice, but this requires closer integration with simulation timer
    //
  end;

PROCEDURE T_challenge.initNewChallenge(CONST expectedAsVisual: P_visualBoard; CONST challengeBoardOption: T_challengeBoardOption; CONST challengePaletteOption: T_challengePaletteOption);
  VAR
    totalInputBits:longint=0;
    i:longint;
  begin
    randomize;
    challengeLevel      :=0;
    callengeCompleted   :=false;
    board               :=nil;

    if expectedBehavior<>nil then dispose(expectedBehavior,destroy);
    if resultTemplate  <>nil then dispose(resultTemplate,destroy);
    if palette         <>nil then dispose(palette,destroy);

    new(palette,create);
    expectedAsVisual^.extractChallenge(challengeBoardOption,palette,expectedBehavior,resultTemplate);
    palette^.finalizePalette(challengeBoardOption,challengePaletteOption);

    challengeTitle      :=expectedAsVisual^.getCaption;
    challengeDescription:=expectedAsVisual^.getDescription;

    if length(tests)>0 then exit;
    Interfaces:=expectedAsVisual^.getInterfaces;
    for i:=0 to length(Interfaces.inputs)-1 do totalInputBits+=Interfaces.inputs[i].wireWidth;
    i:=1;
    while (totalInputBits>0) and (i<256) do begin
      dec(totalInputBits);
      i+=i;
    end;
    if i=1 then i+=4;
    setLength(tests,i);
    generateTestCases(totalInputBits=0);
  end;

FUNCTION randomInput(CONST Interfaces:T_gateInterfaces):T_wireValueArray;
  VAR i,k:longint;
  begin
    setLength(result,length(Interfaces.inputs));
    for k:=0 to length(result)-1 do begin
      result[k].width:=Interfaces.inputs[k].wireWidth;
      for i:=0 to result[k].width-1 do
      if random>0.5
      then result[k].bit[i]:=tsv_true
      else result[k].bit[i]:=tsv_false;
    end;
  end;

FUNCTION undeterminedOutput(CONST Interfaces:T_gateInterfaces):T_wireValueArray;
  VAR i,k:longint;
  begin
    setLength(result,length(Interfaces.outputs));
    for k:=0 to length(result)-1 do begin
      result[k].width:=Interfaces.outputs[k].wireWidth;
      for i:=0 to WIRE_MAX_WIDTH-1 do result[k].bit[i]:=tsv_undetermined;
    end;
  end;

PROCEDURE T_challenge.setNumberOfTestCases(CONST count: longint);
  VAR oldCount,i:longint;
  begin
    challengeTestCreationThread.ensureStop;
    oldCount:=length(tests);
    setLength(tests,count);
    for i:=oldCount to count-1 do with tests[i] do begin
      inputs:=randomInput(Interfaces);
      outputs:=undeterminedOutput(Interfaces);
      maxTotalSteps:=1000;
      actuallyActive:=0;
    end;
    if (count>oldCount) or (challengeTestCreationThread.lastPreparedIndex<count-1)
    then challengeTestCreationThread.restart;
  end;

PROCEDURE T_challenge.generateTestCases(CONST allInputsThenScramble: boolean);
  FUNCTION inputsByIndex(index:longint):T_wireValueArray;
    VAR k,i:longint;
    begin
      setLength(result,length(Interfaces.inputs));
      for k:=0 to length(result)-1 do begin
        result[k].width:=Interfaces.inputs[k].wireWidth;
        for i:=0 to result[k].width-1 do begin
          if odd(index)
          then result[k].bit[i]:=tsv_true
          else result[k].bit[i]:=tsv_false;
          index:=index shr 1;
        end;
      end;
    end;

  VAR i,j,k:longint;

  begin
    if allInputsThenScramble then begin
      for i:=0 to length(tests)-1 do begin
        tests[i].inputs:=inputsByIndex(i);
        tests[i].outputs:=undeterminedOutput(Interfaces);
        tests[i].maxTotalSteps:=1000;
      end;
      k:=length(tests); setLength(tests,k+1);
      for i:=1 to length(tests)-1 do for j:=0 to i-1 do if random<0.5 then begin
        tests[k]:=tests[i]; tests[i]:=tests[j]; tests[j]:=tests[k];
      end;
      setLength(tests,k);
    end else begin
      for i:=0 to length(tests)-1 do begin
        tests[i].inputs:=randomInput(Interfaces);
        tests[i].outputs:=undeterminedOutput(Interfaces);
        tests[i].maxTotalSteps:=1000;
      end;
    end;
    challengeTestCreationThread.restart;
  end;

PROCEDURE T_challenge.updateTestCaseResults;
  begin
    challengeTestCreationThread.restart;
  end;

FUNCTION T_challenge.lastTestCasePrepared: longint;
  begin
    result:=challengeTestCreationThread.lastPreparedIndex;
  end;

PROCEDURE T_challenge.updateTestCaseResults(
  CONST callback: F_caseUpdatedCallback; CONST resume: PBoolean;
  CONST initCounts: boolean);
  VAR i, stepsDone:longint;
  begin
    expectedBehavior^.reset;
    for i:=0 to length(tests)-1 do begin
      tests[i].outputs:=undeterminedOutput(Interfaces);
      tests[i].actuallyActive:=0;
    end;
    if callback<>nil then callback(-1);
    for i:=0 to length(tests)-1 do if (resume=nil) or resume^ then begin
      writeln('Executing test case #',i);
      tests[i].outputs:=expectedBehavior^.simulateSteps(tests[i].maxTotalSteps,tests[i].inputs,stepsDone);
      tests[i].actuallyActive:=stepsDone;
      if initCounts then tests[i].maxTotalSteps:=stepsDone+4+stepsDone shr 3;
      if callback<>nil then callback(i);
    end;
    if (resume<>nil) and not(resume^) and (callback<>nil) then callback(length(tests)-1);
    writeln('Leaving routine updateTestCaseResults');
  end;

end.

