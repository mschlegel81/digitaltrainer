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
    marked:boolean; //for export dialog only
    challengeLevel      :byte;
    callengeCompleted   :boolean;
    resultTemplate      :P_visualBoard;
    expectedBehavior    :P_compoundGate;
    tests:array of record
      inputs:T_wireValueArray;
      actuallyActive,  //during construction only
      maxTotalSteps:longint;
      outputs:T_wireValueArray; //during construction only
    end;
    testRun:record
      succeeded:boolean;
      active:boolean;
      testInputIndex:longint;
      testStep:longint;
    end;

    Interfaces: T_gateInterfaces; //during construction only

    editable:boolean;
    palette             :P_challengePalette;
    challengeTitle      :string;
    challengeDescription:string;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    DESTRUCTOR destroyPartial;
    FUNCTION partialClone:P_challenge;

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    FUNCTION resetChallenge:P_visualBoard;
    PROCEDURE startTesting(CONST board:P_visualBoard);
    PROPERTY currentlyTesting:boolean read testRun.active;
    PROPERTY testSucceeded:boolean read testRun.succeeded;
    FUNCTION testStep(CONST count, timeOutInTicks: longint; CONST board: P_visualBoard): longint;
    FUNCTION getInfoLabelText:string;

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
    challenge:array of P_challenge;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;

    PROCEDURE markAllAsPending;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    PROCEDURE add(CONST c:P_challenge);
    PROCEDURE moveChallenge(CONST index:longint; CONST up:boolean);
    FUNCTION extractForExport:P_challengeSet;
  end;

CONST checkMark='✓';

IMPLEMENTATION
USES sysutils,math;

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
      if continueEvaluation then initialRun:=false;
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
    setLength(challenge,0);
  end;

DESTRUCTOR T_challengeSet.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(challenge)-1 do dispose(challenge[i],destroy);
    setLength(challenge,0);
  end;

PROCEDURE T_challengeSet.markAllAsPending;
  VAR i:longint;
  begin
    for i:=0 to length(challenge)-1 do begin
      challenge[i]^.callengeCompleted:=false;
      challenge[i]^.editable         :=false;
    end;
  end;

FUNCTION T_challengeSet.getSerialVersion: dword;
  begin
    result:=serialVersionOf('T_challengeSet',1);
  end;

FUNCTION T_challengeSet.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
  begin
    if not inherited then exit(false);
    if length(challenge)>0 then exit(false);

    setLength(challenge,stream.readNaturalNumber);
    result:=stream.allOkay;
    for i:=0 to length(challenge)-1 do begin
      new(challenge[i],create);
      result:=result and challenge[i]^.loadFromStream(stream);
      {$ifdef debugMode}
      writeln('Read challenge #',i,'/',length(challenge),' from stream: ',result);
      {$endif}
    end;
  end;

PROCEDURE T_challengeSet.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i:longint;
  begin
    inherited;
    stream.writeNaturalNumber(length(challenge));
    for i:=0 to length(challenge)-1 do challenge[i]^.saveToStream(stream);
  end;

PROCEDURE T_challengeSet.add(CONST c: P_challenge);
  begin
    setLength(challenge,length(challenge)+1);
    challenge[length(challenge)-1]:=c;
    c^.challengeTestCreationThread.ensureStop;
  end;

PROCEDURE T_challengeSet.moveChallenge(CONST index: longint; CONST up: boolean);
  VAR tmp:P_challenge;
      newIndex:longint;
  begin
    if up
    then newIndex:=index-1
    else newIndex:=index+1;

    if (index>=0) and (index<length(challenge)) and (newIndex>=0) and (newIndex<length(challenge)) then begin
      tmp                :=challenge[   index];
      challenge[   index]:=challenge[newIndex];
      challenge[newIndex]:=tmp;
    end;
  end;

FUNCTION T_challengeSet.extractForExport: P_challengeSet;
  VAR i:longint;
  begin
    new(result,create);
    for i:=0 to length(challenge)-1 do if challenge[i]^.marked then result^.add(challenge[i]);
  end;

{ T_challenge }

CONSTRUCTOR T_challenge.create;
  begin
    challengeLevel      :=255;
    callengeCompleted   :=false;
    challengeTitle      :='';
    challengeDescription:='';

    new(palette,create);
    new(resultTemplate  ,create(palette));
    new(expectedBehavior,create(palette));
    setLength(tests,0);
    challengeTestCreationThread.create(@self);
    testRun.active:=false;
  end;

DESTRUCTOR T_challenge.destroy;
  VAR i:longint;
  begin
    challengeTestCreationThread.destroy;
    for i:=0 to length(tests)-1 do setLength(tests[i].inputs,0); setLength(tests,0);
    dispose(resultTemplate,destroy);
    dispose(expectedBehavior,destroy);
    dispose(palette,destroy);
  end;

DESTRUCTOR T_challenge.destroyPartial;
  VAR i:longint;
  begin
    challengeTestCreationThread.destroy;
    for i:=0 to length(tests)-1 do setLength(tests[i].inputs,0); setLength(tests,0);
  end;

FUNCTION T_challenge.partialClone: P_challenge;
  VAR i, j: longint;
  begin
    new(result,create);
    result^.challengeLevel:=challengeLevel;
    result^.challengeTitle:=challengeTitle;
    result^.challengeDescription:=challengeDescription;

    result^.palette:=palette;
    result^.resultTemplate:=resultTemplate;
    result^.expectedBehavior:=expectedBehavior;
    result^.Interfaces:=expectedBehavior^.getInterfaces;

    setLength(result^.tests,length(tests));
    for i:=0 to length(tests)-1 do begin
      setLength(result^.tests[i].inputs,length(tests[i].inputs));
      for j:=0 to length(tests[i].inputs)-1 do result^.tests[i].inputs[j]:=tests[i].inputs[j];
      result^.tests[i].maxTotalSteps:=tests[i].maxTotalSteps;
    end;
  end;

FUNCTION T_challenge.getSerialVersion: dword;
  begin
    result:=serialVersionOf('T_challenge',0);
  end;

FUNCTION T_challenge.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i,j:longint;
      testCount: qword;
  begin
    if not inherited then exit(false);
    challengeLevel      :=stream.readByte;
    callengeCompleted   :=stream.readBoolean;
    editable            :=stream.readBoolean;
    challengeTitle      :=stream.readAnsiString;
    challengeDescription:=stream.readAnsiString;
    if not(stream.allOkay) then exit(false);
    writeln('Reading challenge "',challengeTitle,'"');

    result:=palette^.loadFromStream(stream)
        and resultTemplate^.loadFromStream(stream,false)
        and expectedBehavior^.readPrototypeFromStream(stream,-1)
        and stream.allOkay;
    if not(result) then exit(result);

    testCount:=stream.readNaturalNumber;
    if (testCount>65536) or not(stream.allOkay) then exit(false);
    setLength(tests,testCount);
    writeln('Reading ',testCount,' test cases');
    for i:=0 to length(tests)-1 do with tests[i] do begin
      setLength(inputs,expectedBehavior^.numberOfInputs);
      writeln('Expected behaviour has ',expectedBehavior^.numberOfInputs,' inputs');
      for j:=0 to expectedBehavior^.numberOfInputs-1 do begin
        inputs[j]:=deserialize(stream.readNaturalNumber);
        writeln('Input ',i,'/',j,'=',getBinaryString(inputs[j]),' (',inputs[j].width,')');
      end;
      maxTotalSteps:=stream.readNaturalNumber;
    end;
    writeln('Completely deserialized challenge: ',result);
    result:=result and stream.allOkay;
  end;

PROCEDURE T_challenge.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:longint;
  begin
    inherited;
    stream.writeByte(challengeLevel);
    stream.writeBoolean(callengeCompleted);
    stream.writeBoolean(editable);
    stream.writeAnsiString(challengeTitle);
    stream.writeAnsiString(challengeDescription);

    palette^.saveToStream(stream);
    resultTemplate^.saveToStream(stream,false);
    expectedBehavior^.writePrototypeToStream(stream,-1);

    stream.writeNaturalNumber(length(tests));
    for i:=0 to length(tests)-1 do with tests[i] do begin
      for j:=0 to expectedBehavior^.numberOfInputs-1 do stream.writeNaturalNumber(serialize(inputs[j]));
      stream.writeNaturalNumber(maxTotalSteps);
    end;
  end;

FUNCTION T_challenge.resetChallenge: P_visualBoard;
  VAR board: P_visualBoard;
  begin
    board:=resultTemplate^.clone(not(palette^.allowConfiguration));
    result:=board;
    palette^.resetCounts;
  end;

PROCEDURE T_challenge.startTesting(CONST board: P_visualBoard);
  begin
    with testRun do begin
      active:=board^.interfacesMatch(expectedBehavior);
      testInputIndex:=0;
      testStep:=0;
      if not(active) then exit;
    end;
    board           ^.reset;
    expectedBehavior^.reset;
    board^.setInputs(tests[0].inputs);
  end;

FUNCTION T_challenge.testStep(CONST count, timeOutInTicks: longint; CONST board: P_visualBoard): longint;
  VAR stepsToSimulate: longint;
  begin
    with testRun do stepsToSimulate:=tests[testInputIndex].maxTotalSteps-testStep;
    if count<stepsToSimulate then stepsToSimulate:=count;
    {$ifdef debugMode}
    writeln('Tests to simulate: ',count,'/',stepsToSimulate);
    writeln('Simulating step ',testRun.testStep,' of test case #',testRun.testInputIndex);
    {$endif}

    result:=board^.coSimulateSteps(stepsToSimulate,timeOutInTicks,expectedBehavior);
    testRun.testStep+=result;
    if result=0 then inc(testRun.testStep);
    if testRun.testStep>=tests[testRun.testInputIndex].maxTotalSteps then begin
      if board^.outputsMatch(expectedBehavior) then begin
        inc(testRun.testInputIndex);
        if testRun.testInputIndex>=length(tests) then begin
          testRun.active:=false;
          testRun.succeeded:=true;
          callengeCompleted:=true;
        end else begin
          testRun.testStep:=0;
          board^.setInputs(tests[testRun.testInputIndex].inputs);
        end;
      end else begin
        testRun.active:=false;
        testRun.succeeded:=false;
      end;
    end;
  end;

FUNCTION T_challenge.getInfoLabelText: string;
  begin
    if testRun.active
    then result:='Test '+intToStr(testRun.testInputIndex+1)+' von '+intToStr(length(tests))
    else result:=challengeDescription;
  end;

PROCEDURE T_challenge.initNewChallenge(CONST expectedAsVisual: P_visualBoard;
  CONST challengeBoardOption: T_challengeBoardOption;
  CONST challengePaletteOption: T_challengePaletteOption);
  VAR
    totalInputBits:longint=0;
    i:longint;
  begin
    randomize;
    challengeLevel      :=0;
    callengeCompleted   :=false;
    editable            :=true;

    if expectedBehavior<>nil then dispose(expectedBehavior,destroy);
    if resultTemplate  <>nil then dispose(resultTemplate,destroy);
    if palette         <>nil then dispose(palette,destroy);

    new(palette,create);
    palette^.constructingChallenge:=true;
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

PROCEDURE T_challenge.updateTestCaseResults(CONST callback: F_caseUpdatedCallback; CONST resume: PBoolean; CONST initCounts: boolean);
  VAR i, stepsDone:longint;
  begin
    expectedBehavior^.reset;
    for i:=0 to length(tests)-1 do begin
      tests[i].outputs:=undeterminedOutput(Interfaces);
      tests[i].actuallyActive:=0;
    end;
    if callback<>nil then callback(-1);
    for i:=0 to length(tests)-1 do if (resume=nil) or resume^ then begin
      tests[i].outputs:=expectedBehavior^.simulateSteps(tests[i].maxTotalSteps,tests[i].inputs,stepsDone);
      tests[i].actuallyActive:=stepsDone;
      if initCounts then tests[i].maxTotalSteps:=stepsDone+4+stepsDone shr 3;
      if callback<>nil then callback(i);
    end;
    if (resume<>nil) and not(resume^) and (callback<>nil) then callback(length(tests)-1);
  end;

end.

