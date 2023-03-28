UNIT challenges;

{$mode objfpc}{$H+}

INTERFACE

USES
  compoundGates,logicalGates,visualGates,serializationUtil,paletteHandling;
CONST MAX_NUMBER_OF_CHALLENGE_CHECKS=256;

TYPE
  P_challenge=^T_challenge;
  P_testCreator=^T_testCreator;

  F_caseUpdatedCallback=PROCEDURE(CONST index:longint) of object;
  P_TestCreationThread=^T_testCreationThread;

  { T_testCreationThread }

  T_testCreationThread=object
  private
    lastPreparedIndex:longint;
    PROCEDURE setLastPreparedIndex(CONST index:longint);
  public
    parent:P_testCreator;
    initialRun,continueEvaluation,running:boolean;
    CONSTRUCTOR create(CONST parent_:P_testCreator);
    DESTRUCTOR destroy;
    PROCEDURE restart;
    PROCEDURE ensureStop;
    PROPERTY getLastPreparedIndex:longint read lastPreparedIndex;
  end;

  { T_testCreator }
  T_testCreator=object(T_serializable)
    private
      challengeTestCreationThread:T_testCreationThread;
      PROCEDURE updateTestCaseResults(CONST callback:F_caseUpdatedCallback; CONST resume:PBoolean; CONST initCounts:boolean=false);
    public
      behavior    :P_compoundGate;
      tests:array of record
        inputs:T_wireValueArray;
        actuallyActive,
        maxTotalSteps:longint;
        outputs:T_wireValueArray;
      end;

      Interfaces: T_gateInterfaces;

      CONSTRUCTOR createAsChallengeBasis;
      CONSTRUCTOR createForAnalysis(CONST visualBoard:P_visualBoard);
      DESTRUCTOR destroy;

      PROCEDURE setNumberOfTestCases(CONST count:longint);
      PROCEDURE generateTestCases(CONST continuous:boolean=false; CONST scramble:boolean=true);
      PROCEDURE reInitStepCounts;
      PROCEDURE updateTestCaseResults;
      PROPERTY lastTestCasePrepared:longint read challengeTestCreationThread.lastPreparedIndex;
      PROCEDURE copyTestInputs(CONST origin:P_testCreator);
  end;

  { T_challenge }
  T_challenge=object(T_testCreator)
  private
    testRun:record
      succeeded:boolean;
      active:boolean;
      testInputIndex:longint;
      testStep:longint;
    end;
  public
    marked:boolean; //for export dialog only

    challengeLevel      :byte;
    callengeCompleted   :boolean;
    resultTemplate,
    expectedBehavior    :P_visualBoard;

    editable:boolean;
    palette             :P_challengePalette;
    challengeTitle      :string;
    challengeDescription:string;

    CONSTRUCTOR create;
    DESTRUCTOR destroy; virtual;
    FUNCTION clone:P_challenge;

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    FUNCTION resetChallenge:P_visualBoard; virtual;
    PROCEDURE startTesting(CONST board:P_visualBoard); virtual;
    PROPERTY currentlyTesting:boolean read testRun.active;
    PROPERTY testSucceeded:boolean read testRun.succeeded;
    FUNCTION testStep(CONST count, timeOutInTicks: longint; CONST board: P_visualBoard): longint; virtual;
    FUNCTION getInfoLabelText(CONST uiIdle:boolean):string; virtual;
    FUNCTION ensureBehavior:P_compoundGate;
    PROCEDURE dropBehavior;

    //For creation purposes...
    PROCEDURE initNewChallenge(CONST expectedAsVisual:P_visualBoard);

    FUNCTION equals(CONST c:P_challenge):boolean; virtual;
  end;

  { T_tutorial }
  T_tutorial=object(T_challenge)
    tutorialStep:byte;
    workspaceBoard:P_visualBoard;

    unchangedInput:T_wireValue;
    oldIoMode: T_multibitWireRepresentation;
    CONSTRUCTOR create;
    DESTRUCTOR destroy; virtual;

    FUNCTION resetChallenge:P_visualBoard; virtual;
    PROCEDURE startTesting(CONST board:P_visualBoard); virtual;
    FUNCTION testStep(CONST count, timeOutInTicks: longint; CONST board: P_visualBoard): longint; virtual;
    FUNCTION getInfoLabelText(CONST uiIdle:boolean):string; virtual;

    FUNCTION equals(CONST c:P_challenge):boolean; virtual;
  end;

  { T_challengeSet }
  P_challengeSet=^T_challengeSet;
  T_challengeSet=object(T_serializable)
  private
    PROCEDURE addCopyForExport(CONST c:P_challenge; CONST exportEditable:boolean);
  public
    challenge:array of P_challenge;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;

    PROCEDURE markAllAsPending;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    FUNCTION add(CONST c:P_challenge):boolean;
    PROCEDURE moveChallenge(CONST index:longint; CONST up:boolean);
    PROCEDURE exportSelected  (CONST fileName:string; CONST exportEditable:boolean);
    FUNCTION importChallenges(CONST fileName:string; CONST overwriteExisting:boolean):boolean;
  end;

CONST checkMark='✓';
VAR tutorial:T_tutorial;
IMPLEMENTATION
USES sysutils;

{ T_tutorial }

CONSTRUCTOR T_tutorial.create;
  VAR tempBoard:T_visualBoard;

      input:P_visualGate;
      output:P_visualGate;
      adapter:P_visualGate;
      andGate:P_visualGate;

      tempBehavior:P_abstractGate;
  begin
    inherited;
    tutorialStep:=0;
    challengeTitle      :='Tutorial';
    challengeDescription:='Eine Grundlegende Erklärung der Bedienung';
    challengeLevel      :=0;
    callengeCompleted   :=false;
    editable            :=false;

    tempBoard.create(palette);

    tempBehavior:=newBaseGate(gt_input);
    P_inputGate(tempBehavior)^.width:=4;
    P_inputGate(tempBehavior)^.reset;
    new(input,create(tempBehavior));
    tempBoard.elementAdded(input,true);

    new(output,create(newBaseGate(gt_output)));
    tempBoard.elementAdded(output,true);

    new(adapter,create(newBaseGate(gt_adapter)));
    tempBoard.elementAdded(adapter,true);

    tempBehavior:=newBaseGate(gt_andGate);
    P_andGate(tempBehavior)^.inputCount:=4;
    new(andGate,create(tempBehavior));

    tempBoard.elementAdded(andGate,true);

    tempBoard.addWire(input,0,adapter,0);
    tempBoard.addWire(adapter,0,andGate,0);
    tempBoard.addWire(adapter,1,andGate,1);
    tempBoard.addWire(adapter,2,andGate,2);
    tempBoard.addWire(adapter,3,andGate,3);
    tempBoard.addWire(andGate,0,output,0);

    Interfaces:=tempBoard.getInterfaces;
    behavior:=tempBoard.extractBehavior;

    tempBoard.destroy;

    setNumberOfTestCases(16);
    generateTestCases(true,false);
  end;

DESTRUCTOR T_tutorial.destroy;
  begin
    inherited;
  end;

FUNCTION T_tutorial.resetChallenge: P_visualBoard;
  VAR tmp:P_abstractGate;
  begin
    result:=inherited;
    tutorialStep:=0;
    palette^.clear;
    tmp:=newBaseGate(gt_input);
    tmp^.reset;
    palette^.paletteOption:=co_unconfiguredPaletteWithCounts;
    palette^.ensureBaseGate(tmp);
    palette^.countUpGate(tmp);
    dispose(tmp,destroy);
    workspaceBoard:=result;
  end;

PROCEDURE T_tutorial.startTesting(CONST board: P_visualBoard);
  begin
    if tutorialStep=12 then inherited;
  end;

FUNCTION T_tutorial.testStep(CONST count, timeOutInTicks: longint; CONST board: P_visualBoard): longint;
  begin
    if tutorialStep=12 then result:=inherited;
  end;

FUNCTION T_tutorial.getInfoLabelText(CONST uiIdle:boolean): string;
  CONST stepText:array[0..12] of string=(
  {0} 'Auf der linken Seite siehst du die Palette.'+LineEnding+'Du kannst Bauelemente zur Schaltung hinzufügen, indem du sie mit der linken Maustaste'+LineEnding+'von der Palette auf die Schaltung ziehst.',
  {1} 'Eingänge werden immer am oberen oder linken Rand positioniert,'+LineEnding+'Ausgänge immer am unteren oder rechten.'+LineEnding+'Du kannst das Element wieder entfernen, indem du es zurück zur Palette ziehst oder es markierst und Entf drückst.',
  {2} 'Jetzt fügen wir einen Eingang und einen Ausgang hinzu.',
  {3} 'Du kannst Kabel erstellen, indem du auf einen Ausgang eines Bauelements links-klickst.'+LineEnding+'Ein Linksklick auf ein bestehendes Kabel entfernt dieses wieder.',
  {4} 'Mit einem Rechtsklick auf einen Ein- oder Ausgang kannst du beobachten, was genau dort passiert.',
  {5} 'Du kannst den Wert eines Eingangs ändern, indem du ihn mit Klick auf seine untere Hälfte bearbeitest.'+LineEnding+
      'Beendest du die Eingabe mit Enter, wird sie wieder geschlossen; beendest du sie mit einem Leerzeichen, wird sie übernommen ohne zu schließen',
  {6} 'Alle Bits eines Eingangs werden umgeschaltet, wenn du die in der oberen rechten Ecke des Eingangs angegebene Taste drückst'+LineEnding+'Mit Shift+[Taste] öffnest du den Editor',
  {7} 'Du kannst die Eigenschaften des Eingangs verändern, indem du auf seine obere Hälfte rechts-klickst.'+LineEnding+
      'Setze "Breite Ausgang" auf 4!'+LineEnding+
      'Erlaubte Breiten sind: 1, 4, 8 und 16',
  {8} 'Das Kabel ist verschwunden, weil die Anschlüsse unterschiedliche Breiten haben.'+LineEnding+
      'Dafür gibt es Adapter. Füge den Adapter hinzu und verbinde Eingang mit Adapter!',
  {9} 'Jetzt fügen wir ein Und-Gatter hinzu und setzen dort (Rechtsklick) "Anzahl Eingänge" auf 4.',
  {10} 'Verbinde nun jeden Ausgang des Adapters mit einem Eingang des "and"-Elements und den Ausgang des "and"-Elements mit dem Ausgang'+LineEnding+
       'Du hast damit eine Schaltung erstellt, die genau dann 1 ausgibt wenn alle Eingänge 1 sind.',
  {11} 'Eine Sache noch: Für breitere Eingänge kannst du die Darstellungsform'+LineEnding+
       'per Rechtsklick auf dessen untere Hälfte umschalten.',
  {12} 'Mit einem Klick auf "test" startest du einen Vergleich Deiner Schaltung mit dem vordefinierten Verhalten.'+LineEnding+'Das Beendet das Tutorial.');
  FUNCTION stepCompleted:boolean;
    VAR tmp:P_abstractGate;
    begin
      case tutorialStep of
        0: begin
             result:=gatesTotal(workspaceBoard^.getGateCount)=1;
           end;
        1: begin
             result:=gatesTotal(workspaceBoard^.getGateCount)=0;
             if result then begin
               tmp:=newBaseGate(gt_output);
               palette^.ensureBaseGate(tmp);
               palette^.countUpGate(tmp);
               dispose(tmp,destroy);
             end;
           end;
        2: begin
             result:=(length(workspaceBoard^.inputs)=1) and
                     (length(workspaceBoard^.outputs)=1);
           end;
        3: begin
             result:=(length(workspaceBoard^.inputs)=1) and
                     (length(workspaceBoard^.outputs)=1) and
                     (length(workspaceBoard^.wires)=1);
           end;
        4: begin
             result:=(length(workspaceBoard^.inputs)=1) and
                     (length(workspaceBoard^.outputs)=1) and
                     (length(workspaceBoard^.wires)=1) and
                     (length(workspaceBoard^.watchers)>0);
             if result then begin
               unchangedInput.width:=1;
               unchangedInput.bit[0]:=tsv_false;
               workspaceBoard^.inputs[0]^.getBehavior^.setInput(0,unchangedInput);
             end;
           end;
        5,6: begin
             result:=(length(workspaceBoard^.inputs)=1) and
                     (length(workspaceBoard^.outputs)=1) and
                     (length(workspaceBoard^.wires)=1) and
                     (unchangedInput<>workspaceBoard^.inputs[0]^.getBehavior^.getInput(0));
             if result then unchangedInput:=workspaceBoard^.inputs[0]^.getBehavior^.getInput(0);
           end;
        7: begin
             result:=(length(workspaceBoard^.inputs)=1) and
                     (length(workspaceBoard^.outputs)=1) and
                     (workspaceBoard^.inputs[0]^.getBehavior^.outputWidth(0)=4);
             if result then begin
               tmp:=newBaseGate(gt_adapter);
               palette^.ensureBaseGate(tmp);
               palette^.countUpGate(tmp);
               dispose(tmp,destroy);
               workspaceBoard^.inputs[0]^.fixedProperties:=true;
             end;
           end;
        8: begin
             result:=(length(workspaceBoard^.inputs)=1) and
                     (length(workspaceBoard^.outputs)=1) and
                     (length(workspaceBoard^.wires)=1) and
                     (length(workspaceBoard^.gates)=1) and
                     (workspaceBoard^.isInputConnected(workspaceBoard^.gates[0],0));
             if result then begin
               tmp:=newBaseGate(gt_andGate);
               palette^.ensureBaseGate(tmp);
               palette^.countUpGate(tmp);
               dispose(tmp,destroy);
               workspaceBoard^.gates[0]^.fixedProperties:=true;
             end;
           end;
        9: begin
            result:=(length(workspaceBoard^.inputs)=1) and
                    (length(workspaceBoard^.outputs)=1) and
                    (length(workspaceBoard^.wires)=1) and
                    (length(workspaceBoard^.gates)=2) and
                    (workspaceBoard^.isInputConnected(workspaceBoard^.gates[0],0)) and
                    (workspaceBoard^.gates[1]^.getBehavior^.numberOfInputs=4);
           end;
       10: begin
             result:=(length(workspaceBoard^.inputs)=1) and
                     (length(workspaceBoard^.outputs)=1) and
                     (length(workspaceBoard^.wires)=6) and
                     (length(workspaceBoard^.gates)=2) and
                     (workspaceBoard^.gates[1]^.getBehavior^.numberOfInputs=4) and
                     workspaceBoard^.isInputConnected(workspaceBoard^.gates[0],0) and
                     workspaceBoard^.isInputConnected(workspaceBoard^.gates[1],0) and
                     workspaceBoard^.isInputConnected(workspaceBoard^.gates[1],1) and
                     workspaceBoard^.isInputConnected(workspaceBoard^.gates[1],2) and
                     workspaceBoard^.isInputConnected(workspaceBoard^.gates[1],3) and
                     workspaceBoard^.isInputConnected(workspaceBoard^.outputs[0],0);
             oldIoMode:=workspaceBoard^.inputs[0]^.ioMode;
           end;
        11: begin
          result:=(length(workspaceBoard^.inputs)=1) and
                  (length(workspaceBoard^.outputs)=1) and
                  (length(workspaceBoard^.wires)=6) and
                  (length(workspaceBoard^.gates)=2) and
                  (workspaceBoard^.gates[1]^.getBehavior^.numberOfInputs=4) and
                  workspaceBoard^.isInputConnected(workspaceBoard^.gates[0],0) and
                  workspaceBoard^.isInputConnected(workspaceBoard^.gates[1],0) and
                  workspaceBoard^.isInputConnected(workspaceBoard^.gates[1],1) and
                  workspaceBoard^.isInputConnected(workspaceBoard^.gates[1],2) and
                  workspaceBoard^.isInputConnected(workspaceBoard^.gates[1],3) and
                  workspaceBoard^.isInputConnected(workspaceBoard^.outputs[0],0) and
                 (workspaceBoard^.inputs[0]^.ioMode<>oldIoMode);
            end;
        else result:=false;
      end;
    end;

  begin
    if uiIdle and stepCompleted then inc(tutorialStep);
    if (tutorialStep=12) and currentlyTesting
    then result:=inherited
    else result:=stepText[tutorialStep];
  end;

FUNCTION T_tutorial.equals(CONST c: P_challenge): boolean;
  begin
    //Singleton equality
    result:= c=P_challenge(@self);
  end;

{ T_testCreationThread }

PROCEDURE T_testCreationThread.setLastPreparedIndex(CONST index: longint);
  begin
    lastPreparedIndex:=index;
  end;

CONSTRUCTOR T_testCreationThread.create(CONST parent_: P_testCreator);
  begin
    parent:=parent_;
    initialRun:=true;
    continueEvaluation:=true;
    running:=false;
    lastPreparedIndex:=-1;
  end;

DESTRUCTOR T_testCreationThread.destroy;
  begin
    ensureStop;
  end;

FUNCTION challengeTestCreation(p:pointer):ptrint; Register;
  begin
    with P_TestCreationThread(p)^ do begin
      parent^.updateTestCaseResults(@setLastPreparedIndex,@continueEvaluation,initialRun);
      if continueEvaluation then initialRun:=false;
      running:=false;
    end;
    result:=0;
  end;

PROCEDURE T_testCreationThread.restart;
  begin
    ensureStop;
    running:=true; continueEvaluation:=true;
    beginThread(@challengeTestCreation,@self);
  end;

PROCEDURE T_testCreationThread.ensureStop;
  begin
    while running do begin
      continueEvaluation:=false;
      sleep(1);
    end;
  end;

{ T_challengeSet }

CONSTRUCTOR T_challengeSet.create;
  begin
    setLength(challenge,1);
    challenge[0]:=@tutorial;
  end;

DESTRUCTOR T_challengeSet.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(challenge)-1 do if not(tutorial.equals(challenge[i])) then dispose(challenge[i],destroy);
    setLength(challenge,0);
  end;

PROCEDURE T_challengeSet.markAllAsPending;
  VAR i:longint;
  begin
    for i:=0 to length(challenge)-1 do challenge[i]^.callengeCompleted:=false;
  end;

FUNCTION T_challengeSet.getSerialVersion: dword;
  begin
    result:=serialVersionOf('T_challengeSet',2);
  end;

FUNCTION T_challengeSet.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
  begin
    if not inherited then exit(false);
    if length(challenge)<>1 then exit(false);
    tutorial.callengeCompleted:=stream.readBoolean;
    setLength(challenge,stream.readNaturalNumber+1);
    result:=stream.allOkay;
    for i:=1 to length(challenge)-1 do begin
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
    stream.writeBoolean(tutorial.callengeCompleted);
    stream.writeNaturalNumber(length(challenge)-1);
    for i:=0 to length(challenge)-1 do if not(tutorial.equals(challenge[i])) then begin
      challenge[i]^.saveToStream(stream);
    end;
  end;

FUNCTION T_challengeSet.add(CONST c: P_challenge): boolean;
  VAR q:P_challenge;
  begin
    for q in challenge do if q^.equals(c) then exit(false);
    result:=true;
    setLength(challenge,length(challenge)+1);
    challenge[length(challenge)-1]:=c;
    c^.challengeTestCreationThread.ensureStop;
  end;

PROCEDURE T_challengeSet.addCopyForExport(CONST c:P_challenge; CONST exportEditable:boolean);
  begin
    setLength(challenge,length(challenge)+1);
    challenge[length(challenge)-1]:=c^.clone;
    if not(exportEditable) then challenge[length(challenge)-1]^.editable:=false;
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

PROCEDURE T_challengeSet.exportSelected(CONST fileName:string; CONST exportEditable:boolean);
  VAR i:longint;
      temp:T_challengeSet;
  begin
    temp.create;
    for i:=0 to length(challenge)-1 do if challenge[i]^.marked then temp.addCopyForExport(challenge[i],exportEditable);
    temp.saveToFile(fileName);
    for i:=0 to length(temp.challenge)-1 do if not(tutorial.equals(temp.challenge[i])) then dispose(temp.challenge[i],destroy);
    setLength(temp.challenge,0);
    temp.destroy;
  end;

FUNCTION T_challengeSet.importChallenges(CONST fileName:string; CONST overwriteExisting:boolean):boolean;
  VAR i:longint;
      temp:T_challengeSet;
  begin
    temp.create;
    if not(temp.loadFromFile(fileName)) or (length(temp.challenge)=0) then begin
      temp.destroy;
      exit(false);
    end;
    temp.markAllAsPending;
    if overwriteExisting then begin
      for i:=0 to length(challenge)-1 do if not tutorial.equals(temp.challenge[i]) then dispose(challenge[i],destroy);
      setLength(challenge,length(temp.challenge));
      for i:=0 to length(challenge)-1 do challenge[i]:=temp.challenge[i];
    end else begin
      for i:=0 to length(temp.challenge)-1 do
        if not add(temp.challenge[i]) and not tutorial.equals(temp.challenge[i])
        then dispose(temp.challenge[i],destroy);
    end;
    setLength(temp.challenge,0);
    temp.destroy;
    result:=true;
  end;

{ T_challenge }

CONSTRUCTOR T_challenge.create;
  begin
    inherited createAsChallengeBasis;

    challengeLevel      :=255;
    callengeCompleted   :=false;
    challengeTitle      :='';
    challengeDescription:='';

    new(palette,create);
    new(resultTemplate  ,create(palette));
    new(expectedBehavior,create(palette));
    testRun.active:=false;
  end;

DESTRUCTOR T_challenge.destroy;
  begin
    inherited;
    dispose(resultTemplate,destroy);
    dispose(expectedBehavior,destroy);
    dispose(palette,destroy);
  end;

FUNCTION T_challenge.clone: P_challenge;
  VAR i,j:longint;
  begin
    new(result,create);

    result^.challengeLevel      :=challengeLevel      ;
    result^.callengeCompleted   :=callengeCompleted   ;
    result^.editable            :=editable            ;
    result^.challengeTitle      :=challengeTitle      ;
    result^.challengeDescription:=challengeDescription;

    dispose(result^.resultTemplate,destroy);
    dispose(result^.expectedBehavior,destroy);
    dispose(result^.palette,destroy);
    result^.resultTemplate  :=resultTemplate^.cloneAsTrueCopy;
    result^.expectedBehavior:=expectedBehavior^.cloneAsTrueCopy;
    result^.palette:=palette^.cloneAndMigrate(result^.resultTemplate,result^.expectedBehavior);

    setLength(result^.tests,length(tests));
    for i:=0 to length(tests)-1 do begin
      setLength(result^.tests[i].inputs,
         length(        tests[i].inputs));
      for j:=0 to length(tests[i].inputs)-1 do
        result^.tests[i].inputs[j]
              :=tests[i].inputs[j];
      result^.tests[i].actuallyActive:=tests[i].actuallyActive;
      result^.tests[i].maxTotalSteps :=tests[i].maxTotalSteps;
      setLength(result^.tests[i].outputs,0); //not cloned, because it can be reproduced anyway
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

    result:=palette^.loadFromStream(stream)
        and resultTemplate^.loadFromStream(stream,false)
        and expectedBehavior^.loadFromStream(stream,false)
        and stream.allOkay;
    if not(result) then exit(result);

    testCount:=stream.readNaturalNumber;
    if (testCount>65536) or not(stream.allOkay) then exit(false);
    setLength(tests,testCount);
    for i:=0 to length(tests)-1 do with tests[i] do begin
      setLength(inputs,length(expectedBehavior^.inputs));
      for j:=0 to length(inputs)-1 do begin
        inputs[j]:=deserialize(stream.readNaturalNumber);
      end;
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
    stream.writeBoolean(editable);
    stream.writeAnsiString(challengeTitle);
    stream.writeAnsiString(challengeDescription);

    palette^.saveToStream(stream);
    resultTemplate^.saveToStream(stream,false);
    expectedBehavior^.saveToStream(stream,false);

    stream.writeNaturalNumber(length(tests));
    for i:=0 to length(tests)-1 do with tests[i] do begin
      for j:=0 to length(expectedBehavior^.inputs)-1 do stream.writeNaturalNumber(serialize(inputs[j]));
      stream.writeNaturalNumber(maxTotalSteps);
    end;
  end;

FUNCTION T_challenge.resetChallenge: P_visualBoard;
  VAR board: P_visualBoard;
      i:longint;
      g:P_visualGate;
  begin
    board:=resultTemplate^.cloneAsTrueCopy(not(palette^.allowConfiguration));
    for g in board^.inputs  do g^.fixedPosition:=true;
    for g in board^.outputs do g^.fixedPosition:=true;
    for g in board^.gates   do g^.fixedPosition:=true;
    board^.setCaption(challengeTitle);
    result:=board;
    board^.reset(true);
    palette^.resetCounts;
    for i:=0 to length(palette^.paletteEntries)-1 do with palette^.paletteEntries[i] do
      if prototype<>nil then prototype^.reset;
  end;

PROCEDURE T_challenge.startTesting(CONST board: P_visualBoard);
  begin
    ensureBehavior;
    with testRun do begin
      active:=board^.interfacesMatch(behavior);
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

    result:=board^.coSimulateSteps(stepsToSimulate,timeOutInTicks,ensureBehavior);
    testRun.testStep+=result;
    if result=0 then inc(testRun.testStep);
    if testRun.testStep>=tests[testRun.testInputIndex].maxTotalSteps then begin
      if board^.outputsMatch(behavior) then begin
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

FUNCTION T_challenge.getInfoLabelText(CONST uiIdle: boolean): string;
  begin
    if testRun.active
    then result:='Test '+intToStr(testRun.testInputIndex+1)+' von '+intToStr(length(tests))
    else result:=challengeDescription;
  end;

FUNCTION T_challenge.ensureBehavior:P_compoundGate;
  begin
    if behavior=nil then begin
      Interfaces:=expectedBehavior^.getInterfaces;
      behavior  :=expectedBehavior^.extractBehavior;
    end;
    result:=behavior;
  end;

PROCEDURE T_challenge.dropBehavior;
  begin
    if behavior=nil then exit;
    challengeTestCreationThread.ensureStop;
    dispose(behavior,destroy);
    behavior:=nil;
  end;

PROCEDURE T_challenge.initNewChallenge(CONST expectedAsVisual: P_visualBoard);
  VAR
    totalInputBits:longint=0;
    i:longint;
  begin
    randomize;
    challengeLevel      :=0;
    callengeCompleted   :=false;
    editable            :=true;
    challengeTitle      :=expectedAsVisual^.getCaption;
    challengeDescription:=expectedAsVisual^.getDescription;

    if expectedBehavior<>nil then dispose(expectedBehavior,destroy);
    if resultTemplate  <>nil then dispose(resultTemplate,destroy);
    if palette         <>nil then dispose(palette,destroy);

    new(palette,create);
    palette^.constructingChallenge:=true;
    expectedAsVisual^.extractChallenge(palette,expectedBehavior);
    palette^.constructingChallenge:=false;
    behavior      :=expectedBehavior^.extractBehavior;
    resultTemplate:=expectedBehavior^.cloneAsTrueCopy;

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

PROCEDURE T_testCreator.setNumberOfTestCases(CONST count: longint);
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

PROCEDURE T_testCreator.generateTestCases(CONST continuous:boolean=false; CONST scramble:boolean=true);
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
    if continuous then begin
      for i:=0 to length(tests)-1 do begin
        tests[i].inputs:=inputsByIndex(i);
        tests[i].outputs:=undeterminedOutput(Interfaces);
        tests[i].maxTotalSteps:=1000;
      end;
      if scramble then begin
        k:=length(tests); setLength(tests,k+1);
        for i:=1 to length(tests)-1 do for j:=0 to i-1 do if random<0.5 then begin
          tests[k]:=tests[i]; tests[i]:=tests[j]; tests[j]:=tests[k];
        end;
        setLength(tests,k);
      end;
    end else begin
      for i:=0 to length(tests)-1 do begin
        tests[i].inputs:=randomInput(Interfaces);
        tests[i].outputs:=undeterminedOutput(Interfaces);
        tests[i].maxTotalSteps:=1000;
      end;
    end;
    challengeTestCreationThread.restart;
  end;

PROCEDURE T_testCreator.reInitStepCounts;
  begin
    challengeTestCreationThread.ensureStop;
    challengeTestCreationThread.initialRun:=true;
  end;

PROCEDURE T_testCreator.updateTestCaseResults;
  begin
    challengeTestCreationThread.restart;
  end;

PROCEDURE T_testCreator.copyTestInputs(CONST origin:P_testCreator);
  VAR i:longint;
  begin
    challengeTestCreationThread.ensureStop;
    setLength(tests,length(origin^.tests));
    for i:=0 to length(tests)-1 do begin
      tests[i].inputs       :=origin^.tests[i].inputs;
      tests[i].maxTotalSteps:=origin^.tests[i].maxTotalSteps;
    end;
    for i:=0 to length(Interfaces.outputs)-1 do Interfaces.outputs[i].representation:=origin^.Interfaces.outputs[i].representation;
    challengeTestCreationThread.restart;
  end;

FUNCTION T_challenge.equals(CONST c: P_challenge): boolean;
  VAR i,j:longint;
  begin
    result:=(c^.challengeLevel=challengeLevel)
        and (c^.challengeTitle=challengeTitle)
        and (c^.challengeDescription=challengeDescription)
        and (c^.expectedBehavior^.equals(expectedBehavior))
        and (length(c^.tests)=length(tests));
    if result then for i:=0 to length(tests)-1 do with tests[i] do begin
      result:=result
          and (c^.tests[i].maxTotalSteps=maxTotalSteps)
          and (length(c^.tests[i].inputs)=length(inputs));
      for j:=0 to length(tests[i].inputs)-1 do
        result:=result
           and (c^.tests[i].inputs[j]=inputs[j]);
    end;
  end;

PROCEDURE T_testCreator.updateTestCaseResults(CONST callback: F_caseUpdatedCallback; CONST resume: PBoolean; CONST initCounts: boolean);
  VAR i, stepsDone:longint;
  begin
    behavior^.reset;
    for i:=0 to length(tests)-1 do begin
      tests[i].outputs:=undeterminedOutput(Interfaces);
      tests[i].actuallyActive:=0;
    end;
    if callback<>nil then callback(-1);
    for i:=0 to length(tests)-1 do if (resume=nil) or resume^ then begin
      tests[i].outputs:=behavior^.simulateSteps(tests[i].maxTotalSteps,tests[i].inputs,resume,stepsDone);
      tests[i].actuallyActive:=stepsDone;
      if initCounts then tests[i].maxTotalSteps:=stepsDone+4+stepsDone shr 3;
      if callback<>nil then callback(i);
    end;
    if (resume<>nil) and not(resume^) and (callback<>nil) then callback(length(tests)-1);
  end;

CONSTRUCTOR T_testCreator.createAsChallengeBasis;
  begin
    setLength(tests,0);
    challengeTestCreationThread.create(@self);
    behavior:=nil;
  end;

CONSTRUCTOR T_testCreator.createForAnalysis(CONST visualBoard: P_visualBoard);
  begin
    createAsChallengeBasis;
    behavior:=visualBoard^.extractBehavior;
    Interfaces:=visualBoard^.getInterfaces;
  end;

DESTRUCTOR T_testCreator.destroy;
  VAR i: integer;
  begin
    challengeTestCreationThread.destroy;
    for i:=0 to length(tests)-1 do setLength(tests[i].inputs,0); setLength(tests,0);
    if behavior<>nil then dispose(behavior,destroy);
    setLength(Interfaces.inputs,0);
    setLength(Interfaces.outputs,0);
  end;

INITIALIZATION
  tutorial.create;
FINALIZATION
  tutorial.destroy;

end.

