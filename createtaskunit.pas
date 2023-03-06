UNIT createTaskUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Grids, visualGates, challenges,compoundGates;

TYPE

  { TCreateTaskForm }

  TCreateTaskForm = class(TForm)
    addTaskLabel: TLabel;
    addTaskShape: TShape;
    Label7: TLabel;
    RadioPanel1: TPanel;
    rbPreconfiguredPaletteWithCounts: TRadioButton;
    rbPreconfiguredPalette: TRadioButton;
    rbUnconfiguredPaletteWithCounts: TRadioButton;
    rbFreePalette: TRadioButton;
    TestCasesStringGrid: TStringGrid;
    TestCaseCountEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    RadioPanel: TPanel;
    TestInputsPanel: TPanel;
    generateTestCasesLabel: TLabel;
    generateTestCasesShape: TShape;
    timer1: TTimer;
    TitleEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    DescriptionMemo: TMemo;
    rbIncludeAllGates: TRadioButton;
    rbIncludeIO: TRadioButton;
    rbIncludeHalfOfGates: TRadioButton;
    rbIncludeNothing: TRadioButton;
    DifficultyTrackBar: TTrackBar;
    PROCEDURE addTaskShapeChangeBounds(Sender: TObject);
    PROCEDURE addTaskShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE generateTestCasesShapeMouseDown(Sender: TObject;
      button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE rbIncludeAllGatesChange(Sender: TObject);
    PROCEDURE TestCaseCountEditEditingDone(Sender: TObject);
    PROCEDURE TestCasesStringGridEditingDone(Sender: TObject);
    PROCEDURE TestCasesStringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE TestCasesStringGridValidateEntry(Sender: TObject; aCol,
      aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE Timer1StartTimer(Sender: TObject);
    PROCEDURE Timer1Timer(Sender: TObject);
  private
    challenge:P_challenge;
    lastUpdatedRow:longint;

    FUNCTION minimumDifficulty: longint;
    FUNCTION paletteOption:T_challengePaletteOption;
    FUNCTION boardOption:T_challengeBoardOption;

    PROCEDURE fillTable;
    PROCEDURE updateTableRow(CONST j:longint);

  public
    PROCEDURE showFor(CONST board:P_visualBoard; CONST challenges:P_challengeSet);
    PROCEDURE showForExistingChallenge(CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet );
  end;

FUNCTION CreateTaskForm:TCreateTaskForm;
IMPLEMENTATION
USES logicalGates;
{$R *.lfm}
VAR
  myCreateTaskForm: TCreateTaskForm=nil;

FUNCTION CreateTaskForm: TCreateTaskForm;
  begin
    if myCreateTaskForm=nil then myCreateTaskForm:=TCreateTaskForm.create(nil);
    result:=myCreateTaskForm;
  end;

FUNCTION TCreateTaskForm.minimumDifficulty: longint;
  begin
    if      rbIncludeAllGates   .checked then result:=0
    else if rbIncludeHalfOfGates.checked then result:=17
    else if rbIncludeIO         .checked then result:=17*2
                                         else result:=17*3;
    if      rbPreconfiguredPalette         .checked then result+=17
    else if rbUnconfiguredPaletteWithCounts.checked then result+=17*2
    else if rbFreePalette                  .checked then result+=17*3;
  end;

FUNCTION TCreateTaskForm.paletteOption: T_challengePaletteOption;
  begin
    if rbPreconfiguredPalette         .checked then exit(co_preconfiguredPalette);
    if rbUnconfiguredPaletteWithCounts.checked then exit(co_unconfiguredPaletteWithCounts);
    if rbFreePalette                  .checked then exit(co_freePalette);
    result:=co_preconfiguredPaletteWithCounts;
  end;

FUNCTION TCreateTaskForm.boardOption: T_challengeBoardOption;
  begin
    if rbIncludeAllGates   .checked then exit(co_allGates);
    if rbIncludeHalfOfGates.checked then exit(co_halfGates);
    if rbIncludeIO         .checked then exit(co_ioOnly);
    result:=co_none;
  end;

PROCEDURE TCreateTaskForm.fillTable;
  VAR i,j,k:longint;
      gateInterface: T_gateInterface;
  begin
    TestCasesStringGrid.rowCount:=1+length(challenge^.tests);
    TestCasesStringGrid.colCount:=length(challenge^.Interfaces.inputs)+length(challenge^.Interfaces.outputs)+1;

    //Header
    i:=0;
    for gateInterface in challenge^.Interfaces.inputs do begin
      if gateInterface.wireWidth<=1
      then TestCasesStringGrid.Cells[i,0]:=gateInterface.name
      else TestCasesStringGrid.Cells[i,0]:=gateInterface.name+' ('+ C_multibitWireRepresentationName[gateInterface.representation]+')';
      inc(i);
    end;
    TestCasesStringGrid.Cells[i,0]:='Steps';
    inc(i);
    for gateInterface in challenge^.Interfaces.outputs do begin
      if gateInterface.wireWidth<=1
      then TestCasesStringGrid.Cells[i,0]:=gateInterface.name
      else TestCasesStringGrid.Cells[i,0]:=gateInterface.name+' ('+ C_multibitWireRepresentationName[gateInterface.representation]+')';
      inc(i);
    end;

    //Body
    for j:=0 to length(challenge^.tests)-1 do begin
      i:=0;
      for gateInterface in challenge^.Interfaces.inputs do begin
        TestCasesStringGrid.Cells[i,j+1]:=getWireString(challenge^.tests[j].inputs[i],gateInterface.representation);
        inc(i);
      end;
      TestCasesStringGrid.Cells[i,j+1]:=intToStr(challenge^.tests[j].maxTotalSteps)+' ('+intToStr(challenge^.tests[j].actuallyActive)+')';
      inc(i);
      k:=0;
      for gateInterface in challenge^.Interfaces.outputs do begin
        if k<length(challenge^.tests[j].outputs)
        then TestCasesStringGrid.Cells[i,j+1]:=getWireString(challenge^.tests[j].outputs[k],gateInterface.representation)
        else TestCasesStringGrid.Cells[i,j+1]:='?';
        inc(i); inc(k);
      end;
    end;
    TestCasesStringGrid.AutoSizeColumns;

  end;

PROCEDURE TCreateTaskForm.updateTableRow(CONST j: longint);
  VAR i,k:longint;
      gateInterface: T_gateInterface;
  begin
    if (j<0) or (j=length(challenge^.tests)-1) then begin
      fillTable;
      exit;
    end;

    i:=0;
    for gateInterface in challenge^.Interfaces.inputs do begin
      TestCasesStringGrid.Cells[i,j+1]:=getWireString(challenge^.tests[j].inputs[i],gateInterface.representation);
      inc(i);
    end;
    TestCasesStringGrid.Cells[i,j+1]:=intToStr(challenge^.tests[j].maxTotalSteps)+' ('+intToStr(challenge^.tests[j].actuallyActive)+')';
    inc(i);
    k:=0;
    for gateInterface in challenge^.Interfaces.outputs do begin
      TestCasesStringGrid.Cells[i,j+1]:=getWireString(challenge^.tests[j].outputs[k],gateInterface.representation);
      inc(i); inc(k);
    end;
    Application.ProcessMessages;
  end;

{ TCreateTaskForm }

PROCEDURE TCreateTaskForm.TestCasesStringGridHeaderClick(Sender: TObject;
  IsColumn: boolean; index: integer);
  CONST nextMode:array[T_multibitWireRepresentation] of T_multibitWireRepresentation=(wr_decimal,wr_2complement,wr_binary);
  begin
    if not(IsColumn) then exit;
    if index<length(challenge^.Interfaces.inputs) then begin
      challenge^.Interfaces.inputs[index].representation:=nextMode[challenge^.Interfaces.inputs[index].representation];
      fillTable;
      exit;
    end;
    dec(index,1+length(challenge^.Interfaces.inputs));
    if index<0 then exit;
    if index<length(challenge^.Interfaces.outputs) then begin
      challenge^.Interfaces.outputs[index].representation:=nextMode[challenge^.Interfaces.outputs[index].representation];
      fillTable;
    end;
  end;

PROCEDURE TCreateTaskForm.TestCasesStringGridValidateEntry(Sender: TObject;
  aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR
    newStepCount: longint;
    wireValue: T_wireValue;
    i:longint;
  begin
    //Editing output...
    if aCol>length(challenge^.Interfaces.inputs) then begin
      newValue:=oldValue;
      exit;
    end;
    //Editing number of steps...
    if aCol=length(challenge^.Interfaces.inputs) then begin
      newStepCount:=strToIntDef(newValue,-1);
      if (newStepCount<=1) then begin
        newValue:=oldValue;
      end else begin
        challenge^.tests[aRow-1].maxTotalSteps:=newStepCount;
        lastUpdatedRow:=-1;
        challenge^.updateTestCaseResults;
      end;
      exit;
    end;
    //Editing input...
    wireValue:=parseWire(newValue,challenge^.Interfaces.inputs[aCol].wireWidth,challenge^.Interfaces.inputs[aCol].representation);
    for i:=0 to wireValue.width-1 do if wireValue.bit[i]=tsv_undetermined then begin
      newValue:=oldValue;
      exit;
    end;
    newValue:=getWireString(wireValue,challenge^.Interfaces.inputs[aCol].representation);
    challenge^.tests[aRow-1].inputs[aCol]:=wireValue;
    lastUpdatedRow:=-1;
    challenge^.updateTestCaseResults;
  end;

PROCEDURE TCreateTaskForm.Timer1StartTimer(Sender: TObject);
  begin

  end;

PROCEDURE TCreateTaskForm.Timer1Timer(Sender: TObject);
  VAR i, newUpdatedRow: longint;
  begin
    if challenge=nil then exit;
    newUpdatedRow:=challenge^.lastTestCasePrepared;
    if (newUpdatedRow<>lastUpdatedRow) then begin
      if (lastUpdatedRow<0) or (newUpdatedRow<lastUpdatedRow)
      then fillTable
      else for i:=lastUpdatedRow+1 to newUpdatedRow do updateTableRow(i);
      lastUpdatedRow:=newUpdatedRow;
    end;
  end;

PROCEDURE TCreateTaskForm.TestCasesStringGridEditingDone(Sender: TObject);
begin

end;

PROCEDURE TCreateTaskForm.rbIncludeAllGatesChange(Sender: TObject);
  begin
    if DifficultyTrackBar.position<minimumDifficulty then
       DifficultyTrackBar.position:=minimumDifficulty;
  end;

PROCEDURE TCreateTaskForm.generateTestCasesShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    lastUpdatedRow:=-1;
    challenge^.generateTestCases;
  end;

PROCEDURE TCreateTaskForm.FormShow(Sender: TObject);
  begin
  end;

PROCEDURE TCreateTaskForm.addTaskShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
  end;

PROCEDURE TCreateTaskForm.addTaskShapeChangeBounds(Sender: TObject);
  begin
  end;

PROCEDURE TCreateTaskForm.TestCaseCountEditEditingDone(Sender: TObject);
  VAR newCount: longint;
  begin
    newCount:=strToIntDef(TestCaseCountEdit.text,-1);
    if (newCount>256) or (newCount<=0) or (newCount=length(challenge^.tests)) then begin
      TestCaseCountEdit.text:=intToStr(length(challenge^.tests));
      exit;
    end;
    challenge^.setNumberOfTestCases(newCount);
    lastUpdatedRow:=-1;
  end;

PROCEDURE TCreateTaskForm.showFor(CONST board: P_visualBoard; CONST challenges: P_challengeSet);
  begin
    TestCasesStringGrid.editor.Font.color:=clWhite;
    TestCasesStringGrid.editor.color:=clBlack;
    rbIncludeAllGates.Font.color:=clWhite;

    TitleEdit.text:=StringReplace(board^.getCaption,LineEnding,'\n',[rfReplaceAll]);
    DescriptionMemo.text:=board^.getDescription;

    new(challenge,create);
    challenge^.challengeLevel:=minimumDifficulty;
    challenge^.initNewChallenge(board,boardOption,paletteOption);
    TestCaseCountEdit.text:=intToStr(length(challenge^.tests));
    fillTable;
    lastUpdatedRow:=-1;
    timer1.enabled:=true;
    RadioPanel .visible:=true;
    RadioPanel1.visible:=true;
    Label4.visible:=true;
    Label7.visible:=true;

    if ShowModal=mrOk then begin
      challenge^.initNewChallenge(board,boardOption,paletteOption);
      challenge^.challengeLevel:=DifficultyTrackBar.position;
      challenge^.challengeTitle:=TitleEdit.text;
      challenge^.challengeDescription:=DescriptionMemo.text;
      challenges^.add(challenge);
    end else begin
      dispose(challenge,destroy);
    end;
    challenge:=nil;
    timer1.enabled:=false;
  end;

PROCEDURE TCreateTaskForm.showForExistingChallenge(CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet );
  begin
    challenge:=challenges^.challenge[originalChallengeIndex]^.partialClone;
    RadioPanel .visible:=false;
    RadioPanel1.visible:=false;
    Label4.visible:=false;
    Label7.visible:=false;

    TitleEdit.text:=challenge^.challengeTitle;
    DescriptionMemo.text:=challenge^.challengeDescription;
    DifficultyTrackBar.position:=challenge^.challengeLevel;
    TestCaseCountEdit.text:=intToStr(length(challenge^.tests));

    fillTable;
    lastUpdatedRow:=-1;
    timer1.enabled:=true;
    challenge^.updateTestCaseResults;

    if ShowModal=mrOk then begin
      challenge^.challengeLevel:=DifficultyTrackBar.position;
      challenge^.challengeTitle:=TitleEdit.text;
      challenge^.challengeDescription:=DescriptionMemo.text;

      dispose(challenges^.challenge[originalChallengeIndex],destroyPartial);
      challenges^.challenge[originalChallengeIndex]:=challenge;
    end else dispose(challenge,destroyPartial);
    challenge:=nil;
    timer1.enabled:=false;
  end;

FINALIZATION
  if myCreateTaskForm<>nil then FreeAndNil(myCreateTaskForm);
end.

