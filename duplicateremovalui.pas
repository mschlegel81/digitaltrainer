UNIT duplicateRemovalUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, compoundGates, challenges, workspaces,visualGates,sprites,myGenerics;

TYPE

  { TDuplicateRemovalDialog }

  TDuplicateRemovalDialog = class(TForm)
    CaptionLabelA: TLabel;
    CaptionLabelB: TLabel;
    deleteBLabel: TLabel;
    deleteBShape: TShape;
    DetailsMemoA: TMemo;
    DetailsMemoB: TMemo;
    generateTestCasesLabel: TLabel;
    generateTestCasesLabel1: TLabel;
    deleteALabel: TLabel;
    generateTestCasesShape: TShape;
    generateTestCasesShape1: TShape;
    CompareCaptionLabel: TLabel;
    deleteAShape: TShape;
    Label6: TLabel;
    Label7: TLabel;
    StepCountEdit: TEdit;
    CandidatesPanel: TPanel;
    ComparisonPanel: TPanel;
    ElementAPanel: TPanel;
    ElementBPanel: TPanel;
    CandidateStringGrid: TStringGrid;
    TestCasesStringGrid: TStringGrid;
    TestCaseCountEdit: TEdit;
    Timer1: TTimer;
    PROCEDURE CandidateStringGridSelection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE deleteAShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE deleteBShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE generateTestCasesShape1MouseDown(Sender: TObject;
      button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE generateTestCasesShapeMouseDown(Sender: TObject;
      button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE StepCountEditEditingDone(Sender: TObject);
    PROCEDURE TestCaseCountEditEditingDone(Sender: TObject);
    PROCEDURE TestCasesStringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE TestCasesStringGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE Timer1Timer(Sender: TObject);
  private
    indexA ,indexB  :longint;
    testerA,testerB :P_testCreator;
    //TODO: visualA,visualB :P_visualGate; (Create visual representations, scale them [!!!] to fit into the boxes and paint them...)
    candidateIndexes:T_arrayOfLongint;

    lastUpdatedA,lastUpdatedB:longint;
    PROCEDURE fillTable;
  public
    PROCEDURE showFor(CONST paletteEntryIndex:longint);

  end;

FUNCTION DuplicateRemovalDialog: TDuplicateRemovalDialog;

IMPLEMENTATION
USES paletteHandling, logicalGates,visuals;
VAR myDuplicateRemovalDialog: TDuplicateRemovalDialog=nil;
FUNCTION DuplicateRemovalDialog: TDuplicateRemovalDialog;
  begin
    if myDuplicateRemovalDialog=nil then myDuplicateRemovalDialog:=TDuplicateRemovalDialog.create(nil);
    result:=myDuplicateRemovalDialog;
  end;

{$R *.lfm}

{ TDuplicateRemovalDialog }

PROCEDURE TDuplicateRemovalDialog.TestCaseCountEditEditingDone(Sender: TObject);
  VAR newCount: longint;
  begin
    newCount:=strToIntDef(TestCaseCountEdit.text,-1);
    if (newCount<=0) or (newCount=length(testerA^.tests)) then begin
      TestCaseCountEdit.text:=intToStr(length(testerA^.tests));
      exit;
    end;
    testerA^.setNumberOfTestCases(newCount);
    if testerB<>nil then testerB^.copyTestInputs(testerA);
    lastUpdatedA:=-1;
    lastUpdatedB:=-1;
  end;

PROCEDURE TDuplicateRemovalDialog.TestCasesStringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
  CONST nextMode:array[T_multibitWireRepresentation] of T_multibitWireRepresentation=(wr_decimal,wr_2complement,wr_binary);
  begin
    if not(IsColumn) then exit;
    if index<length(testerA^.Interfaces.inputs) then begin
      testerA^.Interfaces.inputs[index].representation:=nextMode[testerA^.Interfaces.inputs[index].representation];
      fillTable;
      exit;
    end;
    dec(index,1+length(testerA^.Interfaces.inputs));
    if index<0 then exit;
    index:=index mod length(testerA^.Interfaces.outputs);

    testerA^.Interfaces.outputs[index].representation:=nextMode[testerA^.Interfaces.outputs[index].representation];
    fillTable;
  end;

PROCEDURE TDuplicateRemovalDialog.TestCasesStringGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR
    newStepCount: longint;
    wireValue: T_wireValue;
    i:longint;
  begin
    //Editing output...
    if aCol>length(testerA^.Interfaces.inputs) then begin
      newValue:=oldValue;
      exit;
    end;
    //Editing number of steps...
    if aCol=length(testerA^.Interfaces.inputs) then begin
      newStepCount:=strToIntDef(newValue,-1);
      if (newStepCount<1) then begin
        newValue:=oldValue;
      end else begin
        testerA^.tests[aRow-1].maxTotalSteps:=newStepCount;
        lastUpdatedA:=-1;
        lastUpdatedB:=-1;
        testerA^.updateTestCaseResults;
        if testerB<>nil then testerB^.copyTestInputs(testerA);
      end;
      exit;
    end;
    //Editing input...
    wireValue:=parseWire(newValue,testerA^.Interfaces.inputs[aCol].wireWidth,testerA^.Interfaces.inputs[aCol].representation);
    for i:=0 to wireValue.width-1 do if wireValue.bit[i]=tsv_undetermined then begin
      newValue:=oldValue;
      exit;
    end;
    newValue:=getWireString(wireValue,testerA^.Interfaces.inputs[aCol].representation);
    testerA^.tests[aRow-1].inputs[aCol]:=wireValue;
    lastUpdatedA:=-1;
    lastUpdatedB:=-1;
    testerA^.updateTestCaseResults;
    if testerB<>nil then testerB^.copyTestInputs(testerA);
  end;

PROCEDURE TDuplicateRemovalDialog.Timer1Timer(Sender: TObject);
  VAR
    ka, kb: longint;
  begin
    if Assigned(TestCasesStringGrid.editor) and TestCasesStringGrid.editor.showing then exit;

    ka:=testerA^.lastTestCasePrepared;
    if testerB<>nil
    then kb:=testerB^.lastTestCasePrepared
    else kb:=maxLongint;

    if (ka<>lastUpdatedA) or (kb<>lastUpdatedB) then begin
      fillTable;
      lastUpdatedA:=ka;
      lastUpdatedB:=kb;
    end;
  end;

PROCEDURE TDuplicateRemovalDialog.StepCountEditEditingDone(Sender: TObject);
  VAR newCount: int64;
      i:longint;
  begin
    newCount:=StrToInt64Def(StepCountEdit.text,-1);
    if newCount>maxLongint
    then begin
      newCount:=maxLongint;
      StepCountEdit.text:=intToStr(maxLongint);
    end else if (newCount<=0) then begin
      StepCountEdit.text:='';
      exit;
    end;
    for i:=0 to length(testerA^.tests)-1 do testerA^.tests[i].maxTotalSteps:=newCount;
    testerA^.updateTestCaseResults;
    if testerB<>nil then testerB^.copyTestInputs(testerA);
    lastUpdatedA:=-1;
    lastUpdatedB:=-1;
  end;

PROCEDURE TDuplicateRemovalDialog.CandidateStringGridSelection(Sender: TObject; aCol, aRow: integer);
  begin
    dec(aRow);
    if (aRow>=0) and (aRow<length(candidateIndexes)) then begin
      indexB:=candidateIndexes[aRow];
      DetailsMemoB.text:=workspace.getWorkspacePalette^.describeEntry(indexB);
      if testerB<>nil then dispose(testerB,destroy);
      new(testerB,createForAnalysis(workspace.getWorkspacePalette^.paletteEntries[indexB].prototype));
      testerB^.copyTestInputs(testerA);
      lastUpdatedB:=-1;
    end;
  end;

PROCEDURE TDuplicateRemovalDialog.deleteAShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    Timer1.enabled:=false;
    addBackup(@workspace,wht_beforeDuplicateRemoval);
    workspace.clearPreviousStates;
    workspace.getWorkspacePalette^.removeEntryReplacing(indexB,indexA);
    ModalResult:=mrOk;
  end;

PROCEDURE TDuplicateRemovalDialog.deleteBShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    Timer1.enabled:=false;
    addBackup(@workspace,wht_beforeDuplicateRemoval);
    workspace.clearPreviousStates;
    workspace.getWorkspacePalette^.removeEntryReplacing(indexA,indexB);
    ModalResult:=mrOk;
  end;

PROCEDURE TDuplicateRemovalDialog.FormResize(Sender: TObject);
  CONST totalSpacing=15;
  VAR k:longint;
  begin
    k:=(width-totalSpacing) shr 1;
    ElementBPanel.width:=k;
    ElementAPanel.width:=k;
  end;

PROCEDURE TDuplicateRemovalDialog.FormShow(Sender: TObject);
  begin
    applyColorScheme(self);
    ElementBPanel.color:=colorScheme.tableAlternativeColor;
    DetailsMemoB .color:=colorScheme.tableAlternativeColor;
  end;

PROCEDURE TDuplicateRemovalDialog.generateTestCasesShape1MouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    testerA^.generateTestCases(true,false);
    if testerB<>nil then testerB^.copyTestInputs(testerA);
    lastUpdatedA:=-1;
    lastUpdatedB:=-1;
  end;

PROCEDURE TDuplicateRemovalDialog.generateTestCasesShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    testerA^.generateTestCases;
    if testerB<>nil then testerB^.copyTestInputs(testerA);
    lastUpdatedA:=-1;
    lastUpdatedB:=-1;
  end;

PROCEDURE TDuplicateRemovalDialog.fillTable;
  VAR i,j,k:longint;
      gateInterface: T_gateInterface;
  begin
    TestCasesStringGrid.rowCount:=1+length(testerA^.tests);
    k:=length(testerA^.Interfaces.inputs)+length(testerA^.Interfaces.outputs)*2+3;
    while TestCasesStringGrid.Columns.count>k do TestCasesStringGrid.Columns.delete(TestCasesStringGrid.Columns.count-1);
    while TestCasesStringGrid.Columns.count<k do TestCasesStringGrid.Columns.add;

    if Assigned(TestCasesStringGrid.editor) then begin
      TestCasesStringGrid.editor.Font.color:=colorScheme.ENABLED_TEXT_COLOR;
      TestCasesStringGrid.editor.color     :=colorScheme.editorBackgroundColor;
    end;
    //Header
    i:=0;
    for gateInterface in testerA^.Interfaces.inputs do begin
      TestCasesStringGrid.Columns[i].color:=colorScheme.tableColor;
      if gateInterface.wireWidth<=1
      then TestCasesStringGrid.Columns[i].title.caption:=gateInterface.name
      else TestCasesStringGrid.Columns[i].title.caption:=gateInterface.name+' ('+ C_multibitWireRepresentationName[gateInterface.representation]+')';
      inc(i);
    end;

    TestCasesStringGrid.Columns[i].color:=colorScheme.tableAlternativeColor;
    TestCasesStringGrid.Columns[i].title.caption:='Schritte';
    inc(i);
    for gateInterface in testerA^.Interfaces.outputs do begin
      TestCasesStringGrid.Columns[i].color:=colorScheme.tableColor;
      if gateInterface.wireWidth<=1
      then TestCasesStringGrid.Columns[i].title.caption:=gateInterface.name
      else TestCasesStringGrid.Columns[i].title.caption:=gateInterface.name+' ('+ C_multibitWireRepresentationName[gateInterface.representation]+')';
      TestCasesStringGrid.Columns[i].readonly:=true;
      inc(i);
    end;
    TestCasesStringGrid.Columns[i].color:=colorScheme.tableColor;
    TestCasesStringGrid.Columns[i].title.caption:='aktiv für';
    TestCasesStringGrid.Columns[i].readonly:=true;
    inc(i);

    if testerB=nil then for gateInterface in testerA^.Interfaces.outputs do begin
      TestCasesStringGrid.Columns[i].color:=colorScheme.tableAlternativeColor;
      TestCasesStringGrid.Columns[i].title.caption:='';
      TestCasesStringGrid.Columns[i].readonly:=true;
    end else for gateInterface in testerB^.Interfaces.outputs do begin
      TestCasesStringGrid.Columns[i].color:=colorScheme.tableAlternativeColor;
      if gateInterface.wireWidth<=1
      then TestCasesStringGrid.Columns[i].title.caption:=gateInterface.name
      else TestCasesStringGrid.Columns[i].title.caption:=gateInterface.name+' ('+ C_multibitWireRepresentationName[gateInterface.representation]+')';
      TestCasesStringGrid.Columns[i].readonly:=true;
      inc(i);
    end;
    TestCasesStringGrid.Columns[i].color:=colorScheme.tableAlternativeColor;
    TestCasesStringGrid.Columns[i].title.caption:='aktiv für';
    TestCasesStringGrid.Columns[i].readonly:=true;
    inc(i);

    //Body
    for j:=0 to length(testerA^.tests)-1 do begin
      i:=0;
      for gateInterface in testerA^.Interfaces.inputs do begin
        TestCasesStringGrid.Cells[i,j+1]:=getWireString(testerA^.tests[j].inputs[i],gateInterface.representation);
        inc(i);
      end;
      TestCasesStringGrid.Cells[i,j+1]:=intToStr(testerA^.tests[j].maxTotalSteps);
      inc(i);
      k:=0;
      for gateInterface in testerA^.Interfaces.outputs do begin
        if k<length(testerA^.tests[j].outputs)
        then TestCasesStringGrid.Cells[i,j+1]:=getWireString(testerA^.tests[j].outputs[k],gateInterface.representation)
        else TestCasesStringGrid.Cells[i,j+1]:='?';
        inc(i); inc(k);
      end;
      TestCasesStringGrid.Cells[i,j+1]:=intToStr(testerA^.tests[j].actuallyActive);
      inc(i);

      k:=0;
      if testerB=nil then for gateInterface in testerA^.Interfaces.outputs do begin
        TestCasesStringGrid.Cells[i,j+1]:='';
        inc(i);
      end else for gateInterface in testerA^.Interfaces.outputs do begin
        if k<length(testerB^.tests[j].outputs)
        then TestCasesStringGrid.Cells[i,j+1]:=getWireString(testerB^.tests[j].outputs[k],gateInterface.representation)
        else TestCasesStringGrid.Cells[i,j+1]:='?';
        inc(i); inc(k);
      end;
      if testerB=nil
      then TestCasesStringGrid.Cells[i,j+1]:=''
      else TestCasesStringGrid.Cells[i,j+1]:=intToStr(testerB^.tests[j].actuallyActive);
      inc(i);
    end;
    TestCasesStringGrid.AutoSizeColumns;
  end;

PROCEDURE TDuplicateRemovalDialog.showFor(CONST paletteEntryIndex: longint);
  VAR prototypeA: P_visualBoard;
      i,row:longint;
  begin
    if workspace.getWorkspacePalette^.paletteEntries[paletteEntryIndex].prototype=nil
    then exit
    else prototypeA:=workspace.getWorkspacePalette^.paletteEntries[paletteEntryIndex].prototype;

    indexA:=paletteEntryIndex;
    indexB:=-1;

    new(testerA,createForAnalysis(prototypeA));
    testerB:=nil;

    candidateIndexes:=workspace.getWorkspacePalette^.findEntriesWithSameInterfaceAs(indexA);
    CandidateStringGrid.rowCount:=length(candidateIndexes)+1;
    row:=1;
    for i in candidateIndexes do begin
      CandidateStringGrid.Cells[0,row]:=
      StringReplace(titleOf(workspace.getWorkspacePalette^.paletteEntries[i]),LineEnding,'\n',[rfReplaceAll]);
      inc(row);
    end;

    DetailsMemoA.text:=workspace.getWorkspacePalette^.describeEntry(indexA);

    lastUpdatedA:=maxLongint;
    lastUpdatedB:=maxLongint;
    Timer1.enabled:=true;
    ShowModal;
    Timer1.enabled:=false;
    dispose(testerA,destroy);
    testerA:=nil;
    if testerB<>nil then begin
      dispose(testerB,destroy);
      testerB:=nil;
    end;
  end;

FINALIZATION
  if myDuplicateRemovalDialog<>nil then FreeAndNil(myDuplicateRemovalDialog);

end.

