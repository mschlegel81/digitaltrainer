UNIT testFrameUI;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, ExtCtrls, Grids, StdCtrls, ComCtrls,
  challenges;

TYPE

  { TTestCreationFrame }

  TTestCreationFrame = class(TFrame)
    generateTestCasesLabel: TLabel;
    generateTestCasesLabel1: TLabel;
    adaptStepsLabel: TLabel;
    generateTestCasesShape: TShape;
    generateTestCasesShape1: TShape;
    adaptStepsShape: TShape;
    ProgressLabel: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ProgressShape: TShape;
    Splitter1: TSplitter;
    TestCaseCountEdit: TEdit;
    StepCountEdit: TEdit;
    TestCasesStringGrid: TStringGrid;
    TimingGrid: TStringGrid;
    TestInputsPanel: TPanel;
    Timer1: TTimer;
    PROCEDURE adaptStepsShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE generateTestCasesShape1MouseDown(Sender: TObject;
      button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE generateTestCasesShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE StepCountEditEditingDone(Sender: TObject);
    PROCEDURE TestCaseCountEditEditingDone(Sender: TObject);
    PROCEDURE TestCasesStringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE TestCasesStringGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE Timer1Timer(Sender: TObject);
  private
    testGenerator:P_testCreator;
    lastUpdatedRow:longint;
    maximumNumberOfTestCases:longint;
    PROCEDURE updateTableRow(CONST j:longint);
  public
    PROCEDURE fillTable;
    PROCEDURE setTestGenerator(CONST generator:P_testCreator; CONST maxTestCases:longint);
    PROCEDURE detachTestGenerator;
    PROCEDURE importTable(CONST fileName:string);
    PROCEDURE exportTable(CONST fileName:string);
    PROCEDURE CopyToClipboard;
    PROCEDURE initOnCreate;
  end;

IMPLEMENTATION
USES compoundGates,logicalGates, Graphics,visuals;

{$R *.lfm}

{ TTestCreationFrame }

PROCEDURE TTestCreationFrame.TestCasesStringGridValidateEntry(Sender: TObject;
  aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR
    newStepCount: longint;
    wireValue: T_wireValue;
    i:longint;
  begin
    //Editing output...
    if aCol>length(testGenerator^.Interfaces.inputs) then begin
      newValue:=oldValue;
      exit;
    end;
    //Editing number of steps...
    if aCol=length(testGenerator^.Interfaces.inputs) then begin
      newStepCount:=strToIntDef(newValue,-1);
      if (newStepCount<1) then begin
        newValue:=oldValue;
      end else begin
        testGenerator^.tests[aRow-1].maxTotalSteps:=newStepCount;
        lastUpdatedRow:=-1;
        //TODO: Restart of test generator is only necessary if editing an editor before (or equal to) the current test step
        testGenerator^.updateTestCaseResults;
      end;
      exit;
    end;
    //Editing input...
    wireValue:=parseWire(newValue,testGenerator^.Interfaces.inputs[aCol].wireWidth,testGenerator^.Interfaces.inputs[aCol].representation);
    for i:=0 to wireValue.width-1 do if wireValue.bit[i]=tsv_undetermined then begin
      newValue:=oldValue;
      exit;
    end;
    newValue:=getWireString(wireValue,testGenerator^.Interfaces.inputs[aCol].representation);
    testGenerator^.tests[aRow-1].inputs[aCol]:=wireValue;
    lastUpdatedRow:=-1;
    testGenerator^.updateTestCaseResults;
  end;

PROCEDURE TTestCreationFrame.Timer1Timer(Sender: TObject);
  VAR i, newUpdatedRow: longint;
      timing: T_timingDecils;
  begin
    if testGenerator=nil then exit;
    if Assigned(TestCasesStringGrid.editor) and TestCasesStringGrid.editor.showing then exit;

    newUpdatedRow:=testGenerator^.lastTestCasePrepared;
    if (newUpdatedRow<>lastUpdatedRow) then begin
      if (lastUpdatedRow<0) or (newUpdatedRow<lastUpdatedRow)
      then fillTable
      else for i:=lastUpdatedRow+1 to newUpdatedRow do updateTableRow(i);
      lastUpdatedRow:=newUpdatedRow;

      timing:=testGenerator^.getTimingDecils;
      for i:=0 to 10 do TimingGrid.Cells[1,i+1]:=intToStr(timing[i]);
      TimingGrid.AutoSizeColumns();
      if (length(testGenerator^.tests)<=1) or (newUpdatedRow<0)
      then ProgressShape.width:=0
      else ProgressShape.width:=round(TimingGrid.width*newUpdatedRow/(length(testGenerator^.tests)-1));
      ProgressLabel.caption:=intToStr(newUpdatedRow+1)+' ausgeführt';
    end;
  end;

PROCEDURE TTestCreationFrame.updateTableRow(CONST j: longint);
  VAR i,k:longint;
      gateInterface: T_gateInterface;
  begin
    if (j<0) or (j+1>=TestCasesStringGrid.rowCount) or (j>=length(testGenerator^.tests)) then begin
      fillTable;
      exit;
    end;

    i:=0;
    for gateInterface in testGenerator^.Interfaces.inputs do begin
      TestCasesStringGrid.Cells[i,j+1]:=getWireString(testGenerator^.tests[j].inputs[i],gateInterface.representation);
      inc(i);
    end;
    TestCasesStringGrid.Cells[i,j+1]:=intToStr(testGenerator^.tests[j].maxTotalSteps);
    inc(i);
    TestCasesStringGrid.Cells[i,j+1]:=intToStr(testGenerator^.tests[j].actuallyActive);
    inc(i);
    k:=0;
    for gateInterface in testGenerator^.Interfaces.outputs do begin
      TestCasesStringGrid.Cells[i,j+1]:=getWireString(testGenerator^.tests[j].outputs[k],gateInterface.representation);
      inc(i); inc(k);
    end;

    if (j=length(testGenerator^.tests)-1) then TestCasesStringGrid.AutoSizeColumns;
  end;

PROCEDURE TTestCreationFrame.fillTable;
  VAR i,j,k:longint;
      gateInterface: T_gateInterface;
      timing: T_timingDecils;
  begin
    TestCasesStringGrid.rowCount:=1+length(testGenerator^.tests);
    k:=length(testGenerator^.Interfaces.inputs)+length(testGenerator^.Interfaces.outputs)+2;
    while TestCasesStringGrid.Columns.count>k do TestCasesStringGrid.Columns.delete(TestCasesStringGrid.Columns.count-1);
    while TestCasesStringGrid.Columns.count<k do TestCasesStringGrid.Columns.add;

    //Header
    i:=0;
    for gateInterface in testGenerator^.Interfaces.inputs do begin
      TestCasesStringGrid.Columns[i].color:=colorScheme.tableColor;
      if gateInterface.wireWidth<=1
      then TestCasesStringGrid.Columns[i].title.caption:=gateInterface.name
      else TestCasesStringGrid.Columns[i].title.caption:=gateInterface.name+' ('+ C_multibitWireRepresentationName[gateInterface.representation]+')';
      inc(i);
    end;

    TestCasesStringGrid.Columns[i].color:=colorScheme.tableAlternativeColor;
    TestCasesStringGrid.Columns[i].title.caption:='Schritte';
    inc(i);
    TestCasesStringGrid.Columns[i].color:=colorScheme.tableAlternativeColor;
    TestCasesStringGrid.Columns[i].title.caption:='aktiv für';
    inc(i);
    for gateInterface in testGenerator^.Interfaces.outputs do begin
      TestCasesStringGrid.Columns[i].color:=colorScheme.tableColor;
      if gateInterface.wireWidth<=1
      then TestCasesStringGrid.Columns[i].title.caption:=gateInterface.name
      else TestCasesStringGrid.Columns[i].title.caption:=gateInterface.name+' ('+ C_multibitWireRepresentationName[gateInterface.representation]+')';
      inc(i);
    end;

    //Body
    for j:=0 to length(testGenerator^.tests)-1 do begin
      i:=0;
      for gateInterface in testGenerator^.Interfaces.inputs do begin
        TestCasesStringGrid.Cells[i,j+1]:=getWireString(testGenerator^.tests[j].inputs[i],gateInterface.representation);
        inc(i);
      end;
      TestCasesStringGrid.Cells[i,j+1]:=intToStr(testGenerator^.tests[j].maxTotalSteps);
      inc(i);
      TestCasesStringGrid.Cells[i,j+1]:=intToStr(testGenerator^.tests[j].actuallyActive);
      inc(i);
      k:=0;
      for gateInterface in testGenerator^.Interfaces.outputs do begin
        if k<length(testGenerator^.tests[j].outputs)
        then TestCasesStringGrid.Cells[i,j+1]:=getWireString(testGenerator^.tests[j].outputs[k],gateInterface.representation)
        else TestCasesStringGrid.Cells[i,j+1]:='?';
        inc(i); inc(k);
      end;
    end;
    TestCasesStringGrid.AutoSizeColumns;

    timing:=testGenerator^.getTimingDecils;
    for i:=0 to 10 do TimingGrid.Cells[1,i+1]:=intToStr(timing[i]);
    TimingGrid.AutoSizeColumns();

  end;

PROCEDURE TTestCreationFrame.setTestGenerator(CONST generator: P_testCreator;
  CONST maxTestCases: longint);
  begin
    maximumNumberOfTestCases:=maxTestCases;
    testGenerator:=generator;
    TestCasesStringGrid.editor.Font.color:=clWhite;
    TestCasesStringGrid.editor.color:=clBlack;
    TestCaseCountEdit.text:=intToStr(length(testGenerator^.tests));
    fillTable;
    lastUpdatedRow:=-1;
    Timer1.enabled:=true;
  end;

PROCEDURE TTestCreationFrame.detachTestGenerator;
  begin
    Timer1.enabled:=false;
  end;

PROCEDURE TTestCreationFrame.TestCaseCountEditEditingDone(Sender: TObject);
  VAR newCount: longint;
  begin
    newCount:=strToIntDef(TestCaseCountEdit.text,-1);
    if (newCount>maximumNumberOfTestCases) or (newCount<=0) or (newCount=length(testGenerator^.tests)) then begin
      TestCaseCountEdit.text:=intToStr(length(testGenerator^.tests));
      exit;
    end;
    testGenerator^.setNumberOfTestCases(newCount);
    lastUpdatedRow:=-1;
  end;

PROCEDURE TTestCreationFrame.importTable(CONST fileName: string);
  begin
    //TODO: This is tricky; we have to find out how the columns are encoded. Read and interpret header?
    //TestCasesStringGrid.LoadFromCSVFile(OpenDialog1.fileName,';',false,1);
  end;

PROCEDURE TTestCreationFrame.exportTable(CONST fileName: string);
  begin
    TestCasesStringGrid.SaveToCSVFile(fileName,';');
  end;

PROCEDURE TTestCreationFrame.CopyToClipboard;
  begin
    TestCasesStringGrid.CopyToClipboard;
  end;

PROCEDURE TTestCreationFrame.initOnCreate;
  begin
    addButton(adaptStepsShape,adaptStepsLabel);
    addButton(generateTestCasesShape,generateTestCasesLabel);
    addButton(generateTestCasesShape1,generateTestCasesLabel1)
  end;

PROCEDURE TTestCreationFrame.generateTestCasesShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    lastUpdatedRow:=-1;
    testGenerator^.generateTestCases;
    buttonClicked(generateTestCasesShape);
  end;

PROCEDURE TTestCreationFrame.generateTestCasesShape1MouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    lastUpdatedRow:=-1;
    testGenerator^.generateTestCases(true,false);
    buttonClicked(generateTestCasesShape1);
  end;

PROCEDURE TTestCreationFrame.adaptStepsShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    lastUpdatedRow:=-1;
    testGenerator^.reInitStepCounts;
    buttonClicked(adaptStepsShape);
  end;

PROCEDURE TTestCreationFrame.StepCountEditEditingDone(Sender: TObject);
  VAR newCount: int64;
      anyChange:boolean=false;
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
    for i:=0 to length(testGenerator^.tests)-1 do
    if testGenerator^.tests[i].maxTotalSteps<>newCount then begin
      testGenerator^.tests[i].maxTotalSteps:=newCount;
      anyChange:=true;
    end;
    if anyChange then begin
      testGenerator^.updateTestCaseResults;
      lastUpdatedRow:=-1;
    end;
  end;

PROCEDURE TTestCreationFrame.TestCasesStringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
  CONST nextMode:array[T_multibitWireRepresentation] of T_multibitWireRepresentation=(wr_decimal,wr_2complement,wr_binary);
  begin
    if not(IsColumn) then exit;
    if index<length(testGenerator^.Interfaces.inputs) then begin
      testGenerator^.Interfaces.inputs[index].representation:=nextMode[testGenerator^.Interfaces.inputs[index].representation];
      fillTable;
      exit;
    end;
    dec(index,2+length(testGenerator^.Interfaces.inputs));
    if index<0 then exit;
    if index<length(testGenerator^.Interfaces.outputs) then begin
      testGenerator^.Interfaces.outputs[index].representation:=nextMode[testGenerator^.Interfaces.outputs[index].representation];
      fillTable;
    end;
  end;

end.

