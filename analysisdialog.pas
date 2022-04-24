UNIT analysisDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids,
  StdCtrls, ExtCtrls, logicGates,baseGate;

TYPE
  T_scaleType=(st_binary,st_unsigned,st_signed);

  T_ranges=array of record
    y0:longint;
    dy:double;
  end;

  T_graphMetaDataEntry=record
    caption:string;
    bitWidth:longint;
    plotY0:longint;
    ranges:T_ranges;
  end;

  { T_graphMetaData }

  T_graphMetaData=object
    input,output:array of T_graphMetaDataEntry;

    PROCEDURE initialize(CONST gate:P_abstractGate);
    PROCEDURE update(CONST scaleType:T_scaleType; CONST imageHeight:longint);
  end;

  T_simulationOutput=object
    startAtStep,endAtStep:longint;
    inputs:array of T_wireValue;
    outputHistory:array of array of record
      stepIndex:longint;
      value:T_wireValue;
    end;

    CONSTRUCTOR create(CONST firstStepIndex:longint);
    DESTRUCTOR destroy;
    PROCEDURE addInput (CONST v:T_wireValue);
    PROCEDURE addOutput(CONST stepIndex,outputIndex:longint; CONST v:T_wireValue);
    PROCEDURE finishRun;

    PROCEDURE updateTable(CONST scaleType:T_scaleType; CONST rowIndex:longint; CONST table:TStringGrid);
    PROCEDURE paint(CONST scaleType:T_scaleType; CONST meta:T_graphMetaData; CONST zoom,startIndex:longint; CONST image:TImage);
    PROCEDURE paintCaptions(CONST scaleType:T_scaleType; VAR meta:T_graphMetaData; CONST image:TImage);
    FUNCTION stepsTotal:longint;
  end;

  { TanalysisForm }

  TanalysisForm = class(TForm)
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Image1: TImage;
    maxResponseTimeLabel: TLabel;
    minResponseTimeLabel: TLabel;
    maximum: TLabel;
    Minimum: TLabel;
    ProgressBar1: TProgressBar;
    ResetCheckBox: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    rbSimOrdered: TRadioButton;
    rbBitFlip: TRadioButton;
    rbSimRandom: TRadioButton;
    rbBinary: TRadioButton;
    rbPositive: TRadioButton;
    rb2Complement: TRadioButton;
    ScrollBar1: TScrollBar;
    TabSheet2: TTabSheet;
    TrackBar1: TTrackBar;
    UpdateTableButton: TButton;
    PageControl1: TPageControl;
    StringGrid: TStringGrid;
    TabSheet1: TTabSheet;
    PROCEDURE rbBinaryChange(Sender: TObject);
    PROCEDURE UpdateTableButtonClick(Sender: TObject);
  private
    clonedGate:P_abstractGate;
    graphMetaData:T_graphMetaData;
    simulationOutputs:array of T_simulationOutput;
    PROCEDURE setupTable;
    PROCEDURE repaintTable;
    PROCEDURE repaintGraph;
  public
    PROCEDURE showForGate (CONST gate:P_abstractGate);
    PROCEDURE showForBoard(CONST board:P_circuitBoard);
  end;

VAR
  analysisForm: TanalysisForm;

IMPLEMENTATION

{$R *.lfm}

{ T_graphMetaData }

PROCEDURE T_graphMetaData.initialize(CONST gate: P_abstractGate);
  VAR i:longint;
  begin
    if (gate^.gateType=gt_compound) then with P_customGate(gate)^ do begin
      setLength(input,length(inputConnections));
      for i:=0 to length(inputConnections)-1 do begin
        input[i].caption :=inputConnections[i].caption;
        input[i].bitWidth:=inputConnections[i].width;
      end;
      setLength(output,length(outputConnections));
      for i:=0 to length(outputConnections)-1 do begin
        output[i].caption :=outputConnections[i].caption;
        output[i].bitWidth:=outputConnections[i].width;
      end;
    end else begin
      setLength(input,gate^.numberOfInputs);
      for i:=0 to gate^.numberOfInputs-1 do begin
        input[i].caption:='in '+intToStr(i);
        input[i].bitWidth:=gate^.inputWidth(i);
      end;
      setLength(output,gate^.numberOfOutputs);
      for i:=0 to gate^.numberOfOutputs-1 do begin
        output[i].caption:='out '+intToStr(i);
        output[i].bitWidth:=gate^.outputWidth(i);
      end;
    end;
  end;

PROCEDURE T_graphMetaData.update(CONST scaleType: T_scaleType; CONST imageHeight: longint);
  PROCEDURE updateEntry(VAR entry:T_graphMetaDataEntry; CONST zoomFactor:double; VAR yTally:longint);
    VAR weightPerRow:double;
        yOffset:double=0;
        dyFactor:double;
        dynamicRange:longint=1;
        k:longint;
    begin
      entry.plotY0:=yTally;
      case scaleType of
        st_binary: begin
          setLength(entry.ranges,entry.bitWidth);
          dyFactor:=1;
        end;
        st_unsigned: begin
          setLength(entry.ranges,1);
          case entry.bitWidth of
            8: begin dyFactor:=8/256; dynamicRange:=256; end;
            4: begin dyFactor:=4/16;  dynamicRange:=16;  end;
          else dyFactor:=1;
          end;
        end;
        st_signed: begin
          setLength(entry.ranges,1);
          dyFactor:=weightPerRow/256;
          case entry.bitWidth of
            8: begin
                 dyFactor:=8/256;
                 yOffset :=dyFactor*128;
                 dynamicRange:=256;
               end;
            4: begin
                 dyFactor:=4/16;
                 yOffset :=dyFactor*8;
                 dynamicRange:=16;
               end
          else begin
                 dyFactor:=1;
                 yOffset:=0;
               end;
          end;
        end;
      end;
      for k:=0 to length(entry.ranges)-1 do begin
        entry.ranges[k].y0:=round(yTally+yOffset);
        entry.ranges[k].dy:=dyFactor*zoomFactor;
        yTally-=round(0.5+1.1*entry.ranges[k].dy*dynamicRange);
      end;
    end;

  VAR yTally:longint;
      verticalZoom:longint;
      i:longint;
  begin
    //20 pixels space at the top, distribute the rest evenly, negate y-axis ("high" values are represented as "low" pixel coordinates)

    //1. increase zoom until representation is too high
    verticalZoom:=1;
    repeat
      yTally:=imageHeight-1;
      for i:=0 to length(input )-1 do updateEntry(input[i] ,verticalZoom,yTally);
      for i:=0 to length(output)-1 do updateEntry(output[i],verticalZoom,yTally);
      inc(verticalZoom);
    until yTally<20;
    //2. decrease soom, so that representation still fits
    if verticalZoom>1 then begin
      dec(verticalZoom);
      yTally:=imageHeight-1;
      for i:=0 to length(input )-1 do updateEntry(input[i] ,verticalZoom,yTally);
      for i:=0 to length(output)-1 do updateEntry(output[i],verticalZoom,yTally);
    end;
    //3. shift in order to center vertically

    // top gap (is)    =                   yTally@post
    // bottom gap (is) = 0 = imageHeight-1-yTally@pre

    //TODO: RECENTER!!!

  end;

{ T_simulationOutput }

CONSTRUCTOR T_simulationOutput.create(CONST firstStepIndex: longint);
  begin
    setLength(inputs,0);
    setLength(outputHistory,0);
    startAtStep:=firstStepIndex;
    endAtStep:=firstStepIndex;
  end;

DESTRUCTOR T_simulationOutput.destroy;
  VAR i:longint;
  begin
    setLength(inputs,0);
    for i:=0 to length(outputHistory)-1 do setLength(outputHistory[i],0);
    setLength(outputHistory,0);
  end;

PROCEDURE T_simulationOutput.addInput(CONST v: T_wireValue);
  VAR k:longint;
  begin
    k:=length(inputs);
    setLength(inputs,k+1);
    inputs[k]:=v;
  end;

PROCEDURE T_simulationOutput.addOutput(CONST stepIndex, outputIndex: longint; CONST v: T_wireValue);
  VAR k:longint;
  begin
    if outputIndex<0 then exit;
    if outputIndex>=length(outputHistory) then setLength(outputHistory,outputIndex+1);

    k:=length(outputHistory[outputIndex]);
    if (k=0) or (outputHistory[outputIndex,k-1].value<>v) then begin
      setLength(outputHistory[outputIndex],k+1);
      outputHistory[outputIndex,k].value    :=v;
      outputHistory[outputIndex,k].stepIndex:=stepIndex;
    end;
  end;

PROCEDURE T_simulationOutput.finishRun;
  VAR outputIndex:longint;
      k:longint;
      maxSteps:longint=0;
  begin
    for outputIndex:=0 to length(outputHistory)-1 do begin
      k:=length(outputHistory[outputIndex])-1;
      if   maxSteps< outputHistory[outputIndex,k].stepIndex
      then maxSteps:=outputHistory[outputIndex,k].stepIndex;
    end;

    for outputIndex:=0 to length(outputHistory)-1 do begin
      k:=length(outputHistory[outputIndex]);
      if outputHistory[outputIndex,k-1].stepIndex<maxSteps then begin
        setLength(outputHistory[outputIndex],k+1);
        outputHistory[outputIndex,k].value:=outputHistory[outputIndex,k-1].value;
        outputHistory[outputIndex,k].stepIndex:=maxSteps;
      end;
    end;

    endAtStep:=startAtStep+maxSteps;
  end;

PROCEDURE T_simulationOutput.updateTable(CONST scaleType: T_scaleType; CONST rowIndex: longint; CONST table: TStringGrid);
  FUNCTION getIoString(CONST wire:T_wireValue):string;
    begin
      case scaleType of
        st_binary  : result:=getBinaryString     (wire);
        st_unsigned: result:=getDecimalString    (wire);
        st_signed  : result:=get2ComplementString(wire);
      end;
    end;

  VAR col:longint=0;
      i,k:longint;
  begin
    if table.rowCount<=rowIndex then table.rowCount:=rowIndex+1;

    col:=0;
    for i:=0 to length(inputs)-1 do begin
      table.Cells[col,rowIndex]:=getIoString(inputs[i]);
      inc(col);
    end;

    if stepsTotal<=1000
    then table.Cells[col,rowIndex]:=intToStr(stepsTotal)
    else table.Cells[col,rowIndex]:='>1000';

    inc(col);
    for i:=0 to length(outputHistory)-1 do begin
      k:=length(outputHistory[i])-1;
      table.Cells[col,rowIndex]:=getIoString(outputHistory[i,k].value);
      inc(col);
    end;
  end;

PROCEDURE T_simulationOutput.paint(CONST scaleType:T_scaleType; CONST meta:T_graphMetaData; CONST zoom,startIndex:longint; CONST image:TImage);
  VAR i:longint;
      RED:boolean=true;
  begin
    if startIndex                      >endAtStep   then exit;
    if startIndex+(image.width-20)*zoom<startAtStep then exit;

    //image.Picture.Bitmap.Canvas.Font.Color:=clBlack;
    //image.Picture.Bitmap.Canvas.Font.Orientation:=0;
    //image.Picture.Bitmap.Canvas.TextOut(startAtStep-startIndex,3,'Lauf #'+intTos...);

    for i:=0 to length(inputs)-1 do begin
      if RED then image.picture.Bitmap.Canvas.Pen.color:=clRed
             else image.picture.Bitmap.Canvas.Pen.color:=clBlue;
      RED:=not(RED);
      //...

    end;
    for i:=0 to length(outputHistory)-1 do begin
      if RED then image.picture.Bitmap.Canvas.Pen.color:=clRed
             else image.picture.Bitmap.Canvas.Pen.color:=clBlue;
      RED:=not(RED);
      //...
    end;

  end;

PROCEDURE T_simulationOutput.paintCaptions(CONST scaleType:T_scaleType; VAR meta:T_graphMetaData; CONST image:TImage);
  VAR i:longint;
      RED:boolean=true;
  begin
    image.picture.Bitmap.setSize(image.width,image.height);
    image.picture.Bitmap.Canvas.Brush.color:=clBtnFace;
    image.picture.Bitmap.Canvas.clear;

    meta.update(scaleType,image.height);

    image.picture.Bitmap.Canvas.Font.Orientation:=900;
    for i:=0 to length(meta.input)-1 do begin
      if RED then image.picture.Bitmap.Canvas.Font.color:=clRed
             else image.picture.Bitmap.Canvas.Font.color:=clBlue;
      RED:=not(RED);
      image.picture.Bitmap.Canvas.textOut(0,meta.input[i].plotY0,meta.input[i].caption);
    end;
    for i:=0 to length(meta.output)-1 do begin
      if RED then image.picture.Bitmap.Canvas.Font.color:=clRed
             else image.picture.Bitmap.Canvas.Font.color:=clBlue;
      RED:=not(RED);
      image.picture.Bitmap.Canvas.textOut(0,meta.output[i].plotY0,meta.output[i].caption);
    end;
  end;

FUNCTION T_simulationOutput.stepsTotal: longint;
  begin
    result:=endAtStep-startAtStep;
  end;

{ TanalysisForm }

PROCEDURE TanalysisForm.UpdateTableButtonClick(Sender: TObject);
  CONST ONE_MINUTE=1/(24*60);
  VAR input:array of boolean;
      inputsGenerated:longint=0;
      expectedTotalInputs:longint=0;
      generationDeadline:double;
  FUNCTION nextInput:boolean;
    VAR k:longint=0;
        progress:longint=0;
    begin
      //progress by time:
      // 100*(now-start)/(generationDeadline-start) ; start=generationDeadline-ONE_MINUTE;
      //=100*(now-generationDeadline+ONE_MINUTE)/ONE_MINUTE;
      progress:=round(ProgressBar1.max*(now-generationDeadline+ONE_MINUTE)/ONE_MINUTE);
      k:=round(inputsGenerated/expectedTotalInputs*ProgressBar1.max);
      if k>progress then progress:=k;
      if progress<0 then progress:=0 else if progress>ProgressBar1.max then progress:=ProgressBar1.max;
      ProgressBar1.position:=progress;

      inc(inputsGenerated);
      if (inputsGenerated>1000) or (length(input)=0) or (now>generationDeadline) then exit(false);

      k:=0;
      if rbSimOrdered.checked then begin
        //Counting up in binary ;-)
        repeat
          input[k]:=not(input[k]);
          if input[k] then exit(true)
                      else inc(k);
        until k>=length(input);
        result:=false;
      end else if rbBitFlip.checked then begin
        k:=random(length(input));
        input[k]:=not(input[k]);
        result:=true;
      end else begin
        for k:=0 to length(input)-1 do input[k]:=(random>0.5);
        result:=true;
      end;
    end;

  VAR i,stepCount:longint;
      simulationStartStep:longint=0;
      simIndex:longint=0;
      minResponseTime:longint=maxLongint;
      maxResponseTime:longint=0;
      c:longint;
      wIn:array of record
        v:T_wireValue;
        firstIndex:longint;
      end;
      startTicks: qword;
      scaleType:T_scaleType;
  begin
    if rbBinary.checked then scaleType:=st_binary
    else if rbPositive.checked then scaleType:=st_unsigned
    else scaleType:=st_signed;

    for i:=0 to length(simulationOutputs)-1 do simulationOutputs[i].destroy;
    setLength(simulationOutputs,0);

    generationDeadline:=now+ONE_MINUTE;
    startTicks:=GetTickCount64;
    StringGrid.rowCount:=1;
    c:=0;
    setLength(wIn,clonedGate^.numberOfInputs);
    for i:=0 to clonedGate^.numberOfInputs-1 do begin
      wIn[i].firstIndex:=c;
      wIn[i].v.width:=clonedGate^.inputWidth(i);
      c+=clonedGate^.inputWidth(i);
    end;
    setLength(input,c);
    if c<10 then expectedTotalInputs:=1 shl c
            else expectedTotalInputs:=1000;

    for i:=0 to length(input)-1 do input[i]:=false;
    clonedGate^.reset;
    StringGrid.BeginUpdate;
    repeat
      setLength(simulationOutputs,simIndex+1);
      simulationOutputs[simIndex].create(simulationStartStep);

      if ResetCheckBox.checked then clonedGate^.reset;
      for c:=0 to length(wIn)-1 do begin
        for i:=0 to wIn[c].v.width-1 do
          if input[wIn[c].firstIndex+i]
          then wIn[c].v.bit[i]:=tsv_true
          else wIn[c].v.bit[i]:=tsv_false;
        clonedGate^.setInput(c,wIn[c].v);
        simulationOutputs[simIndex].addInput(wIn[c].v);
      end;
      stepCount:=0;
      while (stepCount<=1000) and clonedGate^.simulateStep do begin
        for i:=0 to clonedGate^.numberOfOutputs-1 do
        simulationOutputs[simIndex].addOutput(stepCount,i,clonedGate^.getOutput(i));
        inc(stepCount);
      end;

      simulationOutputs[simIndex].finishRun;
      if simulationOutputs[simIndex].stepsTotal<minResponseTime then begin
        minResponseTime:=simulationOutputs[simIndex].stepsTotal;
        minResponseTimeLabel.caption:=intToStr(minResponseTime);
      end;
      if simulationOutputs[simIndex].stepsTotal>maxResponseTime then begin
        maxResponseTime:=simulationOutputs[simIndex].stepsTotal;
        maxResponseTimeLabel.caption:=intToStr(maxResponseTime);
      end;
      simulationStartStep:=simulationOutputs[simIndex].endAtStep;
      simulationOutputs[simIndex].updateTable(scaleType,simIndex+1,StringGrid);

      if GetTickCount64>startTicks+200 then begin
        StringGrid.EndUpdate();
        Application.ProcessMessages;
        StringGrid.BeginUpdate;
        startTicks:=GetTickCount64;
      end;

      inc(simIndex);
    until not(nextInput);
    StringGrid.EndUpdate();
    ProgressBar1.position:=0;

    if TrackBar1.position*simulationStartStep>Image1.width
    then TrackBar1.position:=trunc(Image1.width/simulationStartStep);
    ScrollBar1.position:=0;
    ScrollBar1.min:=0;
    i:=simulationStartStep-trunc(Image1.width/TrackBar1.position);
    if i<=0 then begin
      ScrollBar1.position:=0;
      ScrollBar1.visible:=false;
    end else begin
      ScrollBar1.max:=i;
      ScrollBar1.visible:=true;
    end;
    repaintGraph;
  end;

PROCEDURE TanalysisForm.rbBinaryChange(Sender: TObject);
  begin
    repaintTable;
    repaintGraph;
  end;

PROCEDURE TanalysisForm.setupTable;
  VAR colIndex:longint=0;
      i:longint;
  begin
    for i:=0 to length(simulationOutputs)-1 do simulationOutputs[i].destroy;
    setLength(simulationOutputs,0);

    graphMetaData.initialize(clonedGate);

    StringGrid.colCount:=clonedGate^.numberOfInputs+clonedGate^.numberOfOutputs+1;
    StringGrid.rowCount:=1;
    minResponseTimeLabel.caption:='?';
    maxResponseTimeLabel.caption:='?';

    for i:=0 to length(graphMetaData.input)-1 do begin
      StringGrid.Cells[colIndex,0]:=graphMetaData.input[i].caption;
      inc(colIndex);
    end;
    StringGrid.Cells[colIndex,0]:='Schritte bis stabil';
    inc(colIndex);
    for i:=0 to length(graphMetaData.output)-1 do begin
      StringGrid.Cells[colIndex,0]:=graphMetaData.output[i].caption;
      inc(colIndex);
    end;
  end;

PROCEDURE TanalysisForm.repaintTable;
  VAR simIndex:longint;
      scaleType: T_scaleType;
  begin
    if rbBinary.checked then scaleType:=st_binary
    else if rbPositive.checked then scaleType:=st_unsigned
    else scaleType:=st_signed;
    for simIndex:=0 to length(simulationOutputs)-1 do simulationOutputs[simIndex].updateTable(scaleType,simIndex+1,StringGrid);
  end;

PROCEDURE TanalysisForm.repaintGraph;
  VAR simIndex:longint;
      scaleType: T_scaleType;
  begin
    if length(simulationOutputs)=0 then exit;
    if rbBinary.checked then scaleType:=st_binary
    else if rbPositive.checked then scaleType:=st_unsigned
    else scaleType:=st_signed;

    simulationOutputs[0].paintCaptions(scaleType,graphMetaData,Image1);
    for simIndex:=0 to length(simulationOutputs)-1 do simulationOutputs[simIndex].paint(scaleType,graphMetaData,TrackBar1.position,ScrollBar1.position,Image1);
  end;

PROCEDURE TanalysisForm.showForGate(CONST gate: P_abstractGate);
  begin
    clonedGate:=gate^.clone;
    setupTable;
    ShowModal;
    dispose(clonedGate,destroy);
  end;

PROCEDURE TanalysisForm.showForBoard(CONST board: P_circuitBoard);
  begin
    new(P_customGate(clonedGate),create(board));
    setupTable;
    ShowModal;
    dispose(clonedGate,destroy);
  end;

end.

