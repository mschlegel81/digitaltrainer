UNIT visualGates;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, logicalGates, compoundGates, StdCtrls, ExtCtrls,serializationUtil,Forms,Controls,wiringUtil,math;
TYPE
  P_uiAdapter=^T_uiAdapter;
  P_visualGate=^T_visualGate;
  P_visualBoard=^T_visualBoard;
  T_visualGateArray=array of P_visualGate;
  {$define includeInterface}
  {$i visualWires.inc}
  {$i uiAdapters.inc}
  {$i visualBoards.inc}

  { T_visualGate }

  T_visualGate=object
    private
      gridPos:T_point;
      gridWidth,gridHeight:longint;
      behavior:P_abstractGate;

      ioLocations: T_ioLocations;

      shapes:array of TShape;
      labels:array of TLabel;
      ioEdit:TEdit;
      ioMode:T_multibitWireRepresentation;

      marked:boolean;
      PROCEDURE ioEditEditingDone(Sender: TObject);
      PROCEDURE ioEditKeyPress(Sender: TObject; VAR key: char);
      PROCEDURE paletteEntryMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE boardElementMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE boardElementOutputMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE ioModeShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    public
      uiAdapter:P_uiAdapter;

      CONSTRUCTOR create(CONST behavior_:P_abstractGate);
      DESTRUCTOR destroy;

      PROCEDURE ensureGuiElements(CONST container:TWinControl); virtual;
      PROCEDURE disposeGuiElements; virtual;

      FUNCTION simulateStep:boolean;
      PROCEDURE updateCaption;
      PROCEDURE updateVisuals;
      PROCEDURE paintAll(CONST tgtImage:TImage; CONST x,y:longint; CONST zoom:longint); virtual;
      FUNCTION  clone:P_visualGate;
      PROCEDURE propertyEditDone(CONST paletteElement:boolean; CONST x0,y0:longint);
      FUNCTION visualHeight:longint;
      FUNCTION visualWidth:longint;

      FUNCTION getInputPositionInGridSize (CONST index:longint):T_point;
      FUNCTION getOutputPositionInGridSize(CONST index:longint):T_point;

      FUNCTION overlaps(CONST other:P_visualGate):boolean;

      PROCEDURE setPaletteEntryMouseActions();
      PROCEDURE setBoardElementMouseActions;
      PROPERTY  getBehavior:P_abstractGate read behavior;
      PROCEDURE BringToFront;
  end;
{$undef includeInterface}
IMPLEMENTATION
{$define includeImplementation}
{$i visualWires.inc}
{$i uiAdapters.inc}
{$i visualBoards.inc}
{$undef includeImplementation}
PROCEDURE T_visualGate.ioEditKeyPress(Sender: TObject; VAR key: char);
  VAR AllowedKeys:array[T_multibitWireRepresentation] of set of char=
      (['0','1'     ,#8], //wr_binary,
       ['0'..'9'    ,#8], //wr_decimal,
       ['0'..'9','-',#8]);//wr_2complement
  begin
    if key=#13 then ioEditEditingDone(Sender)
    else if not(key in AllowedKeys[ioMode]) then key:=#0;
  end;

PROCEDURE T_visualGate.ioEditEditingDone(Sender: TObject);
  begin
    behavior^.setInput(0,parseWire(ioEdit.text,behavior^.inputWidth(0),ioMode));
    ioEdit.text:=getWireString(behavior^.getInput(0),ioMode);
    updateVisuals;
    uiAdapter^.boardUiElements.boardModifiedCallback();
  end;

CONSTRUCTOR T_visualGate.create(CONST behavior_: P_abstractGate);
  begin
    uiAdapter :=nil;
    gridPos[0]:=0;
    gridPos[1]:=0;
    gridWidth :=0;
    gridHeight:=0;
    behavior:=behavior_;

    setLength(shapes,0);
    setLength(labels,0);
  end;

DESTRUCTOR T_visualGate.destroy;
  begin
    disposeGuiElements;
    dispose(behavior,destroy);
  end;

PROCEDURE T_visualGate.ensureGuiElements(CONST container: TWinControl);
  VAR i:longint;
      index:longint=1;
  begin
    if length(shapes)>0 then exit;
    ioLocations:=behavior^.getIoLocations;

    gridHeight:=1;
    if (length(ioLocations.p[gt_input])>0) and (length(ioLocations.p[gt_output])>0)
    then gridWidth:=2
    else gridWidth:=1;

    gridHeight:=max(gridHeight,max(ioLocations.numberOfLeftInputs,ioLocations.numberOfRightOutputs));
    gridWidth :=max(gridWidth,max(ioLocations.numberOfTopInputs,ioLocations.numberOfBottomOutputs));

    gridWidth*=2;
    gridHeight*=2;

    setLength(shapes,1+behavior^.numberOfInputs+behavior^.numberOfOutputs);
    setLength(labels,1+behavior^.numberOfInputs+behavior^.numberOfOutputs);
    shapes[0]:=TShape.create(nil);
    shapes[0].parent:=container;
    shapes[0].Shape :=stRectangle;
    shapes[0].Brush.color:=$00603030;
    shapes[0].Anchors:=[];

    labels[0]:=TLabel.create(nil);
    labels[0].caption:=behavior^.getCaption;
    labels[0].AutoSize:=true;
    labels[0].Font.size:=6;
    labels[0].Font.color:=$00FFFFFF;
    labels[0].parent:=container;
    labels[0].Anchors:=[];
    labels[0].Alignment:=taCenter;

    for i:=0 to behavior^.numberOfInputs-1 do begin
      shapes[index]:=TShape.create(nil);
      shapes[index].Shape:=stCircle;
      shapes[index].Brush.color:=$00804040;
      shapes[index].Pen.color:=$00FFFFFF;
      shapes[index].Tag:=i;
      shapes[index].parent:=container;
      shapes[index].Anchors:=[];

      labels[index]:=TLabel.create(nil);
      labels[index].caption:=ioLocations.p[gt_input,i].ioLabel;
      if labels[index].caption='' then labels[index].visible:=false;
      labels[index].AutoSize:=true;
      labels[index].Font.size:=6;
      labels[index].Font.color:=$00FFFFFF;
      labels[index].parent:=container;
      labels[index].Anchors:=[];
//      labels[index].AnchorVerticalCenterTo(shapes[index]);
//      labels[index].AnchorHorizontalCenterTo(shapes[index]);
      inc(index);
    end;

    for i:=0 to behavior^.numberOfOutputs-1 do begin
      shapes[index]:=TShape.create(nil);
      shapes[index].Shape:=stCircle;
      shapes[index].Tag:=i;
      shapes[index].Brush.color:=$00804040;
      shapes[index].Pen.color:=$00FFFFFF;
      shapes[index].parent:=container;
      shapes[index].Anchors:=[];

      labels[index]:=TLabel.create(nil);
      labels[index].caption:=ioLocations.p[gt_output,i].ioLabel;
      if labels[index].caption='' then labels[index].visible:=false;
      labels[index].AutoSize:=true;
      labels[index].Font.size:=6;
      labels[index].Font.color:=$00FFFFFF;
      labels[index].parent:=container;
      labels[index].Anchors:=[];
//      labels[index].AnchorVerticalCenterTo(shapes[index]);
//      labels[index].AnchorHorizontalCenterTo(shapes[index]);
      inc(index);
    end;

    if behavior^.gateType in [gt_input,gt_output] then begin
      labels[0].Alignment:=taLeftJustify;

      //labels[0].AnchorParallel(akLeft,5,shapes[0]);
      //labels[0].AnchorParallel(akTop ,5,shapes[0]);
      ioMode:=wr_binary;

      ioEdit:=TEdit.create(nil);
      ioEdit.parent:=container;
      ioEdit.Font.color:=$00FFFFFF;
      ioEdit.color:=$00A05050;
      ioEdit.readonly:=behavior^.gateType=gt_output;

      setLength(shapes,index+1);
      setLength(labels,index+1);
      shapes[index]:=TShape.create(nil);
      shapes[index].parent:=container;
      shapes[index].Shape :=stRoundRect;
      shapes[index].Brush.color:=$00A05050;
      shapes[index].Anchors:=[];
      gridWidth:=4;
      gridHeight:=4;

      labels[index]:=TLabel.create(nil);
      labels[index].caption:=C_multibitWireRepresentationName[wr_binary];
      labels[index].AutoSize:=true;
      labels[index].Font.size:=6;
      labels[index].Font.color:=$00FFFFFF;
      labels[index].parent:=container;
      labels[index].Anchors:=[];
//      labels[index].AnchorVerticalCenterTo(shapes[index]);
//      labels[index].AnchorHorizontalCenterTo(shapes[index]);
    end;

  end;

PROCEDURE T_visualGate.disposeGuiElements;
  VAR i:longint;
  begin
    for i:=0 to length(labels)-1 do begin
      labels[i].Anchors:=[];
      FreeAndNil(labels[i]);
    end;
    setLength(labels,0);
    if ioEdit<>nil then FreeAndNil(ioEdit);
    for i:=length(shapes)-1 downto 0 do begin
      shapes[i].Anchors:=[];
      FreeAndNil(shapes[i]);
    end;
    setLength(shapes,0);
  end;

FUNCTION T_visualGate.simulateStep: boolean;
  begin
    result:=behavior^.simulateStep;
  end;

PROCEDURE T_visualGate.updateCaption;
  begin
    labels[0].caption:=behavior^.getCaption;
  end;

PROCEDURE T_visualGate.updateVisuals;
  FUNCTION colorOf(CONST w:T_wireValue):longint;
    begin
      if w.width>1 then begin
        if isFullyDefined(w)
        then result:=$00606060
        else result:=$00804040;
      end else case w.bit[0] of
        tsv_true        : result:=$0000AAFF;
        tsv_false       : result:=$00000000;
        tsv_undetermined: result:=$00804040;
      end;
    end;

  VAR shapeIndex:longint=1;
      i:longint;
  begin
    if length(shapes)=0 then exit;
    if marked
    then shapes[0].Pen.color:=$0000AAFF
    else shapes[0].Pen.color:=$00000000;

    for i:=0 to behavior^.numberOfInputs-1 do begin
      shapes[shapeIndex].Brush.color:=colorOf(behavior^.getInput(i));
      inc(shapeIndex);
    end;
    for i:=0 to behavior^.numberOfOutputs-1 do begin
      shapes[shapeIndex].Brush.color:=colorOf(behavior^.getOutput(i));
      inc(shapeIndex);
    end;

    if (ioEdit<>nil) and not(ioEdit.Focused) then ioEdit.text:=getWireString(behavior^.getInput(0),ioMode);
  end;

PROCEDURE T_visualGate.paintAll(CONST tgtImage:TImage; CONST x, y: longint; CONST zoom: longint);
  VAR k: integer;
      p: T_point;
      shapeIndex:longint=1;
      zoomChanged:boolean=false;
  begin

    shapes[0].Left  :=x;
    shapes[0].top   :=y;
    zoomChanged:=shapes[0].width<>zoom*gridWidth;
    shapes[0].width :=zoom*gridWidth;
    shapes[0].height:=zoom*gridHeight;

    for k:=0 to behavior^.numberOfInputs-1 do begin
      p:=getInputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=x+p[0]*zoom-zoom div 2;
      shapes[shapeIndex].top   :=y+p[1]*zoom-zoom div 2;
      shapes[shapeIndex].width :=                   zoom;
      shapes[shapeIndex].height:=                   zoom;
      inc(shapeIndex);
    end;

    for k:=0 to behavior^.numberOfOutputs-1 do begin
      p:=getOutputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=x+p[0]*zoom-zoom div 2;
      shapes[shapeIndex].top   :=y+p[1]*zoom-zoom div 2;
      shapes[shapeIndex].width :=                   zoom;
      shapes[shapeIndex].height:=                   zoom;
      inc(shapeIndex);
    end;

    if behavior^.gateType in [gt_input,gt_output] then begin
      ioEdit.Left:=shapes[0].Left+5;
      ioEdit.top:=shapes[0].top+shapes[0].height-5-ioEdit.height;
      ioEdit.width:=shapes[0].width-10;

      shapes[2].Left  :=shapes[0].Left+shapes[0].width-zoom*2;
      shapes[2].width :=                               zoom*2;
      shapes[2].top   :=ioEdit.top-ioEdit.height-5;
      shapes[2].height:=ioEdit.height;

      labels[0].Left:=shapes[0].Left+5;
      labels[0].top :=shapes[0].top +5;
      for k:=1 to length(shapes)-1 do begin
        labels[k].Left:=shapes[k].Left+(shapes[k].width-labels[k].width) div 2;
        labels[k].top :=shapes[k].top +(shapes[k].height-labels[k].height) div 2;
      end;
    end else for k:=0 to length(shapes)-1 do begin
      labels[k].Left:=shapes[k].Left+(shapes[k].width-labels[k].width) div 2;
      labels[k].top :=shapes[k].top +(shapes[k].height-labels[k].height) div 2;
    end;

    if zoomChanged then begin
      if behavior^.gateType in [gt_input,gt_output] then begin
        labels[0].Font.size:=min(round(labels[0].Font.size*(shapes[2].Left-shapes[0].Left-10)/labels[0].width),
                                 round(labels[0].Font.size*(shapes[2].top -shapes[0].top -10)/labels[0].height));
        for k:=1 to length(shapes)-1 do if labels[k].visible then
          labels[k].Font.size:=min(round(labels[k].Font.size*shapes[k].width  *0.75/labels[k].width),
                                   round(labels[k].Font.size*shapes[k].height *0.75/labels[k].height));
      end else begin
        for k:=0 to length(shapes)-1 do if labels[k].visible then
          labels[k].Font.size:=min(round(labels[k].Font.size*shapes[k].width  *0.75/labels[k].width),
                                   round(labels[k].Font.size*shapes[k].height *0.75/labels[k].height));
      end;
    end;

  end;

FUNCTION T_visualGate.clone: P_visualGate;
  begin
    new(result,create(behavior^.clone(false)));
    result^.gridPos:=gridPos;
    result^.uiAdapter:=uiAdapter;
  end;

PROCEDURE T_visualGate.propertyEditDone(CONST paletteElement: boolean;
  CONST x0, y0: longint);
  begin
    disposeGuiElements;
    ensureGuiElements(uiAdapter^.mainForm);
    paintAll(uiAdapter^.boardUiElements.wireImage,
             x0+gridPos[0]*uiAdapter^.getZoom,
             y0+gridPos[1]*uiAdapter^.getZoom,uiAdapter^.getZoom);
    updateVisuals;
    if paletteElement
    then setPaletteEntryMouseActions
    else setBoardElementMouseActions;
  end;

FUNCTION T_visualGate.visualHeight: longint;
  VAR s:TShape;
      y0:longint= 1000000;
      y1:longint=-1000000;
      Bottom:longint;
  begin
    for s in shapes do begin
      Bottom:=s.top+s.height;
      if Bottom>y1 then y1:=Bottom;
      if s. top<y0 then y0:=s.top;
    end;
    result:=y1-y0;
    if result<0 then result:=0;
  end;

FUNCTION T_visualGate.visualWidth: longint;
  VAR s:TShape;
      x0:longint= 1000000;
      x1:longint=-1000000;
      Right:longint;
  begin
    for s in shapes do begin
      Right:=s.Left+s.width;
      if Right >x1 then x1:=Right;
      if s.Left<x0 then x0:=s.Left;
    end;
    result:=x1-x0;
    if result<0 then result:=0;
  end;

FUNCTION T_visualGate.getInputPositionInGridSize(CONST index: longint): T_point;
  begin
    if ioLocations.p[gt_input,index].leftOrRight
    then begin
      result[0]:=0;
      result[1]:=(ioLocations.p[gt_input,index].positionIndex*2-(ioLocations.numberOfLeftInputs-1))+gridHeight div 2;
    end else begin
      result[0]:=(ioLocations.p[gt_input,index].positionIndex*2-(ioLocations.numberOfTopInputs-1))+gridWidth div 2;
      result[1]:=0;
    end;
  end;

FUNCTION T_visualGate.getOutputPositionInGridSize(CONST index: longint
  ): T_point;
  begin
    if ioLocations.p[gt_output,index].leftOrRight
    then begin
      result[0]:=gridWidth;
      result[1]:=(ioLocations.p[gt_output,index].positionIndex*2-(ioLocations.numberOfRightOutputs-1))+gridHeight div 2;
    end else begin
      result[0]:=(ioLocations.p[gt_output,index].positionIndex*2-(ioLocations.numberOfBottomOutputs-1))+gridWidth div 2;
      result[1]:=gridHeight;
    end;
  end;

FUNCTION T_visualGate.overlaps(CONST other: P_visualGate): boolean;
  begin
    result:=(max(gridPos[0],other^.gridPos[0])<min(gridPos[0]+gridWidth ,other^.gridPos[0]+other^.gridWidth ))
        and (max(gridPos[1],other^.gridPos[1])<min(gridPos[1]+gridHeight,other^.gridPos[1]+other^.gridHeight));
  end;

PROCEDURE T_visualGate.setPaletteEntryMouseActions;
  begin
    if length(shapes)=0 then exit;
    shapes[0].OnMouseDown:=@paletteEntryMouseDown;
    labels[0].OnMouseDown:=@paletteEntryMouseDown;
    shapes[0].OnMouseMove:=@uiAdapter^.paletteEntryMouseMove;
    labels[0].OnMouseMove:=@uiAdapter^.paletteEntryMouseMove;
    shapes[0].OnMouseUp:=@uiAdapter^.paletteEntryMouseUp;
    labels[0].OnMouseUp:=@uiAdapter^.paletteEntryMouseUp;
    shapes[0].ShowHint:=true;
    shapes[0].Hint:=behavior^.getDescription;
  end;

PROCEDURE T_visualGate.paletteEntryMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR clonedSelf:P_visualGate;
  begin
    if uiAdapter=nil then exit;
    if uiAdapter^.state<>uas_initial then exit;

    if button=mbLeft then begin
      clonedSelf:=clone;
      clonedSelf^.ensureGuiElements(uiAdapter^.mainForm);
      clonedSelf^.paintAll(uiAdapter^.boardUiElements.wireImage, shapes[0].Left,shapes[0].top,uiAdapter^.zoom);
      uiAdapter^.startDrag(x,y,TGraphicControl(Sender),
                           clonedSelf,
                           uas_draggingFromPalette);
    end else if button=mbRight then begin
      uiAdapter^.showPropertyEditorCallback(@self,false,shapes[0].Left+shapes[0].width,shapes[0].top);
    end
  end;

PROCEDURE T_visualGate.boardElementMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if uiAdapter=nil then exit;
    if uiAdapter^.state<>uas_initial then exit;

    if button=mbLeft then begin
      if ssShift in Shift then begin
        marked:=not(marked);
        updateVisuals;
      end else uiAdapter^.startDrag(x,y,TGraphicControl(Sender),
                                    @self,
                                    uas_draggingFromBoard);
    end else if button=mbRight then begin
      uiAdapter^.showPropertyEditorCallback(@self,true,shapes[0].Left+shapes[0].width,shapes[0].top);
    end;
  end;

PROCEDURE T_visualGate.boardElementOutputMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if uiAdapter=nil then exit;
    if uiAdapter^.state<>uas_initial then exit;
    if button=mbLeft then begin
      uiAdapter^.startDrag(x,y,TGraphicControl(Sender),
                           @self,
                           uas_draggingWire,
                           TGraphicControl(Sender).Tag);
    end;
  end;

PROCEDURE T_visualGate.ioModeShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  CONST next:array[T_multibitWireRepresentation] of T_multibitWireRepresentation=(wr_decimal,wr_2complement,wr_binary);
        prev:array[T_multibitWireRepresentation] of T_multibitWireRepresentation=(wr_2complement,wr_binary,wr_decimal);

  begin
    if button=mbLeft
    then ioMode:=next[ioMode]
    else ioMode:=prev[ioMode];
    labels[length(labels)-1].caption:=C_multibitWireRepresentationName[ioMode];
    ioEdit.text:=getWireString(behavior^.getInput(0),ioMode);
    updateVisuals;
  end;

PROCEDURE T_visualGate.setBoardElementMouseActions;
  VAR index,i:longint;

  begin
    if length(shapes)=0 then exit;
    shapes[0].OnMouseDown:=@boardElementMouseDown;
    labels[0].OnMouseDown:=@boardElementMouseDown;
    shapes[0].OnMouseMove:=@uiAdapter^.boardElementMouseMove;
    labels[0].OnMouseMove:=@uiAdapter^.boardElementMouseMove;
    shapes[0].OnMouseUp:=@uiAdapter^.boardElementMouseUp;
    labels[0].OnMouseUp:=@uiAdapter^.boardElementMouseUp;
    index:=1+behavior^.numberOfInputs;
    for i:=0 to behavior^.numberOfOutputs-1 do begin
      shapes[index].Tag:=i;
      shapes[index].OnMouseDown:=@boardElementOutputMouseDown;
      shapes[index].OnMouseMove:=@uiAdapter^.boardOutputMouseMove;
      shapes[index].OnMouseUp  :=@uiAdapter^.boardOutputMouseUp;
      labels[index].Tag:=i;
      labels[index].OnMouseDown:=@boardElementOutputMouseDown;
      labels[index].OnMouseMove:=@uiAdapter^.boardOutputMouseMove;
      labels[index].OnMouseUp  :=@uiAdapter^.boardOutputMouseUp;
      inc(index);
    end;

    if behavior^.gateType in [gt_input,gt_output] then begin
      index:=length(shapes)-1;
      shapes[index].OnMouseDown:=@ioModeShapeMouseDown;
      labels[index].OnMouseDown:=@ioModeShapeMouseDown;
    end;

    if behavior^.gateType=gt_input then begin
      ioEdit.OnKeyPress:=@ioEditKeyPress;
      ioEdit.OnEditingDone:=@ioEditEditingDone;
    end;

    shapes[0].ShowHint:=true;
    shapes[0].Hint:=behavior^.getDescription;
  end;

PROCEDURE T_visualGate.BringToFront;
  VAR i:longint;
  begin
    if length(shapes)=0 then exit;
    for i:=0 to length(shapes)-1 do shapes[i].BringToFront;
    for i:=0 to length(labels)-1 do labels[i].BringToFront;
    if ioEdit<>nil then ioEdit.BringToFront;
  end;

end.

