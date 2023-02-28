UNIT visualGates;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Graphics, logicalGates, compoundGates, StdCtrls, ExtCtrls,
  serializationUtil, Forms, Controls, wiringUtil, math, sprites, BGRABitmap,
  BGRACanvas;
TYPE
  P_uiAdapter=^T_uiAdapter;
  P_visualGate=^T_visualGate;
  P_visualBoard=^T_visualBoard;
  T_visualGateArray=array of P_visualGate;

  T_hoverInfo=record
    subElement:(noHit,block,inputConnector,outputConnector);
    ioIndex:longint;
  end;

  {$define includeInterface}
  {$i visualWires.inc}
  {$i uiAdapters.inc}
  {$i visualBoards.inc}

  { T_visualGate }

  T_visualGate=object
    private
      gridWidth,gridHeight:longint;
      behavior:P_abstractGate;
      ioLocations: T_ioLocations;
      ioMode:T_multibitWireRepresentation;

      marked:boolean;
      PROCEDURE ioEditEditingDone(Sender: TObject);
      PROCEDURE ioEditKeyPress(Sender: TObject; VAR key: char);
    public
      canvasPos,
      gridPos:T_point;
      uiAdapter:P_uiAdapter;

      CONSTRUCTOR create(CONST behavior_:P_abstractGate);
      DESTRUCTOR destroy;
      PROCEDURE setupVisuals;

      FUNCTION simulateStep:boolean;
      PROCEDURE paintAll(CONST Canvas:TCanvas; CONST ioOnly:boolean=false);
      PROCEDURE paintAll(CONST Canvas:TCanvas; CONST zoom:longint; CONST ioOnly:boolean=false);
      FUNCTION  clone:P_visualGate;
      PROCEDURE propertyEditDone(CONST paletteElement:boolean; CONST x0,y0:longint);

      FUNCTION getInputPositionInGridSize (CONST index:longint):T_point;
      FUNCTION getOutputPositionInGridSize(CONST index:longint):T_point;

      FUNCTION overlaps(CONST other:P_visualGate):boolean;

      //PROCEDURE setPaletteEntryMouseActions();
      //PROCEDURE setBoardElementMouseActions;
      PROPERTY  getBehavior:P_abstractGate read behavior;

      PROPERTY getGridWidth:longint read gridWidth;
      PROPERTY getGridHeight:longint read gridHeight;
      FUNCTION isAtGridPos(CONST p:T_point; OUT info:T_hoverInfo):boolean;
      PROCEDURE flipInputBits;
  end;
{$undef includeInterface}
IMPLEMENTATION
USES visuals;
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
    writeln('IO_KEY_PRESS ',ord(key));
    if key=#13 then begin
      ioEditEditingDone(Sender);
      uiAdapter^.hideIoEdit;
    end else if key=#32 then begin
      behavior^.setInput(0,parseWire(uiAdapter^.uiElement.ioEdit.text,behavior^.inputWidth(0),ioMode));
      paintAll(uiAdapter^.uiElement.boardImage.Canvas,true);
      uiAdapter^.uiElement.boardImage.Invalidate;
      uiAdapter^.callback.boardModifiedCallback();
      key:=#0;
    end else if not(key in AllowedKeys[ioMode]) then key:=#0;
  end;

PROCEDURE T_visualGate.ioEditEditingDone(Sender: TObject);
  begin
    behavior^.setInput(0,parseWire(uiAdapter^.uiElement.ioEdit.text,behavior^.inputWidth(0),ioMode));
    uiAdapter^.uiElement.ioEdit.text:=getWireString(behavior^.getInput(0),ioMode);
    paintAll(uiAdapter^.uiElement.boardImage.Canvas,true);
    uiAdapter^.uiElement.boardImage.Invalidate;
    uiAdapter^.callback.boardModifiedCallback();
  end;

CONSTRUCTOR T_visualGate.create(CONST behavior_: P_abstractGate);
  begin
    uiAdapter :=nil;
    canvasPos:=pointOf(0,0);
    gridPos  :=pointOf(0,0);
    gridWidth :=0;
    gridHeight:=0;
    behavior:=behavior_;
    ioLocations:=behavior^.getIoLocations;
    setupVisuals;
  end;

DESTRUCTOR T_visualGate.destroy;
  begin
    setLength(ioLocations.p[gt_input],0);
    setLength(ioLocations.p[gt_output],0);
    dispose(behavior,destroy);
  end;

PROCEDURE T_visualGate.setupVisuals;
  begin
    ioLocations:=behavior^.getIoLocations;
    case behavior^.gateType of
      gt_input,gt_output: begin
        gridWidth :=4;
        gridHeight:=4;
      end;
      gt_7segmentDummy: begin
        gridWidth:=4;
        gridHeight:=6;
      end;
      else begin
        gridHeight:=1;
        if (length(ioLocations.p[gt_input])>0) and (length(ioLocations.p[gt_output])>0)
        then gridWidth:=2
        else gridWidth:=1;
        gridHeight:=2*max(gridHeight,max(ioLocations.numberOfLeftInputs,ioLocations.numberOfRightOutputs ));
        gridWidth :=2*max(gridWidth ,max(ioLocations.numberOfTopInputs ,ioLocations.numberOfBottomOutputs));
      end;
    end;
  end;

FUNCTION T_visualGate.simulateStep: boolean;
  begin
    result:=behavior^.simulateStep;
  end;

PROCEDURE T_visualGate.paintAll(CONST Canvas: TCanvas; CONST ioOnly: boolean);
  begin
    canvasPos:=uiAdapter^.gridToCanvas(gridPos);
    paintAll(Canvas,uiAdapter^.zoom,ioOnly);
  end;

PROCEDURE T_visualGate.paintAll(CONST Canvas: TCanvas; CONST zoom: longint;
  CONST ioOnly: boolean);
  FUNCTION myInputIndex:longint;
    begin
      if behavior^.gateType<>gt_input then exit(-1);
      result:=P_inputGate(behavior)^.ioIndex;
    end;

  VAR k: integer;
      p: T_point;
  begin
    case behavior^.gateType of
      gt_input,gt_output: begin
        if not(ioOnly) then
        getIoBlockSprite(behavior^.getCaption,myInputIndex,marked)^.renderAt(Canvas,zoom,canvasPos);
        getIoTextSprite(behavior^.getInput(0),ioMode)^.renderAt(Canvas,zoom,canvasPos);
      end;
      gt_7segmentDummy:
        get7SegmentSprite(behavior^.getInput(0),marked)^.renderAt(Canvas,zoom,canvasPos);
      else begin
        if not(ioOnly) then
        getBlockSprite  (behavior^.getCaption,gridWidth,gridHeight,marked)^.renderAt(Canvas,zoom,canvasPos);
      end;
    end;

    for k:=0 to length(ioLocations.p[gt_input])-1 do with ioLocations.p[gt_input,k] do
      if leftOrRight then begin
        p:=pointOf(canvasPos[0],
                   canvasPos[1]+zoom*((positionIndex*2-(ioLocations.numberOfLeftInputs-1))+gridHeight div 2));
        getIoSprite(io_right,behavior^.getInput(k),ioLabel)^.renderAt(Canvas,zoom,p);
      end else begin
        p:=pointOf(canvasPos[0]+zoom*((positionIndex*2-(ioLocations.numberOfTopInputs-1))+gridWidth div 2),
                   canvasPos[1]);
        getIoSprite(io_bottom,behavior^.getInput(k),ioLabel)^.renderAt(Canvas,zoom,p);
      end;

    for k:=0 to length(ioLocations.p[gt_output])-1 do with ioLocations.p[gt_output,k] do
      if leftOrRight then begin
        p:=pointOf(canvasPos[0]+gridWidth*zoom,
                   canvasPos[1]+zoom*((positionIndex*2-(ioLocations.numberOfRightOutputs-1))+gridHeight div 2));
        getIoSprite(io_left,behavior^.getOutput(k),ioLabel)^.renderAt(Canvas,zoom,p);
      end else begin
        p:=pointOf(canvasPos[0]+zoom*((positionIndex*2-(ioLocations.numberOfBottomOutputs-1))+gridWidth div 2),
                   canvasPos[1]+zoom*gridHeight);
        getIoSprite(io_top,behavior^.getOutput(k),ioLabel)^.renderAt(Canvas,uiAdapter^.getZoom,p);
      end;
  end;

FUNCTION T_visualGate.clone: P_visualGate;
  begin
    new(result,create(behavior^.clone(true)));
    result^.gridPos  :=gridPos;
    result^.uiAdapter:=uiAdapter;
    result^.canvasPos:=canvasPos;
    result^.ioMode   :=ioMode;
  end;

PROCEDURE T_visualGate.propertyEditDone(CONST paletteElement: boolean;
  CONST x0, y0: longint);
  begin
    setupVisuals;
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

FUNCTION T_visualGate.isAtGridPos(CONST p: T_point; OUT info: T_hoverInfo
  ): boolean;
  VAR i:longint;
      d: T_point;
  begin
    if (p[0]<gridPos[0]) or (p[1]<gridPos[1]) or (p[0]>gridPos[0]+gridWidth) or (p[1]>gridPos[1]+gridHeight) then exit(false);

    for i:=0 to length(ioLocations.p[gt_input])-1 do begin
      d:=gridPos+getInputPositionInGridSize(i);
      if (d=p) then begin
        info.subElement:=inputConnector;
        info.ioIndex:=i;
        exit(true);
      end;
    end;

    for i:=0 to length(ioLocations.p[gt_output])-1 do begin
      d:=gridPos+getOutputPositionInGridSize(i);
      if (d=p) then begin
        info.subElement:=outputConnector;
        info.ioIndex:=i;
        exit(true);
      end;
    end;

    if p[1]-gridPos[1]>=gridHeight div 2 then info.ioIndex:=1 else info.ioIndex:=0;
    info.subElement:=block;
    result:=true;
  end;

PROCEDURE T_visualGate.flipInputBits;
  CONST flipped:array[T_triStateValue] of T_triStateValue=//tsv_false,tsv_undetermined,tsv_true
                                                          (tsv_true,tsv_false,tsv_false);
  VAR wireValue: T_wireValue;
      b:byte;
  begin
    wireValue:=behavior^.getInput(0);
    for b:=0 to wireValue.width-1 do wireValue.bit[b]:=flipped[wireValue.bit[b]];
    behavior^.setInput(0,wireValue);
    paintAll(uiAdapter^.uiElement.boardImage.Canvas);
    uiAdapter^.uiElement.boardImage.Invalidate;
    uiAdapter^.callback.boardModifiedCallback();
    uiAdapter^.hideIoEdit;
  end;

//PROCEDURE T_visualGate.setPaletteEntryMouseActions;
//  begin
//    if length(shapes)=0 then exit;
//    shapes[0].OnMouseDown:=@paletteEntryMouseDown;
//    labels[0].OnMouseDown:=@paletteEntryMouseDown;
//    shapes[0].OnMouseMove:=@uiAdapter^.paletteEntryMouseMove;
//    labels[0].OnMouseMove:=@uiAdapter^.paletteEntryMouseMove;
//    shapes[0].OnMouseUp:=@uiAdapter^.paletteEntryMouseUp;
//    labels[0].OnMouseUp:=@uiAdapter^.paletteEntryMouseUp;
//    shapes[0].ShowHint:=true;
//    shapes[0].Hint:=behavior^.getDescription;
//  end;

//PROCEDURE T_visualGate.paletteEntryMouseDown(Sender: TObject;
//  button: TMouseButton; Shift: TShiftState; X, Y: integer);
//  VAR clonedSelf:P_visualGate;
//  begin
//    if uiAdapter=nil then exit;
//    if uiAdapter^.state<>uas_initial then exit;
//
//    if button=mbLeft then begin
//      clonedSelf:=clone;
//      clonedSelf^.ensureGuiElements(uiAdapter^.mainForm);
//      clonedSelf^.paintAll(uiAdapter^.boardUiElements.wireImage, shapes[0].Left,shapes[0].top,uiAdapter^.zoom);
//      uiAdapter^.startDrag(x,y,TGraphicControl(Sender),
//                           clonedSelf,
//                           uas_draggingFromPalette);
//    end else if button=mbRight then begin
//      uiAdapter^.showPropertyEditorCallback(@self,false,shapes[0].Left+shapes[0].width,shapes[0].top);
//    end
//  end;

//PROCEDURE T_visualGate.boardElementMouseDown(Sender: TObject;
//  button: TMouseButton; Shift: TShiftState; X, Y: integer);
//  begin
//    if uiAdapter=nil then exit;
//    if uiAdapter^.state<>uas_initial then exit;
//
//    if button=mbLeft then begin
//      if ssShift in Shift then begin
//        marked:=not(marked);
//        updateVisuals;
//      end else uiAdapter^.startDrag(x,y,TGraphicControl(Sender),
//                                    @self,
//                                    uas_draggingFromBoard);
//    end else if button=mbRight then begin
//      uiAdapter^.showPropertyEditorCallback(@self,true,shapes[0].Left+shapes[0].width,shapes[0].top);
//    end;
//  end;

//PROCEDURE T_visualGate.boardElementOutputMouseDown(Sender: TObject;
//  button: TMouseButton; Shift: TShiftState; X, Y: integer);
//  begin
//    if uiAdapter=nil then exit;
//    if uiAdapter^.state<>uas_initial then exit;
//    if button=mbLeft then begin
//      uiAdapter^.startDrag(x,y,TGraphicControl(Sender),
//                           @self,
//                           uas_draggingWire,
//                           TGraphicControl(Sender).Tag);
//    end;
//  end;

//PROCEDURE T_visualGate.ioModeShapeMouseDown(Sender: TObject;
//  button: TMouseButton; Shift: TShiftState; X, Y: integer);
//  CONST next:array[T_multibitWireRepresentation] of T_multibitWireRepresentation=(wr_decimal,wr_2complement,wr_binary);
//        prev:array[T_multibitWireRepresentation] of T_multibitWireRepresentation=(wr_2complement,wr_binary,wr_decimal);
//
//  begin
//    if button=mbLeft
//    then ioMode:=next[ioMode]
//    else ioMode:=prev[ioMode];
//    labels[length(labels)-1].caption:=C_multibitWireRepresentationName[ioMode];
//    ioEdit.text:=getWireString(behavior^.getInput(0),ioMode);
//    updateVisuals;
//  end;

//PROCEDURE T_visualGate.setBoardElementMouseActions;
//  VAR index,i:longint;
//
//  begin
//    if length(shapes)=0 then exit;
//    shapes[0].OnMouseDown:=@boardElementMouseDown;
//    labels[0].OnMouseDown:=@boardElementMouseDown;
//    shapes[0].OnMouseMove:=@uiAdapter^.boardElementMouseMove;
//    labels[0].OnMouseMove:=@uiAdapter^.boardElementMouseMove;
//    shapes[0].OnMouseUp:=@uiAdapter^.boardElementMouseUp;
//    labels[0].OnMouseUp:=@uiAdapter^.boardElementMouseUp;
//    index:=1+behavior^.numberOfInputs;
//    for i:=0 to behavior^.numberOfOutputs-1 do begin
//      shapes[index].Tag:=i;
//      shapes[index].OnMouseDown:=@boardElementOutputMouseDown;
//      shapes[index].OnMouseMove:=@uiAdapter^.boardOutputMouseMove;
//      shapes[index].OnMouseUp  :=@uiAdapter^.boardOutputMouseUp;
//      labels[index].Tag:=i;
//      labels[index].OnMouseDown:=@boardElementOutputMouseDown;
//      labels[index].OnMouseMove:=@uiAdapter^.boardOutputMouseMove;
//      labels[index].OnMouseUp  :=@uiAdapter^.boardOutputMouseUp;
//      inc(index);
//    end;
//
//    if behavior^.gateType in [gt_input,gt_output] then begin
//      index:=length(shapes)-1;
//      shapes[index].OnMouseDown:=@ioModeShapeMouseDown;
//      labels[index].OnMouseDown:=@ioModeShapeMouseDown;
//    end;
//
//    if behavior^.gateType=gt_input then begin
//      ioEdit.OnKeyPress:=@ioEditKeyPress;
//      ioEdit.OnEditingDone:=@ioEditEditingDone;
//    end;
//
//    shapes[0].ShowHint:=true;
//    shapes[0].Hint:=behavior^.getDescription;
//  end;

//procedure T_visualGate.BringToFront;
//  VAR i:longint;
//  begin
//    if length(shapes)=0 then exit;
//    for i:=0 to length(shapes)-1 do shapes[i].BringToFront;
//    for i:=0 to length(labels)-1 do labels[i].BringToFront;
//    if ioEdit<>nil then ioEdit.BringToFront;
//  end;

end.

