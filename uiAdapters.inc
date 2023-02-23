{$ifdef includeInterface}
T_uiState=(uas_initial,
           uas_draggingFromPalette,
           uas_draggingFromBoard,
           uas_multiDragFromBoard,
           uas_draggingWire,
           uas_propertyEditFromBoard,
           uas_propertyEditFromPalette,
           uas_draggingGridOutputX0,
           uas_draggingGridOutputY0,
           uas_draggingSelectionFrame);

F_showPropertyEditorCallback=PROCEDURE (CONST gate:P_visualGate; CONST fromBoard:boolean; CONST mouseX,mouseY:longint) of object;
F_procedure=PROCEDURE() of object;
F_setBoardCallback=PROCEDURE (CONST board:P_visualBoard) of object;

{ T_uiAdapter }

T_uiAdapter=object
  private
    uiElement:record
      selectionShape:TShape;
      boardImage,
      paletteImage:TImage;
      paletteComboBox:TComboBox;
      wireLayer:TBGRABitmap;
      boardHorizontalScrollBar,
      boardVerticalScrollBar,
      paletteScrollBar:TScrollBar;
    end;

    callback:record
      showPropertyEditorCallback:F_showPropertyEditorCallback;
      boardModifiedCallback:F_procedure;
      BeginFormUpdate,EndFormUpdate:F_procedure;
    end;

    zoom:longint;
    paletteWidth:longint;
    state:T_uiState;

    dragData:record
      startX,startY:longint;
      relPosX,relPosY:longint;
      draggedGates:array of P_visualGate;
      outputIndex:longint;

      dragTarget:P_visualGate;
      inputIndex:longint;
    end;

    activeBoard:P_visualBoard;

    helperState:record
      Clipboard:P_visualBoard;
      undoList,
      redoList:array of P_visualBoard;
    end;
  public
    CONSTRUCTOR create(CONST selectionShape:TShape;
                       CONST boardImage,paletteImage:TImage;
                       CONST paletteComboBox:TComboBox;
                       CONST boardHorizontalScrollBar,
                             boardVerticalScrollBar,
                             paletteScrollBar:TScrollBar;
                       CONST showPropertyEditorCallback:F_showPropertyEditorCallback;
                       CONST boardModifiedCallback:F_procedure;
                       CONST BeginFormUpdate,EndFormUpdate:F_procedure);
    DESTRUCTOR destroy;

    //PROCEDURE paletteEntryMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    //PROCEDURE paletteEntryMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    //PROCEDURE boardElementMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    //PROCEDURE boardElementMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    //PROCEDURE boardOutputMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    //PROCEDURE boardOutputMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);

    PROCEDURE paletteSizeUpdated(CONST Right:longint);
    PROCEDURE propertyEditorShown(CONST gate:P_visualGate; CONST fromBoard:boolean);
    FUNCTION draggedGate:P_visualGate;

    PROCEDURE startDrag(CONST evtX,evtY:longint; CONST eventOrigin:TGraphicControl;  CONST gateToDrag:P_visualGate; CONST newState:T_uiState; CONST outputIndex:longint=-1);
    PROCEDURE startDragSelectionFrame(CONST evtX,evtY:longint);
    PROCEDURE endSelectionDrag;

    PROCEDURE zoomIn;
    PROCEDURE zoomOut;
    PROPERTY getZoom:longint read zoom;
    PROCEDURE resetState;

    PROCEDURE clearUndoList;
    PROCEDURE saveStateToUndoList;
    FUNCTION performUndo(CONST setBoard:F_setBoardCallback):boolean;
    FUNCTION performRedo(CONST setBoard:F_setBoardCallback):boolean;

    FUNCTION boardOrigin:T_point;
    FUNCTION gridToCanvas(CONST p:T_point):T_point;
    FUNCTION gridToCanvas(CONST x,y:longint):T_point;
    FUNCTION canvasToGrid(CONST p:T_point):T_point;
    FUNCTION canvasToGrid(CONST x,y:longint):T_point;

    PROPERTY paletteComboBox:TComboBox read uiElement.paletteComboBox;
    FUNCTION paletteCanvas:TCanvas;
    PROCEDURE setPaletteSize(CONST paletteWidthInGridSize,paletteHeightInGridSize:longint);
    FUNCTION paletteYOffset:longint;
  end;
{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_uiAdapter.create(CONST selectionShape: TShape; CONST boardImage,
  paletteImage: TImage; CONST paletteComboBox: TComboBox;
  CONST boardHorizontalScrollBar, boardVerticalScrollBar,
  paletteScrollBar: TScrollBar;
  CONST showPropertyEditorCallback: F_showPropertyEditorCallback;
  CONST boardModifiedCallback: F_procedure; CONST BeginFormUpdate,
  EndFormUpdate: F_procedure);
  begin
    uiElement.selectionShape          :=selectionShape;
    uiElement.boardImage              :=boardImage;
    uiElement.paletteImage            :=paletteImage;
    uiElement.wireLayer               :=TBGRABitmap.create(boardImage.width,boardImage.height);
    uiElement.paletteComboBox         :=paletteComboBox;
    uiElement.boardHorizontalScrollBar:=boardHorizontalScrollBar;
    uiElement.boardVerticalScrollBar  :=boardVerticalScrollBar  ;
    uiElement.paletteScrollBar        :=paletteScrollBar        ;
    callback.showPropertyEditorCallback:=showPropertyEditorCallback;
    callback.boardModifiedCallback:=boardModifiedCallback;
    callback.BeginFormUpdate:=BeginFormUpdate;
    callback.EndFormUpdate:=EndFormUpdate;

    zoom:=30;
    state:=uas_initial;
  end;

DESTRUCTOR T_uiAdapter.destroy;
  begin
    clearUndoList;
    if helperState.Clipboard<>nil then dispose(helperState.Clipboard,destroy);
  end;

PROCEDURE T_uiAdapter.paletteSizeUpdated(CONST Right: longint);
begin

end;

//PROCEDURE T_uiAdapter.paletteEntryMouseMove(Sender: TObject;
//  Shift: TShiftState; X, Y: integer);
//  begin
//    if state=uas_draggingFromPalette then
//    with dragData do begin
//      BeginFormUpdate();
//      draggedGate^.paintAll(boardUiElements.wireImage,x+startX-relPosX,y+startY-relPosY,zoom);
//      EndFormUpdate();
//    end;
//  end;
//
//PROCEDURE T_uiAdapter.paletteEntryMouseUp(Sender: TObject;
//  button: TMouseButton; Shift: TShiftState; X, Y: integer);
//  VAR screenX,screenY:longint;
//  begin
//    if state<>uas_draggingFromPalette then exit;
//    state:=uas_initial;
//    with dragData do begin
//      screenX:=x+startX-relPosX;
//      screenY:=y+startY-relPosY;
//      if activeBoard=nil
//      then dispose(draggedGate,destroy)
//      else activeBoard^.elementAdded(draggedGate,screenX,screenY);
//      setLength(draggedGates,0);
//    end;
//  end;

//PROCEDURE T_uiAdapter.boardElementMouseMove(Sender: TObject;
//  Shift: TShiftState; X, Y: integer);
//  VAR i,g0x,g0y,dx,dy:longint;
//  begin
//    //TODO: Reimplement boardElementMouseMove - we need an additional, transparent canvas
//
//
//    //if state=uas_draggingFromBoard then with dragData do begin
//    //  BeginFormUpdate();
//    //  draggedGate^.paintAll(boardUiElements.wireImage,x+startX-relPosX,y+startY-relPosY,zoom);
//    //  startX+=x-relPosX;
//    //  startY+=y-relPosY;
//    //  EndFormUpdate();
//    //end else if state=uas_multiDragFromBoard then with dragData do begin
//    //  BeginFormUpdate();
//    //  g0x:=draggedGates[0]^.shapes[0].Left;
//    //  g0y:=draggedGates[0]^.shapes[0].top;
//    //  dx:=x+startX-relPosX-g0x;
//    //  dy:=y+startY-relPosY-g0y;
//    //  for i:=length(draggedGates)-1 downto 1 do draggedGates[i]^.paintAll(
//    //    boardUiElements.wireImage,
//    //    draggedGates[i]^.shapes[0].Left+dx,
//    //    draggedGates[i]^.shapes[0].top+dy,
//    //    zoom);
//    //  draggedGate^.paintAll(boardUiElements.wireImage,x+startX-relPosX,y+startY-relPosY,zoom);
//    //  startX+=x-relPosX;
//    //  startY+=y-relPosY;
//    //  EndFormUpdate();
//    //end;
//  end;
//
//PROCEDURE T_uiAdapter.boardElementMouseUp(Sender: TObject;
//  button: TMouseButton; Shift: TShiftState; X, Y: integer);
//  VAR screenX,screenY:longint;
//  begin
//    if state=uas_draggingFromBoard then with dragData do begin
//      screenX:=x+startX-relPosX;
//      screenY:=y+startY-relPosY;
//      activeBoard^.repositionElement(draggedGate,false);
//      setLength(draggedGates,0);
//    end else if state=uas_multiDragFromBoard then with dragData do begin
//      activeBoard^.repositionElement(draggedGates,false);
//      setLength(draggedGates,0);
//    end;
//    state:=uas_initial;
//  end;
//
//PROCEDURE T_uiAdapter.boardOutputMouseMove(Sender: TObject; Shift: TShiftState;
//  X, Y: integer);
//  begin
//    if state=uas_draggingWire then begin
//      activeBoard^.paintWirePreview(draggedGate^.getOutputPositionInGridSize(dragData.outputIndex)
//                                   +draggedGate^.gridPos,
//                                   x+dragData.startX,
//                                   y+dragData.startY);
//    end;
//  end;
//
//PROCEDURE T_uiAdapter.boardOutputMouseUp(Sender: TObject; button: TMouseButton;
//  Shift: TShiftState; X, Y: integer);
//  begin
//    if state=uas_draggingWire then with dragData do
//    if (activeBoard<>nil) and (dragTarget<>nil) then
//      activeBoard^.addWire(draggedGate,outputIndex,dragTarget,inputIndex);
//    state:=uas_initial;
//  end;

//PROCEDURE T_uiAdapter.paletteSizeUpdated(CONST Right: longint);
//  begin
//    if activeBoard<>nil then begin
//      activeBoard^.ui.wireImage.Left:=Right;
//      activeBoard^.checkSizes;
//    end;
//  end;

PROCEDURE T_uiAdapter.propertyEditorShown(CONST gate: P_visualGate;
  CONST fromBoard: boolean);
  begin
    if fromBoard
    then state:=uas_propertyEditFromBoard
    else state:=uas_propertyEditFromPalette;
    setLength(dragData.draggedGates,1);
    dragData.draggedGates[0]:=gate;
  end;

FUNCTION T_uiAdapter.draggedGate: P_visualGate;
  begin
    if length(dragData.draggedGates)>0
    then result:=dragData.draggedGates[0]
    else result:=nil;
  end;

PROCEDURE T_uiAdapter.startDrag(CONST evtX, evtY: longint;
  CONST eventOrigin: TGraphicControl; CONST gateToDrag: P_visualGate;
  CONST newState: T_uiState; CONST outputIndex: longint);
  begin
    state:=newState;
    dragData.startX:=eventOrigin.Left+evtX;
    dragData.startY:=eventOrigin.top +evtY;
    dragData.relPosX:=evtX;
    dragData.relPosY:=evtY;
    setLength(dragData.draggedGates,1);
    dragData.draggedGates[0]:=gateToDrag;
    dragData.outputIndex:=outputIndex;
    dragData.dragTarget:=nil;
    dragData.inputIndex:=-1;

    case state of
      uas_draggingFromBoard: begin;
        if activeBoard^.startMultiDrag(gateToDrag)
        then state:=uas_multiDragFromBoard;
      end;
    end;

  end;

PROCEDURE T_uiAdapter.startDragSelectionFrame(CONST evtX, evtY: longint);
  begin
    state:=uas_draggingSelectionFrame;
    with dragData do begin
      startX:=evtX;
      startY:=evtY;
      with uiElement do begin
        selectionShape.visible:=true;
        selectionShape.Left:=startX;
        selectionShape.top :=startY;
        selectionShape.width :=0;
        selectionShape.height:=0;
      end;
    end;
  end;

PROCEDURE T_uiAdapter.endSelectionDrag;
  begin
    uiElement.selectionShape.visible:=false;
    state:=uas_initial;
  end;

PROCEDURE T_uiAdapter.zoomIn;
  begin
    zoom:=round(zoom*1.1);
  end;

PROCEDURE T_uiAdapter.zoomOut;
  begin
    zoom:=round(zoom/1.1);
  end;

PROCEDURE T_uiAdapter.resetState;
  begin
    state:=uas_initial;
  end;

PROCEDURE T_uiAdapter.clearUndoList;
  VAR i:longint;
  begin
    with helperState do begin
      for i:=0 to length(undoList)-1 do dispose(undoList[i],destroy); setLength(undoList,0);
      for i:=0 to length(redoList)-1 do dispose(redoList[i],destroy); setLength(redoList,0);
    end;
  end;

PROCEDURE T_uiAdapter.saveStateToUndoList;
  VAR i:longint;
  begin
    with helperState do begin
      setLength(undoList,length(undoList)+1);
      undoList[length(undoList)-1]:=activeBoard^.clone;
      for i:=0 to length(redoList)-1 do dispose(redoList[i],destroy); setLength(redoList,0);
    end;
  end;

FUNCTION T_uiAdapter.performUndo(CONST setBoard: F_setBoardCallback): boolean;
  begin
    with helperState do if length(undoList)>0 then begin
      setLength(redoList,length(redoList)+1);
      redoList[length(redoList)-1]:=activeBoard;
      activeBoard^.detachUI;
      activeBoard:=undoList[length(undoList)-1];
      setLength(undoList,length(undoList)-1);
      setBoard(activeBoard);
      activeBoard^.attachUI(@self);
      result:=true;
    end else result:=false;
  end;

FUNCTION T_uiAdapter.performRedo(CONST setBoard: F_setBoardCallback): boolean;
  begin
    with helperState do if length(redoList)>0 then begin
      setLength(undoList,length(undoList)+1);
      undoList[length(undoList)-1]:=activeBoard;
      activeBoard^.detachUI;
      activeBoard:=redoList[length(redoList)-1];
      setLength(redoList,length(redoList)-1);
      setBoard(activeBoard);
      activeBoard^.attachUI(@self);
      result:=true;
    end else result:=false;
  end;

FUNCTION T_uiAdapter.boardOrigin: T_point;
  begin
    result:=pointOf(-uiElement.boardHorizontalScrollBar.position,
                    -uiElement.boardVerticalScrollBar  .position);
  end;

FUNCTION T_uiAdapter.gridToCanvas(CONST p: T_point): T_point;
  begin
    result:=pointOf(p[0]*zoom-uiElement.boardHorizontalScrollBar.position,
                    p[1]*zoom-uiElement.boardVerticalScrollBar  .position);
  end;

FUNCTION T_uiAdapter.gridToCanvas(CONST x, y: longint): T_point;
  begin
    result:=gridToCanvas(pointOf(x,y));
  end;

FUNCTION T_uiAdapter.canvasToGrid(CONST p: T_point): T_point;
  begin
    result:=pointOf(round((p[0]+uiElement.boardHorizontalScrollBar.position)/zoom),
                    round((p[1]+uiElement.boardVerticalScrollBar  .position)/zoom));
  end;

FUNCTION T_uiAdapter.canvasToGrid(CONST x, y: longint): T_point;
  begin
    result:=canvasToGrid(pointOf(x,y));
  end;

FUNCTION T_uiAdapter.paletteCanvas: TCanvas;
  begin
    result:=uiElement.paletteImage.picture.bitmap.Canvas;
  end;

PROCEDURE T_uiAdapter.setPaletteSize(CONST paletteWidthInGridSize, paletteHeightInGridSize: longint);
  VAR paletteWidthInPixels,
      paletteHeightInPixels:longint;
  begin
    paletteWidthInPixels :=paletteWidthInGridSize *zoom;
    paletteHeightInPixels:=paletteHeightInGridSize*zoom;

    if uiElement.paletteImage.height<paletteHeightInPixels then begin
      uiElement.paletteScrollBar.enabled:=false;
      uiElement.paletteScrollBar.max     :=uiElement.paletteImage.height;
      uiElement.paletteScrollBar.PageSize:=uiElement.paletteImage.height;
      uiElement.paletteScrollBar.position:=0;
    end else begin
      uiElement.paletteScrollBar.enabled:=true;
      uiElement.paletteScrollBar.max     :=paletteHeightInPixels;
      uiElement.paletteScrollBar.PageSize:=uiElement.paletteImage.height;
    end;

    uiElement.paletteImage.width:=paletteWidthInPixels;
    uiElement.paletteImage.picture.bitmap.setSize(uiElement.paletteImage.width,uiElement.paletteImage.height);

    //TODO: Check if the board now needs scroll bars, possibly repaint
  end;

FUNCTION T_uiAdapter.paletteYOffset: longint;
  begin
    if uiElement.paletteScrollBar.visible
    then result:=-uiElement.paletteScrollBar.position
    else result:=0;
  end;

{$endif}

