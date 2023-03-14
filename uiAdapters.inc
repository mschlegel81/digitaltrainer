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
F_repositionPropertyEditorCallback=PROCEDURE (CONST mouseX,mouseY:longint; CONST hideEditor:boolean) of object;
F_procedure=PROCEDURE() of object;
F_setBoardCallback=PROCEDURE (CONST board:P_visualBoard) of object;
F_gateHitInfo=FUNCTION (CONST gridPos:T_point; OUT gate:P_visualGate):boolean of object;

{ T_uiAdapter }

T_uiAdapter=object
  private
    uiElement:record
      selectionShape:TShape;
      paletteWidth:longint;
      boardImage:TImage;
      paletteComboBox:TComboBox;
      wireLayer:TBGRABitmap;
      boardHorizontalScrollBar,
      boardVerticalScrollBar,
      paletteScrollBar:TScrollBar;
      ioEdit:TEdit;
    end;

    callback:record
      showPropertyEditorCallback:F_showPropertyEditorCallback;
      repositionPropertyEditorCallback:F_repositionPropertyEditorCallback;
      boardModifiedCallback:F_procedure;
      BeginFormUpdate,EndFormUpdate:F_procedure;
      paletteItemAt:F_gateHitInfo;
      paintPalette:F_procedure;
      ProcessMessages:F_procedure;
    end;

    zoom:longint;
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
    wiringInBackground:boolean;
    PROCEDURE boardImageMouseWheel(Sender: TObject;  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint;  VAR Handled: boolean);
  public
    CONSTRUCTOR create(CONST selectionShape:TShape;
                       CONST boardImage:TImage;
                       CONST paletteComboBox:TComboBox;
                       CONST boardHorizontalScrollBar,
                             boardVerticalScrollBar,
                             paletteScrollBar:TScrollBar;
                       CONST ioEdit:TEdit;
                       CONST showPropertyEditorCallback:F_showPropertyEditorCallback;
                       CONST repositionPropertyEditorCallback:F_repositionPropertyEditorCallback;
                       CONST boardModifiedCallback:F_procedure;
                       CONST BeginFormUpdate,EndFormUpdate,ProcessMessages:F_procedure);
    DESTRUCTOR destroy;

    PROCEDURE propertyEditorShown(CONST gate:P_visualGate; CONST fromBoard:boolean);
    FUNCTION draggedGate:P_visualGate;

    PROCEDURE startDrag(CONST evtX,evtY:longint; CONST gateToDrag:P_visualGate; CONST newState:T_uiState; CONST outputIndex:longint=-1);
    PROCEDURE startDragSelectionFrame(CONST evtX,evtY:longint);
    PROCEDURE endSelectionDrag;

    PROCEDURE zoomIn;
    PROCEDURE zoomOut;
    PROPERTY getZoom:longint read zoom;
    PROCEDURE resetState;
    PROPERTY getState:T_uiState read state;

    PROCEDURE clearUndoList;
    PROCEDURE saveStateToUndoList;
    FUNCTION performUndo(CONST setBoard:F_setBoardCallback):boolean;
    FUNCTION performRedo(CONST setBoard:F_setBoardCallback):boolean;
    FUNCTION isPrototypeInUse(CONST prototype: P_captionedAndIndexed; OUT usedByActiveBoard,usedByClipboard,usedByUndoList:boolean):boolean;
    PROCEDURE prototypeUpdated(CONST oldPrototype,newPrototype:P_visualBoard);

    FUNCTION boardOrigin:T_point;
    FUNCTION gridToCanvas(CONST p:T_point):T_point;
    FUNCTION gridToCanvas(CONST x,y:longint):T_point;
    FUNCTION canvasToGrid(CONST p:T_point):T_point;
    FUNCTION canvasToGrid(CONST x,y:longint):T_point;

    PROCEDURE palletteConnected(CONST paintEvent:F_procedure; CONST gateHitInfo:F_gateHitInfo);
    PROPERTY paletteComboBox:TComboBox read uiElement.paletteComboBox;
    FUNCTION paletteCanvas:TCanvas;
    PROCEDURE updateBoardScrollbars;
    PROCEDURE setPaletteSize(CONST paletteWidthInGridSize,paletteHeightInGridSize:longint);
    FUNCTION paletteYOffset:longint;
    FUNCTION paletteWidth:longint;
    PROCEDURE paletteIndexChanged;

    PROCEDURE showIoEdit(CONST gate:P_visualGate);
    PROCEDURE hideIoEdit;

    PROCEDURE paintAll;
    PROCEDURE repaintImage;
  end;
{$endif}
{$ifdef includeImplementation}

CONSTRUCTOR T_uiAdapter.create(CONST selectionShape: TShape;
                               CONST boardImage: TImage;
                               CONST paletteComboBox: TComboBox;
                               CONST boardHorizontalScrollBar, boardVerticalScrollBar, paletteScrollBar: TScrollBar;
                               CONST ioEdit: TEdit;
                               CONST showPropertyEditorCallback: F_showPropertyEditorCallback;
                               CONST repositionPropertyEditorCallback:F_repositionPropertyEditorCallback;
                               CONST boardModifiedCallback: F_procedure;
                               CONST BeginFormUpdate, EndFormUpdate, ProcessMessages: F_procedure);
  begin
    uiElement.selectionShape          :=selectionShape;
    uiElement.boardImage              :=boardImage;
    uiElement.wireLayer               :=TBGRABitmap.create(boardImage.width,boardImage.height);
    uiElement.paletteComboBox         :=paletteComboBox;
    uiElement.boardHorizontalScrollBar:=boardHorizontalScrollBar;
    uiElement.boardVerticalScrollBar  :=boardVerticalScrollBar  ;
    uiElement.paletteScrollBar        :=paletteScrollBar        ;
    uiElement.ioEdit                  :=ioEdit;
    callback.showPropertyEditorCallback:=showPropertyEditorCallback;
    callback.repositionPropertyEditorCallback:=repositionPropertyEditorCallback;
    callback.boardModifiedCallback:=boardModifiedCallback;
    callback.BeginFormUpdate:=BeginFormUpdate;
    callback.EndFormUpdate:=EndFormUpdate;
    callback.ProcessMessages:=ProcessMessages;
    uiElement.boardImage.OnMouseWheel:=@boardImageMouseWheel;
    zoom:=30;
    state:=uas_initial;
    wiringInBackground:=true;
  end;

DESTRUCTOR T_uiAdapter.destroy;
  begin
    clearUndoList;
    FreeAndNil(uiElement.wireLayer);
    if helperState.Clipboard<>nil then dispose(helperState.Clipboard,destroy);
  end;

PROCEDURE T_uiAdapter.boardImageMouseWheel(Sender: TObject;  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint;  VAR Handled: boolean);
  VAR g: P_visualGate;
  begin
    if MousePos.X<paletteWidth
    then begin
      if uiElement.paletteScrollBar.visible then begin
        uiElement.paletteScrollBar.position:=uiElement.paletteScrollBar.position-WheelDelta;
        if   uiElement.paletteScrollBar.position> uiElement.paletteScrollBar.max-uiElement.paletteScrollBar.PageSize
        then uiElement.paletteScrollBar.position:=uiElement.paletteScrollBar.max-uiElement.paletteScrollBar.PageSize;
        //TODO: This is also implemented in the main form. How can we deduplicate this?
        paintAll;
        if state=uas_propertyEditFromPalette then begin
          g:=draggedGate;
          callback.repositionPropertyEditorCallback(
            g^.canvasPos[0]+g^.getGridWidth*zoom+uiElement.boardImage.Left,
            g^.canvasPos[1]                     +uiElement.boardImage.top,
            false);
        end;
      end;
    end else begin
      if uiElement.boardVerticalScrollBar.visible
      then begin
        uiElement.boardVerticalScrollBar.position:=uiElement.boardVerticalScrollBar.position-WheelDelta;
        if   uiElement.boardVerticalScrollBar.position> uiElement.boardVerticalScrollBar.max-uiElement.boardVerticalScrollBar.PageSize
        then uiElement.boardVerticalScrollBar.position:=uiElement.boardVerticalScrollBar.max-uiElement.boardVerticalScrollBar.PageSize;
      end;
      activeBoard^.ScrollbarScroll(nil,scTrack,WheelDelta);
    end;
  end;

PROCEDURE T_uiAdapter.propertyEditorShown(CONST gate: P_visualGate; CONST fromBoard: boolean);
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
  CONST gateToDrag: P_visualGate; CONST newState: T_uiState;
  CONST outputIndex: longint);
  begin
    state:=newState;
    hideIoEdit;
    dragData.startX:=evtX;
    dragData.startY:=evtY;
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
    activeBoard^.wiresUpToDate:=false;
    hideIoEdit;
  end;

PROCEDURE T_uiAdapter.zoomOut;
  begin
    zoom:=round(zoom/1.1);
    activeBoard^.wiresUpToDate:=false;
    hideIoEdit;
  end;

PROCEDURE T_uiAdapter.resetState;
  begin
    state:=uas_initial;
    wiringInBackground:=true;
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
      activeBoard^.paintBoard;
      hideIoEdit;
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
      activeBoard^.paintBoard;
      hideIoEdit;
      result:=true;
    end else result:=false;
  end;

FUNCTION T_uiAdapter.isPrototypeInUse(CONST prototype: P_captionedAndIndexed; OUT usedByActiveBoard,usedByClipboard,usedByUndoList:boolean):boolean;
  VAR b:P_visualBoard;
  begin
    usedByActiveBoard:=false;
    usedByClipboard:=false;
    usedByUndoList:=false;
    if (activeBoard<>nil) and (activeBoard^.usesPrototype(prototype)) then usedByActiveBoard:=true;
    with helperState do begin
      if (Clipboard<>nil) and Clipboard^.usesPrototype(prototype) then usedByClipboard:=true;
      for b in undoList do if b^.usesPrototype(prototype) then begin usedByUndoList:=true; exit(true); end;
      for b in redoList do if b^.usesPrototype(prototype) then begin usedByUndoList:=true; exit(true); end;
    end;
    result:=usedByUndoList or usedByClipboard or usedByActiveBoard;
  end;

PROCEDURE T_uiAdapter.prototypeUpdated(CONST oldPrototype, newPrototype: P_visualBoard);
  VAR b:P_visualBoard;
  begin
    if (activeBoard<>nil) then activeBoard^.prototypeUpdated(oldPrototype,newPrototype);
    with helperState do begin
      if (Clipboard<>nil) then Clipboard^.prototypeUpdated(oldPrototype,newPrototype);
      for b in undoList do b^.prototypeUpdated(oldPrototype,newPrototype);
      for b in redoList do b^.prototypeUpdated(oldPrototype,newPrototype);
    end;
  end;

FUNCTION T_uiAdapter.boardOrigin: T_point;
  begin
    result:=pointOf(-uiElement.boardHorizontalScrollBar.position,
                    -uiElement.boardVerticalScrollBar  .position);
  end;

FUNCTION T_uiAdapter.gridToCanvas(CONST p: T_point): T_point;
  begin
    result:=pointOf(p[0]*zoom-uiElement.boardHorizontalScrollBar.position+uiElement.paletteWidth,
                    p[1]*zoom-uiElement.boardVerticalScrollBar  .position);
  end;

FUNCTION T_uiAdapter.gridToCanvas(CONST x, y: longint): T_point;
  begin
    result:=gridToCanvas(pointOf(x,y));
  end;

FUNCTION T_uiAdapter.canvasToGrid(CONST p: T_point): T_point;
  begin
    result:=pointOf(round((p[0]+uiElement.boardHorizontalScrollBar.position-uiElement.paletteWidth)/zoom),
                    round((p[1]+uiElement.boardVerticalScrollBar  .position)/zoom));
  end;

FUNCTION T_uiAdapter.canvasToGrid(CONST x, y: longint): T_point;
  begin
    result:=canvasToGrid(pointOf(x,y));
  end;

PROCEDURE T_uiAdapter.palletteConnected(CONST paintEvent: F_procedure;
  CONST gateHitInfo: F_gateHitInfo);
  begin
    callback.paintPalette:=paintEvent;
    callback.paletteItemAt:=gateHitInfo;
  end;

FUNCTION T_uiAdapter.paletteCanvas: TCanvas;
  begin
    result:=uiElement.boardImage.Canvas;
  end;

PROCEDURE T_uiAdapter.updateBoardScrollbars;
  VAR boardWidthInPixels,
      boardHeightInPixels,
      effBoardImageHeight,
      effBoardImageWidth:longint;
  begin
    //Board:
    boardWidthInPixels :=(activeBoard^.gridX1+5)*zoom;
    boardHeightInPixels:=(activeBoard^.gridY1+5)*zoom;
    effBoardImageHeight:=uiElement.boardImage.height-uiElement.boardHorizontalScrollBar.height-uiElement.paletteWidth;
    effBoardImageWidth :=uiElement.boardImage.width -uiElement.boardVerticalScrollBar  .width;

    if effBoardImageHeight>boardHeightInPixels then begin
      uiElement.boardVerticalScrollBar.visible:=false;
      uiElement.boardVerticalScrollBar.PageSize:=effBoardImageHeight;
      uiElement.boardVerticalScrollBar.max     :=0;
      uiElement.boardVerticalScrollBar.position:=0;
    end else begin
      uiElement.boardVerticalScrollBar.visible:=true;
      uiElement.boardVerticalScrollBar.max     :=boardHeightInPixels;
      uiElement.boardVerticalScrollBar.PageSize:=effBoardImageHeight;
    end;
    if effBoardImageWidth>boardWidthInPixels then begin
      uiElement.boardHorizontalScrollBar.visible:=false;
      uiElement.boardHorizontalScrollBar.max     :=0;
      uiElement.boardHorizontalScrollBar.PageSize:=effBoardImageWidth;
      uiElement.boardHorizontalScrollBar.position:=0;
    end else begin
      uiElement.boardHorizontalScrollBar.visible:=true;
      uiElement.boardHorizontalScrollBar.max     :=boardWidthInPixels;
      uiElement.boardHorizontalScrollBar.PageSize:=effBoardImageWidth;
    end;
  end;

PROCEDURE T_uiAdapter.setPaletteSize(CONST paletteWidthInGridSize, paletteHeightInGridSize: longint);
  VAR paletteWidthInPixels,
      paletteHeightInPixels:longint;

  begin
    if (uiElement.boardImage.picture.Bitmap.width <>uiElement.boardImage.width) or
       (uiElement.boardImage.picture.Bitmap.height<>uiElement.boardImage.height)
    then uiElement.boardImage.picture.Bitmap.setSize(uiElement.boardImage.width,uiElement.boardImage.height);

    //Palette:
    paletteWidthInPixels :=paletteWidthInGridSize *zoom;
    paletteHeightInPixels:=paletteHeightInGridSize*zoom;

    if uiElement.boardImage.height>paletteHeightInPixels then begin
      uiElement.paletteScrollBar.visible:=false;
      uiElement.paletteScrollBar.PageSize:=uiElement.boardImage.height;
      uiElement.paletteScrollBar.max     :=uiElement.boardImage.height;
      uiElement.paletteScrollBar.position:=0;
    end else begin
      if not uiElement.paletteScrollBar.visible
      then uiElement.paletteScrollBar.position:=0;
      uiElement.paletteScrollBar.visible:=true;
      uiElement.paletteScrollBar.Left:=0;
      uiElement.paletteScrollBar.PageSize:=uiElement.boardImage.height;
      uiElement.paletteScrollBar.max     :=paletteHeightInPixels;
    end;

    uiElement.paletteWidth         :=paletteWidthInPixels;
    uiElement.paletteComboBox.width:=paletteWidthInPixels-10;

    if activeBoard<>nil then updateBoardScrollbars;
  end;

FUNCTION T_uiAdapter.paletteYOffset: longint;
  begin
    if uiElement.paletteScrollBar.visible
    then result:=-uiElement.paletteScrollBar.position
    else result:=0;
  end;

FUNCTION T_uiAdapter.paletteWidth: longint;
  begin
    result:=uiElement.paletteWidth;
  end;

PROCEDURE T_uiAdapter.paletteIndexChanged;
  begin
    uiElement.paletteScrollBar.position:=0;
  end;

PROCEDURE T_uiAdapter.showIoEdit(CONST gate: P_visualGate);
  begin
    uiElement.ioEdit.visible:=true;
    uiElement.ioEdit.OnKeyPress:=@gate^.ioEditKeyPress;
    uiElement.ioEdit.OnEditingDone:=@gate^.ioEditEditingDone;
    uiElement.ioEdit.Left:=gate^.canvasPos[0]+uiElement.boardImage.Left;
    uiElement.ioEdit.width:=max(gate^.gridWidth*zoom,64);
    uiElement.ioEdit.top:=gate^.canvasPos[1]+gate^.getGridHeight*zoom+uiElement.boardImage.top-uiElement.ioEdit.height;
    uiElement.ioEdit.text:=getWireString(gate^.behavior^.getInput(0),gate^.ioMode);
    uiElement.ioEdit.SetFocus;
  end;

PROCEDURE T_uiAdapter.hideIoEdit;
  begin
    uiElement.ioEdit.OnKeyPress:=nil;
    uiElement.ioEdit.OnEditingDone:=nil;
    uiElement.ioEdit.visible:=false;
  end;

PROCEDURE T_uiAdapter.paintAll;
  begin
    with uiElement.boardImage do begin
      Canvas.Brush.color:=BOARD_COLOR;
      Canvas.Brush.style:=bsSolid;
      Canvas.Pen.style:=psClear;
      Canvas.FillRect(0,0,width,height);
    end;
    activeBoard^.paintBoard(state in [uas_draggingFromBoard,uas_multiDragFromBoard,uas_draggingFromPalette]);
  end;

PROCEDURE T_uiAdapter.repaintImage;
  begin
    uiElement.boardImage.repaint;
  end;

{$endif}

