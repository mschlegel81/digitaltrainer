UNIT paletteHandling;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils,StdCtrls, ExtCtrls,logicalGates,visualGates,compoundGates, myGenerics,serializationUtil;

TYPE
  P_palette=^T_palette;

  { T_palette }

  T_palette=object(T_abstractPrototypeSource)
    visualPaletteItems: array of P_visualGate;

    ui:record
      bgShape:TShape;
      combobox:TComboBox;
      paletteScroll:TScrollBar;
      uiAdapter:P_uiAdapter;
      lastSubPaletteIndex:longint;
    end;

    FUNCTION subPaletteNames:T_arrayOfString; virtual; abstract;
    PROCEDURE selectSubPalette(CONST index:longint); virtual; abstract;
    PROCEDURE ensureVisualPaletteItems; virtual; abstract;
    PROCEDURE checkSizes;
    PROCEDURE detachUI;
    PROCEDURE attachUI(CONST bgShape:TShape;
                       CONST combobox:TComboBox;
                       CONST paletteScroll:TScrollBar;
                       CONST uiAdapter:P_uiAdapter);

    PROCEDURE comboBoxSelect(Sender:TObject);
    PROCEDURE ScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; VAR ScrollPos: integer);
    FUNCTION allowDeletion(CONST gate:P_abstractGate):boolean;
  end;

  { T_workspacePalette }

  P_workspacePalette=^T_workspacePalette;
  T_workspacePalette=object(T_palette)
  private
    PROCEDURE reindex;
  public
    paletteNames:T_arrayOfString;
    paletteEntries:array of record
      subPaletteIndex:longint;
      entryType:T_gateType;
      prototype:P_visualBoard;
    end;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;

    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    PROCEDURE initDefaults;
    FUNCTION subPaletteNames:T_arrayOfString; virtual;
    PROCEDURE selectSubPalette(CONST index:longint); virtual;
    PROCEDURE ensureVisualPaletteItems; virtual;

    FUNCTION readGate(VAR stream:T_bufferedInputStreamWrapper):P_abstractGate; virtual;
    FUNCTION findEntry(CONST gate:P_abstractGate):longint;
    PROCEDURE reassignEntry(CONST gate:P_abstractGate; CONST newPalette:string);

    PROCEDURE addBoard   (CONST board:P_visualBoard; subPaletteIndex:longint; CONST subPaletteName:string);
    PROCEDURE updateEntry(CONST board:P_visualBoard; subPaletteIndex:longint; CONST subPaletteName:string);
    PROCEDURE deleteEntry(CONST index:longint);
  end;

  { T_challengePalette }
  P_challengePalette=^T_challengePalette;
  T_challengePalette=object(T_palette)
    paletteEntries:array of record
      visible:boolean;
      entryType:T_gateType;
      prototype:P_circuitBoard;
    end;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;

    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    FUNCTION subPaletteNames:T_arrayOfString; virtual;
    PROCEDURE selectSubPalette(CONST index:longint); virtual;
    PROCEDURE ensureVisualPaletteItems; virtual;

    FUNCTION readGate(VAR stream:T_bufferedInputStreamWrapper):P_abstractGate; virtual;
  end;

IMPLEMENTATION

{ T_challengePalette }

CONSTRUCTOR T_challengePalette.create;
  begin
    setLength(paletteEntries,0);
  end;

DESTRUCTOR T_challengePalette.destroy;
  VAR i:longint;
  begin
    for i:=length(paletteEntries)-1 downto 0 do
    with paletteEntries[i] do
    if prototype<>nil then dispose(prototype,destroy);
    setLength(paletteEntries,0);
  end;

FUNCTION T_challengePalette.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
  begin
    setLength(paletteEntries,stream.readNaturalNumber);
    for i:=0 to length(paletteEntries)-1 do with paletteEntries[i] do begin
      visible:=stream.readBoolean;
      entryType:=T_gateType(stream.readByte([byte(low(T_gateType))..byte(high(T_gateType))]));
      if entryType=gt_compound then begin
        new(prototype,create(@self));
        prototype^.readPrototypeFromStream(stream,i);
      end else prototype:=nil;
    end;
    result:=stream.allOkay;
  end;

PROCEDURE T_challengePalette.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i:longint;
  begin
    stream.writeNaturalNumber(length(paletteEntries));
    for i:=0 to length(paletteEntries)-1 do with paletteEntries[i] do begin
      stream.writeBoolean(visible);
      stream.writeByte(byte(entryType));
      if entryType=gt_compound then prototype^.writePrototypeToStream(stream,i);
    end;
  end;

FUNCTION T_challengePalette.subPaletteNames: T_arrayOfString;
  begin
    setLength(result,0);
  end;

PROCEDURE T_challengePalette.selectSubPalette(CONST index: longint);
  begin
    if ui.lastSubPaletteIndex<0 then begin
      ui.lastSubPaletteIndex:=0;
      ensureVisualPaletteItems;
      checkSizes;
    end;
  end;

PROCEDURE T_challengePalette.ensureVisualPaletteItems;
  VAR i,k:longint;
      behavior: P_abstractGate;
  begin
    for i:=0 to length(visualPaletteItems)-1 do dispose(visualPaletteItems[i],destroy);
    k:=0;
    for i:=0 to length(paletteEntries)-1 do if paletteEntries[i].visible then begin
      if paletteEntries[i].entryType=gt_compound
      then behavior:=paletteEntries[i].prototype^.clone(false)
      else behavior:=newBaseGate(paletteEntries[i].entryType);

      setLength(visualPaletteItems,k+1);
      new(visualPaletteItems[k],create(behavior));
      visualPaletteItems[k]^.uiAdapter:=ui.uiAdapter;
      visualPaletteItems[k]^.ensureGuiElements(ui.bgShape.parent);
      visualPaletteItems[k]^.paintAll(0,0,ui.uiAdapter^.getZoom);
      visualPaletteItems[k]^.setPaletteEntryMouseActions;
      inc(k);
    end;
  end;

FUNCTION T_challengePalette.readGate(VAR stream: T_bufferedInputStreamWrapper): P_abstractGate;
  VAR gateType:T_gateType;
      prototypeIndex:longint;
  begin
    gateType:=T_gateType(stream.readByte([byte(low(gateType))..byte(high(gateType))]));
    if gateType=gt_compound then begin
      prototypeIndex:=stream.readNaturalNumber;
      assert((prototypeIndex>=0) and (prototypeIndex<length(paletteEntries)),'Prototype index out of bounds');
      result:=paletteEntries[prototypeIndex].prototype^.clone(false);
    end else begin
      result:=newBaseGate(gateType);
      result^.readMetaDataFromStream(stream);
    end;
  end;

{ T_workspacePalette }

PROCEDURE T_workspacePalette.reindex;
  begin
    //TODO: Sort palette entries, so that no entry uses an entry with a higher index
    //TODO: Tell all the entries their respective index
  end;

CONSTRUCTOR T_workspacePalette.create;
  begin
    setLength(paletteEntries,0);
    setLength(paletteNames,0);
  end;

DESTRUCTOR T_workspacePalette.destroy;
  VAR i:longint;
  begin
    for i:=length(paletteEntries)-1 downto 0 do with paletteEntries[i] do if prototype<>nil then dispose(paletteEntries[i].prototype,destroy);
    setLength(paletteEntries,0);
    setLength(paletteNames,0);
  end;

FUNCTION T_workspacePalette.loadFromStream(
  VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
  begin
    setLength(paletteNames,stream.readNaturalNumber);
    for i:=0 to length(paletteNames)-1 do paletteNames[i]:=stream.readAnsiString;

    setLength(paletteEntries,stream.readNaturalNumber);
    for i:=0 to length(paletteEntries)-1 do with paletteEntries[i] do begin
      subPaletteIndex:=stream.readNaturalNumber;
      entryType:=T_gateType(stream.readByte([byte(low(T_gateType))..byte(high(T_gateType))]));
      if entryType=gt_compound then begin
        new(prototype,create(@self));
        prototype^.loadPaletteEntryFromStream(stream,i);
      end else prototype:=nil;
    end;
    result:=stream.allOkay;
  end;

PROCEDURE T_workspacePalette.saveToStream(
  VAR stream: T_bufferedOutputStreamWrapper);
  VAR i:longint;
  begin
    stream.writeNaturalNumber(length(paletteNames));
    for i:=0 to length(paletteNames)-1 do stream.writeAnsiString(paletteNames[i]);

    stream.writeNaturalNumber(length(paletteEntries));
    for i:=0 to length(paletteEntries)-1 do with paletteEntries[i] do begin
      stream.writeNaturalNumber(subPaletteIndex);
      stream.writeByte(byte(entryType));
      if entryType=gt_compound then prototype^.saveToStream(stream);
    end;
  end;

PROCEDURE T_workspacePalette.initDefaults;
  begin
    setLength(paletteNames,3);
    paletteNames[0]:='I/O';
    paletteNames[1]:='Basisgatter';
    paletteNames[2]:='Spezial';

    setLength(paletteEntries,16);
    with paletteEntries[ 0] do begin prototype:=nil; entryType:=gt_notGate;             subPaletteIndex:=1; end;
    with paletteEntries[ 1] do begin prototype:=nil; entryType:=gt_andGate;             subPaletteIndex:=1; end;
    with paletteEntries[ 2] do begin prototype:=nil; entryType:=gt_orGate;              subPaletteIndex:=1; end;
    with paletteEntries[ 3] do begin prototype:=nil; entryType:=gt_xorGate;             subPaletteIndex:=1; end;
    with paletteEntries[ 4] do begin prototype:=nil; entryType:=gt_nandGate;            subPaletteIndex:=1; end;
    with paletteEntries[ 5] do begin prototype:=nil; entryType:=gt_norGate;             subPaletteIndex:=1; end;
    with paletteEntries[ 6] do begin prototype:=nil; entryType:=gt_nxorGate;            subPaletteIndex:=1; end;
    with paletteEntries[ 7] do begin prototype:=nil; entryType:=gt_input;               subPaletteIndex:=0; end;
    with paletteEntries[ 8] do begin prototype:=nil; entryType:=gt_output;              subPaletteIndex:=0; end;
    with paletteEntries[ 9] do begin prototype:=nil; entryType:=gt_clock;               subPaletteIndex:=2; end;
    with paletteEntries[10] do begin prototype:=nil; entryType:=gt_gatedClock;          subPaletteIndex:=2; end;
    with paletteEntries[11] do begin prototype:=nil; entryType:=gt_adapter;             subPaletteIndex:=2; end;
    with paletteEntries[12] do begin prototype:=nil; entryType:=gt_true;                subPaletteIndex:=0; end;
    with paletteEntries[13] do begin prototype:=nil; entryType:=gt_false;               subPaletteIndex:=0; end;
    with paletteEntries[14] do begin prototype:=nil; entryType:=gt_undeterminedToTrue;  subPaletteIndex:=2; end;
    with paletteEntries[15] do begin prototype:=nil; entryType:=gt_undeterminedToFalse; subPaletteIndex:=2; end;
  end;

FUNCTION T_workspacePalette.subPaletteNames: T_arrayOfString;
  begin
    result:=paletteNames;
  end;

PROCEDURE T_workspacePalette.selectSubPalette(CONST index: longint);
  begin
    if index=ui.lastSubPaletteIndex then exit;
    if index<0 then ui.combobox.ItemIndex:=ui.lastSubPaletteIndex
    else begin
      ui.lastSubPaletteIndex:=index;
      ensureVisualPaletteItems;
      checkSizes;
    end;
  end;

PROCEDURE T_workspacePalette.ensureVisualPaletteItems;
  VAR i,k:longint;
      behavior: P_abstractGate;
  begin
    for i:=0 to length(visualPaletteItems)-1 do dispose(visualPaletteItems[i],destroy);
    k:=0;
    for i:=0 to length(paletteEntries)-1 do if paletteEntries[i].subPaletteIndex=ui.lastSubPaletteIndex then begin
      if paletteEntries[i].entryType=gt_compound
      then behavior:=paletteEntries[i].prototype^.extractBehavior
      else behavior:=newBaseGate(paletteEntries[i].entryType);

      setLength(visualPaletteItems,k+1);
      new(visualPaletteItems[k],create(behavior));
      visualPaletteItems[k]^.uiAdapter:=ui.uiAdapter;
      visualPaletteItems[k]^.ensureGuiElements(ui.bgShape.parent);
      visualPaletteItems[k]^.paintAll(0,0,ui.uiAdapter^.getZoom);
      visualPaletteItems[k]^.setPaletteEntryMouseActions;
      inc(k);
    end;
  end;

FUNCTION T_workspacePalette.readGate(VAR stream: T_bufferedInputStreamWrapper
  ): P_abstractGate;
  VAR gateType:T_gateType;
      prototypeIndex:longint;
  begin
    gateType:=T_gateType(stream.readByte([byte(low(gateType))..byte(high(gateType))]));
    if gateType=gt_compound then begin
      prototypeIndex:=stream.readNaturalNumber;
      assert((prototypeIndex>=0) and (prototypeIndex<length(paletteEntries)),'Prototype index out of bounds');
      result:=paletteEntries[prototypeIndex].prototype^.extractBehavior;
    end else begin
      result:=newBaseGate(gateType);
      result^.readMetaDataFromStream(stream);
    end;
  end;

FUNCTION T_workspacePalette.findEntry(CONST gate: P_abstractGate): longint;
  VAR i:longint;
  begin
    for i:=0 to length(paletteEntries)-1 do if paletteEntries[i].entryType=gate^.gateType then begin
      if (gate^.gateType=gt_compound) and (P_circuitBoard(gate)^.prototype=P_captionedAndIndexed(paletteEntries[i].prototype)) or
         (gate^.gateType<>gt_compound) then exit(i);
    end;
    result:=-1;
  end;

PROCEDURE T_workspacePalette.reassignEntry(CONST gate: P_abstractGate; CONST newPalette: string);
  VAR entryIndex:longint;
      paletteIndex:longint=0;
  begin
    entryIndex:=findEntry(gate);
    if entryIndex<0 then exit;
    while (paletteIndex<length(paletteNames)) and (paletteNames[paletteIndex]<>newPalette) do inc(paletteIndex);
    if paletteIndex>=length(paletteNames) then begin
      setLength(paletteNames,paletteIndex+1);
      paletteNames[paletteIndex]:=newPalette;
    end;
    //TODO: Remove empty subpalettes
    paletteEntries[entryIndex].subPaletteIndex:=paletteIndex;
  end;

PROCEDURE T_workspacePalette.addBoard(CONST board: P_visualBoard; subPaletteIndex: longint; CONST subPaletteName: string);
  VAR i:longint;
  begin
    if subPaletteIndex<0 then for i:=0 to length(paletteNames)-1 do if paletteNames[i]=subPaletteName then subPaletteIndex:=i;
    if subPaletteIndex<0 then begin
      subPaletteIndex:=length(paletteNames);
      setLength(paletteNames,subPaletteIndex+1);
      paletteNames[subPaletteIndex]:=subPaletteName;
    end;
    i:=length(paletteEntries);
    setLength(paletteEntries,i+1);
    paletteEntries[i].entryType:=gt_compound;
    paletteEntries[i].prototype:=board^.clone;
    paletteEntries[i].subPaletteIndex:=subPaletteIndex;
    reindex;
  end;

PROCEDURE T_workspacePalette.updateEntry(CONST board: P_visualBoard; subPaletteIndex: longint; CONST subPaletteName: string);
  VAR i:longint;
  begin
    if board^.getIndexInPalette<0 then exit;
    if subPaletteIndex<0 then for i:=0 to length(paletteNames)-1 do if paletteNames[i]=subPaletteName then subPaletteIndex:=i;
    if subPaletteIndex<0 then begin
      subPaletteIndex:=length(paletteNames);
      setLength(paletteNames,subPaletteIndex+1);
      paletteNames[subPaletteIndex]:=subPaletteName;
    end;
    i:=board^.getIndexInPalette;
    paletteEntries[i].entryType:=gt_compound;
    if  paletteEntries[i].prototype<>nil
    then dispose(paletteEntries[i].prototype,destroy);
    paletteEntries[i].prototype:=board^.clone;
    paletteEntries[i].subPaletteIndex:=subPaletteIndex;
    reindex;
  end;

PROCEDURE T_workspacePalette.deleteEntry(CONST index: longint);
  VAR j:longint;
  begin
    //Todo: ensure that this is never called while a palette entry is being edited!
    //Todo: ensure that no entry is deleted which is referenced by another entry!
    if (index>=0) and (index<length(paletteEntries)) then begin

    end;
    reindex;
  end;

{ T_palette }

PROCEDURE T_palette.checkSizes;
  VAR i:longint;
      totalPaletteHeightInCurrentZoom:longint=0;
      availableHeight:longint;
      width:longint;
      requiredWidth:longint=0;
      y0:longint;
  begin
    for i:=0 to length(visualPaletteItems)-1 do begin
      totalPaletteHeightInCurrentZoom+=visualPaletteItems[i]^.visualHeight;
      width:=visualPaletteItems[i]^.visualWidth;
      if width>requiredWidth then requiredWidth:=width;
    end;
    totalPaletteHeightInCurrentZoom+=length(visualPaletteItems)*ui.uiAdapter^.getZoom*2;
    requiredWidth                  +=                           ui.uiAdapter^.getZoom*4;
    if requiredWidth<120 then requiredWidth:=120;
    ui.bgShape.width:=requiredWidth;

    if ui.combobox.visible
    then y0:=ui.combobox.top+ui.combobox.height+ui.uiAdapter^.getZoom
    else y0:=                                   ui.uiAdapter^.getZoom;
    availableHeight:=ui.bgShape.height-y0;

    if availableHeight>totalPaletteHeightInCurrentZoom then begin
      ui.paletteScroll.position:=0;
      ui.paletteScroll.visible:=false;
    end else begin
      ui.paletteScroll.visible:=true;
      ui.paletteScroll.Left:=0;
      ui.paletteScroll.min:=0;
      ui.paletteScroll.max:=totalPaletteHeightInCurrentZoom-availableHeight;
    end;

    y0-=ui.paletteScroll.position;

    for i:=0 to length(visualPaletteItems)-1 do begin
      visualPaletteItems[i]^.paintAll(ui.uiAdapter^.getZoom*2,y0,ui.uiAdapter^.getZoom);
      y0+=ui.uiAdapter^.getZoom*2+visualPaletteItems[i]^.visualHeight;
    end;
    ui.uiAdapter^.paletteSizeUpdated(ui.bgShape.Left+ui.bgShape.width);

  end;

PROCEDURE T_palette.detachUI;
  VAR i:longint;
  begin
    for i:=0 to length(visualPaletteItems)-1 do dispose(visualPaletteItems[i],destroy);
  end;

PROCEDURE T_palette.attachUI(CONST bgShape: TShape; CONST combobox: TComboBox;
  CONST paletteScroll: TScrollBar; CONST uiAdapter: P_uiAdapter);
  VAR names: T_arrayOfString;
      s:string;
  begin
    ui.bgShape:=bgShape;
    ui.combobox:=combobox;
    ui.paletteScroll:=paletteScroll;
    ui.uiAdapter:=uiAdapter;
    names:=subPaletteNames;
    if length(names)=0 then begin
      combobox.visible:=false;
      ui.lastSubPaletteIndex:=-1;
    end else begin
      combobox.visible:=true;
      combobox.items.clear;
      for s in names do combobox.items.add(s);
      combobox.ItemIndex:=0;
      ui.lastSubPaletteIndex:=-1;
    end;
    selectSubPalette(0);

    ui.combobox.OnSelect:=@comboBoxSelect;
    ui.paletteScroll.OnScroll:=@ScrollbarScroll;
  end;

PROCEDURE T_palette.comboBoxSelect(Sender: TObject);
  begin
    selectSubPalette(ui.combobox.ItemIndex);
  end;

PROCEDURE T_palette.ScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; VAR ScrollPos: integer);
  begin
    checkSizes;
  end;

FUNCTION T_palette.allowDeletion(CONST gate: P_abstractGate): boolean;
  begin
    if gate^.gateType<>gt_compound then exit(false);
    //TODO...

  end;

end.

