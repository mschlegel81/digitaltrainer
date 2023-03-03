UNIT paletteHandling;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils,StdCtrls, ExtCtrls,logicalGates,visualGates,compoundGates, myGenerics,serializationUtil,wiringUtil;

TYPE
  P_palette=^T_palette;

  { T_palette }

  T_palette=object(T_abstractPrototypeSource)

    visualPaletteItems: array of P_visualGate;

    lastSubPaletteIndex:longint;
    ui:P_uiAdapter;

    PROCEDURE checkSizes;

    FUNCTION subPaletteNames:T_arrayOfString; virtual; abstract;
    PROCEDURE selectSubPalette(CONST index:longint); virtual; abstract;
    PROCEDURE ensureVisualPaletteItems; virtual; abstract;
    PROCEDURE detachUI;
    PROCEDURE attachUI(CONST uiAdapter:P_uiAdapter);
    FUNCTION isGateHit(CONST gridPos:T_point; OUT gate:P_visualGate):boolean;
    PROCEDURE comboBoxSelect(Sender:TObject);
    FUNCTION allowDeletion(CONST gate:P_abstractGate):boolean; virtual;
    PROCEDURE paint;
    PROCEDURE dropPaletteItem(CONST gatePtr:pointer); virtual;

    FUNCTION ensurePrototype(CONST prototypeIndex:longint):boolean; virtual;
    PROCEDURE addPrototype(CONST prototypeIndex:longint; CONST behavior:P_compoundGate; CONST visible:boolean); virtual;
    PROCEDURE ensureBaseGate(CONST gate:P_abstractGate); virtual;
    PROCEDURE countUpGate(CONST gate:P_abstractGate); virtual;

    PROCEDURE gateDeleted(CONST gate:P_visualGate); virtual;
  end;

  { T_workspacePalette }

  T_workspacePaletteEntry= record
    visualSorting,
    subPaletteIndex:longint;
    entryType:T_gateType;
    prototype:P_visualBoard;
  end;

  P_workspacePalette=^T_workspacePalette;
  T_workspacePalette=object(T_palette)
  private
    PROCEDURE removeSubPalette(CONST index:longint);
    PROCEDURE reindex;
  public
    filter:longint;
    paletteNames:T_arrayOfString;
    paletteEntries:array of T_workspacePaletteEntry;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;

    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    PROCEDURE initDefaults;
    FUNCTION subPaletteNames:T_arrayOfString; virtual;
    PROCEDURE selectSubPalette(CONST index:longint); virtual;
    PROCEDURE ensureVisualPaletteItems; virtual;

    FUNCTION readGate(VAR stream:T_bufferedInputStreamWrapper):P_abstractGate; virtual;
    FUNCTION obtainGate(CONST prototypeIndex:longint):P_compoundGate; virtual;
    FUNCTION findEntry(CONST gate:P_abstractGate):longint;
    PROCEDURE reassignEntry(CONST gate:P_abstractGate; CONST newPalette:string);

    PROCEDURE addBoard   (CONST board:P_visualBoard; subPaletteIndex:longint; CONST subPaletteName:string);
    PROCEDURE updateEntry(CONST board:P_visualBoard; subPaletteIndex:longint; CONST subPaletteName:string);
    PROCEDURE deleteEntry(CONST prototype:P_captionedAndIndexed);
    FUNCTION  allowDeletion(CONST gate:P_abstractGate):boolean; virtual;
    PROCEDURE setFilter(CONST newValue:longint);
    PROCEDURE ensureIndexes;

    PROCEDURE dropPaletteItem(CONST gatePtr:pointer); virtual;
  end;

  { T_challengePalette }
  P_challengePalette=^T_challengePalette;
  T_challengePalette=object(T_palette)
    paletteEntries:array of record
      visualSorting,
      visible:boolean;
      entryType:T_gateType;
      prototype:P_compoundGate;
      availableCount:longint;
    end;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;

    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    FUNCTION subPaletteNames:T_arrayOfString; virtual;
    PROCEDURE selectSubPalette(CONST index:longint); virtual;
    PROCEDURE ensureVisualPaletteItems; virtual;

    FUNCTION readGate(VAR stream:T_bufferedInputStreamWrapper):P_abstractGate; virtual;
    FUNCTION obtainGate(CONST prototypeIndex:longint):P_compoundGate; virtual;

    FUNCTION ensurePrototype(CONST prototypeIndex:longint):boolean; virtual;
    PROCEDURE addPrototype(CONST prototypeIndex:longint; CONST behavior:P_compoundGate; CONST visible:boolean); virtual;
    PROCEDURE ensureBaseGate(CONST gate:P_abstractGate); virtual;
    PROCEDURE countUpGate(CONST gate:P_abstractGate); virtual;

    PROCEDURE gateDeleted(CONST gate:P_visualGate); virtual;
  end;

IMPLEMENTATION
USES visuals,Graphics;

{ T_challengePalette }

CONSTRUCTOR T_challengePalette.create;
  begin
    setLength(paletteEntries,0);
  end;

DESTRUCTOR T_challengePalette.destroy;
  VAR i:longint;
  begin
    detachUI;
    for i:=length(paletteEntries)-1 downto 0 do
    with paletteEntries[i] do
    if prototype<>nil then dispose(prototype,destroy);
    setLength(paletteEntries,0);
  end;

FUNCTION T_challengePalette.loadFromStream(
  VAR stream: T_bufferedInputStreamWrapper): boolean;
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

PROCEDURE T_challengePalette.saveToStream(
  VAR stream: T_bufferedOutputStreamWrapper);
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
    if lastSubPaletteIndex<0 then begin
      lastSubPaletteIndex:=0;
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
      visualPaletteItems[k]^.uiAdapter:=ui;
      inc(k);
    end;
  end;

FUNCTION T_challengePalette.readGate(VAR stream: T_bufferedInputStreamWrapper
  ): P_abstractGate;
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

FUNCTION T_challengePalette.obtainGate(CONST prototypeIndex: longint
  ): P_compoundGate;
  begin
    assert((prototypeIndex>=0) and (prototypeIndex<length(paletteEntries)),'Prototype index out of bounds');
    result:=P_compoundGate(paletteEntries[prototypeIndex].prototype^.clone(false));
  end;

FUNCTION T_challengePalette.ensurePrototype(CONST prototypeIndex: longint): boolean;
  begin
    //TODO: Implement T_challengePalette.ensurePrototype
  end;

PROCEDURE T_challengePalette.addPrototype(CONST prototypeIndex: longint; CONST behavior: P_compoundGate; CONST visible: boolean);
  begin
    //TODO: Implement T_challengePalette.addPrototype
  end;

PROCEDURE T_challengePalette.ensureBaseGate(CONST gate: P_abstractGate);
  begin
    //TODO: Implement T_challengePalette.ensureBaseGate();
  end;

PROCEDURE T_challengePalette.countUpGate(CONST gate: P_abstractGate);
  begin
    //TODO: Implement T_challengePalette.countUpGate
  end;

PROCEDURE T_challengePalette.gateDeleted(CONST gate: P_visualGate);
  begin
    //TODO: Implement T_challengePalette.gateDeleted
  end;

{ T_workspacePalette }

PROCEDURE T_workspacePalette.removeSubPalette(CONST index: longint);
  VAR i:longint;
  begin
    //if so, decrease all subPaletteIndexes>previousPaletteIndex ...
    for i:=0 to length(paletteEntries)-1 do
      if paletteEntries[i].subPaletteIndex>index
      then dec(paletteEntries[i].subPaletteIndex);

    //... and remove palette name
    for i:=index to length(paletteNames)-2 do
      paletteNames[i]:=paletteNames[i+1];
    setLength(paletteNames,length(paletteNames)-1);
  end;

PROCEDURE T_workspacePalette.reindex;
  VAR paletteNameUsed:array of boolean;
      i,j: integer;
      tmp:T_workspacePaletteEntry;
      anySwapped:boolean=true;
  begin
    setLength(paletteNameUsed,length(paletteNames));
    for i:=0 to length(paletteNameUsed)-1 do  paletteNameUsed[i]:=false;
    for i:=0 to length(paletteEntries)-1 do paletteNameUsed[paletteEntries[i].subPaletteIndex]:=true;
    for i:=length(paletteNameUsed)-1 downto 0 do if not(paletteNameUsed[i]) then removeSubPalette(i);

    repeat
      anySwapped:=false;
      for i:=0 to length(paletteEntries)-2 do
      if paletteEntries[i].prototype<>nil then begin
        j:=i+1;
        while (j<length(paletteEntries)) and ((paletteEntries[j].prototype=nil) or not(paletteEntries[i].prototype^.usesPrototype(paletteEntries[j].prototype))) do inc(j);
        if j<length(paletteEntries) then begin
          anySwapped:=true;
          tmp:=paletteEntries[i];
          paletteEntries[i]:=paletteEntries[j];
          paletteEntries[j]:=tmp;
        end;
      end;
    until not(anySwapped);

    for i:=0 to length(paletteEntries)-1 do if paletteEntries[i].prototype<>nil then paletteEntries[i].prototype^.setIndexInPalette(i);
  end;

CONSTRUCTOR T_workspacePalette.create;
  begin
    setLength(paletteEntries,0);
    setLength(paletteNames,0);
    filter:=maxLongint;
  end;

DESTRUCTOR T_workspacePalette.destroy;
  VAR i:longint;
  begin
    detachUI;
    for i:=length(paletteEntries)-1 downto 0 do with paletteEntries[i] do if prototype<>nil then dispose(paletteEntries[i].prototype,destroy);
    setLength(paletteEntries,0);
    setLength(paletteNames,0);
  end;

FUNCTION T_workspacePalette.loadFromStream(
  VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
  begin
    filter:=stream.readLongint;
    setLength(paletteNames,stream.readNaturalNumber);
    for i:=0 to length(paletteNames)-1 do paletteNames[i]:=stream.readAnsiString;

    setLength(paletteEntries,stream.readNaturalNumber);
    for i:=0 to length(paletteEntries)-1 do with paletteEntries[i] do begin
      visualSorting:=stream.readNaturalNumber;
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
    stream.writeLongint(filter);
    stream.writeNaturalNumber(length(paletteNames));
    for i:=0 to length(paletteNames)-1 do stream.writeAnsiString(paletteNames[i]);

    stream.writeNaturalNumber(length(paletteEntries));
    for i:=0 to length(paletteEntries)-1 do with paletteEntries[i] do begin
      stream.writeNaturalNumber(visualSorting);
      stream.writeNaturalNumber(subPaletteIndex);
      stream.writeByte(byte(entryType));
      if entryType=gt_compound then prototype^.savePaletteEntryToStream(stream,i);
    end;
  end;

PROCEDURE T_workspacePalette.initDefaults;
  begin
    setLength(paletteNames,3);
    paletteNames[0]:='I/O';
    paletteNames[1]:='Basisgatter';
    paletteNames[2]:='Spezial';
    filter:=maxLongint;

    setLength(paletteEntries,19);
    with paletteEntries[ 0] do begin prototype:=nil; entryType:=gt_notGate;             subPaletteIndex:=1; visualSorting:=0; end;
    with paletteEntries[ 1] do begin prototype:=nil; entryType:=gt_andGate;             subPaletteIndex:=1; visualSorting:=0; end;
    with paletteEntries[ 2] do begin prototype:=nil; entryType:=gt_orGate;              subPaletteIndex:=1; visualSorting:=0; end;
    with paletteEntries[ 3] do begin prototype:=nil; entryType:=gt_xorGate;             subPaletteIndex:=1; visualSorting:=0; end;
    with paletteEntries[ 4] do begin prototype:=nil; entryType:=gt_nandGate;            subPaletteIndex:=1; visualSorting:=0; end;
    with paletteEntries[ 5] do begin prototype:=nil; entryType:=gt_norGate;             subPaletteIndex:=1; visualSorting:=0; end;
    with paletteEntries[ 6] do begin prototype:=nil; entryType:=gt_nxorGate;            subPaletteIndex:=1; visualSorting:=0; end;
    with paletteEntries[ 7] do begin prototype:=nil; entryType:=gt_input;               subPaletteIndex:=0; visualSorting:=0; end;
    with paletteEntries[ 8] do begin prototype:=nil; entryType:=gt_output;              subPaletteIndex:=0; visualSorting:=0; end;
    with paletteEntries[ 9] do begin prototype:=nil; entryType:=gt_clock;               subPaletteIndex:=2; visualSorting:=0; end;
    with paletteEntries[10] do begin prototype:=nil; entryType:=gt_gatedClock;          subPaletteIndex:=2; visualSorting:=0; end;
    with paletteEntries[11] do begin prototype:=nil; entryType:=gt_adapter;             subPaletteIndex:=0; visualSorting:=0; end;
    with paletteEntries[12] do begin prototype:=nil; entryType:=gt_true;                subPaletteIndex:=0; visualSorting:=0; end;
    with paletteEntries[13] do begin prototype:=nil; entryType:=gt_false;               subPaletteIndex:=0; visualSorting:=0; end;
    with paletteEntries[14] do begin prototype:=nil; entryType:=gt_undeterminedToTrue;  subPaletteIndex:=2; visualSorting:=0; end;
    with paletteEntries[15] do begin prototype:=nil; entryType:=gt_undeterminedToFalse; subPaletteIndex:=2; visualSorting:=0; end;
    with paletteEntries[16] do begin prototype:=nil; entryType:=gt_rom;                 subPaletteIndex:=2; visualSorting:=0; end;
    with paletteEntries[17] do begin prototype:=nil; entryType:=gt_ram;                 subPaletteIndex:=2; visualSorting:=0; end;
    with paletteEntries[18] do begin prototype:=nil; entryType:=gt_7segmentDummy;       subPaletteIndex:=2; visualSorting:=0; end;
  end;

FUNCTION T_workspacePalette.subPaletteNames: T_arrayOfString;
  begin
    result:=paletteNames;
  end;

PROCEDURE T_workspacePalette.selectSubPalette(CONST index: longint);
  begin
    if index=lastSubPaletteIndex then exit;
    if index<0 then ui^.paletteComboBox.ItemIndex:=lastSubPaletteIndex
    else begin
      lastSubPaletteIndex:=index;
      ensureVisualPaletteItems;
      checkSizes;
    end;
  end;

PROCEDURE T_workspacePalette.ensureVisualPaletteItems;
  FUNCTION excludedByFilter(CONST paletteIndex:longint):boolean;
    begin
      if (filter<0) or
         (filter>=length(paletteEntries)) or
         (paletteEntries[filter].prototype=nil) or
         (paletteEntries[paletteIndex].prototype=nil)
      then exit(false);
      result:=(filter=paletteIndex) or
              paletteEntries[paletteIndex].prototype^.usesPrototype(paletteEntries[filter].prototype);
    end;
  TYPE T_item=record
        entryIndex,visualSorting:longint;
        visualItem:P_visualGate;
      end;

  VAR items:array of T_item;
      tmp:T_item;

      i,j,k:longint;
      behavior: P_abstractGate;

  begin
    for i:=0 to length(visualPaletteItems)-1 do dispose(visualPaletteItems[i],destroy);
    setLength(items,0);
    k:=0;
    for i:=0 to length(paletteEntries)-1 do
    if (paletteEntries[i].subPaletteIndex=lastSubPaletteIndex) and not(excludedByFilter(i)) then begin
      if paletteEntries[i].entryType=gt_compound
      then behavior:=paletteEntries[i].prototype^.extractBehavior
      else behavior:=newBaseGate(paletteEntries[i].entryType);
      setLength(items,k+1);
      new(items[k].visualItem,create(behavior));
      items[k].visualItem^.uiAdapter:=ui;
      items[k].entryIndex:=i;
      items[k].visualSorting:=paletteEntries[i].visualSorting;
      for j:=0 to k-1 do if items[j].visualSorting>items[k].visualSorting then begin
        tmp     :=items[j];
        items[j]:=items[k];
        items[k]:=tmp;
      end;
      inc(k);
    end;

    //items are already sorted; now ensure that visualSorting are distinct numbers
    if length(items)>0 then begin
      j:=items[0].visualSorting;
      for k:=1 to length(items)-1 do begin
        if items[k].visualSorting<=j then begin
          items[k]                           .visualSorting:=j+1;
          paletteEntries[items[k].entryIndex].visualSorting:=j+1;
        end;
        j:=items[k].visualSorting;
      end;
    end;

    setLength(visualPaletteItems,length(items));
    for k:=0 to length(visualPaletteItems)-1 do visualPaletteItems[k]:=items[k].visualItem;

    setLength(items,0);
  end;

FUNCTION T_workspacePalette.readGate(VAR stream: T_bufferedInputStreamWrapper
  ): P_abstractGate;
  VAR gateType:T_gateType;
      prototypeIndex:longint;
  begin
    gateType:=T_gateType(stream.readByte([byte(low(gateType))..byte(high(gateType))]));
    if gateType=gt_compound then begin
      prototypeIndex:=stream.readLongint;
      assert((prototypeIndex>=0) and (prototypeIndex<length(paletteEntries)),'Prototype index out of bounds');
      result:=paletteEntries[prototypeIndex].prototype^.extractBehavior;
    end else begin
      result:=newBaseGate(gateType);
      result^.readMetaDataFromStream(stream);
    end;
  end;

FUNCTION T_workspacePalette.obtainGate(CONST prototypeIndex: longint
  ): P_compoundGate;
  begin
    assert((prototypeIndex>=0) and (prototypeIndex<length(paletteEntries)),'Prototype index out of bounds');
    result:=paletteEntries[prototypeIndex].prototype^.extractBehavior;
  end;

FUNCTION T_workspacePalette.findEntry(CONST gate: P_abstractGate): longint;
  VAR i:longint;
  begin
    for i:=0 to length(paletteEntries)-1 do if paletteEntries[i].entryType=gate^.gateType then begin
      if (gate^.gateType=gt_compound) and (P_compoundGate(gate)^.prototype=P_captionedAndIndexed(paletteEntries[i].prototype)) or
         (gate^.gateType<>gt_compound) then exit(i);
    end;
    result:=-1;
  end;

PROCEDURE T_workspacePalette.reassignEntry(CONST gate: P_abstractGate;
  CONST newPalette: string);
  VAR entryIndex:longint;
      paletteIndex:longint=0;
      previousPaletteIndex:longint;
  begin
    entryIndex:=findEntry(gate);
    if entryIndex<0 then exit;
    while (paletteIndex<length(paletteNames)) and (paletteNames[paletteIndex]<>newPalette) do inc(paletteIndex);
    if paletteIndex>=length(paletteNames) then begin
      setLength(paletteNames,paletteIndex+1);
      paletteNames[paletteIndex]:=newPalette;
    end;
    previousPaletteIndex:=paletteEntries[entryIndex].subPaletteIndex;
    paletteEntries[entryIndex].subPaletteIndex:=paletteIndex;

    //previous palette may not contain any entries anymore...
    for entryIndex:=0 to length(paletteEntries)-1 do
    if paletteEntries[entryIndex].subPaletteIndex=previousPaletteIndex then exit;
    removeSubPalette(previousPaletteIndex);
  end;

PROCEDURE T_workspacePalette.addBoard(CONST board: P_visualBoard;
  subPaletteIndex: longint; CONST subPaletteName: string);
  VAR i:longint;
      visualIndex:longint=0;
  begin
    if subPaletteIndex<0 then for i:=0 to length(paletteNames)-1 do if paletteNames[i]=subPaletteName then subPaletteIndex:=i;
    if subPaletteIndex<0 then begin
      subPaletteIndex:=length(paletteNames);
      setLength(paletteNames,subPaletteIndex+1);
      paletteNames[subPaletteIndex]:=subPaletteName;
    end else begin
      for i:=0 to length(paletteEntries)-1 do
      if (paletteEntries[i].subPaletteIndex=subPaletteIndex) and
          (visualIndex< 1+paletteEntries[i].visualSorting)
      then visualIndex:=1+paletteEntries[i].visualSorting;
    end;
    i:=length(paletteEntries);
    setLength(paletteEntries,i+1);

    paletteEntries[i].entryType      :=gt_compound;
    paletteEntries[i].prototype      :=board^.clone;
    paletteEntries[i].subPaletteIndex:=subPaletteIndex;
    paletteEntries[i].visualSorting  :=visualIndex;
    reindex;
    filter:=-1;
  end;

PROCEDURE T_workspacePalette.updateEntry(CONST board: P_visualBoard;
  subPaletteIndex: longint; CONST subPaletteName: string);
  VAR i,j:longint;
      clonedBoard: P_visualBoard;
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
    clonedBoard:=board^.clone;

    //Update prototype everywhere
    ui^.prototypeUpdated(paletteEntries[i].prototype,clonedBoard);
    for j:=0 to length(paletteEntries)-1 do
    if (j<>i) and (paletteEntries[j].prototype<>nil)
    then paletteEntries[j].prototype^.prototypeUpdated(paletteEntries[i].prototype,clonedBoard);

    if  paletteEntries[i].prototype<>nil
    then dispose(paletteEntries[i].prototype,destroy);
    paletteEntries[i].prototype:=clonedBoard;
    paletteEntries[i].subPaletteIndex:=subPaletteIndex;

    reindex;
    filter:=-1;
    ensureVisualPaletteItems;
  end;

PROCEDURE T_workspacePalette.deleteEntry(CONST prototype: P_captionedAndIndexed
  );
  VAR i,i0:longint;
  begin
    if ui^.isPrototypeInUse(prototype) then exit;
    i0:=prototype^.getIndexInPalette;
    for i:=0 to length(paletteEntries)-1 do
      if (i<>i0) and
         (paletteEntries[i].prototype<>nil) and
         (paletteEntries[i].prototype^.usesPrototype(prototype)) then exit;

    dispose(paletteEntries[i0].prototype,destroy);
    for i:=i0 to length(paletteEntries)-2 do paletteEntries[i]:=paletteEntries[i+1];
    setLength(paletteEntries,length(paletteEntries)-1);
    reindex;
    ensureVisualPaletteItems;
    checkSizes;
    ui^.paintAll;
  end;

FUNCTION T_workspacePalette.allowDeletion(CONST gate: P_abstractGate): boolean;
  VAR i:longint;
      prototype: P_visualBoard;
  begin
    if gate^.gateType<>gt_compound then exit(false);
    if P_compoundGate(gate)^.prototype=nil then exit(false);
    if not(P_compoundGate(gate)^.prototype^.isVisualBoard) then exit(false);
    prototype:=P_visualBoard(P_compoundGate(gate)^.prototype);

    if ui^.isPrototypeInUse(prototype) then exit(false);
    for i:=0 to length(paletteEntries)-1 do
      if (i<>prototype^.getIndexInPalette) and
         (paletteEntries[i].prototype<>nil) and
         (paletteEntries[i].prototype^.usesPrototype(prototype)) then exit(false);
    result:=true;
  end;

PROCEDURE T_workspacePalette.setFilter(CONST newValue: longint);
  VAR changed:boolean;
  begin
    if (newValue>=0) and (newValue<length(paletteEntries)) and (paletteEntries[newValue].prototype=nil) then exit;
    changed:=filter<>newValue;
    filter:=newValue;
    if changed then begin
      ensureVisualPaletteItems;
      checkSizes;
    end;
  end;

PROCEDURE T_workspacePalette.ensureIndexes;
  VAR i:longint;
  begin
    for i:=0 to length(paletteEntries)-1 do
      if paletteEntries[i].prototype<>nil
      then paletteEntries[i].prototype^.setIndexInPalette(i);
  end;

PROCEDURE T_workspacePalette.dropPaletteItem(CONST gatePtr: pointer);
  TYPE T_item=record
        index:longint;
        visualSorting:longint;
        visual:P_visualGate;
      end;
  VAR movedIdx:longint=-1;
      k, canvasY:longint;
      items:array of T_item;
      temp:T_item;
      gate:P_visualGate;
  begin

    if length(visualPaletteItems)<=1 then exit; //nothing to do here

    gate:=P_visualGate(gatePtr);
    canvasY:=gate^.canvasPos[1];
    setLength(items,length(visualPaletteItems));
    for k:=0 to length(items)-1 do begin
      items[k].index:=findEntry(visualPaletteItems[k]^.getBehavior);
      assert(items[k].index>=0);
      items[k].visualSorting:=paletteEntries[items[k].index].visualSorting;
      items[k].visual:=visualPaletteItems[k];
      if visualPaletteItems[k]^.getBehavior^.equals(gate^.getBehavior) then movedIdx:=k;
    end;
    if movedIdx<0 then exit;

    if canvasY<items[movedIdx].visual^.canvasPos[1]
    then while (movedIdx>0) and (items[movedIdx-1].visual^.canvasPos[1]>canvasY) do begin
      temp             .index :=items[movedIdx-1].index;
      items[movedIdx-1].index :=items[movedIdx  ].index;
      items[movedIdx  ].index :=temp             .index;
      temp             .visual:=items[movedIdx-1].visual;
      items[movedIdx-1].visual:=items[movedIdx  ].visual;
      items[movedIdx  ].visual:=temp             .visual;
      dec(movedIdx);
    end else while (movedIdx<length(visualPaletteItems)-1) and (items[movedIdx+1].visual^.canvasPos[1]<canvasY) do begin
      temp             .index :=items[movedIdx+1].index;
      items[movedIdx+1].index :=items[movedIdx  ].index;
      items[movedIdx  ].index :=temp             .index;
      temp             .visual:=items[movedIdx+1].visual;
      items[movedIdx+1].visual:=items[movedIdx  ].visual;
      items[movedIdx  ].visual:=temp             .visual;
      inc(movedIdx);
    end;
    for k:=0 to length(items)-1 do paletteEntries[items[k].index].visualSorting:=items[k].visualSorting;
    ensureVisualPaletteItems;
    checkSizes;
    paint;
  end;

{ T_palette }

PROCEDURE T_palette.checkSizes;
  VAR i:longint;
      requiredHeight:longint=1;
      width:longint;
      requiredWidth:longint=0;
  begin
    for i:=0 to length(visualPaletteItems)-1 do begin
      visualPaletteItems[i]^.gridPos[0]:=1;
      visualPaletteItems[i]^.gridPos[1]:=requiredHeight;
      requiredHeight+=visualPaletteItems[i]^.getGridHeight+2;
      width:=visualPaletteItems[i]^.getGridWidth;
      if width>requiredWidth then requiredWidth:=width;
    end;
    requiredHeight+=1;
    requiredWidth +=4;
    ui^.setPaletteSize(requiredWidth,requiredHeight);
  end;

PROCEDURE T_palette.detachUI;
  VAR i:longint;
  begin
    for i:=0 to length(visualPaletteItems)-1 do dispose(visualPaletteItems[i],destroy);
    setLength(visualPaletteItems,0);
  end;

PROCEDURE T_palette.attachUI(CONST uiAdapter: P_uiAdapter);
  VAR names: T_arrayOfString;
      s:string;
  begin
    ui:=uiAdapter;
    names:=subPaletteNames;
    if length(names)=0 then begin
      ui^.paletteComboBox.visible:=false;
      lastSubPaletteIndex:=-1;
    end else begin
      ui^.paletteComboBox.visible:=true;
      ui^.paletteComboBox.items.clear;
      for s in names do ui^.paletteComboBox.items.add(s);
      ui^.paletteComboBox.ItemIndex:=0;
      lastSubPaletteIndex:=-1;
    end;
    selectSubPalette(0);

    ui^.paletteComboBox.OnSelect:=@comboBoxSelect;
    ui^.palletteConnected(@paint,@isGateHit);
  end;

FUNCTION T_palette.isGateHit(CONST gridPos: T_point; OUT gate: P_visualGate
  ): boolean;
  VAR g:P_visualGate;
     hoverInfo: T_hoverInfo;
  begin
    for g in visualPaletteItems do if g^.isAtGridPos(gridPos,hoverInfo) then begin gate:=g; exit(true); end;
    gate:=nil;
    result:=false;
  end;

PROCEDURE T_palette.comboBoxSelect(Sender: TObject);
  begin
    selectSubPalette(ui^.paletteComboBox.ItemIndex);
    ui^.paintAll;
    ui^.repaintImage;
  end;

FUNCTION T_palette.allowDeletion(CONST gate: P_abstractGate): boolean;
  begin
    result:=false;
  end;

PROCEDURE T_palette.paint;
  VAR g:P_visualGate;
      yOffset:longint;
  begin
    ui^.paletteCanvas.Brush.color:=BOARD_COLOR;
    ui^.paletteCanvas.Brush.style:=bsSolid;
    ui^.paletteCanvas.Pen.color:=clBlack;
    ui^.paletteCanvas.Pen.style:=psSolid;
    ui^.paletteCanvas.Rectangle(-1,-1,ui^.paletteWidth,2000);

    yOffset:=ui^.paletteYOffset;
    for g in visualPaletteItems do begin
      g^.canvasPos:=pointOf(g^.gridPos[0]*ui^.getZoom,
                            g^.gridPos[1]*ui^.getZoom+yOffset);
      g^.paintAll(ui^.paletteCanvas,ui^.getZoom);
    end;
  end;

PROCEDURE T_palette.dropPaletteItem(CONST gatePtr: pointer);
  begin end; //dummy

FUNCTION T_palette.ensurePrototype(CONST prototypeIndex: longint): boolean;
  begin end; //dummy

PROCEDURE T_palette.addPrototype(CONST prototypeIndex: longint; CONST behavior: P_compoundGate; CONST visible: boolean);
  begin end; //dummy

PROCEDURE T_palette.ensureBaseGate(CONST gate: P_abstractGate);
  begin end; //dummy

PROCEDURE T_palette.countUpGate(CONST gate: P_abstractGate);
  begin end; //dummy

PROCEDURE T_palette.gateDeleted(CONST gate: P_visualGate);
  begin end; //dummy

end.

