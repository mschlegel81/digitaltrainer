UNIT paletteHandling;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils,StdCtrls,logicalGates,visualGates,compoundGates, myGenerics,serializationUtil,wiringUtil;

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
    FUNCTION getGateAtScreenPosition(CONST x,y:longint):pointer; virtual;
    FUNCTION isGateHit(CONST gridPos:T_point; OUT gate:P_visualGate):boolean;
    PROCEDURE comboBoxSelect(Sender:TObject);
    FUNCTION allowDeletion(CONST gate:P_abstractGate; OUT reasonForFalse:string):boolean; virtual;
    PROCEDURE paint;
    PROCEDURE dropPaletteItem(CONST gatePtr:pointer); virtual;
    FUNCTION findEntry(CONST gate:P_abstractGate):longint; virtual; abstract;

    FUNCTION hasPrototype(CONST prototypeIndex:longint):boolean; virtual;
    PROCEDURE addPrototype(CONST prototypeIndex:longint; CONST behavior:P_compoundGate; CONST visible:boolean); virtual;
    PROCEDURE ensureBaseGate(CONST gate:P_abstractGate; CONST visible:boolean); virtual;
    PROCEDURE countUpGate(CONST gate:P_abstractGate); virtual;
    PROCEDURE countDownGate(CONST gate:P_abstractGate); virtual;
  end;

  { T_workspacePalette }

  T_workspacePaletteEntry= record
    markedForExport:boolean;
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
    FUNCTION findEntry(CONST gate:P_abstractGate):longint; virtual;
    PROCEDURE reassignEntry(CONST gate:P_abstractGate; CONST newPalette:string);

    FUNCTION  addBoard   (CONST board:P_visualBoard; subPaletteIndex:longint; CONST subPaletteName:string):P_visualBoard;
    PROCEDURE updateEntry(CONST board:P_visualBoard; subPaletteIndex:longint; CONST subPaletteName:string);
    PROCEDURE updateEntry(CONST board:P_visualBoard);
    PROCEDURE deleteEntry(CONST prototype:P_captionedAndIndexed);
    FUNCTION  allowDeletion(CONST gate:P_abstractGate; OUT reasonForFalse:string):boolean; virtual;
    PROCEDURE deleteEntry(CONST index:longint);
    FUNCTION  allowDeletion(CONST index:longint):boolean;
    PROCEDURE setFilter(CONST newValue:longint);
    PROCEDURE ensureIndexes;

    PROCEDURE dropPaletteItem(CONST gatePtr:pointer); virtual;
    FUNCTION isWorkspacePalette:boolean; virtual;

    FUNCTION setPaletteEntryCaption    (CONST index:longint; CONST value:string):boolean;
    FUNCTION setPaletteEntryDescription(CONST index:longint; CONST value:string):boolean;
    PROCEDURE setPaletteEntrySubPalette (CONST index:longint; CONST value:string);
    PROCEDURE markAllEntriesForExport(CONST Selected:boolean);
    PROCEDURE markEntryForExportToggle(CONST index:longint);
    PROCEDURE markEntryForExport(CONST index:longint; CONST Selected:boolean);
    PROCEDURE exportSelected(CONST fileName:string);
    PROCEDURE importPalette(CONST fileName:string);
    PROCEDURE swapPaletteName(CONST index:longint; CONST up:boolean);
    PROCEDURE removeDuplicates(CONST byBehavior:boolean);
  end;

  T_challengePaletteEntry=record
      visible,preconfigured:boolean;
      initialAvailableCount,
      currentAvailableCount:longint;
      entryType:T_gateType;
      prototype:P_abstractGate;
      sourcePaletteIndex:longint; //not persisted; only relevant during construction
    end;

  { T_challengePalette }
  P_challengePalette=^T_challengePalette;
  T_challengePalette=object(T_palette)
    private
      FUNCTION IndexOf(CONST gate:P_abstractGate):longint;
    public
    paletteEntries:array of T_challengePaletteEntry;
    allowConfiguration:boolean;
    constructingChallenge:boolean;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;

    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    FUNCTION subPaletteNames:T_arrayOfString; virtual;
    PROCEDURE selectSubPalette(CONST index:longint); virtual;
    PROCEDURE ensureVisualPaletteItems; virtual;
    PROCEDURE resetCounts;

    FUNCTION readGate(VAR stream:T_bufferedInputStreamWrapper):P_abstractGate; virtual;
    FUNCTION obtainGate(CONST prototypeIndex:longint):P_compoundGate; virtual;

    FUNCTION hasPrototype(CONST prototypeIndex:longint):boolean; virtual;
    PROCEDURE addPrototype(CONST prototypeIndex:longint; CONST behavior:P_compoundGate; CONST visible:boolean); virtual;
    PROCEDURE ensureBaseGate(CONST gate:P_abstractGate; CONST visible:boolean); virtual;
    PROCEDURE countUpGate(CONST gate:P_abstractGate); virtual;
    PROCEDURE countDownGate(CONST gate:P_abstractGate); virtual;
    FUNCTION findEntry(CONST gate:P_abstractGate):longint; virtual;

    PROCEDURE finalizePalette(CONST boardOptions:T_challengeBoardOption; CONST paletteOptions: T_challengePaletteOption);
    FUNCTION isWorkspacePalette:boolean; virtual;
  end;

IMPLEMENTATION
USES visuals,Graphics,sprites;

{ T_challengePalette }

FUNCTION T_challengePalette.IndexOf(CONST gate: P_abstractGate): longint;
  VAR i:longint;
  begin
    if gate^.gateType=gt_compound then begin
      for i:=0 to length(paletteEntries)-1 do
        if (paletteEntries[i].entryType=gt_compound) and
           (P_compoundGate(gate)^.equalsInOtherPalette(paletteEntries[i].prototype) or
            (P_compoundGate(gate)^.prototype=P_captionedAndIndexed(paletteEntries[i].prototype)))
        then exit(i);
    end else begin
      for i:=0 to length(paletteEntries)-1 do
        if (paletteEntries[i].entryType=gate^.gateType) and
           (not(paletteEntries[i].preconfigured) or gate^.equals(paletteEntries[i].prototype))
        then exit(i);
    end;
    result:=-1;
  end;

CONSTRUCTOR T_challengePalette.create;
  begin
    setLength(paletteEntries,0);
    constructingChallenge:=true;
  end;

DESTRUCTOR T_challengePalette.destroy;
  VAR i:longint;
  begin
    detachUI;
    for i:=length(paletteEntries)-1 downto 0 do
    with paletteEntries[i] do if prototype<>nil then dispose(prototype,destroy);
    setLength(paletteEntries,0);
  end;

FUNCTION T_challengePalette.loadFromStream(
  VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
  begin
    constructingChallenge:=false;
    allowConfiguration:=stream.readBoolean;
    setLength(paletteEntries,stream.readNaturalNumber);
    for i:=0 to length(paletteEntries)-1 do with paletteEntries[i] do begin
      visible              :=stream.readBoolean;
      initialAvailableCount:=stream.readLongint;
      currentAvailableCount:=stream.readLongint;
      sourcePaletteIndex:=i;

      entryType:=T_gateType(stream.readByte([byte(low(T_gateType))..byte(high(T_gateType))]));
      {$ifdef debugMode}
      writeln('Reading palette entry #',i,' of type ',entryType);
      {$endif}
      if entryType=gt_compound then begin
        new(P_compoundGate(prototype),create(@self));
        P_compoundGate(prototype)^.readPrototypeFromStream(stream,i);
        preconfigured:=true; //compound gates are always preconfigured...
      end else begin
        preconfigured:=stream.readBoolean;
        if preconfigured then begin
          prototype:=newBaseGate(entryType);
          prototype^.readMetaDataFromStream(stream);
        end else prototype:=nil;
      end;
    end;
    result:=stream.allOkay;
  end;

PROCEDURE T_challengePalette.saveToStream(
  VAR stream: T_bufferedOutputStreamWrapper);
  VAR i:longint;
  begin
    stream.writeBoolean(allowConfiguration);
    stream.writeNaturalNumber(length(paletteEntries));
    for i:=0 to length(paletteEntries)-1 do with paletteEntries[i] do begin
      stream.writeBoolean(visible);
      stream.writeLongint(initialAvailableCount);
      stream.writeLongint(currentAvailableCount);
      stream.writeByte(byte(entryType));

      if entryType=gt_compound
      then P_compoundGate(prototype)^.writePrototypeToStream(stream,i)
      else begin
        stream.writeBoolean(preconfigured);
        if preconfigured then prototype^.writeToStream(stream,true);
      end;
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
      y0:longint=0;
      behavior: P_abstractGate;
  begin
    for i:=0 to length(visualPaletteItems)-1 do dispose(visualPaletteItems[i],destroy);

    {$ifdef debugMode}
    writeln('Challenge palette on ensureVisualPaletteItems:');
    for i:=0 to length(paletteEntries)-1 do begin
      writeln('  ',i,' ',paletteEntries[i].entryType,' ',paletteEntries[i].initialAvailableCount,' ',paletteEntries[i].visible);
    end;
    {$endif}

    k:=0;
    for i:=0 to length(paletteEntries)-1 do if (paletteEntries[i].visible) and (paletteEntries[i].currentAvailableCount>0) then begin

      if (paletteEntries[i].entryType=gt_compound) or (paletteEntries[i].preconfigured)
      then behavior:=paletteEntries[i].prototype^.clone(false)
      else behavior:=newBaseGate(paletteEntries[i].entryType);

      setLength(visualPaletteItems,k+1);
      new(visualPaletteItems[k],create(behavior));
      visualPaletteItems[k]^.uiAdapter:=ui;
      if not(allowConfiguration) then visualPaletteItems[k]^.fixedProperties:=true;

      visualPaletteItems[k]^.gridPos:=pointOf(1,y0);
      y0+=visualPaletteItems[k]^.getGridHeight;

      inc(k);
    end;
    setLength(visualPaletteItems,k);
  end;

PROCEDURE T_challengePalette.resetCounts;
  VAR i:longint;
  begin
    for i:=0 to length(paletteEntries)-1 do with paletteEntries[i] do
      currentAvailableCount:=initialAvailableCount;
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
  VAR i: integer;
  begin
    if constructingChallenge then begin
      for i:=0 to length(paletteEntries)-1 do if paletteEntries[i].sourcePaletteIndex=prototypeIndex then
        exit(P_compoundGate(paletteEntries[i].prototype^.clone(false)));
      assert(false,'Prototype could not be found');
    end else begin
      assert((prototypeIndex>=0) and (prototypeIndex<length(paletteEntries)),'Prototype index out of bounds');
      result:=P_compoundGate(paletteEntries[prototypeIndex].prototype^.clone(false));
    end;
  end;

FUNCTION T_challengePalette.hasPrototype(CONST prototypeIndex: longint
  ): boolean;
  VAR i:longint;
  begin
    if constructingChallenge then begin
      for i:=0 to length(paletteEntries)-1 do
        if (paletteEntries[i].sourcePaletteIndex=prototypeIndex) and
           (paletteEntries[i].prototype<>nil) then exit(true);
      result:=false;
    end else begin
      result:=(prototypeIndex>=0) and
              (prototypeIndex<length(paletteEntries)) and
              (paletteEntries[prototypeIndex].prototype<>nil);
    end;
  end;

PROCEDURE T_challengePalette.addPrototype(CONST prototypeIndex: longint; CONST behavior: P_compoundGate; CONST visible: boolean);
  VAR i:longint;
  begin
    assert(constructingChallenge,'This should only be called during challenge creation');
    i:=length(paletteEntries);
    setLength(paletteEntries,i+1);

    paletteEntries[i].visible           :=visible;
    paletteEntries[i].entryType         :=gt_compound;
    paletteEntries[i].prototype         :=behavior;
    paletteEntries[i].sourcePaletteIndex:=prototypeIndex;
    paletteEntries[i].currentAvailableCount:=0;
    paletteEntries[i].initialAvailableCount:=0;
    {$ifdef debugMode}
    writeln('Added palette entry #',i,': ',paletteEntries[i].entryType,' ',behavior^.gateType,' ',behavior^.getCaption,' ',prototypeIndex);
    {$endif}
  end;

PROCEDURE T_challengePalette.ensureBaseGate(CONST gate: P_abstractGate;
  CONST visible: boolean);
  VAR idx:longint;
  begin
    idx:=IndexOf(gate); if idx>=0 then exit;

    idx:=length(paletteEntries);
    setLength(paletteEntries,idx+1);

    paletteEntries[idx].visible           :=visible;
    paletteEntries[idx].entryType         :=gate^.gateType;
    paletteEntries[idx].prototype         :=gate^.clone(false);
    paletteEntries[idx].preconfigured     :=true;
    paletteEntries[idx].sourcePaletteIndex:=-1;
    paletteEntries[idx].currentAvailableCount:=0;
    paletteEntries[idx].initialAvailableCount:=0;
    {$ifdef debugMode}
    writeln('Added palette entry #',idx,': ',paletteEntries[idx].entryType,' ',gate^.gateType,' ',gate^.getCaption);
    {$endif}
  end;

PROCEDURE T_challengePalette.countUpGate(CONST gate: P_abstractGate);
  VAR idx:longint;
  begin
    idx:=IndexOf(gate);
    assert(idx>=0);
    if idx<0 then exit;
    inc(paletteEntries[idx].currentAvailableCount);
    if (paletteEntries[idx].currentAvailableCount=1) and (ui<>nil) then begin
      ensureVisualPaletteItems;
    end;
  end;

PROCEDURE T_challengePalette.countDownGate(CONST gate: P_abstractGate);
  VAR idx:longint;
  begin
    idx:=IndexOf(gate);
    assert(idx>=0);
    if idx<0 then exit;
    dec(paletteEntries[idx].currentAvailableCount);
    if (paletteEntries[idx].currentAvailableCount<=0) and (ui<>nil)  then begin
      ensureVisualPaletteItems;
    end;
  end;

FUNCTION T_challengePalette.findEntry(CONST gate: P_abstractGate): longint;
  begin
    result:=IndexOf(gate);
  end;

PROCEDURE T_challengePalette.finalizePalette(
  CONST boardOptions: T_challengeBoardOption;
  CONST paletteOptions: T_challengePaletteOption);
  VAR i,j:longint;
      gateTypesAvailable:array [T_gateType] of longint;
      gt:T_gateType;
  begin
    for i:=0 to length(paletteEntries)-1 do with paletteEntries[i] do
      initialAvailableCount:=currentAvailableCount;

    //Remove configured base gates
    if paletteOptions in [co_unconfiguredPaletteWithCounts,co_freePalette]
    then begin
      for gt in T_gateType do gateTypesAvailable[gt]:=-1;
      j:=0;
      for i:=0 to length(paletteEntries)-1 do begin
        if paletteEntries[i].entryType<>gt_compound then begin
          dispose(paletteEntries[i].prototype,destroy);
          paletteEntries[i].prototype:=nil;
          paletteEntries[i].preconfigured:=false;
          if gateTypesAvailable[paletteEntries[i].entryType]<0 then begin
            gateTypesAvailable[paletteEntries[i].entryType]:=j;
            paletteEntries[j]:=paletteEntries[i];
            inc(j);
          end else paletteEntries[gateTypesAvailable[paletteEntries[i].entryType]].initialAvailableCount+=paletteEntries[i].initialAvailableCount;
        end else begin
          paletteEntries[j]:=paletteEntries[i];
          inc(j);
        end;
      end;
      setLength(paletteEntries,j);
      allowConfiguration:=true;
    end else allowConfiguration:=false;

    //Set available count to (virtually) infinite, if counts should be ignored
    if paletteOptions in [co_preconfiguredPalette,co_freePalette]
    then for i:=0 to length(paletteEntries)-1 do
      paletteEntries[i].initialAvailableCount:=maxLongint shr 1;

    //Set all entries to visible if appropriate...
    if paletteOptions=co_freePalette then for i:=0 to length(paletteEntries)-1 do paletteEntries[i].visible:=true;

    constructingChallenge:=false;

    {$ifdef debugMode}
    for i:=0 to length(paletteEntries)-2 do if paletteEntries[i].entryType=gt_compound then
      for j:=i+1 to length(paletteEntries)-1 do if paletteEntries[j].entryType=gt_compound then
        assert(not(P_compoundGate(paletteEntries[i].prototype)^.usesPrototype(paletteEntries[j].prototype)));

    writeln('Challenge palette after finalization:');
    for i:=0 to length(paletteEntries)-1 do begin
      writeln('  ',i,' ',paletteEntries[i].entryType,' ',paletteEntries[i].initialAvailableCount,' ',paletteEntries[i].visible);
    end;

    {$endif}

  end;

FUNCTION T_challengePalette.isWorkspacePalette: boolean;
  begin
    result:=false;
  end;

{ T_workspacePalette }

PROCEDURE T_workspacePalette.removeSubPalette(CONST index: longint);
  VAR i:longint;
  begin
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
      ui^.paletteIndexChanged;
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
      prototypeIndex:=stream.readNaturalNumber;
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

FUNCTION T_workspacePalette.addBoard(CONST board: P_visualBoard; subPaletteIndex: longint; CONST subPaletteName: string): P_visualBoard;
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
    result:=paletteEntries[i].prototype;
    paletteEntries[i].subPaletteIndex:=subPaletteIndex;
    paletteEntries[i].visualSorting  :=visualIndex;
    reindex;
    filter:=-1;
  end;

PROCEDURE T_workspacePalette.updateEntry(CONST board:P_visualBoard);
  VAR i: longint;
  begin
    if board^.getIndexInPalette<0 then exit;
    i:=board^.getIndexInPalette;
    updateEntry(board,paletteEntries[i].subPaletteIndex,paletteNames[paletteEntries[i].subPaletteIndex]);
  end;

PROCEDURE T_workspacePalette.updateEntry(CONST board: P_visualBoard; subPaletteIndex: longint; CONST subPaletteName: string);
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

PROCEDURE T_workspacePalette.deleteEntry(CONST prototype: P_captionedAndIndexed);
  VAR i,i0:longint;
      d0, d1, d2: boolean;
  begin
    if ui^.isPrototypeInUse(prototype,d0,d1,d2) then exit;
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

FUNCTION T_workspacePalette.allowDeletion(CONST gate: P_abstractGate; OUT reasonForFalse:string): boolean;
  VAR i:longint;
      prototype: P_visualBoard;
      usedByUndoList, usedByClipboard, usedByActiveBoard: boolean;
  PROCEDURE addReason(CONST s:string);
    begin
      if reasonForFalse='' then reasonForFalse:='Das Baulement wird noch verwendet von:';
      reasonForFalse+=LineEnding+'  '+s;
    end;

  begin
    reasonForFalse:='';
    result:=true;
    if gate^.gateType<>gt_compound then begin
      reasonForFalse:='Grundelemente können nicht gelöscht werden.';
      exit(false);
    end;
    if P_compoundGate(gate)^.prototype=nil then begin
      reasonForFalse:='DAS HIER SOLLTE NIE ANGEZEIGT WERDEN! (prototype=nil)';
      exit(false);
    end;
    if not(P_compoundGate(gate)^.prototype^.isVisualBoard) then begin
      reasonForFalse:='DAS HIER SOLLTE NIE ANGEZEIGT WERDEN! (non visual prototype)';
      exit(false);
    end;
    prototype:=P_visualBoard(P_compoundGate(gate)^.prototype);

    if ui^.isPrototypeInUse(prototype,usedByActiveBoard,usedByClipboard,usedByUndoList) then begin
      if usedByUndoList then addReason('der aktuellen Schaltung');
      if usedByClipboard then addReason('der Zwischenablage');
      if usedByUndoList then addReason('der Undo-Liste');
      result:=false;
    end;
    for i:=0 to length(paletteEntries)-1 do
      if (i<>prototype^.getIndexInPalette) and
         (paletteEntries[i].prototype<>nil) and
         (paletteEntries[i].prototype^.usesPrototype(prototype)) then begin
      addReason(StringReplace(paletteEntries[i].prototype^.getCaption,LineEnding,'\n',[rfReplaceAll]));
      result:=false;
    end;
  end;

PROCEDURE T_workspacePalette.deleteEntry(CONST index: longint);
  VAR
    prototype: P_visualBoard;
    i: integer;
    d0, d1, d2: boolean;
  begin
    if (index<0) or (index>=length(paletteEntries)) or (paletteEntries[index].entryType<>gt_compound) then exit;
    prototype:=paletteEntries[index].prototype;
    if ui^.isPrototypeInUse(prototype,d0,d1,d2) then exit;
    for i:=index+1 to length(paletteEntries)-1 do
      if (paletteEntries[i].prototype<>nil) and
         (paletteEntries[i].prototype^.usesPrototype(prototype)) then exit;
    dispose(paletteEntries[index].prototype,destroy);
    for i:=index to length(paletteEntries)-2 do paletteEntries[i]:=paletteEntries[i+1];
    setLength(paletteEntries,length(paletteEntries)-1);
    reindex;
  end;

FUNCTION T_workspacePalette.allowDeletion(CONST index: longint): boolean;
  VAR i:longint;
      prototype: P_visualBoard;
      d0, d1, d2: boolean;
  begin
    if (index<0) or (index>=length(paletteEntries)) or (paletteEntries[index].entryType<>gt_compound) then exit(false);
    prototype:=paletteEntries[index].prototype;
    if ui^.isPrototypeInUse(prototype,d0,d1,d2) then exit(false);
    for i:=index+1 to length(paletteEntries)-1 do
      if (paletteEntries[i].prototype<>nil) and
         (paletteEntries[i].prototype^.usesPrototype(prototype)) then exit(false);
    result:=true;
  end;

PROCEDURE T_workspacePalette.setFilter(CONST newValue: longint);
  VAR changed:boolean;
  begin
    if (newValue>=0) and (newValue<length(paletteEntries)) and (paletteEntries[newValue].prototype=nil) then exit;
    changed:=filter<>newValue;
    filter:=newValue;
    if changed and (ui<>nil) then begin
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

FUNCTION T_workspacePalette.isWorkspacePalette: boolean;
  begin
    result:=true;
  end;

FUNCTION T_workspacePalette.setPaletteEntryCaption    (CONST index:longint; CONST value:string):boolean;
  begin
    if (index<0) or (index>=length(paletteEntries)) or (paletteEntries[index].entryType<>gt_compound) then exit(false);
    paletteEntries[index].prototype^.setCaption(value);
    result:=true;
  end;

FUNCTION T_workspacePalette.setPaletteEntryDescription(CONST index:longint; CONST value:string):boolean;
  begin
    if (index<0) or (index>=length(paletteEntries)) or (paletteEntries[index].entryType<>gt_compound) then exit(false);
    paletteEntries[index].prototype^.setDescription(value);
    result:=true;
  end;

PROCEDURE T_workspacePalette.setPaletteEntrySubPalette (CONST index:longint; CONST value:string);
  VAR subPaletteIndex:longint=0;
  begin
    if (index<0) or (index>=length(paletteEntries)) then exit;
    while (subPaletteIndex<length(paletteNames)) and (paletteNames[subPaletteIndex]<>value) do inc(subPaletteIndex);
    if subPaletteIndex>=length(paletteNames) then begin
      setLength(paletteNames,length(paletteNames)+1);
      paletteNames[subPaletteIndex]:=value;
    end;
    paletteEntries[index].subPaletteIndex:=subPaletteIndex;
    reindex;
  end;

PROCEDURE T_workspacePalette.markAllEntriesForExport(CONST Selected: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(paletteEntries)-1 do paletteEntries[i].markedForExport:=Selected;
  end;

PROCEDURE T_workspacePalette.markEntryForExportToggle(CONST index: longint);
  begin
    if (index<0) or (index>=length(paletteEntries)) then exit;
    markEntryForExport(index,not(paletteEntries[index].markedForExport));
  end;

PROCEDURE T_workspacePalette.markEntryForExport(CONST index: longint;
  CONST Selected: boolean);
  VAR i:longint;
      prototype: P_visualBoard;
  begin
    if (index<0) or (index>=length(paletteEntries)) then exit;
    with paletteEntries[index] do if (entryType<>gt_compound) or (markedForExport=Selected) then begin
      markedForExport:=Selected;
      exit;
    end;

    prototype:=paletteEntries[index].prototype;
    paletteEntries[index].markedForExport:=Selected;

    if Selected then begin
      //We want to mark. Additional marks may be required.
      for i:=0 to index-1 do
        if (paletteEntries[i].entryType=gt_compound) and
           not(paletteEntries[i].markedForExport) and
           prototype^.usesPrototype(paletteEntries[i].prototype)
        then markEntryForExport(i,true);
    end else begin
      //We want to unmark...
      for i:=index+1 to length(paletteEntries)-1 do
        if (paletteEntries[i].entryType=gt_compound) and
           (paletteEntries[i].markedForExport) and
           paletteEntries[i].prototype^.usesPrototype(prototype)
        then markEntryForExport(i,false);
    end;
  end;

PROCEDURE T_workspacePalette.exportSelected(CONST fileName: string);
  VAR temp:T_workspacePalette;
      i:longint;
      j:longint=0;

      originalProtoypes:array of record
        proto:P_visualBoard;
        spi:longint;
      end;
  begin
    temp.create;
    setLength(temp.paletteNames,length(paletteNames));
    for i:=0 to length(paletteNames)-1 do temp.paletteNames[i]:=paletteNames[i];
    setLength(originalProtoypes,length(paletteEntries));
    for i:=0 to length(paletteEntries)-1 do if (paletteEntries[i].markedForExport) and (paletteEntries[i].entryType=gt_compound)
      then begin
        originalProtoypes[j].proto:=paletteEntries[i].prototype;
        originalProtoypes[j].spi  :=paletteEntries[i].subPaletteIndex;
        inc(j);
      end;
    setLength(originalProtoypes,j);
    setLength(temp.paletteEntries,j);
    for i:=0 to length(temp.paletteEntries)-1 do with temp.paletteEntries[i] do begin
      markedForExport:=false;
      visualSorting:=i;
      subPaletteIndex:=originalProtoypes[i].spi;
      entryType:=gt_compound;
      prototype:=originalProtoypes[i].proto^.clone;
      prototype^.moveToPalette(@temp);
      for j:=0 to i-1 do prototype^.prototypeUpdated(originalProtoypes[j].proto,temp.paletteEntries[j].prototype);
    end;

    temp.reindex;
    temp.saveToFile(fileName);
    temp.destroy;

    reindex;
  end;

PROCEDURE T_workspacePalette.importPalette(CONST fileName: string);
  VAR temp:T_workspacePalette;
      i,j:longint;
      updatedPrototypes:array of P_visualBoard;
  begin
    temp.create;
    if not(temp.loadFromFile(fileName)) then begin
      temp.destroy;
      exit;
    end;
    setLength(updatedPrototypes,length(temp.paletteEntries));
    for i:=0 to length(temp.paletteEntries)-1 do if (temp.paletteEntries[i].entryType=gt_compound) then begin
      updatedPrototypes[i]:=addBoard(temp.paletteEntries[i].prototype,-1,temp.paletteNames[temp.paletteEntries[i].subPaletteIndex]);
      updatedPrototypes[i]^.moveToPalette(@self);
      for j:=0 to i-1 do updatedPrototypes[i]^.prototypeUpdated(temp.paletteEntries[j].prototype,updatedPrototypes[j]);
    end;
    temp.destroy;
  end;

PROCEDURE T_workspacePalette.swapPaletteName(CONST index: longint; CONST up: boolean);
  VAR otherIndex:longint;
      s:string;
      k:longint;
  begin
    if up then begin
      otherIndex:=index-1;
      if otherIndex<0 then exit;
    end else begin
      otherIndex:=index+1;
      if otherIndex>=length(paletteNames) then exit;
    end;
    s:=paletteNames[index];
    paletteNames[index]:=paletteNames[otherIndex];
    paletteNames[otherIndex]:=s;

    for k:=0 to length(paletteEntries)-1 do
      if paletteEntries[k].subPaletteIndex=index
      then paletteEntries[k].subPaletteIndex:=otherIndex else
      if paletteEntries[k].subPaletteIndex=otherIndex
      then paletteEntries[k].subPaletteIndex:=index;
  end;

PROCEDURE T_workspacePalette.removeDuplicates(CONST byBehavior:boolean);
  VAR hashGroups:array of T_arrayOfLongint;
      k:longint;
      i,j:longint;
      protRetain,protRemove:P_visualBoard;
      behvRetain,behvRemove:P_compoundGate;
      h:word;
      equals: boolean;
      pairsToCleanup:array of record
        retainIndex,
        removeIndex:longint;
      end;
      removed:T_arrayOfLongint;

  PROCEDURE addPairToCleanup(CONST retain,remove:longint);
    VAR i:longint;
    begin
      i:=length(pairsToCleanup);
      setLength(pairsToCleanup,i+1);
      with pairsToCleanup[i] do begin
        retainIndex:=retain;
        removeIndex:=remove;
      end;
    end;

  begin
    setLength(pairsToCleanup,0);
    setLength(hashGroups,65536);
    for k:=0 to length(hashGroups)-1 do hashGroups[k]:=C_EMPTY_LONGINT_ARRAY;

    for k:=0 to length(paletteEntries)-1 do if paletteEntries[k].entryType=gt_compound then begin
      if byBehavior
      then h:=paletteEntries[k].prototype^.interfaceHash
      else h:=paletteEntries[k].prototype^.hash;
      append(hashGroups[h],k);
    end;

    {$ifdef debugMode}
    writeln('DUPLICATE SCAN over ',length(paletteEntries),' entries');
    for h:=0 to 65535 do if length(hashGroups[h])>1 then begin
      write('Group #',h,': ');
      for k in hashGroups[h] do write(k,',');
      writeln;
    end;
    {$endif}

    for h:=0 to 65535 do begin
      if length(hashGroups[h])>1 then begin
        for i:=0 to length(hashGroups[h])-2 do for j:=i+1 to length(hashGroups[h])-1 do begin
          protRetain:=paletteEntries[hashGroups[h,i]].prototype;
          protRemove:=paletteEntries[hashGroups[h,j]].prototype;
          if byBehavior then begin
             behvRetain:=protRetain^.extractBehavior;
             behvRemove:=protRemove^.extractBehavior;
             equals:=behvRetain^.behaviorEquals(behvRemove);
             dispose(behvRetain,destroy);
             dispose(behvRemove,destroy);
          end else equals:=protRetain^.equals(protRemove);
          if equals then addPairToCleanup(hashGroups[h,i],hashGroups[h,j]);
          {$ifdef debugMode}
          writeln('Comparing ',hashGroups[h,j],' (',protRetain^.getCaption,') against ',hashGroups[h,i],' (',protRemove^.getCaption,') : ',equals);
          {$endif}
        end;
      end;
      setLength(hashGroups[h],0);
    end;
    setLength(hashGroups,0);

    setLength(removed,0);
    {$ifdef debugMode}
    writeln('To clean up:');
    {$endif}
    for k:=0 to length(pairsToCleanup)-1 do with pairsToCleanup[k] do begin
      {$ifdef debugMode}
      writeln('Retain ',retainIndex,'; remove: ',removeIndex);
      {$endif}
      protRetain:=paletteEntries[retainIndex].prototype;
      protRemove:=paletteEntries[removeIndex].prototype;
      for i:=removeIndex+1 to length(paletteEntries)-1 do
        if paletteEntries[i].entryType=gt_compound
        then paletteEntries[i].prototype^.prototypeUpdated(protRemove,protRetain);
      //Leave the removed entries where they are for the moment, but remember their index
      append(removed,removeIndex);
    end;

    //Actually remove the entries:
    j:=0;
    for i:=0 to length(paletteEntries)-1 do if arrContains(removed,i) then begin
      dispose(paletteEntries[i].prototype,destroy);
    end else begin
      paletteEntries[j]:=paletteEntries[i];
      inc(j);
    end;
    setLength(paletteEntries,j);
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
    ui:=nil;
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

FUNCTION T_palette.getGateAtScreenPosition(CONST x,y:longint):pointer;
  VAR g: P_visualGate;
      zoom,yOffset:longint;
      gridPos: T_point;
      info: T_hoverInfo;
  begin
    result:=nil;
    zoom:=ui^.getZoom;
    yOffset:=ui^.paletteYOffset;
    gridPos:=pointOf(
      round((x        )/zoom),
      round((y-yOffset)/zoom));
    for g in visualPaletteItems do if g^.isAtGridPos(gridPos,info) then exit(g);
  end;

FUNCTION T_palette.isGateHit(CONST gridPos: T_point; OUT gate: P_visualGate): boolean;
  VAR g:P_visualGate;
     hoverInfo: T_hoverInfo;
  begin
    for g in visualPaletteItems do if g^.isAtGridPos(gridPos,hoverInfo) then begin gate:=g; exit(true); end;
    gate:=nil;
    result:=false;
  end;

PROCEDURE T_palette.comboBoxSelect(Sender: TObject);
  {$ifdef debugMode} VAR startTime:qword; {$endif}
  begin
    {$ifdef debugMode} startTime:=GetTickCount64; {$endif}
    selectSubPalette(ui^.paletteComboBox.ItemIndex);
    ui^.paintAll;
    ui^.repaintImage;
    {$ifdef debugMode} writeln('Palette switch took ',GetTickCount64-startTime,' ticks'); {$endif}
  end;

FUNCTION T_palette.allowDeletion(CONST gate: P_abstractGate; OUT reasonForFalse:string): boolean;
  begin
    reasonForFalse:='Aus dieser Palette kann nichts gelöscht werden.';
    result:=false;
  end;

PROCEDURE T_palette.paint;
  VAR g:P_visualGate;
      yOffset:longint;
      Canvas: TCanvas;
  begin
    Canvas:=ui^.paletteCanvas;
    Canvas.Brush.color:=BOARD_COLOR;
    Canvas.Brush.style:=bsSolid;
    Canvas.Pen.color:=clBlack;
    Canvas.Pen.style:=psSolid;
    Canvas.Rectangle(-1,-1,ui^.paletteWidth,2000);
    gradientSprite.renderRect(Canvas,ui^.getZoom,ui^.paletteWidth-ui^.getZoom,0,Canvas.height-1);

    yOffset:=ui^.paletteYOffset;
    for g in visualPaletteItems do begin
      g^.canvasPos:=pointOf(g^.gridPos[0]*ui^.getZoom,
                            g^.gridPos[1]*ui^.getZoom+yOffset);
      g^.paintAll(Canvas,ui^.getZoom);
    end;
  end;

PROCEDURE T_palette.dropPaletteItem(CONST gatePtr: pointer);
  begin end; //dummy

FUNCTION T_palette.hasPrototype(CONST prototypeIndex: longint): boolean;
  begin result:=false; end; //dummy

PROCEDURE T_palette.addPrototype(CONST prototypeIndex: longint;
  CONST behavior: P_compoundGate; CONST visible: boolean);
  begin end; //dummy

PROCEDURE T_palette.ensureBaseGate(CONST gate: P_abstractGate; CONST visible:boolean);
  begin end; //dummy

PROCEDURE T_palette.countUpGate(CONST gate: P_abstractGate);
  begin end; //dummy

PROCEDURE T_palette.countDownGate(CONST gate: P_abstractGate);
  begin end; //dummy

end.

