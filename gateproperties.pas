UNIT gateProperties;
{$mode objfpc}{$H+}
INTERFACE
USES logicalGates,compoundGates, myGenerics,paletteHandling,ValEdit,Classes;
TYPE
  T_gatePropertyType=(pt_number,pt_string,pt_wireWidth,pt_connectionCount,pt_enumWithOptionForNewEntry,pt_data);
  T_gatePropertyEnum=(gpe_captionReadOnly,
                      gpe_caption,
                      gpe_subPalette,
                      gpe_descriptionReadOnly,
                      gpe_description,
                      gpe_editableLabel,
                      gpe_intervalGreaterZero,
                      gpe_inputWidth,
                      gpe_outputWidth,
                      gpe_inputCount,
                      gpe_romData);
  T_gatePropertyEnums=set of T_gatePropertyEnum;
  T_gateProperty=record
    name:string;
    typ:T_gatePropertyType;
    minValue,maxValue:longint;
    readonly:boolean;
  end;

CONST
  C_gateProperty:array[T_gatePropertyEnum] of T_gateProperty=
  ((name:'Name'        ;            typ:pt_string; minValue:0; maxValue:         0; readonly:true),
   (name:'Name'        ;            typ:pt_string; minValue:0; maxValue:         0; readonly:false),
   (name:'Palette'     ;            typ:pt_enumWithOptionForNewEntry; minValue:0; maxValue: 0; readonly:false),
   (name:'Beschreibung';            typ:pt_string; minValue:0; maxValue:         0; readonly:true),
   (name:'Beschreibung';            typ:pt_string; minValue:0; maxValue:         0; readonly:false),
   (name:'Label';                   typ:pt_string; minValue:0; maxValue:         0; readonly:false),
   (name:'Intervall';               typ:pt_number; minValue:1; maxValue:maxLongint; readonly:false),
   (name:'Breite Eingang (bits)';   typ:pt_wireWidth; minValue:1; maxValue:WIRE_MAX_WIDTH; readonly:false),
   (name:'Breite Ausgang (bits)';   typ:pt_wireWidth; minValue:1; maxValue:WIRE_MAX_WIDTH; readonly:false),
   (name:'Anzahl Eingänge';         typ:pt_connectionCount; minValue:2; maxValue:WIRE_MAX_WIDTH; readonly:false),
   (name:'Speicherinhalt';          typ:pt_data; minValue:0; maxValue:         0; readonly:false));

  C_availableProperies:array[T_gateType,false..true] of T_gatePropertyEnums=
  {gt_notGate} (([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly]),
  {gt_andGate}  ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_inputCount]),
  {gt_orGate}   ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_inputCount]),
  {gt_xorGate}  ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_inputCount]),
  {gt_nandGate} ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_inputCount]),
  {gt_norGate}  ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_inputCount]),
  {gt_nxorGate} ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_inputCount]),
  {gt_input}    ([gpe_captionReadOnly,gpe_subPalette],          [gpe_editableLabel,gpe_outputWidth]),
  {gt_output}   ([gpe_captionReadOnly,gpe_subPalette],          [gpe_editableLabel,gpe_inputWidth]),
  {gt_compound} ([gpe_caption,gpe_description,gpe_subPalette],  [gpe_captionReadOnly,gpe_descriptionReadOnly]),
  {gt_clock}    ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_intervalGreaterZero]),
  {gt_adapter}  ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_inputWidth,gpe_outputWidth]),
                ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly]),
                ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly]),
  {gt_gatedCl..}([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_intervalGreaterZero]),
  {1/2->1}      ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_inputWidth]),
  {1/2->0}      ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_inputWidth]),
                ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly]),
                ([gpe_captionReadOnly,gpe_subPalette],          [gpe_captionReadOnly,gpe_romData]),
                ([gpe_captionReadOnly]               ,          [gpe_captionReadOnly]));

TYPE
  T_gatePropertyValue=record
    s:string;
    n:longint;
    romContents:T_wireValueArray
  end;

  T_gateProperties=array of T_gateProperty;

  { T_gatePropertyValues }

  T_gatePropertyValues=object
    private
      onAccept: TNotifyEvent;
      gate:P_abstractGate;
      palette:P_workspacePalette;
      entry:array of record
        prop   :T_gatePropertyEnum;
        value  :T_gatePropertyValue;
        modified:boolean;
      end;
      PROCEDURE EditButtonClick(Sender: TObject);
      FUNCTION fetchValue(CONST prop:T_gatePropertyEnum):T_gatePropertyValue;
      PROCEDURE applyValue(CONST prop:T_gatePropertyEnum; CONST value:T_gatePropertyValue);

      PROCEDURE ValueListEditorValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
      PROCEDURE connectEditor(editor:TValueListEditor);
    public
      CONSTRUCTOR createForPaletteEntry(editor:TValueListEditor; onModify:TNotifyEvent; CONST gate_:P_abstractGate; CONST palette_:P_palette);
      CONSTRUCTOR createForBoardEntry  (editor:TValueListEditor; onModify:TNotifyEvent; CONST gate_:P_abstractGate);

      DESTRUCTOR destroy;
      FUNCTION acceptNewValue(CONST index:longint; CONST newValue:string):boolean;
      FUNCTION applyValues:boolean;
      FUNCTION arePropertiesForBoard:boolean;

  end;

IMPLEMENTATION
USES sysutils,romEditorUnit;
{ T_gatePropertyValues }

FUNCTION T_gatePropertyValues.fetchValue(CONST prop: T_gatePropertyEnum): T_gatePropertyValue;
  begin
    result.n:=0;
    result.s:='';
    initialize(result.romContents);
    setLength(result.romContents,0);
    case prop of
      gpe_caption,gpe_captionReadOnly:
        result.s:=StringReplace(gate^.getCaption,LineEnding,'\n',[rfReplaceAll]);
      gpe_description,gpe_descriptionReadOnly:
        result.s:=StringReplace(gate^.getDescription,LineEnding,'\n',[rfReplaceAll]);
      gpe_editableLabel:
        if gate^.gateType in [gt_input,gt_output]
        then result.s:=P_inputGate(gate)^.getCaption;
      gpe_intervalGreaterZero:
        if gate^.gateType in [gt_clock,gt_gatedClock]
        then result.n:=P_clock(gate)^.interval;
      gpe_inputWidth:
        case gate^.gateType of
          gt_output: result.n:=P_outputGate(gate)^.width;
          gt_adapter: result.n:=P_adapter(gate)^.inputWidth(0);
          gt_undeterminedToFalse: result.n:=P_tendToFalse(gate)^.input.width;
          gt_undeterminedToTrue : result.n:=P_tendToTrue (gate)^.input.width;
        end;
      gpe_outputWidth:
        case gate^.gateType of
          gt_input: result.n:=P_inputGate(gate)^.width;
          gt_adapter: result.n:=P_adapter(gate)^.outputWidth(0);
        end;
      gpe_inputCount :
        if gate^.gateType in [gt_andGate,gt_orGate,gt_xorGate,gt_nandGate,gt_norGate,gt_nxorGate]
        then result.n:=P_binaryBaseGate(gate)^.inputCount;
      gpe_subPalette: begin
        assert(palette<>nil);
        result.n:=palette^.findEntry(gate);
        if (result.n>=0) and (result.n<length(palette^.paletteEntries))
        then begin
          result.n:=palette^.paletteEntries[result.n].subPaletteIndex;
          if (result.n>=0) and (result.n<length(palette^.paletteNames))
          then result.s:=palette^.paletteNames[result.n]
          else begin
            result.s:='?';
            result.n:=-1;
          end;
        end else begin
          result.s:='?';
          result.n:=-1;
        end;
      end;
      gpe_romData: result.romContents:=P_RomGate(gate)^.data;
      else assert(false);
    end;
  end;

PROCEDURE T_gatePropertyValues.EditButtonClick(Sender: TObject);
  VAR i:longint=0;
  begin
    while (i<length(entry)) and (entry[i].prop<>gpe_romData) do inc(i);
    if i>=length(entry) then exit;
    if RomEditorForm.showFor(entry[i].value.romContents) then begin
      entry[i].modified:=true;
      onAccept(Sender);
    end;
  end;

PROCEDURE T_gatePropertyValues.applyValue(CONST prop: T_gatePropertyEnum; CONST value: T_gatePropertyValue);
  begin
    case prop of
      gpe_caption: begin
        P_compoundGate(gate)^.prototype^.setCaption(StringReplace(value.s,'\n',LineEnding,[rfReplaceAll]));
      end;
      gpe_description: begin
        P_compoundGate(gate)^.prototype^.setDescription(StringReplace(value.s,'\n',LineEnding,[rfReplaceAll]));
      end;
      gpe_subPalette: begin
        //we could fetch the value first, so all should be okay...
        palette^.reassignEntry(gate,value.s);
      end;
      gpe_editableLabel: if gate^.gateType in [gt_input,gt_output] then begin
        P_inputGate(gate)^.ioLabel:=value.s;
      end;
      gpe_intervalGreaterZero: if gate^.gateType in [gt_clock,gt_gatedClock] then begin
        P_clock(gate)^.interval:=value.n;
      end;
      gpe_inputWidth:
        case gate^.gateType of
          gt_output: begin
            P_outputGate(gate)^.width:=value.n;
            P_outputGate(gate)^.reset;
          end;
          gt_adapter: P_adapter(gate)^.setIoWidth(value.n,gate^.outputWidth(0));
          gt_undeterminedToFalse: begin P_tendToFalse(gate)^.input.width:=value.n; P_tendToFalse(gate)^.output.width:=value.n; gate^.reset; end;
          gt_undeterminedToTrue : begin P_tendToTrue (gate)^.input.width:=value.n; P_tendToTrue (gate)^.output.width:=value.n; gate^.reset; end;
        end;
      gpe_outputWidth:
        case gate^.gateType of
          gt_input: begin
            P_inputGate(gate)^.width:=value.n;
            P_inputGate(gate)^.reset;
          end;
          gt_adapter: P_adapter(gate)^.setIoWidth(gate^.inputWidth(0),value.n);
        end;
      gpe_inputCount :
        if gate^.gateType in [gt_andGate,gt_orGate,gt_xorGate,gt_nandGate,gt_norGate,gt_nxorGate]
        then P_binaryBaseGate(gate)^.inputCount:=value.n;
      gpe_romData: begin
        if gate^.gateType in [gt_ram,gt_rom]
        then P_RomGate(gate)^.data:=value.romContents;
      end;
    end;
  end;

PROCEDURE T_gatePropertyValues.ValueListEditorValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  begin
    if aCol=0 then begin
      newValue:=oldValue;
    end else begin
      if acceptNewValue(aRow-1,newValue)
      then onAccept(Sender)
      else newValue:=oldValue;
    end;
  end;

PROCEDURE T_gatePropertyValues.connectEditor(editor: TValueListEditor);
  VAR
    i: integer;
    s: string;
  begin
    editor.OnValidateEntry:=@ValueListEditorValidateEntry;
    editor.OnEditButtonClick:=@EditButtonClick;
    editor.clear;
    editor.rowCount:=length(entry);
    for i:=0 to length(entry)-1 do begin
      editor.Cells[0,i+1]:=C_gateProperty[entry[i].prop].name;
      case C_gateProperty[entry[i].prop].typ of
        pt_number,pt_wireWidth,pt_connectionCount: begin
          editor.Cells[1,i+1]:=intToStr(entry[i].value.n);
          editor.ItemProps[i].EditStyle:=esSimple;
        end;
        pt_string: begin
          editor.Cells[1,i+1]:=entry[i].value.s;
          editor.ItemProps[i].EditStyle:=esSimple;
        end;
        pt_enumWithOptionForNewEntry: begin
          editor.Cells[1,i+1]:=entry[i].value.s;
          editor.ItemProps[i].EditStyle:=esPickList;
          editor.ItemProps[i].PickList.clear;
          for s in palette^.subPaletteNames do editor.ItemProps[i].PickList.add(s);
        end;
        pt_data: begin
          editor.Cells[1,i+1]:='<data>';
          editor.ItemProps[i].EditStyle:=esEllipsis;
        end;
      end;
    end;
    editor.editor.color:=$00703838;
    editor.editor.Font.color:=$00FFFFFF;
    editor.AutoSizeColumn(0);
  end;

CONSTRUCTOR T_gatePropertyValues.createForPaletteEntry(editor: TValueListEditor; onModify: TNotifyEvent; CONST gate_: P_abstractGate; CONST palette_: P_palette);
  VAR p:T_gatePropertyEnum;
      i:longint=0;
  begin
    onAccept:=onModify;
    gate:=gate_;
    palette:=P_workspacePalette(palette_);
    setLength(entry,0);
    for p in C_availableProperies[gate^.gateType,false] do begin
      setLength(entry,i+1);
      entry[i].prop:=p;
      entry[i].value:=fetchValue(p);
      entry[i].modified:=false;
      inc(i);
    end;
    connectEditor(editor);
  end;

CONSTRUCTOR T_gatePropertyValues.createForBoardEntry(editor: TValueListEditor; onModify: TNotifyEvent; CONST gate_: P_abstractGate);
  VAR p:T_gatePropertyEnum;
      i:longint=0;
  begin
    onAccept:=onModify;
    gate:=gate_;
    palette:=nil;
    setLength(entry,0);
    for p in C_availableProperies[gate^.gateType,true] do begin
      setLength(entry,i+1);
      entry[i].prop:=p;
      entry[i].value:=fetchValue(p);
      entry[i].modified:=false;
      inc(i);
    end;
    connectEditor(editor);
  end;

DESTRUCTOR T_gatePropertyValues.destroy;
  begin
    setLength(entry,0);
  end;

FUNCTION T_gatePropertyValues.acceptNewValue(CONST index: longint;
  CONST newValue: string): boolean;
  VAR newNumber:int64;
  begin
    if C_gateProperty[entry[index].prop].readonly then exit(false);
    case C_gateProperty[entry[index].prop].typ of
      pt_number: begin
        newNumber:=StrToInt64Def(newValue,int64(maxLongint)+1);
        if (newNumber>C_gateProperty[entry[index].prop].maxValue) or
           (newNumber<C_gateProperty[entry[index].prop].minValue) then exit(false);
        entry[index].modified:=entry[index].modified or (entry[index].value.n<>newNumber);
        entry[index].value.n:=longint(newNumber);
        result:=true;
      end;
      pt_wireWidth: begin
        newNumber:=StrToInt64Def(newValue,int64(maxLongint)+1);
        if not((newNumber=1) or (newNumber=4) or (newNumber=8) or (newNumber=16)) then exit(false);
        entry[index].modified:=entry[index].modified or (entry[index].value.n<>newNumber);
        entry[index].value.n:=byte(newNumber);
        result:=true;
      end;
      pt_connectionCount: begin
        newNumber:=StrToInt64Def(newValue,int64(maxLongint)+1);
        if (newNumber<2) or (newNumber>WIRE_MAX_WIDTH) then exit(false);
        entry[index].modified:=entry[index].modified or (entry[index].value.n<>newNumber);
        entry[index].value.n:=byte(newNumber);
        result:=true;
      end;
      pt_string: begin
        entry[index].modified:=entry[index].modified or (entry[index].value.s<>newValue);
        entry[index].value.s:=newValue;
        result:=true;
      end;
      pt_enumWithOptionForNewEntry: begin
        entry[index].modified:=entry[index].modified or (entry[index].value.s<>newValue);
        entry[index].value.s:=newValue;
        result:=true;
      end
      else result:=false;
    end;
  end;

FUNCTION T_gatePropertyValues.applyValues:boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=0 to length(entry)-1 do
    with entry[i] do
    if modified
    then begin
      applyValue(prop,value);
      result:=true;
    end;
  end;

FUNCTION T_gatePropertyValues.arePropertiesForBoard: boolean;
  begin
    result:=palette=nil;
  end;

end.

