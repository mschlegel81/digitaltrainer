UNIT gateProperties;
{$mode objfpc}{$H+}
INTERFACE
USES serializationUtil,logicGates;
TYPE
  T_gatePropertyType=(pt_number,pt_string);
  T_gatePropertyEnum=(gpe_caption,
                      gpe_description,
                      gpe_editableLabel,
                      gpe_intervalGreaterZero);
  T_gatePropertyEnums=set of T_gatePropertyEnum;
  T_gateProperty=record
    name:string;
    typ:T_gatePropertyType;
    minValue,maxValue:longint;
    readonly:boolean;
  end;

CONST
  C_gateProperty:array[T_gatePropertyEnum] of T_gateProperty=
  ((name:'Name'        ; typ:pt_string; minValue:0; maxValue:   0; readonly:true),
   (name:'Beschreibung'; typ:pt_string; minValue:0; maxValue:   0; readonly:true),
   (name:'Label';        typ:pt_string; minValue:0; maxValue:   0; readonly:false),
   (name:'Intervall';    typ:pt_number; minValue:1; maxValue:1024; readonly:false));
  C_availableProperies:array[T_gateType] of T_gatePropertyEnums=
  {gt_notGate} ([gpe_caption],
  {gt_andGate}  [gpe_caption],
  {gt_orGate}   [gpe_caption],
  {gt_xorGate}  [gpe_caption],
  {gt_nandGate} [gpe_caption],
  {gt_norGate}  [gpe_caption],
  {gt_nxorGate} [gpe_caption],
  {gt_input}    [gpe_editableLabel],
  {gt_output}   [gpe_editableLabel],
  {gt_compound} [gpe_caption,gpe_description],
  {gt_clock}    [gpe_caption,gpe_intervalGreaterZero]);

TYPE
  T_gatePropertyValue=record
    s:string;
    n:longint;
  end;

  T_gateProperties=array of T_gateProperty;

  { T_gatePropertyValues }

  T_gatePropertyValues=object(T_serializable)
    private
      gate:P_abstractGate;
      entry:array of record
        prop :T_gatePropertyEnum;
        value:T_gatePropertyValue;
        modified:boolean;
      end;
      FUNCTION fetchValue(CONST prop:T_gatePropertyEnum):T_gatePropertyValue;
      PROCEDURE applyValue(CONST prop:T_gatePropertyEnum; CONST value:T_gatePropertyValue);

    public
      CONSTRUCTOR create(CONST gate_:P_abstractGate);
      DESTRUCTOR destroy;

      FUNCTION getSerialVersion:dword; virtual;
      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

      FUNCTION count:longint;
      FUNCTION key           (CONST index:longint):string;
      FUNCTION value         (CONST index:longint):string;
      FUNCTION acceptNewValue(CONST index:longint; CONST newValue:string):boolean;
      PROCEDURE applyValues;
  end;

IMPLEMENTATION
USES sysutils;
{ T_gatePropertyValues }

FUNCTION T_gatePropertyValues.fetchValue(CONST prop: T_gatePropertyEnum): T_gatePropertyValue;
  begin
    result.n:=0;
    result.s:='';
    case prop of
      gpe_caption: result.s:=gate^.caption;
      gpe_description: result.s:=gate^.getDescription;
      gpe_editableLabel: if gate^.gateType in [gt_input,gt_output] then begin
        result.s:=P_inputGate(gate)^.caption;
      end;
      gpe_intervalGreaterZero: if gate^.gateType=gt_clock then begin
        result.n:=P_clock(gate)^.interval;
      end;
    end;
  end;

PROCEDURE T_gatePropertyValues.applyValue(CONST prop: T_gatePropertyEnum; CONST value: T_gatePropertyValue);
  begin
    case prop of
      gpe_editableLabel: if gate^.gateType in [gt_input,gt_output] then begin
        P_inputGate(gate)^.ioLabel:=value.s;
      end;
      gpe_intervalGreaterZero: if gate^.gateType=gt_clock then begin
        P_clock(gate)^.interval:=value.n;
      end;
    end;
  end;

CONSTRUCTOR T_gatePropertyValues.create(CONST gate_: P_abstractGate);
  VAR p:T_gatePropertyEnum;
      i:longint=0;
  begin
    gate:=gate_;
    setLength(entry,0);
    for p in C_availableProperies[gate^.gateType] do begin
      setLength(entry,i+1);
      entry[i].prop:=p;
      entry[i].value:=fetchValue(p);
      inc(i);
    end;
  end;

DESTRUCTOR T_gatePropertyValues.destroy;
  begin
    setLength(entry,0);
  end;

FUNCTION T_gatePropertyValues.getSerialVersion: dword;
  begin
    result:=123;
  end;

FUNCTION T_gatePropertyValues.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
  begin
    for i:=0 to length(entry)-1 do if not(C_gateProperty[entry[i].prop].readonly) then begin
      case C_gateProperty[entry[i].prop].typ of
        pt_number: entry[i].value.n:=stream.readLongint;
        pt_string: entry[i].value.s:=stream.readAnsiString;
      end;
    end;
  end;

PROCEDURE T_gatePropertyValues.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i:longint;
  begin
    for i:=0 to length(entry)-1 do if not(C_gateProperty[entry[i].prop].readonly) then begin
      case C_gateProperty[entry[i].prop].typ of
        pt_number: stream.writeLongint   (entry[i].value.n);
        pt_string: stream.writeAnsiString(entry[i].value.s);
      end;
    end;
  end;

FUNCTION T_gatePropertyValues.count:longint;
  begin
    result:=length(entry);
  end;

FUNCTION T_gatePropertyValues.key(CONST index: longint): string;
  begin
    result:=C_gateProperty[entry[index].prop].name;
  end;

FUNCTION T_gatePropertyValues.value(CONST index: longint): string;
  begin
    case C_gateProperty[entry[index].prop].typ of
      pt_number: result:=intToStr(entry[index].value.n);
      pt_string: result:=         entry[index].value.s;
    end;
  end;

FUNCTION T_gatePropertyValues.acceptNewValue(CONST index: longint; CONST newValue: string): boolean;
  VAR newNumber:int64;
  begin
    if C_gateProperty[entry[index].prop].readonly then exit(false);
    case C_gateProperty[entry[index].prop].typ of
      pt_number: begin
        newNumber:=StrToInt64Def(newValue,int64(maxLongint)+1);
        if (newNumber>C_gateProperty[entry[index].prop].maxValue) or
           (newNumber<C_gateProperty[entry[index].prop].minValue) then exit(false);
        entry[index].value.n:=longint(newNumber);
        result:=true;
      end;
      pt_string: begin
        entry[index].value.s:=newValue;
        result:=true;
      end;
      else result:=false;
    end;
    if result then entry[index].modified:=true;
  end;

PROCEDURE T_gatePropertyValues.applyValues;
  VAR i:longint;
  begin
    for i:=0 to length(entry)-1 do
    with entry[i] do
    if modified
    then applyValue(prop,value);
  end;

end.
