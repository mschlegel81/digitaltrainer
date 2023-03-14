UNIT logicalGates;
{$mode objfpc}{$H+}
INTERFACE
USES serializationUtil;
CONST WIRE_MAX_WIDTH=16;
TYPE
  T_gateType=(gt_notGate,
              gt_andGate,
              gt_orGate,
              gt_xorGate,
              gt_nandGate,
              gt_norGate,
              gt_nxorGate,
              gt_input,
              gt_output,
              gt_compound,
              gt_clock,
              gt_adapter,
              gt_true,
              gt_false,
              gt_gatedClock,
              gt_undeterminedToTrue,
              gt_undeterminedToFalse,
              gt_ram,
              gt_rom,
              gt_7segmentDummy);
  T_gateTypeSet=array of T_gateType;
  T_gateCount=array[T_gateType] of longint;
  T_multibitWireRepresentation=(wr_binary,wr_decimal,wr_2complement);

CONST
  C_multibitWireRepresentationName:array[T_multibitWireRepresentation] of string=('bin','dec','2cmp');
  C_gateDefaultDescription:array [T_gateType] of string=
  {gt_notGate}   ('Logische Negation',
  {gt_andGate}    'Logisches und',
  {gt_orGate}     'Logisches oder (inklusiv)',
  {gt_xorGate}    'Logisches oder (exklusiv)',
  {gt_nandGate}   'Logisches nicht-und',
  {gt_norGate}    'Logisches nicht-oder',
  {gt_nxorGate}   'Logisches nicht-exklusiv-oder',
  {gt_input}      'Eingabe-Baustein',
  {gt_output}     'Ausgabe-Baustein',
  {gt_compound}   '<compound>',
  {gt_clock}      'Zeitgeber'+LineEnding+'Ausgangssignal wechselt periodisch',
  {gt_adapter}    'Ein Adapter für unterschiedliche Kabel-Breiten',
  {gt_true}       'Konstant 1',
  {gt_false}      'Konstant 0',
  {gt_gatedClock} 'Geschalter Zeitgeber'+LineEnding+'Ausgangssignal wechselt periodisch'+LineEnding+'falls am Eingang 1 anliegt',
  {gt_un....true} 'Gibt für unbestimmten Eingang 1 aus',
  {gt_un...false} 'Gibt für unbestimmten Eingang 0 aus',
                  'Speicher'+LineEnding+'Schreibt auf fallender Flanke von clk',
                  'Nur-Lese-Speicher',
                  '7-Segment-Anzeige');

  C_gateTypeName:array[T_gateType] of string=
    {gt_notGate}   ('not',
    {gt_andGate}    'and',
    {gt_orGate}     'or',
    {gt_xorGate}    'xor',
    {gt_nandGate}   'nand',
    {gt_norGate}    'nor',
    {gt_nxorGate}   'nxor',
    {gt_input}      'input',
    {gt_output}     'output',
    {gt_compound}   '<compound>',
    {gt_clock}      'clock',
    {gt_adapter}    'adapter',
    {gt_true}       'constant true',
    {gt_false}      'constant false',
    {gt_gatedClock} 'gated clock',
    {gt_un....true} 'tend to true',
    {gt_un...false} 'tend to false',
                    'ram',
                    'rom',
                    '7seg');

TYPE
  T_triStateValue=(tsv_false,tsv_undetermined,tsv_true);

  { T_ioLocations }

  T_ioLocation=record
    leftOrRight:boolean;
    positionIndex:longint;
    ioLabel:string;
  end;

  T_ioLocations=object
    numberOfLeftInputs,
    numberOfTopInputs,
    numberOfRightOutputs,
    numberOfBottomOutputs:longint;
    p:array[gt_input..gt_output] of array of T_ioLocation;
    PROCEDURE init;
    PROCEDURE add(CONST gt:T_gateType; CONST leftOrRight:boolean; CONST pos:longint; CONST lab:string);
  end;

  T_wireValue=record
    bit:array[0..WIRE_MAX_WIDTH-1] of T_triStateValue;
    width:byte;
  end;

  T_wireValueArray=array of T_wireValue;

  { T_captionedAndIndexed }

  P_captionedAndIndexed=^T_captionedAndIndexed;
  T_captionedAndIndexed=object

    FUNCTION  getCaption:shortstring;       virtual; abstract;
    FUNCTION  getDescription:string;   virtual; abstract;
    PROCEDURE setCaption(CONST s:string);       virtual;
    PROCEDURE setDescription(CONST s:string);   virtual;
    FUNCTION  getIndexInPalette:longint; virtual;
    FUNCTION  isVisualBoard:boolean; virtual;
  end;

  P_abstractGate=^T_abstractGate;

  { T_abstractGate }

  T_abstractGate=object(T_captionedAndIndexed)
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy; virtual;
      PROCEDURE reset;                   virtual; abstract;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate; virtual; abstract;
      FUNCTION  getCaption:shortstring;       virtual;
      FUNCTION  getDescription:string;   virtual;
      FUNCTION  numberOfInputs :longint; virtual; abstract;
      FUNCTION  numberOfOutputs:longint; virtual; abstract;
      FUNCTION  getIoLocations:T_ioLocations; virtual;
      FUNCTION  inputWidth (CONST index:longint):byte; virtual;
      FUNCTION  outputWidth(CONST index:longint):byte; virtual;
      FUNCTION  gateType:T_gateType;     virtual; abstract;
      FUNCTION  simulateStep:boolean;    virtual; abstract;
      FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual; abstract;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual; abstract;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual; abstract;

      PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false); virtual;
      PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;

      PROCEDURE countGates(VAR gateCount:T_gateCount); virtual;
      FUNCTION equals(CONST other:P_abstractGate):boolean; virtual;
      FUNCTION behaviorEquals(CONST other:P_abstractGate):boolean; virtual;
    end;

  { T_constantGate }
  P_constantGate=^T_constantGate;
  T_constantGate=object(T_abstractGate)
    private
      c:T_triStateValue;
    public
      CONSTRUCTOR create(CONST constantTrue:boolean);
      PROCEDURE reset;                   virtual;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate; virtual;
      FUNCTION  getCaption:shortstring; virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;
  end;

  T_gateConnector=object
    gate:P_abstractGate;
    index:longint;
    FUNCTION getOutputValue:T_wireValue;
    FUNCTION setInputValue(CONST v:T_wireValue):boolean;
    FUNCTION inputWidth:byte;
    FUNCTION outputWidth:byte;
  end;

  P_notGate=^T_notGate;
  T_notGate=object(T_abstractGate)
    private
      input,output:T_triStateValue;
    public
      CONSTRUCTOR create;
      PROCEDURE reset;                   virtual;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;
  end;

  P_inputGate=^T_inputGate;

  { T_inputGate }

  T_inputGate=object(T_abstractGate)
    private
      io:T_wireValue;
    public
      ioLabel:shortstring;
      ioIndex:longint;
      width:byte;

      onLeftOrRightSide:boolean;
      positionIndex:longint;
      CONSTRUCTOR create;
      PROCEDURE reset;                   virtual;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
      FUNCTION  getCaption:shortstring; virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  getIoLocations:T_ioLocations; virtual;
      FUNCTION  inputWidth (CONST index:longint):byte; virtual;
      FUNCTION  outputWidth(CONST index:longint):byte; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;

      PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false); virtual;
      PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
      FUNCTION  equals(CONST other:P_abstractGate):boolean; virtual;
  end;

  P_outputGate=^T_outputGate;
  T_outputGate=object(T_inputGate)
    public
      CONSTRUCTOR create;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
      FUNCTION  getCaption:shortstring; virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
  end;

  P_7segmentGate=^T_7segmentGate;

  { T_7segmentGate }

  T_7segmentGate=object(T_outputGate)
    CONSTRUCTOR create;
    FUNCTION  gateType:T_gateType; virtual;
    PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false); virtual;
    PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
    FUNCTION  equals(CONST other:P_abstractGate):boolean; virtual;
    FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
  end;

  P_adapter=^T_adapter;
  T_adapter=object(T_abstractGate)
    private
      inWidth,outWidth,inCount,outCount:byte;
      io:T_wireValue;
    public
      CONSTRUCTOR create(CONST inW,outW:byte);
      PROCEDURE reset;                   virtual;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
      FUNCTION  getCaption:shortstring; virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  inputWidth (CONST index:longint):byte; virtual;
      FUNCTION  outputWidth(CONST index:longint):byte; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;
      PROCEDURE setIoWidth(CONST inW,outW:byte);

      PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false); virtual;
      PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
      FUNCTION  equals(CONST other:P_abstractGate):boolean; virtual;
  end;

  P_binaryBaseGate=^T_binaryBaseGate;
  T_binaryBaseGate=object(T_abstractGate)
     private
       input:array[0..WIRE_MAX_WIDTH-1] of T_triStateValue;
       output:T_triStateValue;
     public
       inputCount:byte;
       CONSTRUCTOR create;
       PROCEDURE reset;                   virtual;
       FUNCTION  numberOfInputs :longint; virtual;
       FUNCTION  numberOfOutputs:longint; virtual;
       FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
       FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
       FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
       FUNCTION  getInput(CONST index:longint):T_wireValue;              virtual;

       PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false); virtual;
       PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
       FUNCTION  equals(CONST other:P_abstractGate):boolean; virtual;
   end;

   P_andGate=^T_andGate;
   T_andGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_orGate=^T_orGate;
   T_orGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_xorGate=^T_xorGate;
   T_xorGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_nandGate=^T_nandGate;
   T_nandGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_norGate=^T_norGate;
   T_norGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_nxorGate=^T_nxorGate;
   T_nxorGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_clock=^T_clock;
   T_clock=object(T_abstractGate)
      tick:boolean;
      interval,counter:longint;

      CONSTRUCTOR create;
      PROCEDURE reset;                   virtual;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
      FUNCTION  getCaption:shortstring; virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;

      PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false); virtual;
      PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
      FUNCTION  equals(CONST other:P_abstractGate):boolean; virtual;
    end;

   P_gatedClock=^T_gatedClock;
   T_gatedClock=object(T_clock)
      enable:T_triStateValue;
      CONSTRUCTOR create;
      PROCEDURE reset;                   virtual;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
      FUNCTION  getCaption:shortstring; virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;
   end;

   P_tendToTrue=^T_tendToTrue;
   T_tendToTrue=object(T_abstractGate)
     public
       input,output:T_wireValue;
       CONSTRUCTOR create;
       PROCEDURE reset;                   virtual;
       FUNCTION  getCaption:shortstring; virtual;
       FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
       FUNCTION  numberOfInputs :longint; virtual;
       FUNCTION  numberOfOutputs:longint; virtual;
       FUNCTION  gateType:T_gateType;     virtual;
       FUNCTION  simulateStep:boolean;    virtual;
       FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
       FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
       FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;
       PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false); virtual;
       PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
       FUNCTION  equals(CONST other:P_abstractGate):boolean; virtual;
   end;

   P_tendToFalse=^T_tendToFalse;
   T_tendToFalse=object(T_tendToTrue)
     public
       CONSTRUCTOR create;
       PROCEDURE reset;                   virtual;
       FUNCTION  getCaption:shortstring; virtual;
       FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
       FUNCTION  numberOfInputs :longint; virtual;
       FUNCTION  numberOfOutputs:longint; virtual;
       FUNCTION  gateType:T_gateType;     virtual;
       FUNCTION  simulateStep:boolean;    virtual;
   end;

   P_RomGate=^T_romGate;
   T_romGate=object(T_abstractGate)
     data:T_wireValueArray;
     decodedAdress:longint;
     readAdr:T_wireValue;

     CONSTRUCTOR create;
     DESTRUCTOR destroy; virtual;
     PROCEDURE reset;                   virtual;
     FUNCTION  clone(CONST includeState:boolean):P_abstractGate; virtual;
     FUNCTION  getCaption:shortstring;       virtual;
     FUNCTION  numberOfInputs :longint; virtual;
     FUNCTION  numberOfOutputs:longint; virtual;
     FUNCTION  getIoLocations:T_ioLocations; virtual;
     FUNCTION  inputWidth (CONST index:longint):byte; virtual;
     FUNCTION  outputWidth(CONST index:longint):byte; virtual;
     FUNCTION  gateType:T_gateType;     virtual;
     FUNCTION  simulateStep:boolean;    virtual;
     FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
     FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
     FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;
     PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false); virtual;
     PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
     FUNCTION equals(CONST other:P_abstractGate):boolean; virtual;
     FUNCTION behaviorEquals(CONST other:P_abstractGate):boolean; virtual;
   end;

   P_RamGate=^T_ramGate;

   { T_ramGate }

   T_ramGate=object(T_romGate)
     writeAdr,
     dataIn,
     clockIn:T_wireValue;
     clockWasHigh:dword;

     CONSTRUCTOR create;
     PROCEDURE reset;                   virtual;
     FUNCTION  clone(CONST includeState:boolean):P_abstractGate; virtual;
     FUNCTION  getCaption:shortstring;       virtual;
     FUNCTION  numberOfInputs :longint; virtual;
     FUNCTION  getIoLocations:T_ioLocations; virtual;
     FUNCTION  inputWidth (CONST index:longint):byte; virtual;
     FUNCTION  outputWidth(CONST index:longint):byte; virtual;
     FUNCTION  gateType:T_gateType;     virtual;
     FUNCTION  simulateStep:boolean;    virtual;
     FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
     FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
     FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;
     PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false); virtual;
     PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
     FUNCTION equals(CONST other:P_abstractGate):boolean; virtual;
     FUNCTION behaviorEquals(CONST other:P_abstractGate):boolean; virtual;
   end;

FUNCTION newBaseGate(CONST gateType:T_gateType):P_abstractGate;
OPERATOR =(CONST x,y:T_gateConnector):boolean;
OPERATOR :=(CONST x:T_triStateValue):T_wireValue;
FUNCTION isFullyDefined(CONST w:T_wireValue):boolean;
OPERATOR =(CONST x,y:T_wireValue):boolean;

FUNCTION getBinaryString     (CONST wire:T_wireValue):shortstring;
FUNCTION getDecimalString    (CONST wire:T_wireValue):shortstring;
FUNCTION get2ComplementString(CONST wire:T_wireValue):shortstring;
FUNCTION getWireString       (CONST wire:T_wireValue; CONST mode:T_multibitWireRepresentation):shortstring;

FUNCTION parseWireBin        (CONST s:string; CONST width:byte):T_wireValue;
FUNCTION parseWireDecimal    (CONST s:string; CONST width:byte):T_wireValue;
FUNCTION parseWire2Complement(CONST s:string; CONST width:byte):T_wireValue;
FUNCTION parseWire           (CONST s:string; CONST width:byte; CONST mode:T_multibitWireRepresentation):T_wireValue;

FUNCTION getDecimalValue     (CONST wire:T_wireValue; OUT valid:boolean):longint;
FUNCTION get2ComplementValue (CONST wire:T_wireValue; OUT valid:boolean):longint;

FUNCTION gatesTotal(CONST gateCount:T_gateCount):longint;

FUNCTION serialize(CONST wireValue: T_wireValue): qword;
FUNCTION deserialize(n: qword): T_wireValue;

IMPLEMENTATION
USES sysutils;
FUNCTION getBinaryString(CONST wire: T_wireValue): shortstring;
  VAR i:longint;
  begin
    result:='';
    for i:=wire.width-1 downto 0 do
    case wire.bit[i] of
      tsv_true        : result+='1';
      tsv_false       : result+='0';
      tsv_undetermined: result+='?';
    end;
  end;

FUNCTION getDecimalString(CONST wire: T_wireValue): shortstring;
  VAR i:longint;
      k:int64=0;
  begin
    for i:=wire.width-1 downto 0 do begin
      k:=k shl 1;
      case wire.bit[i] of
        tsv_true        : inc(k);
        tsv_false       : begin end;
        tsv_undetermined: exit('?');
      end;
    end;
    result:=intToStr(k);
  end;

FUNCTION getDecimalValue(CONST wire: T_wireValue; OUT valid: boolean): longint;
  VAR i:longint;
      k:int64=0;
  begin
    valid:=true;
    for i:=wire.width-1 downto 0 do begin
      k:=k shl 1;
      case wire.bit[i] of
        tsv_true        : inc(k);
        //tsv_false       : begin end;
        tsv_undetermined: valid:=false;
      end;
    end;
    result:=k;
  end;

FUNCTION get2ComplementString(CONST wire: T_wireValue): shortstring;
  VAR i:longint;
      k:int64=0;
      maxVal:int64;
  begin
    for i:=wire.width-1 downto 0 do begin
      k:=k shl 1;
      case wire.bit[i] of
        tsv_true        : inc(k);
        tsv_false       : begin end;
        tsv_undetermined: exit('?');
      end;
    end;
    if (wire.width>1) then begin
      maxVal:=1 shl (wire.width-1);
      if k>maxVal then k-=maxVal+maxVal;
    end;
    result:=intToStr(k);
  end;

FUNCTION getWireString(CONST wire: T_wireValue; CONST mode: T_multibitWireRepresentation): shortstring;
  begin
    case mode of
      wr_binary     : result:=getBinaryString(wire);
      wr_decimal    : result:=getDecimalString(wire);
      wr_2complement: result:=get2ComplementString(wire);
    end;
  end;

FUNCTION get2ComplementValue(CONST wire: T_wireValue; OUT valid: boolean): longint;
  VAR i:longint;
      k:int64=0;
      maxVal:int64;
  begin
    valid:=true;
    for i:=wire.width-1 downto 0 do begin
      k:=k shl 1;
      case wire.bit[i] of
        tsv_true        : inc(k);
        tsv_false       : begin end;
        tsv_undetermined: begin
          valid:=false;
          exit(0);
        end;
      end;
    end;
    if (wire.width>1) then begin
      maxVal:=1 shl (wire.width-1);
      if k>maxVal then k-=maxVal+maxVal;
    end;
    result:=k;
  end;

FUNCTION serialize(CONST wireValue: T_wireValue): qword;
  CONST BYTE_OF:array[T_triStateValue] of byte=(0,1,2);
  VAR i:longint;
  begin
    result:=0;
    for i:=wireValue.width-1 downto 0 do
      result:=result*3+BYTE_OF[wireValue.bit[i]];
    result*=4;
    case wireValue.width of
      1: result+=0;
      4: result+=1;
      8: result+=2;
     16: result+=3;
    else assert(false);
    end;
  end;

FUNCTION deserialize(n: qword): T_wireValue;
  CONST WIRE_VALUE_OF:array[0..2] of T_triStateValue=(tsv_false,tsv_undetermined,tsv_true);
  VAR i: integer;
  begin
    initialize(result);
    case byte(n and 3) of
      0: result.width:=1;
      1: result.width:=4;
      2: result.width:=8;
      3: result.width:=16;
    end;
    n:=n div 4;
    for i:=0 to result.width-1 do begin
      result.bit[i]:=WIRE_VALUE_OF[n mod 3];
      n:=n div 3;
    end;
  end;

FUNCTION gatesTotal(CONST gateCount: T_gateCount): longint;
  VAR gt:T_gateType;
  begin
    result:=0;
    for gt in T_gateType do result+=gateCount[gt];
  end;

FUNCTION parseWireBin(CONST s: string; CONST width: byte): T_wireValue;
  VAR i:longint;
      k:longint;
  begin
    result.width:=width;
    for i:=0 to WIRE_MAX_WIDTH-1 do result.bit[i]:=tsv_false;
    k:=length(s)-1;
    if k>=WIRE_MAX_WIDTH then k:=WIRE_MAX_WIDTH-1;
    for i:=0 to k do case s[length(s)-i] of
      '1': result.bit[i]:=tsv_true;
      '0': result.bit[i]:=tsv_false;
    end;
  end;

FUNCTION parseWireDecimal(CONST s: string; CONST width: byte): T_wireValue;
  VAR n:int64;
      i:longint;
  begin
    result.width:=width;
    n:=StrToInt64Def(s,-1);
    if n<0 then n:=0;
    for i:=0 to WIRE_MAX_WIDTH-1 do begin
      if odd(n) then result.bit[i]:=tsv_true
                else result.bit[i]:=tsv_false;
      n:=n shr 1;
    end;
  end;

FUNCTION parseWire2Complement(CONST s: string; CONST width: byte): T_wireValue;
  VAR n:int64;
      i:longint;
      maxVal:int64;
  begin
    result.width:=width;
    n:=StrToInt64Def(s,maxLongint+1);
    if (n>maxLongint) or (n<-65536) then n:=0;

    if (width>1) then begin
      maxVal:=1 shl width;
      while (n<0) do n+=maxVal;
    end;
    for i:=0 to WIRE_MAX_WIDTH-1 do begin
      if odd(n) then result.bit[i]:=tsv_true
                else result.bit[i]:=tsv_false;
      n:=n shr 1;
    end;
  end;

FUNCTION parseWire(CONST s: string; CONST width: byte; CONST mode: T_multibitWireRepresentation): T_wireValue;
  begin
    initialize(result);
    case mode of
      wr_binary     : result:=parseWireBin(s,width);
      wr_decimal    : result:=parseWireDecimal(s,width);
      wr_2complement: result:=parseWire2Complement(s,width);
    end;
  end;

OPERATOR=(CONST x, y: T_gateConnector): boolean;
  begin
    result:=(x.gate=y.gate) and (x.index=y.index);
  end;

FUNCTION newBaseGate(CONST gateType: T_gateType): P_abstractGate;
  begin
    case gateType of
      gt_notGate : new(P_notGate   (result),create);
      gt_andGate : new(P_andGate   (result),create);
      gt_orGate  : new(P_orGate    (result),create);
      gt_xorGate : new(P_xorGate   (result),create);
      gt_nandGate: new(P_nandGate  (result),create);
      gt_norGate : new(P_norGate   (result),create);
      gt_nxorGate: new(P_nxorGate  (result),create);
      gt_input   : new(P_inputGate (result),create);
      gt_output  : new(P_outputGate(result),create);
      gt_clock   : new(P_clock     (result),create);
      gt_adapter : new(P_adapter(result),create(4,1));
      gt_true :new(P_constantGate(result),create(true ));
      gt_false:new(P_constantGate(result),create(false));
      gt_gatedClock: new(P_gatedClock(result),create);
      gt_undeterminedToFalse: new(P_tendToFalse(result),create);
      gt_undeterminedToTrue : new(P_tendToTrue (result),create);
      gt_ram: new(P_RamGate(result),create);
      gt_rom: new(P_RomGate(result),create);
      gt_7segmentDummy: new(P_7segmentGate(result),create);
      else result:=nil;
    end;
  end;

{ T_7segmentGate }

CONSTRUCTOR T_7segmentGate.create;
  begin
    inherited;
    io:=tsv_false; ioLabel:=''; width:=8; onLeftOrRightSide:=false; positionIndex:=0;
  end;

FUNCTION T_7segmentGate.gateType: T_gateType;
  begin
    result:=gt_7segmentDummy;
  end;

PROCEDURE T_7segmentGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST metaDataOnly: boolean);
  begin
    if not(metaDataOnly) then stream.writeByte(byte(gateType));
  end;

PROCEDURE T_7segmentGate.readMetaDataFromStream(VAR stream: T_bufferedInputStreamWrapper);
  begin
  end;

FUNCTION T_7segmentGate.equals(CONST other: P_abstractGate): boolean;
  begin
    result:=other^.gateType=gateType;
  end;

FUNCTION T_7segmentGate.clone(CONST includeState: boolean): P_abstractGate;
  begin
    new(P_7segmentGate(result),create);
  end;

{ T_ramGate }

CONSTRUCTOR T_ramGate.create;
  begin
    inherited;
  end;

PROCEDURE T_ramGate.reset;
  VAR i:longint;
  begin
    inherited;
    writeAdr.width:=16;
    dataIn  .width:=16;
    for i:=0 to WIRE_MAX_WIDTH-1 do writeAdr.bit[i]:=tsv_undetermined;
    clockIn.width:=1;
    clockIn.bit[0]:=tsv_undetermined;
    setLength(data,0);
    clockWasHigh:=1431655765;
  end;

FUNCTION T_ramGate.clone(CONST includeState: boolean): P_abstractGate;
  VAR i:longint;
  begin
    new(P_RamGate(result),create);

    setLength(P_RamGate(result)^.data,length(data));
    for i:=0 to length(data)-1 do P_RomGate(result)^.data[i]:=data[i];
    P_RomGate(result)^.readAdr:=readAdr;
  end;

FUNCTION T_ramGate.getCaption: shortstring;
  begin
    result:='RAM';
  end;

FUNCTION T_ramGate.numberOfInputs: longint;
  begin
    result:=4;
  end;

FUNCTION T_ramGate.getIoLocations: T_ioLocations;
  begin
    result.init;
    result.add(gt_input,true,0,'readAdr');
    result.add(gt_input,true,1,'writeAdr');
    result.add(gt_input,true,2,'data');
    result.add(gt_input,true,3,'clk');
    result.add(gt_output,true,0,'data');
  end;

FUNCTION T_ramGate.inputWidth(CONST index: longint): byte;
  begin
    if index=3 then result:=1 else result:=16;
  end;

FUNCTION T_ramGate.outputWidth(CONST index: longint): byte;
  begin
    result:=16;
  end;

FUNCTION T_ramGate.gateType: T_gateType;
  begin
    result:=gt_ram;
  end;

CONST zero:T_wireValue=(bit:(tsv_false,tsv_false,tsv_false,tsv_false,
                             tsv_false,tsv_false,tsv_false,tsv_false,
                             tsv_false,tsv_false,tsv_false,tsv_false,
                             tsv_false,tsv_false,tsv_false,tsv_false); width:16);

FUNCTION T_ramGate.simulateStep: boolean;
  VAR writeIndex:longint;
      i0,i:longint;
  begin
    if (clockWasHigh=4294967295) and (clockIn.bit[0]=tsv_false) and isFullyDefined(dataIn) then begin
      writeIndex:=getDecimalValue(writeAdr,result);
      result:=result and (writeIndex>=0) and (writeIndex<=65535);
      if result then begin
        i0:=length(data);
        if writeIndex>=i0 then begin
          setLength(data,writeIndex+1);
          for i:=i0 to writeIndex-1 do data[i]:=zero;
        end;
        data[writeIndex]:=dataIn;
      end;
    end else result:=(clockWasHigh<>4294967295) and (clockWasHigh<>0);
    clockWasHigh:=clockWasHigh shl 1;
    if clockIn.bit[0]=tsv_true then clockWasHigh:=clockWasHigh or 1;
    if inherited then result:=true;
  end;

FUNCTION T_ramGate.getOutput(CONST index: longint): T_wireValue;
  begin
    result:=inherited;
  end;

FUNCTION T_ramGate.setInput(CONST index: longint; CONST value: T_wireValue): boolean;
  begin
    if (index<0) or (index>3) then result:=false
    else case byte(index) of
      0: begin
           result:=readAdr<>value;
           readAdr:=value;
         end;
      1: begin
           result:=writeAdr<>value;
           writeAdr:=value;
         end;
      2: begin
           result:=dataIn<>value;
           dataIn:=value;
         end;
      3: begin
           result:=clockIn<>value;
           clockIn:=value;
         end;
    end;
  end;

FUNCTION T_ramGate.getInput(CONST index: longint): T_wireValue;
  begin
    case byte(index) of
      0: result:=readAdr;
      1: result:=writeAdr;
      2: result:=dataIn;
      3: result:=clockIn;
    end;
  end;

PROCEDURE T_ramGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST metaDataOnly: boolean);
  begin
    if not(metaDataOnly) then stream.writeByte(byte(gateType));
  end;

PROCEDURE T_ramGate.readMetaDataFromStream( VAR stream: T_bufferedInputStreamWrapper);
  begin
    //Mo meta data
  end;

FUNCTION T_ramGate.equals(CONST other: P_abstractGate): boolean;
  begin
    result:=other^.gateType=gateType;
  end;

FUNCTION T_ramGate.behaviorEquals(CONST other: P_abstractGate): boolean;
  begin
    result:=other^.gateType=gateType;
  end;

{ T_romGate }

CONSTRUCTOR T_romGate.create;
  begin
    inherited;
    initialize(data);
    reset;
  end;

DESTRUCTOR T_romGate.destroy;
  begin
    setLength(data,0);
    inherited;
  end;

PROCEDURE T_romGate.reset;
  VAR i:byte;
  begin
    readAdr.width:=16;
    for i:=0 to WIRE_MAX_WIDTH-1 do readAdr.bit[i]:=tsv_undetermined;
  end;

FUNCTION T_romGate.clone(CONST includeState: boolean): P_abstractGate;
  VAR i:longint;
  begin
    new(P_RomGate(result),create);
    setLength(P_RomGate(result)^.data,length(data));
    for i:=0 to length(data)-1 do P_RomGate(result)^.data[i]:=data[i];
    P_RomGate(result)^.readAdr:=readAdr;
  end;

FUNCTION T_romGate.getCaption: shortstring;
  begin
    result:='ROM';
  end;

FUNCTION T_romGate.numberOfInputs: longint;
  begin
    result:=1;
  end;

FUNCTION T_romGate.numberOfOutputs: longint;
  begin
    result:=1;
  end;

FUNCTION T_romGate.getIoLocations: T_ioLocations;
  begin
    result.init;
    result.add(gt_input,true,0,'readAdr');
    result.add(gt_output,true,0,'data');
  end;

FUNCTION T_romGate.inputWidth(CONST index: longint): byte;
  begin
    result:=16;
  end;

FUNCTION T_romGate.outputWidth(CONST index: longint): byte;
  begin
    result:=16;
  end;

FUNCTION T_romGate.gateType: T_gateType;
  begin
    result:=gt_rom;
  end;

FUNCTION T_romGate.simulateStep: boolean;
  VAR newAdress:longint;
  begin
    newAdress:=getDecimalValue(readAdr,result);
    result:=result and (newAdress<>decodedAdress);
    decodedAdress:=newAdress;
  end;

FUNCTION T_romGate.getOutput(CONST index: longint): T_wireValue;
  begin
    if (decodedAdress>=0) and (decodedAdress<length(data))
    then result:=data[decodedAdress]
    else result:=zero;
  end;

FUNCTION T_romGate.setInput(CONST index: longint; CONST value: T_wireValue): boolean;
  begin
    result:=value<>readAdr;
    readAdr:=value;
  end;

FUNCTION T_romGate.getInput(CONST index: longint): T_wireValue;
  begin
    result:=readAdr;
  end;

PROCEDURE T_romGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST metaDataOnly: boolean);
  VAR i:longint;
  begin
    inherited;
    stream.writeNaturalNumber(length(data));
    for i:=0 to length(data)-1 do stream.writeNaturalNumber(serialize(data[i]));
  end;

PROCEDURE T_romGate.readMetaDataFromStream(VAR stream: T_bufferedInputStreamWrapper);
  VAR i:longint;
  begin
    i:=stream.readNaturalNumber;
    if (i>65536) or (i<0) then begin
      stream.logWrongTypeError;
      exit;
    end;
    setLength(data,i);
    for i:=0 to length(data)-1 do data[i]:=deserialize(stream.readNaturalNumber);
  end;

FUNCTION T_romGate.equals(CONST other: P_abstractGate): boolean;
  VAR i:longint;
  begin
    if (other^.gateType<>gateType) then exit(false);
    if (length(data)<>length(P_RomGate(other)^.data)) then exit(false);
    for i:=0 to length(data)-1 do if data[i]<>P_RomGate(other)^.data[i] then exit(false);
    result:=true;
  end;

FUNCTION T_romGate.behaviorEquals(CONST other: P_abstractGate): boolean;
  begin
    result:=equals(other);
  end;

{ T_ioLocations }

PROCEDURE T_ioLocations.init;
  begin
    numberOfLeftInputs   :=0;
    numberOfTopInputs    :=0;
    numberOfRightOutputs :=0;
    numberOfBottomOutputs:=0;
    setLength(p[gt_input ],0);
    setLength(p[gt_output],0);
  end;

PROCEDURE T_ioLocations.add(CONST gt: T_gateType; CONST leftOrRight: boolean; CONST pos: longint; CONST lab: string);
  VAR i:longint;
  begin
    i:=length(p[gt]);
    setLength(p[gt],i+1);
    p[gt,i].leftOrRight  :=leftOrRight;
    p[gt,i].positionIndex:=pos;
    p[gt,i].ioLabel      :=lab;
    case gt of
      gt_input:
        if leftOrRight
        then inc(numberOfLeftInputs)
        else inc(numberOfTopInputs);
      gt_output:
        if leftOrRight
        then inc(numberOfRightOutputs)
        else inc(numberOfBottomOutputs);
    end;
  end;

{ T_captionedAndIndexed }

PROCEDURE T_captionedAndIndexed.setCaption(CONST s: string);
  begin
  end;

PROCEDURE T_captionedAndIndexed.setDescription(CONST s: string);
  begin
  end;

FUNCTION T_captionedAndIndexed.getIndexInPalette: longint;
  begin
    result:=-1;
  end;

FUNCTION T_captionedAndIndexed.isVisualBoard: boolean;
  begin
    result:=false;
  end;

CONSTRUCTOR T_tendToFalse.create;
  begin inherited; end;

PROCEDURE T_tendToFalse.reset;
  VAR i:longint;
  begin
    for i:=0 to input.width-1 do input.bit[i]:=tsv_undetermined;
    for i:=0 to input.width-1 do output.bit[i]:=tsv_false;
  end;

FUNCTION T_tendToFalse.getCaption: shortstring;
  begin
    result:='½→0';
  end;

FUNCTION T_tendToFalse.clone(CONST includeState: boolean): P_abstractGate;
  begin
    new(P_tendToFalse(result),create);
    P_tendToFalse(result)^.input:=input;
    P_tendToFalse(result)^.output:=output;
    if not includeState then result^.reset;
  end;

FUNCTION T_tendToFalse.numberOfInputs: longint;
  begin result:=1; end;

FUNCTION T_tendToFalse.numberOfOutputs: longint;
  begin result:=1; end;

FUNCTION T_tendToFalse.gateType: T_gateType;
  begin
    result:=gt_undeterminedToFalse;
  end;

FUNCTION T_tendToFalse.simulateStep: boolean;
  VAR i:longint;
      newOut:T_wireValue;
  begin
    newOut:=output;
    for i:=0 to input.width-1 do
    if input.bit[i]=tsv_true then newOut.bit[i]:=tsv_true
                             else newOut.bit[i]:=tsv_false;
    result:=output<>newOut;
    output:=newOut;
  end;

{ T_tendToTrue }

CONSTRUCTOR T_tendToTrue.create;
  begin inherited; reset; input.width:=1; output.width:=1; end;

PROCEDURE T_tendToTrue.reset;
  VAR i:longint;
  begin
    for i:=0 to input.width-1 do input.bit[i]:=tsv_undetermined;
    for i:=0 to input.width-1 do output.bit[i]:=tsv_true;
  end;

FUNCTION T_tendToTrue.getCaption: shortstring;
  begin
    result:='½→1';
  end;

FUNCTION T_tendToTrue.clone(CONST includeState: boolean): P_abstractGate;
  begin
    new(P_tendToTrue(result),create);
    P_tendToTrue(result)^.input:=input;
    P_tendToTrue(result)^.output:=output;
    if not includeState then result^.reset;
  end;

FUNCTION T_tendToTrue.numberOfInputs: longint;
  begin
    result:=1;
  end;

FUNCTION T_tendToTrue.numberOfOutputs: longint;
  begin
    result:=1;
  end;

FUNCTION T_tendToTrue.gateType: T_gateType;
  begin
    result:=gt_undeterminedToTrue;
  end;

FUNCTION T_tendToTrue.simulateStep: boolean;
  VAR i:longint;
      newOut:T_wireValue;
  begin
    newOut:=output;
    for i:=0 to input.width-1 do
    if input.bit[i]=tsv_false then newOut.bit[i]:=tsv_false
                              else newOut.bit[i]:=tsv_true;
    result:=output<>newOut;
    output:=newOut;
  end;

FUNCTION T_tendToTrue.getOutput(CONST index: longint): T_wireValue;
  begin
    result:=output;
  end;

FUNCTION T_tendToTrue.setInput(CONST index: longint; CONST value: T_wireValue): boolean;
  begin
    result:=value<>input;
    input:=value;
  end;

FUNCTION T_tendToTrue.getInput(CONST index: longint): T_wireValue;
  begin
    result:=input;
  end;

PROCEDURE T_tendToTrue.writeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false);
  begin
    inherited;
    stream.writeByte(input.width);
  end;

PROCEDURE T_tendToTrue.readMetaDataFromStream(VAR stream: T_bufferedInputStreamWrapper);
  begin
    input.width:=stream.readByte;
    output.width:=input.width;
  end;

{ T_gatedClock }

CONSTRUCTOR T_gatedClock.create;
  begin
    inherited;
  end;

PROCEDURE T_gatedClock.reset;
  begin
    inherited;
    enable:=tsv_undetermined;
  end;

FUNCTION T_gatedClock.getCaption: shortstring;
  begin
    result:='G'+inherited;
  end;

FUNCTION T_gatedClock.numberOfInputs: longint;
  begin
    result:=1;
  end;

FUNCTION T_gatedClock.gateType: T_gateType;
  begin
    result:=gt_gatedClock;
  end;

FUNCTION T_gatedClock.simulateStep: boolean;
  begin
    if enable<>tsv_false
    then result:=inherited
    else result:=false;
  end;

FUNCTION T_gatedClock.setInput(CONST index: longint; CONST value: T_wireValue): boolean;
  begin
    result:=
    enable<>value.bit[0];
    enable:=value.bit[0];
  end;

FUNCTION T_gatedClock.getInput(CONST index: longint): T_wireValue;
  begin
    result.width:=1;
    result.bit[0]:=enable;
  end;

{ T_constantGate }

CONSTRUCTOR T_constantGate.create(CONST constantTrue: boolean);
  begin
    inherited create;
    if constantTrue then c:=tsv_true else c:=tsv_false;
  end;

PROCEDURE T_constantGate.reset;
  begin end;

FUNCTION T_constantGate.getCaption: shortstring;
  begin
    case c of
      tsv_true : result:='1';
      tsv_false: result:='0';
      else       result:='?';
    end;
  end;

FUNCTION T_constantGate.numberOfInputs: longint;
  begin result:=0; end;

FUNCTION T_constantGate.numberOfOutputs: longint;
  begin result:=1; end;

FUNCTION T_constantGate.gateType: T_gateType;
  begin
    if c=tsv_true then result:=gt_true else result:=gt_false;
  end;

FUNCTION T_constantGate.simulateStep: boolean;
  begin result:=false; end;

FUNCTION T_constantGate.getOutput(CONST index: longint): T_wireValue;
  begin result:=c; end;

FUNCTION T_constantGate.setInput(CONST index: longint; CONST value: T_wireValue): boolean;
  begin result:=false; end;

FUNCTION T_constantGate.getInput(CONST index: longint): T_wireValue;
  begin
    result.width:=1;
    result.bit[0]:=tsv_undetermined;
  end;

{ T_adapter }

CONSTRUCTOR T_adapter.create(CONST inW, outW: byte);
  begin
    setIoWidth(inW,outW);
    reset;
  end;

PROCEDURE T_adapter.reset;
  VAR i:byte;
  begin
    for i:=0 to WIRE_MAX_WIDTH-1 do io.bit[i]:=tsv_undetermined;
  end;

FUNCTION T_adapter.getCaption: shortstring;
  begin result:='Adapter'+LineEnding+intToStr(inWidth)+'bit -> '+intToStr(outWidth)+'bit'; end;

FUNCTION T_adapter.numberOfInputs: longint;
  begin result:=inCount; end;

FUNCTION T_adapter.numberOfOutputs: longint;
  begin result:=outCount; end;

FUNCTION T_adapter.inputWidth(CONST index: longint): byte;
  begin result:=inWidth; end;

FUNCTION T_adapter.outputWidth(CONST index: longint): byte;
  begin result:=outWidth; end;

FUNCTION T_adapter.gateType: T_gateType;
  begin result:=gt_adapter; end;

FUNCTION T_adapter.simulateStep: boolean;
  begin result:=false; end;

FUNCTION T_adapter.getOutput(CONST index: longint): T_wireValue;
  VAR k0,k:longint;
  begin
    if outCount=1 then result:=io
    else begin
      result.width:=outWidth;
      k0:=outWidth*index;
      for k:=0 to result.width-1 do result.bit[k]:=io.bit[k+k0];
    end;
  end;

FUNCTION T_adapter.setInput(CONST index: longint; CONST value: T_wireValue): boolean;
  VAR k0,k:longint;
  begin
    if inCount=1 then begin
      result:=
      io<>value;
      io:=value;
    end else begin
      k0:=inWidth*index;
      result:=false;
      for k:=0 to inWidth-1 do begin
        result:=result or
       (io.bit[k+k0]<>value.bit[k]);
        io.bit[k+k0]:=value.bit[k]
      end;
    end;
  end;

FUNCTION T_adapter.getInput(CONST index: longint): T_wireValue;
  VAR k0,k:longint;
  begin
    if inCount=1 then result:=io
    else begin
      result.width:=inWidth;
      k0:=inWidth*index;
      for k:=0 to inWidth-1 do
        result.bit[k]:=io.bit[k+k0];
    end;
  end;

PROCEDURE T_adapter.setIoWidth(CONST inW, outW: byte);
  begin
    inWidth:=inW;
    outWidth:=outW;
    if inWidth>outWidth then begin
      inCount:=1;
      io.width:=inWidth;
      outCount:=inWidth div outWidth;
    end else begin
      outCount:=1;
      io.width:=outWidth;
      inCount:=outWidth div inWidth;
    end;
  end;

PROCEDURE T_adapter.writeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false);
  begin
    inherited;
    stream.writeByte(inWidth);
    stream.writeByte(outWidth);
  end;

PROCEDURE T_adapter.readMetaDataFromStream(VAR stream: T_bufferedInputStreamWrapper);
  VAR iw,ow:byte;
  begin
    iw:=stream.readByte;
    ow:=stream.readByte;
    setIoWidth(iw,ow);
  end;

{ T_clock }

CONSTRUCTOR T_clock.create;
  begin
    inherited;
    tick:=true;
    interval:=50;
    counter:=0;
  end;

PROCEDURE T_clock.reset;
  begin
    tick:=false;
    counter:=0;
  end;

FUNCTION T_clock.getCaption: shortstring;
  begin
    result:=#240#159#149#145;
  end;

FUNCTION T_clock.numberOfInputs: longint;
  begin result:=0; end;

FUNCTION T_clock.numberOfOutputs: longint;
  begin result:=1; end;

FUNCTION T_clock.gateType: T_gateType;
  begin result:=gt_clock; end;

FUNCTION T_clock.simulateStep: boolean;
  begin
    inc(counter);
    if counter>=interval then begin
      tick:=not(tick); counter:=0;
    end;
    result:=true;
  end;

OPERATOR:=(CONST x: T_triStateValue): T_wireValue;
  VAR i:longint;
  begin
    result.width:=1;
    result.bit[0]:=x;
    for i:=1 to WIRE_MAX_WIDTH-1 do result.bit[i]:=tsv_undetermined;
  end;

FUNCTION isFullyDefined(CONST w: T_wireValue): boolean;
  VAR i:longint;
  begin
    result:=true;
    for i:=0 to w.width-1 do if not(w.bit[i] in [tsv_true,tsv_false]) then exit(false);
  end;

OPERATOR=(CONST x, y: T_wireValue): boolean;
  VAR i:longint;
  begin
    result:=x.width=y.width;
    for i:=0 to x.width-1 do result:=result and (x.bit[i]=y.bit[i]);
  end;

FUNCTION T_clock.getOutput(CONST index: longint): T_wireValue;
  CONST T:array[false..true] of T_triStateValue=(tsv_false,tsv_true);
  begin result:=T[tick]; end;

FUNCTION T_clock.setInput(CONST index: longint; CONST value: T_wireValue):boolean;
  begin
    result:=false;
  end;

FUNCTION T_clock.getInput(CONST index: longint): T_wireValue;
  begin result:=tsv_undetermined; end;

PROCEDURE T_clock.writeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false);
  begin
    inherited;
    stream.writeNaturalNumber(interval);
  end;

PROCEDURE T_clock.readMetaDataFromStream(VAR stream: T_bufferedInputStreamWrapper);
  begin
    interval:=stream.readNaturalNumber;
  end;

{ T_outputGate }

CONSTRUCTOR T_outputGate.create;
  begin inherited; end;

FUNCTION T_outputGate.getCaption: shortstring;
  begin
    if ioLabel=''
    then result:='out '+intToStr(ioIndex)
    else result:=ioLabel;
  end;

FUNCTION T_outputGate.numberOfInputs: longint;
  begin result:=1; end;

FUNCTION T_outputGate.numberOfOutputs: longint;
  begin result:=0; end;

FUNCTION T_outputGate.gateType: T_gateType;
  begin result:=gt_output; end;

{ T_inputGate }

CONSTRUCTOR T_inputGate.create;
  begin inherited; io:=tsv_true; ioLabel:=''; width:=1; onLeftOrRightSide:=true; positionIndex:=0; end;

PROCEDURE T_inputGate.reset;
  VAR i:longint;
  begin
    io.width:=width;
    for i:=0 to WIRE_MAX_WIDTH-1 do io.bit[i]:=tsv_undetermined;
  end;

FUNCTION T_inputGate.getCaption: shortstring;
  begin
    if ioLabel=''
    then result:='in '+intToStr(ioIndex)
    else result:=ioLabel;
  end;

FUNCTION T_inputGate.numberOfInputs: longint;
  begin result:=0; end;

FUNCTION T_inputGate.numberOfOutputs: longint;
  begin result:=1; end;

FUNCTION T_inputGate.getIoLocations: T_ioLocations;
  VAR i:longint;
  begin
    result.init;
    for i:=0 to numberOfInputs-1  do result.add(gt_input ,onLeftOrRightSide,i,'');
    for i:=0 to numberOfOutputs-1 do result.add(gt_output,onLeftOrRightSide,i,'');
  end;

FUNCTION T_inputGate.inputWidth(CONST index: longint): byte;
  begin
    result:=width;
  end;

FUNCTION T_inputGate.outputWidth(CONST index: longint): byte;
  begin
    result:=width;
  end;

FUNCTION T_inputGate.gateType: T_gateType;
  begin result:=gt_input; end;

FUNCTION T_inputGate.simulateStep: boolean;
  begin
    result:=false;
  end;

FUNCTION T_inputGate.getOutput(CONST index: longint): T_wireValue;
  begin result:=io; end;

FUNCTION T_inputGate.setInput(CONST index: longint; CONST value: T_wireValue): boolean;
  begin
    result:=io<>value;
    io:=value;
  end;

FUNCTION T_inputGate.getInput(CONST index: longint): T_wireValue;
  begin result:=io; end;

PROCEDURE T_inputGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false);
  begin
    inherited;
    stream.writeShortString(ioLabel);
    stream.writeNaturalNumber(ioIndex);
    stream.writeByte(width);
    stream.writeBoolean(onLeftOrRightSide);
    stream.writeNaturalNumber(positionIndex);
  end;

PROCEDURE T_inputGate.readMetaDataFromStream(
  VAR stream: T_bufferedInputStreamWrapper);
  begin
    ioLabel:=stream.readShortString;
    ioIndex:=stream.readNaturalNumber;
    width:=stream.readByte;
    onLeftOrRightSide:=stream.readBoolean;
    positionIndex:=stream.readNaturalNumber;
  end;

{ T_abstractGate }

CONSTRUCTOR T_abstractGate.create;
  begin end; //pro forma

DESTRUCTOR T_abstractGate.destroy;
  begin end; //pro forma

FUNCTION T_abstractGate.getDescription: string;
  begin result:=C_gateDefaultDescription[gateType]; end;

FUNCTION T_abstractGate.getIoLocations: T_ioLocations;
  VAR i:longint;
  begin
    result.init;
    for i:=0 to numberOfInputs-1  do result.add(gt_input,true,i,'');
    for i:=0 to numberOfOutputs-1 do result.add(gt_output,true,i,'');
  end;

FUNCTION T_abstractGate.inputWidth(CONST index: longint): byte;
  begin result:=1; end;

FUNCTION T_abstractGate.outputWidth(CONST index: longint): byte;
  begin result:=1; end;

PROCEDURE T_abstractGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false);
  begin
    if not(metaDataOnly) then stream.writeByte(byte(gateType));
  end;

PROCEDURE T_abstractGate.readMetaDataFromStream(
  VAR stream: T_bufferedInputStreamWrapper);
  begin
  end;

FUNCTION T_gateConnector.getOutputValue: T_wireValue;
  begin
    result:=gate^.getOutput(index);
  end;

FUNCTION T_gateConnector.setInputValue(CONST v: T_wireValue):boolean;
  begin
    result:=gate^.setInput(index,v);
  end;

FUNCTION T_gateConnector.inputWidth:byte;
  begin
    result:=gate^.inputWidth(index);
  end;

FUNCTION T_gateConnector.outputWidth:byte;
  begin
    result:=gate^.outputWidth(index);
  end;

CONSTRUCTOR T_notGate.create;
  begin inherited; input:=tsv_undetermined; output:=tsv_undetermined; end;

PROCEDURE T_notGate.reset;
  begin input:=tsv_undetermined; output:=tsv_undetermined; end;

FUNCTION T_abstractGate.getCaption: shortstring;
  begin result:=C_gateTypeName[gateType]; end;

FUNCTION T_notGate.numberOfInputs: longint;
  begin  result:=1; end;

FUNCTION T_notGate.numberOfOutputs: longint;
  begin result:=1; end;

FUNCTION T_notGate.gateType: T_gateType;
  begin result:=gt_notGate; end;

CONST NOT_TABLE:array[T_triStateValue] of T_triStateValue=(tsv_true,tsv_undetermined,tsv_false);
      OR_TABLE :array[T_triStateValue,T_triStateValue] of T_triStateValue=
                   //0                ?                1
                {0}((tsv_false       ,tsv_undetermined,tsv_true),
                {?} (tsv_undetermined,tsv_undetermined,tsv_true),
                {1} (tsv_true        ,tsv_true        ,tsv_true));
      XOR_TABLE:array[T_triStateValue,T_triStateValue] of T_triStateValue=
                   //0                ?                1
                {0}((tsv_false       ,tsv_undetermined,tsv_true        ),
                {?} (tsv_undetermined,tsv_undetermined,tsv_undetermined),
                {1} (tsv_true        ,tsv_undetermined,tsv_false       ));
      AND_TABLE:array[T_triStateValue,T_triStateValue] of T_triStateValue=
                   //0         ?                1
                {0}((tsv_false,tsv_false       ,tsv_false       ),
                {?} (tsv_false,tsv_undetermined,tsv_undetermined),
                {1} (tsv_false,tsv_undetermined,tsv_true        ));

FUNCTION T_notGate.simulateStep:boolean;
  VAR previous:T_triStateValue;
  begin
    previous:=output;
    output:=NOT_TABLE[input];
    result:=output<>previous;
  end;

FUNCTION T_notGate.getOutput(CONST index: longint): T_wireValue;
  begin result:=output; end;

FUNCTION T_notGate.setInput(CONST index: longint; CONST value: T_wireValue):boolean;
  begin
    result:=input<>value.bit[0];
    input:=value.bit[0];
  end;

FUNCTION T_notGate.getInput(CONST index: longint): T_wireValue;
  begin result:=input; end;

CONSTRUCTOR T_binaryBaseGate.create;
  begin inherited; inputCount:=2; input[0]:=tsv_undetermined; input[1]:=tsv_undetermined; output:=tsv_undetermined; end;
PROCEDURE T_binaryBaseGate.reset;
  begin input[0]:=tsv_undetermined; input[1]:=tsv_undetermined; output:=tsv_undetermined; end;

FUNCTION T_binaryBaseGate.numberOfInputs: longint;
  begin result:=inputCount; end;

FUNCTION T_binaryBaseGate.numberOfOutputs: longint;
  begin result:=1; end;

FUNCTION T_binaryBaseGate.getOutput(CONST index:longint):T_wireValue;
  begin result:=output; end;

FUNCTION T_binaryBaseGate.setInput(CONST index: longint; CONST value: T_wireValue):boolean;
  begin
    result:=input[index]<>value.bit[0];
    input[index]:=value.bit[0];
  end;

FUNCTION T_binaryBaseGate.getInput(CONST index: longint): T_wireValue;
  begin result:=input[index]; end;

PROCEDURE T_binaryBaseGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false);
  begin
    inherited;
    stream.writeByte(inputCount);
  end;

PROCEDURE T_binaryBaseGate.readMetaDataFromStream(VAR stream: T_bufferedInputStreamWrapper);
  begin
    inputCount:=stream.readByte;
  end;

CONSTRUCTOR T_nxorGate.create; begin inherited; end;
CONSTRUCTOR T_norGate .create; begin inherited; end;
CONSTRUCTOR T_nandGate.create; begin inherited; end;
CONSTRUCTOR T_xorGate .create; begin inherited; end;
CONSTRUCTOR T_orGate  .create; begin inherited; end;
CONSTRUCTOR T_andGate .create; begin inherited; end;

FUNCTION T_nxorGate.simulateStep:boolean;
  VAR previous:T_triStateValue;
      i:longint;
  begin
    previous:=output;
    output:=input[0]; for i:=1 to inputCount-1 do output:=XOR_TABLE[output,input[i]]; output:=NOT_TABLE[output];
    result:=output<>previous;
  end;

FUNCTION T_norGate .simulateStep:boolean;
  VAR previous:T_triStateValue;
      i:longint;
  begin
    previous:=output;
    output:=input[0]; for i:=1 to inputCount-1 do output:=OR_TABLE[output,input[i]]; output:=NOT_TABLE[output];
    result:=output<>previous;
  end;

FUNCTION T_nandGate.simulateStep:boolean;
  VAR previous:T_triStateValue;
      i:longint;
  begin
    previous:=output;
    output:=input[0]; for i:=1 to inputCount-1 do output:=AND_TABLE[output,input[i]]; output:=NOT_TABLE[output];
    result:=output<>previous;
  end;

FUNCTION T_xorGate .simulateStep:boolean;
  VAR previous:T_triStateValue;
      i:longint;
  begin
    previous:=output;
    output:=input[0]; for i:=1 to inputCount-1 do output:=XOR_TABLE[output,input[i]];
    result:=output<>previous;
  end;

FUNCTION T_orGate  .simulateStep:boolean;
  VAR previous:T_triStateValue;
      i:longint;
  begin
    previous:=output;
    output:=input[0]; for i:=1 to inputCount-1 do output:=OR_TABLE[output,input[i]];
    result:=output<>previous;
  end;

FUNCTION T_andGate .simulateStep:boolean;
  VAR previous:T_triStateValue;
      i:longint;
  begin
    previous:=output;
    output:=input[0]; for i:=1 to inputCount-1 do output:=AND_TABLE[output,input[i]];
    result:=output<>previous;
  end;

FUNCTION T_nxorGate.gateType: T_gateType; begin result:=gt_nxorGate; end;
FUNCTION T_norGate .gateType: T_gateType; begin result:=gt_norGate;  end;
FUNCTION T_nandGate.gateType: T_gateType; begin result:=gt_nandGate; end;
FUNCTION T_xorGate .gateType: T_gateType; begin result:=gt_xorGate;  end;
FUNCTION T_orGate  .gateType: T_gateType; begin result:=gt_orGate;   end;
FUNCTION T_andGate .gateType: T_gateType; begin result:=gt_andGate;  end;

FUNCTION T_gatedClock.clone(CONST includeState:boolean): P_abstractGate;
  begin
    new(P_gatedClock(result),create);
    P_gatedClock(result)^.interval:=interval;
    if not includeState then exit(result);
    P_gatedClock(result)^.tick    :=tick;
    P_gatedClock(result)^.counter :=counter;
    P_gatedClock(result)^.enable  :=enable;
  end;

FUNCTION T_constantGate.clone(CONST includeState:boolean): P_abstractGate;
  begin
    new(P_constantGate(result),create(c=tsv_true));
  end;

FUNCTION T_adapter.clone(CONST includeState:boolean): P_abstractGate;
  begin
    new(P_adapter(result),create(inWidth,outWidth));
    if includeState then P_adapter(result)^.io:=io;
  end;

FUNCTION T_clock.clone(CONST includeState:boolean): P_abstractGate;
  begin
    new(P_clock(result),create);
    P_clock(result)^.interval:=interval;
    if not includeState then exit(result);
    P_clock(result)^.tick    :=tick;
    P_clock(result)^.counter :=counter;
  end;

FUNCTION T_notGate.clone(CONST includeState:boolean):P_abstractGate;
  begin
    new(P_notGate (result),create);
    if includeState then begin
      P_notGate (result)^.input :=input;
      P_notGate (result)^.output:=output;
    end;
  end;

FUNCTION T_inputGate.clone(CONST includeState: boolean): P_abstractGate;
  begin
    new(P_inputGate(result),create);
    P_inputGate(result)^.ioIndex:=ioIndex;
    P_inputGate(result)^.ioLabel:=ioLabel;
    P_inputGate(result)^.width:=width;
    P_inputGate(result)^.onLeftOrRightSide:=onLeftOrRightSide;
    P_inputGate(result)^.positionIndex:=positionIndex;
    result^.reset;
    if includeState then P_inputGate(result)^.io:=io;
  end;

FUNCTION T_outputGate.clone(CONST includeState:boolean):P_abstractGate;
  begin
    new(P_outputGate(result),create);
    P_outputGate(result)^.ioIndex:=ioIndex;
    P_outputGate(result)^.ioLabel:=ioLabel;
    P_outputGate(result)^.width:=width;
    P_inputGate(result)^.onLeftOrRightSide:=onLeftOrRightSide;
    P_inputGate(result)^.positionIndex:=positionIndex;
    result^.reset;
    if includeState then P_outputGate(result)^.io:=io;
  end;

FUNCTION T_binaryBaseGate.clone(CONST includeState:boolean):P_abstractGate;
  begin
    result:=newBaseGate(gateType);
    P_binaryBaseGate(result)^.inputCount:=inputCount;
    if includeState then begin
      P_binaryBaseGate(result)^.input:=input;
      P_binaryBaseGate(result)^.output:=output;
    end;
  end;

PROCEDURE T_abstractGate.countGates(VAR gateCount: T_gateCount);
  begin
    inc(gateCount[gateType]);
  end;

FUNCTION T_abstractGate.equals(CONST other: P_abstractGate): boolean;
  begin
    result:=other^.gateType=gateType;
  end;

FUNCTION T_inputGate.equals(CONST other: P_abstractGate): boolean;
  begin
    result:=(other^.gateType=gateType) and
      ((P_inputGate(other)^.ioIndex=ioIndex) and
       (P_inputGate(other)^.width  =width  ) and
       (P_inputGate(other)^.onLeftOrRightSide=onLeftOrRightSide) and
       (P_inputGate(other)^.ioLabel=ioLabel));
  end;

FUNCTION T_adapter.equals(CONST other:P_abstractGate):boolean;
  begin
    result:=(other^.gateType=gateType) and
      (P_adapter(other)^.inWidth=inWidth) and
      (P_adapter(other)^.outWidth=outWidth);
  end;

FUNCTION T_binaryBaseGate.equals(CONST other:P_abstractGate):boolean;
  begin
    result:=(other^.gateType=gateType) and (P_binaryBaseGate(other)^.inputCount=inputCount);
  end;

FUNCTION T_clock.equals(CONST other:P_abstractGate):boolean;
  begin
    result:=(other^.gateType=gateType) and (P_clock(other)^.interval=interval);
  end;

FUNCTION T_tendToTrue.equals(CONST other:P_abstractGate):boolean;
  begin
    result:=(other^.gateType=gateType) and (P_tendToTrue(other)^.input.width=input.width);
  end;

FUNCTION T_abstractGate.behaviorEquals(CONST other: P_abstractGate): boolean;
  VAR stepsUntilConfidence:longint=1024;
      stillSimulating:boolean;
      remainingSteps:longint;

  FUNCTION outputsDiffer:boolean;
    VAR s1,s2:boolean;
        i:longint;
    begin
      result:=false;
      dec(remainingSteps);
      dec(stepsUntilConfidence);
      s1:=       simulateStep;
      s2:=other^.simulateStep;
      stillSimulating:=s1 and s2 and (remainingSteps>0);
      if (s1<>s2) then exit(true);
      for i:=0 to numberOfOutputs-1 do if getOutput(i)<>other^.getOutput(i) then exit(true);
    end;
  VAR totalInputWidth:longint=0;
      bitToFlip:longint=-1;

  PROCEDURE setRandomInput;
    VAR i,j:longint;
        w:T_wireValue;
    begin
      stillSimulating:=true;
      if (bitToFlip<totalInputWidth) then remainingSteps:=256 else remainingSteps:=random(4096);
      if (bitToFlip>=0) and (bitToFlip<totalInputWidth) then begin
        j:=bitToFlip;
        i:=0;
        while j>=inputWidth(i) do begin
          j-=inputWidth(i);
          inc(i);
        end;
        w:=getInput(i);
        w.bit[j]:=NOT_TABLE[w.bit[j]];
        setInput       (i,w);
        other^.setInput(i,w);
      end else begin
        for i:=0 to numberOfInputs-1 do begin
          w.width:=inputWidth(i);
          for j:=0 to w.width-1 do
            if random<0.5
            then w.bit[j]:=tsv_true
            else w.bit[j]:=tsv_false;
          setInput       (i,w);
          other^.setInput(i,w);
        end;
      end;
      inc(bitToFlip);
    end;

  VAR k:longint;
  begin
    if (other=@self) then exit(true);
    //Check interfaces
    if (numberOfInputs <>other^.numberOfInputs ) or
       (numberOfOutputs<>other^.numberOfOutputs)
    then exit(false);
    for k:=0 to numberOfInputs-1 do begin
      if inputWidth (k)<>other^.inputWidth (k) then exit(false);
      totalInputWidth+=inputWidth(k);
    end;
    for k:=0 to numberOfOutputs-1 do if outputWidth(k)<>other^.outputWidth(k) then exit(false);

    //Simulate...
    //The wider the input, the more steps should be invested until gates are considered to behave equally.
    reset;
    other^.reset;
    setRandomInput;
    repeat
      if outputsDiffer
      then exit(false);
    until not(stillSimulating) or (stepsUntilConfidence<=0);

    while (stepsUntilConfidence>0) and (bitToFlip<totalInputWidth shl 4) do begin
      setRandomInput;
      repeat
        if outputsDiffer
        then exit(false);
      until not(stillSimulating) or (stepsUntilConfidence<=0);
    end;
    result:=true;
  end;

end.
