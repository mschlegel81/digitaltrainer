UNIT logicGates;

{$mode objfpc}

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
              gt_gatedClock);
  T_gateTypeSet=array of T_gateType;
  T_gateCount=array[T_gateType] of longint;

CONST
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
    {gt_gatedClock} 'gated clock');

TYPE
  T_triStateValue=(tsv_false,tsv_undetermined,tsv_true);

  T_wireValue=record
    bit:array[0..WIRE_MAX_WIDTH-1] of T_triStateValue;
    width:byte;
  end;

  P_abstractGate=^T_abstractGate;
  T_abstractGate=object
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy; virtual;
      PROCEDURE reset;                   virtual; abstract;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate; virtual; abstract;
      FUNCTION  caption:string;          virtual;
      FUNCTION  getDescription:string;   virtual;
      FUNCTION  numberOfInputs :longint; virtual; abstract;
      FUNCTION  numberOfOutputs:longint; virtual; abstract;
      FUNCTION  inputWidth (CONST index:longint):byte; virtual;
      FUNCTION  outputWidth(CONST index:longint):byte; virtual;
      FUNCTION  gateType:T_gateType;     virtual; abstract;
      FUNCTION  simulateStep:boolean;    virtual; abstract;
      FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual; abstract;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual; abstract;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual; abstract;

      PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
      PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;

      PROCEDURE countGates(VAR gateCount:T_gateCount); virtual;
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
      FUNCTION  caption:string;          virtual;
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
  T_inputGate=object(T_abstractGate)
    private
      io:T_wireValue;
    public
      ioLabel:shortstring;
      ioIndex:longint;
      width:byte;
      CONSTRUCTOR create;
      PROCEDURE reset;                   virtual;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  inputWidth (CONST index:longint):byte; virtual;
      FUNCTION  outputWidth(CONST index:longint):byte; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;

      PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
      PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
  end;

  P_outputGate=^T_outputGate;
  T_outputGate=object(T_inputGate)
    public
      CONSTRUCTOR create;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
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
      FUNCTION  caption:string;          virtual;
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

      PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
      PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
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

       PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
       PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
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
      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;

      PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
      PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;
    end;

   P_gatedClock=^T_gatedClock;
   T_gatedClock=object(T_clock)
      enable:T_triStateValue;
      CONSTRUCTOR create;
      PROCEDURE reset;                   virtual;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate;    virtual;
      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;
   end;

FUNCTION newBaseGate(CONST gateType:T_gateType):P_abstractGate;
OPERATOR =(CONST x,y:T_gateConnector):boolean;
OPERATOR :=(CONST x:T_triStateValue):T_wireValue;
FUNCTION isFullyDefined(CONST w:T_wireValue):boolean;
OPERATOR =(CONST x,y:T_wireValue):boolean;
FUNCTION getBinaryString(CONST wire:T_wireValue):string;
FUNCTION getDecimalString(CONST wire:T_wireValue):string;
FUNCTION getDecimalValue(CONST wire:T_wireValue; OUT valid:boolean):longint;
FUNCTION get2ComplementString(CONST wire:T_wireValue):string;
FUNCTION get2ComplementValue(CONST wire:T_wireValue; OUT valid:boolean):longint;

FUNCTION parseWireBin        (CONST s:string; CONST width:byte):T_wireValue;
FUNCTION parseWireDecimal    (CONST s:string; CONST width:byte):T_wireValue;
FUNCTION parseWire2Complement(CONST s:string; CONST width:byte):T_wireValue;
IMPLEMENTATION
USES sysutils;
FUNCTION getBinaryString(CONST wire:T_wireValue):string;
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

FUNCTION getDecimalString(CONST wire:T_wireValue):string;
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

FUNCTION getDecimalValue(CONST wire:T_wireValue; OUT valid:boolean):longint;
  VAR i:longint;
      k:int64=0;
  begin
    valid:=true;
    for i:=wire.width-1 downto 0 do begin
      k:=k shl 1;
      case wire.bit[i] of
        tsv_true        : inc(k);
        tsv_false       : begin end;
        tsv_undetermined: begin
          valid:=false;
          exit(1 shl (wire.width-1));
        end;
      end;
    end;
    result:=k;
  end;

FUNCTION get2ComplementString(CONST wire:T_wireValue):string;
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

FUNCTION get2ComplementValue(CONST wire:T_wireValue; OUT valid:boolean):longint;
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

OPERATOR =(CONST x,y:T_gateConnector):boolean;
  begin
    result:=(x.gate=y.gate) and (x.index=y.index);
  end;

VAR lastAdapterInputWidth:byte=1;
    lastAdapterOutputWidth:byte=4;

FUNCTION newBaseGate(CONST gateType:T_gateType):P_abstractGate;
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
      gt_adapter : new(P_adapter(result),create(lastAdapterInputWidth,lastAdapterOutputWidth));
      gt_true :new(P_constantGate(result),create(true ));
      gt_false:new(P_constantGate(result),create(false));
      gt_gatedClock: new(P_gatedClock(result),create);
      else result:=nil;
    end;
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

FUNCTION T_gatedClock.caption: string;
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

FUNCTION T_constantGate.caption: string;
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

FUNCTION T_adapter.caption: string;
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
    lastAdapterInputWidth :=inWidth;
    lastAdapterOutputWidth:=outWidth;
  end;

PROCEDURE T_adapter.writeToStream(VAR stream: T_bufferedOutputStreamWrapper);
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

FUNCTION T_clock.caption: string;
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

OPERATOR :=(CONST x:T_triStateValue):T_wireValue;
  VAR i:longint;
  begin
    result.width:=1;
    result.bit[0]:=x;
    for i:=1 to WIRE_MAX_WIDTH-1 do result.bit[i]:=tsv_undetermined;
  end;

FUNCTION isFullyDefined(CONST w:T_wireValue):boolean;
  VAR i:longint;
  begin
    result:=true;
    for i:=0 to w.width-1 do if not(w.bit[i] in [tsv_true,tsv_false]) then exit(false);
  end;

OPERATOR =(CONST x,y:T_wireValue):boolean;
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

PROCEDURE T_clock.writeToStream(VAR stream: T_bufferedOutputStreamWrapper);
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

FUNCTION T_outputGate.caption: string;
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
  begin inherited; io:=tsv_true; ioLabel:=''; width:=1; end;

PROCEDURE T_inputGate.reset;
  VAR i:longint;
  begin
    io.width:=width;
    for i:=0 to WIRE_MAX_WIDTH-1 do io.bit[i]:=tsv_undetermined;
  end;

FUNCTION T_inputGate.caption: string;
  begin
    if ioLabel=''
    then result:='in '+intToStr(ioIndex)
    else result:=ioLabel;
  end;

FUNCTION T_inputGate.numberOfInputs: longint;
  begin result:=0; end;

FUNCTION T_inputGate.numberOfOutputs: longint;
  begin result:=1; end;

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

PROCEDURE T_inputGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    inherited;
    stream.writeShortString(ioLabel);
    stream.writeNaturalNumber(ioIndex);
    stream.writeByte(width);
  end;

PROCEDURE T_inputGate.readMetaDataFromStream(VAR stream: T_bufferedInputStreamWrapper);
  begin
    ioLabel:=stream.readShortString;
    ioIndex:=stream.readNaturalNumber;
    width:=stream.readByte;
  end;

{ T_abstractGate }

CONSTRUCTOR T_abstractGate.create;
  begin end; //pro forma

DESTRUCTOR T_abstractGate.destroy;
  begin end; //pro forma

FUNCTION T_abstractGate.getDescription:string;
  begin result:=''; end; //plausible default

FUNCTION T_abstractGate.inputWidth(CONST index:longint):byte;
  begin result:=1; end;

FUNCTION T_abstractGate.outputWidth(CONST index:longint):byte;
  begin result:=1; end;

PROCEDURE T_abstractGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    stream.writeByte(byte(gateType));
  end;

PROCEDURE T_abstractGate.readMetaDataFromStream(VAR stream: T_bufferedInputStreamWrapper);
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

CONSTRUCTOR T_notGate.create;
  begin inherited; input:=tsv_undetermined; output:=tsv_undetermined; end;

PROCEDURE T_notGate.reset;
  begin input:=tsv_undetermined; output:=tsv_undetermined; end;

FUNCTION T_abstractGate.caption: string;
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

PROCEDURE T_binaryBaseGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper);
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

FUNCTION T_inputGate.clone(CONST includeState:boolean): P_abstractGate;
  begin
    new(P_inputGate(result),create);
    P_inputGate(result)^.ioIndex:=ioIndex;
    P_inputGate(result)^.ioLabel:=ioLabel;
    P_inputGate(result)^.width:=width;
    result^.reset;
    if includeState then P_inputGate(result)^.io:=io;
  end;

FUNCTION T_outputGate.clone(CONST includeState:boolean):P_abstractGate;
  begin
    new(P_outputGate(result),create);
    P_outputGate(result)^.ioIndex:=ioIndex;
    P_outputGate(result)^.ioLabel:=ioLabel;
    P_outputGate(result)^.width:=width;
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

PROCEDURE T_abstractGate.countGates(VAR gateCount:T_gateCount);
  begin
    inc(gateCount[gateType]);
  end;

end.

