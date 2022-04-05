UNIT logicGates;

{$mode objfpc}{$H+}

INTERFACE
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
              gt_clock);

  T_triStateValue=(tsv_false,tsv_undetermined,tsv_true);

  P_abstractGate=^T_abstractGate;
  T_abstractGate=object
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy; virtual;
      PROCEDURE reset;                   virtual; abstract;
      FUNCTION  clone:P_abstractGate;    virtual; abstract;
      FUNCTION  caption:string;          virtual; abstract;
      FUNCTION  getDescription:string;   virtual;
      FUNCTION  numberOfInputs :longint; virtual; abstract;
      FUNCTION  numberOfOutputs:longint; virtual; abstract;
      FUNCTION  gateType:T_gateType;     virtual; abstract;
      FUNCTION  simulateStep:boolean;    virtual; abstract;
      FUNCTION  getOutput(CONST index:longint):T_triStateValue;             virtual; abstract;
      PROCEDURE setInput(CONST index:longint; CONST value:T_triStateValue); virtual; abstract;
      FUNCTION  getInput(CONST index:longint):T_triStateValue;              virtual; abstract;
    end;

  T_gateConnector=object
    gate:P_abstractGate;
    index:longint;
    FUNCTION getOutputValue:T_triStateValue;
    PROCEDURE setInputValue(CONST v:T_triStateValue);
  end;

  P_notGate=^T_notGate;
  T_notGate=object(T_abstractGate)
    private
      input,output:T_triStateValue;
    public
      CONSTRUCTOR create;
      PROCEDURE reset;                   virtual;
      FUNCTION  clone:P_abstractGate;    virtual;
      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  getOutput(CONST index:longint):T_triStateValue;             virtual;
      PROCEDURE setInput(CONST index:longint; CONST value:T_triStateValue); virtual;
      FUNCTION  getInput(CONST index:longint):T_triStateValue;              virtual;
  end;

  P_inputGate=^T_inputGate;
  T_inputGate=object(T_abstractGate)
    private
      io:T_triStateValue;
    public
      ioLabel:string;
      ioIndex:longint;
      CONSTRUCTOR create;
      PROCEDURE reset;                   virtual;
      FUNCTION  clone:P_abstractGate;    virtual;
      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  getOutput(CONST index:longint):T_triStateValue;             virtual;
      PROCEDURE setInput(CONST index:longint; CONST value:T_triStateValue); virtual;
      FUNCTION  getInput(CONST index:longint):T_triStateValue;              virtual;
  end;

  P_outputGate=^T_outputGate;
  T_outputGate=object(T_inputGate)
    public
      CONSTRUCTOR create;
      FUNCTION  clone:P_abstractGate;    virtual;
      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
  end;

  T_binaryBaseGate=object(T_abstractGate)
     private
       input:array[0..1] of T_triStateValue;
       output:T_triStateValue;
     public
       CONSTRUCTOR create;
       PROCEDURE reset;                   virtual;
       FUNCTION  numberOfInputs :longint; virtual;
       FUNCTION  numberOfOutputs:longint; virtual;
       FUNCTION  getOutput(CONST index:longint):T_triStateValue; virtual;
       PROCEDURE setInput(CONST index:longint; CONST value:T_triStateValue); virtual;
       FUNCTION  getInput(CONST index:longint):T_triStateValue;              virtual;
   end;

   P_andGate=^T_andGate;
   T_andGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  clone:P_abstractGate;    virtual;
     FUNCTION  caption:string; virtual;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_orGate=^T_orGate;
   T_orGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  clone:P_abstractGate;    virtual;
     FUNCTION  caption:string; virtual;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_xorGate=^T_xorGate;
   T_xorGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  clone:P_abstractGate;    virtual;
     FUNCTION  caption:string; virtual;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_nandGate=^T_nandGate;
   T_nandGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  clone:P_abstractGate;    virtual;
     FUNCTION  caption:string; virtual;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_norGate=^T_norGate;
   T_norGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  clone:P_abstractGate;    virtual;
     FUNCTION  caption:string; virtual;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_nxorGate=^T_nxorGate;
   T_nxorGate=object(T_binaryBaseGate)
     CONSTRUCTOR create;
     FUNCTION  clone:P_abstractGate;    virtual;
     FUNCTION  caption:string; virtual;
     FUNCTION  simulateStep:boolean; virtual;
     FUNCTION  gateType:T_gateType; virtual;
   end;

   P_clock=^T_clock;

   { T_clock }

   T_clock=object(T_abstractGate)
      tick:boolean;
      interval,counter:longint;

      CONSTRUCTOR create;
      PROCEDURE reset;                   virtual;
      FUNCTION  clone:P_abstractGate;    virtual;
      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  getOutput(CONST index:longint):T_triStateValue;             virtual;
      PROCEDURE setInput(CONST index:longint; CONST value:T_triStateValue); virtual;
      FUNCTION  getInput(CONST index:longint):T_triStateValue;              virtual;
    end;

FUNCTION newBaseGate(CONST gateType:T_gateType):P_abstractGate;
OPERATOR =(CONST x,y:T_gateConnector):boolean;
IMPLEMENTATION
USES sysutils;
OPERATOR =(CONST x,y:T_gateConnector):boolean;
  begin
    result:=(x.gate=y.gate) and (x.index=y.index);
  end;

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
      else result:=nil;
    end;
  end;

{ T_clock }

CONSTRUCTOR T_clock.create;
  begin
    inherited;
    tick:=true;
    interval:=1;
    counter:=0;
  end;

PROCEDURE T_clock.reset;
  begin
    tick:=true;
    counter:=0;
  end;

FUNCTION T_clock.clone: P_abstractGate;
  begin
    new(P_clock(result),create);
    P_clock(result)^.interval:=interval;
  end;

FUNCTION T_clock.caption: string;
  begin
    result:=#240#159#149#145;
    //0xF0 0x9F 0x95 0x91;
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

FUNCTION T_clock.getOutput(CONST index: longint): T_triStateValue;
  CONST T:array[false..true] of T_triStateValue=(tsv_false,tsv_true);
  begin result:=T[tick]; end;

PROCEDURE T_clock.setInput(CONST index: longint; CONST value: T_triStateValue);
  begin end;

FUNCTION T_clock.getInput(CONST index: longint): T_triStateValue;
  begin result:=tsv_undetermined; end;

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
  begin inherited; io:=tsv_true; ioLabel:=''; end;

PROCEDURE T_inputGate.reset;
  begin io:=tsv_true; end;

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

FUNCTION T_inputGate.gateType: T_gateType;
  begin result:=gt_input; end;

FUNCTION T_inputGate.simulateStep:boolean;
  begin
    result:=false;
  end;

FUNCTION T_inputGate.getOutput(CONST index: longint): T_triStateValue;
  begin result:=io; end;

PROCEDURE T_inputGate.setInput(CONST index: longint; CONST value: T_triStateValue);
  begin io:=value; end;

FUNCTION T_inputGate.getInput(CONST index: longint): T_triStateValue;
  begin result:=io; end;

{ T_abstractGate }

CONSTRUCTOR T_abstractGate.create;
  begin end; //pro forma

DESTRUCTOR T_abstractGate.destroy;
  begin end; //pro forma

FUNCTION T_abstractGate.getDescription:string;
  begin result:=''; end; //plausible default

FUNCTION T_gateConnector.getOutputValue: T_triStateValue;
  begin
    result:=gate^.getOutput(index);
  end;

PROCEDURE T_gateConnector.setInputValue(CONST v: T_triStateValue);
  begin
    gate^.setInput(index,v);
  end;

CONSTRUCTOR T_notGate.create;
  begin inherited; input:=tsv_undetermined; output:=tsv_undetermined; end;

PROCEDURE T_notGate.reset;
  begin input:=tsv_undetermined; output:=tsv_undetermined; end;

FUNCTION T_notGate.caption: string;
  begin result:='NOT'; end;

FUNCTION T_notGate.numberOfInputs: longint;
  begin  result:=1; end;

FUNCTION T_notGate.numberOfOutputs: longint;
  begin result:=1; end;

FUNCTION T_notGate.gateType: T_gateType;
  begin result:=gt_notGate; end;

FUNCTION T_notGate.simulateStep:boolean;
  CONST F:array[T_triStateValue] of T_triStateValue=(tsv_true,tsv_undetermined,tsv_false);
  VAR previous:T_triStateValue;
  begin
    previous:=output;
    output:=F[input];
    result:=output<>previous;
  end;

FUNCTION T_notGate.getOutput(CONST index: longint): T_triStateValue;
  begin result:=output; end;

PROCEDURE T_notGate.setInput(CONST index: longint; CONST value: T_triStateValue);
  begin input:=value;  end;

FUNCTION T_notGate.getInput(CONST index: longint): T_triStateValue;
  begin result:=input; end;

CONSTRUCTOR T_binaryBaseGate.create;
  begin inherited; input[0]:=tsv_undetermined; input[1]:=tsv_undetermined; output:=tsv_undetermined; end;
PROCEDURE T_binaryBaseGate.reset;
  begin input[0]:=tsv_undetermined; input[1]:=tsv_undetermined; output:=tsv_undetermined; end;

FUNCTION T_binaryBaseGate.numberOfInputs: longint;
  begin result:=2; end;

FUNCTION T_binaryBaseGate.numberOfOutputs: longint;
  begin result:=1; end;

FUNCTION T_binaryBaseGate.getOutput(CONST index:longint):T_triStateValue;
  begin result:=output; end;

PROCEDURE T_binaryBaseGate.setInput(CONST index: longint; CONST value: T_triStateValue);
  begin input[index]:=value; end;

FUNCTION T_binaryBaseGate.getInput(CONST index: longint): T_triStateValue;
  begin result:=input[index]; end;

CONSTRUCTOR T_nxorGate.create; begin inherited; end;
CONSTRUCTOR T_norGate .create; begin inherited; end;
CONSTRUCTOR T_nandGate.create; begin inherited; end;
CONSTRUCTOR T_xorGate .create; begin inherited; end;
CONSTRUCTOR T_orGate  .create; begin inherited; end;
CONSTRUCTOR T_andGate .create; begin inherited; end;

FUNCTION T_nxorGate.caption: string; begin result:='NXOR'; end;
FUNCTION T_norGate .caption: string; begin result:='NOR';  end;
FUNCTION T_nandGate.caption: string; begin result:='NAND'; end;
FUNCTION T_xorGate .caption: string; begin result:='XOR';  end;
FUNCTION T_orGate  .caption: string; begin result:='OR';   end;
FUNCTION T_andGate .caption: string; begin result:='AND';  end;

FUNCTION T_nxorGate.simulateStep:boolean;
  CONST F:array[T_triStateValue,T_triStateValue] of T_triStateValue=
          //0                ?                1
       {0}((tsv_true        ,tsv_undetermined,tsv_false       ),
       {?} (tsv_undetermined,tsv_undetermined,tsv_undetermined),
       {1} (tsv_false       ,tsv_undetermined,tsv_true        ));
  VAR previous:T_triStateValue;
  begin
    previous:=output;
    output:=F[input[0],input[1]];
    result:=output<>previous;
  end;

FUNCTION T_norGate .simulateStep:boolean;
  CONST F:array[T_triStateValue,T_triStateValue] of T_triStateValue=
          //0                ?                1
       {0}((tsv_true        ,tsv_undetermined,tsv_false),
       {?} (tsv_undetermined,tsv_undetermined,tsv_false),
       {1} (tsv_false       ,tsv_false       ,tsv_false));
  VAR previous:T_triStateValue;
  begin
    previous:=output;
    output:=F[input[0],input[1]];
    result:=output<>previous;
  end;

FUNCTION T_nandGate.simulateStep:boolean;
  CONST F:array[T_triStateValue,T_triStateValue] of T_triStateValue=
          //0        ?                1
       {0}((tsv_true,tsv_true        ,tsv_true        ),
       {?} (tsv_true,tsv_undetermined,tsv_undetermined),
       {1} (tsv_true,tsv_undetermined,tsv_false       ));
  VAR previous:T_triStateValue;
  begin
    previous:=output;
    output:=F[input[0],input[1]];
    result:=output<>previous;
  end;

FUNCTION T_xorGate .simulateStep:boolean;
  CONST F:array[T_triStateValue,T_triStateValue] of T_triStateValue=
          //0                ?                1
       {0}((tsv_false       ,tsv_undetermined,tsv_true        ),
       {?} (tsv_undetermined,tsv_undetermined,tsv_undetermined),
       {1} (tsv_true        ,tsv_undetermined,tsv_false       ));
  VAR previous:T_triStateValue;
  begin
    previous:=output;
    output:=F[input[0],input[1]];
    result:=output<>previous;
  end;

FUNCTION T_orGate  .simulateStep:boolean;
  CONST F:array[T_triStateValue,T_triStateValue] of T_triStateValue=
          //0                ?                1
       {0}((tsv_false       ,tsv_undetermined,tsv_true),
       {?} (tsv_undetermined,tsv_undetermined,tsv_true),
       {1} (tsv_true        ,tsv_true        ,tsv_true));
  VAR previous:T_triStateValue;
  begin
    previous:=output;
    output:=F[input[0],input[1]];
    result:=output<>previous;
  end;

FUNCTION T_andGate .simulateStep:boolean;
  CONST F:array[T_triStateValue,T_triStateValue] of T_triStateValue=
          //0         ?                1
       {0}((tsv_false,tsv_false       ,tsv_false       ),
       {?} (tsv_false,tsv_undetermined,tsv_undetermined),
       {1} (tsv_false,tsv_undetermined,tsv_true        ));
  VAR previous:T_triStateValue;
  begin
    previous:=output;
    output:=F[input[0],input[1]];
    result:=output<>previous;
  end;

FUNCTION T_nxorGate.gateType: T_gateType; begin result:=gt_nxorGate; end;
FUNCTION T_norGate .gateType: T_gateType; begin result:=gt_norGate;  end;
FUNCTION T_nandGate.gateType: T_gateType; begin result:=gt_nandGate; end;
FUNCTION T_xorGate .gateType: T_gateType; begin result:=gt_xorGate;  end;
FUNCTION T_orGate  .gateType: T_gateType; begin result:=gt_orGate;   end;
FUNCTION T_andGate .gateType: T_gateType; begin result:=gt_andGate;  end;

FUNCTION T_notGate   .clone:P_abstractGate; begin new(P_notGate (result),create); end;
FUNCTION T_nxorGate  .clone:P_abstractGate; begin new(P_nxorGate(result),create); end;
FUNCTION T_norGate   .clone:P_abstractGate; begin new(P_norGate (result),create); end;
FUNCTION T_nandGate  .clone:P_abstractGate; begin new(P_nandGate(result),create); end;
FUNCTION T_xorGate   .clone:P_abstractGate; begin new(P_xorGate (result),create); end;
FUNCTION T_orGate    .clone:P_abstractGate; begin new(P_orGate  (result),create); end;
FUNCTION T_andGate   .clone:P_abstractGate; begin new(P_andGate (result),create); end;
FUNCTION T_inputGate .clone:P_abstractGate; begin new(P_inputGate (result),create); P_inputGate (result)^.ioIndex:=ioIndex; P_inputGate (result)^.ioLabel:=ioLabel; end;
FUNCTION T_outputGate.clone:P_abstractGate; begin new(P_outputGate(result),create); P_outputGate(result)^.ioIndex:=ioIndex; P_outputGate(result)^.ioLabel:=ioLabel; end;

end.

