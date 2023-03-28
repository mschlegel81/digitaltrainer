UNIT visuals;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, StdCtrls,ExtCtrls,Forms;

TYPE  T_shapeAndLabel=record colorIndex:byte; Shape:TShape; labl:TLabel; end;
      T_colorScheme  =record
        ENABLED_BUTTON_COLOR ,
        DISABLED_BUTTON_COLOR,
        ENABLED_TEXT_COLOR   ,
        DISABLED_TEXT_COLOR  ,
        GATE_COLOR           ,
        GATE_LABEL_COLOR     ,
        MARK_COLOR           ,
        BOARD_COLOR          ,
        SHADOW_COLOR         ,
        CORRECT_COLOR        ,
        INCORRECT_COLOR      ,
        WIRE_COLOR           ,
        BOARD_BOUNDARY_COLOR ,
        TRUE_COLOR        ,
        FALSE_COLOR       ,
        UNDETERMINED_COLOR,
        MULTIBIT_COLOR    :longint;

        SEVEN_SEGMENT_COLOR:array[false..true] of longint;

        buttonColorTable:array[0..10] of longint;
        tableColor,
        tableFixedColor,
        tableAlternativeColor,
        editorBackgroundColor,
        secondaryFormColor,
        panelColor:longint;
      end;

VAR colorScheme:T_colorScheme;
FUNCTION getColorSchemeIndex:longint;
PROCEDURE setColorScheme(CONST index:longint);
PROCEDURE setEnableButton(Shape:TShape; CONST labl:TLabel; CONST enable:boolean);
PROCEDURE applyColorScheme(CONST form:TForm);
IMPLEMENTATION
USES Controls, Graphics, Grids,ValEdit;
VAR colorSchemeIndex:longint;

FUNCTION settingsFileName:string;
  begin
    result:=ChangeFileExt(paramStr(0),'.visuals');
  end;

PROCEDURE saveSettins;
  VAR handle:textFile;
  begin
    assign(handle,settingsFileName);
    rewrite(handle);
    writeln(handle,colorSchemeIndex);
    close(handle);
  end;

PROCEDURE loadSettings;
  VAR handle:textFile;
  begin
    try
      assign(handle,settingsFileName);
      reset(handle);
      readln(handle,colorSchemeIndex);
      close(handle);
    except
      colorSchemeIndex:=0;
    end;
    setColorScheme(colorSchemeIndex);
  end;

FUNCTION getColorSchemeIndex: longint;
  begin
    result:=colorSchemeIndex;
  end;

PROCEDURE setColorScheme(CONST index: longint);
CONST DEFAULT_SCHEME:T_colorScheme=
      (ENABLED_BUTTON_COLOR :$00603030;
       DISABLED_BUTTON_COLOR:$00703838;
       ENABLED_TEXT_COLOR   :$00FFFFFF;
       DISABLED_TEXT_COLOR  :$00909090;
       GATE_COLOR           :$00603030;
       GATE_LABEL_COLOR     :$00FFFFFF;
       MARK_COLOR           :$0000AAFF;
       BOARD_COLOR          :$00804040;
       SHADOW_COLOR         :$00402020;
       CORRECT_COLOR        :$0000FF00;
       INCORRECT_COLOR      :$000000ff;
       WIRE_COLOR           :$00FFFFFF;
       BOARD_BOUNDARY_COLOR :$00603030;
       TRUE_COLOR           :$000080ff;
       FALSE_COLOR          :0;
       UNDETERMINED_COLOR   :$00402020;
       MULTIBIT_COLOR       :$00808080;
       SEVEN_SEGMENT_COLOR  :($00402020,$0000AAFF);

       buttonColorTable:($00603030,$00703838,$00804040,$00904848,$00A05050,$00B05858,$00BF5F5F,$00CF6767,$00DF6F6F,$00EF7777,$00FF7F7F);

       tableColor     :$00804040;
       tableFixedColor:$00603030;
       tableAlternativeColor:$00603030;
       editorBackgroundColor:$00703838;
       secondaryFormColor: $00703838;
       panelColor:$00703838);

       BLACK_ON_WHITE_SCHEME:T_colorScheme=
      (ENABLED_BUTTON_COLOR :$00FFFFFF;
       DISABLED_BUTTON_COLOR:$00E0E0E0;
       ENABLED_TEXT_COLOR   :$00000000;
       DISABLED_TEXT_COLOR  :$00505050;
       GATE_COLOR           :$00FFFFFF;
       GATE_LABEL_COLOR     :$00000000;
       MARK_COLOR           :$0000AAFF;
       BOARD_COLOR          :$00FFFFFF;
       SHADOW_COLOR         :$00e0e0e0;
       CORRECT_COLOR        :$00008000;
       INCORRECT_COLOR      :$000000ff;
       WIRE_COLOR           :$00000000;
       BOARD_BOUNDARY_COLOR :$00c0c0c0;
       TRUE_COLOR           :$0080ff80;
       FALSE_COLOR          :$008080ff;
       UNDETERMINED_COLOR   :$00FFFFFF;
       MULTIBIT_COLOR       :$00A0A0A0;
       SEVEN_SEGMENT_COLOR  :($00E0E0E0,$00000000);

       buttonColorTable:($00FFFFFF,$00EEEEEE,$00DDDDDD,$00CCCCCC,$00BBBBBB,$00AAAAAA,$00999999,$00888888,$00777777,$00666666,$00555555);

       tableColor     :$00FFFFFF;
       tableFixedColor:$00E0E0E0;
       tableAlternativeColor:$00E0E0E0;
       editorBackgroundColor:$00FFFFFF;
       secondaryFormColor: $00FFFFFF;
       panelColor:$00FFFFFF);

  begin
    colorSchemeIndex:=index;
    if index=1 then colorScheme:=BLACK_ON_WHITE_SCHEME else colorScheme:=DEFAULT_SCHEME;

  end;

PROCEDURE setEnableButton(Shape: TShape; CONST labl:TLabel; CONST enable: boolean);
  begin
    Shape.enabled:=enable;
    labl .enabled:=enable;
    if enable then begin
      Shape.Brush.color:=colorScheme.ENABLED_BUTTON_COLOR;
      labl.Font.color:=colorScheme.ENABLED_TEXT_COLOR;
    end else begin
      Shape.Brush.color:=colorScheme.DISABLED_BUTTON_COLOR;
      labl.Font.color:=colorScheme.DISABLED_TEXT_COLOR;
    end;
  end;

PROCEDURE applyColorScheme(CONST form:TForm);
  PROCEDURE applyScheme(CONST control:TControl);
    VAR i:longint;
    begin

      control.Font.color:=colorScheme.ENABLED_TEXT_COLOR;

      if control is TShape then (TShape(control).Brush.color:=colorScheme.ENABLED_BUTTON_COLOR)
      else if control is TStringGrid then begin
        TStringGrid(control).FixedColor:=colorScheme.tableFixedColor;
        control.color:=colorScheme.tableColor;
      end else if control is TValueListEditor then begin
        TValueListEditor(control).FixedColor:=colorScheme.tableFixedColor;
        control.color:=colorScheme.tableColor;
      end else if control.color<>clNone then control.color:=colorScheme.panelColor;

      if control is TWinControl
      then for i:=0 to TWinControl(control).ControlCount-1
           do applyScheme(TWinControl(control).Controls[i]);
    end;

  VAR i:longint;
  begin
    form.color:=colorScheme.secondaryFormColor;
    form.Font.color:=colorScheme.ENABLED_TEXT_COLOR;
    for i:=0 to form.ControlCount-1
    do applyScheme(form.Controls[i]);
  end;

INITIALIZATION
  loadSettings;
FINALIZATION
  saveSettins;

end.

