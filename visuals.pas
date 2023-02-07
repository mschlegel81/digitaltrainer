UNIT visuals;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, StdCtrls,ExtCtrls;

TYPE   T_shapeAndLabel=record colorIndex:byte; Shape:TShape; labl:TLabel; end;

CONST ENABLED_BUTTON_COLOR =$00603030;
      DISABLED_BUTTON_COLOR=$00703838;
      ENABLED_TEXT_COLOR   =$00FFFFFF;
      DISABLED_TEXT_COLOR  =$00909090;

PROCEDURE setEnableButton(Shape:TShape; CONST labl:TLabel; CONST enable:boolean);
IMPLEMENTATION

PROCEDURE setEnableButton(Shape: TShape; CONST labl:TLabel; CONST enable: boolean);
  begin
    Shape.enabled:=enable;
    labl .enabled:=enable;
    if enable then begin
      Shape.Brush.color:=ENABLED_BUTTON_COLOR;
      labl.Font.color:=ENABLED_TEXT_COLOR;
    end else begin
      Shape.Brush.color:=DISABLED_BUTTON_COLOR;
      labl.Font.color:=DISABLED_TEXT_COLOR;
    end;
  end;

end.

